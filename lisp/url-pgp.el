;;; url-pgp.el --- PGP encapsulation of HTTP
;; Author: $Author: wmperry $
;; Created: $Date: 1998/12/01 22:12:09 $
;; Version: $Revision: 1.1 $
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1998 Free Software Foundation, Inc.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url-vars)
(require 'url-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UUencoding
;;; ----------
;;; These functions are needed for the (RI)PEM encoding.  PGP can
;;; handle binary data, but (RI)PEM requires that it be uuencoded
;;; first, or it will barf severely.  How rude.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-uuencode-buffer (&optional buff)
  "UUencode buffer BUFF, with a default of the current buffer."
  (setq buff (or buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (url-lazy-message "UUencoding...")
    (call-process-region (point-min) (point-max)
			 url-uuencode-program t t nil "url-temp-file")
    (url-lazy-message "UUencoding... done.")))

(defun url-uudecode-buffer (&optional buff)
  "UUdecode buffer BUFF, with a default of the current buffer."
  (setq buff (or buff (current-buffer)))
  (let ((newname (url-generate-unique-filename)))
    (save-excursion
      (set-buffer buff)
      (goto-char (point-min))
      (re-search-forward "^begin [0-9][0-9][0-9] \\(.*\\)$" nil t)
      (replace-match (concat "begin 600 " newname))
      (url-lazy-message "UUdecoding...")
      (call-process-region (point-min) (point-max) url-uudecode-program)
      (url-lazy-message "UUdecoding...")
      (erase-buffer)
      (insert-file-contents-literally newname)
      (url-lazy-message "UUdecoding... done.")
      (condition-case ()
	  (delete-file newname)
	(error nil)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoding PGP/PEM responses
;;; --------------------------
;;; A PGP/PEM encrypted/signed response contains all the real headers,
;;; so this is just a quick decrypt-then-reparse hack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-decode-pgp/pem (arg)
  "Decode a pgp/pem response from an HTTP/1.0 server.
This expects the decoded message to contain all the necessary HTTP/1.0 headers
to correctly act on the decoded message (new content-type, etc)."
  (mc-decrypt-message)
  (url-parse-mime-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PGP/PEM Encryption
;;; ------------------
;;; This implements the highly secure PGP/PEM encrypted requests, as
;;; specified by NCSA and CERN.
;;;
;;; The complete online spec of this scheme was done by Tony Sanders
;;; <sanders@bsdi.com>, and can be seen at
;;; http://www.bsdi.com/HTTP:TNG/ripem-http.txt
;;;
;;; This section of code makes use of the EXCELLENT mailcrypt.el
;;; package by Jin S Choi (jsc@mit.edu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun url-public-key-exists (entity scheme)
  "Return t iff a key for ENTITY exists using public key system SCHEME.
ENTITY is the username/hostname combination we are checking for.
SCHEME is a symbol representing what public key encryption program to use.
       Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
       recognized."
  (let (retval)
    (save-excursion
      (cond
       ((eq 'pgp scheme)			; PGP encryption
	(set-buffer (get-buffer-create " *keytmp*"))
	(erase-buffer)
	(call-process mc-pgp-path nil t nil "+batchmode" "-kxaf" entity)
	(goto-char (point-min))
	(setq retval (search-forward mc-pgp-key-begin-line nil t)))
       ((eq 'pem scheme)			; PEM encryption
	(set-buffer (find-file-noselect mc-ripem-pubkeyfile))
	(goto-char (point-min))
	(setq retval (search-forward entity nil t)))
       (t
	(url-warn 'security
		  (format
		   "Bad value for SCHEME in url-public-key-exists %s"
		   scheme))))
      (kill-buffer (current-buffer)))
    retval))

(defun url-get-server-keys (entity &optional scheme)
  "Make sure the key for ENTITY exists using SCHEME.
ENTITY is the username/hostname combination to get the info for.  
       This should be a string you could pass to 'finger'.
SCHEME is a symbol representing what public key encryption program to use.
       Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
       recognized."
  (or scheme (setq scheme mc-default-scheme))
  (save-excursion
    (cond
     ((url-public-key-exists entity scheme) nil)
     (t
      (string-match "\\([^@]+\\)@\\(.*\\)" entity)
      (let ((url-working-buffer " *url-get-keys*"))
	(url-retrieve (format "gopher://%s:79/0%s/w" (url-match entity 1)
			     (url-match entity 2)))
	(mc-snarf-keys)
	(kill-buffer url-working-buffer))))))
   
(defun url-fetch-with-pgp (url recipient type)
  "Retrieve a document with public-key authentication.
      URL is the url to request from the server.
RECIPIENT is the server's entity name (usually webmaster@host)
     TYPE is a symbol representing what public key encryption program to use.
          Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
          recognized."
  (or noninteractive (require 'mailcrypt))
  (let ((request (url-create-mime-request url "PGP-Redirect"))
	(url-request-data nil)
	(url-request-extra-headers nil))
    (save-excursion
      (url-get-server-keys recipient type)
      (set-buffer (get-buffer-create " *url-encryption*"))
      (erase-buffer)
      (insert "\n\n" mail-header-separator "\n" request)
      (mc-encrypt-message recipient type)
      (goto-char (point-min))
      (if (re-search-forward (concat "\n" mail-header-separator "\n") nil t)
	  (delete-region (point-min) (point)))
      (setq url-request-data (buffer-string)
	    url-request-extra-headers
	    (list (cons "Authorized" (format "%s entity=\"%s\""
					     (cond
					      ((eq type 'pgp) "PGP")
					      ((eq type 'pem) "PEM"))
					     url-pgp/pem-entity))
		  (cons "Content-type" (format "application/x-www-%s-reply"
					       (cond
						((eq type 'pgp) "pgp")
						((eq type 'pem) "pem")))))))
    (kill-buffer " *url-encryption*")
    (url-retrieve (url-expand-file-name "/") t)))
     
(provide 'url-pgp)
