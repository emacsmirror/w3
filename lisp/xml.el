;;; xml.el --- XML parser
;; Author: $Author: wmperry $
;; Created: $Date: 1998/12/01 22:12:12 $
;; Version: $Revision: 1.1 $
;; Keywords: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997 - 1998 Free Software Foundation, Inc.
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

(require 'cl)
(require 'custom)

(defgroup xml nil
  "XML parsing options"
  :group 'w3)

(defcustom xml-parsing-program "xmlelisp"
  "External program to parse XML documents."
  :group 'xml
  :type 'file)

(defun xml-parse (&optional buffer)
  "Parse contents of BUFFER as XML.
BUFFER defaults to the current buffer.
Destructively alters contents of BUFFER.
Returns a data structure containing the parsed information."
  (save-excursion
    (let ((parse-buffer (generate-new-buffer " *xml*"))
	  (proc nil)
	  (marker (make-marker))
	  (done nil))
      (if buffer (set-buffer buffer))
      (unwind-protect
	  (progn
	    (setq proc (start-process "xml" parse-buffer xml-parsing-program "-"))
	    (if (not (eq (process-status proc) 'run))
		(error "Could not start parsing process"))
	    (set-process-sentinel proc (lambda (proc string) (setq done t)))
	    (accept-process-output proc 0.5)
	    (process-send-string proc (buffer-substring (point-min) (point-max)))
	    (process-send-eof proc)
	    (while (not done)
	      (accept-process-output proc 1))
	    (set-buffer parse-buffer)
	    (goto-char (point-min))
	    (read parse-buffer))
	(kill-buffer parse-buffer)))))
	
