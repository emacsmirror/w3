;;; w3-xem20.el -- XEmacs 20.0 with Mule specific functions
;; Author: MORIOKA Tomohiko
;; Created: $Date: 1998/12/01 22:12:11 $
;; Version: $Revision: 1.1 $
;; Keywords: faces, help, i18n, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by MORIOKA Tomohiko
;;;
;;; This file is part of GNU Emacs.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing a mule buffer as postscript.  Requires m2ps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mule-sysdp)

(defun w3-m2ps-buffer (&optional buffer)
  "Print a buffer by passing it through m2ps and lpr."
  (or buffer (setq buffer (current-buffer)))
  (let ((x (save-excursion (set-buffer buffer) tab-width)))
    (save-excursion
      (set-buffer (get-buffer-create " *mule-print*"))
      (erase-buffer)
      (insert-buffer buffer)
      (if (/= x tab-width)
	  (progn
	    (setq tab-width x)
	    (message "Converting tabs")
	    (untabify (point-min) (point-max))))
      (setq file-coding-system *internal*)
      (shell-command-on-region (point-min) (point-max)
			       "m2ps | lpr" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multi-Lingual Emacs (MULE) Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar attributed-region nil
  "Bogus definition to get rid of compile-time warnings.")

(defun w3-inhibit-code-conversion (proc buf)
  "Inhibit Mule's subprocess PROC from code converting in BUF."
  (save-excursion
    (set-buffer buf)
    (setq mc-flag nil))
  (set-process-input-coding-system proc mule-no-coding-system)
  (set-process-output-coding-system proc mule-no-coding-system))

(defvar w3-mime-list-for-code-conversion
  '("text/plain" "text/html")
  "List of MIME types that require Mules' code conversion.")
(make-variable-buffer-local 'w3-mime-list-for-code-conversion)

(defun w3-convert-code-for-mule (mmtype)
  "Convert current data into the appropriate coding system"
  (and (or (not mmtype) (member mmtype w3-mime-list-for-code-conversion))
       (let* ((c (detect-coding-region (point-min) (point-max)))
	      (code (or (and (listp c) (car c)) c)))
	 (setq mc-flag t)
	 (decode-coding-region (point-min) (point-max) code)
	 (set-file-coding-system code)
	 )))

(provide 'w3-xem20)
