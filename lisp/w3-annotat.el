;;; w3-annotat.el --- Annotation functions for Emacs-W3
;; Author: $Author: wmperry $
;; Created: $Date: 1998/12/01 22:12:12 $
;; Version: $Revision: 1.1 $
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1998 Free Software Foundation, Inc.
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
;;; Private annotation support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-personal-annotations ()
  ;; Read in personal annotation file
  (if (and
       (file-exists-p (format "%s/LOG" w3-personal-annotation-directory))
       (file-readable-p (format "%s/LOG" w3-personal-annotation-directory)))
      (save-excursion
	(setq w3-personal-annotations nil);; nuke the old list
	(let ((start nil)
	      (end nil)
	      (txt nil)
	      (url nil)
	      (num nil))
	  (set-buffer (get-buffer-create " *panno*"))
	  (erase-buffer)
	  (insert-file-contents-literally
	   (format "%s/LOG" w3-personal-annotation-directory))
	  (goto-char (point-min))
	  (w3-replace-regexp "\n+" "\n")
	  (goto-char (point-min))
	  ;; nuke the header lines
	  (delete-region (point-min) (progn (forward-line 2) (point)))
	  (cond
	   ((eobp) nil)			; Empty LOG file
	   (t
	    (if (/= (char-after (1- (point-max))) ?\n)
		(save-excursion
		  (goto-char (point-max))
		  (insert "\n")))
	    (while (not (eobp))
	      (setq start (point)
		    end (prog2 (end-of-line) (point) (forward-char 1))
		    txt (buffer-substring start end)
		    url (substring txt 0 (string-match " " txt))
		    num (url-split
			 (substring txt (1+ (string-match " " txt)) nil)
			 "[ \t]"))
	      (while num
		(setq w3-personal-annotations
		      (cons
		       (list url
			     (list (car (car num))
				   (w3-grok-annotation-format
				    (car (car num)))))
		       w3-personal-annotations)
		      num (cdr num))))))
	  (kill-buffer " *panno*")))))

(defun w3-grok-annotation-format (anno)
  ;; Grab the title from an annotation
  (let ((fname  (format "%s/PAN-%s.html"
			w3-personal-annotation-directory anno)))
    (save-excursion
      (set-buffer (get-buffer-create " *annotmp*"))
      (erase-buffer)
      (if (file-exists-p fname)
	  (insert-file-contents-literally fname))
      (goto-char (point-min))
      (prog1
	  (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
	      (buffer-substring (match-beginning 1) (match-end 1))
	    (concat "Annotation on "
		    (current-time-string (nth 5 (file-attributes fname)))))
	(kill-buffer " *annotmp*")))))

(defun w3-is-personal-annotation (url)
  ;; Is URL a personal annotation?
  (string-match "file:/.*/PAN-.*\\.html" url))

(defun w3-delete-personal-annotation-internal (url num)
  (save-excursion
    (set-buffer (get-buffer-create " *annotmp*"))
    (erase-buffer)
    (insert-file-contents-literally (format "%s/LOG"
					    w3-personal-annotation-directory))
    (replace-regexp (format "[ \t]+\\b%s\\b[ \t]*" num) " ")
    (goto-char (point-min))
    (delete-matching-lines (format "^%s +$" url))
    (let ((make-backup-files nil)
	  (version-control nil)
	  (require-final-newline t))
      (write-region (point-min) (point-max)
		    (format "%s/LOG"
			    w3-personal-annotation-directory)))
    (kill-buffer " *annotmp*")
    (let ((anno w3-personal-annotations))
      (setq w3-personal-annotations nil)
      (while anno
	(if (not (string= num (car (car (cdr (car anno))))))
	    (setq w3-personal-annotations
		  (cons (car anno) w3-personal-annotations)))
	(setq anno (cdr anno)))
      (delete-file (format "%s/PAN-%s.html"
			   w3-personal-annotation-directory num)))))

(defun w3-delete-personal-annotation ()
  "Delete a personal annotation."
  (interactive)
  (let ((url (url-view-url t)))
    (cond
     ((w3-is-personal-annotation (url-view-url t))
      (let ((num nil)
	    (annotated-url nil)
	    (anno w3-personal-annotations))
	(string-match "file:/.*/PAN-\\(.*\\)\\.html" url)
	(setq num (match-string 1 url))
	(while anno
	  (if (equal num (car (car (cdr (car anno)))))
	      (setq annotated-url (car (car anno))))
	  (setq anno (cdr anno)))
	(if (not annotated-url)
	    (message "Couldn't find url that this is annotating!")
	  (w3-delete-personal-annotation-internal annotated-url num)
	  (w3-quit))))
     (t
      (let* ((tmp w3-personal-annotations)
	     (thelist nil)
	     (node nil)
	     (todel nil))
	(if (not (assoc url tmp))
	    (message "No personal annotations.")
	  (while tmp
	    (setq node (car tmp))
	    (if (string= (car node) url)
		(setq thelist (cons (cons (nth 1 (nth 1 node)) "") thelist)))
	    (setq tmp (cdr tmp)))
	  (setq todel (completing-read "Delete annotation: " thelist nil t))
	  ;; WORK ;;
	  (message "I should delete %s, but can't." todel)))))))

(defun w3-personal-annotation-add ()
  "Add an annotation to this document."
  (interactive)
  (let ((url (url-view-url t))
	(buf (get-buffer-create "*Personal Annotation*"))
	(title (read-string "Title: "
			    (format "Annotation by %s on %s"
				    (user-real-login-name)
				    (current-time-string)))))
    (set-buffer buf)
    (switch-to-buffer buf)
    (erase-buffer)
    (if (and w3-annotation-mode (fboundp w3-annotation-mode))
	(funcall w3-annotation-mode)
      (message "%S is undefined, using %s" w3-annotation-mode
	       default-major-mode)
      (funcall default-major-mode))
    (w3-annotation-minor-mode 1)
    (setq w3-current-annotation (cons url title))
    (insert "<html>\n"
	    " <head>\n"
	    "  <title>" (url-insert-entities-in-string title) "</title>"
	    " </head>\n"
	    "  <h1>" (url-insert-entities-in-string title) "</h1>\n"
	    "  <p>\n"
	    "   <address>" (url-insert-entities-in-string (user-full-name))
            (if (stringp url-personal-mail-address)
                (concat " &lt;" (url-insert-entities-in-string
			      url-personal-mail-address) "&gt;")
              "")
	    "</address>\n"
	    "   <address>" (current-time-string) "</address>\n"
	    "  </p>\n"
	    "  <pre>\n")
    (save-excursion
      (insert "\n\n\n  </pre>\n"
	      "</html>"))
    (message "Hit C-cC-c to send this annotation.")))

(defun w3-annotation-minor-mode (&optional arg)
  "Minimal minor mode for entering annotations.  Just rebinds C-cC-c to
finish the annotation."
  (interactive "P")
  (cond
   ((null arg) (setq w3-annotation-minor-mode (not w3-annotation-minor-mode)))
   ((= 0 arg)  (setq w3-annotation-minor-mode nil))
   (t          (setq w3-annotation-minor-mode t)))
  )

(defun w3-annotation-find-highest-number ()
  ;; Find the highest annotation number in this buffer
  (let (x)
    (goto-char (point-min))
    (while (re-search-forward "[^ \t\n]*[ \t]\\(.*\\)" nil t)
      (setq x (nconc (mapcar (function (lambda (x) (string-to-int (car x))))
			     (url-split (buffer-substring (match-beginning 1)
							 (match-end 1))
				       "[ \t]")) x)))
    (if (not x) (setq x '(0)))
    (1+ (car (sort x '>)))))

(defun w3-personal-annotation-finish ()
  "Finish doing a personal annotation."
  (interactive)
  (if (or (not w3-personal-annotation-directory)
	  (not (file-exists-p w3-personal-annotation-directory))
	  (not (file-directory-p w3-personal-annotation-directory)))
      (error "No personal annotation directory!")
    (let ((url (car w3-current-annotation))
	  (txt (buffer-string))
	  (title (cdr w3-current-annotation))
	  (fname nil)
	  (num nil))
      (save-excursion
	(not-modified)
	(kill-buffer (current-buffer))
	(set-buffer (get-buffer-create " *annotmp*"))
	(erase-buffer)
	(if (file-exists-p		; Insert current LOG file if
					; it exists.
	     (format "%s/LOG" w3-personal-annotation-directory))
	    (insert-file-contents-literally
	     (format "%s/LOG" w3-personal-annotation-directory))
	  (progn			; Otherwise, create a file
	    (goto-char (point-min))	; that conforms to first
					; annotation format from NCSA
	    (insert "ncsa-mosaic-personal-annotation-log-format-1\n")
	    (insert "Personal\n")))
	(goto-char (point-min))
	(setq num (int-to-string (w3-annotation-find-highest-number))
	      fname (format "%s/PAN-%s.html"
			    w3-personal-annotation-directory num))
	(goto-char (point-min))
	(if (re-search-forward (regexp-quote url) nil t)
	    (progn
	      (end-of-line)
	      (insert " "))
	  (goto-char (point-max))
	  (insert "\n" url " "))
	(insert num)
	(let ((make-backup-files nil)
	      (version-control nil)
	      (require-final-newline t))
	  (write-region (point-min) (point-max)
			(format "%s/LOG" w3-personal-annotation-directory))
	  (erase-buffer)
	  (insert w3-annotation-marker txt)
	  (write-region (point-min) (point-max) fname))
	(setq w3-personal-annotations
	      (cons (list url (list num title)) w3-personal-annotations))))))

(defun w3-annotation-add ()
  "Add an annotation to the current document."
  (interactive)
  (w3-personal-annotation-add))
