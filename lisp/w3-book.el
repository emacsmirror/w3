;;; w3-book.el --- Bookmark abstraction
;; Author: $Author: wmperry $
;; Created: $Date: 1998/12/01 22:12:10 $
;; Version: $Revision: 1.1 $
;; Keywords: menu, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997, 1998 Free Software Foundation, Inc.
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
(require 'cl)

(defstruct bookmark
  location				; The URL
  label					; short description
  description				; A longer description
  added					; Date added
  visited				; Date last visited
  annotations				; Any annotations
  children				; Subnodes
  ;; These are really only relevant to HTTP requests
  method				; What HTTP method was used
  headers				; Extra HTTP headers
  data					; Any extra data sent in the request
  )

(defun w3-bookmark-to-menu (bookmark)
  (if (bookmark-children bookmark)
      (cons (or (bookmark-label bookmark) "...")
	    (mapcar 'w3-bookmark-to-menu (bookmark-children bookmark)))
    (vector (or (bookmark-label bookmark)
		(bookmark-location bookmark))
	    (list 'w3-bookmark-fetch bookmark)
	    (bookmark-location bookmark))))

(defun w3-bookmark-fetch (bookmark)
  (interactive)
  (let ((url-request-method (bookmark-method bookmark))
	(url-request-data (bookmark-data bookmark))
	(url-request-extra-headers (bookmark-headers bookmark)))
    (w3-fetch (bookmark-location bookmark))))

(defun w3-bookmark-delete (bookmark)
  (interactive)
  )

(defun w3-bookmark-add ()
  (interactive)
  )

(defun w3-bookmark-browse ()
  (interactive)
  )


;; HTML Bookmarks
(defun w3-bookmark-flatten-list (list)
  (cond
   ((atom list) (list list))
   ((and (cdr list) (atom (cdr list))) (list list))
   (t (apply 'append (mapcar 'w3-bookmark-flatten-list list)))))

(defun w3-bookmark-html-strip-strings (tree)
  (setq tree (w3-bookmark-flatten-list tree))
  (w3-normalize-spaces
   (mapconcat (function (lambda (n) (if (stringp n) n ""))) tree "")))

(defmacro w3-bookmark-html-handle-content (node)
  (`
   (progn
     (push content stack)
     (setq content (nth 2 (, node))))))

(defmacro w3-bookmark-html-handle-empty-tag ()
  (`
   (progn
     (push content stack)
     (setq content nil))))

(defun w3-bookmark-read-html-internal (tree)
  (let (tag attr node content stack last bkmrk bkmrk-stack tag-stack)
    (setq stack (list tree))
    (while stack
      (setq content (pop stack))
      (case (pop tag-stack)
	((dl ol ul dir menu)
	 (debug)
	 (pop bkmrk-stack)))
      (while content
	(setq node (pop content))
	(if (not (stringp node))
	    (setq tag (nth 0 node)
		  attr (nth 1 node))
	  (setq tag '%text))
	(push tag tag-stack)
	(case tag
	  (%text
	   (w3-bookmark-html-handle-empty-tag))
	  (title
	   (setq bkmrk (make-bookmark :location nil
				      :label (w3-bookmark-html-strip-strings
					      (nth 2 node))))
	   (w3-bookmark-html-handle-empty-tag))
	  (a				; Marks a link
	   (let ((n (make-bookmark :location (cdr-safe (assq 'href attr))
				   :label (or (cdr-safe (assq 'title attr))
					      (w3-bookmark-html-strip-strings
					       (nth 2 node)))
				   :added (cdr-safe (assq 'add_date attr))
				   :visited (cdr-safe (assq 'last_visit attr)))))
	     (if bkmrk-stack
		 (push n (bookmark-children (car bkmrk-stack)))
	       (push n (bookmark-children bkmrk))))
	   (w3-bookmark-html-handle-empty-tag))
	  ((dl ol ul dir menu)		; Marks a parent node
	   (push (make-bookmark :location nil
				:label "submenu") bkmrk-stack)
	   (push (car bkmrk-stack) (bookmark-children bkmrk))
	   (w3-bookmark-html-handle-content node))
	  ((li dt)			; Marks a description block
	   (w3-bookmark-html-handle-content node))
	  (otherwise
	   (w3-bookmark-html-handle-content node)))))
    bkmrk))

(defun w3-bookmark-read-html (url)
  "Import an HTML file into the Emacs-w3 format."
  (interactive "fBookmark file: ")
  (if (not (file-readable-p url))
      (error "Can not read %s..." url))
  (save-excursion
    (set-buffer (get-buffer-create " *bookmark-work*"))
    (erase-buffer)
    (insert-file-contents url)
    (let* ((w3-debug-html nil)
	   (bkmarks nil)
	   (parse (w3-parse-buffer (current-buffer))))
      (setq bkmarks (w3-bookmark-read-html-internal parse)
	    w3-html-bookmarks (w3-bookmark-to-menu bkmarks)))))

(defun w3-bookmark-write-html (fname)
  )

(provide 'w3-book)
