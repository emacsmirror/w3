;;; url-hash.el --- Hashtable functions
;; Author: $Author: wmperry $
;; Created: $Date: 1998/12/01 22:12:09 $
;; Version: $Revision: 1.1 $
;; Keywords: lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995,1996 by William M. Perry <wmperry@cs.indiana.edu>
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

;; Hash tables
(eval-when-compile
  (require 'cl)
  (load-library "cl-extra"))

(cond
 ((and (fboundp 'maphash) (subrp (symbol-function 'maphash)))
    ;; Builtins!
  (defun url-puthash (key val table)
    (let ((sym (if (stringp key) (intern key) key)))
      (puthash sym val table)))

  (defun url-gethash (key table &optional default)
    (let ((sym (if (stringp key) (intern-soft key) key)))
      (if (not sym)
	  default
	(gethash sym table))))

  (mapcar (function
	   (lambda (sym)
	     (let ((new-sym (intern (format "url-%s" sym))))
	       (defalias new-sym sym))))
	  '(make-hashtable 
	    make-key-weak-hashtable
	    make-value-weak-hashtable
	    make-weak-hashtable
	    hashtablep
	    clrhash
	    maphash
	    copy-hashtable)))
 (t
  (defconst url-hashtable-primes
    '(13 29 37 47 59 71 89 107 131 163 197 239 293 353 431 521 631 761 919
	 1103 1327 1597 1931 2333 2801 3371 4049 4861 5839 7013 8419 10103
	 12143 14591 17519 21023 25229 30293 36353 43627 52361 62851 75431
	 90523 108631 130363 156437 187751 225307 270371 324449 389357 467237
	 560689 672827 807403 968897 1162687 1395263 1674319 2009191 2411033
	 2893249)
    "A list of some good prime #s to use as  sizes for hashtables.")

  (defun url-make-hashtable (size)
    "Make a hashtable of initial size SIZE"
    (if (not size) (setq size 37))
    (if (not (memq size url-hashtable-primes))
	;; Find a suitable prime # to use as the hashtable size
	(let ((primes url-hashtable-primes))
	  (while (<= (car primes) size)
	    (setq primes (cdr primes)))
	  (setq size (car primes))))
    (make-vector (or size 2893249) 0))

  (fset 'url-make-key-weak-hashtable 'url-make-hashtable)
  (fset 'url-make-value-weak-hashtable 'url-make-hashtable)
  (fset 'url-make-weak-hashtable 'url-make-hashtable)

  (defun url-hashtablep (obj)
    "Return t if OBJ is a hashtable, else nil."
    (vectorp obj))

  (defun url-puthash (key val table)
    "Hash KEY to VAL in TABLE."
    (let ((sym (intern (if (stringp key) key (prin1-to-string key)) table)))
      (put sym 'val val)
      (put sym 'key key)))

  (defun url-gethash (key table &optional default)
    "Find hash value for KEY in TABLE.
If there is no corresponding value, return DEFAULT (defaults to nil)."
    (let ((sym (intern-soft (if (stringp key)
				key
			      (prin1-to-string key)) table)))
      (or (and sym (get sym 'val))
	  default)))

  (put 'url-gethash 'sysdep-defined-this t)

  (defun url-clrhash (table)
    "Flush TABLE"
    (fillarray table 0))

  (defun url-maphash (function table)
    "Map FUNCTION over entries in TABLE, calling it with two args,
each key and value in the table."
    (mapatoms
     (function
      (lambda (sym)
	(funcall function (get sym 'key) (get sym 'val)))) table))

  (defun url-copy-hashtable (old-table)
    "Make a new hashtable which contains the same keys and values
as the given table.  The keys and values will not themselves be copied."
    (copy-sequence old-table))
  ))

(provide 'url-hash)
