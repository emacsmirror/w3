;;; w3-compat.el --- Compatibility stubs

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file defines aliases or stub functions starting with `w3-', to
;; compensate for the possible nonexistence of needed functions.  The
;; prefix is used to avoid polluting the namespace and confusing other
;; packages.

;; Currently no attempt is made to support anything before Emacs 22.

;;; Code:

;; `url-basepath' was morphed into `url-file-directory' and
;; `url-file-nondirectory' post Emacs 22.
(cond
 ((fboundp 'url-file-nondirectory)
  (defalias 'w3-url-file-nondirectory 'url-file-directory))
 ((fboundp 'url-basepath)
  (defsubst w3-url-file-nondirectory (file)
    "Return the directory part of FILE, for a URL."
    (url-basepath file t)))
 (t
  (error "Couldn't define `w3-url-file-nondirectory'")))

(provide 'w3-compat)
;;; w3-compat.el ends here
