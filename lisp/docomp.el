;;; First things first - if they do not have the WIDGETDIR environment
;;; variable set, choke, scream, and die.
(if (or (not (getenv "WIDGETDIR"))
	(string= (getenv "WIDGETDIR") "no"))
    (progn
      (message "Could not find custom libraries.")
      (message "Please rerun `configure' in the top level directory, and")
      (message "provide the `--with-custom=XXX' flag.")
      (kill-emacs 1)))

(setq load-path (append (list (expand-file-name 
			       (or (getenv "W3SRCDIR") "./"))
			      "."
			      (or (getenv "WIDGETDIR")
				  (expand-file-name "../widget"))
			      (or (getenv "GNUSDIR")
				  (expand-file-name "../gnus"))
			      )
			load-path))

(setq max-specpdl-size (* 10 max-specpdl-size)
      max-lisp-eval-depth (* 10 max-lisp-eval-depth))

(defun w3-declare-variables (&rest args)
  (while args
    (eval (list 'defvar (car args) nil ""))
    (setq args (cdr args))))

;; For Emacs 19
(w3-declare-variables 'track-mouse 'menu-bar-help-menu 'menu-bar-mode
		      'global-face-data)

;; For XEmacs/Lucid
(w3-declare-variables 'current-menubar 'default-menubar 'extent
		      'mode-motion-hook 'mode-popup-menu 'sound-alist
		      'menubar-visible-p
		      'inhibit-help-echo 'default-toolbar
		      'bottom-toolbar-height 'top-toolbar-height
		      'toolbar-buttons-captioned-p
		      'right-toolbar-width 'left-toolbar-width
		      'top-toolbar 'bottom-toolbar 'right-toolbar
		      'left-toolbar 'device-fonts-cache
		      'has-modeline-p 'baud-rate)

;; For MULE
(w3-declare-variables '*noconv* '*autoconv* '*euc-japan* '*internal*
		      'w3-mime-list-for-code-conversion 'lc-ltn1
		      'mule-version 'enable-multibyte-characters
		      'mc-flag 'charset-latin-iso8859-1
		      'default-enable-multibyte-characters
		      'buffer-file-coding-system
		      'file-coding-system-for-read 'file-coding-system)

;; For TM
(w3-declare-variables 'mime/editor-mode-flag 'mime-tag-format)
			  
;; For NNTP
(w3-declare-variables 'nntp-server-buffer 'nntp-server-process 'nntp/connection
		      'gnus-nntp-server 'nntp-server-name 'nntp-version
		      'gnus-default-nntp-server)

;; For xpm-button
(w3-declare-variables 'x-library-search-path)

;; For emacspeak
(w3-declare-variables 'dtk-voice-table 'dtk-punctuation-mode)

;; For a few internal things
(w3-declare-variables 'tag 'w3-working-buffer 'proxy-info 'args
		      'w3-image-widgets-waiting 'w3-form-info
		      'w3-last-parse-tree 'command-line-args-left
		      'standard-display-table 'w3-html-bookmarks
		      'browse-url-browser-function 'widget-keymap)

;; GNUS
(w3-declare-variables 'gnus-group-buffer 'gnus-version)		      

;; If we are building w3 in a different directory than the source
;; directory, we must read *.el from source directory and write *.elc
;; into the building directory.  For that, we define this function
;; before loading bytecomp.  Bytecomp doesn't overwrite this function.
(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name.
 In addition, remove directory name part from FILENAME."
  (setq filename (byte-compiler-base-file-name filename))
  (setq filename (file-name-sans-versions filename))
  (setq filename (file-name-nondirectory filename))
  (if (memq system-type '(win32 w32 mswindows windows-nt))
      (setq filename (downcase filename)))
  (cond ((eq system-type 'vax-vms)
 	 (concat (substring filename 0 (string-match ";" filename)) "c"))
 	((string-match emacs-lisp-file-regexp filename)
 	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
 	(t (concat filename ".elc"))))

(require 'bytecomp)

;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars)
      byte-optimize t)

;; This is part of bytecomp.el in 19.35: Without it, any defvar'd
;; variables show up as 'free variables' to the byte compiler, which
;; is bogus.
(if (not (get 'custom-declare-variable 'byte-hunk-handler))
    (progn
      (put 'custom-declare-variable 'byte-hunk-handler
	   'byte-compile-file-form-custom-declare-variable)
      (defun byte-compile-file-form-custom-declare-variable (form)
	(if (memq 'free-vars byte-compile-warnings)
	    (setq byte-compile-bound-variables
		  (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
	form)))

(defun compile-it ()
  (let ((files (directory-files "." t ".*.[eE][lL]$" nil)))
    (while files
      (if (and (not (file-directory-p (car files)))
	       (not (string-match "w3-sysdp.el$" (car files))))
	  (byte-compile-file (car files)))
      (setq files (cdr files)))))

(defun emacs-build-autoloads (dir autofile)
  (require 'autoload)
  (let ((files (directory-files dir t ".*.[eE][lL]$" nil)))
    (save-excursion
      (find-file autofile)
      (erase-buffer)
      (mapcar 'generate-file-autoloads files)
      (goto-char (point-max))
      (insert "\n(provide 'w3-autoloads)\n")
      (save-buffer)
      (kill-buffer (current-buffer))))

  ;; Now we need to munge that file to deal with
  (find-file "w3-auto.el")
  (erase-buffer)
  (insert-file-contents autofile)
  (goto-char (point-min))
  (while (re-search-forward "w3-autoloads" nil t)
      (replace-match "w3-auto"))
  (save-buffer)
  (kill-buffer (current-buffer))
  (kill-emacs))

(defun emacs-batch-build-autoloads ()
  (emacs-build-autoloads (nth 0 command-line-args-left)
			 (nth 1 command-line-args-left)))

(defun emacs-build-custom-load (dir)
  (let ((foundit t))
    (save-excursion
      (condition-case ()
	  (load-library "cus-dep")
	(error (setq foundit nil)))
      (if foundit
	  (let ((command-line-args-left (list dir)))
	    (custom-make-dependencies))
	(write-region "\n" nil "cus-load.el")))))

(defun emacs-batch-build-custom-load ()
  (emacs-build-custom-load (car command-line-args-left)))

(provide 'w3-auto)
(autoload 'w3-load-flavors "w3")

(w3-load-flavors)
(w3-setup-version-specifics)

(require 'cl)
(require 'w3-sysdp)
(require 'w3-vars)
(require 'url)
(require 'mm)
