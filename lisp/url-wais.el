;;; url-wais.el --- WAIS Uniform Resource Locator retrieval code
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
;;; WAIS support
;;; ------------
;;; Here are even more gross hacks that I call native WAIS support.
;;; This code requires a working waisq program that is fully
;;; compatible with waisq from think.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-create-wais-source (server port dbase)
  ;; Create a temporary wais source description file.  Returns the
  ;; file name the description is in.
  (let ((x (url-generate-unique-filename))
	(y (get-buffer-create " *waisq-tmp*")))
    (save-excursion
      (set-buffer y)
      (erase-buffer)
      (insert 
       (format
	(concat "(:source\n:version 3\n"
		":ip-name \"%s\"\n:tcp-port %s\n"
		":database-name \"%s\"\n)")
	server (if (equal port "") "210" port) dbase))
      (write-region (point-min) (point-max) x nil nil)
      (kill-buffer y))
    x))

(defun url-wais-stringtoany (str)
  ;; Return a wais subelement that specifies STR in any database
  (concat "(:any :size " (length str) " :bytes #( "
	  (mapconcat 'identity str " ")
	  " ) )"))

;(defun url-retrieve-wais-docid (server port dbase local-id)
;  (call-process "waisretrieve" nil url-working-buffer nil
;		(format "%s:%s@%s:%s" (url-unhex-string local-id)
;			dbase server port)))

;(url-retrieve-wais-docid "quake.think.com" "210" "directory-of-servers"
;			"0 2608 /proj/wais/wais-sources/vpiej-l.src")
(defun url-retrieve-wais-docid (server port dbase local-id)
  ;; Retrieve a wais document.
  ;; SERVER is the server the database is on (:ip-name in source description)
  ;; PORT is the port number to contact (:tcp-port in the source description)
  ;; DBASE is the database name (:database-name in the source description)
  ;; LOCAL-ID is the document (:original-local-id in the question description)
  (let* ((dbf (url-create-wais-source server port dbase))
	 (qstr (format
		(concat "(:question :version 2\n"
			"           :result-documents\n"
			"           ( (:document-id\n"
			"              :document\n"
			"              (:document\n"
			"               :headline \"\"\n"
			"               :doc-id\n"
			"               (:doc-id :original-database %s\n"
			"                :original-local-id %s )\n"
			"               :number-of-bytes -1\n"
			"               :type \"\"\n"
			"               :source\n"
			"               (:source-id :filename \"%s\") ) ) ) )")
		(url-wais-stringtoany dbase)
		(url-wais-stringtoany (url-unhex-string local-id))
		dbf))
	 (qf (url-generate-unique-filename)))
    (set-buffer (get-buffer-create url-working-buffer))
    (insert qstr)
    (write-region (point-min) (point-max) qf nil nil)
    (erase-buffer)
    (call-process url-waisq-prog nil url-working-buffer nil "-f" qf "-v" "1")
    (save-excursion
      (set-buffer url-working-buffer)
      (setq url-current-file (url-unhex-string local-id)))
    (condition-case ()
	(delete-file dbf)
      (error nil))
    (condition-case ()
	(delete-file qf)
      (error nil))))

;(url-perform-wais-query "quake.think.com" "210" "directory-of-servers" "SGML")
(defun url-perform-wais-query (server port dbase search)
  ;; Perform a wais query.
  ;; SERVER is the server the database is on (:ip-name in source description)
  ;; PORT is the port number to contact (:tcp-port in the source description)
  ;; DBASE is the database name (:database-name in the source description)
  ;; SEARCH is the search term (:seed-words in the question description)"
  (let ((dbfname (url-create-wais-source server port dbase))
	(qfname (url-generate-unique-filename))
	(results 'url-none-gotten))
    (save-excursion
      (url-clear-tmp-buffer)
      (insert
       (format
	(concat "(:question\n"
		" :version 2\n"
		" :seed-words \"%s\"\n"
		" :sourcepath \"" url-temporary-directory "\"\n"
		" :sources\n"
		" (  (:source-id\n"
		"     :filename \"%s\"\n"
		"    )\n"
		" )\n"
		" :maximum-results 100)\n")
	search dbfname))
      (write-region (point-min) (point-max) qfname nil nil)
      (erase-buffer)
      (call-process url-waisq-prog nil url-working-buffer nil "-g" "-f" qfname)
      (set-buffer url-working-buffer)
      (erase-buffer)
      (setq url-current-server server
	    url-current-port port
	    url-current-file dbase)
      (insert-file-contents-literally qfname)
      (goto-char (point-min))
      (if (re-search-forward "(:question" nil t)
	  (delete-region (point-min) (match-beginning 0)))
      (url-replace-regexp "Process.*finished.*" "")
      (subst-char-in-region (point-min) (point-max) 35 32)
      (goto-char (point-min))
      (message "Done reading info - parsing results...")
      (if (re-search-forward ":result-documents[^(]+" nil t)
	  (progn
	    (goto-char (match-end 0))
	    (while (eq results 'url-none-gotten)
	      (condition-case ()
		  (setq results (read (current-buffer)))
		(error (progn
			 (setq results 'url-none-gotten)
			 (goto-char (match-end 0))))))
	    (erase-buffer)
	    (insert "<title>Results of WAIS search</title>\n"
		    "<h1>Searched " dbase " for " search "</h1>\n"
		    "<hr>\n"
		    "Found <b>" (int-to-string (length results))
		    "</b> matches.\n"
		    "<ol>\n<li>"
		    (mapconcat 'url-parse-wais-doc-id results "\n<li>")
		    "\n</ol>\n<hr>\n"))
	(message "No results"))
      (setq url-current-mime-type "text/html")
      (condition-case ()
	  (delete-file qfname)
	(error nil))
      (condition-case ()
	  (delete-file dbfname)
	(error nil)))))

(defun url-wais-anytostring (x)
  ;; Convert a (:any ....) wais construct back into a string.
  (mapconcat 'char-to-string (car (cdr (memq ':bytes x))) ""))

(defun url-parse-wais-doc-id (x)
  ;; Return a list item that points at the doc-id specified by X
  (let* ((document (car (cdr (memq ':document x))))
	 (doc-id (car (cdr (memq ':doc-id document))))
	 (score (car (cdr (memq ':score x)))) 
	 (title (car (cdr (memq ':headline document))))
	 (type (car (cdr (memq ':type document))))
	 (size (car (cdr (memq ':number-of-bytes document))))
	 (server (car (cdr (memq ':original-server doc-id))))
	 (dbase (car (cdr (memq ':original-database doc-id))))
	 (localid (car (cdr (memq ':original-local-id doc-id))))
	 (dist-server (car (cdr (memq ':distributor-server doc-id))))
	 (dist-dbase (car (cdr (memq ':distributor-database doc-id))))
	 (dist-id (car (cdr (memq ':distributor-local-id doc-id))))
	 (copyright (or (car (cdr (memq ':copyright-disposition doc-id))) 0)))
    (format "<a href=\"wais://%s:%s/%s/%s/%d/1=%s;2=%s;3=%s;4=%s;5=%s;6=%s;7=%d;\">%s (Score = %s)</a>"
	    url-current-server url-current-port url-current-file
	    type size
	    (url-hexify-string (url-wais-anytostring server))
	    (url-hexify-string (url-wais-anytostring dbase))
	    (url-hexify-string (url-wais-anytostring localid))
	    (url-hexify-string (url-wais-anytostring dist-server))
	    (url-hexify-string (url-wais-anytostring dist-dbase))
	    (url-hexify-string (url-wais-anytostring dist-id))
	    copyright title score)))

(defun url-grok-wais-href (url)
  "Return a list of server, port, database, search-term, doc-id"
  (if (string-match "wais:/+\\([^/:]+\\):*\\([^/]*\\)/+\\(.*\\)" url)
      (let ((host (url-match url 1))
	    (port (url-match url 2))
	    (data (url-match url 3)))
	(list host port data))
    (make-list 3 nil)))

(defun url-wais (url)
  ;; Retrieve a document via WAIS
  (if (and url-wais-gateway-server url-wais-gateway-port)
      (url-retrieve
       (format "http://%s:%s/%s"
	       url-wais-gateway-server
	       url-wais-gateway-port
	       (substring url (match-end 0) nil)))
    (let ((href (url-grok-wais-href url)))
      (url-clear-tmp-buffer)
      (setq url-current-type "wais"
	    url-current-server (nth 0 href)
	    url-current-port (nth 1 href)
	    url-current-file (nth 2 href))
      (cond
       ((string-match "2=\\(.*\\);3=\\([^ ;]+\\)" (nth 2 href)); full link
	(url-retrieve-wais-docid (nth 0 href) (nth 1 href)
				(url-match (nth 2 href) 1)
				(url-match (nth 2 href) 2)))
       ((string-match "\\([^\\?]+\\)\\?\\(.*\\)" (nth 2 href)) ; stored query
	(url-perform-wais-query (nth 0 href) (nth 1 href)
			       (url-match (nth 2 href) 1)
			       (url-match (nth 2 href) 2)))
       (t
	(insert "<title>WAIS search</title>\n"
		"<h1>WAIS search of " (nth 2 href) "</h1>"
		"<hr>\n"
		(format "<form action=\"%s\" enctype=\"application/x-w3-wais\">\n" url)
		"Enter search term: <input name=\"internal-wais\">\n"
		"</form>\n"
		"<hr>\n"))))))

(provide 'url-wais)

