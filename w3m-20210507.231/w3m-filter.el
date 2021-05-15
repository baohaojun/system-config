;;; w3m-filter.el --- filtering utility of advertisements on WEB sites

;; Copyright (C) 2001-2008, 2012-2015, 2017-2021
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; w3m-filter.el is the add-on utility to filter advertisements on WEB
;; sites.


;;; Code:

(provide 'w3m-filter)

(require 'w3m)

(defcustom w3m-filter-configuration
  `((t
     ("Strip Google's click-tracking code from link urls"
      "Google の click-tracking コードをリンクの url から取り除きます")
     "\\`https?://[a-z]+\\.google\\."
     w3m-filter-google-click-tracking)
    (t
     ("Align table columns vertically to shrink the table width in Google"
      "Google 検索結果のテーブルを縦方向で揃えて幅を狭めます")
     "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\."
     w3m-filter-google-shrink-table-width)
    (t
     ("Add name anchors that w3m can handle in all pages"
      "すべてのページに w3m が扱える name アンカーを追加します")
     ""
     w3m-filter-add-name-anchors)
    (t
     ("Substitute disabled attr with readonly attr in forms"
      "フォーム中の disabled 属性を readonly 属性で代用します")
     ""
     w3m-filter-subst-disabled-with-readonly)
    (nil
     ("Render <tfoot>...</tfoot> after <tbody>...</tbody>"
      "テーブル内の <tfoot> を <tbody> の後に描画します")
     ""
     w3m-filter-fix-tfoot-rendering)
    (t
     "Filter top and bottom cruft for rt.com"
     "\\`https://www.rt\\.com/"
     w3m-filter-rt)
    (t
     "Filter for slashdot"
     "\\`https?://\\([a-z]+\\.\\)?slashdot\\.org/"
     w3m-filter-slashdot)
    (t
     "GNOME Bugzilla - ignore <pre class=\"bz_comment_text\">
   in order to fold long lines"
     "\\`https://bugzilla\\.gnome\\.org/show_bug\\.cgi\\?id="
     w3m-filter-gnome-bugzilla)
    (nil
     "Generic page header filter"
     "\\`https?://"
     w3m-filter-generic-page-header)
    (t
     "Filter top and bottom cruft for stackexchange.com"
     "\\`https://\\(?:[0-9A-Za-z_~-]+\\.\\)*stackexchange\\.com\\(?:\\'\\|/\\)"
     w3m-filter-stackexchange)
    (nil
     ("Remove garbage in http://www.geocities.co.jp/*"
      "http://www.geocities.co.jp/* でゴミを取り除きます")
     "\\`http://www\\.geocities\\.co\\.jp/"
     w3m-filter-geocities-remove-garbage)
    (nil
     ("Remove ADV in http://*.hp.infoseek.co.jp/*"
      "http://*.hp.infoseek.co.jp/* で広告を取り除きます")
     "\\`http://[a-z]+\\.hp\\.infoseek\\.co\\.jp/"
     w3m-filter-infoseek-remove-ads)
    (nil
     ("Remove ADV in http://linux.ascii24.com/linux/*"
      "http://linux.ascii24.com/linux/* で広告を取り除きます")
     "\\`http://linux\\.ascii24\\.com/linux/"
     w3m-filter-ascii24-remove-ads)
    (nil
     "A filter for Google"
     "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\."
     w3m-filter-google)
    (nil
     "A filter for Amazon"
     "\\`https?://\\(?:www\\.\\)?amazon\\.\
\\(?:com\\|co\\.\\(?:jp\\|uk\\)\\|fr\\|de\\)/"
     w3m-filter-amazon)
    (nil
     ("A filter for Mixi.jp"
      "ミクシィ用フィルタ")
     "\\`https?://mixi\\.jp" w3m-filter-mixi)
    (nil
     "A filter for http://eow.alc.co.jp/*/UTF-8*"
     "\\`http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8" w3m-filter-alc)
    (nil
     "A filter for http://imepita.jp/NUM/NUM*"
     "\\`http://imepita\\.jp/[0-9]+/[0-9]+" w3m-filter-imepita)
    (nil
     "A filter for http://allatanys.jp/*"
     "\\`http://allatanys\\.jp/" w3m-filter-allatanys)
    (nil
     "A filter for Wikipedia"
     "\\`http://.*\\.wikipedia\\.org/" w3m-filter-wikipedia)
    (t "filter for github.com repository main page"
       "\\`http[s]?://github\\.com/[^/]+/[^/]+[/]?\\'"
       w3m-filter-github-repo-main-page)
    (t "xkcd filter" "\\`http[s]?://xkcd.com/" w3m-filter-xkcd)
    (nil
     ("Remove inline frames in all pages"
      "すべてのページでインラインフレームを取り除きます")
     ""
     w3m-filter-iframe)
    (t
     ("Prefer a lazy image specified with data-src= in img tags"
      "img タグ内の data-src= で指定される遅延画像を優先します")
     ""
     w3m-filter-prefer-lazy-images))
  "List of filter configurations applied to web contents.
Each filter configuration consists of the following form:

(FLAG DESCRIPTION REGEXP FUNCTION)

FLAG
  Non-nil means this filter is enabled.
DESCRIPTION
  Describe what this filter does.  The value may be a string or a list
  of two strings; in the latter case, those descriptions are written in
  English and Japanese in order respectively, and the unwanted one will
  be hidden according to `w3m-language' when customizing this variable.
REGEXP
  Regular expression to restrict this filter so as to run only on web
  contents of which the url matches.
FUNCTION
  Filter function to run on web contents.  The value may be a function
  or a list of a function and rest argument(s).  A function should take
  at least one argument, a url of contents retrieved then, as the first
  argument even if it is useless.  Use the latter (i.e. a function and
  arguments) if the function requires rest arguments."
  :group 'w3m
  :type '(repeat
	  :convert-widget w3m-widget-type-convert-widget
	  (let ((locker (lambda (fn)
			  `(lambda (&rest args)
			     (when (and (not inhibit-read-only)
					(eq (get-char-property (point) 'face)
					    'widget-inactive))
			       (when (and (not debug-on-error)
					  (eventp (cadr args))
					  (memq 'down
						(event-modifiers (cadr args))))
				 (setq before-change-functions
				       `((lambda (from to)
					   (setq before-change-functions
						 ',before-change-functions)))))
			       (error "The widget here is not active"))
			     (apply #',fn args)))))
	    `((group
	       :format "\n%v" :indent 0
	       :value-create
	       (lambda (widget)
		 (widget-group-value-create widget)
		 (unless (car (widget-value widget))
		   (let ((children (widget-get widget :children)))
		     (widget-specify-inactive
		      (cadr (widget-get widget :args))
		      (widget-get (car children) :to)
		      (widget-get (car (last children)) :to)))))
	       (checkbox
		:action
		(lambda (widget &optional event)
		  (let ((widget-edit-functions
			 (lambda (widget)
			   (let* ((parent (widget-get widget :parent))
				  (child (cadr (widget-get parent :args))))
			     (if (widget-value widget)
				 (progn
				   (widget-specify-active child)
				   (widget-put child :inactive nil))
			       (widget-specify-inactive
				child
				(widget-get widget :to)
				(widget-get
				 (car (last
				       (widget-get
					(car (last
					      (widget-get parent :children)))
					:children))) :to)))))))
		    (widget-checkbox-action widget event))))
	       (group
		:format "%v" :indent 2 :inline t
		(choice
		 :format " %v"
		 (string :format "%v")
		 (string :documentation-property
			 (lambda (value)
			   (if (equal "Japanese" w3m-language)
			       (concat (cadr value) "\n(" (car value) ")")
			     (concat (car value) "\n(" (cadr value) ")")))
			 :format "%h"
			 :match
			 (lambda (_widget value)
			   (and (stringp (car-safe value))
				(stringp (car-safe (cdr-safe value))))))
		 (const :format "Not documented\n" nil))
		(regexp :format "Regexp matching url: %v")
		(choice
		 :tag "Type" :format "Function %[Type%]: %v"
		 :action ,(funcall locker 'widget-choice-action)
		 (function :tag "Function with no rest arg" :format "%v")
		 (group
		  :tag "Function and rest arg(s)" :indent 0 :offset 4
		  (function :format "%v")
		  (editable-list
		   :inline t
		   :entry-format "%i %d Arg: %v"
		   :insert-before
		   ,(funcall locker 'widget-editable-list-insert-before)
		   :delete-at
		   ,(funcall locker 'widget-editable-list-delete-at)
		   (sexp :format "%v"))))))))))

(defcustom w3m-filter-rules nil
  "Rules to filter advertisements on WEB sites.
This variable is semi-obsolete; use `w3m-filter-configuration' instead."
  :group 'w3m
  :type '(repeat
	  (group :format "%v" :indent 2
		 (regexp :format "Regexp: %v" :value ".*")
		 (choice
		  :format "Filtering Rule:\n  %[Value Menu%]\n  %v"
		  (group :format "%v" :inline t
			 :tag "Delete regions surrounded with these patterns"
			 (const :format "Function: %v\n"
				w3m-filter-delete-regions)
			 (string :format "Start: %v"
				 :value "not a regexp")
			 (string :format "  End: %v"
				 :value "not a regexp"))
		  (function :tag "Filter with a user defined function"
			    :format "Function: %v"
			    :match
			    (lambda (_widget value)
			      (unless (eq value 'w3m-filter-delete-regions)
				(or (fboundp value) (functionp value)
				    (symbolp value)))))))))

(defcustom w3m-filter-google-use-utf8 (not (equal "Japanese" w3m-language))
  ;; FIXME: what does this docstring say? - ky
  "Use the converting rule to UTF-8 on the site of Google."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-filter-google-use-ruled-line  t
  "Use the ruled line on the site of Google."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-filter-google-separator "<hr>"
  "Field separator for Google's search results ."
  :group 'w3m
  :type 'string)

(defcustom w3m-filter-amazon-regxp
  (concat
   "\\`\\(https?://\\(?:www\\.\\)?amazon\\."
   "\\(?:com\\|co\\.\\(?:jp\\|uk\\)\\|fr\\|de\\)"
   ;; "Joyo.com"
   "\\)/"
   "\\(?:"
   "\\(?:exec/obidos\\|o\\)/ASIN"
   "\\|"
   "gp/product"
   "\\|"
   "\\(?:[^/]+/\\)?dp"
   "\\)"
   "/\\([0-9]+\\)")
  "Regexp to extract ASIN number for Amazon."
  :group 'w3m
  :type 'string)

(defcustom w3m-filter-amazon-short-url-bottom nil
  "Amazon short URLs insert bottom position."
  :group 'w3m
  :type 'boolean)

;;;###autoload
(defun w3m-filter (url)
  "Apply filtering rule of URL against a content in this buffer."
  (save-match-data
    (dolist (elem (append w3m-filter-rules
			  (delq nil
				(mapcar
				 (lambda (config)
				   (when (car config)
				     (if (consp (nth 3 config))
					 (cons (nth 2 config) (nth 3 config))
				       (list (nth 2 config) (nth 3 config)))))
				 w3m-filter-configuration))))
      (when (string-match (car elem) url)
	(apply (cadr elem) url (cddr elem))))))

(defvar w3m-filter-selection-history nil)

;;;###autoload
(defun w3m-toggle-filtering (arg)
  "Toggle whether to modify html source by the filters before rendering.
With prefix arg, prompt for a single filter to toggle (a function
toggled last will first appear) with completion."
  (interactive "P")
  (if (not arg)
      ;; toggle state for all filters
      (progn
	(setq w3m-use-filter (not w3m-use-filter))
	(message (concat
		  "web page filtering now "
		  (if w3m-use-filter "enabled" "disabled"))))
    ;; the remainder of this function if for the case of toggling
    ;; an individual filter
    (let* ((selection-list (delq nil (mapcar
				      (lambda (elem)
					(when (and (symbolp (nth 3 elem))
						   (fboundp (nth 3 elem)))
					  (symbol-name (nth 3 elem))))
				      w3m-filter-configuration)))
	   (choice (completing-read
		    "Enter filter name: " selection-list nil t
		    (or (car w3m-filter-selection-history)
			(car selection-list))
		    'w3m-filter-selection-history))
	   (filters w3m-filter-configuration)
	   elem)
      (unless (string= "" choice)
	(setq choice (intern choice))
	(while (setq elem (pop filters))
	  (when (eq choice (nth 3 elem))
	    (setq filters nil)
	    (setcar elem (not (car elem)))
	    (when (car elem)
	      (setq w3m-use-filter t))
	    (message "filter `%s' now %s"
		     choice
		     (if (car elem) "enabled" "disabled"))))))))

(defmacro w3m-filter-delete-regions (url start end
					 &optional without-start without-end
					 use-regex start-pos end-pos count)
  "Delete regions surrounded with a START pattern and an END pattern.
Return t if at least one region is deleted.
If WITHOUT-START is non-nil, do not delete the START pattern.
If WITHOUT-END is non-nil, do not delete the the END strings.
If USE-REGEX is non-nil, treat START and END as regular expressions.
START-POS is a position from which to begin deletions.
END-POS is a position at which to stop deletions.
COUNT is the maximum number of deletions to make."
  `(let (p (i 0) _dummy)
     (goto-char ,(or start-pos '(point-min)))
     (while (and ,@(if count `((< i ,count)))
		 ,(if use-regex
		      `(re-search-forward ,start ,end-pos t)
		    `(search-forward ,start ,end-pos t))
		 (setq p ,(if without-start
			      '(match-end 0)
			    '(match-beginning 0)))
		 ,(if use-regex
		      `(re-search-forward ,end ,end-pos t)
		    `(search-forward ,end ,end-pos t)))
       (delete-region p ,(if without-end
			     '(match-beginning 0)
			   '(match-end 0)))
       (setq i (1+ i)))
     (setq _dummy (> i 0))))

(defmacro w3m-filter-replace-regexp (url regexp to-string
					 &optional start-pos end-pos count)
  "Replace all occurrences of REGEXP with TO-STRING.
Return t if at least one replacement is made.
Optional START-POS, END-POS, and COUNT limit the scope of
the replacements."
  `(let ((i 0) _dummy)
     (goto-char ,(or start-pos '(point-min)))
     (while ,(if count
		 `(and (< i ,count)
		       #1=(re-search-forward ,regexp ,end-pos t))
	       `#1#)
       (replace-match ,to-string nil nil)
       (setq i (1+ i)))
     (setq _dummy (> i 0))))

;; Filter functions:
(defun w3m-filter-google-click-tracking (url)
  "Strip Google's click-tracking code from link urls"
  (goto-char (point-min))
  (while (re-search-forward "\\(<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
href=\"\\)\\(?:[^\"]+\\)?/\\(?:imgres\\?imgurl\\|url\\?\\(?:q\\|url\\)\\)=\
\\([^&]+\\)[^>]+>" nil t)
    ;; In a search result Google encodes some special characters like "+"
    ;; and "?" to "%2B" and "%3F" in a real url, so we need to decode them.
    (insert (w3m-url-decode-string
	     (prog1
		 (concat (match-string 1) (match-string 2) "\">")
	       (delete-region (match-beginning 0) (match-end 0)))))))

(defun w3m-filter-google-shrink-table-width (url)
  "Align table columns vertically to shrink the table width."
  (let ((case-fold-search t)
	last)
    (goto-char (point-min))
    (while (re-search-forward "<tr[\t\n\r >]" nil t)
      (when (w3m-end-of-tag "tr")
	(save-restriction
	  (narrow-to-region (goto-char (match-beginning 0))
			    (match-end 0))
	  (setq last nil)
	  (while (re-search-forward "<td[\t\n\r >]" nil t)
	    (when (w3m-end-of-tag "td")
	      (setq last (match-end 0))
	      (replace-match "<tr>\\&</tr>")))
	  (when last
	    (goto-char (+ 4 last))
	    (delete-char 4))
	  (goto-char (point-max)))))
    ;; Remove rowspan and width specs, and <br>s.
    (goto-char (point-min))
    (while (re-search-forward "<table[\t\n\r >]" nil t)
      (when (w3m-end-of-tag "table")
	(save-restriction
	  (narrow-to-region (goto-char (match-beginning 0))
			    (match-end 0))
	  (while (re-search-forward "\
[\t\n\r ]*\\(?:\\(?:rowspan\\|width\\)=\"[^\"]+\"\\|<br>\\)[\t\n\r ]*"
				    nil t)
	    ;; Preserve a space at the line-break point.
	    (replace-match " "))
	  ;; Insert a space between ASCII and non-ASCII characters
	  ;; and after a comma.
	  (goto-char (point-min))
	  (while (re-search-forward "\
\\([!-;=?-~]\\)\\([^ -~]\\)\\|\\([^ -~]\\)\\([!-;=?-~]\\)\\|\\(,\\)\\([^ ]\\)"
				    nil t)
	    (forward-char -1)
	    (insert " ")
	    (forward-char))
	  (goto-char (point-max)))))))

(defun w3m-filter-add-name-anchors (url)
  ;;  cf. [emacs-w3m:11153], [emacs-w3m:12339], [emacs-w3m:12422],
  ;; [emacs-w3m:12812], [emacs-w3m:12830]
  "Add name anchors that w3m can handle.
This function adds ``<a name=\"FOO_BAR\"></a>'' in front of
``<TAG ... id=\"FOO_BAR\" ...>FOO BAR</TAG>'' in the current buffer."
  (let ((case-fold-search t)
	(maxregexps 10)
	names regexp i st nd)
    (goto-char (point-min))
    (while (re-search-forward "<a[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*\
href=\"#\\([^\"]+\\)\"" nil t)
      (add-to-list 'names (match-string 1)))
    (setq case-fold-search nil)
    (while names
      (setq regexp "[\t\n\r ]+[Ii][Dd]=\"\\("
	    i maxregexps)
      (while (and names (> i 0))
	(setq regexp (concat regexp (regexp-quote (pop names)) "\\|")
	      i (1- i)))
      (setq regexp (concat (substring regexp 0 -1) ")\""))
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t)
	(setq st (match-beginning 0)
	      nd (match-end 0))
	(goto-char st)
	(if (re-search-forward regexp nd t)
	    (progn
	      (goto-char st)
	      (insert "<a name=" (match-string 1) "></a>")
	      (goto-char (+ nd (- (point) st))))
	  (goto-char nd))))))

(defun w3m-filter-subst-disabled-with-readonly (url)
  ;;  cf. [emacs-w3m:12146] [emacs-w3m:12222]
  "Substitute disabled attr with readonly attr in forms."
  (let ((case-fold-search t) st opt nd val default)
    (goto-char (point-min))
    (while (re-search-forward "\
<\\(?:input\\|\\(option\\)\\|textarea\\)[\t\n ]" nil t)
      (setq st (match-beginning 0)
	    opt (match-beginning 1))
      (when (and (search-forward ">" nil t)
		 (progn
		   (setq nd (match-end 0))
		   (goto-char (1+ st))
		   (re-search-forward "[\t\n ]\
\\(?:\\(disabled\\(=\"[^\"]+\"\\)?\\)\\|\\(readonly\\(?:=\"[^\"]+\"\\)?\\)\\)"
				      nd t)))
	(setq val (if (match-beginning 1)
		      (if (match-beginning 2)
			  "readonly=\"readonly\""
			"readonly")
		    (match-string 3)))
	(if opt
	    ;; Unfortunately w3m doesn't support readonly attr in `select'
	    ;; forms, so we replace them with read-only input forms.
	    (if (and (re-search-backward "<select[\t\n ]" nil t)
		     (w3m-end-of-tag "select")
		     (< st (match-end 0)))
		(save-restriction
		  (narrow-to-region (match-beginning 0) (match-end 0))
		  (goto-char (+ (match-beginning 0) 8))
		  (w3m-parse-attributes (id name)
		    (if (and id name)
			(progn
			  (goto-char (point-min))
			  (setq default
				(when (re-search-forward "<option\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]selected\\(?:=\"[^\"]+\"\\)?\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n /]*>[\t\n ]*\\([^<]+\\)" nil t)
				  (goto-char (match-end 1))
				  (skip-chars-backward "\t\n ")
				  (buffer-substring (match-beginning 1)
						    (point))))
			  (delete-region (point-min) (point-max))
			  (insert "<input id=\"" id "\" name=\"" name "\""
				  (if default
				      (concat " value=\"" default "\" ")
				    " ")
				  val
				  ;; Fit the width to that of the select form.
				  " size=\"13\">"))
		      (goto-char (point-max)))))
	      (goto-char nd))
	  (if (match-beginning 1)
	      (save-restriction
		(narrow-to-region st nd)
		(delete-region (goto-char (match-beginning 1)) (match-end 1))
		(insert val)
		(goto-char (point-max)))
	    (goto-char nd)))))))

(defun w3m-filter-fix-tfoot-rendering (url &optional recursion)
  "Render <tfoot>...</tfoot> after <tbody>...</tbody>."
  (let ((table-exists recursion)
	(mark "!-- emacs-w3m-filter ")
	(tbody-end (make-marker))
	tfoots)
    (goto-char (if table-exists (match-end 0) (point-min)))
    (while (or table-exists (re-search-forward "<table[\t\n\r >]" nil t))
      (setq table-exists nil)
      (save-restriction
	(if (w3m-end-of-tag "table")
	    (narrow-to-region (match-beginning 0) (match-end 0))
	  (narrow-to-region (match-beginning 0) (point-max)))
	(goto-char (1+ (match-beginning 0)))
	(insert mark)
	(while (re-search-forward "<table[\t\n\r >]" nil t)
	  (w3m-filter-fix-tfoot-rendering url t))
	(goto-char (point-min))
	(while (search-forward "</tbody>" nil t)
	  (set-marker tbody-end (match-end 0))
	  (goto-char (1+ (match-beginning 0)))
	  (insert mark))
	(unless (bobp)
	  (setq tfoots nil)
	  (goto-char (point-min))
	  (while (re-search-forward "<tfoot[\t\n\r >]" nil t)
	    (when (w3m-end-of-tag "tfoot")
	      (push (match-string 0) tfoots)
	      (delete-region (match-beginning 0) (match-end 0))))
	  (when tfoots
	    (goto-char tbody-end)
	    (dolist (tfoot (nreverse tfoots))
	      (insert "<" mark (substring tfoot 1)))))
	(goto-char (point-max))))
    (set-marker tbody-end nil)
    (unless recursion
      (goto-char (point-min))
      (while (search-forward mark nil t)
	(delete-region (match-beginning 0) (match-end 0))))))

(defun w3m-filter-google (url)
  "Insert separator within items."
  (goto-char (point-min))
  (let ((endm (make-marker))
	(case-fold-search t)
	pos beg end)
    (when (and w3m-filter-google-use-utf8
	       (re-search-forward "\
<a class=. href=\"http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\."
				  nil t)
	       (setq pos (match-beginning 0))
	       (search-backward "<table" nil t)
	       (setq beg (match-beginning 0))
	       (search-forward "</table" nil t)
	       (set-marker endm (match-end 0))
	       (< pos (marker-position endm)))
      (goto-char beg)
      (while (re-search-forward "[?&][io]e=\\([^&]+\\)&" endm t)
	(replace-match "UTF-8" nil nil nil 1))
      (setq end (marker-position endm)))
    (when (string-match "\\`http://www\\.google\\.[^/]+/search\\?" url)
      (goto-char (point-max))
      (when (and w3m-filter-google-use-ruled-line
		 (search-backward "<div class=" end t)
		 (search-forward "</div>" nil t))
	(insert w3m-filter-google-separator))
      (if w3m-filter-google-use-ruled-line
	  (while (search-backward "<div class=" end t)
	    (insert w3m-filter-google-separator))
	(while (search-backward "<div class=" end t)
	  (insert "<p>"))))))

(defun w3m-filter-amazon (url)
  "Insert Amazon short URIs."
  (when (string-match w3m-filter-amazon-regxp url)
    (let* ((base (match-string 1 url))
	   (asin (match-string 2 url))
	   (shorturls `(,(concat base "/dp/" asin "/")
			,(concat base "/o/ASIN/" asin "/")
			,(concat base "/gp/product/" asin "/")))
	   (case-fold-search t)
	   shorturl)
      (goto-char (point-min))
      (setq url (file-name-as-directory url))
      (when (or (and (not w3m-filter-amazon-short-url-bottom)
		     (search-forward "<body" nil t)
		     (search-forward ">" nil t))
		(and w3m-filter-amazon-short-url-bottom
		     (search-forward "</body>" nil t)
		     (goto-char (match-beginning 0))))
	(insert "\n")
	(while (setq shorturl (car shorturls))
	  (setq shorturls (cdr shorturls))
	  (unless (string= url shorturl)
	    (insert (format "Amazon Short URL: <a href=\"%s\">%s</a><br>\n"
			    shorturl shorturl))))
	(insert "\n")))))

(defun w3m-filter-mixi (url)
  "Direct jump to the external diary."
  (goto-char (point-min))
  (let (newurl)
    (while (re-search-forward "<a href=\"?view_diary\\.pl\\?url=\\([^>]+\\)>"
			      nil t)
      (setq newurl (match-string 1))
      (when newurl
	(delete-region (match-beginning 0) (match-end 0))
	(when (string-match "&owner_id=[0-9]+\"?\\'" newurl)
	  (setq newurl (substring newurl 0 (match-beginning 0))))
	(insert (format "<a href=\"%s\">"
			(w3m-url-readable-string newurl)))))))

(defun w3m-filter-alc (url)
  (let ((baseurl "http://eow.alc.co.jp/%s/UTF-8/")
	curl cword beg tmp1)
    (when (string-match "\\`http://eow\\.alc\\.co\\.jp/\\([^/]+\\)/UTF-8/" url)
      (setq curl (match-string 0 url))
      (setq cword (match-string 1 url))
      (setq cword (car (split-string (w3m-url-decode-string cword 'utf-8)
				     " ")))
      (goto-char (point-min))
      (while (search-forward "データの転載は禁じられています" nil t)
	(delete-region (line-beginning-position) (line-end-position))
	(insert "<br>"))
      (goto-char (point-min))
      (when (search-forward "<body" nil t)
	(forward-line 1)
	(insert "<h1>英辞朗 on the WEB<h1>\n")
	(setq beg (point))
	(when (search-forward "<!-- ▼検索文字列 -->" nil t)
	  (forward-line 1)
	  (delete-region beg (point)))
	(when (search-forward "<!-- ▼ワードリンク 履歴 -->" nil t)
	  (forward-line 1)
	  (setq beg (point))
	  (when (search-forward "</body>" nil t)
	    (delete-region beg (match-beginning 0))))
	(insert "<br>＊データの転載は禁じられています。")
	;; next/previous page
	(goto-char (point-min))
	(while (re-search-forward
		"<a href='javascript:goPage(\"\\([0-9]+\\)\")'>"
		nil t)
	  (setq tmp1 (match-string 1))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (format "<a href=\"%s?pg=%s\">" curl tmp1)))
	;; wordlink
	(goto-char (point-min))
	(while (re-search-forward
		"<span class=\"wordlink\">\\([^<]+\\)</span>"
		nil t)
	  (setq tmp1 (match-string 1))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (format "<a href=\"%s\">%s</a>" (format baseurl tmp1) tmp1)))
	;; goGradable/goFairWord
	(goto-char (point-min))
	(while (re-search-forward
		"<a href='javascript:\\(goGradable\\|goFairWord\\)(\"\\([^\"]+\\)\")'>"
		nil t)
	  (setq tmp1 (match-string 2))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (format "<a href=\"%s\">" (format baseurl tmp1))))
	;; remove spacer
	(goto-char (point-min))
	(while (search-forward "img/spacer.gif" nil t)
	  (delete-region (line-beginning-position) (line-end-position)))
	(goto-char (point-min))
	;; remove ワードリンク
	(when (search-forward "alt=\"ワードリンク\"" nil t)
	  (delete-region (line-beginning-position) (line-end-position)))
	;; 全文を表示するは無理
	(goto-char (point-min))
	(while (re-search-forward
		(concat "<br */> *⇒<strong>"
			"<a href='javascript:goFullText(\"[^\"]+\", \"[^\"]+\")'>"
			"全文を表示する</a>")
		nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
	;; Java Document write... ;_;
	;; (while (re-search-forward
	;; 	"<a href='javascript:goFullText(\"\\([^\"]+\\)\", \"\\([^\"]+\\)\")'>"
	;; 	nil t)
	;;   (setq tmp1 (match-string 1))
	;;   (setq tmp2 (match-string 2))
	;;   (delete-region (match-beginning 0) (match-end 0))
	;;   ;; &dk=JE, &dk=EJ
	;;   (insert (format "<a href=\"%s?ref=ex&exp=%s&dn=%s&dk=%s\">"
	;; 		  curl tmp1 tmp2
	;; 		  (if (string-match "\\Cj" cword) "JE" "EJ"))))
	))))

(defun w3m-filter-imepita (url)
  "JavaScript emulation."
  (goto-char (point-min))
  (let (tmp)
    (when (re-search-forward
	   (concat "<script><!--\ndocument.write('\\([^\n]*\\)');\r\n//--></script>\n"
		   "<noscript>.*</noscript>")
	   nil t)
      (setq tmp (match-string 1))
      (delete-region (match-beginning 0) (match-end 0))
      (insert tmp))))

(defun w3m-filter-iframe (url)
  (goto-char (point-min))
  (while (re-search-forward "<iframe [^>]*src=\"\\([^\"]*\\)\"[^>]*>" nil t)
    (insert (concat "[iframe:<a href=\"" (match-string 1) "\">" (match-string 1) "</a>]"))))

(defun w3m-filter-allatanys (url)
  "JavaScript emulation."
  (goto-char (point-min))
  (let (aturl atexpurl)
    (if (re-search-forward
	 (concat "<body[ \t\r\f\n]+onload=\"window\\.top\\.location\\.replace('"
		 w3m-html-string-regexp
		 "');\">")
	 nil t)
	(progn
	  (setq aturl (match-string 1))
	  (setq atexpurl (w3m-expand-url aturl url))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert "<body>\n"
		  "<hr>"
		  "Body has a <b>url=window.top.location.replace()</b><br><br>\n"
		  (format "Goto: <a href=%s>%s</a>\n" aturl atexpurl)
		  "<hr>")
	  (goto-char (point-min))
	  (insert (format "<meta HTTP-EQUIV=\"Refresh\" CONTENT=\"0;URL=%s\">\n"
			  aturl)))
      (while (re-search-forward (concat "<a[ \t\r\f\n]+href=\"javascript:[^(]+('"
					"\\([^']+\\)')\">")
				nil t)
	(setq aturl (match-string 1))
	(delete-region (match-beginning 0) (match-end 0))
	(insert (format "<a href=\"%s\">" aturl))))))

(defun w3m-filter-wikipedia (url)
  "Make anchor reference to work."
  (goto-char (point-min))
  (let (matched-text refid)
    (while (re-search-forward
	    "<\\(?:sup\\|cite\\) id=\"\\([^\"]*\\)\"" nil t)
      (setq matched-text (match-string 0)
	    refid        (match-string 1))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (format "<a name=\"%s\"></a>%s" refid matched-text)))))

(defun w3m-filter-rt (url)
  "Filter top and bottom cruft for rt.com."
  (if (w3m-filter-delete-regions url
				 "<body.*>"
				 "<h1.*>"
				 t t t nil nil 1)
      (w3m-filter-delete-regions url "<div class=\"news-block\"" "</body>")
    ;; BEGIN: When page is Home or Article Index pages ...
    (w3m-filter-delete-regions url
			       "<body>"
			       "<div class=\"header__date\">"
			       nil nil nil nil nil 1)
    (w3m-filter-delete-regions url
			       "</time>"
			       "<div class=\"layout__content \">"
			       nil nil nil (point) nil 1)
    (w3m-filter-delete-regions url
			       "<div class=\"follows-channel"
			       "</body>"
			       nil nil nil (point) nil 1)
    (goto-char (point-min))
    (w3m-filter-delete-regions url
			       "<div class=\"news-block news-block_high-hidden"
			       "</body>"
			       nil nil nil (point) nil 1)
    (goto-char (point-min))
    (when (search-forward "<h2 class=\"main-viralbox__title\">" nil t)
      (let ((p (match-beginning 0)))
	(goto-char (point-min))
	(while (re-search-forward "<a[^>]+>\\(<img[^>]+>\\)</a>" p t)
	  (replace-match "\\1")))
      (while (re-search-forward "<a[^>]+>\\(<div[^>]+>\\)?\
\\(<img[^>]+>\\)\\(.*?\\)\\(<a[^<]+</a>\\)"
				nil t)
	(replace-match "\\3\\4 \\2")))
    (w3m-filter-delete-regions url
			       "<button"
			       "</button>")
    (goto-char (point-min))
    (while (re-search-forward "<h2[^>]*>" nil t)
      (replace-match "<b>")
      (when (search-forward "</h2>" nil t)
	(replace-match "</b>"))
      t)
    (goto-char (point-min))
    (when (search-forward "\
<strong class=\"news-line__title\">News line</strong>\
<div class=\"news-line__content\">"
			  nil t)
      (replace-match
       "<br><a href=\"https://www.rt.com/news/line/\">News Line:</a>")
      (when (re-search-forward "<ul[^>]+>" nil t)
	(replace-match ""))
      (w3m-filter-replace-regexp url
				 "<li class=\"news-line__item\">[^>]+>"
				 "<br>* "
				 (point))
      (w3m-filter-delete-regions url "<ul class=\"buttons\">" "</ul>")
      (w3m-filter-delete-regions url
				 "<div class=\"banners__border\">" "</div>")))
  ;; END: When page is Home or Article Index pages ...
  (goto-char (point-min))
  (while (re-search-forward "alt=\"[^\"]+\"" nil t)
    (replace-match "alt=[photo]"))
  (goto-char (point-min))
  (when (re-search-forward "</h1>.*me:" nil t)
    (replace-match "</b><blockquote>Published:"))
  (when (search-forward
	 "</div><div class=\"article__short-url\"><div class=\"short-url\">"
	 nil t)
    (replace-match ""))
  (when (search-forward "Get short URL" nil t)
    (replace-match " [Short URL]"))
  (when (search-forward
	 "</div></div><div class=\"article__cover\"><div class=\"media  \">"
	 nil t)
    (replace-match " "))
  (when (w3m-filter-delete-regions url
				   "<div class=\"article__share\">"
				   "<div class=\"article__summary summary \">"
				   nil nil nil (point) nil 1)
    (insert "</blockquote><p>"))
  (goto-char (point-min))
  (while (search-forward "</p>&mdash;" nil t)
    (replace-match "<br>&mdash;"))
  (goto-char (point-min))
  (while (search-forward "<center>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\(<a [^>]+>\\)?<strong>\\(<a [^>]+>\\)?\
READ MORE:\\([^<]+\\)\\(</a>\\)?</strong>\\(</p>\\)?"
			    nil t)
    (replace-match
     "<blockquote>Related Article:<br>\\1\\2\\3</a></blockquote>"))
  (goto-char (point-min))
  (while (search-forward "</a><a href" nil t)
    (replace-match "</a> - <a href"))
  (goto-char (point-min))
  (while (re-search-forward "\\(<a[^>]+>\\) *\n+  *\\(<img[^>]+>\\)" nil t)
    (replace-match "\\2<br>\\1"))
  (goto-char (point-min))
  (while (search-forward "<div class=\"tags-trends\">" nil t)
    (replace-match "<blockquote>")
    (when (search-forward "Trends<" nil t)
      (replace-match "Trends: <"))
    (when (search-forward "</div>" nil t)
      (replace-match "</blockquote>")))
  (goto-char (point-min))
  (while (search-forward "<div class=\"read-more__title\">" nil t)
    (replace-match "<blockquote>\\1")
    (when (re-search-forward "Read more:?")
      (replace-match "Related article:"))
    (when (search-forward "</a>")
      (replace-match "\\1</blockquote>")))
  (goto-char (point-min))
  (while (re-search-forward "\\(<div[^>]+>\\)+" nil t)
    (replace-match "")))

(defun w3m-filter-slashdot (url)
  "Filter js deadlinks, top and bottom cruft for slashdot"
  (goto-char (point-min))
  ;; BEGIN: When page is Home or Article Index pages ...
  (when (search-forward "<body class=\"anon index" nil t)
    ;; Title pages have css rule to treat <i> as <blockquote>
    (w3m-filter-replace-regexp url "<i>" "<blockquote>")
    (w3m-filter-replace-regexp url "</i>" "</blockquote>")
    (goto-char (point-min))
    ;; Title page tighten display
    (while (re-search-forward "<h2[^>]+>" nil t)
      (replace-match "<p><b>"))
    (goto-char (point-min))
    (search-forward "<!-- WIT -->" nil t)
    (w3m-filter-delete-regions url
			       "<!-- WIT -->"
			       "<header id=\"mostdiscussed-title\">"
			       nil nil nil (point) nil 1)
    (w3m-filter-delete-regions url
			       "<header id=\"srandblock-title\">"
			       "<nav role=\"firehose footer\">"
			       nil nil nil (point) nil 1)
    (insert "Jump to stories from:<br>&nbsp; ")
    (search-forward "<ul id=\"pagination-controls\">" nil t)
    (replace-match "")
    (w3m-filter-replace-regexp url
			       "<li class=\"fleft\">"
			       ""
			       (point) nil 8)
    (w3m-filter-delete-regions url
			       "</li>"
			       "</body>"
			       nil t nil (point) nil 1)
    (goto-char (point-min))
    (while (w3m-filter-delete-regions url
				      "<div class=\"story-tags\">"
				      "<span id=\"tagbar-.+>"
				      nil nil t (point) nil 1)
      (insert "<br>Tags: "))
    (goto-char (point-min))
    (while (w3m-filter-delete-regions url
				      "<div class=\"tag-menu\">"
				      "</footer>"
				      nil nil nil (point) nil 1)
      (insert "<hr><p>")))
  ;; END: When page is Home or Article Index pages ...
  (w3m-filter-delete-regions url
			     "<body"
			     "<header>"
			     nil nil nil nil nil 1)
  (insert "<body>")
  (w3m-filter-replace-regexp url
			     "<div class=\"body\"[^>]+>"
			     "<p>"
			     (point))
  (goto-char (point-min))
  (while (w3m-filter-delete-regions url
				    "<!-- comment bubble -->"
				    "<span class=\"story-byline\">"
				    nil nil nil (point) nil 1)
    (insert "</b><br>&nbsp;&nbsp;&nbsp;&nbsp;")
    (when (search-forward "</time>" nil t)
      (replace-match "<br>&nbsp;&nbsp;&nbsp;&nbsp;"))
    t)
  (goto-char (point-min))
  (when (search-forward "<aside class=\"grid_24 view_mode\">" nil t)
    (w3m-filter-replace-regexp url
			       "<i>"
			       ""
			       nil (match-beginning 0)))
  (when (w3m-filter-delete-regions url
				   "<aside class=\"grid_24 view_mode\">"
				   "<span class=\"totalcommentcnt\">"
				   nil nil t (point) nil 1)
    (insert "<br><b>"))
  (when (w3m-filter-delete-regions url
				   "</a>"
				   "<ul id=\"commentlisting\" class=\"d2\">"
				   nil nil t (point) nil 1)
    (insert "</b><ul>"))
  (w3m-filter-delete-regions url
			     "<div class=\"grid_10 d1or2\""
			     "<ul id=\"commtree_"
			     nil t t (point) nil 1)
  (w3m-filter-replace-regexp url
			     "<div id=\"comment_body_[^>]+> *<p>"
			     "")
  (w3m-filter-replace-regexp url
			     "<div class=\"commentSub\""
			     "<p>\\&")
  (w3m-filter-replace-regexp url
			     "<div class=\"quote\"><p>\\(.+?\\)</div>"
			     "<blockquote>\\1</blockquote>")
  (w3m-filter-delete-regions url
			     "(<span class=\"ind\""
			     "</a>)")
  (w3m-filter-delete-regions url
			     "<div class=\"commentSub\""
			     "<div id=\"replyto_[0-9]+\">" nil nil t)
  (w3m-filter-delete-regions url
			     "<a id=\"reply_link_"
			     "Flag as Inappropriate</a>")
  (w3m-filter-delete-regions url
			     "<noscript><p><b>There may be more comments"
			     "</body>")
  (goto-char (point-min))
  (while (re-search-forward "<ul id=\"group_[^>]+>\n\t+\
<li id=\"hiddens_[0-9]+\" class=\"hide\"></li>\n</ul>"
			    nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<li id=\"tree_[0-9]+\" class=\"comment hidden\">"
			    nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<div[^>]+>\n*</div>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<span id=\"comment_score_.+:</span>" nil t)
    (replace-match "</b>(Score:"))
  (goto-char (point-min))
  (while (re-search-forward "</a> *<span class=\"uid\"><a[^>]+>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (search-forward "<h4>" nil t)
    (replace-match "<b>")
    (when (search-forward "</h4>" nil t)
      (replace-match "</b><br>"))
    t)
  (w3m-filter-delete-regions url "<article" "<header>")
  (goto-char (point-min))
  (while (re-search-forward
	  "\\( beneath your current threshold.\\)</b></noscript>"
	  nil t)
    (replace-match "</b>\\1<p></noscript>"))
  (w3m-filter-replace-regexp url "</a>" "</a> ")
  (goto-char (point-min))
  (while (re-search-forward "\n[\n\t ]+" nil t)
    (replace-match "")))

(defun w3m-filter-github-repo-main-page (url)
  "filter distractions for the main page of a github repository."
  (w3m-filter-delete-regions
   url
   "<div class=\"d-flex" "</header>" nil t nil nil nil 1)
  (w3m-filter-delete-regions
   url
   "<div class=\"signup-prompt-bg" "<div class=\"f4\">" nil t nil nil nil 1)
  (insert "<hr>")
  ;; NOTE: There is inconsistency in some pages. Some have DIV element
  ;;       'repository-topics-container', while others have 'overall-summary'.
  (w3m-filter-delete-regions
   url
   "<div class=\"repository-topics-container" "<div class=\"commit-tease"
   nil t nil nil nil 1)
  (w3m-filter-delete-regions
   url
   "<div class=\"overall-summary" "<div class=\"commit-tease"
   nil t nil nil nil 1)
  (w3m-filter-delete-regions
   url
   "<div class=\"footer container-lg" "</body>" nil t nil nil nil 1)
  (goto-char (point-min))
  (search-forward "<div class=\"commit-tease" nil t)
  (goto-char (match-beginning 0))
  (insert "<hr>")
  (search-forward "<div class=\"file-wrap\">" nil t)
  (goto-char (match-beginning 0))
  (insert "<hr>")
  (search-forward "</table>" nil t)
  (insert "<hr>")
  )

(defun w3m-filter-xkcd (url)
  "filter distractions for xkcd."
  (w3m-filter-delete-regions url
			     "<body>" "<div ide=\"comic\">" t t :count 1)
  (w3m-filter-delete-regions url
			     "<div id=\"bottom" "</body>" nil t)
  (w3m-filter-replace-regexp url "</?\\(li\\|ul\\)[^>]*>" "" (point-min))
  (w3m-filter-replace-regexp url "<body>" "<body><center>" (point-min))
  )

(defun w3m-filter-gnome-bugzilla (url)
  "Filter <pre class=\"bz_comment_text\"> to fold long lines in GNOME Bugzilla."
  (goto-char (point-min))
  (while (re-search-forward "\
<pre\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"bz_comment_text\"[^>]*>" nil t)
    (or (save-match-data
	  (when (w3m-end-of-tag "pre")
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (insert "<br>\n")
	    (delete-region (match-end 1) (match-end 0))
	    (delete-region (goto-char (match-beginning 0)) (match-beginning 1))
	    (insert "<br>\n")
	    (while (search-forward "\n" nil t)
	      (replace-match "<br>\n"))
	    (widen)
	    t))
	(replace-match ""))))

(defun w3m-filter-generic-page-header (url)
  "Generic page header filter."
  (let (p1 p2 p3 title found)
    (and
     (w3m-filter-delete-regions url "<head>" "<title>" t t)
     (setq p1 (point))
     (search-forward "</title>" nil t)
     (setq title (buffer-substring-no-properties
		  p1 (setq p2 (match-beginning 0))))
     (w3m-filter-delete-regions url "</title>" "</head>" t t nil p2)
     (setq p1 (point)
	   p3 p1)
     (or
      (while (and (not found) (re-search-forward "<h[^>]+>\\([^<]+\\)<" nil t))
	(setq p2 (match-beginning 0))
	(when (string-match (regexp-quote (match-string 1)) title)
	  (goto-char p1)
	  (when (re-search-forward "<body" nil t)
	    (delete-region (match-beginning 0) p2)
	    (setq found t))))
      (w3m-filter-delete-regions url "<body" "<h1" nil t)
      (w3m-filter-delete-regions url "<body" "<h2" nil t)
      (w3m-filter-delete-regions url "<body" "<h3" nil t)
      (w3m-filter-delete-regions url "<body" "<h4" nil t))
     (goto-char p3)
     (insert "<body>"))))

(defun w3m-filter-stackexchange (url)
  "Filter top and bottom cruft for stackexchange.com."
  (w3m-filter-delete-regions url
			     "<body.*>" "<h1.*>" t t t nil nil 1)
  (w3m-filter-delete-regions url "<h2 class=\"space\">Your Answer</h2>"
			     "<h4 id=\"h-related\">Related</h4>"
			     nil t nil nil nil 1)
  (w3m-filter-delete-regions
   url
   "<div id=\"hot-network-questions\" class=\"module tex2jax_ignore\">"
   "</body>"
   nil t nil nil nil 1)

  ;; (when (search-forward "<table>" nil t)
  ;;   (replace-match ""))

  (goto-char (point-min))
  (w3m-filter-delete-regions url
			     "<a class=\"vote-[ud]"
			     "</a>" nil nil t (point))
  (goto-char (point-min))
  (w3m-filter-delete-regions url
			     "<a class=\"star-off"
			     "</td>")
  (w3m-filter-replace-regexp url
			     "<span itemprop=\"upvoteCount[^>]+>"
			     "Votes: ")
  (w3m-filter-replace-regexp url
			     "<div class=\"post-text[^>]+>"
			     "<blockquote>")
  (w3m-filter-replace-regexp url
			     "<div class=\"post-taglist[^>]+>"
			     "</blockquote>")
  (w3m-filter-delete-regions url
			     "<a name='new-answer'>"
			     "</form>" nil nil nil nil nil 1)

  (w3m-filter-replace-regexp
   url
   "<div class=\"spacer\">[^>]+>[^>]+>+?\\([0-9]+\\)</div></a>"
   "\\1 ")
  (w3m-filter-delete-regions url
			     "<td class=\"vt\">"
			     "</td>")

  (goto-char (point-min))
  (while (search-forward "<div class=\"user-info \">" nil t)
    (let ((p1 (match-end 0))
	  (p2 (if (search-forward "<li" nil t)
		  (match-beginning 0)
		(point-max))))
      (w3m-filter-delete-regions
       url "<div class=\"user-details\">" "</a>" nil nil nil p1 p2)
      (goto-char p1)
      (while (re-search-forward "</?div[^>]*>" p2 t)
	(replace-match ""))
      (goto-char p1)
      (while (re-search-forward "<span class=\"reputation-score[^>]*>"
				p2 t)
	(replace-match "[rep:"))
      (goto-char p1)
      (while (re-search-forward "<span class=\"badge1\">" p2 t)
	(replace-match  "] [gold:"))
      (goto-char p1)
      (while (re-search-forward "<span class=\"badge2\">" p2 t)
	(replace-match  "] [silver:"))
      (goto-char p1)
      (while (re-search-forward "<span class=\"badge3\">" p2 t)
	(replace-match  "] [bronze:"))
      (goto-char p1)
      (while (re-search-forward "</?span[^>]*>" p2 t)
	(replace-match  ""))))

  (w3m-filter-replace-regexp url
			     "<td" "<td valign=top")

  (w3m-filter-delete-regions url
			     "<div id=\"tabs\">"
			     "<a name" nil t)

  (goto-char (point-min))
  (while (search-forward "<div id=\"answer-" nil t)
    (replace-match "</ul><hr>\\&"))

  (w3m-filter-delete-regions url
			     "<div id=\"comments-link"
			     "</div>")

  (goto-char (point-min))
  (when (search-forward "<h4 id=\"h-linked\">Linked</h4>" nil t)
    (replace-match "<p><b>Linked</b><br>")
    (let ((p1 (match-end 0))
	  p2)
      (dolist (strs '(("^\t</a>"  "")
		      ("</a>"     "</a><br>")
		      ("</div>"   " ")))
	(goto-char p1)
	(when (setq p2 (when (search-forward "<h4" nil t)
			 (match-beginning 0)))
	  (goto-char p1)
	  (while (re-search-forward (car strs) p2 t)
	    (replace-match (cadr strs))))))
    (w3m-filter-delete-regions
     url
     "<div class=\"spacer\">"
     "<div class=\"answer-votes answered-accepted [^>]+>"
     nil nil t))

  (goto-char (point-min))
  (when (search-forward "<table id=\"qinfo\">" nil t)
    (replace-match "")
    (let ((p1 (match-end 0))
	  (p2 (progn
		(search-forward "</table>" nil t)
		(replace-match "")
		(match-end 0))))
      (w3m-filter-replace-regexp url "<tr>" "" p1 p2)
      (w3m-filter-replace-regexp url "</tr>" "<br>" p1 p2)
      (w3m-filter-replace-regexp url "</?td[^>]*>" "" p1 p2)
      (w3m-filter-replace-regexp url "<b>" "" p1 p2)
      (w3m-filter-replace-regexp url "<a[^>]+>" "" p1 p2)
      (w3m-filter-replace-regexp url "</?p[^>]*>" "" p1 p2))))

(defun w3m-filter-geocities-remove-garbage (url)
  "Remove garbage in http://www.geocities.co.jp/*."
  (w3m-filter-delete-regions
   url
   "<DIV ALIGN=CENTER>\n<!--*/GeoGuide/*-->" "<!--*/GeoGuide/*-->\n</DIV>"))

(defun w3m-filter-infoseek-remove-ads (url)
  "Remove ADV in http://*.hp.infoseek.co.jp/*."
  (w3m-filter-delete-regions url "<!-- start AD -->" "<!-- end AD -->"))

(defun w3m-filter-ascii24-remove-ads (url)
  "Remove ADV in http://linux.ascii24.com/linux/*."
  (w3m-filter-delete-regions
   url
   "<!-- DAC CHANNEL AD START -->" "<!-- DAC CHANNEL AD END -->"))

(defun w3m-filter-prefer-lazy-images (url)
  "Prefer a lazy image specified with data-src= in img tags.
This function replaces that of src= with it."
  (goto-char (point-min))
  (let ((case-fold-search t)
	st nd data-src)
    (while (re-search-forward "<img[\t\n ]+[^>]+>" nil t)
      (setq st (goto-char (match-beginning 0))
	    nd (cadr (match-data))) ;; a marker version of (match-end 0)
      (when (re-search-forward "[\t\n ]+data-\\(src=\"[^\"]+\"\\)" nd t)
	(setq data-src (match-string 1))
	(replace-match "")
	(goto-char st)
	(if (re-search-forward "[\t\n ]+\\(src=\"[^\"]+\"\\)" nd t)
	    (replace-match data-src nil nil nil 1)
	  (forward-char 4)
	  (insert " " data-src)))
      (goto-char nd))))

;;; w3m-filter.el ends here
