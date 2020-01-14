;;; w3m.el --- an Emacs interface to w3m -*- lexical-binding: t -*-

;; Copyright (C) 2000-2020 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          Tsuyoshi CHO       <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: w3m, WWW, hypermedia

;; This file is the main part of emacs-w3m.

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

;; Emacs-w3m is an Emacs interface to the w3m program.  For more
;; detail about w3m, see:
;;
;;    http://w3m.sourceforge.net/


;;; How to install:

;; See the README file in any case.  We also recommend you check
;; whether a newer version of w3m is released.
;;
;; The outline of installation is: run the `configure' script and type
;; `make install' in the top directory of the emacs-w3m distribution.


;;; Code:

;; Delete this section when emacs-w3m drops the Emacs 25 support.
;; Note that `gv' should be loaded before `cl-lib' autoloads it.
(eval-and-compile
  (unless (>= emacs-major-version 26)
    (require 'gv))) ;; gv--defsetter

;; Note that `cl-lib' should be loaded before `cl' even if `cl' loads
;; `cl-lib', otherwise the functions `cl-lib' provides will be marked
;; as "might not be defined at runtime" because of `eval-when-compile'.
(require 'cl-lib) ;; cl-decf, cl-incf, cl-labels, cl-remove-if

;; Delete this section when emacs-w3m drops the Emacs 25 support.
;; In Emacs 26 and greater, c[ad][ad][ad]+r are what subr.el provides.
(eval-when-compile
  (unless (>= emacs-major-version 26)
    (require 'cl))) ;; c[ad][ad][ad]+r

(require 'w3m-util)
(require 'w3m-proc)

(defvar w3m-use-tab-line)
(defvar tab-line-exclude-modes) ;; 27.1

;; `w3m-use-tab-line' refers to this variable.
(eval-and-compile
  (defcustom w3m-use-tab t
    "Use emacs-w3m in \"Tabbed\" display mode.
This variable is now DEPRECATED!  Please use `w3m-display-mode'
instead.  When non-nil, emacs-w3m will make a reasonable effort to
display all its buffers in a single window, which has a clickable
tab bar along the top.
See also `w3m-use-tab-line' and `w3m-use-tab-menubar'."
    :group 'w3m
    :type 'boolean
    :set (lambda (symbol value)
	   (prog1
	       (custom-set-default symbol value)
	     (when (boundp 'w3m-use-tab-line)
	       ;; w3m.elc has already been loaded
	       ;; or `w3m-use-tab-line' has already been set.
	       (if (and value w3m-use-tab-line)
		   (progn
		     (require 'tab-line nil t)
		     (when (boundp 'tab-line-exclude-modes)
		       (add-to-list 'tab-line-exclude-modes 'w3m-mode)))
		 (when (boundp 'tab-line-exclude-modes)
		   (setq tab-line-exclude-modes
			 (delq 'w3m-mode tab-line-exclude-modes)))))))))

;; Why the two variable `w3m-select-buffer-hook' and `w3m-use-tab-line'
;; are here is because w3m-ems.el uses.
(defcustom w3m-select-buffer-hook nil
  "Hook run when a different emacs-w3m buffer is selected."
  :group 'w3m
  :type 'hook)

(eval-and-compile
  (defcustom w3m-use-tab-line (boundp 'tab-line-format)
    "Use `tab-line-format' instead of `header-line-format' to display tabs.
See also `w3m-use-tab'."
    :group 'w3m
    :type 'boolean
    :set (lambda (symbol value)
	   (prog1
	       (setq value (and (boundp 'tab-line-format) value))
	     (if (boundp symbol)
		 (unless (equal (symbol-value symbol) value)
		   (custom-set-default symbol value)
		   (when (featurep 'w3m-ems)
		     (defvar w3m-tab-separator-map)
		     (setcar (cadr w3m-tab-separator-map)
			     (if value 'tab-line 'header-line))
		     (let ((cur (cons (selected-frame) (selected-window)))
			   window)
		       (dolist (buffer (let (w3m-fb-mode) (ignore w3m-fb-mode)
					    (w3m-list-buffers t)))
			 (with-current-buffer buffer
			   (w3m-setup-tab-line)
			   (w3m-tab-make-keymap t)
			   ;; Redisplay window in order to redraw tabs
			   ;; (`force-mode-line-update', `redraw-display', etc.
			   ;;  didn't work but is this really necessary?).
			   (when (setq window
				       (get-buffer-window buffer 'visible))
			     (unwind-protect
				 (progn
				   (select-frame (window-frame window))
				   (select-window window t)
				   (with-temp-buffer
				     (switch-to-buffer (current-buffer))
				     (sit-for 0)))
			       (select-frame (car cur))
			       (select-window (cdr cur)))))))))
	       (custom-set-default symbol value))
	     (if (and value w3m-use-tab)
		 (progn
		   (require 'tab-line)
		   (when (boundp 'tab-line-exclude-modes)
		     (add-to-list 'tab-line-exclude-modes 'w3m-mode)))
	       (when (boundp 'tab-line-exclude-modes)
		 (setq tab-line-exclude-modes
		       (delq 'w3m-mode tab-line-exclude-modes))))))))

(eval-and-compile (require 'w3m-ems))

(add-hook 'w3m-select-buffer-hook #'w3m-set-buffer-seen)

(require 'bookmark-w3m)
(require 'w3m-fb)
(require 'w3m-hist)
(require 'timezone)
(require 'image-mode)

;; Add-on programs:
(eval-and-compile
  (autoload 'w3m-bookmark-view "w3m-bookmark"
    "Display the bookmark" t)
  (autoload 'w3m-bookmark-view-new-session "w3m-bookmark"
    "Display the bookmark on a new session" t)
  (autoload 'w3m-bookmark-add-this-url "w3m-bookmark"
    "Add a link under point to the bookmark." t)
  (autoload 'w3m-bookmark-add-current-url "w3m-bookmark"
    "Add a url of the current page to the bookmark." t)
  (autoload 'w3m-bookmark-add-all-urls "w3m-bookmark"
    "Add urls of all pages being visited to the bookmark." t)
  (autoload 'w3m-bookmark-add "w3m-bookmark" "Add URL to bookmark.")
  (autoload 'w3m-bookmark-iterator "w3m-bookmark"
    "Iteration bookmark groups/entries.")
  (autoload 'w3m-search "w3m-search"
    "Search a word using search engines." t)
  (autoload 'w3m-search-new-session "w3m-search"
    "Search a word using search engines in a new session." t)
  (autoload 'w3m-search-uri-replace "w3m-search")
  (autoload 'w3m-weather "w3m-weather"
    "Display a weather report." t)
  (autoload 'w3m-about-weather "w3m-weather")
  (autoload 'w3m-antenna "w3m-antenna"
    "Report changes of web sites." t)
  (autoload 'w3m-antenna-add-current-url "w3m-antenna"
    "Add a link address of the current page to the antenna database." t)
  (autoload 'w3m-about-antenna "w3m-antenna")
  (autoload 'w3m-dtree "w3m-dtree"
    "Display a directory tree." t)
  (autoload 'w3m-about-dtree "w3m-dtree")
  (autoload 'w3m-namazu "w3m-namazu"
    "Search files with Namazu." t)
  (autoload 'w3m-about-namazu "w3m-namazu")
  (autoload 'w3m-perldoc "w3m-perldoc"
    "View Perl documents" t)
  (autoload 'w3m-about-perldoc "w3m-perldoc")
  (autoload 'w3m-fontify-forms "w3m-form")
  (autoload 'w3m-fontify-textareas "w3m-form")
  (autoload 'w3m-form-textarea-file-cleanup "w3m-form")
  (autoload 'w3m-form-textarea-files-remove "w3m-form")
  (autoload 'w3m-form-kill-buffer "w3m-form")
  (autoload 'w3m-form-set-number "w3m-form")
  (autoload 'w3m-form-expand-form "w3m-form")
  (autoload 'w3m-form-unexpand-form "w3m-form")
  (autoload 'w3m-form-make-form-data "w3m-form")
  (autoload 'w3m-filter "w3m-filter")
  (autoload 'w3m-toggle-filtering "w3m-filter"
    "Toggle whether web pages will have their html modified by w3m's \
filters before being rendered."
    t)
  (autoload 'w3m-setup-tab-menu "w3m-tabmenu")
  (autoload 'w3m-setup-bookmark-menu "w3m-bookmark")
  (autoload 'w3m-switch-buffer "w3m-tabmenu")
  (autoload 'w3m-cookie-set "w3m-cookie")
  (autoload 'w3m-cookie-get "w3m-cookie")
  (autoload 'w3m-cookie "w3m-cookie")
  (autoload 'w3m-about-cookie "w3m-cookie")
  (autoload 'w3m-cookie-shutdown "w3m-cookie" nil t)
  (autoload 'report-emacs-w3m-bug "w3m-bug" nil t)
  (autoload 'w3m-replace-symbol "w3m-symbol" nil t)
  (autoload 'w3m-mail "w3m-mail" nil t)
  (autoload 'w3m-lnum-mode "w3m-lnum" nil t)
  (autoload 'w3m-lnum-follow "w3m-lnum" nil t)
  (autoload 'w3m-lnum-goto "w3m-lnum" nil t)
  (autoload 'w3m-lnum-universal "w3m-lnum" nil t)
  (autoload 'w3m-lnum-toggle-inline-image "w3m-lnum" nil t)
  (autoload 'w3m-lnum-view-image "w3m-lnum" nil t)
  (autoload 'w3m-lnum-external-view-this-url "w3m-lnum" nil t)
  (autoload 'w3m-lnum-edit-this-url "w3m-lnum" nil t)
  (autoload 'w3m-lnum-print-this-url "w3m-lnum" nil t)
  (autoload 'w3m-lnum-download-this-url "w3m-lnum" nil t)
  (autoload 'w3m-lnum-bookmark-add-this-url "w3m-lnum" nil t)
  (autoload 'w3m-lnum-zoom-in-image "w3m-lnum" nil t)
  (autoload 'w3m-lnum-zoom-out-image "w3m-lnum" nil t)
  (autoload 'w3m-session-select "w3m-session"
    "Select session from session list." t)
  (autoload 'w3m-session-save "w3m-session"
    "Save list of displayed session." t)
  (autoload 'w3m-setup-session-menu "w3m-session")
  (autoload 'w3m-session-automatic-save "w3m-session")
  (autoload 'w3m-session-deleted-save "w3m-session")
  (autoload 'w3m-session-last-autosave-session "w3m-session")
  (autoload 'w3m-session-goto-session "w3m-session")
  (autoload 'w3m-session-crash-recovery-save "w3m-session")
  (autoload 'w3m-session-last-crashed-session "w3m-session")
  (autoload 'w3m-save-buffer "w3m-save"
    "Save the current page and its image data locally."))

;; Avoid byte-compile warnings.
(declare-function doc-view-mode "doc-view")
(declare-function doc-view-mode-p "doc-view" (type))
(declare-function image-backward-hscroll "image-mode" (&optional n))
(declare-function image-bol "image-mode" (arg))
(declare-function image-eol "image-mode" (arg))
(declare-function image-forward-hscroll "image-mode" (&optional n))
(declare-function image-mode-setup-winprops "image-mode")
(declare-function image-scroll-down "image-mode" (&optional n))
(declare-function image-scroll-up "image-mode" (&optional n))
(declare-function quit-window "window" (&optional kill window))
(declare-function rfc2368-parse-mailto-url "rfc2368" (mailto-url))
(declare-function w3m-search-escape-query-string "w3m-search"
		  (str &optional coding))
(declare-function widget-convert-button "wid-edit" (type from to &rest args))
(declare-function widget-forward "wid-edit" (arg))
(declare-function widget-get "wid-edit" (widget property))
(declare-function zone-call "zone" (program &optional timeout))
(defvar bidi-paragraph-direction)
(defvar doc-view-mode-map)
(defvar w3m-bookmark-mode)
(defvar w3m-bookmark-menu-items)
(defvar w3m-bookmark-menu-items-pre)
(defvar w3m-tab-menubar-make-items-preitems)
(defvar w3m-search-engine-alist)
(defvar w3m-session-menu-items-pre)
(defvar w3m-session-menu-items)

;; The version is decided by the final revision 1.1717 which was
;; hosted by the CVS repository.
(defconst emacs-w3m-version
  "1.4.632"
  "Version number of this package.
Not to be confused with `w3m-version'.")

(defgroup w3m nil
  "Emacs-w3m - the web browser of choice."
  :group 'hypermedia)

(defgroup w3m-face nil
  "Faces used for emacs-w3m."
  :group 'w3m
  :prefix "w3m-")

(defcustom w3m-command nil
  "Name of the executable file of the w3m command.
You normally don't have to specify the value, since emacs-w3m looks
for the existing commands in order of w3m, w3mmee and w3m-m17n in the
`exec-path' directories in order if it is nil in the beginning.

If you want to use the other w3m command, specify the value of this
variable explicitly in the .emacs file or customize the value and save
it.  In this case, you need to restart Emacs and emacs-w3m.  That is,
there is currently no way to apply the changing of the w3m command to
all the emacs-w3m programs safely after loading the w3m.elc module."
  :group 'w3m
  :type '(radio (const :format "Not specified " nil)
		(string :format "Command: %v")))

(defcustom w3m-display-ins-del 'auto
  "Value of `display_ins_del' option."
  :group 'w3m
  :type '(radio (const :format "Delect automatically " auto)
		(const :format "Use fontify\n" fontify)
		(const :format "Use tag " tag)
		(const :format "No have option" nil)))

(defvar w3m-type nil
  "Type of the w3m command.
The valid values include `w3m', `w3mmee', and `w3m-m17n'.")
(defvar w3m-compile-options nil
  "Compile options that the w3m command was built with.")
(defvar w3m-version nil
  "Version string of the external w3m command.

Not to be confused with `emacs-w3m-version'.")

;; Set w3m-command, w3m-type, w3m-version and w3m-compile-options
(if noninteractive ;; Don't call the external command when compiling.
    (unless w3m-command
      (setq w3m-command "w3m"))
  (when (or (null w3m-command)
	    (null w3m-type)
	    (null w3m-version)
	    (null w3m-compile-options))
    (let ((command (or w3m-command
		       (w3m-which-command "w3m")
		       (w3m-which-command "w3mmee")
		       (w3m-which-command "w3m-m17n"))))
      (when command
	(setq w3m-command command)
	(with-temp-buffer
	  (call-process command nil t nil "-version")
	  (goto-char (point-min))
	  (when (re-search-forward "version \\(w3m/0\\.[3-9]\
\\(?:\\.[0-9\\]\\)*\\(?:rc[0-9]+\\)?\
\\(?:-stable\\|\\(?:\\+cvs\\(?:-[0-9]+\\.[0-9]+\\)?\\)\\)?\
\\(?:-inu\\|\\(-m17n\\|\\(\\+mee\\)\\)\\)?[^,]*\\)" nil t)
	    (setq w3m-version (match-string 1))
	    (setq w3m-type
		  (cond
		   ((match-beginning 3) 'w3mmee)
		   ((match-beginning 2) 'w3m-m17n)
		   ((match-beginning 1) 'w3m)
		   (t 'other))))
	  (when (re-search-forward "options +" nil t)
	    (setq w3m-compile-options
		  (or (split-string (buffer-substring (match-end 0)
						      (point-at-eol))
				    ",")
		      (list nil)))
	    (when (member "m17n" w3m-compile-options)
	      (setq w3m-type 'w3m-m17n))))))))

(when (not (stringp w3m-command))
  (error "\
Install w3m command in `exec-path' or set `w3m-command' variable correctly"))

(defcustom w3m-user-agent (concat "Emacs-w3m/" emacs-w3m-version
				  " " w3m-version)
  "String used for the User-Agent field.  See also `w3m-add-user-agent'."
  :group 'w3m
  :type 'string)

(defcustom w3m-add-user-agent t
  "Non-nil means add the User-Agent field to the request header.
The value of `w3m-user-agent' is used for the field body."
  :group 'w3m
  :type 'boolean)

(defvar w3m-user-agent-default-alist
  `(("Emacs-w3m (user default)" . w3m-user-agent)
    ("Emacs-w3m (package default)"
     . ,(concat "Emacs-w3m/" emacs-w3m-version " " w3m-version)))
  "An default alist of user agent strings.
This is used when offering the user the opportunity to change user
agent strings. This should normally not be modified; instead modify
`w3m-user-agent-alist'.")

(defcustom w3m-user-agent-alist
  '(("Android Webkit" . "\
Mozilla/5.0 (Linux; U; Android 4.0.3; ko-kr; LG-L160L Build/IML74K)\
 AppleWebkit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30")
    ("Firefox 57" . "\
Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:57.0) Gecko/20100101 Firefox/57.0")
    ("IE Mobile" . "\
Mozilla/5.0 (compatible; MSIE 9.0; Windows Phone OS 7.5; Trident/5.0;\
 IEMobile/9.0)")
    ("Opera Mini 9.80" . "\
Opera/9.80 (J2ME/MIDP; Opera Mini/9.80 (S60; SymbOS; Opera Mobi/23.348; U; en)\
 Presto/2.5.25 Version/10.54")
    ("Opera Mobile 12" . "\
Opera/12.02 (Android 4.1; Linux; Opera Mobi/ADR-1111101157; U; en-US)\
 Presto/2.9.201 Version/12.02")
    ("Chrome 51" . "\
Mozilla/5.0 (Linux; Android 5.1.1; VS810PP Build/LMY47V) AppleWebKit/537.36\
 (KHTML, like Gecko) Chrome/51.0.2704.81 Mobile Safari/537.36"))
  "An alist of user agent strings.
Each entry should be a cons of a short descriptive string and
the user agent string."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 2
		       (string :tag "Short Description")
		       (string :tag "User Agent string"))))

(defcustom w3m-user-agent-site-specific-alist nil
  "An alist of user-agent strings to be used for specific URLs.
Each entry should be a cons of a regexp for the URLs to be
covered by the rule, and a user-agent string to be used."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 2
		       (string :tag "URL regexp")
		       (string :tag "User Agent string"))))

(defcustom w3m-language (if (equal #1="Japanese" current-language-environment)
			    #1#)
  "Your preferred language used in emacs-w3m sessions."
  :group 'w3m
  :type '(radio
	  :value-to-internal (lambda (_widget value)
			       (if (and (stringp value)
					(let ((case-fold-search t))
					  (string-match "\\`japan" value)))
				   "Japanese"))
	  :value-to-external (lambda (_widget value)
			       (if (equal value "Japanese") "Japanese"))
	  (const :format "%v " "Japanese")
	  (const :tag "Other" nil)))

(defcustom w3m-command-arguments
  (if (eq w3m-type 'w3mmee) '("-o" "concurrent=0" "-F") nil)
  "List of the default arguments passed to the w3m command.
See also `w3m-command-arguments-alist'."
  :group 'w3m
  :type '(repeat (string :format "Argument: %v")))

(defcustom w3m-command-arguments-alist nil
  "Alist of regexps matching urls and additional arguments passed to w3m.
A typical usage of this variable is to specify whether to use the proxy
server for the particular hosts.  The first match made will be used.
Here is an example of how to set this variable:

(setq w3m-command-arguments-alist
      \\='(;; Don't use the proxy server to visit local web pages.
	(\"\\\\`http://\\\\(?:[^/]*\\\\.\\\\)*your-company\\\\.com\\\\(?:/\\\\|\\\\'\\\\)\"
	 \"-no-proxy\")
	;; Use the proxy server to visit any foreign urls.
	(\"\"
	 \"-o\" \"http_proxy=http://proxy.your-company.com:8080/\")))

Where the first element matches the url that the scheme is \"http\" and
the hostname is either \"your-company.com\" or a name ended with
\".your-company.com\", and the proxy server is not used for those hosts.
If you are a novice on the regexps, you can use the
`w3m-no-proxy-domains' variable instead."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 4
		       (regexp :format "%t: %v")
		       (repeat :tag "Arguments passed to w3m command"
			       (string :format "Arg: %v")))))

(defcustom w3m-no-proxy-domains nil
  "List of domain names with which emacs-w3m will not use a proxy server.
Each element should be exactly a domain name which means the latter
common part of the host names, not a regexp."
  :group 'w3m
  :type '(repeat (string :format "Domain name: %v")))

(defcustom w3m-command-environment
  (delq nil
	(list
	 (if (eq w3m-type 'w3mmee)
	     (cons "W3MLANG" "ja_JP.kterm"))
	 (if (eq system-type 'cygwin)
	     (cons "CYGWIN" "binmode"))
	 (cons "LC_ALL" "C")))
  "Alist of environment variables for subprocesses to inherit."
  :group 'w3m
  :type '(repeat
	  (cons :format "%v" :indent 4
		(string :format "Name: %v")
		(string :format "    Value: %v"))))

(defcustom w3m-fill-column -1
  "Integer used as the value for `fill-column' in emacs-w3m buffers.
If it is positive, pages will be displayed within the columns of that
number.  If it is zero or negative, the number of columns which
subtracted that number from the window width is applied to the maximum
width of pages."
  :group 'w3m
  :type 'integer)

(defcustom w3m-mailto-url-function nil
  "Function used to handle the `mailto' urls.
Function is called with one argument, just a url.  If it is nil, a
function specified by the `mail-user-agent' variable will be used for
composing mail messages."
  :group 'w3m
  :type '(radio (const :tag "Not specified" nil)
		(function :format "%t: %v")))

(defcustom w3m-mailto-url-popup-function-alist
  '((cmail-mail-mode . pop-to-buffer)
    (mail-mode . pop-to-buffer)
    (message-mode . pop-to-buffer)
    (mew-draft-mode . pop-to-buffer)
    (mh-letter-mode . pop-to-buffer)
    (wl-draft-mode . pop-to-buffer))
  "Alist of (MAJOR-MODE . FUNCTION) pairs used to pop to a mail buffer up.
If a user clicks on a `mailto' url and a mail buffer is composed by
`mail-user-agent' with the MAJOR-MODE, FUNCTION will be called with
a mail buffer as an argument.
Note that the variables `display-buffer-alist',
`same-window-buffer-names' and `same-window-regexps' will be bound to
nil while popping to a buffer up."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 11
		       (symbol :format "Major-mode: %v")
		       (function :format "%t: %v"))))

(defcustom w3m-use-ange-ftp nil
  "Non-nil means that `ange-ftp' or `efs' is used to access FTP servers."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-doc-view-content-types
  (condition-case nil
      (delq nil (mapcar (lambda (type)
			  (if (doc-view-mode-p type)
			      (format "application/%s" type)))
			'(dvi postscript pdf)))
    (error nil))
  "List of content types for which to use `doc-view-mode' to view contents.
This overrides `w3m-content-type-alist'."
  :group 'w3m
  :type '(repeat (string :tag "Type" :value "application/")))

(define-obsolete-variable-alias
  'w3m-imitate-widget-button 'w3m-handle-non-anchor-buttons "sometime")
(define-obsolete-function-alias
  'w3m-imitate-widget-button 'w3m-handle-non-anchor-buttons "sometime")

(defcustom w3m-handle-non-anchor-buttons '(derived-mode-p 'gnus-article-mode)
  "If non-nil, regard non anchor buttons as anchor buttons.
It is useful for moving about in a Gnus article buffer using TAB key.
The value may be a boolean, or a list that will be `eval'd."
  :group 'w3m
  :type '(radio (const :format "%v " nil) (const :format "%v \n" t)
		(sexp :format "Sexp to be eval'd: %v")))

(defcustom w3m-treat-image-size t
  "Non-nil means let w3m mind the ratio of the size of images and text.

If it is non-nil, the w3m command will make a halfdump which reserves
rectangle spaces in which images will be put, and also `alt' texts
will be truncated or padded with spaces so that their display width
will be the same as the width of images.

See also `w3m-pixels-per-character' and `w3m-pixels-per-line'.  Those
values will be passed to the w3m command in order to compute columns
and lines which images occupy."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pixels-per-line 64
  "Integer used for the `-ppl' argument of the w3m command.
If nil, the height of the default face is used.  It is valid only when
`w3m-treat-image-size' is non-nil.  Note that a small value may not
induce a good result.  If you want to use emacs-w3m in a character
terminal and make `w3m-treat-image-size' effective, you need to set
this variable properly."
  :group 'w3m
  :type '(choice (const :tag "Auto Detect" nil)
		 (integer :tag "Specify Pixels")))

(defcustom w3m-pixels-per-character nil
  "Integer used for the `-ppc' argument of the w3m command.
If nil, the width of the default face is used.  It is valid only when
`w3m-treat-image-size' is non-nil.  If you want to use emacs-w3m in a
character terminal and make `w3m-treat-image-size' effective, you need
to set this variable properly."
  :group 'w3m
  :type '(radio (const :tag "Auto Detect" nil)
		(integer :format "Specify Pixels: %v")))

(defcustom w3m-image-default-background nil
  "Color name used as transparent color of image.
Nil means to use the background color of the Emacs frame.  Note that
this value is effective only to xbm and monochrome pbm images in Emacs
22 and greater."
  :group 'w3m
  :type '(radio (string :format "Color: %v"
			:match (lambda (widget value)
				 (and (stringp value) (> (length value) 0))))
		(const :tag "Use the background color of the Emacs frame" nil)
		(const :tag "Null string" "")))

(defvar w3m-accept-japanese-characters
  (and (not noninteractive)
       (or (memq w3m-type '(w3mmee w3m-m17n))
	   ;; Examine whether the w3m command specified by `w3m-command'
	   ;; uses `euc-japan' for the internal character set.
	   (let ((str
		  (eval-when-compile
		    (format
		     (concat
		      "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">"
		      "<html><head><meta http-equiv=\"Content-Type\" "
		      "content=\"text/html; charset=ISO-2022-JP\">"
		      "</head><body>%s</body>\n")
		     (string 27 36 66 52 65 59 122 27 40 66)))))
	     (with-temp-buffer
	       (set-buffer-multibyte nil)
	       (insert str)
	       (let ((coding-system-for-write 'binary)
		     (coding-system-for-read 'binary)
		     (default-process-coding-system (cons 'binary 'binary)))
		 (call-process-region (point-min) (point-max) w3m-command
				      t t nil "-T" "text/html" "-halfdump")
		 (goto-char (point-min))
		 (and (re-search-forward (string ?\264 ?\301 ?\273 ?\372)
					 nil t)
		      t))))))
  "Non-nil means that the w3m command accepts Japanese characters.")

(defcustom w3m-coding-system (if (eq w3m-type 'w3mmee)
				 'iso-2022-7bit-ss2
			       'iso-2022-7bit)
  "Default coding system used to communicate with the w3m command."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-terminal-coding-system (if w3m-accept-japanese-characters
					  'euc-japan 'iso-8859-1)
  "Default coding system used when writing to w3m processes.
It is just a default value to set process' coding system initially.
(This variable name is analogically derived from the behavior of the
w3m command which accepts data from Emacs just like reads from the
terminal.)"
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-output-coding-system (if (eq w3m-type 'w3mmee) 'ctext 'utf-8)
  "Coding system used when reading from w3m processes."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-input-coding-system w3m-output-coding-system
  "Coding system used when writing to w3m processes.
It overrides `coding-system-for-write' if it is not `binary'.
Otherwise, the value of the `w3m-current-coding-system' variable is
used instead."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-file-coding-system 'iso-2022-7bit
  "Coding system used when writing configuration files.
This value will be referred to by the `w3m-save-list' function."
  :group 'w3m
  :type 'coding-system)

(defvar w3m-file-coding-system-for-read nil
  "*Coding system used when reading configuration files.
It is strongly recommended that you do not set this variable if there
is no particular reason.  The value will be referred to by the
`w3m-load-list' function.")

(defcustom w3m-file-name-coding-system
  (if (memq system-type '(windows-nt OS/2 emx))
      'shift_jis 'euc-japan)
  "Coding system used to convert pathnames when emacs-w3m accesses files."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-default-coding-system
  (if (equal "Japanese" w3m-language) 'shift_jis 'iso-8859-1)
  "Default coding system used to encode url strings and post-data."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-coding-system-priority-list
  (if (equal "Japanese" w3m-language) '(shift_jis))
  "Coding systems in order of priority used for emacs-w3m sessions."
  :group 'w3m
  :type '(repeat (coding-system :format "%t: %v")))

(defcustom w3m-url-coding-system-alist
  '(("\\`https?://\\(?:[^./?#]+\\.\\)+google\\(?:\\.[^./?#]+\\)+/"
     . (lambda (url)
	 (if (string-match "&ie=\\([^&]+\\)" url)
	     (w3m-charset-to-coding-system (match-string 1 url)))))
    (nil . utf-8))
  "Alist of url regexps and coding systems used to encode url to retrieve.
The form looks like:
  ((\"REGEXP\" . CODING) (\"REGEXP\" . CODING)...(nil . CODING))

Where REGEXP is a regular expression that matches a url.  REGEXP nil
means any url; element of which the car is nil, that is the default,
has to be the last item of this alist.
CODING is a coding system used to encode a url that REGEXP matches.
CODING nil means using the coding system corresponding to a charset
used to encode the current page.  CODING may also be a function that
takes one argument URL and returns a coding system.

If the example.com site requires a browser to use `shift_jis' to encode
url for example, you can add it to this variable as follows:

(add-to-list
 \\='w3m-url-coding-system-alist
 \\='(\"\\\\\\=`https?://\\\\(?:[^./?#]+\\\\.\\\\)*example\\\\.com/\" . shift_jis))"
  :group 'w3m
  :type '(repeat (cons :format "\n%v" :indent 2
		       (radio :format "nil or regexp:\n  %v"
			      (const :format "Any " nil)
			      regexp)
		       (radio :format "coding-system or function:\n  %v"
			      (const :format "Page's coding system " nil)
			      coding-system
			      function))))

(defcustom w3m-key-binding nil
  "Type of key binding set used in emacs-w3m sessions.
The valid values include `info' which provides Info-like keys, and
nil which provides Lynx-like keys."
  :group 'w3m
  :type '(choice
	  (const :tag "Use Info-like key mapping." info)
	  (const :tag "Use Lynx-like key mapping." nil))
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (if (or noninteractive
		   ;; Loading w3m.elc is just in progress...
		   (not (featurep 'w3m)))
	       nil
	     (if (and;; Some program might bind `w3m-mode-map' for compiling.
		  (boundp 'w3m-mode-map)
		  (boundp 'w3m-info-like-map)
		  (boundp 'w3m-lynx-like-map))
		 ;; It won't be bound at the first time.
		 (eval
		  `(setq w3m-mode-map (if (eq ',value 'info)
					  w3m-info-like-map
					w3m-lynx-like-map)
			 w3m-minor-mode-map (w3m-make-minor-mode-keymap))))
	     (let ((buffers (buffer-list)))
	       (save-current-buffer
		 (while buffers
		   (set-buffer (car buffers))
		   (if (eq major-mode 'w3m-mode)
		       (condition-case nil
			   (progn
			     (use-local-map (symbol-value 'w3m-mode-map))
			     (w3m-setup-toolbar)
			     (w3m-setup-menu))
			 (error)))
		   (setq buffers (cdr buffers)))))))))

(defcustom w3m-use-cygdrive (eq system-type 'cygwin)
  "If non-nil, use /cygdrive/ prefix when performing `expand-file-name'."
  :group 'w3m
  :type 'boolean)

(eval-and-compile
  (defconst w3m-treat-drive-letter (memq system-type '(windows-nt OS/2 emx))
    "Say whether the system uses drive letters."))

(defcustom w3m-profile-directory
  (concat "~/." (file-name-sans-extension
		 (file-name-nondirectory w3m-command)))
  "Directory where emacs-w3m config files are loaded from or saved to."
  :group 'w3m
  :type 'directory)

(defcustom w3m-init-file "~/.emacs-w3m"
  "Your emacs-w3m startup file name.
If a file with the `.el' or `.elc' suffixes exists, it will be read
instead.  Nil means no init file will be loaded.

Note: This file is used as the startup configuration *NOT* for the w3m
command but for emacs-w3m.  In order to modify configurations for the
w3m command, edit the file named \"~/.w3m/config\" normally."
  :group 'w3m
  :type '(radio file (const :format "None " nil)))

(defcustom w3m-default-save-directory
  (concat "~/." (file-name-sans-extension
		 (file-name-nondirectory w3m-command)))
  "Default directory where downloaded files will be saved to."
  :group 'w3m
  :type 'directory)

(defcustom w3m-external-view-temp-directory w3m-profile-directory
  "Directory where files are saved for the external file viewer."
  :group 'w3m
  :type 'directory)

(defcustom w3m-default-directory nil
  "Directory used as the current directory in emacs-w3m buffers.
The valid values include a string specifying an existing directory,
a symbol of which the value specifies an existing directory,
a function which takes a url as an argument and returns a directory,
and nil.  If the specified directory does not exist or it is nil,
the value of `w3m-profile-directory' is used.

Note that there is an exception: if a page visits a local file or
visits a remote file using ftp, the directory in which the file exists
is used as the current directory instead."
  :group 'w3m
  :type '(radio (directory :format "%{%t%}: %v" :value "~/")
		(symbol :format "%{%t%}: %v"
			:match (lambda (widget value) value)
			:value default-directory)
		(function :format "%{%t%}: %v")
		(const nil)))

(defcustom w3m-accept-languages
  (let ((file (expand-file-name "config" w3m-profile-directory)))
    (or (when (file-readable-p file)
	  (with-temp-buffer
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (when (re-search-forward "^accept_language[\t ]+\\(.+\\)$" nil t)
	      (delete "" (split-string (match-string 1)
				       "[ \t\r\f\n]*,[ \t\r\f\n]*")))))
	(when (string= w3m-language "Japanese")
	  '("ja" "en"))))
  "List of acceptable languages in descending order of priority.
The default value is set according to the accept_language entry of the
w3m configuration file (normally \"~/.w3m/config\")."
  :group 'w3m
  :type '(repeat (string :format "Lang: %v")))

(defcustom w3m-delete-duplicated-empty-lines t
  "Non-nil means display two or more continuous empty lines into single."
  :group 'w3m
  :type 'boolean)

(defvar w3m-display-inline-images nil
  "Internal variable controls whether to show images in emacs-w3m buffers.
This variable is buffer-local which defaults to the value of
`w3m-default-display-inline-images'.  Don't set it directly; modify
the `w3m-default-display-inline-images' variable or use the\
 `\\<w3m-mode-map>\\[w3m-toggle-inline-images]' command
to change the appearance of images.
See also `w3m-toggle-inline-images-permanently'.")
(make-variable-buffer-local 'w3m-display-inline-images)

(defcustom w3m-default-display-inline-images nil
  "Non-nil means display images inline in emacs-w3m buffers.
You can toggle the visibility of images by the\
 `\\<w3m-mode-map>\\[w3m-toggle-inline-images]' command.
See also `w3m-toggle-inline-images-permanently'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-toggle-inline-images-permanently t
  "Non-nil means let the visibility of images continue permanently.
The visibility of images is initialized according to
`w3m-default-display-inline-images' at the first time, and except that
it may be toggled by the `\\<w3m-mode-map>\\[w3m-toggle-inline-images]'\
 command, it does not change hereafter, if
it is non-nil.  Otherwise, whether images are visible is initialized
according to `w3m-default-display-inline-images' whenever you visit a
new page or reload the current page in an emacs-w3m buffer."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-icon-directory
  (let (dir)
    (or
     (catch 'found-dir
       (let* ((path (locate-library "w3m"))
	      (paths (if path
			 (cons (file-name-directory path) load-path)
		       load-path)))
	 (while paths
	   (setq path (car paths)
		 paths (cdr paths))
	   (if path
	       (progn
		 (if (file-directory-p
		      (setq dir
			    (expand-file-name "../../etc/images/w3m/" path)))
		     (throw 'found-dir dir))
		 (if (file-directory-p
		      (setq dir
			    (expand-file-name "../etc/images/w3m/" path)))
		     (throw 'found-dir dir))
		 (if (file-directory-p
		      (setq dir
			    (expand-file-name "../../etc/w3m/icons/" path)))
		     (throw 'found-dir dir))
		 (if (file-directory-p
		      (setq dir
			    (expand-file-name "../etc/w3m/icons/" path)))
		     (throw 'found-dir dir)))))))
     (and (file-directory-p
	   (setq dir (expand-file-name "images/w3m/" data-directory)))
	  dir)
     (and (file-directory-p
	   (setq dir (expand-file-name "w3m/icons/" data-directory)))
	  dir)))
  "Directory where emacs-w3m should find icon files."
  :group 'w3m
  :type '(radio (const :tag "Not specified")
		(directory :format "%t: %v")))

(defcustom w3m-broken-proxy-cache nil
  "Set it to t if the proxy server seems not to work properly in caching.
Note that this may be the double-edged sword; setting it to t will
likely be harmful if the proxy server sends bad requests (e.g., not
including the Host header, see RFC2616 section 14.23) to foreign
servers when the w3m command specifies the \"no-cache\" directive.  Also
note that it may not be effective if you are using old w3m command."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-quick-start t
  "Non-nil means let emacs-w3m start quickly w/o requiring confirmation.
When you invoke the `w3m' command, it attempts to visit the page of a
string like url around the cursor or the value of `w3m-home-page'.
You won't be asked for the confirmation then if this value is non-nil.
Otherwise, you will be prompted for that url with the editing form."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-home-page
  (or (getenv "HTTP_HOME")
      (getenv "WWW_HOME")
      "about:")
  "This variable specifies the url string to open when emacs-w3m starts.
Don't say HP, which is the abbreviated name of a certain company. ;-)"
  :group 'w3m
  :type '(radio
	  :convert-widget w3m-widget-type-convert-widget
	  `(,@(if (getenv "HTTP_HOME")
		  `((const :format "HTTP_HOME: \"%v\"\n"
			   ,(getenv "HTTP_HOME"))))
	    ,@(if (getenv "WWW_HOME")
		  `((const :format "WWW_HOME: \"%v\"\n"
			   (getenv "WWW_HOME"))))
	    (const :tag "About emacs-w3m" "about:")
	    (const :tag "Blank page" "about:blank")
	    (string :format "URL: %v"))))

(defcustom w3m-arrived-file
  (expand-file-name ".arrived" w3m-profile-directory)
  "Name of the file to keep the arrived URLs database."
  :group 'w3m
  :type 'file)

(defcustom w3m-keep-arrived-urls 500
  "Maximum number of URLs which the arrived URLs database keeps."
  :group 'w3m
  :type 'integer)

(defcustom w3m-prefer-cache nil
  "Non-nil means that cached contents are used without checking headers."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-keep-cache-size 300
  "Maximum number of pages to be cached in emacs-w3m."
  :group 'w3m
  :type 'integer)

(defcustom w3m-follow-redirection 9
  "Maximum number of redirections which emacs-w3m honors and follows.
If nil, redirections are followed by the w3m command.  Don't set it to
nil if you allow to use cookies (i.e., you have set `w3m-use-cookies'
to non-nil) since cookies may be shared among many redirected pages."
  :group 'w3m
  :type '(radio (const :format "Ignore redirections " nil)
		integer))

(defcustom w3m-redirect-with-get t
  "If non-nil, use the GET method after redirection.
It controls how emacs-w3m works when a server responds the code 301 or
302.  Here is an extract from RFC2616:

Note: RFC 1945 and RFC 2068 specify that the client is not allowed
to change the method on the redirected request.  However, most
existing user agent implementations treat 302 as if it were a 303
response, performing a GET on the Location field-value regardless
of the original request method."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-resize-image-scale 50
  "Number of steps in percent used when resizing images."
  :group 'w3m
  :type 'integer)

(defface w3m-anchor
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan"))
    (t (:underline t)))
  "Face used for displaying anchors."
  :group 'w3m-face)

(defface w3m-arrived-anchor
  '((((class color) (background light)) (:foreground "navy"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:underline t)))
  "Face used for displaying anchors which have already arrived."
  :group 'w3m-face)

(defface w3m-current-anchor
  '((t (:underline t :bold t)))
  "Face used to highlight the current anchor."
  :group 'w3m-face)

(defface w3m-image
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:underline t)))
  "Face used for displaying alternate strings of images."
  :group 'w3m-face)

(defface w3m-image-anchor
  '((((class color) (background light)) (:background "light yellow"))
    (((class color) (background dark)) (:background "dark green"))
    (t (:underline t)))
  "Face used for displaying alternate strings of images which are in anchors."
  :group 'w3m-face)

(defface w3m-history-current-url
  ;; The following strange code compounds the attributes of the
  ;; `secondary-selection' face and the `w3m-arrived-anchor' face,
  ;; and generates the new attributes for this face.
  (let ((base-attributes (custom-face-attributes-get 'secondary-selection nil))
	(attributes (custom-face-attributes-get 'w3m-arrived-anchor nil))
	attribute)
    (while base-attributes
      (setq attribute (car base-attributes))
      (unless (memq attribute '(:foreground :underline))
	(setq attributes (plist-put attributes attribute
				    (cadr base-attributes))))
      (setq base-attributes (cddr base-attributes)))
    (list (list t attributes)))
  "Face used to highlight the current url in the \"about://history/\" page."
  :group 'w3m-face)

(defface w3m-bold '((t (:bold t)))
  "Face used for displaying bold text."
  :group 'w3m-face)

(defface w3m-italic '((((type nil)) (:underline t))
		      (t (:italic t)))
  "Face used for displaying italic text.
By default it will be a underline face on a non-window system."
  :group 'w3m-face)

(defface w3m-underline '((t (:underline t)))
  "Face used for displaying underlined text."
  :group 'w3m-face)

(defface w3m-strike-through `((((class color)) (:strike-through t))
			      (t (:underline t)))
  "Face used for displaying strike-through text."
  :group 'w3m-face)

(defface w3m-insert
  '((((class color) (background light))
     (:foreground "purple"))
    (((class color) (background dark))
     (:foreground "orchid"))
    (t (:underline t)))
  "Face used for displaying insert text."
  :group 'w3m-face)

(defcustom w3m-mode-hook nil
  "Hook run after `w3m-mode' initialization.
This hook is evaluated by the `w3m-mode' function."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-before-hook nil
  "Hook run when starting to fontify emacs-w3m buffers.
This hook is evaluated by the `w3m-fontify' function."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-after-hook nil
  "Hook run after fontifying emacs-w3m buffers.
This hook is evaluated by the `w3m-fontify' function."
  :group 'w3m
  :type 'hook)

(defcustom w3m-display-hook
  '(w3m-move-point-for-localcgi
    w3m-history-highlight-current-url
    w3m-db-history-fix-indentation)
  "Hook run after displaying pages in emacs-w3m buffers.
Each function is called with a url string as the argument.  This hook
is evaluated by the `w3m-goto-url' function."
  :group 'w3m
  :type 'hook
  :initialize 'w3m-custom-hook-initialize)

(defcustom w3m-after-cursor-move-hook
  '(w3m-highlight-current-anchor
    w3m-show-form-hint
    w3m-print-this-url
    w3m-auto-show)
  "Hook run each time after the cursor moves in emacs-w3m buffers.
This hook is called by the `w3m-check-current-position' function by
way of `post-command-hook'."
  :group 'w3m
  :type 'hook
  :initialize 'w3m-custom-hook-initialize)

(defcustom w3m-delete-buffer-hook
  '(w3m-pack-buffer-numbers)
  "Hook run when every emacs-w3m buffer is deleted."
  :group 'w3m
  :type 'hook
  :initialize 'w3m-custom-hook-initialize)

(defcustom w3m-async-exec t
  "Non-nil means execute the w3m command asynchronously in Emacs process."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-process-connection-type
  (not (and (memq system-type '(darwin macos))
	    (let ((ver (shell-command-to-string "uname -r")))
	      (and (string-match "\\`\\([0-9]+\\)\\." ver)
		   (< (string-to-number (match-string 1 ver)) 7)))))
  "Value for `process-connection-type' used when communicating with w3m."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-default-content-type "text/html"
  "Default value assumed as the content type of local files."
  :group 'w3m
  :type 'string)

(require 'mailcap)
(mailcap-parse-mimetypes)

(defvar w3m-content-type-alist
  (let ((additions
	 '(("text/sgml" "\\.sgml?\\'" nil "text/plain")
	   ("text/xml" "\\.xml\\'" nil "text/plain")
	   ("text/x-markdown" "\\.md\\'" nil w3m-prepare-markdown-content)
	   ("application/xml" "\\.xml\\'" nil w3m-detect-xml-type)
	   ("application/rdf+xml" "\\.rdf\\'" nil "text/plain")
	   ("application/rss+xml" "\\.rss\\'" nil "text/plain")
	   ("application/xhtml+xml" nil nil "text/html")
	   ("application/x-bzip2" "\\.bz2\\'" nil nil)
	   ("application/x-gzip" "\\.gz\\'" nil nil)))
	(extensions (copy-sequence mailcap-mime-extensions))
	elem ext type exts tem viewer rest)
    ;; items w/ file extensions
    (while (setq elem (pop extensions))
      (setq ext (car elem)
	    type (cdr elem))
      (unless (zerop (length ext))
	(setq exts (list ext))
	(while (setq tem (rassoc type extensions))
	  (unless (or (zerop (length (car tem))) (member (car tem) exts))
	    (push (car tem) exts))
	  (setq extensions (delq tem extensions)))
	(push (list type
		    (concat (if (cdr exts)
				(regexp-opt exts)
			      (regexp-quote ext))
			    "\\'")
		    (list 'mailcap-mime-info type)
		    nil)
	      rest)))
    ;; items w/o file extension
    (dolist (major mailcap-mime-data)
      (dolist (minor (cdr major))
	(unless (string-match "\\`\\.\\*\\'" (car minor))
	  (setq type (cdr (assq 'type (cdr minor)))
		viewer (cdr (assq 'viewer (cdr minor))))
	  (or (string-match "/\\*\\'" type)
	      (assoc type rest)
	      (push (list type nil (when (stringp viewer) viewer) nil)
		    rest)))))
    ;; convert viewers
    (dolist (elem rest)
      (when (and (setq viewer (car (cddr elem)))
		 (not (and (consp viewer)
			   (eq 'mailcap-mime-info (car viewer)))))
	(dolist (v (prog1 (split-string viewer) (setq viewer nil)))
	  (push (cond ((string-equal "%s" v) 'file)
		      ((string-equal "%u" v) 'url)
		      (t v))
		viewer))
	(setcar (cddr elem) (nreverse viewer))))
    ;; addition
    (dolist (elem additions)
      (if (setq tem (assoc (car elem) rest))
	  (setcdr tem (cdr elem)) ;; overkill?
	(push elem rest)))
    (nreverse rest))
  "*Alist of content types, regexps, commands to view, and filters.
Each element is a list which consists of the following data:

1. Content type.

2. Regexp matching a url or a file name.

3. Method to view contents.  The following four types may be used:
   a. Lisp function which takes the url to view as an argument.
   b. (\"COMMAND\" [ARG...]) -- where \"COMMAND\" is the external command
      and ARG's are the arguments passed to the command if any.  The
      symbols `file' and `url' that appear in ARG's will be replaced
      respectively with the name of a temporary file which contains
      the contents and the string of the url to view.
   c. nil which means to download the url into the local file.
   d. (mailcap-mime-info \"Content-Type\") -- it will be replaced with
      the return value that is gotten by evaluatiing this form.  Value
      may be one of the above three types.

4. Content type that overrides the one specified by `1. Content type'.
   Valid values include:
   a. Lisp function that takes three arguments URL, CONTENT-TYPE, and
      CHARSET, and returns a content type.
   b. String that specifies a content type.
   c. nil that means not to override the content type.")

;; FIXME: we need to rearrange the complicated and redundant relation of
;; `w3m-encoding-type-alist', `w3m-decoder-alist', and `w3m-encoding-alist'.
(defcustom w3m-encoding-type-alist
  '(("\\.gz\\'" . "gzip")
    ("\\.bz2?\\'" . "bzip"))
  "Alist of file suffixes and content encoding types."
  :group 'w3m
  :type '(repeat
	  (cons :format "%v" :indent 14
		(string :format "Regexp of Suffixes: %v")
		(string :format "Encoding Type: %v"))))

(defcustom w3m-decoder-alist
  `((gzip "gzip" ("-d"))	;; Don't use "gunzip" and "bunzip2"
    (bzip "bzip2" ("-d"))	;; for broken OS and implementations.
    (deflate
      ,(if (not noninteractive)
	   (let ((exec-path
		  (let ((prefix (file-name-directory
				 (directory-file-name
				  (file-name-directory
				   (w3m-which-command w3m-command))))))
		    (list (expand-file-name "libexec/w3m" prefix)
			  (expand-file-name "lib/w3m" prefix)))))
	     (w3m-which-command "inflate")))
      nil))
  "Alist of encoding types, decoder commands, and arguments."
  :group 'w3m
  :type '(repeat
	  (group :format "\n%v" :indent 4
		 (radio :format "Encoding: %v"
			(const :format "%v " gzip)
			(const :format "%v " bzip)
			(const deflate))
		 (string :format "Command: %v")
		 (repeat :tag "Arguments" :extra-offset 2
			 (string :format "%v")))))

(defcustom w3m-charset-coding-system-alist
  (let ((rest
	 '((us_ascii      . raw-text)
	   (us-ascii      . raw-text)
	   (gb2312	  . cn-gb-2312)
	   (cn-gb	  . cn-gb-2312)
	   (iso-2022-jp-2 . iso-2022-7bit-ss2)
	   (iso-2022-jp-3 . iso-2022-7bit-ss2)
	   (tis-620	  . tis620)
	   (windows-874	  . tis-620)
	   (cp874	  . tis-620)
	   (x-ctext       . ctext)
	   (unknown       . undecided)
	   (x-unknown     . undecided)
	   (windows-1250  . cp1250)
	   (windows-1251  . cp1251)
	   (windows-1252  . cp1252)
	   (windows-1253  . cp1253)
	   (windows-1254  . cp1254)
	   (windows-1255  . cp1255)
	   (windows-1256  . cp1256)
	   (windows-1257  . cp1257)
	   (windows-1258  . cp1258)
	   (euc-jp	  . euc-japan)
	   (shift-jis     . shift_jis)
	   (shift_jis     . shift_jis)
	   (sjis	  . shift_jis)
	   (x-euc-jp      . euc-japan)
	   (x-shift-jis   . shift_jis)
	   (x-shift_jis   . shift_jis)
	   (x-sjis	  . shift_jis)))
	dest)
    (while rest
      (or (w3m-find-coding-system (car (car rest)))
	  (setq dest (cons (car rest) dest)))
      (setq rest (cdr rest)))
    dest)
  "Alist of MIME charsets and coding systems.
Both charsets and coding systems must be symbols."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 2
		       (symbol :format "%t: %v")
		       (coding-system :format "%t: %v"))))

(defcustom w3m-correct-charset-alist
  '(("windows-874"  . "tis-620")
    ("cp874"	    . "tis-620")
    ("cp1250" . "windows-1250")
    ("cp1251" . "windows-1251")
    ("cp1252" . "windows-1252")
    ("cp1253" . "windows-1253")
    ("cp1254" . "windows-1254")
    ("cp1255" . "windows-1255")
    ("cp1256" . "windows-1256")
    ("cp1257" . "windows-1257")
    ("cp1258" . "windows-1258")
    ("shift-jis"    . "shift_jis")
    ("sjis"	    . "shift_jis")
    ("x-euc-jp"     . "euc-jp")
    ("x-shift-jis"  . "shift_jis")
    ("x-shift_jis"  . "shift_jis")
    ("x-sjis"	    . "shift_jis"))
  "Alist of MIME charsets; strange ones and standard ones."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 11
		       (string :format "From: %v")
		       (string :format "To: %v"))))

(defcustom w3m-horizontal-scroll-columns 10
  "Number of steps in columns used when scrolling a window horizontally."
  :group 'w3m
  :type 'integer)

(defcustom w3m-horizontal-shift-columns 2
  "Number of steps in columns used when shifting a window horizontally.
The term `shifting' means a fine level scrolling."
  :group 'w3m
  :type 'integer)

(defcustom w3m-view-recenter 1
  "Argument passed to `recenter' that runs when jumping to an anchor.
If it is 1 for instance, show the anchor position on the second line
of the window.  t runs `recenter' with no arguments, and `nil' means
don't recenter, let the display follow point in the usual way."
  :group 'w3m
  :type '(radio (integer :format "%{%t%}: %v  " :value 1 :size 1)
		(const :format "%v  " t)
		(const :format "%v" nil)))

(defcustom w3m-clear-display-while-reading t
  "If non-nil, clear the display while reading a new page."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-form t
  "Non-nil means make it possible to use form extensions. (EXPERIMENTAL)"
  :group 'w3m
  :type 'boolean
  :require 'w3m-form)

(defcustom w3m-submit-form-safety-check nil
  "Non-nil means ask you for confirmation when submitting a form."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-cookies t
  "Non-nil means enable emacs-w3m to use cookies.  (EXPERIMENTAL)"
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-filter t
  "Non-nil means use filter programs to convert web contents.
See also `w3m-filter-configuration'."
  :group 'w3m
  :type 'boolean
  :require 'w3m-filter)

(defcustom w3m-use-symbol (eq w3m-type 'w3m-m17n)
  "Non-nil means replace symbols that the <_SYMBOL> tags lead into.
It is meaningful only when the w3m-m17n command is used and Emacs
handles unicode charsets."
  :group 'w3m
  :type 'boolean
  :require 'w3m-symbol)

(defcustom w3m-edit-function 'find-file
  "Function used for editing local files.
It is used when either `w3m-edit-current-url' or `w3m-edit-this-url'
is invoked for local pages."
  :group 'w3m
  :type '(radio
	  (const :tag "Edit it in the current window" find-file)
	  (const :tag "Edit it in another window" find-file-other-window)
	  (const :tag "Edit it in another frame" find-file-other-frame)
	  (const :tag "View it in another window" view-file-other-window)
	  (function :format "Other function: %v\n" :value view-file)))

(defcustom w3m-edit-function-alist
  '(("\\`[^?]+/hiki\\.cgi\\?" . hiki-edit-url))
  "Alist of functions used for editing pages.
This option is referred to decide which function should be used to
edit a specified page, when either `w3m-edit-current-url' or
`w3m-edit-this-url' is invoked.  When no suitable function is found
from this alist, `w3m-edit-function' is used."
  :group 'w3m
  :type '(repeat (cons :format "%v" :indent 4
		       (regexp :format "URL: %v")
		       (function
			:match-alternatives (functionp symbolp)))))

(defcustom w3m-url-local-directory-alist
  (when (boundp 'yahtml-path-url-alist)
    (mapcar
     (lambda (pair)
       (cons (cdr pair) (car pair)))
     (symbol-value 'yahtml-path-url-alist)))
  "Alist of URLs and local directories.
If directory names of a given URL and the car of an element are the
same, emacs-w3m assumes that the file exists in the local directory
where the cdr of an element points to.  The default value will be set
to a value of the `yahtml-path-url-alist' variable which exchanged the
car and the cdr in each element if it is available."
  :type '(repeat
	  (cons :format "%v" :indent 3
		(string :format "URL: %v")
		(directory :format "%t: %v")))
  :group 'w3m)

(defcustom w3m-track-mouse t
  "Whether to track the mouse and message the url under the mouse.
See also `show-help-function'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-title-buffer-name nil
  "Non-nil means use name of buffer included current title."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-japanese-menu (equal "Japanese" w3m-language)
  "Non-nil means use Japanese characters for Menu if possible."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-menu-on-forefront nil
  "Non-nil means place the emacs-w3m menus on the forefront of the menu bar."
  :group 'w3m
  :type 'boolean
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (unless noninteractive
	     (w3m-menu-on-forefront value)))))

(defcustom w3m-add-tab-number nil
  "Non-nil means put sequential number to a title on tab."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-tab-menubar t
  "Non-nil means use the TAB pull-down menu in the menubar.

This feature makes it possible to see a llst of emacs-w3m buffers in a
single window, and select one by clicking a mouse on it.

For web page titles written in non-ascii text, this feature
requires Emacs to have been built to be able to display
multilingual text in the menubar.

See also `w3m-display-mode'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-new-session-url "about:blank"
  "Default url to be opened in a tab or a session which is created newly."
  :group 'w3m
  :type '(radio
	  :convert-widget w3m-widget-type-convert-widget
	  `((const :tag "About emacs-w3m" "about:")
	    (const :tag "Blank page" "about:blank")
	    (const :tag "Bookmark" "about://bookmark/")
	    (const :tag ,(format "Home page (%s)" w3m-home-page)
		   ,w3m-home-page)
	    (string :format "URL: %v"
		    :value ,(if (equal w3m-language "Japanese")
				"https://emacs-w3m.github.io/index-ja.html"
			      "https://emacs-w3m.github.io/index.html")))))

(defcustom w3m-make-new-session nil
  "Non-nil means making new emacs-w3m buffers when visiting new pages.
If it is non-nil and there are already emacs-w3m buffers, the `w3m'
command makes a new emacs-w3m buffer if a user specifies a url string
in the minibuffer, and the `w3m-safe-view-this-url' command also makes
a new buffer if a user invokes it in a buffer not being running the
`w3m-mode'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-favicon t
  "Non-nil means show favicon images if they are available.
It will be set to nil automatically if ImageMagick's `convert' program
does not support the ico format."
  :get (lambda (symbol)
	 (and (not noninteractive)
	      (default-value symbol)
	      (w3m-favicon-usable-p)))
  :set (lambda (symbol value)
	 (custom-set-default symbol (and (not noninteractive)
					 value
					 (w3m-favicon-usable-p))))
  :group 'w3m
  :type 'boolean)

(defcustom w3m-show-graphic-icons-in-mode-line t
  "Non-nil means show graphic status indicators in the mode-line.
If it is nil, also the favicon won't be shown in the mode-line even if
`w3m-use-favicon' is non-nil."
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (if (and (not noninteractive)
		    ;; Make sure it is not the first time.
		    (featurep 'w3m)
		    (fboundp 'w3m-initialize-graphic-icons))
	       (w3m-initialize-graphic-icons))))
  :group 'w3m
  :type 'boolean)

(define-obsolete-variable-alias
  'w3m-show-graphic-icons-in-header-line 'w3m-show-graphic-icons-in-tab-line
  "27.1")

(defcustom w3m-show-graphic-icons-in-tab-line t
  "Non-nil means show graphic status indicators in the tab-line.
If it is nil, also the favicon won't be shown in the tab-line even
if `w3m-use-favicon' is non-nil."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pop-up-windows t
  "Use emacs-w3m in dual-pane mode.

This variable is now DEPRECATED! Please use `w3m-display-mode'
instead.

When non-nil, once more than one emacs-w3m buffer exists, a
reasonable attempt is made to present emacs-w3m in two windows on
the same frame. Any action to open a new emacs-w3m buffer, such
as `w3m-goto-url-new-session' or `w3m-search-new-session'
displays the new buffer in the unfocused pane, and transfers
focus there.

Note that this display mode setting is of the lowest priority, in
that if either `w3m-pop-up-frames' or `w3m-use-tab' is non-nil,
this setting will be ignored."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pop-up-frames nil
  "Use emacs-w3m in a dedicated frame mode.

This variable is now DEPRECATED! Please use `w3m-display-mode'
instead.

When non-nil, emacs-w3m makes a reasonable attempt to display its
buffers in dedicated frames, although you can manually subvert
that if you insist.

There are actually two types of dedicated frame display modes,
DEDICATED-FRAMES and TABBED-DEDICATED-FRAMES.

DEDICATED-FRAMES mode creates a new single-window frame for each
new emacs-w3m buffer you create.

TABBED-DEDICATED-FRAMES mode opens new emacs-w3m buffers in the
same window of the frame from which it was spawned, and those
buffers are not easily visible to emacs-w3m buffers associated
with other frames. The window includes a clickable tab bar along
the top. To enable this variant display mode under the old
scheme, you need to set this variable and also variable
`w3m-use-tab'. When using this display mode, you can still
manually create multiple emacs-w3m frames by using the basic
command `w3m'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-new-session-in-background nil
  "Say whether not to focus on a new tab or a new session in target.
It influences only when a new emacs-w3m buffer is created."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-popup-frame-parameters nil
  "Alist of frame parameters used when creating a new emacs-w3m frame."
  :group 'w3m
  :type '(repeat :format "Frame Parameters:\n%v%i\n"
		 (cons :format "%v" :indent 3
		       (symbol :format "Parameter: %v")
		       (sexp :format "%t: %v"))))

(defcustom w3m-auto-show t
  "Non-nil means provide the ability to horizontally scroll the window.
Automatic horizontal scrolling is made when the point gets away from
both ends of the window, but nothing occurs if `truncate-lines' is set
to nil.

This feature works with the specially made program in emacs-w3m; usual
`auto-hscroll-mode', `automatic-hscrolling', `auto-show-mode' or
`hscroll-mode' will all be invalidated in emacs-w3m buffers."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-horizontal-scroll-division 4
  "Integer used by the program making the point certainly visible.
The cursor definitely does not go missing even when it has been driven
out of the window while wandering around anchors and forms in an
emacs-w3m buffer.

Suppose that the value of this variable is N.  When the point is
outside the left of the window, emacs-w3m scrolls the window so that
the point may be displayed on the position within 1/N of the width of
the window from the left.  Similarly, when the point is outside the
right of the window, emacs-w3m scrolls the window so that the point
may be displayed on the position of 1/N of the width of the window
from the right.

This feature doesn't work if `w3m-auto-show' is nil.  The value must
be a larger integer than 1."
  :group 'w3m
  :type '(integer :match (lambda (_widget _value) t)
		  :value-to-internal
		  (lambda (_widget value)
		    (if (and (integerp value) (> value 1))
			(prin1-to-string value) "4"))
		  :value-to-external
		  (lambda (_widget value)
		    (setq value (condition-case nil
				    (string-to-number value)
				  (error 4)))
		    (if (> value 1) value 4))))

(defcustom w3m-show-error-information t
  "Non-nil means show an error information as a web page.
Page is made when the foreign server doesn't respond to a request to
retrieve data."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-refresh t
  "Non-nil means honor the REFRESH attribute in META tags.
If non-nil, emacs-w3m arbitrarily takes you to a url the attribute
specifies."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-refresh-minimum-interval 5
  "Number of seconds used to override the meta refresh a page specifies.
If the meta refresh seconds the page specifies is less than this value,
this will be used instead of that."
  :group 'w3m
  :type 'integer)

(defcustom w3m-mbconv-command "mbconv"
  "Name of the \"mbconv\" command provided by the \"libmoe\" package.
The \"libmoe\" package is what is used when `w3m-command' is w3mmee."
  :group 'w3m
  :type 'string)

(defcustom w3m-markdown-converter
  (cond
   ((w3m-which-command "markdown")
    '("markdown"))
   ((w3m-which-command "grip")
    '("grip" "--quiet" "--title" "" "--export" "-")))
  "Command used to convert a file that is markdown formed to html format.
The value is a list containing a command, and if necessary arguments."
  :group 'w3m
  :type '(radio (const :format "none\n")
		(cons :format "%v"
		      (string :format "Command: %v")
		      (repeat :format "Arguments:\n%v%i"
			      (string :format "%v")))))

(defcustom w3m-local-find-file-regexps
  (cons nil
	(concat "\\."
		(regexp-opt (append '("htm"
				      "html"
				      "shtm"
				      "shtml"
				      "xhtm"
				      "xhtml"
				      "txt")
				    (and w3m-markdown-converter '("md"))
				    (and (w3m-image-type-available-p 'jpeg)
					 '("jpeg" "jpg"))
				    (and (w3m-image-type-available-p 'gif)
					 '("gif"))
				    (and (w3m-image-type-available-p 'png)
					 '("png"))
				    (and (w3m-image-type-available-p 'xbm)
					 '("xbm"))
				    (and (w3m-image-type-available-p 'xpm)
					 '("xpm"))))
		"\\'"))
  "Cons of two regexps matching and not matching with local file names.
If a url of the `file:' scheme in which you entered matches the first
form and does not match the latter form, it will be opened by the
function specified by the `w3m-local-find-file-function' variable.
Nil for the regexp matches any file names.

For instance, the value `(nil . \"\\\\.[sx]?html?\\\\'\")' allows
\"file:///some/where/w3m.el\", not \"file:///any/where/index.html\", to
open by the function specified by `w3m-local-find-file-function'.  The
latter will be opened as a normal web page.  Furthermore, if you would
like to view some types of contents in the local system using the
viewers specified by the `w3m-content-type-alist' variable, you can
add regexps matching those file names to the second element of this
variable.  For example:

(setq w3m-local-find-file-regexps
      \\='(nil . \"\\\\.\\\\(?:[sx]?html?\\\\|dvi\\\\|ps\\\\|pdf\\\\)\\\\\\='\"))

It is effective only when the `w3m-local-find-file-function' variable
is set properly."
  :group 'w3m
  :type '(cons (radio :tag "Match"
		      (const :format "All " nil)
		      (regexp :format "%t: %v"))
	       (radio :tag "Nomatch"
		      (const :format "All " nil)
		      (regexp :format "%t: %v"))))

(defcustom w3m-local-find-file-function
  '(if (w3m-popup-frame-p)
       'find-file-other-frame
     'find-file-other-window)
  "Function used to open local files.
If a url of the `file:' scheme in which you entered agrees with the
rule of the `w3m-local-find-file-regexps' variable (which see), it is
used to open the file.

Function should take one argument, the string naming the local file.
It can also be any Lisp form returning a function.  Set this to nil if
you want to always use emacs-w3m to see local files."
  :group 'w3m
  :type 'sexp)

(defcustom w3m-local-directory-view-method 'w3m-cgi
  "Symbol of the method to view a local directory tree.
The valid values include `w3m-cgi' using the CGI program specified by
the `w3m-dirlist-cgi-program' variable (which see), and `w3m-dtree'
using the w3m-dtree Lisp module."
  :group 'w3m
  :type '(radio (const :format "Dirlist CGI  " w3m-cgi)
		(const :tag "Directory tree" w3m-dtree)))

(defcustom w3m-dirlist-cgi-program
  (cond ((eq system-type 'windows-nt)
	 "c:/usr/local/lib/w3m/dirlist.cgi")
	((memq system-type '(OS/2 emx))
	 (expand-file-name "dirlist.cmd" (getenv "W3M_LIB_DIR")))
	(t nil))
  "Name of the CGI program used to list a local directory.
If it is nil, the dirlist.cgi module of the w3m command will be used."
  :group 'w3m
  :type `(radio
	  (const :tag "w3m internal CGI" nil)
	  (file :format "path of 'dirlist.cgi': %v"
		:value ,(if (not noninteractive)
			    (expand-file-name
			     (concat "../lib/"
				     (file-name-nondirectory w3m-command)
				     "/dirlist.cgi")
			     (file-name-directory
			      (w3m-which-command w3m-command)))))))

(defcustom w3m-add-referer
  '("\\`https?:" . "\\`https?://\\(?:localhost\\|127\\.0\\.0\\.1\\)/")
  "Rule of sending referers.
There are five choices as a valid value for this option.

(1) nil: this means that emacs-w3m never send referers.
(2) t: this means that emacs-w3m always send referers.
(3) lambda: this means that emacs-w3m send referers only when both
    the current page and the target page are provided by the same
    server.
(4) a cons cell keeping two regular expressions: this means that
    emacs-w3m send referers when the url of the current page matches
    the first regular expression and does not match the second regular
    expression.  Nil for the regexp matches any url.
(5) a function: emacs-w3m send referers when this function which takes
    two arguments, URL and REFERER, returns non-nil.

If you become nervous about leak of your private WEB browsing history,
set `nil' or `lambda' to this option.  When your computer belongs to a
secret network, you may set a pair of regular expressions to inhibit
sending referers which will disclose your private informations, as
follows:

(setq w3m-add-referer
      '(\"\\\\`https?:\"
	. \"\\\\`https?://\\\\(?:[^./]+\\\\.\\\\)*example\\\\.net/\"))
"
  :group 'w3m
  :type '(choice
	  :format "%{%t%}:\n%[Value Menu%] %v"
	  (const :tag "Never send referers" nil)
	  (const :tag "Always send referers" t)
	  (const :tag "Send referers when accessing the same server" lambda)
	  (cons :tag "Send referers when URI matches"
		(list :inline t :format "%v"
		      (radio :indent 2 :sample-face underline
			     :tag "Allow"
			     (regexp :format "%t: %v")
			     (const :tag "Don't allow all" nil))
		      (radio :indent 2 :sample-face underline
			     :tag "Don't allow"
			     (regexp :format "%t: %v")
			     (const :tag "Allow all" nil))))
	  (function :tag "Function that takes two arguments" :format
		    "Send referers when this function returns non-nil\n%v\
(Note: this function takes two argumants URL and REFERER)\n")))

(defcustom w3m-touch-command (w3m-which-command "touch")
  "Name of the executable file of the touch command.
Note that the command is required to be able to modify file's
timestamp with the `-t' option."
  :group 'w3m
  :type 'string)

(defcustom w3m-puny-utf-16be 'utf-16be
  "Coding system for PUNY coding. if nil, don't use PUNY code."
  :group 'w3m
  :type '(radio (coding-system :tag "UTF-16BE with no signature")
		(const "Don't use" nil)))

(defcustom w3m-uri-replace-alist
  '(("\\`gg:" w3m-search-uri-replace "google")
    ("\\`ya:" w3m-search-uri-replace "yahoo")
    ("\\`bts:" w3m-search-uri-replace "debian-bts")
    ("\\`dpkg:" w3m-search-uri-replace "debian-pkg")
    ("\\`alc:"  w3m-search-uri-replace "alc")
    ("\\`urn:ietf:rfc:\\([0-9]+\\)" w3m-pattern-uri-replace
     "http://www.ietf.org/rfc/rfc\\1.txt"))
  "Alist of regexps matching URIs, and some types of replacements.
This alist is used universally to replace locally produced URI
strings (eg. via user input) to the valid internet forms.

Each element should take the form (REGEXP FUNCTION ARGS).
FUNCTION will be called with URI as its first argument and ARGS
as its second.

REGEXP may include grouping constructs \"\\(...\\)\", so that they can be
referred by the \"N\" forms in a replacement (which is one of OPTIONS).

`Emacs-w3m' comes with two predefined functions for use with this alist:

`w3m-pattern-uri-replace'
    Replace a URI using PATTERN (which is just an OPTION).  It is
    allowed that PATTERN contains the \"\\N\" forms in the same manner
    of `replace-match'.

`w3m-search-uri-replace'
    Generate the valid forms to query words to some specified search
    engines.  For example, the element

    (\"\\\\`gg:\" w3m-search-uri-replace \"google\")

    makes it possible to replace the URI \"gg:emacs\" to the form to
    query the word \"emacs\" to the Google site.

Two additional forms for elements are available, but they are
deprecated and support for them are subject to being removed at
any time:

1. (REGEXP REPLACE-PATTERN) Function `w3m-pattern-uri-replace'
   will be called to perform the replacement.

2. (REGEXP FUNCTION) FUNCTION will be called will URI as its
   argument to perform the replacement."
  :group 'w3m
  :type '(repeat
	  :convert-widget w3m-widget-type-convert-widget
	  `((choice
	     :format "%[Value Menu%] %v" :tag "Replacing URI with"
	     (list :indent 4 :tag "Replacement Using Pattern"
		   (regexp :format "%t: %v")
		   (function-item :format "" w3m-pattern-uri-replace)
		   (string :format "Pattern: %v"))
	     (list :format "%t:\n%v" :indent 4 :tag "Quick Search"
		   (regexp :format "Prefix URI %t: %v" :value "")
		   (function-item :format "" w3m-search-uri-replace)
		   (string :format "Quick Search Engine: %v" :value ""))
	     ,@(progn
		 (require 'w3m-search)
		 (mapcar
		  (lambda (elem)
		    (let* ((engine (car elem))
			   (prefix (mapconcat 'identity
					      (split-string (downcase engine))
					      "-")))
		      `(list
			:format "Quick Search:\n%v"
			:indent 4
			:tag ,(concat "Quick Search: " prefix)
			(regexp :tag "Prefix URL Regexp"
				,(concat "\\`" (regexp-quote prefix) ":"))
			(function-item :format "" w3m-search-uri-replace)
			(string :tag "Quick Search Engine" ,engine))))
		  w3m-search-engine-alist))
	     (list :indent 4 :tag "User Defined Function"
		   (regexp :format "%t: %v\n")
		   (function :format "%t: %v\n")
		   (repeat :extra-offset 2 :tag "Options"
			   (sexp :format "%t: %v\n")))))))

(defcustom w3m-relationship-estimate-rules
  `((w3m-relationship-simple-estimate
     "\\`https?://\\(?:www\\|blogsearch\\|groups\\|news\\|images\\)\
\\.google\\.[^/]+/\\(?:\\(?:blog\\|code\\)?search\\|groups\\|news\\|images\
\\|cse\\?cx=\\|custom\\?\\(?:q\\|hl\\)=\\)"
     ,(concat "<a[^>]+?href=" w3m-html-string-regexp "[^>]*aria-label=\""
	      "\\(?:Next page\\|次のページ\\|下一页"
	      "\\|&#45796;&#51020; &#54168;&#51060;&#51648;\\)\"")
     ,(concat "<a[^>]+?href=" w3m-html-string-regexp "[^>]*aria-label=\""
	      "\\(?:Previous page\\|前のページ\\|上一页"
	      "\\|&#51060;&#51204; &#54168;&#51060;&#51648;\\)\"")
     nil nil)
    (w3m-relationship-simple-estimate
     "\\`https?://www\\.google\\.[^/]+/gwt/n\\?u="
     ,(concat "<a[^>]+?href=" w3m-html-string-regexp
	      "[ \t\n]+accesskey=\"3\">")
     ,(concat "<a[^>]+?href=" w3m-html-string-regexp
	      "[ \t\n]+accesskey=\"1\">")
     nil nil)
    (w3m-relationship-simple-estimate
     "\\`http://beta\\.search\\.yahoo\\.co\\.jp/"
     ,(concat "<a href=" w3m-html-string-regexp
	      "><img src=http://i\\.yimg\\.jp/images/common/ar_next\\.gif")
     ,(concat "<a href=" w3m-html-string-regexp
	      "><img src=http://i\\.yimg\\.jp/images/common/ar_prev\\.gif")
     nil nil)
    (w3m-relationship-simple-estimate
     "\\`http://www\\.zdnet\\.co\\.jp/news/"
     ,(concat "<a href=" w3m-html-string-regexp ">次のページ</a>")
     ,(concat "<a href=" w3m-html-string-regexp ">前のページ</a>")
     nil nil)
    (w3m-relationship-oddmuse-estimate)
    (w3m-relationship-magicpoint-estimate)
    (w3m-relationship-slashdot-estimate)
    (w3m-relationship-alc-estimate))
  "Rules to estimate relationships between a retrieved page and others.

This variable supports the emacs-w3m ability to navigate
structured websites, using shortcut keys, to a current page's
parent page (`w3m-view-parent-page'), or next / previous
pages (`w3m-scroll-up-or-next-url', `w3m-scroll-down-or-previous-url').

The variable is a list of RULES. Each RULE is sequence in the
form FUNCTION URL NEXT PREV START CONTENTS.

FUNCTION is the method to to be used to evaluate the other
fields. A FUNCTION should set variables `w3m-start-url',
`w3m-next-url', `w3m-previous-url', `w3m-contents-url'. Emacs-w3m
provides functions `w3m-relationship-simple-estimate',
`w3m-relationship-magicpoint-estimate',
`w3m-relationship-oddmuse-estimate',
`w3m-relationship-slashdot-estimate',
`w3m-relationship-alc-estimate', but of course you can add your
own.

URL is a regex defining the urls for which to use this rule.

NEXT, PREV, START and CONTENTS are regexs of html source code
from which to extract the values of variables `w3m-next-url',
`w3m-previous-url', `w3m-start-url', `w3m-contents-url'
respectively. The url to store should be defined as a regex
collection group."
  :group 'w3m
  :type '(repeat
	  (choice
	   :format "%[Value Menu%] %v"
	   (list :tag "Estimate relationships from anchors matching"
		 :indent 1
		 (const :format "Function: %v\n"
			w3m-relationship-simple-estimate)
		 (regexp :tag "URL")
		 (regexp :tag "Next")
		 (regexp :tag "Prev")
		 (radio :format "Start: %v"
			(const :format "%v " nil) regexp)
		 (radio :format "Contents: %v"
			(const :format "%v " nil) regexp))
	   (list :tag "Estimate with a user defined function"
		 :indent 1
		 function
		 (repeat :tag "Args" :extra-offset 1 (sexp :format "%v"))))))

(defcustom w3m-enable-feeling-searchy t
  "Non-nil enables you to enter any words as well as a url when prompted.
In that case, emacs-w3m uses the default search engine to search
for the words."
  :group 'w3m
  :type 'boolean)

(defconst w3m-entity-table
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entity '(("nbsp" . " ")
		      ("gt" . ">")
		      ("lt" . "<")
		      ("amp" . "&")
		      ("quot" . "\"")
		      ("apos" . "'")
		      ("circ" . "^")
		      ("tilde" . "~")))
      (puthash (car entity) (cdr entity) table))
    (dolist (entity
	     '(;("nbsp" . 160)
	       ("iexcl" . 161) ("cent" . 162) ("pound" . 163) ("curren" . 164)
	       ("yen" . 165) ("brvbar" . 166) ("sect" . 167) ("uml" . 168)
	       ("copy" . 169) ("ordf" . 170) ("laquo" . 171) ("not" . 172)
	       ("shy" . 173) ("reg" . 174) ("macr" . 175) ("deg" . 176)
	       ("plusmn" . 177) ("sup2" . 178) ("sup3" . 179) ("acute" . 180)
	       ("micro" . 181) ("para" . 182) ("middot" . 183) ("cedil" . 184)
	       ("sup1" . 185) ("ordm" . 186) ("raquo" . 187) ("frac14" . 188)
	       ("frac12" . 189) ("frac34" . 190) ("iquest" . 191)
	       ("Agrave" . 192) ("Aacute" . 193) ("Acirc" . 194)
	       ("Atilde" . 195) ("Auml" . 196) ("Aring" . 197) ("AElig" . 198)
	       ("Ccedil" . 199) ("Egrave" . 200) ("Eacute" . 201)
	       ("Ecirc" . 202) ("Euml" . 203) ("Igrave" . 204) ("Iacute" . 205)
	       ("Icirc" . 206) ("Iuml" . 207) ("ETH"  . 208) ("Ntilde" . 209)
	       ("Ograve" . 210) ("Oacute" . 211) ("Ocirc" . 212)
	       ("Otilde" . 213) ("Ouml" . 214) ("times" . 215) ("Oslash" . 216)
	       ("Ugrave" . 217) ("Uacute" . 218) ("Ucirc" . 219) ("Uuml" . 220)
	       ("Yacute" . 221) ("THORN" . 222) ("szlig" . 223) ("agrave" . 224)
	       ("aacute" . 225) ("acirc" . 226) ("atilde" . 227) ("auml" . 228)
	       ("aring" . 229) ("aelig" . 230) ("ccedil" . 231) ("egrave" . 232)
	       ("eacute" . 233) ("ecirc" . 234) ("euml" . 235) ("igrave" . 236)
	       ("iacute" . 237) ("icirc" . 238) ("iuml" . 239) ("eth" . 240)
	       ("ntilde" . 241) ("ograve" . 242) ("oacute" . 243)
	       ("ocirc" . 244) ("otilde" . 245) ("ouml" . 246) ("divide" . 247)
	       ("oslash" . 248) ("ugrave" . 249) ("uacute" . 250)
	       ("ucirc" . 251) ("uuml" . 252) ("yacute" . 253) ("thorn" . 254)
	       ("yuml" . 255)))
      (puthash (car entity)
	       (char-to-string (make-char 'latin-iso8859-1 (cdr entity)))
	       table))
    (dolist (entity
	     '(("Alpha" . 65) ("Beta" . 66) ("Gamma" . 67) ("Delta" . 68)
	       ("Epsilon" . 69) ("Zeta" . 70) ("Eta" . 71) ("Theta" . 72)
	       ("Iota" . 73) ("Kappa" . 74) ("Lambda" . 75) ("Mu" . 76)
	       ("Nu" . 77) ("Xi" . 78) ("Omicron" . 79) ("Pi" . 80)
	       ("Rho" . 81)	; No ("Sigmaf" . 82)
	       ("Sigma" . 83) ("Tau" . 84) ("Upsilon" . 85) ("Phi" . 86)
	       ("Chi" . 87) ("Psi" . 88) ("Omega" . 89)
	       ("alpha" . 97) ("beta" . 98) ("gamma" . 99) ("delta" . 100)
	       ("epsilon" . 101) ("zeta" . 102) ("eta" . 103) ("theta" . 104)
	       ("iota" . 105) ("kappa" . 106) ("lambda" . 107) ("mu" . 108)
	       ("nu" . 109) ("xi" . 110) ("omicron" . 111) ("pi" . 112)
	       ("rho" . 113) ("sigmaf" . 114) ("sigma" . 115) ("tau" . 116)
	       ("upsilon" . 117) ("phi" . 118) ("chi" . 119) ("psi" . 120)
	       ("omega" . 121)))
      (puthash (car entity)
	       (char-to-string (make-char 'greek-iso8859-7 (cdr entity)))
	       table))
    (when (w3m-mule-unicode-p)
      (let ((latin-extended-a
	     '((32 . (("OElig" . 114) ("oelig" . 115)))
	       (33 . (("Scaron" . 32) ("scaron" . 33) ("Yuml" . 56)))))
	    (latin-extended-b '((33 . (("fnof" . 82)))))
	    ;;(spacing-modifier-letters '(36 . (("circ" . 120) ("tilde" . 124))))
	    (general-punctuation
	     '((114 .
		    (("ensp" . 98) ("emsp" . 99) ("thinsp" . 105) ("zwnj" . 108)
		     ("zwj" . 109) ("lrm" . 110) ("rlm" . 111) ("ndash" . 115)
		     ("mdash" . 116) ("lsquo" . 120) ("rsquo" . 121)
		     ("sbquo" . 122) ("ldquo" . 124) ("rdquo" . 125)
		     ("bdquo" . 126)))
	       (115 .
		    (("dagger" . 32) ("Dagger" . 33) ("permil" . 48)
		     ("lsaquo" . 57) ("rsaquo" . 58)
		     ("bull" . 34) ("hellip" . 38) ("prime" . 50) ("Prime" . 51)
		     ("oline" . 62) ("frasl" . 68)))
	       (116 .
		    (("euro" . 76)))))
	    (greek '((39 . (("thetasym" . 81) ("upsih" . 82) ("piv" . 86)))))
	    (letterlike-symbols
	     '((117 .
		    (("weierp" . 88) ("image" . 81) ("real" . 92)
		     ("trade" . 98) ("alefsym" . 117)))))
	    (arrows
	     '((118 .
		    (("larr" . 112) ("uarr" . 113) ("rarr" . 114) ("darr" . 115)
		     ("harr" . 116)))
	       (119 .
		    (("crarr" . 53) ("lArr" . 80) ("uArr" . 81) ("rArr" . 81)
		     ("dArr" . 83) ("hArr" . 84)))))
	    (mathematical-operators
	     '((120 .
		    (("forall" . 32) ("part" . 34) ("exist" . 35) ("empty" . 37)
		     ("nabla" . 39) ("isin" . 40) ("notin" . 41) ("ni" . 43)
		     ("prod" . 47) ("sum" . 49) ("minus" . 50) ("lowast" . 55)
		     ("radic" . 58) ("prop" . 61) ("infin" . 62) ("ang" . 64)
		     ("and" . 71) ("or" . 72) ("cap" . 73) ("cup" . 74)
		     ("int" . 75) ("there4" . 84) ("sim" . 92) ("cong" . 101)
		     ("asymp" . 104)))
	       (121 .
		    (("ne" . 32) ("equiv" . 33) ("le" . 36) ("ge" . 37)
		     ("sub" . 66) ("sup" . 67) ("nsub" . 68) ("sube" . 70)
		     ("supe" . 71) ("oplus" . 85) ("otimes" . 87)
		     ("perp" . 101)))
	       (122 . (("sdot" . 37)))))
	    (miscellaneous-technical
	     '((122 . (("lceil" . 104) ("rceil" . 105) ("lfloor" . 106)
		       ("rfloor" . 107)))
	       (123 . (("lang" . 41) ("rang" . 42)))))
	    (suit
	     '(("loz" . (34 . 42)) ("spades" . (35 . 96)) ("clubs" . (35 . 99))
	       ("hearts" . (35 . 101)) ("diams" . (35 . 102)))))
	(dolist (entities `(,@latin-extended-a
			    ,@latin-extended-b
			    ,@general-punctuation
			    ,@greek ,@letterlike-symbols ,@arrows
			    ,@mathematical-operators
			    ,@miscellaneous-technical))
	  (let ((code1 (car entities)))
	    (dolist (entity (cdr entities))
	      (puthash (car entity)
		       (char-to-string
			(make-char 'mule-unicode-0100-24ff
				   code1 (cdr entity)))
		       table))))
	(dolist (entity suit)
	  (puthash (car entity)
		   (char-to-string
		    (make-char 'mule-unicode-2500-33ff
			       (car (cdr entity)) (cdr (cdr entity))))
		   table))))
    table)
  "Table of html character entities and values.")

(defvar w3m-extra-numeric-character-reference
  (mapcar
   (lambda (item)
     (cons (car item) (string (w3m-ucs-to-char (cdr item)))))
   '((?\C-m . #x20) ;; [emacs-w3m:12378]
     (#x80 . #x20AC) (#x82 . #x201A) (#x83 . #x0192) (#x84 . #x201E)
     (#x85 . #x2026) (#x86 . #x2020) (#x87 . #x2021) (#x88 . #x02C6)
     (#x89 . #x2030) (#x8A . #x0160) (#x8B . #x2039) (#x8C . #x0152)
     (#x8E . #x017D) (#x91 . #x2018) (#x92 . #x2019) (#x93 . #x201C)
     (#x94 . #x201D) (#x95 . #x2022) (#x96 . #x2013) (#x97 . #x2014)
     (#x98 . #x02DC) (#x99 . #x2122) (#x9A . #x0161) (#x9B . #x203A)
     (#x9C . #x0153) (#x9E . #x017E) (#x9F . #x0178)))
  "*Alist of (numeric . string) pairs for numeric character reference
other than ISO 10646.")

(defconst w3m-entity-reverse-table
  (let ((table (make-hash-table :test 'equal)))
    (maphash (lambda (key val) (puthash val key table))
	     w3m-entity-table)
    table)
  "Revision table of html character entities and values.")

(defconst w3m-entity-regexp
  (let (buf)
    (maphash (lambda (key _val) (push key buf))
	     w3m-entity-table)
    (concat "&\\(" (regexp-opt buf)
	    "\\|#\\(?:[xX][0-9a-fA-F]+\\|[0-9]+\\)\\)\\(\\'\\|[^0-9a-zA-Z]\\)"))
  "Regexp matching html character entities.")

(defconst w3m-encoding-alist
  (eval-when-compile
    (apply 'nconc
	   (mapcar (lambda (elem)
		     (mapcar (lambda (x) (cons x (car elem)))
			     (cdr elem)))
		   '((gzip . ("gzip" "x-gzip" "compress" "x-compress"))
		     (bzip . ("x-bzip" "bzip" "bzip2"))
		     (deflate . ("x-deflate" "deflate"))))))
  "Alist of content encoding types and decoder symbols.
Decoders are specified by `w3m-decoder-alist' (which see).")

(defconst w3m-emacs-w3m-icon "\
R0lGODlhUwAOAPIAAFUq/H8AvwC/AP8AAAAAv79/Af///wAAACH/C05FVFNDQVBFMi4wAwEA
AAAh+QQAIQD/ACwAAAAAUwAOAAADxmi63P4wykmrvXiWvbP/4NIpY2ieUFlSQRssrRG7DGET
DQEAzL5PAoEiSCo0RoOBIblkKmKyV/RFsymsBqzh99vyvBKiYbQaG5vKZFoZfUqhUO0C2613
gUJzsVhy+tkuNG2DWjd0Xw0+iEGMYgJGHWVjbH8KTlAMcThZm1iHEYwakKMOlU2WgFKZUp6d
m3YKdwtiEmRnfZS5qG5Ub6yuVzg+C1xfAES0EbZ7u6fOTlOqrcFzxcSyjRXLqGoLptAo4eLj
5OUNCQAh+QQAIQD/ACwAAAAAUwAOAAADImi63P4wykmrvTjrzbv/YCiOZGmeaKqubOu+cCzP
dG3fagIAIfkEACEA/wAsAAAAAFMADgAAAz5outz+MMpJq7046827/2AYBWSwkAZaimyFpiZs
rm0tvXj82rxT0rmekLE7xYZIRVF5TA5XQWfyJ61ar9hsAgAh+QQAIQD/ACwAAAAAUwAOAAAD
Vmi63P4wykmrvTjrzbv/YBgFZLCQBloyREs0rxiiqVmba6voBi//tKCN5lsUf7OSUEGM9VxO
ZNLR3MV4R6mHxqg+rVrpavktZ8MgpfHKNqLf8Lh8XkkAACH5BAAhAP8ALAAAAABTAA4AAANw
aLrc/jDKSau9OOvNu/9gGAVksJAGWjJESzQEADCyLGJoaurm2io/Q9BgsxFnx5slx9zlhoug
cWpULktNxfMFdHGrtJq1kmNsu2jhFznulE+7oHytoLY1q6w6/RPXZ1N3F1hRXHNRRWx+goyN
jo+QCQAh+QQAIQD/ACwAAAAAUwAOAAADhWi63P4wykmrvTjrzbv/YBgFZLCQBloyREs0BAAw
sjwJgoKHaGr6plVLMTQUDTYbcraU7ESKnvTXOy6KyqzyloMCV77o7+jCMhu1M2797EJ7jOrL
OC+aI2tvBX4a1/8KWoFnC096EitTRIB0S2dJTAA7hocjYI2YZJALNQxslaChoqOkDgkAIfkE
ACEA/wAsAAAAAFMADgAAA6doutz+MMpJq714lr2z/+DSKWNonlBZUkEbLK0RuwxhEw0BAMy+
TwKBIkgqNFaSmOy1fNFsCqhBavj9qjyshGgYIZERpZippC6k1/QVKOwa3UVw2DVWlHHRG37d
8y2CgFwCRh1gbxVKDHd5jFN7WQ+AGoSUJokwTFKajwpqDlwSXm9yLDNkmXibWJBWWQBEoBGi
RSB0Z6m4Z60Lfn+SFLMowsPExcbFCQAh+QQAIQD/ACwAAAAAUwAOAAADxmi63P4wykmrvXiW
vbP/4NIpY2ieUFlSQRssrRG7DGETDQEAzL5PAoEiSCo0RoOBIblkKmKyV/RFsymsBqzh99vy
vBKiYbQaG5vKZFoZfUqhUO0C2613gUJzsVhy+tkuNG2DWjd0Xw0+iEGMYgJGHWVjbH8KTlAM
cThZm1iHEYwakKMOlU2WgFKZUp6dm3YKdwtiEmRnfZS5qG5Ub6yuVzg+C1xfAES0EbZ7u6fO
TlOqrcFzxcSyjRXLqGoLptAo4eLj5OUNCQA7"
  "A small image to be displayed in the about: page.
It is encoded in the optimized interlaced endlessly animated gif format
and base64.")

(defcustom w3m-process-modeline-format " loaded: %s"
  "Format used when displaying the progress of the external w3m process.
The value should be a format string or a function that takes a string.
It shows a percentage of the data loaded from the web server."
  :group 'w3m
  :type '(choice :format "%{%t%}:\n%[Value Menu%] %v"
		 (string :tag "Format")
		 (function :format
			   "%t: %v   (Note: it takes a string)\n")))

(defcustom w3m-ignored-image-url-regexp nil
  "Regexp matching image urls which you don't want to view.
It is effective even if `w3m-display-inline-images' is non-nil.
For instance, the value \"\\\\`https?://www\\.google\\.com/\" conceals
Google's logo and navigation images, but display YouTube's
thumbnail."
  :group 'w3m
  :type '(radio (const :format "Accept any image\n" nil)
		(regexp :format "URL regexp: %v")))

(defvar w3m-modeline-process-status-on "<PRC>"
  "Modeline control for displaying the status when the process is running.
The value will be modified for displaying the graphic icon.")

(defvar w3m-modeline-image-status-on "[IMG]"
  "Modeline control to display the status when inline images are turned on.
The value will be modified for displaying the graphic icon.")

(defvar w3m-modeline-status-off "[ - ]"
  "Modeline control for displaying the status for the default.
The value will be modified for displaying the graphic icon.")

(defvar w3m-modeline-ssl-image-status-on "[IMG(SSL)]"
  "Modeline control for displaying the status when images and SSL are on.
The value will be modified for displaying the graphic icon.")

(defvar w3m-modeline-ssl-status-off "[SSL]"
  "Modeline control for displaying the status when SSL is turned on.
The value will be modified for displaying the graphic icon.")

(defvar w3m-modeline-separator " / "
  "String used to separate a status and a title in the modeline.")

(defvar w3m-modeline-favicon nil
  "Modeline control for displaying a favicon.
This variable will be made buffer-local.")

(defvar w3m-favicon-image nil
  "Favicon image of the page.
This variable will be made buffer-local")

(defvar w3m-current-process nil
  "List of external processes running in the buffer.
Mainly used for a flag to say whether an external process runs.
This variable will be made buffer-local.")
(make-variable-buffer-local 'w3m-current-process)

(defvar w3m-refresh-timer nil
  "Variable used to keep a timer object for refreshing a page.
It will be supplied by the REFRESH attribute in the META tag, and made
buffer-local in each emacs-w3m buffer.")
(make-variable-buffer-local 'w3m-refresh-timer)

(defvar w3m-mail-user-agents '(gnus-user-agent
			       message-user-agent
			       mew-user-agent
			       vm-user-agent
			       wl-user-agent)
  "List of mail user agents that `w3m-mail' supports.
See also w3m-mail.el.")

(defvar w3m-current-base-url nil
  "URL specified by <base...> tag in <head> element of the page source.")
(defvar w3m-current-forms nil
  "Variable used to keep forms data for the current emacs-w3m buffer.")
(defvar w3m-current-coding-system nil
  "Coding system used when decoding the current emacs-w3m buffer.")
(defvar w3m-current-content-charset nil
  "Content charset of the page specified by the server or the META tag.")
(defvar w3m-current-title nil
  "Title of a page visiting in the current buffer.")
(defvar w3m-current-url nil
  "A url of a page visiting in the current buffer.")
(defvar w3m-icon-data nil
  "Cons of icon data and its image-type for the current emacs-w3m buffer.
It is used for favicon data.  The type is often `ico'.")
(defvar w3m-next-url nil
  "URL as the next document in the author-defined sequence.")
(defvar w3m-previous-url nil
  "URL as the previous document in the author-defined sequence.")
(defvar w3m-start-url nil
  "URL as the first document in the author-defined sequence.")
(defvar w3m-contents-url nil
  "URL as the table of contents for the current page.")
(defvar w3m-current-refresh nil
  "Cons of number of seconds and a url specified by the REFRESH attribute.")
(defvar w3m-current-ssl nil
  "SSL certification indicator for the current emacs-w3m buffer.")
(defvar w3m-name-anchor-from-hist nil
  "List of the points of where `w3m-search-name-anchor' come from.")

(make-variable-buffer-local 'w3m-current-url)
(make-variable-buffer-local 'w3m-current-base-url)
(make-variable-buffer-local 'w3m-current-title)
(make-variable-buffer-local 'w3m-current-forms)
(make-variable-buffer-local 'w3m-current-coding-system)
(make-variable-buffer-local 'w3m-current-content-charset)
(make-variable-buffer-local 'w3m-icon-data)
(make-variable-buffer-local 'w3m-next-url)
(make-variable-buffer-local 'w3m-previous-url)
(make-variable-buffer-local 'w3m-start-url)
(make-variable-buffer-local 'w3m-contents-url)
(make-variable-buffer-local 'w3m-current-refresh)
(make-variable-buffer-local 'w3m-current-ssl)
(make-variable-buffer-local 'w3m-name-anchor-from-hist)

(defvar w3m-last-window-width nil
  "Variable that keeps the last window width of the w3m-mode window.
`w3m-redisplay-pages-automatically' uses this.")
(make-variable-buffer-local 'w3m-last-window-width)

(defun w3m-clear-local-variables ()
  (setq w3m-current-url nil
	w3m-current-base-url nil
	w3m-current-title nil
	w3m-current-coding-system nil
	w3m-current-content-charset nil
	w3m-icon-data nil
	w3m-next-url nil
	w3m-previous-url nil
	w3m-start-url nil
	w3m-contents-url nil
	w3m-current-refresh nil
	w3m-current-ssl nil
	w3m-name-anchor-from-hist nil))

(defun w3m-copy-local-variables (from-buffer)
  (let (url base title cs char icon next prev start toc refresh ssl)
    (with-current-buffer from-buffer
      (setq url w3m-current-url
	    base w3m-current-base-url
	    title w3m-current-title
	    cs w3m-current-coding-system
	    char w3m-current-content-charset
	    icon w3m-icon-data
	    next w3m-next-url
	    prev w3m-previous-url
	    start w3m-start-url
	    toc w3m-contents-url
	    refresh w3m-current-refresh
	    ssl w3m-current-ssl))
    (setq w3m-current-url url
	  w3m-current-base-url base
	  w3m-current-title title
	  w3m-current-coding-system cs
	  w3m-current-content-charset char
	  w3m-icon-data icon
	  w3m-next-url next
	  w3m-previous-url prev
	  w3m-start-url start
	  w3m-contents-url toc
	  w3m-current-refresh refresh
	  w3m-current-ssl ssl)))

(defcustom w3m-verbose nil
  "If non-nil, `w3m-message' will log echo messages in *Messages* buffer.
Echo messages will be displayed no matter what this variable is unless
`w3m-message-silent' is not temprarily bound to a non-nil value."
  :group 'w3m
  :type 'boolean)

(defvar w3m-safe-url-regexp nil
  "Regexp matching urls which are considered to be safe.
The nil value means all urls are considered to be safe.

Note: The value, that might be bound to a certain value while rendering
contents, will be held by the `w3m-safe-url-regexp' text property that
is set over the rendered contents in a buffer.  So, programs that use
the value to test whether a url of a link in a buffer is safe should
use the value of the text property, not the value of this variable.
See the function definitions of `w3m-toggle-inline-image',
`w3m-toggle-inline-images', `w3m-safe-view-this-url', and
`w3m-mouse-safe-view-this-url'.")

(defvar w3m-current-buffer nil)
(defvar w3m-input-url-history nil)

(defvar w3m-cache-buffer nil
  "A buffer that keeps the caches of the web contents in order.")
(defvar w3m-cache-articles nil
  "An alist of url and pointer in cached order.
The pointer is a cons of beg and end markers in `w3m-cache-buffer'.
This variable will be made buffer-local in `w3m-cache-buffer'.")
(defvar w3m-cache-hashtb nil
  "Hash table used to cache headers associated with urls.")
;; Why `w3m-cache-hashtb' does not cache web contents along with headers
;; is that it is hard to know the oldest cache in the hash table and to
;; know the number of caches in the hash table is not so efficient.

(defvar w3m-http-status-alist
  '((400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Time-out")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Request Entity Too Large")
    (414 . "Request-URI Too Large")
    (415 . "Unsupported Media Type")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Time-out")
    (505 . "HTTP Version not supported"))
  "Alist of HTTP status codes.")

(defvar w3m-http-status nil)

(defvar w3m-arrived-db nil
  "Hash table, the arrived URLs database.
The name of each symbol represents a url, the arrival time in the
Emacs style (a list of three integers) is stored as the value, and
informations including a title, a modification time, a content charset
and a content type are stored as the properties of the symbol.  The
nil value means it has not been initialized.")

(defvar w3m-arrived-setup-functions nil
  "Hook functions run after setting up the arrived URLs database.")
(defvar w3m-arrived-shutdown-functions nil
  "Hook functions run after saving the arrived URLs database.")

(defvar w3m-image-type-alist
  (let ((types (and (boundp 'image-types)
		    (delq 'imagemagick (copy-sequence image-types)))))
    (when types
      (delq nil
	    (nconc
	     (mapcar (lambda (type) (cons (format "image/%s" type) type))
		     types)
	     (when (memq 'xbm types) '(("image/x-xbm" . xbm)))
	     (when (memq 'xpm types) '(("image/x-xpm" . xpm)))))))
  "Alist of content types and image types defined as the Emacs features.
The content types not listed in this variable, like image/webp, might
be able to display if image-converter.el and some converter program are
available, or ImageMagick is built-in in Emacs or installed.")

(defconst w3m-toolbar-buttons
  '("back" "parent" "forward" "reload" "open" "home" "search" "image"
    "copy" "weather" "antenna" "save" "history" "db-history")
  "List of prefix strings for the toolbar buttons.")

(defconst w3m-toolbar
  (if (equal "Japanese" w3m-language)
      (let ((a "ア"))
	`([w3m-toolbar-back-icon w3m-view-previous-page
				 (w3m-history-previous-link-available-p)
				 "前のページに戻る"]
	  [w3m-toolbar-parent-icon w3m-view-parent-page
				   (w3m-parent-page-available-p)
				   "上のディレクトリへ移動する"]
	  [w3m-toolbar-forward-icon w3m-view-next-page
				    (w3m-history-next-link-available-p)
				    "次のページに進む"]
	  [w3m-toolbar-reload-icon w3m-reload-this-page
				   w3m-current-url
				   "サーバからページをもう一度読み込む"]
	  [w3m-toolbar-open-icon w3m-goto-url t "URL を入力してページを開く"]
	  [w3m-toolbar-home-icon w3m-gohome w3m-home-page
				 "ホームページへジャンプ"]
	  [w3m-toolbar-search-icon w3m-search t "インターネット上を検索"]
	  [w3m-toolbar-image-icon w3m-toggle-inline-images t
				  "画像の表示をトグルする"]
	  [w3m-toolbar-copy-icon w3m-copy-buffer t
				 "このセッションのコピーを作る"]
	  [w3m-toolbar-weather-icon w3m-weather t "天気予報を見る"]
	  [w3m-toolbar-antenna-icon w3m-antenna t
				    ,(concat a "ンテナで受信する")]
	  [w3m-toolbar-save-icon w3m-save-buffer (w3m-url-savable-p)
				 "セーブ..."]
	  [w3m-toolbar-history-icon w3m-history t "ヒストリー"]
	  [w3m-toolbar-db-history-icon w3m-db-history t
				       "過去に訪問した URL の履歴を見る"]))
    '([w3m-toolbar-back-icon w3m-view-previous-page
			     (w3m-history-previous-link-available-p)
			     "Back to Previous Page"]
      [w3m-toolbar-parent-icon w3m-view-parent-page
			       (w3m-parent-page-available-p)
			       "View the parent page"]
      [w3m-toolbar-forward-icon w3m-view-next-page
				(w3m-history-next-link-available-p)
				"Forward to Next Page"]
      [w3m-toolbar-reload-icon w3m-reload-this-page
			       w3m-current-url
			       "Reload This Page"]
      [w3m-toolbar-open-icon w3m-goto-url t "Go to..."]
      [w3m-toolbar-home-icon w3m-gohome w3m-home-page "Go to Home Page"]
      [w3m-toolbar-search-icon w3m-search t "Search the Internet"]
      [w3m-toolbar-image-icon w3m-toggle-inline-images t "Toggle Images"]
      [w3m-toolbar-copy-icon w3m-copy-buffer t "Make a Copy of This Session"]
      [w3m-toolbar-weather-icon w3m-weather t "Weather Forecast"]
      [w3m-toolbar-antenna-icon w3m-antenna t "Investigate with Antenna"]
      [w3m-toolbar-save-icon w3m-save-buffer (w3m-url-savable-p) "Save to..."]
      [w3m-toolbar-history-icon w3m-history t "History"]
      [w3m-toolbar-db-history-icon w3m-db-history t "View Arrived URLs"]))
  "Toolbar definition for emacs-w3m.")

;; "View" is page viewing
;; "Show" is link list showing
(defconst w3m-menubar
  (let ((a (when w3m-use-japanese-menu "ア")))
    `("w3m"
      [,(w3m-make-menu-item "この URL を新しいセッションで開く"
			    "Open This URL in a new session")
       w3m-view-this-url-new-session (or (w3m-anchor) (w3m-image))]
      [,(w3m-make-menu-item "この URL をダウンロードする" "Download This URL")
       w3m-download-this-url (or (w3m-anchor) (w3m-image))]
      [,(w3m-make-menu-item "ダウンロード..." "Download to...")
       w3m-download t]
      [,(w3m-make-menu-item "この URL をセーブする..." "Save to...")
       w3m-save-buffer (w3m-url-savable-p)]
      "----" ;; separator
      [,(w3m-make-menu-item "前のページに戻る" "Back to Previous Page")
       w3m-view-previous-page
       (w3m-history-previous-link-available-p)]
      [,(w3m-make-menu-item "次のページに移動する" "Forward to Next Page")
       w3m-view-next-page
       (w3m-history-next-link-available-p)]
      [,(w3m-make-menu-item "上の階層に移動する" "Up to Parent Page")
       w3m-view-parent-page
       (w3m-parent-page-available-p)]
      "----" ;; separator
      [,(w3m-make-menu-item "現在のページを browse-url で開く"
			    "Open The Current Page using browse-url")
       w3m-view-url-with-browse-url w3m-current-url]
      [,(w3m-make-menu-item "このリンクを browse-url で開く"
			    "Open This Link using browse-url")
       w3m-view-url-with-browse-url (or (w3m-anchor) (w3m-image))]
      [,(w3m-make-menu-item "このページのソースをコマンドに送る..."
			    "Pipe Page Source to Command...")
       w3m-pipe-source  w3m-current-url]
      "----" ;; separator
      (,(w3m-make-menu-item "再表示" "Redisplay")
       [,(w3m-make-menu-item "このページを再取得する" "Reload This Page")
	w3m-reload-this-page w3m-current-url]
       [,(w3m-make-menu-item "すべてのページを再取得する" "Reload All Pages")
	w3m-reload-all-pages (cdr (w3m-list-buffers))]
       "----" ;; separator
       [,(w3m-make-menu-item "画像表示の切替(全部)" "Toggle Images")
	w3m-toggle-inline-images (display-images-p)]
       [,(w3m-make-menu-item "画像表示の切替(この画像)" "Toggle This Image")
	w3m-toggle-inline-image (w3m-image)]
       [,(w3m-make-menu-item "画像表示を止める" "Turn off Images")
	w3m-turnoff-inline-images (display-images-p)]
       "----" ;; separator
       [,(w3m-make-menu-item "再描画する" "Redisplay This Page")
	w3m-redisplay-this-page w3m-current-url]
       [,(w3m-make-menu-item "Charset を指定して再描画する"
			     "Redisplay This Page with Charset")
	w3m-redisplay-with-charset w3m-current-url]
       [,(w3m-make-menu-item "Content-type を指定して再描画する"
			     "Redisplay This Page with Content-type")
	w3m-redisplay-with-content-type w3m-current-url]
       [,(w3m-make-menu-item "指定した Charset と Content-type を破棄する"
			     "Reset Charset and Content-type")
	w3m-redisplay-and-reset w3m-current-url]
       ) ;; end redisplay
      [,(w3m-make-menu-item "ホームページへ移動" "Go to Home Page")
       w3m-gohome w3m-home-page]
      (,(w3m-make-menu-item "ブックマーク" "Bookmark")
       [,(w3m-make-menu-item "ブックマークを表示" "View Bookmark")
	w3m-bookmark-view t]
       [,(w3m-make-menu-item "新しいセッションでブックマークを表示"
			     "View Bookmark in a New Session")
	w3m-bookmark-view-new-session t])
      [,(w3m-make-menu-item "移動..." "Go to...")
       w3m-goto-url t]
      "----" ;; separator
      (,(w3m-make-menu-item "履歴" "History")
       [,(w3m-make-menu-item "木構造で履歴を表示" "Show a Visited URLs Tree")
	w3m-history t]
       [,(w3m-make-menu-item "リストで履歴を表示" "Show an Arrived URLs List")
	w3m-db-history t]
       ) ;; end history
      [,(w3m-make-menu-item "インターネットでの検索..."
			    "Search the Internet...")
       w3m-search t]
      [,(w3m-make-menu-item "新しいセッションで検索..."
			    "Search the Internet in a New Session...")
       w3m-search-new-session t]
      [,(w3m-make-menu-item "天気予報" "Weather Forecast")
       w3m-weather t]
      [,(w3m-make-menu-item (concat a "ンテナで取得")
			    "Investigate with Antenna")
       w3m-antenna t]
      (,(w3m-make-menu-item "ヘルプ" "Resource")
       [,(w3m-make-menu-item "プロセスを中止する" "Cancel Process")
	w3m-process-stop w3m-current-process]
       [,(w3m-make-menu-item "ソースを見る" "View Source")
	w3m-view-source t]
       [,(w3m-make-menu-item "ヘッダーを見る" "View Header")
	w3m-view-header t]
       ) ;; end resource
      "----" ;; separator
      [,(w3m-make-menu-item "このページをメールで送る" "Mail this page")
       w3m-mail (memq mail-user-agent w3m-mail-user-agents)]
      "----" ;; separator
      [,(w3m-make-menu-item "バグレポートを送る" "Send a Bug Report")
       report-emacs-w3m-bug t]
      "----" ;; separator
      [,(w3m-make-menu-item "この URL を表示する" "Print the Current URL")
       w3m-print-current-url t]
      [,(w3m-make-menu-item "w3m を閉じる" "Close w3m")
       w3m-close-window t]
      [,(w3m-make-menu-item "w3m を終了する" "Quit w3m")
       w3m-quit t]
      )) ;; end w3m
  "Menubar definition for emacs-w3m.")

(defvar w3m-rmouse-menubar
  `("w3m"
    [,(w3m-make-menu-item "前のページに戻る" "Back to Previous Page")
     w3m-view-previous-page
     (w3m-history-previous-link-available-p)]
    [,(w3m-make-menu-item "次のページに移動する" "Forward to Next Page")
     w3m-view-next-page
     (w3m-history-next-link-available-p)]
    [,(w3m-make-menu-item "上の階層に移動する" "Up to Parent Page")
     w3m-view-parent-page
     (w3m-parent-page-available-p)]
    "----" ;; separator
    [,(w3m-make-menu-item "このページを再取得する" "Reload This Page")
     w3m-reload-this-page w3m-current-url]
    [,(w3m-make-menu-item "すべてのページを再取得する" "Reload All Pages")
     w3m-reload-all-pages (cdr (w3m-list-buffers))]
    [,(w3m-make-menu-item "プロセスを中止する" "Cancel Process")
     w3m-process-stop w3m-current-process])
  "*Menubar for click the right mouse button.")

(defvar w3m-cid-retrieve-function-alist nil)
(defvar w3m-force-redisplay t)

(defvar w3m-work-buffer-list nil)
(defconst w3m-work-buffer-name " *w3m-work*")
(defconst w3m-select-buffer-name " *w3m buffers*")

(defconst w3m-dump-head-source-command-arguments
  (cond ((eq w3m-type 'w3mmee)
	 (list "-dump=extra,head,source"))
	(t
	 (list
	  '(if w3m-accept-languages
	       '("-o"
		 (concat "accept_language="
			 (mapconcat 'identity w3m-accept-languages ","))))
	  "-dump_extra")))
  "Arguments passed to the w3m command to run \"dump_extra\".")

(defvar w3m-halfdump-command nil
  "Alternative w3m command used to run \"halfdump\".
If it is nil, the command specified to `w3m-command' is used.")

(defconst w3m-halfdump-command-arguments
  (if (eq w3m-type 'w3mmee)
      (list '(if w3m-treat-image-size
		 "-dump=half-buffer,single-row-image"
	       "-dump=half-buffer")
	    '(if (eq w3m-input-coding-system 'ctext)
		 (list "-I" "x-ctext")
	       (when (and (eq w3m-input-coding-system 'binary)
			  charset)
		 (list "-I" 'charset)))
	    "-o" "concurrent=0")
    (list "-halfdump"
	  "-o" "ext_halfdump=1"
	  "-o" "strict_iso2022=0"
	  "-o" "fix_width_conv=1"
	  "-o" "use_jisx0201=0"
	  "-o" "ucs_conv=1"
	  '(if (eq w3m-input-coding-system 'binary)
	       (if charset (list "-I" 'charset))
	     (list "-I" (cond
			 ((eq w3m-input-coding-system 'utf-8)
			  "UTF-8")
			 ((eq w3m-input-coding-system 'iso-8859-1)
			  "ISO-8859-1")
			 (t
			  "ISO-2022-JP-2"))))
	  "-O"
	  '(cond
	    ((eq w3m-output-coding-system 'utf-8)
	     "UTF-8")
	    ((eq w3m-output-coding-system 'iso-8859-1)
	     "ISO-8859-1")
	    (t
	     "ISO-2022-JP-2"))))
  "Arguments passed to the w3m command to run \"halfdump\".")

(defconst w3m-halfdump-command-common-arguments
  (list "-T" "text/html" "-t" tab-width "-cols" '(w3m-display-width)
	'(cond
	  ((and (eq w3m-display-ins-del 'fontify) (display-graphic-p))
	   (list "-o" "display_ins_del=2"))
	  ((or (eq w3m-display-ins-del 'tag)
	       (and (eq w3m-display-ins-del 'fontify)
		    (not (display-graphic-p))))
	   (list "-o" "display_ins_del=1"))))
  "Arguments used in common by the w3m command variants to run \"halfdump\".")

(defconst w3m-arrived-ignored-regexp
  "\\`about:\\(?://\\(?:header\\|source\\|history\\|\
db-history\\|antenna\\|namazu\\|dtree\\)/.*\\)?\\'\
\\|\\`about:/*blank/?\\'"
  "Regexp matching urls which aren't stored in the arrived URLs database.")

(defconst w3m-history-ignored-regexp
  "\\`about:\\(?://\\(?:header\\|source\\|history\\|\
db-history\\|antenna\\|namazu\\|dtree\\)/.*\\)?\\'\
\\|\\`about:/*blank/?\\'"
  "Regexp matching urls which aren't stored in the history.")

(defvar w3m-mode-map nil "Keymap for emacs-w3m buffers.")

(defvar w3m-url-completion-map (let ((map (make-sparse-keymap)))
				 (define-key map " " 'self-insert-command)
				 (define-key map "?" 'self-insert-command)
				 map)
  "*Keymap that overrides the default keymap when `w3m-input-url' runs.
By default SPC and \"?\" are bound to `self-insert-command' since those
are common url characters.  Use TAB for completion.")

(defvar w3m-link-map nil "Keymap used on links.")

(defvar w3m-doc-view-map nil
  "Keymap used in `doc-view-mode' that emacs-w3m launches.
`doc-view-mode-map' gets to be its parent keymap.")

(defvar w3m-mode-setup-functions nil
  "Hook functions run after setting up the `w3m-mode'.")
(defvar w3m-display-functions nil
  "Hook functions run after displaying pages in emacs-w3m buffers.
Each function is called with a url string as the argument.  This hook
is evaluated just before evaluating `w3m-display-hook'.")

(defvar w3m-load-hook nil
  "*Hook run after loading the w3m.elc module.
It is not recommended that you use this hook instead of writing into
`w3m-init-file' for customization.")


;; Generic functions:
(defun w3m-url-to-file-name (url)
  "Return the file name which is pointed to by URL.
When URL does not point to any local files, it returns nil.  The
actual performance of this function is to strip off the scheme part
and the net_loc part from URL.  It is meaningless to give an argument
whose net_loc part is not empty, a null string or the localhost name
to this function."
  (cond
   ((string-match "\\`\\(\\(file:/\\{0,2\\}\\)\\|about://dtree\\)/" url)
    (setq url (substring url (match-end 1)))
    (when (and (match-beginning 2) ;; file:
	       (< (match-end 2) 7) ;; file:// or file:/
	       (string-match "\\`\\(/[^/]+[^/:|]\\)/" url))
      (cond ((file-directory-p (match-string 0 url))
	     ) ;; The directory "/hostname/" exists.
	    ((string-match (concat "\\`/\\(localhost\\|127\\.0\\.0\\.1\\|"
				   (regexp-quote (system-name)) "\\)/")
			   url)
	     ;; Strip the localhost name.
	     (setq url (substring url (match-end 1))))
	    (t
	     ;; Make it a Tramp url: /hostname:/...
	     ;; See `tramp-default-method' and `tramp-default-method-alist'.
	     (setq url (concat (substring url 0 (match-end 1))
			       ":"
			       (substring url (match-end 1)))))))
    ;; Process abs_path part in Windows.
    (when (and w3m-treat-drive-letter
	       (string-match
		"\\`/\\(?:\\([a-zA-Z]\\)[|:]?\\|cygdrive/\\([a-zA-Z]\\)\\)/"
		url))
      (setq url (concat (or (match-string 1 url) (match-string 2 url))
			":/"
			(substring url (match-end 0)))))
    (if (string-match "\\`/[^/:]\\{2,\\}:/" url)
	;; Don't check for a Tramp url.
	url
      (if (file-exists-p url)
	  url
	(let ((x (w3m-url-decode-string url w3m-file-name-coding-system)))
	  (if (file-exists-p x) x url)))))
   ((string-match "\\`\\(?:[~/]\\|[a-zA-Z]:/\\|\\.\\.?/\\)" url) url)
   (t
    (catch 'found-file
      (dolist (pair w3m-url-local-directory-alist)
	(and (string-match (concat "\\`"
				   (regexp-quote
				    (file-name-as-directory (car pair))))
			   url)
	     (let ((file (expand-file-name (substring url (match-end 0))
					   (cdr pair))))
	       (when (or (file-exists-p file)
			 (file-exists-p
			  (setq file (w3m-url-decode-string
				      file w3m-file-name-coding-system))))
		 (throw 'found-file file)))))))))

(defun w3m-expand-file-name-as-url (file &optional directory)
  "Return a url string which points to the FILE.
Optional DIRECTORY is a directory to start with if FILE is relative
(i.e., FILE doesn't start with slash).  It defaults to the current
directory."
  (setq file (expand-file-name file directory))
  (concat "file://"
	  (if (string-match "\\`\\([a-zA-Z]\\):" file)
	      (format (if w3m-use-cygdrive "/cygdrive/%s%s" "/%s|%s")
		      (match-string 1 file)
		      (substring file (match-end 0)))
	    file)))

;;; Managing the arrived URLs database:
(defmacro w3m-arrived-intern (url &optional soft)
  "Look up URL in hash table `w3m-arrived-db' and return its symbol.
If SOFT is nil, create a key for URL in `w3m-arrived-db' if not exists.
If SOFT is non-nil, return nil if URL is not found in `w3m-arrived-db'.
In `w3m-arrived-db' URL is normalized so as not to have the last `/'."
  (let* ((url `(let* ((url ,url)
		      (len (length url)))
		 (if (and (not (zerop len))
			  (eq (aref url (1- len)) ?/))
		     (setq url (substring url 0 -1)))))
	 (get '(gethash url w3m-arrived-db))
	 (put `(or ,get (puthash url (make-symbol url) w3m-arrived-db))))
    (cond ((and (not (consp soft)) soft)
	   ;; SOFT is neither a function form nor nil.
	   `(,@url ,get))
	  ((not (or (consp soft) soft))
	   ;; SOFT is neither a function form nor a non-nil symbol.
	   `(,@url ,put))
	  (t ;; SOFT looks like a function form.
	   `(,@url (if ,soft ,get ,put))))))

(defun w3m-arrived-add (url &optional title modification-time
			    arrival-time content-charset content-type)
  "Add URL to the arrived URLs database.
Optional TITLE, MODIFICATION-TIME, ARRIVAL-TIME, CONTENT-CHARSET and
CONTENT-TYPE are also be added."
  (unless (string-match w3m-arrived-ignored-regexp url)
    (let ((ident (w3m-arrived-intern url)))
      (if (string-match "\\`\\([^#]+\\)#" url)
	  (w3m-arrived-add (substring url 0 (match-end 1))
			   title modification-time arrival-time
			   content-charset content-type)
	(when title
	  (put ident 'title title))
	(when modification-time
	  (put ident 'last-modified modification-time))
	(when content-charset
	  (put ident 'content-charset content-charset))
	(when content-type
	  (put ident 'content-type content-type)))
      (set ident arrival-time))))

(defun w3m-arrived-p (url)
  "Return non-nil if a page of URL has arrived."
  (or (string-match w3m-arrived-ignored-regexp url)
      (w3m-arrived-intern url t)))

(defun w3m-arrived-time (url)
  "Return the arrival time of a page of URL if it has arrived.
Otherwise return nil."
  (let ((v (w3m-arrived-intern url t)))
    (and v (symbol-value v))))
(gv-define-setter w3m-arrived-time (arrived-time url)
  `(w3m-arrived-add ,url nil nil ,arrived-time))

(defun w3m-arrived-put (url property value)
  "Store VALUE in the arrived URLs database as the PROPERTY of URL.
Return VALUE if a page of URL has arrived.  Otherwise, VALUE is
ignored and return nil."
  (let ((symbol (w3m-arrived-intern url t)))
    (and symbol (put symbol property value))))

(defun w3m-arrived-get (url property)
  "Return the value of URL's PROPERTY stored in the arrived URLs database.
If a page of URL has not arrived, return nil."
  (let ((symbol (w3m-arrived-intern url t)))
    (and symbol (get symbol property))))

(gv-define-simple-setter w3m-arrived-get w3m-arrived-put)

(defmacro w3m-arrived-title (url)
  "Return the title of URL having stored in the arrived URLs database."
  `(w3m-arrived-get ,url 'title))

(defmacro w3m-arrived-last-modified (url)
  "Return the mod time of URL having stored in the arrived URLs database.
If a page of URL has not arrived yet, return nil."
  `(w3m-arrived-get ,url 'last-modified))

(defmacro w3m-arrived-content-charset (url)
  "Return the content charset of URL stored in the arrived URLs database.
If it has not been specified or a page of URL has not arrived yet,
return nil."
  `(w3m-arrived-get ,url 'content-charset))

(defmacro w3m-arrived-content-type (url)
  "Return the content type of URL stored in the arrived URLs database.
If it has not been specified or a page of URL has not arrived yet,
return nil."
  `(w3m-arrived-get ,url 'content-type))

(defun w3m-arrived-load-list ()
  "Load the arrived URLs database file.
The file is specified by `w3m-arrived-file'.  If the data is in old
format, they will simply be ignored."
  (let ((list (w3m-load-list w3m-arrived-file)))
    (when (or
	   ;; Before the revision 1.120, every element of the list was
	   ;; a string that represented an arrived URL.
	   (stringp (car list))
	   ;; Before the revision 1.135, every element was a cons
	   ;; cell: its car kept a URL, and its cdr kept a time when
	   ;; the URL was arrived.
	   ;; Before the revision 1.178, every element was a 4-tuple
	   ;; that consisted of a URL, a title, a modification time,
	   ;; and an arrival time.
	   ;; An element of the modern database is a 6-tuple that
	   ;; consisted of a URL, a title, a modification time, an
	   ;; arrival time, a charset, and a content type.
	   ;; Thus, the following condition eliminates the revision
	   ;; 1.177 and olders.
	   (<= (length (car list)) 4))
      (setq list nil)
      (when (file-exists-p w3m-arrived-file)
	(delete-file w3m-arrived-file)))
    list))

(defun w3m-arrived-setup ()
  "Load the arrived URLs database file and set up the hashed database.
It is performed only when `w3m-arrived-db' has not been initialize yet.
The file is specified by `w3m-arrived-file'.  This function also sets
up `w3m-input-url-history'."
  (unless w3m-arrived-db
    (setq w3m-arrived-db (make-hash-table :test 'equal))
    (let ((list (w3m-arrived-load-list)))
      (dolist (elem list)
	;; Ignore an element that lacks an arrival time information.
	(when (nth 3 elem)
	  (w3m-arrived-add (if (string-match "\\`/" (car elem))
			       (w3m-expand-file-name-as-url (car elem))
			     (car elem))
			   (nth 1 elem)
			   (nth 2 elem)
			   (nth 3 elem)
			   (when (stringp (nth 4 elem)) (nth 4 elem))
			   (nth 5 elem))))
      (unless w3m-input-url-history
	(setq w3m-input-url-history
	      (let ((result))
		(dolist (category (w3m-bookmark-iterator))
		  (dolist (entry (cdr category))
		    ;; bookmark url
		    (push (car entry) result)
		    ;; bookmark title
		    (push (replace-regexp-in-string "\\`[\s\t]+" "" (cdr entry))
			  result)))
		(nconc (mapcar (function car) list) ;; w3m-input-url-history
		       result)))))
    (run-hooks 'w3m-arrived-setup-functions)))

(defun w3m-arrived-shutdown ()
  "Save the arrived URLs database in the file.
The database `w3m-arrived-db' will be cleared after saving.  The file
is specified by `w3m-arrived-file'."
  (when w3m-arrived-db
    ;; Don't error out no matter what happens
    ;; since `kill-emacs-hook' runs this function.
    (condition-case err
	(progn
	  ;; Re-read the database file, and if there are data which another
	  ;; Emacs process registered, merge them to the current database.
	  (dolist (elem (w3m-arrived-load-list))
	    (when (w3m-time-newer-p (nth 3 elem) (w3m-arrived-time (car elem)))
	      (w3m-arrived-add (if (string-match "\\`/" (car elem))
				   (w3m-expand-file-name-as-url (car elem))
				 (car elem))
			       (nth 1 elem)
			       (nth 2 elem)
			       (nth 3 elem)
			       (when (stringp (nth 4 elem)) (nth 4 elem))
			       (nth 5 elem))))
	  ;; Convert current database to a list.
	  (let (list)
	    (maphash
	     (lambda (_key sym)
	       (and sym
		    (boundp sym)
		    (symbol-value sym) ; Ignore an entry lacks an arrival time.
		    (push (list (symbol-name sym)
				(get sym 'title)
				(get sym 'last-modified)
				(symbol-value sym)
				(get sym 'content-charset)
				(get sym 'content-type))
			  list)))
	     w3m-arrived-db)
	    (w3m-save-list w3m-arrived-file
			   (w3m-sub-list
			    (sort list
				  (lambda (a b)
				    (if (equal (nth 3 a) (nth 3 b))
					(string< (car a) (car b))
				      (w3m-time-newer-p (nth 3 a) (nth 3 b)))))
			    w3m-keep-arrived-urls)
			   nil t))
	  (setq w3m-arrived-db nil)
	  (run-hooks 'w3m-arrived-shutdown-functions))
      (error
       (message "Error while running w3m-arrived-shutdown: %s"
		(error-message-string err))))))

(add-hook 'kill-emacs-hook 'w3m-arrived-shutdown)
(add-hook 'w3m-arrived-shutdown-functions 'w3m-cleanup-temp-files)

;;; Generic macros and inline functions:
(defun w3m-attributes (url &optional no-cache handler)
  "Return a list of attributes corresponding to URL.
Return nil if it failed in retrieving of the header.
Otherwise, return a list which includes the following elements:

 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.

If the optional argument NO-CACHE is non-nil, cache is not used."
  (if (not handler)
      (condition-case nil
	  (w3m-process-with-wait-handler
	    (w3m-attributes url no-cache handler))
	(w3m-process-timeout nil))
    (setq url (w3m-url-strip-fragment url))
    (cond
     ((string= "about://emacs-w3m.gif" url)
      (list "image/gif" nil nil nil nil url url))
     ((string-match "\\`about://source/" url)
      (let ((src (substring url (match-end 0))))
	(w3m-process-do
	    (attrs (w3m-attributes src no-cache handler))
	  (list "text/plain"
		(or (w3m-arrived-content-charset (w3m-url-strip-authinfo src))
		    (cadr attrs))
		(nth 2 attrs)
		(nth 3 attrs)
		(nth 4 attrs)
		(concat "about://source/" (nth 5 attrs))))))
     ((string-match "\\`about:" url)
      (list "text/html" w3m-coding-system nil nil nil url))
     ((string-match "\\`cid:" url)
      (let ((w3m-current-buffer (current-buffer)))
	(w3m-process-do-with-temp-buffer
	    (type (w3m-cid-retrieve url nil nil))
	  (list type nil nil nil nil url url))))
     ((string-match "\\`data:" url)
      (let ((w3m-current-buffer (current-buffer)))
	(w3m-process-do-with-temp-buffer
	    (type (w3m-data-retrieve url nil nil))
	  (list type nil nil nil nil url url))))
     ((w3m-url-local-p url)
      (w3m-local-attributes url))
     (t
      (w3m-w3m-attributes url no-cache handler)))))

(defmacro w3m-content-type (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (car attrs)))
    `(car (w3m-attributes ,url ,no-cache))))
(defmacro w3m-content-charset (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 1 attrs)))
    `(nth 1 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-content-length (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 2 attrs)))
    `(nth 2 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-content-encoding (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 3 attrs)))
    `(nth 3 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-last-modified (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 4 attrs)))
    `(nth 4 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-real-url-1 (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 5 attrs)))
    `(nth 5 (w3m-attributes ,url ,no-cache))))

(defun w3m-real-url (url &optional no-cache handler)
  (w3m-string-match-url-components url)
  (let ((name (match-string 9 url))
	(real (w3m-real-url-1 url no-cache handler)))
    (w3m-string-match-url-components real)
    (cond ((match-beginning 8) real)
	  (name (concat real "#" name))
	  (t real))))

(defmacro w3m-make-help-echo (property)
  "Make a function returning a string used for the `help-echo' message.
PROPERTY is a symbol (which doesn't need to be quoted) of a text
property with the value of a string which should be in the place where
having to show a help message."
  `(lambda (window _object pos)
     (if w3m-track-mouse
	 (let ((deactivate-mark nil)
	       (msg (w3m-url-readable-string
		     (get-text-property
		      pos ',property (window-buffer window)))))
	   (message nil)	; Clear the echo area.
	   (if (memq this-command '(backward-button forward-button))
	       (truncate-string-to-width
		(subst-char-in-string ?\n ?  msg) (window-width))
	     msg)))))

(defvar w3m-current-message nil
  "The string currently displayed by `w3m-message' in the echo area.")
(defvar w3m-message-silent nil
  "If bound to non-nil, `w3m-message' will do nothing.")

(defun w3m-message (&rest args)
  "Display a message at the bottom of the screen.
This function works like `message' if `w3m-verbose' is non-nil.  In
that case, the message also goes into the \"*Messages*\" buffer if
`message-log-max' is non-nil.  But if `w3m-verbose' is nil, this
function only displays the message, does not log the message into the
\"*Messages*\" buffer (no matter what `message-log-max' is).
If `w3m-message-silent' is temporarily bound to non-nil, this function
does neither display nor log the message."
  ;; Always clear previous message in order to shrink the window height
  ;; of the echo area.
  (unless (< (string-width (or (current-message) "")) (window-width))
    (message nil))
  (unless w3m-message-silent
    (if w3m-verbose
	(apply (function message) args)
      (if (when w3m-process-background
	    (or (window-minibuffer-p (selected-window))
		(when (current-message)
		  (not (equal (current-message) w3m-current-message)))))
	  (apply (function format) args)
	(let (message-log-max)
	  (setq w3m-current-message (apply (function message) args)))))))

(defun w3m-time-parse-string (string)
  "Parse the time-string STRING into a time in the Emacs style."
  (ignore-errors
    (let ((x (timezone-fix-time string nil nil)))
      (encode-time (aref x 5) (aref x 4) (aref x 3)
		   (aref x 2) (aref x 1) (aref x 0)
		   (aref x 6)))))

(defun w3m-sub-list (list n)
  "Return a list of the first N elements of LIST.
If N is negative, return a list of the last N elements of LIST."
  (if (integerp n)
      (if (< n 0)
	  ;; N is negative, extract the last items
	  (if (>= (- n) (length list))
	      (copy-sequence list)
	    (nthcdr (+ (length list) n) (copy-sequence list)))
	;; N is positive, extract the first items
	(if (>= n (length list))
	    (copy-sequence list)
	  (nreverse (nthcdr (- (length list) n) (reverse list)))))
    (copy-sequence list)))

(defun w3m-load-list (file &optional coding-system)
  "Read an emacs-w3m data file FILE and return contents as a list.
It is used for loading `w3m-arrived-file', `w3m-cookie-file',
`w3m-favicon-cache-file' and `w3m-antenna-file' (which see).
CODING-SYSTEM is used to read FILE which defaults to the value of
`w3m-file-coding-system-for-read'."
  (when (and (file-readable-p file)
	     (not (file-directory-p file)))
    (with-temp-buffer
      (when (condition-case nil
		(let ((coding-system-for-read
		       (or coding-system w3m-file-coding-system-for-read)))
		  (insert-file-contents file))
	      (error
	       (message "Error while loading %s" file)
	       nil))
	;; point is not always moved to the beginning of the buffer
	;; after `insert-file-contents' is done.
	(goto-char (point-min))
	(condition-case err
	    (read (current-buffer))
	  (error
	   (message "Error while reading %s; %s"
		    file (error-message-string err))
	   nil))))))

(defun w3m-save-list (file list &optional coding-system escape-ctl-chars)
  "Save a LIST form into the emacs-w3m data file FILE.
Contents will be encoded with CODING-SYSTEM which defaults to the
value of `w3m-file-coding-system'.  Optional ESCAPE-CTL-CHARS if it is
non-nil, control chars will be represented with ^ as `cat -v' does."
  (when (and list (file-writable-p file))
    (with-temp-buffer
      (let ((coding-system-for-write (or coding-system w3m-file-coding-system))
	    (standard-output (current-buffer))
	    (print-fn (if escape-ctl-chars
			  'w3m-prin1
			'prin1))
	    element print-length print-level)
	(insert (format "\
;;; %s  -*- mode: emacs-lisp%s -*-
;; This file is generated automatically by emacs-w3m v%s.

"
			(file-name-nondirectory file)
			(if coding-system-for-write
			    (format "; coding: %s" coding-system-for-write)
			  "")
			emacs-w3m-version))
	(insert "(")
	(while list
	  (setq element (car list)
		list (cdr list))
	  (if (consp element)
	      (progn
		(insert "(")
		(funcall print-fn (car element))
		(insert "\n")
		(while (setq element (cdr element))
		  (insert "  ")
		  (funcall print-fn (car element))
		  (insert "\n"))
		(backward-delete-char 1)
		(insert ")\n "))
	    (funcall print-fn element)
	    (insert "\n")))
	(skip-chars-backward "\n ")
	(delete-region (point) (point-max))
	(insert ")\n")
	(let ((mode (and (file-exists-p file)
			 (file-modes file))))
	  (write-region (point-min) (point-max) file nil 'nomsg)
	  (when mode (set-file-modes file mode)))))))

(defun w3m-url-coding-system (url)
  "Return coding system suitable to URL to retrieve."
  (let ((alist w3m-url-coding-system-alist)
	(case-fold-search t)
	elt coding)
    (while alist
      (setq elt (pop alist))
      (if (or (not (car elt))
	      (and (stringp (car elt)) (string-match (car elt) url)))
	  (setq coding (cdr elt)
		alist nil)))
    (when (functionp coding)
      (setq coding (funcall coding url)))
    (or coding
	w3m-current-coding-system
	(cdr (assq nil w3m-url-coding-system-alist))
	w3m-default-coding-system)))

(defun w3m-url-encode-string (str &optional coding encode-space)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((and (char-equal ch ?\x20); space
		   encode-space)
	      "+")
	     (t
	      (format "%%%02X" ch))))	; escape
	  (encode-coding-string (or str "")
				(or coding (w3m-url-coding-system str))))))

(defun w3m-url-encode-string-2 (str)
  "Encode `(' and `)', apt to be misidentified as boundaries."
  (replace-regexp-in-string ")" "%29"
			    (replace-regexp-in-string "(" "%28" str)))

(defun w3m-url-decode-string (str &optional coding regexp)
  (or regexp (setq regexp "%\\(?:\\([0-9a-f][0-9a-f]\\)\\|0d%0a\\)"))
  (let ((start 0)
	(case-fold-search t))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (while (string-match regexp str start)
	(insert (substring str start (match-beginning 0))
		(if (match-beginning 1)
		    (string-to-number (match-string 1 str) 16)
		  ?\n))
	(setq start (match-end 0)))
      (insert (substring str start))
      (decode-coding-string
       (buffer-string)
       (or (if (listp coding)
	       (w3m-detect-coding-region (point-min) (point-max) coding)
	     coding)
	   w3m-default-coding-system
	   w3m-coding-system
	   'iso-2022-7bit)))))

(defun w3m-url-readable-string (url)
  "Return a readable string for a given encoded URL."
  (when (stringp url)
    (setq url (w3m-puny-decode-url url))
    (if (string-match "[^\000-\177]" url)
	url
      (w3m-url-decode-string url (unless (let ((case-fold-search t))
					   (string-match "\\`mailto:" url))
				   (w3m-url-coding-system url))))))

(defun w3m-url-transfer-encode-string (url &optional coding)
  "Encode non-ascii characters in URL into the sequence of escaped octets.
Optional CODING is a coding system, that defaults to the one determined
according to URL and `w3m-url-coding-system-alist', used to encode URL.

This function is designed for conversion for safe transmission of URL,
i.e., it handles only non-ASCII characters that can not be transmitted
safely through the network.  For the other general purpose, you should
use `w3m-url-encode-string' instead."
  (setq url (w3m-puny-encode-url url))
  (unless coding
    (setq coding (w3m-url-coding-system url)))
  (let ((start 0)
	buf)
    (while (string-match "[^\x21-\x7e]+" url start)
      (setq buf
	    (cons (apply 'concat
			 (mapcar
			  (lambda (c) (format "%%%02X" c))
			  (encode-coding-string (match-string 0 url) coding)))
		  (cons (substring url start (match-beginning 0))
			buf))
	    start (match-end 0)))
    (apply 'concat
	   (nreverse (cons (substring url start) buf)))))

;;; HTML character entity handling:
(defun w3m-entity-value (name)
  "Get a char corresponding to NAME from the html char entities database.
The database is kept in `w3m-entity-table'."
  ;; Return a value of the specified entity, or nil if it is unknown.
  (or (gethash name w3m-entity-table)
      (and (eq (aref name 0) ?#)
	   (let ((num (if (memq (aref name 1) '(?X ?x))
			  (string-to-number (substring name 2) 16)
			(string-to-number (substring name 1)))))
	     (or (cdr (assq num w3m-extra-numeric-character-reference))
		 (string (w3m-ucs-to-char num)))))))

(defun w3m-fontify-bold ()
  "Fontify bold text in the buffer containing halfdump."
  (goto-char (point-min))
  (while (search-forward "<b>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (re-search-forward "</b[ \t\r\f\n]*>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(w3m-add-face-property start (match-beginning 0) 'w3m-bold)))))

(defun w3m-fontify-italic ()
  "Fontify italic text in the buffer containing halfdump."
  (goto-char (point-min))
  (while (search-forward "<i>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (re-search-forward "</i[ \t\r\f\n]*>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(w3m-add-face-property start (match-beginning 0) 'w3m-italic)))))

(defun w3m-fontify-underline ()
  "Fontify underline text in the buffer containing halfdump."
  (goto-char (point-min))
  (while (search-forward "<u>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (re-search-forward "</u[ \t\r\f\n]*>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(w3m-add-face-property start (match-beginning 0) 'w3m-underline)))))

(defun w3m-fontify-strike-through ()
  "Fontify strike-through text in the buffer containing halfdump."
  (when (display-graphic-p)
    (goto-char (point-min))
    (if (eq w3m-display-ins-del 'fontify)
	(while (search-forward "<s>" nil t)
	  (let ((start (match-beginning 0)))
	    (delete-region start (match-end 0))
	    (when (re-search-forward "</s[ \t\r\f\n]*>" nil t)
	      (delete-region (match-beginning 0) (match-end 0))
	      (w3m-add-face-property start (match-beginning 0)
				     'w3m-strike-through))))
      (while (re-search-forward
	      (concat "<U>\\(?:\\(?::\\(?:\\(?:DEL\\|S\\)]\\)\\|"
		      "\\[\\(?:\\(?:DEL\\|S\\):\\)\\)</U>\\)")
	      nil t)
	(w3m-add-face-property (match-beginning 0) (match-end 0)
			       'w3m-strike-through)))))

(defun w3m-fontify-insert ()
  "Fontify insert text in the buffer containing halfdump."
  (when (display-graphic-p)
    (goto-char (point-min))
    (if (eq w3m-display-ins-del 'fontify)
	(while (search-forward "<ins>" nil t)
	  (let ((start (match-beginning 0)))
	    (delete-region start (match-end 0))
	    (when (re-search-forward "</ins[ \t\r\f\n]*>" nil t)
	      (delete-region (match-beginning 0) (match-end 0))
	      (w3m-add-face-property start (match-beginning 0) 'w3m-insert))))
      (while (re-search-forward "<U>\\(?:\\(?::INS]\\|\\[INS:\\)</U>\\)"
				nil t)
	(w3m-add-face-property (match-beginning 0) (match-end 0)
			       'w3m-insert)))))

(defun w3m-decode-anchor-string (str)
  ;; FIXME: This is a quite ad-hoc function to process encoded url string.
  ;; More discussion about timing &-sequence decode is required.  The
  ;; following article (written in Japanese) is the origin of this issue:
  ;;
  ;; [emacs-w3m:00150] <URL:http://emacs-w3m.namazu.org/ml/msg00149.html>
  ;;
  ;; Takaaki MORIYAMA wrote in the article that the string "&amp;" which
  ;; is replaced from "&" and embedded in the w3m's halfdump should be
  ;; restored into "&" some time.
  (let ((start 0) (buf))
    (while (string-match "\\(&amp;\\)\\|\\([\t\r\f\n]+\\)" str start)
      (setq buf (cons (if (match-beginning 1) "&" " ")
		      (cons (substring str start (match-beginning 0)) buf))
	    start (match-end 0)))
    (apply (function concat)
	   (nreverse (cons (substring str start) buf)))))

(defun w3m-image-type (content-type)
  "Return an image type which corresponds to CONTENT-TYPE.
The return value `image-convert' or `convert' means to convert image
data to be able to display using the `image-convert' function or the
external `convert' program respectively."
  (and content-type w3m-image-type-alist ;; this is not emacs-nox
       (or (cdr (assoc content-type w3m-image-type-alist))
	   (let ((case-fold-search t) type)
	     (and (string-match "\\`image/" content-type)
		  (or (eq imagemagick-enabled-types t)
		      (member (downcase (substring content-type (match-end 0)))
			      (mapcar
			       (lambda (x) (downcase (symbol-name x)))
			       (if (fboundp 'imagemagick-types)
				   (imagemagick-types)
				 (cons 'WEBP imagemagick-enabled-types)))))
		  (setq type (if (memq 'imagemagick image-types)
				 'imagemagick ;; ImageMagick has been built-in
			       (if (boundp 'image-use-external-converter)
				   ;; Emacs >=27; to make it work, some
				   ;; converter program is required to have
				   ;; been installed; see image-converter.el.
				   'image-convert
				 (if (w3m-which-command "convert")
				     'convert))))
		  (push (cons content-type type) w3m-image-type-alist)
		  type)))))

(defun w3m-handle-non-anchor-buttons ()
  "Return a boolean value corresponding to the variable of the same name."
  (if (listp w3m-handle-non-anchor-buttons)
      (condition-case nil
	  (eval w3m-handle-non-anchor-buttons)
	(error nil))
    (and w3m-handle-non-anchor-buttons t)))

(defun w3m-fontify-anchors ()
  "Fontify anchor tags in the buffer which contains halfdump."
  (let ((help (w3m-make-help-echo w3m-balloon-help))
	prenames start end bhhref first)
    (goto-char (point-min))
    (while (re-search-forward "<_id[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (setq prenames (get-text-property start 'w3m-name-anchor))
      (w3m-parse-attributes (id)
	(delete-region start (point))
	(w3m-add-text-properties start (point-max)
				 (list 'w3m-name-anchor
				       (cons (w3m-decode-entities-string id)
					     prenames)))))
    (goto-char (point-min))
    (while (re-search-forward "<a[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (setq prenames (get-text-property start 'w3m-name-anchor2))
      (w3m-parse-attributes (href name id title
				  (rel :case-ignore) (hseq :integer))
	(unless name
	  (setq name id))
	(when href
	  (setq href (w3m-decode-anchor-string href)))
	(when rel
	  (setq rel (split-string rel))
	  (cond
	   ((member "next" rel) (setq w3m-next-url href))
	   ((or (member "prev" rel) (member "previous" rel))
	    (setq w3m-previous-url href))
	   ((member "start" rel) (setq w3m-start-url href))
	   ((member "contents" rel) (setq w3m-contents-url href))))
	(delete-region start (point))
	(cond
	 (href
	  (when (re-search-forward "[ \t\r\f\n]*\\(</a>\\)" nil t)
	    (setq end (match-beginning 0))
	    (delete-region (match-beginning 1) (match-end 1))
	    (setq href (w3m-expand-url href))
	    (unless (or (w3m-url-local-p href)
			(let ((case-fold-search t))
			  (string-match "\\`mailto:" href)))
	      (w3m-string-match-url-components href)
	      (setq href (if (match-beginning 8)
			     (let ((tmp (match-string 9 href)))
			       (concat (w3m-url-transfer-encode-string
					(substring href 0 (match-beginning 8)))
				       "#" tmp))
			   (w3m-url-transfer-encode-string href))))
	    (setq hseq (or (and (null hseq) 0) (abs hseq)))
	    (w3m-add-face-property start end (if (w3m-arrived-p href)
						 'w3m-arrived-anchor
					       'w3m-anchor))
	    (if title
		(progn
		  (setq title (w3m-decode-entities-string title))
		  (setq bhhref (concat (w3m-decode-anchor-string title)
				       "\n"
				       (w3m-url-readable-string href))))
	      (setq bhhref (w3m-url-readable-string href)))
	    (w3m-add-text-properties start end
				     (list 'w3m-href-anchor href
					   'w3m-balloon-help bhhref
					   'w3m-anchor-title title
					   'mouse-face 'highlight
					   'w3m-anchor-sequence hseq
					   'help-echo help
					   'keymap w3m-link-map))
	    (and (w3m-handle-non-anchor-buttons) (not first)
		 ;; Add a widget so `forward-button' and `widget-forward'
		 ;; work from outside of rendered area."
		 (let ((ovl (make-overlay start end)))
		   (overlay-put ovl 'evaporate t)
		   (overlay-put ovl 'button '(w3m))
		   (overlay-put ovl 'category 'w3m)
		   (add-text-properties start end '(button w3m category w3m))
		   (setq first t)))
	    (when name
	      (w3m-add-text-properties
	       start (point-max)
	       (list 'w3m-name-anchor2
		     (cons (w3m-decode-entities-string name)
			   prenames))))))
	 (name
	  (w3m-add-text-properties
	   start (point-max)
	   (list 'w3m-name-anchor2
		 (cons (w3m-decode-entities-string name)
		       prenames)))))))
    (when w3m-icon-data
      (setq w3m-icon-data (cons (and (car w3m-icon-data)
				     (w3m-expand-url (car w3m-icon-data)))
				(or (w3m-image-type (cdr w3m-icon-data))
				    'ico))))
    (when w3m-next-url
      (setq w3m-next-url (w3m-expand-url w3m-next-url)))
    (when w3m-previous-url
      (setq w3m-previous-url (w3m-expand-url w3m-previous-url)))
    (when w3m-start-url
      (setq w3m-start-url (w3m-expand-url w3m-start-url)))
    (when w3m-contents-url
      (setq w3m-contents-url (w3m-expand-url w3m-contents-url)))))

(defun w3m-setup-menu ()
  "Define menubar buttons."
  (w3m-menu-on-forefront w3m-menu-on-forefront t)
  (unless (keymapp (lookup-key w3m-mode-map [menu-bar w3m]))
    (let ((map (make-sparse-keymap (car w3m-menubar))))
      (define-key w3m-mode-map [menu-bar] (make-sparse-keymap))
      (w3m-setup-session-menu)
      (when w3m-use-tab-menubar (w3m-setup-tab-menu))
      (w3m-setup-bookmark-menu)
      (define-key w3m-mode-map [menu-bar w3m] (cons (car w3m-menubar) map))
      (require 'easymenu)
      (easy-menu-define
	w3m-mode-menu w3m-mode-map
	"w3m menu item" w3m-menubar)
      (easy-menu-add w3m-mode-menu))
    (let ((map (make-sparse-keymap)))
      (easy-menu-define
	w3m-rmouse-menu map
	"w3m rmouse menu item" w3m-rmouse-menubar))))

(defun w3m-fontify-images ()
  "Fontify img_alt strings of images in the buffer containing halfdump."
  (goto-char (point-min))
  (let (upper start end help src1)
    (while (re-search-forward "<\\(img_alt\\)[^>]+>" nil t)
      (setq upper (string= (match-string 1) "IMG_ALT")
	    start (match-beginning 0)
	    end (match-end 0))
      (goto-char (match-end 1))
      (w3m-parse-attributes (src
			     (width :integer)
			     (height :integer)
			     title
			     usemap)
	(delete-region start end)
	(setq src (w3m-expand-url (w3m-decode-anchor-string src)))
	;; Use the identical Lisp object for a string used as the value
	;; of the `w3m-image' property.
	(if (equal src src1)
	    (setq src src1)
	  (setq src1 src))
	(when (search-forward "</img_alt>" nil t)
	  (delete-region (setq end (match-beginning 0)) (match-end 0))
	  (setq help (get-text-property start 'w3m-balloon-help))
	  (cond
	   ((and help title)
	    (setq help (format "%s\nalt: %s\nimg: %s" help title src)))
	   (help
	    (setq help (format "%s\nimg: %s" help src)))
	   (title
	    (setq help (format "alt: %s\nimg: %s" title src)))
	   (t
	    (setq help (format "img: %s" src))))
	  (w3m-add-text-properties start end
				   (list 'w3m-image src
					 'w3m-image-size
					 (when (or width height)
					   (cons width height))
					 'w3m-image-alt title
					 'w3m-balloon-help help
					 'w3m-image-usemap usemap
					 'w3m-image-status 'off
					 'w3m-image-redundant upper
					 'keymap w3m-link-map))
	  (unless (w3m-action start)
	    ;; No need to use `w3m-add-text-properties' here.
	    (w3m-add-face-property start end
				   (if (w3m-anchor start)
				       'w3m-image-anchor
				     'w3m-image))
	    (unless (w3m-anchor start)
	      (add-text-properties start end
				   (list 'mouse-face 'highlight
					 'help-echo help)))))))))

(defvar w3m-idle-images-show-timer nil)
(defvar w3m-idle-images-show-list nil)
(defvar w3m-idle-images-show-interval 1)

(defun w3m-idle-images-show ()
  (let ((repeat t)
	(onbuffer (member (current-buffer) (w3m-list-buffers))))
    (while (and repeat w3m-idle-images-show-list)
      (let* ((item (or
		    (and
		     onbuffer
		     (or (get-text-property (point) 'w3m-idle-image-item)
			 (let* ((prev (previous-single-property-change
				       (point) 'w3m-idle-image-item))
				(next (next-single-property-change
				       (point) 'w3m-idle-image-item))
				(prev-diff (and prev (abs (- (point) prev))))
				(next-diff (and next (abs (- (point) next)))))
			   (cond
			    ((and prev next)
			     (get-text-property
			      (if (< prev-diff next-diff) prev next)
			      'w3m-idle-image-item))
			    (prev
			     (get-text-property prev
						'w3m-idle-image-item))
			    (next
			     (get-text-property next
						'w3m-idle-image-item))
			    (t nil)))))
		    (car (last w3m-idle-images-show-list))))
	     (start    (nth 0 item))
	     (end      (nth 1 item))
	     (iurl     (nth 2 item))
	     (url      (nth 3 item))
	     (no-cache (nth 4 item))
	     (size     (nth 5 item)))
	(setq w3m-idle-images-show-list
	      (delete item w3m-idle-images-show-list))
	(if (buffer-live-p (marker-buffer start))
	    (with-current-buffer (marker-buffer start)
	      (save-restriction
		(widen)
		(let ((inhibit-read-only t))
		  (remove-text-properties start end '(w3m-idle-image-item))
		  (set-buffer-modified-p nil))
		(w3m-process-with-null-handler
		  (w3m-process-do
		      (image (let ((w3m-current-buffer (current-buffer))
				   (w3m-message-silent t))
			       (w3m-create-image
				iurl no-cache
				url
				size handler)))
		    (when (buffer-live-p (marker-buffer start))
		      (with-current-buffer (marker-buffer start)
			(save-restriction
			  (widen)
			  (if image
			      (when (equal url w3m-current-url)
				(let ((inhibit-read-only t))
				  (w3m-insert-image start end image iurl))
				;; Redisplay
				(when w3m-force-redisplay
				  (sit-for 0)))
			    (let ((inhibit-read-only t))
			      (w3m-add-text-properties
			       start end '(w3m-image-status off))))
			  (set-buffer-modified-p nil))
			(set-marker start nil)
			(set-marker end nil)))))))
	  (set-marker start nil)
	  (set-marker end nil)
	  (w3m-idle-images-show-unqueue (marker-buffer start))))
      (setq repeat (sit-for 0.1 t)))
    (if w3m-idle-images-show-list
	(when (input-pending-p)
	  (w3m-cancel-timer w3m-idle-images-show-timer)
	  (setq w3m-idle-images-show-timer
		(run-with-idle-timer w3m-idle-images-show-interval
				     t
				     'w3m-idle-images-show)))
      (w3m-cancel-timer w3m-idle-images-show-timer)
      (setq w3m-idle-images-show-timer nil))))

(defun w3m-idle-images-show-unqueue (buffer)
  (when (w3m-cancel-timer w3m-idle-images-show-timer)
    (setq w3m-idle-images-show-timer nil)
    (setq w3m-idle-images-show-list
	  (delq nil
		(mapcar (lambda (x)
			  (and (not (eq buffer (marker-buffer (nth 0 x))))
			       x))
			w3m-idle-images-show-list)))
    (when w3m-idle-images-show-list
      (setq w3m-idle-images-show-timer
	    (run-with-idle-timer w3m-idle-images-show-interval
				 t
				 'w3m-idle-images-show)))))

(defcustom w3m-confirm-leaving-secure-page t
  "If non-nil, you'll be asked for confirmation when leaving secure pages.
This option controls whether the confirmation is made also when
retrieving data (typically images) in a secure page from non-secure
pages.  It is STRONGLY recommended to set non-nil value to this option.
You MUST understand what you want to do completely before switching
off this option."
  :group 'w3m
  :type 'boolean)

(defvar w3m-image-no-idle-timer nil)
(defun w3m-toggle-inline-images-internal (status
					  &optional no-cache url
					  begin-pos end-pos
					  safe-regexp)
  "Toggle displaying of inline images on current buffer.
STATUS is current image status.
If NO-CACHE is non-nil, cache is not used.
If URL is specified, only the image with URL is toggled."
  (let ((cur-point (point))
	(inhibit-read-only t)
	(end (or begin-pos (point-min)))
	(allow-non-secure-images (not w3m-confirm-leaving-secure-page))
	start iurl image size)
    (unless end-pos (setq end-pos (point-max)))
    (save-excursion
      (if (equal status 'off)
	  (while (< (setq start
			  (if (w3m-image end)
			      end
			    (next-single-property-change end 'w3m-image
							 nil end-pos)))
		    end-pos)
	    (setq end (or (next-single-property-change start 'w3m-image)
			  (point-max))
		  iurl (w3m-image start)
		  size (get-text-property start 'w3m-image-size))
	    (when (and (or (and (not url)
				(or (not w3m-ignored-image-url-regexp)
				    (not (string-match
					  w3m-ignored-image-url-regexp
					  iurl))))
			   ;; URL is specified and is same as the image URL.
			   (string= url iurl))
		       (not (eq (get-text-property start 'w3m-image-status)
				'on)))
	      (w3m-add-text-properties start end '(w3m-image-status on))
	      (if (get-text-property start 'w3m-image-redundant)
		  (progn
		    ;; Insert a dummy string instead of a redundant image.
		    (setq image (make-string
				 (string-width (buffer-substring start end))
				 ? ))
		    (w3m-add-text-properties start end '(invisible t))
		    (goto-char end)
		    (w3m-add-text-properties
		     end (progn (insert image) (point))
		     '(w3m-image-dummy t w3m-image "dummy"))
		    (setq end (point)))
		(goto-char cur-point)
		(when (and (w3m-url-valid iurl)
			   (or (null safe-regexp)
			       (string-match safe-regexp iurl))
			   (not (and (not (w3m-url-local-p w3m-current-url))
				     (w3m-url-local-p iurl)))
			   (or (not w3m-current-ssl)
			       (string-match "\\`\\(?:ht\\|f\\)tps://" iurl)
			       allow-non-secure-images
			       (and (prog1
					(y-or-n-p "\
You are retrieving non-secure image(s).  Continue? ")
				      (message nil))
				    (setq allow-non-secure-images t))))
		  (if (or w3m-image-no-idle-timer
			  (and (null (and size w3m-resize-images))
			       (or (string-match "\\`\\(?:cid\\|data\\):" iurl)
				   (w3m-url-local-p iurl)
				   (w3m-cache-available-p iurl))))
		      (w3m-process-with-null-handler
			(let ((start (set-marker (make-marker) start))
			      (end (set-marker (make-marker) end))
			      (url w3m-current-url))
			  (w3m-process-do
			      (image
			       (let ((w3m-current-buffer (current-buffer)))
				 (w3m-create-image
				  iurl no-cache w3m-current-url	size handler)))
			    (when (buffer-live-p (marker-buffer start))
			      (with-current-buffer (marker-buffer start)
				(if image
				    (when (equal url w3m-current-url)
				      (let ((inhibit-read-only t))
					(w3m-insert-image start end image iurl))
				      ;; Redisplay
				      (when w3m-force-redisplay
					(sit-for 0)))
				  (let ((inhibit-read-only t))
				    (w3m-add-text-properties
				     start end '(w3m-image-status off))))
				(set-buffer-modified-p nil)))
			    (set-marker start nil)
			    (set-marker end nil))))
		    (let ((item (list (set-marker (make-marker) start)
				      (set-marker (make-marker) end)
				      (w3m-url-transfer-encode-string iurl)
				      w3m-current-url
				      no-cache
				      size)))
		      (setq w3m-idle-images-show-list
			    (cons item w3m-idle-images-show-list))
		      (w3m-add-text-properties
		       start end
		       `(w3m-idle-image-item ,item))
		      (unless w3m-idle-images-show-timer
			(setq w3m-idle-images-show-timer
			      (run-with-idle-timer
			       w3m-idle-images-show-interval
			       t 'w3m-idle-images-show)))))))))
	;; Remove.
	(while (< (setq start (if (w3m-image end)
				  end
				(next-single-property-change end 'w3m-image
							     nil end-pos)))
		  end-pos)
	  (setq end (or (next-single-property-change start 'w3m-image)
			(point-max))
		iurl (w3m-image start))
	  ;; IMAGE-ALT-STRING DUMMY-STRING
	  ;; <--------w3m-image---------->
	  ;; <---redundant--><---dummy--->
	  ;; <---invisible-->
	  (when (and (or (not url)
			 ;; URL is specified and is not same as the image URL.
			 (string= url iurl))
		     (not (eq (get-text-property start 'w3m-image-status)
			      'off)))
	    (cond
	     ((get-text-property start 'w3m-image-redundant)
	      ;; Remove invisible property.
	      (put-text-property start end 'invisible nil))
	     ((get-text-property start 'w3m-image-dummy)
	      ;; Remove dummy string.
	      (delete-region start end)
	      (setq end start))
	     (t (w3m-remove-image start end)))
	    (w3m-add-text-properties
	     start end
	     '(w3m-image-status off w3m-idle-image-item nil))))
	(set-buffer-modified-p nil)))))

(defun w3m-toggle-inline-image (&optional force no-cache)
  "Toggle the visibility of an image under point or images in the region.
If FORCE is non-nil, displaying an image is forced.  If NO-CACHE is
non-nil, cached data will not be used."
  (interactive "P")
  (unless (display-images-p)
    (error "Can't display images in this environment"))
  (let (toggle-list begin end)
    (if (region-active-p)
	(let ((p (region-beginning))
	      iurl)
	  (setq begin (region-beginning)
		end (region-end))
	  (deactivate-mark)
	  (while (< p end)
	    (setq p (next-single-property-change p 'w3m-image nil end))
	    (when (and (< p end)
		       (setq iurl (w3m-image p))
		       (not (assoc iurl toggle-list)))
	      (setq toggle-list (cons (cons iurl p) toggle-list)))))
      (setq toggle-list (and (w3m-image)
			     `(,(cons (w3m-image) (point))))))
    (if toggle-list
	(dolist (x toggle-list)
	  (let* ((url (car x))
		 (pos (cdr x))
		 (status (get-text-property pos 'w3m-image-status))
		 safe-regexp)
	    (if (and (get-text-property pos 'w3m-image-scale)
		     (equal status 'off))
		(w3m-zoom-in-image 0)
	      (if (w3m-url-valid url)
		  (if (eq status 'on)
		      (progn
			(if force (setq status 'off))
			(w3m-toggle-inline-images-internal
			 status no-cache url
			 (or begin (point-min))
			 (or end (point-max))))
		    (setq safe-regexp
			  (get-text-property (point) 'w3m-safe-url-regexp))
		    (if (or force
			    (not safe-regexp)
			    (string-match safe-regexp url))
			(w3m-toggle-inline-images-internal
			 status no-cache url
			 (or begin (point-min))
			 (or end (point-max)))
		      (when (w3m-interactive-p)
			(w3m-message "This image is considered to be unsafe;\
 use the prefix arg to force display"))))))))
      (if begin
	  (w3m-message "No images in region")
	(w3m-message "No image at point")))))

(defun w3m-turnoff-inline-images ()
  "Turn off to display all images in the buffer or in the region."
  (interactive)
  (w3m-toggle-inline-images 'turnoff))

(defun w3m-toggle-inline-images (&optional force no-cache)
  "Toggle the visibility of all images in the buffer or in the region.
If FORCE is neither nil nor `turnoff', displaying images is forced.
The value `turnoff' is special; it turns displaying images off anyway.
If NO-CACHE is non-nil, cached data will not be used.

Note that the status of whether images are visible is kept hereafter
even in new sessions if the `w3m-toggle-inline-images-permanently'
variable is non-nil (default=t)."
  (interactive "P")
  (unless (display-images-p)
    (error "Can't display images in this environment"))
  (let ((status (cond ((eq force 'turnoff) t)
		      (force nil)
		      (t w3m-display-inline-images)))
	(safe-p t)
	beg end safe-regexp pos url)
    (if (region-active-p)
	(progn
	  (setq beg (region-beginning)
		end (region-end))
	  (deactivate-mark))
      (setq beg (point-min)
	    end (point-max)))
    (unless status
      (when (setq safe-regexp (get-text-property beg 'w3m-safe-url-regexp))
	;; Scan the buffer for searching for an insecure image url.
	(setq pos beg)
	(setq
	 safe-p
	 (catch 'done
	   (when (setq url (get-text-property pos 'w3m-image))
	     (unless (string-match safe-regexp url)
	       (throw 'done nil))
	     (setq pos (next-single-property-change pos 'w3m-image)))
	   (while (< pos end)
	     (when (and
		    (setq pos (next-single-property-change pos 'w3m-image
							   nil end))
		    (setq url (get-text-property pos 'w3m-image)))
	       (unless (string-match safe-regexp url)
		 (throw 'done nil)))
	     (setq pos (next-single-property-change pos 'w3m-image
						    nil end)))
	   t))))
    (if (or force
	    status
	    (not safe-regexp)
	    safe-p)
	(progn
	  (unwind-protect
	      (w3m-toggle-inline-images-internal (if status 'on 'off)
						 no-cache nil beg end
						 (unless (w3m-interactive-p)
						   safe-regexp))
	    (setq w3m-display-inline-images (not status))
	    (when status
	      (w3m-process-stop (current-buffer))
	      (w3m-idle-images-show-unqueue (current-buffer)))
	    (force-mode-line-update)))
      (w3m-message "There are some images considered unsafe;\
 use the prefix arg to force display"))))

(defun w3m-resize-inline-image-internal (_url rate)
  "Resize an inline image on the cursor position.
RATE is a number of percent used when resizing an image."
  (let* ((inhibit-read-only t)
	 (start (point))
	 (end (or (next-single-property-change start 'w3m-image)
		  (point-max)))
	 (iurl (w3m-image start))
	 (size (get-text-property start 'w3m-image-size))
	 (iscale (or (get-text-property start 'w3m-image-scale) '100))
	 (allow-non-secure-images (not w3m-confirm-leaving-secure-page))
	 scale image)
    (w3m-add-text-properties start end '(w3m-image-status on))
    (setq scale (* iscale rate 0.01))
    (w3m-add-text-properties start end (list 'w3m-image-scale scale))
    (setq scale (round scale))
    (if (get-text-property start 'w3m-image-redundant)
	(progn
	  ;; Insert a dummy string instead of a redundant image.
	  (setq image (make-string
		       (string-width (buffer-substring start end))
		       ? ))
	  (w3m-add-text-properties start end '(invisible t))
	  (w3m-add-text-properties (point)
				   (progn (insert image) (point))
				   '(w3m-image-dummy t
						     w3m-image "dummy")))
      (when (and (w3m-url-valid iurl)
		 (or (not w3m-current-ssl)
		     (string-match "\\`\\(?:ht\\|f\\)tps://" iurl)
		     allow-non-secure-images
		     (and (prog1
			      (y-or-n-p "\
You are retrieving non-secure image(s).  Continue? ")
			    (message nil))
			  (setq allow-non-secure-images t))))
	(w3m-process-with-null-handler
	  (setq start (set-marker (make-marker) start)
		end (set-marker (make-marker) end))
	  (let ((rate scale)
		(url w3m-current-url))
	    (w3m-process-do
		(image (let ((w3m-current-buffer (current-buffer)))
			 (w3m-create-resized-image
			  iurl
			  rate
			  w3m-current-url
			  size handler)))
	      (when (buffer-live-p (marker-buffer start))
		(with-current-buffer (marker-buffer start)
		  (if image
		      (when (equal url w3m-current-url)
			(let ((inhibit-read-only t))
			  (w3m-insert-image start end image iurl)
			  (w3m-image-animate image))
			;; Redisplay
			(when w3m-force-redisplay
			  (sit-for 0)))
		    (let ((inhibit-read-only t))
		      (w3m-add-text-properties
		       start end '(w3m-image-status off))))
		  (set-buffer-modified-p nil))
		(set-marker start nil)
		(set-marker end nil)))))))))

(defun w3m-resize-image-interactive (image &optional rate changed-rate)
  "Interactively resize IMAGE.
RATE is a number in percent used to enlarge or shrink the image,
defaults to the value of `w3m-resize-image-scale'.  CHANGED-RATE is
a float number that says how much the image has already been resized."
  (let* ((msg-prompt "Resize: [+ =] enlarge [-] shrink [0] original [q] quit")
	 (changed-rate (or changed-rate 1))
	 (rate (or (and rate (min rate 99)) w3m-resize-image-scale))
	 char)
    (while
	(cond
	 ((memq
	   (setq char
		 (read-char-exclusive
		  (propertize msg-prompt 'face 'w3m-lnum-minibuffer-prompt)))
	   '(?+ ?=))
	  (let ((percent (+ 100 rate)))
	    (w3m-resize-inline-image-internal image percent)
	    (setq changed-rate (* changed-rate
				  (/ percent 100.0)))))
	 ((eq char ?-)
	  (let ((percent (/ 10000.0 (+ 100 rate))))
	    (w3m-resize-inline-image-internal image percent)
	    (setq changed-rate (* changed-rate
				  (/ percent 100.0)))))
	 ((eq char ?0)
	  (w3m-resize-inline-image-internal image
					    (/ 100.0 changed-rate))
	  (setq changed-rate 1))
	 (t nil)))))

(defun w3m-zoom-in-image (&optional rate)
  "Zoom in an image on the point.
Numeric prefix specifies how many percent the image is enlarged by
(30 means enlarging the image by 130%).  The default is the value of
the `w3m-resize-image-scale' variable."
  (interactive "P")
  (unless (display-images-p)
    (error "Can't display images in this environment"))
  (unless (w3m-imagick-convert-program-available-p)
    (error "ImageMagick's `convert' program is required"))
  (let ((url (w3m-image)))
    (if url
	(w3m-resize-inline-image-internal
	 url
	 (+ 100 (or rate w3m-resize-image-scale)))
      (w3m-message "No image at point"))))

(defun w3m-zoom-out-image (&optional rate)
  "Zoom out an image on the point.
Numeric prefix specifies how many percent the image is shrunk by.
The default is the value of the `w3m-resize-image-scale'
variable.

The shrink percentage is interpreted as if the current image size
is 100+percent and it is to be reduced back to 100.  This is the
inverse of `w3m-zoom-in-image' so zooming in then back out gives
the original again."
  (interactive "P")
  (unless (display-images-p)
    (error "Can't display images in this environment"))
  (unless (w3m-imagick-convert-program-available-p)
    (error "ImageMagick's `convert' program is required"))
  (let ((url (w3m-image)))
    (if url
	(w3m-resize-inline-image-internal
	 url
	 (/ 10000.0 (+ 100 (or rate w3m-resize-image-scale))))
      (w3m-message "No image at point"))))

(defun w3m-decode-entities (&optional keep-properties)
  "Decode entities in the current buffer.
If optional KEEP-PROPERTIES is non-nil, text property is reserved."
  (save-excursion
    (goto-char (point-min))
    ;; Character entity references are case-sensitive.
    ;; cf. http://www.w3.org/TR/1999/REC-html401-19991224/charset.html#h-5.3.2
    (let (case-fold-search start fid prop value)
      (while (re-search-forward w3m-entity-regexp nil t)
	(setq start (match-beginning 0)
	      fid (get-text-property start 'w3m-form-field-id))
	(unless (and fid
		     (save-match-data
		       (string-match "/type=\\(?:text\\|select\\)/name=[^/]+/"
				     fid)))
	  (when keep-properties
	    (setq prop (text-properties-at start)))
	  (unless (eq (char-after (match-end 1)) ?\;)
	    (goto-char (match-end 1)))
	  (when (setq value (w3m-entity-value (match-string 1)))
	    (delete-region start (point))
	    (insert value))
	  (when prop
	    (w3m-add-text-properties start (point) prop)))))))

(defun w3m-decode-entities-string (str)
  "Decode entities in the string STR."
  (save-match-data
    ;; Character entity references are case-sensitive.
    ;; cf. http://www.w3.org/TR/1999/REC-html401-19991224/charset.html#h-5.3.2
    (let ((case-fold-search) (pos 0) (buf))
      (while (string-match w3m-entity-regexp str pos)
	(setq buf (cons (or (w3m-entity-value (match-string 1 str))
			    (match-string 1 str))
			(cons (substring str pos (match-beginning 0))
			      buf))
	      pos (if (eq (aref str (match-end 1)) ?\;)
		      (match-end 0)
		    (match-end 1))))
      (if buf
	  (apply 'concat (nreverse (cons (substring str pos) buf)))
	str))))

(defun w3m-encode-specials-string (str)
  "Encode special characters in the string STR."
  (let ((pos 0)
	(buf))
    (while (string-match "[<>&]" str pos)
      (setq buf
	    (cons ";"
		  (cons (gethash (match-string 0 str) w3m-entity-reverse-table)
			(cons "&"
			      (cons (substring str pos (match-beginning 0))
				    buf))))
	    pos (match-end 0)))
    (if buf
	(apply 'concat (nreverse (cons (substring str pos) buf)))
      str)))

(defun w3m-fontify ()
  "Fontify the current buffer."
  (let ((case-fold-search t)
	(inhibit-read-only t))
    (w3m-message "Fontifying...")
    (run-hooks 'w3m-fontify-before-hook)
    ;; Remove hidden anchors like "<a href=url> </a>".
    (goto-char (point-min))
    (while (re-search-forward "<a[\t\n ]+[^>]+>[\t\n ]*</a>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Delete <?xml ... ?> tag
    (goto-char (point-min))
    (if (search-forward "<?xml" nil t)
	(let ((start (match-beginning 0)))
	  (search-forward "?>" nil t)
	  (delete-region start (match-end 0))
	  (goto-char (point-min))))
    ;; Delete extra title tag.
    (let (start)
      (and (search-forward "<title>" nil t)
	   (setq start (match-beginning 0))
	   (search-forward "</title>" nil t)
	   (delete-region start (match-end 0))))
    (w3m-fontify-bold)
    (w3m-fontify-italic)
    (w3m-fontify-strike-through)
    (w3m-fontify-insert)
    (w3m-fontify-underline)
    (when w3m-use-symbol
      (w3m-replace-symbol))
    (w3m-fontify-anchors)
    (when w3m-use-form
      (w3m-fontify-forms))
    (w3m-fontify-images)
    ;; Remove other markups.
    (goto-char (point-min))
    (while (re-search-forward "</?[A-Za-z_][^>]*>" nil t)
      (let* ((start (match-beginning 0))
	     (fid (get-text-property start 'w3m-form-field-id)))
	(if (and fid (string-match "/type=text\\(?:area\\)?/" fid))
	    (goto-char (1+ start))
	  (delete-region start (match-end 0)))))
    ;; Decode escaped characters (entities).
    (w3m-decode-entities 'reserve-prop)
    (when w3m-use-form
      (w3m-fontify-textareas))
    (goto-char (point-min))
    (when w3m-delete-duplicated-empty-lines
      (while (re-search-forward "^[ \t]*\n\\(?:[ \t]*\n\\)+" nil t)
	(delete-region (match-beginning 0) (1- (match-end 0)))))

    ;; FIXME: The code above reduces number of empty lines but one line
    ;; remains.  While such empty lines might have been inserted for
    ;; making sure of rooms for displaying images, they all should be
    ;; removed since they are useless for emacs-w3m.  However, currently
    ;; we don't have a proper way to identify whether they were inserted
    ;; intentionally by the author or not.  So, we decided to remove only
    ;; that one at the beginning of the buffer though it is unwillingness.
    (goto-char (point-min))
    (skip-chars-forward "\t\n 　")
    (delete-region (point-min) (point-at-bol))

    (w3m-header-line-insert)
    (put-text-property (point-min) (point-max)
		       'w3m-safe-url-regexp w3m-safe-url-regexp)
    (w3m-message "Fontifying...done")
    (run-hooks 'w3m-fontify-after-hook)))

(defun w3m-refontify-anchor (&optional buff)
  "Refontify anchors as they have already arrived.
It replaces the faces on the arrived anchors from `w3m-anchor' to
`w3m-arrived-anchor'."
  (with-current-buffer (or buff (current-buffer))
    (let (prop)
      (when (and (eq major-mode 'w3m-mode)
		 (get-text-property (point) 'w3m-anchor-sequence)
		 (setq prop (get-text-property (point) 'face))
		 (listp prop)
		 (member 'w3m-anchor prop))
	(let ((start)
	      (end (next-single-property-change (point) 'w3m-anchor-sequence))
	      (inhibit-read-only t))
	  (when (and end
		     (setq start (previous-single-property-change
				  end 'w3m-anchor-sequence))
		     (w3m-arrived-p (get-text-property (point)
						       'w3m-href-anchor)))
	    (w3m-remove-face-property start end 'w3m-anchor)
	    (w3m-remove-face-property start end 'w3m-arrived-anchor)
	    (w3m-add-face-property start end 'w3m-arrived-anchor))
	  (set-buffer-modified-p nil))))))

(defun w3m-url-completion (url predicate flag)
  "Completion function for URL."
  (if (string-match "\\`\\(?:file:\\|[/~]\\|\\.\\.?/\\|[a-zA-Z]:\\)" url)
      (if (eq flag 'lambda)
	  (file-exists-p (w3m-url-to-file-name url))
	(let* ((partial
		(expand-file-name
		 (cond
		  ((string-match "\\`file:[^/]" url)
		   (substring url 5))
		  ((string-match "/\\(~\\)" url)
		   (substring url (match-beginning 1)))
		  (t (w3m-url-to-file-name url)))))
	       (collection
		(let ((dir (file-name-directory partial)))
		  (mapcar
		   (lambda (f)
		     (list (w3m-expand-file-name-as-url f dir)))
		   (file-name-all-completions (file-name-nondirectory partial)
					      dir)))))
	  (setq partial
		(if (string-match "/\\.\\'" url)
		    (concat (file-name-as-directory
			     (w3m-expand-file-name-as-url partial))
			    ".")
		  (w3m-expand-file-name-as-url partial)))
	  (cond
	   ((not flag)
	    (try-completion partial collection predicate))
	   ((eq flag t)
	    (all-completions partial collection predicate)))))
    (cond
     ((not flag)
      (try-completion url w3m-arrived-db))
     ((eq flag t)
      (all-completions url w3m-arrived-db))
     ((eq flag 'lambda)
      (if (w3m-arrived-p url) t nil)))))

(defun w3m-shr-url-at-point ()
  "Return a url that shr.el provides at point."
  (w3m-get-text-property-around 'shr-url))

(defun w3m-header-line-url ()
  "Return w3m-current-url if point stays at header line.
Where \"header line\" means the first line of a page buffer, not
header-line that `header-line-format' controls."
  (let ((faces (get-text-property (point) 'face)))
    (when (and (eq major-mode 'w3m-mode)
	       (listp faces)
	       (or (memq 'w3m-header-line-title faces)
		   (memq 'w3m-header-line-content faces))
	       w3m-current-url)
      w3m-current-url)))

(autoload 'ffap-url-at-point "ffap")
(defun w3m-url-at-point ()
  (or (w3m-shr-url-at-point)
      (w3m-header-line-url)
      (ffap-url-at-point)))

(defvar ffap-url-regexp)
(eval-after-load "ffap"
  '(if (and ffap-url-regexp
	    (not (string-match ffap-url-regexp "nntp://bar"))
	    (string-match "\\(\\\\(news\\\\(post\\\\)\\?:\\)\\(\\\\|\\)"
			  ffap-url-regexp))
       ;; Add nntp:
       (setq ffap-url-regexp (replace-match "\\1\\\\|nntp:\\2"
					    nil nil ffap-url-regexp))))

(defun w3m-active-region-or-url-at-point (&optional default-to)
  "Return an active region or a url around the cursor.
In Transient Mark mode, deactivate the mark.
If DEFAULT-TO is `never', only return a url found in the specified
region if any.  If it is nil, default to a url that an anchor at
the point points to.  If it is neither `never' nor nil, default to
a url of the current page as the last resort."
  (if (region-active-p)
      (prog1
	  (let ((string (buffer-substring-no-properties
			 (region-beginning) (region-end))))
	    (with-temp-buffer
	      (insert string)
	      (skip-chars-backward "\t\n\f\r 　")
	      (delete-region (point) (point-max))
	      (goto-char (point-min))
	      (skip-chars-forward "\t\n\f\r 　")
	      (delete-region (point-min) (point))
	      (while (re-search-forward "\
\\(?:[\t\f\r 　]+\n[\t\f\r 　]*\\|[\t\f\r 　]*\n[\t\f\r 　]+\\)+" nil t)
		(delete-region (match-beginning 0) (match-end 0)))
	      (buffer-string)))
	(deactivate-mark))
    (unless (eq default-to 'never)
      (or (w3m-anchor)
	  (unless w3m-display-inline-images
	    (w3m-image))
	  (w3m-url-at-point)
	  (and default-to
	       (stringp w3m-current-url)
	       (if (string-match "\\`about://\\(?:header\\|source\\)/"
				 w3m-current-url)
		   (substring w3m-current-url (match-end 0))
		 (let ((name
			(or
			 (car (get-text-property (point) 'w3m-name-anchor))
			 (car (get-text-property (point) 'w3m-name-anchor2)))))
		   (if name
		       (concat w3m-current-url "#" name)
		     w3m-current-url))))))))

(declare-function w3m-search-do-search "w3m-search")
(defvar w3m-search-default-engine)

(defun w3m-canonicalize-url (url &optional feeling-searchy)
  "Fix URL that does not look like a valid url.
For a query that neither has a scheme part nor is an existing
local file, return a search url for the default search engine.
Also fix URL that fails to have put a separator following a
domain name."
  (if (string-match "\\`\\(https?://[-.0-9a-z]+\\)\\([#?].*\\)" url)
      (concat (match-string 1 url) "/" (match-string 2 url))
    (let (replaced)
      (cond
       ((and (setq replaced (ignore-errors (w3m-uri-replace url)))
	     (not (string= url replaced)))
	replaced)
       ((file-exists-p url)
	(concat "file://" (expand-file-name url)))
       ((progn (w3m-string-match-url-components url) (match-beginning 1))
	url)
       (feeling-searchy
	(require 'w3m-search)
	(w3m-search-do-search (lambda (url &rest _rest) url)
			      w3m-search-default-engine url))
       (t
	(concat "https://" url))))))

(defcustom w3m-input-url-provide-initial-content nil
  "Provide an initial minibuffer content (if any) when entering a url.
A url string is not worth editing in most cases since a url thing is
generally a list of arbitrary letters, not a human readable one.  So,
we provide no initial content when prompting you for a url by default.
But sometimes there will be a case to be convenient if you can modify
the url string of [1]the link under the cursor or of [2]the current
page.  In that case, you can type the `M-n' key [1]once or [2]twice to
fill the minibuffer with an initial content.  Otherwise, set this
variable to a non-nil value to always provide an initial content."
  :group 'w3m
  :type 'boolean)

(defun w3m-input-url-default-add-completions ()
  "Use the current url string (if any) as the next history by default.
This function is used as `minibuffer-default-add-function'."
  (let* ((to-add (with-current-buffer
		     (window-buffer (minibuffer-selected-window))
		   (or (w3m-active-region-or-url-at-point) w3m-current-url)))
	 (def minibuffer-default)
	 (all (all-completions ""
			       minibuffer-completion-table
			       minibuffer-completion-predicate))
	 (add2 (if (listp to-add) to-add (list to-add)))
	 (def2 (if (listp def) def (list def))))
    (append def2 add2
	    (delete def2 (delete def (delete add2 (delete to-add all)))))))

(defun w3m--get-url-from-bookmark-title (url)
  "Return the actual url of a bookmark title entered to `w3m-input-url'.
If in response to function `w3m-input-url',a user entered a
bookmark title string, replace it with that bookmark's url."
  ;; TODO: Consider storing the flat bookmark data in memory instead
  ;;       of constantly going to disk and re-evaluating it based upon
  ;;       function `w3m-bookmark-iterator'.
  (let ((bookmarks (w3m-bookmark-iterator))
	category entry found)
    (while (and (not found)
		(setq category (cdr (pop bookmarks))))
      (while (and (not found)
		  (setq entry (pop category)))
	(when (string= url
		       (replace-regexp-in-string "\\`[\s\t]+" "" (cdr entry)))
	  (setq url (car entry))
	  (setq found t))))
    url))

(defun w3m-input-url (&optional prompt initial default quick-start
				feeling-searchy no-initial)
  "Read a url from the minibuffer, prompting with string PROMPT."
  (let ((url
	 ;; Check whether the region is active.
	 (w3m-active-region-or-url-at-point 'never)))
    (w3m-arrived-setup)
    (cond ((null initial)
	   (when (setq initial
		       (or url
			   (when (or w3m-input-url-provide-initial-content
				     (not no-initial))
			     (w3m-active-region-or-url-at-point t))))
	     (if (string-match "\\`about:" initial)
		 (setq initial nil)
	       (unless (string-match "[^\000-\177]" initial)
		 (setq
		  initial
		  (w3m-url-decode-string initial
					 (or (w3m-url-coding-system initial)
					     w3m-current-coding-system)
					 "%\\([2-9a-f][0-9a-f]\\)"))))))
	  ((string= initial "")
	   (setq initial nil)))
    (when initial
      (setq initial (w3m-puny-decode-url initial)))
    (cond ((null default)
	   (setq default (or url
			     (w3m-active-region-or-url-at-point)
			     w3m-home-page)))
	  ((string= default "")
	   (setq default nil)))
    (if (and quick-start
	     default
	     (not initial))
	default
      (unless w3m-enable-feeling-searchy
	(setq feeling-searchy nil))
      (setq url (let ((minibuffer-setup-hook
		       (append minibuffer-setup-hook
			       (list (lambda ()
				       (set
					(make-local-variable
					 'minibuffer-default-add-function)
					'w3m-input-url-default-add-completions)
				       (beginning-of-line)
				       (if (looking-at "[a-z]+:\\(?:/+\\)?")
					   (goto-char (match-end 0)))))))
		      (keymap (copy-keymap w3m-url-completion-map))
		      (minibuffer-completion-table 'w3m-url-completion)
		      (minibuffer-completion-predicate nil)
		      (minibuffer-completion-confirm nil))
		  (set-keymap-parent keymap minibuffer-local-completion-map)
		  (read-from-minibuffer
		   (if prompt
		       (if default
			   (progn
			     (when (string-match " *: *\\'" prompt)
			       (setq prompt
				     (substring prompt 0
						(match-beginning 0))))
			     (concat prompt " (default "
				     (cond ((equal default "about:blank")
					    "BLANK")
					   ((equal default w3m-home-page)
					    "HOME")
					   ((equal default w3m-current-url)
					    "CURRENT")
					   (t default))
				     "): "))
			 prompt)
		     (if default
			 (format "URL %s(default %s): "
				 (if feeling-searchy "or Keyword " "")
				 (if (stringp default)
				     (cond ((string= default "about:blank")
					    "BLANK")
					   ((string= default w3m-home-page)
					    "HOME")
					   (t default))
				   (prin1-to-string default)))
		       (if feeling-searchy "URL or Keyword: " "URL: ")))
		   initial keymap nil 'w3m-input-url-history default)))
      (setq url (w3m--get-url-from-bookmark-title url))
      (if (string-equal url "")
	  (or default ;; It may be a symbol like `popup'.
	      "")
	(when (stringp url)
	  ;; remove duplication
	  (setq w3m-input-url-history
		(cons url (delete url w3m-input-url-history))))
	url))))

;;; Cache:
(defun w3m-cache-setup ()
  "Initialize the variables for managing the cache."
  (unless (and (bufferp w3m-cache-buffer)
	       (buffer-live-p w3m-cache-buffer))
    (with-current-buffer (w3m-get-buffer-create " *w3m cache*")
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (set (make-local-variable 'w3m-cache-articles) nil)
      (setq buffer-read-only t
	    w3m-cache-buffer (current-buffer)
	    w3m-cache-hashtb (make-hash-table :test 'equal)))))

(defun w3m-cache-shutdown ()
  "Clear all the variables managing the cache, and the cache itself."
  (when (buffer-live-p w3m-cache-buffer)
    (kill-buffer w3m-cache-buffer))
  (setq w3m-cache-hashtb nil))

(defun w3m-cache-header-delete-variable-part (header)
  (let (buf flag)
    (dolist (line (split-string header "\n+"))
      (if (string-match "\\`\\(?:Date\\|Server\\|W3m-[^:]+\\):" line)
	  (setq flag t)
	(unless (and flag (string-match "\\`[ \t]" line))
	  (setq flag nil)
	  (push line buf))))
    (mapconcat (function identity) (nreverse buf) "\n")))

(defun w3m-cache-header (url header &optional overwrite)
  "Store HEADER into the cache so that it corresponds to URL.
If OVERWRITE is non-nil, it forces the storing even if there has
already been the data corresponding to URL in the cache."
  (w3m-cache-setup)
  (setq url (w3m-w3m-canonicalize-url url))
  (let ((cache (gethash url w3m-cache-hashtb 'void)))
    (if (eq cache 'void)
	(puthash url header w3m-cache-hashtb)
      (if (and (not overwrite)
	       (string= (w3m-cache-header-delete-variable-part header)
			(w3m-cache-header-delete-variable-part cache)))
	  cache
	(w3m-cache-remove url)
	(puthash url header w3m-cache-hashtb)))))

(defun w3m-cache-request-header (url)
  "Return the header string of URL when it is stored in the cache."
  (w3m-cache-setup)
  (let ((cache (gethash (w3m-w3m-canonicalize-url url) w3m-cache-hashtb 'void)))
    (unless (eq cache 'void)
      cache)))

(defun w3m-cache-remove-oldest ()
  "Remove the oldest cache.
Must run in `w3m-cache-buffer' where read-only is lifted."
  (when w3m-cache-articles
    (let* ((art (pop w3m-cache-articles))
	   (beg (cadr art))
	   (end (cddr art)))
      (remhash (car art) w3m-cache-hashtb)
      (delete-region beg end)
      (set-marker beg nil)
      (set-marker end nil))))

(defun w3m-cache-remove-1 (url)
  "Remove the data coresponding to URL from the cache.
Must run in `w3m-cache-buffer' where read-only is lifted."
  (let ((art (assoc url w3m-cache-articles))
	beg end)
    (when art
      ;; It should be in the cache.
      (remhash (car art) w3m-cache-hashtb)
      (delete-region (setq beg (cadr art)) (setq end (cddr art)))
      (set-marker beg nil)
      (set-marker end nil)
      (setq w3m-cache-articles (delq art w3m-cache-articles)))))

(defun w3m-cache-remove (url)
  "Remove the data coresponding to URL from the cache."
  (w3m-cache-setup)
  (with-current-buffer w3m-cache-buffer
    (let ((inhibit-read-only t))
      (inline (w3m-cache-remove-1 url)))))

(defun w3m-cache-contents (url buffer)
  "Store the contents of URL into the cache.
The contents are assumed to be in BUFFER."
  (w3m-cache-setup)
  (setq url (w3m-w3m-canonicalize-url url))
  (with-current-buffer w3m-cache-buffer
    (let ((inhibit-read-only t))
      (w3m-cache-remove-1 url)
      ;; Remove the oldest article, if necessary.
      (and (numberp w3m-keep-cache-size)
	   (>= (length w3m-cache-articles) w3m-keep-cache-size)
	   (w3m-cache-remove-oldest))
      ;; Insert the new article.
      (goto-char (point-max))
      (let ((b (point)))
	(insert-buffer-substring buffer)
	;; Tag the beginning of the article with URL.
	(when (> (point-max) b)
	  (setq w3m-cache-articles
		(nconc w3m-cache-articles
		       (list (cons url (cons (copy-marker b)
					     (point-max-marker)))))))))))

(defun w3m-cache-request-contents (url &optional buffer)
  "Insert contents of URL into BUFFER.
Return t if the contents are found in the cache, otherwise nil.  When
BUFFER is nil, all contents will be inserted in the current buffer."
  (w3m-cache-setup)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer w3m-cache-buffer
    (let ((art (assoc url w3m-cache-articles))
	  beg end)
      (when art
	;; It should be in the cache.
	(setq beg (cadr art)
	      end (cddr art))
	(with-current-buffer buffer
	  (let ((inhibit-read-only t))
	    (insert-buffer-substring w3m-cache-buffer beg end))
	  t)))))

;; FIXME: we need to check whether contents were updated in remote servers.
(defun w3m-cache-available-p (url)
  "Return non-nil if contents of URL has already been cached."
  ;; Note that as `string-match'es here are used to find a line in the middle
  ;; of a header string, "^" and "$" must not be replaced with "\\`" and "\\'".
  (w3m-cache-setup)
  (let (head time expire (case-fold-search t))
    (and (stringp url)
	 (not (eq (setq head (gethash url w3m-cache-hashtb 'void)) 'void))
	 (or
	  w3m-prefer-cache
	  (save-match-data
	    (cond
	     ((and (string-match "^\\(?:date\\|etag\\):[ \t]" head)
		   (or (string-match "^pragma:[ \t]+no-cache\n" head)
		       (string-match "\
^cache-control:\\(?:[^\n]+\\)?[ \t,]\\(?:no-cache\\|max-age=0\\)[,\n]"
				     head)))
	      nil)
	     ((and
	       (string-match "^date:[ \t]\\([^\n]+\\)\n" head)
	       (setq time (match-string 1 head))
	       (setq time (w3m-time-parse-string time))
	       (string-match "\
^cache-control:\\(?:[^\n]+\\)?[ \t,]max-age=\\([1-9][0-9]*\\)"
			     head)
	       (setq expire (string-to-number (match-string 1 head))))
	      (setq time (decode-time time))
	      (setcar time (+ (car time) expire))
	      ;; Work around too large integer.
	      (when (floatp (car time))
		(setcar time (eval '(lsh -1 -1))))
	      (setq expire (apply 'encode-time time))
	      (w3m-time-newer-p expire (current-time)))
	     ((and
	       (string-match "^expires:[ \t]+\\([^\n]+\\)\n" head)
	       (setq expire (match-string 1 head))
	       (setq expire (w3m-time-parse-string expire)))
	      (w3m-time-newer-p expire (current-time)))
	     (t
	      ;; Adhoc heuristic rule: pages with neither
	      ;; Last-Modified header and ETag header are treated as
	      ;; dynamically-generated pages.
	      (string-match "^\\(?:last-modified\\|etag\\):" head))))))))

(defun w3m-read-file-name (&optional prompt dir default existing)
  (unless prompt
    (setq prompt (if (and default (not (string-equal default "")))
		     (format "Save to (%s): " default)
		   "Save to: ")))
  (setq dir (file-name-as-directory (or dir w3m-default-save-directory)))
  (let ((default-directory dir)
	(file (read-file-name prompt dir nil existing default)))
    (if (not (file-directory-p file))
	(setq w3m-default-save-directory
	      (or (file-name-directory file) w3m-default-save-directory))
      (setq w3m-default-save-directory file)
      (if default
	  (setq file (expand-file-name default file))))
    (expand-file-name file)))

;;; Handling character sets:
(defun w3m-charset-to-coding-system (charset)
  "Return a coding system which is most suitable to CHARSET.
CHARSET is a symbol whose name is MIME charset.
This function is imported from mcharset.el."
  (if (stringp charset)
      (setq charset (intern (downcase charset))))
  (let ((cs (assq charset w3m-charset-coding-system-alist)))
    (w3m-find-coding-system (if cs (cdr cs) charset))))

;; FIXME: we need to investigate the kind of Content-Charsets being
;; actually possible.
(defun w3m-read-content-charset (prompt &optional default)
  "Read a content charset from the minibuffer, prompting with string PROMPT.
The second argument DEFAULT is the default value, which is used as the
value to return if the user enters the empty string."
  (let ((charset (completing-read
		  prompt
		  (nconc
		   (mapcar (lambda (c) (cons (symbol-name c) c))
			   (coding-system-list))
		   (mapcar (lambda (c) (cons (symbol-name (car c)) c))
			   w3m-charset-coding-system-alist))
		  nil t)))
    (if (string= "" charset)
	default
      charset)))

(declare-function zlib-available-p "decompress.c")
(declare-function zlib-decompress-region "decompress.c"
		  (start end &optional allow-partial))

;;; Handling encoding of contents:
(defun w3m-decode-encoded-contents (encoding)
  "Decode encoded contents in the current buffer.  Return t if successful.
This function supports the encoding types gzip, bzip, and deflate."
  (let ((x (and (stringp encoding)
		(assoc (downcase encoding) w3m-encoding-alist))))
    (or (and (eq (cdr x) 'gzip) (fboundp 'zlib-available-p) (zlib-available-p)
	     (zlib-decompress-region (point-min) (point-max)))
	(not (and x (setq x (cdr (assq (cdr x) w3m-decoder-alist)))))
	(let ((coding-system-for-write 'binary)
	      (coding-system-for-read 'binary)
	      (default-process-coding-system (cons 'binary 'binary)))
	  (w3m-process-with-environment w3m-command-environment
	    (zerop (apply 'call-process-region
			  (point-min) (point-max)
			  (w3m-which-command (car x))
			  t '(t nil) nil (cadr x))))))))

(defmacro w3m-correct-charset (charset)
  `(or (and ,charset (stringp ,charset)
	    (cdr (assoc (downcase ,charset) w3m-correct-charset-alist)))
       ,charset))

(defun w3m-detect-meta-charset ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward "\\(<!--\\)\\|<meta[ \t\r\f\n]+" nil t)
	(if (match-beginning 1)
	    (w3m-end-of-tag)
	  (w3m-parse-attributes ((http-equiv :case-ignore)
				 (content :case-ignore))
	    (when (and (string= http-equiv "content-type")
		       content
		       (string-match ";[ \t\n]*charset=\\([^\";]+\\)" content))
	      (throw 'found (match-string 1 content)))))))))

(defun w3m-detect-xml-charset ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (looking-at "[ \t\r\f\n]*<\\?xml[ \t\r\f\n]+")
      (goto-char (match-end 0))
      (or (w3m-parse-attributes ((encoding :case-ignore))
	    encoding)
	  "utf-8"))))

(defvar w3m-compatible-encoding-alist
  '((gb2312 . gbk)
    (iso-8859-1 . windows-1252)
    (iso-8859-8 . windows-1255)
    (iso-8859-9 . windows-1254))
  "Alist of encodings and those supersets.
The cdr of each element is used to decode data if it is available when
the car is what the data specify as the encoding.  Or, the car is used
for decoding when the cdr that the data specify is not available.")

(defvar w3m-view-source-decode-level 0
  "Say whether `w3m-view-source' decodes html sources.
Users should never modify the value.  See also `w3m-view-source'.")

(defun w3m-decode-buffer (url &optional content-charset content-type)
  (let* ((sourcep (string-match "\\`about://source/" url))
	 (level (if sourcep w3m-view-source-decode-level 0))
	 cs)
    (unless (>= level 4)
      (unless content-type
	(setq content-type (w3m-content-type url)))
      (unless content-charset
	(setq content-charset
	      (or (w3m-content-charset url)
		  (when (or (string= "text/html" content-type) sourcep)
		    (w3m-detect-meta-charset))
		  (w3m-detect-xml-charset))))
      (cond
       ((or (and (stringp content-charset)
		 (string= "x-moe-internal" (downcase content-charset)))
	    (eq content-charset 'x-moe-internal))
	(setq cs (w3m-x-moe-decode-buffer))
	(setq content-charset (symbol-name cs)))
       (content-charset
	(setq content-charset (w3m-correct-charset content-charset))
	(setq cs (w3m-charset-to-coding-system content-charset))))
      (setq w3m-current-content-charset content-charset)
      (unless cs
	(setq cs (w3m-detect-coding-region
		  (point-min) (point-max) (if (w3m-url-local-p url)
					      nil
					    w3m-coding-system-priority-list))))
      (setq w3m-current-coding-system
	    (or (w3m-find-coding-system
		 (cdr (assq cs w3m-compatible-encoding-alist)))
		(w3m-find-coding-system cs)
		(w3m-find-coding-system
		 (car (rassq cs w3m-compatible-encoding-alist)))))
      ;; Decode `&#nnn;' entities in 128..159 and 160.
      (when (and (<= level 1)
		 (rassq w3m-current-coding-system
			w3m-compatible-encoding-alist))
	(goto-char (point-min))
	(let ((case-fold-search t))
	  (while (re-search-forward "\
\\(?:&#\\(12[89]\\|1[3-5][0-9]\\)\;\\)\\|\\(?:&#x\\([89][0-9a-f]\\)\;\\)"
				    nil t)
	    (insert (prog1
			(if (match-beginning 1)
			    (string-to-number (match-string 1))
			  (string-to-number (match-string 2) 16))
		      (delete-region (match-beginning 0) (match-end 0)))))
	  (goto-char (point-min))
	  (while (re-search-forward "\240\\|&#160;\\|&#xa0;" nil t)
	    (replace-match "&nbsp;"))))
      (insert
       (prog1
	   (decode-coding-string (buffer-string) w3m-current-coding-system)
	 (erase-buffer)
	 (set-buffer-multibyte t))))))

(defun w3m-x-moe-decode-buffer ()
  (let ((args '("-i" "-cs" "x-moe-internal"))
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(default-process-coding-system (cons 'binary 'binary))
	charset)
    (if (w3m-find-coding-system 'utf-8)
	(setq args (append args '("-o" "-cs" "utf-8"))
	      charset 'utf-8)
      (setq args
	    (append args (list "-o" "-cs" (symbol-name w3m-coding-system))))
      (setq charset w3m-coding-system))
    (w3m-process-with-environment w3m-command-environment
      (apply 'call-process-region (point-min) (point-max)
	     w3m-mbconv-command t t nil args))
    charset))

(defun w3m-safe-decode-buffer (url &optional content-charset content-type)
  (and (not w3m-current-coding-system)
       (stringp content-type)
       (string-match "\\`text/" content-type)
       (w3m-decode-buffer url content-charset content-type)))

;;; Retrieving local data:
(defun w3m-local-file-type (url)
  "Return the content type and the content encoding of URL."
  (setq url (or (w3m-url-to-file-name url)
		(file-name-nondirectory url)))
  (if (or (and (file-name-absolute-p url)
	       (file-directory-p url))
	  (string-match "\\`news:" url)) ;; FIXME: isn't this a kludge?
      (cons "text/html" nil)
    (let ((encoding
	   (catch 'encoding-detected
	     (dolist (elem w3m-encoding-type-alist)
	       (when (string-match (car elem) url)
		 (setq url (substring url 0 (match-beginning 0)))
		 (throw 'encoding-detected (cdr elem)))))))
      (cons (catch 'type-detected
	      (dolist (elem w3m-content-type-alist)
		(when (and (cadr elem) (string-match (cadr elem) url))
		  (throw 'type-detected (car elem))))
	      "unknown")
	    encoding))))

(defmacro w3m-local-content-type (url)
  `(car (w3m-local-file-type ,url)))

(defun w3m-local-attributes (url &rest _args)
  "Return a list of attributes corresponding to URL.
Return nil if it failed in retrieving of the header.
Otherwise, return a list which includes the following elements:

 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
"
  (let* ((file (w3m-url-to-file-name url))
	 (attr (when (file-exists-p file)
		 (file-attributes file)))
	 (type (w3m-local-file-type url)))
    (list (or (w3m-arrived-content-type url) (car type))
	  nil
	  (nth 7 attr)
	  (cdr type)
	  (nth 5 attr)
	  (w3m-expand-file-name-as-url (file-truename file)))))

(defun w3m-local-retrieve (url &optional no-uncompress &rest _args)
  "Retrieve contents of local URL and put it into the current buffer.
This function will return the content-type of URL as a string when
retrieval is successful."
  (let ((file (w3m-url-to-file-name url)))
    (when (file-readable-p file)
      (if (file-directory-p file)
	  (w3m-local-dirlist-cgi url)
	(let ((coding-system-for-read 'binary))
	  (if no-uncompress
	      (let (jka-compr-compression-info-list
		    format-alist)
		(insert-file-contents file))
	    (insert-file-contents file))))
      (or (w3m-arrived-content-type url)
	  (w3m-local-content-type file)))))

(defun w3m-local-dirlist-cgi (url)
  (w3m-message "Reading %s..." (w3m-url-readable-string url))
  (if w3m-dirlist-cgi-program
      (if (file-executable-p w3m-dirlist-cgi-program)
	  (let ((coding-system-for-read 'binary)
		(default-process-coding-system
		  (cons 'binary 'binary))
		(lcookie (make-temp-name
			  (format "%s.%d." (user-login-name) (emacs-pid))))
		(cfile (make-temp-name
			(expand-file-name "w3melck" w3m-profile-directory)))
		(env (copy-sequence w3m-command-environment))
		file)
	    (with-temp-buffer
	      (insert lcookie)
	      (write-region (point-min) (point-max) cfile 'nomsg))
	    (w3m-process-with-environment
		(append
		 (list
		  (cons "LOCAL_COOKIE" lcookie)
		  (cons "LOCAL_COOKIE_FILE" cfile)
		  (cons "QUERY_STRING"
			(format
			 "dir=%s&cookie=%s"
			 (encode-coding-string (w3m-url-to-file-name url)
					       w3m-file-name-coding-system)
			 lcookie)))
		 (delq (assoc "LOCAL_COOKIE" env)
		       (delq (assoc "LOCAL_COOKIE_FILE" env)
			     (delq (assoc "QUERY_STRING" env) env))))
	      (call-process w3m-dirlist-cgi-program nil t nil))
	    ;; delete local cookie file
	    (when (and (file-exists-p cfile) (file-writable-p cfile))
	      (delete-file cfile))
	    (goto-char (point-min))
	    (when (re-search-forward "^<html>" nil t)
	      (delete-region (point-min) (match-beginning 0))
	      (while (re-search-forward "<a href=\"\\([^\"]+\\)\"\\(?:>\\| \\)"
					nil t)
		(setq file (match-string 1))
		(delete-region (goto-char (match-beginning 1)) (match-end 1))
		(if (file-directory-p file)
		    (setq file (w3m-expand-file-name-as-url
				(file-name-as-directory file)))
		  (setq file (w3m-expand-file-name-as-url file)))
		(insert (encode-coding-string
			 (w3m-url-decode-string file
						w3m-file-name-coding-system)
			 w3m-file-name-coding-system)))))
	(error "Can't execute: %s" w3m-dirlist-cgi-program))
    ;; execute w3m internal CGI
    (w3m-process-with-wait-handler
      (setq w3m-current-url url)
      (w3m-process-start handler
			 w3m-command
			 (append w3m-command-arguments
				 (list "-dump_source" url)))))
  ;; bind charset to w3m-file-name-coding-system
  (let ((charset (or (car (rassq w3m-file-name-coding-system
				 w3m-charset-coding-system-alist))
		     w3m-file-name-coding-system))
	beg)
    (goto-char (point-min))
    (when (search-forward "<head>" nil t)
      (insert "\n<meta http-equiv=\"CONTENT-TYPE\" "
	      "content=\"text/html; charset="
	      (symbol-name charset)
	      "\">"))
    (goto-char (point-min))
    ;; Remove <form>...</form>
    (when (search-forward "<form " nil t)
      (setq beg (match-beginning 0))
      (when (search-forward "</form>" nil t)
	(delete-region beg (match-end 0)))))
  (w3m-message "Reading %s...done" (w3m-url-readable-string url)))

;;; Retrieving data via HTTP:
(defun w3m-remove-redundant-spaces (str)
  "Remove leading and trailing whitespace from STR."
  (save-match-data
    (when (string-match "\\`[ \t\r\f\n]+" str)
      (setq str (substring str (match-end 0))))
    (if (string-match "[ \t\r\f\n]+\\'" str)
	(substring str 0 (match-beginning 0))
      str)))

(defun w3m-w3m-parse-header (url header)
  "Parse a given string HEADER as a MIME header of URL.
Return a list which includes:

 0. Status code.
 1. Type of contents.
 2. Charset of contents.
 3. Size in bytes.
 4. Encoding of contents.
 5. Last modification time.
 6. Real URL.
"
  (let ((case-fold-search t)
	(headers)
	(status))
    (dolist (line (split-string header "[ \f\t\r]*\n"))
      (cond
       ((string-match "\\`HTTP/1\\.[0-9] \\([0-9][0-9][0-9]\\)\\b" line)
	(setq status (string-to-number (match-string 1 line))))
       ((string-match (eval-when-compile
			(concat "\\`\\("
				(regexp-opt
				 '(;; MEMO: ファイルをダウンロードする
				   ;; ときの 適切なデフォルト名を決定
				   ;; するためには content-disposition
				   ;; の解釈が必要．
				   "content-disposition"
				   "content-encoding"
				   "content-length"
				   ;; MEMO: See [emacs-w3m:02341].
				   "content-transfer-encoding"
				   "content-type"
				   "last-modified"
				   "location"
				   "w3m-current-url"
				   "w3m-document-charset"
				   "w3m-ssl-certificate"
				   "x-w3m-content-encoding"
				   "alternates"))
				"\\):[ \t]*"))
		      line)
	(push (cons (downcase (match-string 1 line))
		    (substring line (match-end 0)))
	      headers))))
    (let (alt real-url type charset xmoe)
      (when (and (setq alt (cdr (assoc "alternates" headers)))
		 (string-match "\\`{[\t ]*\"\\(.+\\)\"" alt))
	(setq real-url (w3m-expand-url (match-string 1 alt) url))
	(when (string-match "{[\t ]*type[\t ]+\\([^\t }]+\\)" alt)
	  (setq type (downcase (match-string 1 alt))))
	(when (string-match "{[\t ]*charset[\t ]+\\([^\t }]+\\)" alt)
	  (setq charset (downcase (match-string 1 alt)))))
      (when (and (not type)
		 (setq type (cdr (assoc "content-type" headers))))
	(if (string-match ";[ \t]*charset=\"?\\([^\"]+\\)\"?" type)
	    (setq charset (w3m-remove-redundant-spaces
			   (match-string 1 type))
		  type (w3m-remove-redundant-spaces
			(substring type 0 (match-beginning 0))))
	  (setq type (w3m-remove-redundant-spaces type)))
	(when (string-match ";" type)
	  (setq type (substring type 0 (match-beginning 0))))
	(setq type (downcase type)))
      (setq w3m-current-ssl (cdr (assoc "w3m-ssl-certificate" headers)))
      (when (string-match "\\`ftps?:" url)
	(setq url (or (cdr (assoc "w3m-current-url" headers))
		      url)))
      (when (and (setq xmoe (cdr (assoc "w3m-document-charset" headers)))
		 (string= xmoe "x-moe-internal"))
	(setq charset xmoe))
      (list status
	    (if (string-match "\\`ftps?:.*/\\'" url)
		"text/html"
	      (or type (w3m-local-content-type url)))
	    charset
	    (let ((v (cdr (assoc "content-length" headers))))
	      (and v (setq v (string-to-number v)) (> v 0) v))
	    (cdr (or (assoc "content-encoding" headers)
		     (assoc "x-w3m-content-encoding" headers)))
	    (let ((v (cdr (assoc "last-modified" headers))))
	      (and v (w3m-time-parse-string v)))
	    (or real-url
		(let ((v (cdr (assoc "location" headers))))
		  ;; RFC2616 says that the field value of the Location
		  ;; response-header consists of a single absolute
		  ;; URI.  However, some broken servers return
		  ;; relative URIs.
		  (and v (w3m-expand-url v url)))
		url)))))

(defun w3m-w3m-dump-head (url handler)
  "Return the header string of URL."
  (let ((silent w3m-message-silent))
    (w3m-message "Request sent, waiting for response...")
    (w3m-process-do-with-temp-buffer
	(success (progn
		   (setq w3m-current-url url
			 url (w3m-url-strip-authinfo url))
		   (w3m-process-start handler
				      w3m-command
				      (append w3m-command-arguments
					      (list "-o" "follow_redirection=0"
						    "-dump_head" url)))))
      (let ((w3m-message-silent silent))
	(w3m-message "Request sent, waiting for response...done")
	(when success
	  (buffer-string))))))

(defun w3m-w3m-canonicalize-url (url)
  "Add a slash to an URL, when its server part is not ended with a slash."
  ;; Because URLs encountered in WEB documents are no less reliable
  ;; than URLs given by users, a minimum canonicalization may be
  ;; required in the backend side.  For more detail, please see
  ;; [emacs-w3m:07000].
  (if (string-match "\\`\\(?:ht\\|f\\)tps?://[^/]+\\'" url)
      (concat url "/")
    url))

(defun w3m-w3m-attributes (url no-cache handler)
  "Return a list of attributes corresponding to URL.
Return nil if it failed in retrieving of the header.
Otherwise, return a list which includes the following elements:

 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.

If the optional argument NO-CACHE is non-nil, cache is not used."
  (w3m-w3m-attributes-1 (w3m-w3m-canonicalize-url url)
			no-cache
			(or w3m-follow-redirection 0)
			handler))

(defun w3m-w3m-attributes-1 (url no-cache counter handler)
  "A subroutine for `w3m-w3m-attributes'."
  (w3m-process-do
      (header (or (unless no-cache
		    (w3m-cache-request-header url))
		  (w3m-w3m-dump-head url handler)))
    (when header
      (let ((attr (w3m-w3m-parse-header url header)))
	(w3m-cache-header url header)
	(if (memq (car attr) '(301 302 303 304 305 306 307))
	    (if (zerop counter)
		;; Redirect counter exceeds `w3m-follow-redirection'.
		(list "text/html" "us-ascii" nil nil nil url)
	      ;; Follow redirection.
	      (w3m-w3m-attributes-1 (nth 6 attr) no-cache
				    (1- counter) handler))
	  (cdr attr))))))

(defun w3m-w3m-expand-arguments (arguments &optional charset)
  "Expand ARGUMENTS so to be used as arguments passed to `w3m-command'.
CHARSET is used to substitute the `charset' symbols specified in
ARGUMENTS with its value."
  (let ((bndp (boundp 'charset))
	(chst charset))
    (unless bndp (set (intern "charset") nil))
    ;; Now `charset' is dynamically bound.  It will match with the symbols
    ;; of the same name specified in `w3m-halfdump-command-arguments'.
    (unwind-protect
	(let ((charset chst))
	  (apply 'append
		 (mapcar
		  (lambda (x)
		    (cond
		     ((stringp x) (list x))
		     ((setq x (eval x))
		      (cond ((stringp x)
			     (list x))
			    ((listp x)
			     (w3m-w3m-expand-arguments x charset))
			    (t
			     (let (print-level print-length)
			       (list (prin1-to-string x))))))))
		  arguments)))
      (unless bndp (makunbound 'charset)))))

(defun w3m--dump-extra--handler-function (url silent success)
  (let ((w3m-message-silent silent))
    (w3m-message "Reading %s...done" (w3m-url-readable-string url))
    (when success
      (goto-char (point-min))
      (let ((case-fold-search t))
	(when (and (re-search-forward "^w3m-current-url:" nil t)
		   (progn
		     (delete-region (point-min) (match-beginning 0))
		     (search-forward "\n\n" nil t)))
	  (let ((header (buffer-substring (point-min) (point))))
	    (when w3m-use-cookies
	      (w3m-cookie-set url (point-min) (point)))
	    (delete-region (point-min) (point))
	    (w3m-cache-header url header)
	    (w3m-cache-contents url (current-buffer))
	    (w3m-w3m-parse-header url header)))))))

(defun w3m-w3m-dump-extra (url handler)
  "Retrive headers and contents pointed to by URL"
  (let ((silent w3m-message-silent))
    (setq w3m-current-url url
	  url (w3m-url-strip-authinfo url))
    (w3m-message "Reading %s...%s"
		 (w3m-url-readable-string url)
		 (if (and w3m-async-exec (not w3m-process-waited))
		     (substitute-command-keys "\
\n (Type `\\<w3m-mode-map>\\[w3m-process-stop]' to stop asynchronous process)")
		   ""))
    (w3m-process-do
	(success (w3m-process-start
		  handler w3m-command
		  (append w3m-command-arguments
			  (w3m-w3m-expand-arguments
			   w3m-dump-head-source-command-arguments)
			  (list url))))
      (w3m--dump-extra--handler-function url silent success))))

(defun w3m-additional-command-arguments (url)
  "Return a list of additional arguments passed to the w3m command.
You may specify additional arguments for the particular urls using the
option `w3m-command-arguments-alist', or using `w3m-no-proxy-domains'
to add the option \"-no-proxy\"."
  (let ((defs w3m-command-arguments-alist)
	def args host)
    (while (and defs
		(null args))
      (setq def (car defs)
	    defs (cdr defs))
      (when (string-match (car def) url)
	(setq args (cdr def))))
    (when (and w3m-no-proxy-domains
	       (not (member "-no-proxy" args))
	       (string-match "\\`[a-z]+://\\([^/:]+\\)" url)
	       (catch 'domain-match
		 (setq host (match-string 1 url))
		 (dolist (domain w3m-no-proxy-domains)
		   (when (string-match (concat "\\(?:\\`\\|\\.\\)"
					       (regexp-quote domain)
					       "\\'")
				       host)
		     (throw 'domain-match t)))))
      (push "-no-proxy" args))
    args))

(defun w3m-add-referer-p (url referer)
  "Return non-nil when URL and REFERER satisfies the condition
specified by `w3m-add-referer'."
  (when (stringp referer)
    (cond
     ((eq w3m-add-referer 'lambda)
      (let (host)
	(w3m-string-match-url-components url)
	(when (match-beginning 4)
	  (setq host (match-string 4 url))
	  (w3m-string-match-url-components referer)
	  (when (match-beginning 4)
	    (string= host (match-string 4 referer))))))
     ((consp w3m-add-referer)
      (and (not (and (cdr w3m-add-referer)
		     (string-match (cdr w3m-add-referer) referer)))
	   (car w3m-add-referer)
	   (string-match (car w3m-add-referer) referer)))
     ((functionp w3m-add-referer)
      (funcall w3m-add-referer url referer))
     (t w3m-add-referer))))

;; Currently, -request argument is supported only by w3mmee.
(defun w3m-request-arguments (method url temp-file
				     &optional body referer content-type)
  "Make the arguments for `-request' or `-header' option passed to w3m.
METHOD is an HTTP method name.
TEMP-FILE is a name of temporary file to write request content to.
Optional BODY is the body content string.
Second optional REFERER is the Referer: field content.
Third optional CONTENT-TYPE is the Content-Type: field content."
  (with-temp-buffer
    (let ((modes (default-file-modes))
	  (cookie (and w3m-use-cookies (w3m-cookie-get url))))
      (if (and (null cookie)(null body)
	       (null content-type))
	  (append
	   (list "-header" (concat "User-Agent:"
				   (if w3m-add-user-agent
				       (or (w3m-user-agent-site-specific url)
					   w3m-user-agent)
				     "?")))
	   (when (w3m-add-referer-p url referer)
	     (list "-header" (concat "Referer: " referer)))
	   (when w3m-accept-languages
	     (list "-header" (concat
			      "Accept-Language: "
			      (mapconcat 'identity w3m-accept-languages
					 " ")))))
	(insert "User-Agent: " (if w3m-add-user-agent w3m-user-agent "?") "\n")
	(when (w3m-add-referer-p url referer)
	  (insert "Referer: " referer "\n"))
	(when w3m-accept-languages
	  (insert "Accept-Language: "
		  (mapconcat 'identity w3m-accept-languages " ") "\n"))
	(when cookie
	  (insert "Cookie: " cookie "\n"))
	(when content-type
	  (insert "Content-Type: " content-type "\n"))
	(insert "\n")
	(when body
	  (insert body))
	(unwind-protect
	    (let ((coding-system-for-write 'binary))
	      (set-default-file-modes (* 64 6))
	      (write-region (point-min) (point-max) temp-file nil 'silent))
	  (set-default-file-modes modes))
	(list "-request" (concat method ":" temp-file))))))

;; Currently, w3m uses this function.
(defun w3m-header-arguments (method url temp-file
				    &optional body referer content-type)
  "Make the arguments for the `-header' option passed to the w3m command.
METHOD is an HTTP method name.
TEMP-FILE is a name of temporary file to write post body to.
Optional BODY is the post body content string.
Optional second REFERER is the Referer: field content.
Third optional CONTENT-TYPE is the Content-Type: field content."
  (let ((modes (default-file-modes))
	(cookie (and w3m-use-cookies (w3m-cookie-get url)))
	args)
    (setq args
	  (nconc args
		 (list "-o" (concat "user_agent="
				    (if w3m-add-user-agent
					(or (w3m-user-agent-site-specific url)
					    w3m-user-agent)
				      "?")))))
    (when cookie
      (setq args (nconc args
			(list "-header" (concat "Cookie: " cookie)))))
    (when (and (string= method "post") temp-file)
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(when body (insert body))
	(unwind-protect
	    (let ((coding-system-for-write 'binary))
	      (set-default-file-modes (* 64 6))
	      (write-region (point-min) (point-max) temp-file nil 'silent))
	  (set-default-file-modes modes)))
      (setq args (nconc args
			(when content-type
			  (list "-header" (concat "Content-Type: "
						  content-type)))
			(list "-post" temp-file))))
    (when (w3m-add-referer-p url referer)
      (setq args (nconc args (list "-header" (concat "Referer: " referer)))))
    args))

(defun w3m--retrieve--handler-function (url silent no-uncompress current-buffer
					    attr)
  (setq w3m-http-status (car-safe attr))
  (let ((w3m-message-silent silent))
    (when attr
      (cond
       ((eq attr 'redirection-exceeded)
	"X-w3m-error/redirection")
       ((or (not (string-match "\\`https?:" url))
	    (memq (car attr) '(200 300)))
	(if (or no-uncompress
		(w3m-decode-encoded-contents (nth 4 attr)))
	    (let ((temp-buffer (current-buffer)))
	      (with-current-buffer current-buffer
		(insert-buffer-substring temp-buffer))
	      (goto-char (point-min))
	      (cadr attr))
	  (ding)
	  (w3m-message "Can't decode encoded contents: %s" url)
	  nil))
       (t nil)))))

(defun w3m-w3m-retrieve (url no-uncompress no-cache post-data referer handler)
  "Retrieve web contents pointed to by URL using the external w3m command.
It will put the retrieved contents into the current buffer.  See
`w3m-retrieve' for how does it work asynchronously with the arguments."
  (setq url (w3m-w3m-canonicalize-url url))
  (let ((current-buffer (current-buffer))
	(silent w3m-message-silent))
    (w3m-process-do-with-temp-buffer
	(attr (progn
		(set-buffer-multibyte nil)
		(w3m-w3m-retrieve-1 url post-data referer no-cache
				    (or w3m-follow-redirection 0) handler)))
      (w3m--retrieve--handler-function url silent no-uncompress current-buffer
				       attr))))

(defun w3m--retrieve-1--handler-function (url post-data referer no-cache
					      counter handler temp-file attr)
  (when (nth 6 attr)
    (setf (nth 6 attr) (w3m--url-strip-queries (nth 6 attr))))
  (and temp-file
       (file-exists-p temp-file)
       (delete-file temp-file))
  (if (memq (car attr) '(301 302 303 304 305 306 307))
      (if (zerop counter)
	  ;; Redirect counter exceeds `w3m-follow-redirection'.
	  'redirection-exceeded
	;; Follow redirection.
	(erase-buffer)
	(unless (and post-data
		     (cond
		      ((memq (car attr) '(301 302))
		       (if w3m-redirect-with-get
			   (setq post-data nil)
			 (not (y-or-n-p
			       (format "Send POST data to `%s'?" url)))))
		      ((eq (car attr) 303) ; => See Other
		       (setq post-data nil))
		      ((eq (car attr) 307) ; => Temporary redirection
		       (not (y-or-n-p
			     (format "Send POST data to `%s'?" url))))))
	  (w3m-w3m-retrieve-1
	   (nth 6 attr) post-data referer no-cache (1- counter) handler)))
    (if (and (eq (car attr) 406)
	     (not (equal url (nth 6 attr))))
	;; Attempt to retrieve an alternative url.
	(progn
	  (erase-buffer)
	  (w3m-w3m-retrieve-1 (nth 6 attr) post-data referer no-cache
			      counter handler))
      (or (w3m-w3m-onload-redirection attr counter) attr))))

(defun w3m-w3m-retrieve-1 (url post-data referer no-cache counter handler)
  "A subroutine for `w3m-w3m-retrieve'."
  (let ((w3m-command-arguments
	 (append w3m-command-arguments
		 (when (member "cookie" w3m-compile-options)
		   (list "-no-cookie"))
		 (list "-o" "follow_redirection=0")
		 (w3m-additional-command-arguments url)))
	(cachep (w3m-cache-available-p url))
	temp-file)
    (when (and w3m-broken-proxy-cache
	       (or no-cache (not cachep)))
      (setq w3m-command-arguments
	    (append w3m-command-arguments '("-o" "no_cache=1"))))
    (setq temp-file
	  (when (or (eq w3m-type 'w3mmee) post-data)
	    (make-temp-name
	     (expand-file-name "w3mel" w3m-profile-directory))))
    (setq w3m-command-arguments
	  (append w3m-command-arguments
		  (apply (if (eq w3m-type 'w3mmee)
			     'w3m-request-arguments
			   'w3m-header-arguments)
			 (list (if post-data "post" "get")
			       url
			       temp-file
			       (if (consp post-data)
				   (cdr post-data)
				 post-data)
			       referer
			       (if (consp post-data) (car post-data))))))
    (w3m-process-do
	(attr (or (unless no-cache
		    (and cachep
			 (w3m-cache-request-contents url)
			 (w3m-w3m-parse-header
			  url (w3m-cache-request-header url))))
		  (w3m-w3m-dump-extra url handler)))
      (w3m--retrieve-1--handler-function
       url post-data referer no-cache counter handler temp-file attr))))

(defvar w3m-onload-url nil "Url redirected to by onload.")

(defun w3m-w3m-onload-redirection (attr counter)
  "Do the onload redirection in the current buffer.  Return ATTR or nil.
Run recursively together with `w3m-w3m-retrieve-1' and replace ATTR's
and the buffer's contents with new data if redirection is done within
COUNTER times.  ATTR is a list of attributions of raw contents having
retrieved in the buffer."
  (and w3m-use-form
       w3m-use-cookies
       (eq (car attr) 200)
       (equal (cadr attr) "text/html")
       (let ((case-fold-search t)
	     decoded form xurl post-data)
	 (w3m-decode-encoded-contents (nth 4 attr))
	 (setf (nth 4 attr) nil)
	 (goto-char (point-min))
	 ;; FIXME: Is there any other name that does not end
	 ;; with ".submit" for the function used to sumbit?
	 (when (re-search-forward "\
<body[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*onload=\
[^\t\n\r ()>]+\\.submit()" nil t)
	   (setq decoded (buffer-string))
	   (w3m-region (point-min) (point-max))
	   (erase-buffer)
	   (if (and (setq form (car w3m-current-forms))
		    (setq xurl (aref form 2))
		    (setq post-data (w3m-form-make-form-data form)))
	       (prog2
		   (w3m-message "Redirect to %s..." xurl)
		   (setq attr (w3m-process-with-wait-handler
				(w3m-w3m-retrieve-1 xurl post-data (nth 6 attr)
						    t counter handler)))
		 (setq w3m-onload-url (ignore-errors (nth 6 attr))))
	     (insert decoded)
	     nil)))))

(defun w3m-about-retrieve (url &optional no-uncompress no-cache
			       post-data referer handler)
  "Retrieve the about: page which is pointed to by URL.
It will put the retrieved contents into the current buffer.  See
`w3m-retrieve' for how does it work asynchronously with the arguments."
  (cond
   ((string= "about://emacs-w3m.gif" url)
    (let ((icon (base64-decode-string w3m-emacs-w3m-icon)))
      (set-buffer-multibyte (multibyte-string-p icon))
      (insert icon)
      (set-buffer-multibyte nil))
    "image/gif")
   ((string-match "\\`about://source/" url)
    (setq url (substring url (match-end 0)))
    (w3m-process-do
	(type (w3m-retrieve
	       url no-uncompress no-cache post-data referer handler))
      (cond
       (type "text/plain")
       ((w3m-cache-request-contents url)
	(w3m-decode-encoded-contents (w3m-content-encoding url))
	"text/plain")
       (t nil))))
   ((string-match "\\`about:/*blank/?\\'" url)
    "text/plain")
   (t
    (let ((output-buffer (current-buffer)))
      (w3m-process-do-with-temp-buffer
	  (type (let (func)
		  (setq w3m-current-url url)
		  (set-buffer-multibyte t)
		  (if (and (string-match "\\`about://\\([^/]+\\)/" url)
			   (setq func
				 (intern-soft (concat "w3m-about-"
						      (match-string 1 url))))
			   (fboundp func))
		      (funcall func url no-uncompress no-cache
			       post-data referer handler)
		    (w3m-about url no-uncompress no-cache))))
	(when type
	  (when (string-match "\\`text/" type)
	    (encode-coding-region (point-min) (point-max) w3m-coding-system))
	  (set-buffer-multibyte nil)
	  (when (buffer-name output-buffer)
	    (let ((temp-buffer (current-buffer)))
	      (with-current-buffer output-buffer
		(insert-buffer-substring temp-buffer))))
	  type))))))

(defun w3m-cid-retrieve (url &optional no-uncompress no-cache)
  "Retrieve contents pointed to by URL prefixed with the cid: scheme.
This function is mainly used when displaying text/html MIME parts in
message user agents, e.g., Gnus, Mew, T-gnus, Wanderlust, and possibly
VM.

It calls a function according to the `major-mode' of the buffer
specified by `w3m-current-buffer'.  Functions to be called are
specified by the `w3m-cid-retrieve-function-alist' variable.

Each function in that variable should take three arguments passed
through this function, extract and insert contents specified by URL
(which can be found in the raw message itself) into the current buffer,
and return the content type of the data.

The optional two arguments can be omitted by functions; NO-UNCOMPRESS
specifies whether functions should not uncompress extracted contents;
NO-CACHE specifies whether functions should not use cached contents."
  (let ((func (cdr (assq (with-current-buffer w3m-current-buffer major-mode)
			 w3m-cid-retrieve-function-alist))))
    (when func (funcall func url no-uncompress no-cache))))

(defun w3m-data-retrieve (url &optional _no-uncompress _no-cache)
  "Retrieve contents pointed to by URL prefixed with the data: scheme.
See RFC2397."
  (let ((case-fold-search t) (mime-type "text/plain")
	(coding nil) (encode nil) (param "")
	data-string)
    (when (string-match
	   "data:\\(\\([^/;,]+\\(/[^;,]+\\)?\\)\\(;[^;,]+\\)*\\)?,\\(.*\\)"
	   url)
      (setq  mime-type (or (match-string-no-properties 2 url)
			   mime-type)
	     param (or (match-string-no-properties 4 url)
		       param)
	     data-string (match-string-no-properties 5 url))
      (when (string-match "\\`.*\\(;[ \t]*base64\\)\\'" param)
	(setq param (substring param 0 (match-beginning 1)))
	(setq encode 'base64))
      (when (string-match "charset=\\([^;]+\\)" param)
	(setq coding (w3m-charset-to-coding-system
		      (match-string-no-properties 1 param))))
      (when data-string
	(erase-buffer)
	(let ((decoded (cond ((eq encode 'base64)
			      (base64-decode-string
			       (w3m-url-decode-string data-string 'us-ascii)))
			     (t
			      (w3m-url-decode-string data-string coding)))))
	  (set-buffer-multibyte (multibyte-string-p decoded))
	  (insert decoded)
	  (set-buffer-multibyte nil))))
    mime-type))

;;;###autoload
(defun w3m-retrieve (url &optional no-uncompress no-cache
			 post-data referer handler)
  "Retrieve web contents pointed to by URL.
It will put the retrieved contents into the current buffer.

If HANDLER is nil, this function will retrieve web contents, return
the content type of the retrieved data, and then come to an end.  This
behavior is what is called a synchronous operation.  You have to
specify HANDLER in order to make this function show its real ability,
which is called an asynchronous operation.

If HANDLER is a function, this function will come to an end in no time.
In this case, contents will be retrieved by the asynchronous process
after a while.  And after finishing retrieving contents successfully,
HANDLER will be called on the buffer where this function starts.  The
content type of the retrieved data will be passed to HANDLER as a
string argument.

NO-UNCOMPRESS specifies whether this function should not uncompress contents.
NO-CACHE specifies whether this function should not use cached contents.
POST-DATA and REFERER will be sent to the web server with a request."
  (set (make-local-variable 'w3m-http-status) nil)
  (if (not handler)
      (condition-case nil
	  (w3m-process-with-wait-handler
	    (w3m-retrieve url
			  no-uncompress no-cache post-data referer handler))
	(w3m-process-timeout nil))
    (unless (and w3m-safe-url-regexp
		 (not (string-match w3m-safe-url-regexp url)))
      (setq url (w3m-url-strip-fragment url))
      (set-buffer-multibyte nil)
      (cond
       ((string-match "\\`about:" url)
	(w3m-about-retrieve url
			    no-uncompress no-cache post-data referer handler))
       ((string-match "\\`cid:" url)
	(w3m-cid-retrieve url no-uncompress no-cache))
       ((string-match "\\`data:" url)
	(w3m-data-retrieve url no-uncompress no-cache))
       ((w3m-url-local-p url)
	(w3m-local-retrieve url no-uncompress))
       (t
	(w3m-w3m-retrieve
	 url no-uncompress no-cache post-data referer handler))))))

;;;###autoload
(defun w3m-download (&optional url filename _no-cache handler _post-data)
  "Download contents of URL to a file named FILENAME.
NO-CACHE is ignored (always download)."
  (interactive)
  (unless url
    (while (string-equal
	    (setq url (w3m-canonicalize-url
		       (w3m-input-url
			"Download URL: " nil
			(or (w3m-active-region-or-url-at-point) "")
			nil nil 'no-initial)))
	    "")
      (message "A url is required")
      (sit-for 1)))
  (if filename
      (when (file-exists-p filename)
	(if (file-directory-p filename)
	    (error "File(%s) is a directory" filename)
	  (delete-file filename)))
    (let ((basename (file-name-nondirectory (w3m-url-strip-query url)))
	  ;; Make the M-p command offer an original url string.
	  (file-name-history
	   (cons
	    (abbreviate-file-name
	     (expand-file-name
	      (if (string-match "\\`[^\n\t :]+:/+" url)
		  (substring url (match-end 0))
		url)
	      (condition-case nil
		  (eval (car (get 'w3m-default-save-directory 'standard-value))
			t)
		(error w3m-default-save-directory))))
	    file-name-history))
	  dir)
      (when (string-match "\\`[\t ]*\\'" basename)
	(when (string-match "\\`[\t ]*\\'"
			    (setq basename (file-name-nondirectory url)))
	  (setq basename "index.html")))
      (while (not filename)
	(setq filename (w3m-read-file-name
			(format "Download %s to: " url)
			w3m-default-save-directory basename)
	      dir (directory-file-name (file-name-directory filename)))
	(cond ((file-exists-p filename)
	       (if (file-directory-p filename)
		   (message "File(%s) is a directory" (prog1 filename
							(sit-for 1)
							(setq filename nil)))
		 (if (y-or-n-p (format "File(%s) already exists. Overwrite? "
				       filename))
		     (delete-file filename)
		   (setq filename nil))))
	      ((file-exists-p dir)
	       (unless (file-directory-p dir)
		 (if (y-or-n-p (format "File(%s) is not a directory. Delete? "
				       dir))
		     (delete-file dir)
		   (setq filename nil)))))
	(message nil))
      (unless (file-exists-p dir)
	(make-directory dir t))))
  (if (and w3m-use-ange-ftp (string-match "\\`ftp://" url))
      (w3m-goto-ftp-url url filename)
    (let ((args (concat (mapconcat
			 #'(lambda (x) (replace-regexp-in-string
					"\\([\t ;]\\)" "\\\\\\1" x))
			 `(,w3m-command
			   ,@w3m-command-arguments
			   ,@(w3m-w3m-expand-arguments
			      w3m-dump-head-source-command-arguments)
			   ,url)
			 " ")
			;; awk should be GNU awk that supports BINMODE and RT.
			;; END stuff makes it sure to download an empty file.
			"| awk -v BINMODE=3 'BEGIN{Body=0; Line=\"\"}"
			"(Body==0)&&(Line!=$0){Line=$0; print $0}"
			"(Body==1){printf \"%s%s\",$0,RT>\"" filename "\"}"
			"/^$/{Body=1} END{printf \"\">>\"" filename "\"}'"))
	  (page-buffer (current-buffer))
	  temp process header status reason)
      (w3m-process-do-with-temp-buffer
	  (success ;; t if success
	   (let ((w3m-current-buffer page-buffer)
		 (progress (cons (setq temp (buffer-name)) "-")))
	     (prog1
		 (setq process (w3m-process-start handler shell-file-name
						  (list "-c" args)))
	       (with-current-buffer page-buffer
		 (push process w3m-current-process)
		 (setq w3m-process-modeline-string
		       (nconc w3m-process-modeline-string
			      (list progress)))))))
	(if (and success
		 (file-exists-p filename)
		 (progn
		   (goto-char (point-min))
		   (re-search-forward (concat "^W3m-current-url: "
					      (regexp-quote url) "$")
				      nil t))
		 (progn
		   (setq header (buffer-substring (point) (point-max))
			 status (car (w3m-w3m-parse-header url header)))
		   (and (numberp status) (>= status 200) (< status 300))))
	    (let* ((case-fold-search t)
		   (decoder (when (string-match
				   "^content-encoding:[\t\n ]*\\([^\t\n ]+\\)"
				   header)
			      (downcase (match-string 1 header))))
		   tempname)
	      (w3m-cache-header url header t)
	      (and decoder
		   (setq decoder (cdr (assoc decoder w3m-encoding-alist)))
		   (setq decoder (cdr (assq decoder w3m-decoder-alist)))
		   (setq tempname (concat filename (make-temp-name ".")))
		   (if (zerop (call-process
			       shell-file-name nil nil nil
			       "-c"
			       (concat "cat \"" filename "\"|\""
				       (car decoder) "\" \""
				       (mapconcat #'identity
						  (cadr decoder) "\" \"")
				       "\">\"" tempname "\"")))
		       (rename-file tempname filename t)
		     (when (file-exists-p tempname)
		       (delete-file tempname))))
	      (set-file-times filename (w3m-last-modified url))
	      (with-current-buffer page-buffer
		(let ((w3m-verbose t))
		  (w3m-message "File(%s) has been downloaded" filename))))
	  (when (file-exists-p filename) (delete-file filename))
	  (setq reason (if (numberp status)
			   (and (let ((case-fold-search t))
				  (string-match
				   "^http[^\t\n\r ]*[\t ]*\\(.+\\)$"
				   header))
				(concat ": " (match-string 1 header)))
			 (goto-char (point-max))
			 (skip-chars-backward "\t\n ")
			 (unless (bobp)
			   (concat ":\n"
				   (buffer-substring (point-min) (point))))))
	  (with-current-buffer page-buffer
	    (let ((w3m-verbose t))
	      (w3m-message "File(%s) downloading failed%s"
			   filename (or reason "")))))
	(with-current-buffer page-buffer
	  (setq w3m-process-modeline-string
		(delq (assoc temp w3m-process-modeline-string)
		      w3m-process-modeline-string)
		w3m-current-process (delq process w3m-current-process)))
	success))))

;;; Retrieve data:
(defun w3m-remove-comments ()
  "Remove HTML comments in the current buffer."
  (goto-char (point-min))
  (let (beg)
    (while (search-forward "<!--" nil t)
      (setq beg (match-beginning 0))
      (if (search-forward "-->" nil t)
	  (delete-region beg (point))))))

(defun w3m-remove-invisible-image-alt ()
  "Remove alt=\"whitespace\" attributes in img tags.
Such attributes not only obscure them but also might make images not
be displayed especially in shimbun articles."
  (goto-char (point-min))
  (let ((case-fold-search t)
	start end)
    (while (and (re-search-forward "\\(<img\\)[\t\n\f\r ]+" nil t)
		(progn
		  (setq start (match-end 1))
		  (search-forward ">" nil t))
		(progn
		  (setq end (match-beginning 0))
		  (goto-char start)
		  (re-search-forward "[\t\n\f\r ]+alt=\"[\t\n\f\r ]*\""
				     end t)))
      (delete-region (match-beginning 0) (match-end 0)))))

(defun w3m-check-header-tags ()
  "Process header tags (<LINK>,<BASE>) in the current buffer."
  (let ((case-fold-search t)
	tag)
    (goto-char (point-min))
    (when (re-search-forward "</head\\(?:[ \t\r\f\n][^>]*\\)?>" nil t)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward "<\\(link\\|base\\)[ \t\r\f\n]+" nil t)
	  (setq tag (downcase (match-string 1)))
	  (cond
	   ((string= tag "link")
	    (w3m-parse-attributes ((rel :case-ignore) href type)
	      (when rel
		(setq rel (split-string rel))
		(cond
		 ((member "icon" rel) (setq w3m-icon-data (cons href type)))
		 ((member "next" rel) (setq w3m-next-url href))
		 ((or (member "prev" rel) (member "previous" rel))
		  (setq w3m-previous-url href))
		 ((member "start" rel) (setq w3m-start-url href))
		 ((member "contents" rel) (setq w3m-contents-url href))))))
	   ;; <base> ought to be absolute but if not then absolutize for
	   ;; w3m-current-base-url.  Helps bad <base href="/foo/bar/"> seen
	   ;; from from archive.org circa 2015.
	   ((string= tag "base")
	    (w3m-parse-attributes (href)
	      (when (< 0 (length href))
		(setq w3m-current-base-url (w3m-expand-url href)))))))))))

(defun w3m-check-refresh-attribute ()
  "Get REFRESH attribute in META tags."
  (setq w3m-current-refresh nil)
  (when w3m-use-refresh
    (let ((case-fold-search t)
	  (refurl w3m-current-url)
	  sec)
      (goto-char (point-min))
      (catch 'found
	(while (re-search-forward "<meta[ \t\r\f\n]+" nil t)
	  (w3m-parse-attributes ((http-equiv :case-ignore) content)
	    (when (string= http-equiv "refresh")
	      (cond
	       ((string-match "\\`[0-9]+\\'" content)
		(setq sec (match-string-no-properties 0 content)))
	       ((string-match
		 "\\([^;]+\\);[ \t\n]*url=[\"']?\\([^\"']+\\)"
		 content)
		(setq sec (match-string-no-properties 1 content))
		(setq refurl (w3m-decode-entities-string
			      (match-string-no-properties 2 content)))
		(when (string-match "\\`[\"']\\(.*\\)[\"']\\'" refurl)
		  (setq refurl (match-string 1 refurl)))))
	      (when (and sec (string-match "\\`[0-9]+\\'" sec))
		(throw 'found
		       (setq w3m-current-refresh
			     (cons (string-to-number sec)
				   (w3m-expand-url refurl))))))))))))

(defun w3m-remove-meta-charset-tags ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (catch 'found
      (when (re-search-forward "<meta[ \t\r\f\n]+" nil t)
	(let ((start (match-beginning 0)))
	  (w3m-parse-attributes ((http-equiv :case-ignore)
				 (content :case-ignore))
	    (when (and (string= http-equiv "content-type")
		       content
		       (string-match ";[ \t\n]*charset=" content))
	      (delete-region start (point))
	      (throw 'found nil))))))))

(defun w3m-fix-illegal-blocks ()
  "Replace <div>...</div> within <a>...</a> with <span>...<br></span>.
<div> is a block element that should not appear within inline elements
like <a>, however some web sites, e.g., news.google.com.tw, do so and
w3m regards it as an incomplete <a> tag that is not closed."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "<a[\t\n ]" nil t)
      (save-restriction
	(narrow-to-region (match-beginning 0)
			  (or (w3m-end-of-tag "a") (point-max)))
	(goto-char (point-min))
	(while (re-search-forward "<div[\t\n >]" nil t)
	  (when (w3m-end-of-tag "div")
	    (replace-match (concat "<span" (substring (buffer-substring
						       (match-beginning 0)
						       (match-end 1)) 4)
				   "<br></span>")
			   t t)
	    (goto-char (match-beginning 0))))
	(goto-char (point-max))))))

(defun w3m-rendering-extract-title ()
  "Extract the title from the halfdump and put it into the current buffer."
  (goto-char (point-min))
  (or (when (re-search-forward "<title_alt[ \t\n]+title=\"\\([^\"]+\\)\">"
			       nil t)
	(prog1 (w3m-decode-entities-string
		(mapconcat 'identity
			   (save-match-data (split-string (match-string 1)))
			   " "))
	  (delete-region (match-beginning 0) (match-end 0))))
      (when (and (stringp w3m-current-url)
		 (string-match "/\\([^/]+\\)/?\\'" w3m-current-url))
	(match-string 1 w3m-current-url))
      "<no-title>"))

(defun w3m-set-display-ins-del ()
  (when (eq w3m-display-ins-del 'auto)
    (with-temp-buffer
      (let* ((coding-system-for-read w3m-output-coding-system)
	     (coding-system-for-write (if (eq 'binary w3m-input-coding-system)
					  w3m-current-coding-system
					w3m-input-coding-system))
	     (default-process-coding-system
	       (cons coding-system-for-read coding-system-for-write))
	     (env (copy-sequence w3m-command-environment))
	     type)
	(setq w3m-display-ins-del nil)
	(w3m-process-with-environment (cons '("LANG" . "C")
					    (delq (assoc "LANG" env) env))
	  (call-process (or w3m-halfdump-command w3m-command) nil t nil "-o")
	  (goto-char (point-min))
	  (when (re-search-forward "display_ins_del=<\\([^>]+\\)>" nil t)
	    (setq type (match-string 1))
	    (cond
	     ((string= type "number")
	      (setq w3m-display-ins-del 'fontify))
	     ((string= type "bool")
	      (setq w3m-display-ins-del 'tag)))))))))

(defun w3m-rendering-half-dump (charset)
  "Run w3m -halfdump on buffer's contents.
CHARSET is used to substitute the `charset' symbols specified in
`w3m-halfdump-command-arguments' with its value."
  (ignore charset)
  (w3m-set-display-ins-del)
  (let* ((coding-system-for-read w3m-output-coding-system)
	 (coding-system-for-write (if (eq 'binary w3m-input-coding-system)
				      w3m-current-coding-system
				    w3m-input-coding-system))
	 (default-process-coding-system
	   (cons coding-system-for-read coding-system-for-write)))
    (w3m-process-with-environment w3m-command-environment
      (apply 'call-process-region
	     (point-min)
	     (point-max)
	     (or w3m-halfdump-command w3m-command)
	     t t nil
	     (w3m-w3m-expand-arguments
	      (append w3m-halfdump-command-arguments
		      w3m-halfdump-command-common-arguments
		      ;; Image size conscious rendering
		      (when (member "image" w3m-compile-options)
			(if (and w3m-treat-image-size
				 (or (display-images-p)
				     (and w3m-pixels-per-line
					  w3m-pixels-per-character)))
			    (list "-o" "display_image=on"
				  "-ppl" (number-to-string
					  (or w3m-pixels-per-line
					      (frame-char-height)))
				  "-ppc" (number-to-string
					  (or w3m-pixels-per-character
					      (frame-char-width))))
			  (list "-o" "display_image=off"))))
	      charset)))))

(defun w3m-markup-urls-nobreak ()
  "Make things that look like urls unbreakable.
This function prevents non-link long urls from being broken (w3m tries
to fold them).  Things in textarea won't be modified."
  (let ((case-fold-search t)
	(beg (point-min))
	(regexp
	 (eval-when-compile
	   ;; A copy of `gnus-button-url-regexp'.
	   (concat
	    "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
	    "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
	    "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
	    (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
		  (punct "!?:;.,"))
	      (concat
	       "\\(?:"
	       ;; Match paired parentheses, e.g. in Wikipedia URLs, cf.
	       ;; <https://lists.gnu.org/archive/html/bug-gnu-emacs/2013-07/msg00890.html>
	       "[" chars punct "]+" "(" "[" chars punct "]+" "[" chars "]*)"
	       "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
	       "\\|"
	       "[" chars punct "]+" "[" chars "]"
	       "\\)"))
	    "\\)")))
	(nd (make-marker))
	st)
    (goto-char beg)
    (while beg
      (save-restriction
	(narrow-to-region
	 beg
	 (if (re-search-forward "[\t\n ]*\\(<textarea[\t\n ]\\)" nil t)
	     (prog1
		 (match-beginning 0)
	       (goto-char beg)
	       (setq beg (match-beginning 1)))
	   (point-max)))
	(while (re-search-forward regexp nil t)
	  (set-marker nd (match-end 0))
	  (setq st (goto-char (match-beginning 0)))
	  (if (and (re-search-backward "\\(<\\)\\|>" nil t)
		   (match-beginning 1))
	      (goto-char nd)
	    (goto-char st)
	    (skip-chars-backward "\t\f ")
	    (if (string-match "&lt;" (buffer-substring (max (- (point) 4)
							    (point-min))
						       (point)))
		(forward-char -4)
	      (goto-char st))
	    (insert "<nobr>")
	    (goto-char nd)
	    (when (looking-at "[\t\f ]*&gt;")
	      (goto-char (match-end 0)))
	    (insert "</nobr>")))
	(goto-char (point-max)))
      (setq beg (and (not (eobp))
		     (progn
		       (goto-char beg)
		       (w3m-end-of-tag "textarea")))))
    (set-marker nd nil)))

(defun w3m-rendering-buffer (&optional charset)
  "Do rendering of contents in the currenr buffer as HTML and return title."
  (w3m-message "Rendering...")
  (w3m-remove-comments)
  (w3m-remove-invisible-image-alt)
  (w3m-check-header-tags)
  (w3m-check-refresh-attribute)
  (unless (eq w3m-type 'w3m-m17n)
    (w3m-remove-meta-charset-tags))
  (w3m-fix-illegal-blocks)
  (w3m-markup-urls-nobreak)
  (w3m-rendering-half-dump charset)
  (w3m-message "Rendering...done")
  (w3m-rendering-extract-title))

(defun w3m--retrieve-and-render--handler-function (url silent page-buffer
						       arrival-time charset
						       type)
  (when w3m-onload-url
    (setq url w3m-onload-url
	  w3m-onload-url nil))
  (let ((w3m-message-silent silent))
    (when (buffer-live-p page-buffer)
      (setq url (w3m-url-strip-authinfo url))
      (if type
	  (if (string= type "X-w3m-error/redirection")
	      (when (w3m-show-redirection-error-information url page-buffer)
		(w3m-arrived-add url nil (current-time) (current-time))
		(w3m-message "Cannot retrieve URL: %s" url))
	    (let ((modified-time (w3m-last-modified url)))
	      (w3m-arrived-add url nil modified-time arrival-time)
	      (unless modified-time
		(setf (w3m-arrived-last-modified url) nil))
	      (let ((real (w3m-real-url url)))
		(unless (string= url real)
		  (w3m-arrived-add url nil nil arrival-time)
		  (setf (w3m-arrived-title real)
			(w3m-arrived-title url))
		  (setf (w3m-arrived-last-modified real)
			(w3m-arrived-last-modified url))
		  (setq url real)))
	      (prog1 (w3m-create-page
		      url
		      (or (w3m-arrived-content-type url)
			  type)
		      (or charset
			  (w3m-arrived-content-charset url)
			  (w3m-content-charset url))
		      page-buffer)
		(w3m-force-window-update-later page-buffer 1e-9)
		(unless (or w3m-message-silent (get-buffer-window page-buffer))
		  (w3m-message "The content (%s) has been retrieved in %s"
			       url (buffer-name page-buffer))
		  ;; Make the message sure to be shown for a while.
		  (run-at-time
		   0.5 nil
		   (lambda (orig) (setq w3m-after-cursor-move-hook orig))
		   (prog1
		       w3m-after-cursor-move-hook
		     (setq w3m-after-cursor-move-hook nil)))))))
	(when (and w3m-clear-display-while-reading
		   (string-match "\\`file:" url))
	  (with-current-buffer page-buffer
	    (let ((inhibit-read-only t))
	      (when (ignore-errors (require 'zone))
		(sit-for 0.5)
		(zone-call 'zone-pgm-dissolve 1))
	      (erase-buffer)
	      (insert-char ?\n (/ (window-height) 2))
	      (insert-char ?  (max 0 (/ (- (window-width) (length url) 15) 2)))
	      (insert "Reading " url " ")
	      (put-text-property (point) (progn
					   (insert "failed!")
					   (point))
				 'face 'w3m-error)
	      (setq w3m-current-url url
		    w3m-current-title "Fail"))))
	(w3m-arrived-add url nil (current-time) (current-time))
	(ding)
	(when (eq (car w3m-current-forms) t)
	  (setq w3m-current-forms (cdr w3m-current-forms)))
	(prog1 (when (and w3m-show-error-information
			  (not (or (w3m-url-local-p url)
				   (string-match "\\`about:" url))))
		 (w3m-show-error-information url charset page-buffer))
	  (with-current-buffer page-buffer
	    (w3m-message
	     "Cannot retrieve URL: %s%s" url
	     (cond ((and w3m-process-exit-status
			 (not (equal w3m-process-exit-status 0)))
		    (format " (exit status: %s)" w3m-process-exit-status))
		   (w3m-http-status
		    (format " (http status: %s)" w3m-http-status))
		   (t "")))))))))

(defun w3m-retrieve-and-render (url &optional no-cache charset
				    post-data referer handler)
  "Retrieve contents of URL and render them in the current buffer.
It returns a `w3m-process' object and comes to an end immediately.
The HANDLER function will be called when rendering is complete.  When
a new content is retrieved in the buffer, the HANDLER function will be
called with t as an argument.  Otherwise, it will be called with nil."
  (when (and w3m-clear-display-while-reading
	     (get-buffer-window (current-buffer) 'visible)
	     (not (string-match "\\`about:" url)))
    ;; Clear the current display while reading a new page.
    (set-window-hscroll nil 0)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (w3m-display-progress-message url)))
  (unless (and w3m-current-ssl
	       w3m-confirm-leaving-secure-page
	       ;; Permit leaving safe pages without confirmation for
	       ;; several safe commands.  For more detail of
	       ;; definition of safe commands, see the thread
	       ;; beginning at [emacs-w3m:09767].
	       (not
		(or (memq this-command
			  '(w3m
			    w3m-goto-url w3m-redisplay-this-page
			    w3m-reload-this-page w3m-history
			    w3m-view-next-page w3m-view-previous-page
			    w3m-view-header w3m-view-source))
		    (string-match "\\`\\(?:ht\\|f\\)tps://" url)
		    (prog1
			(y-or-n-p "You are leaving secure page.  Continue? ")
		      (message nil)))))
    (setq url (w3m-url-strip-fragment url))
    (let ((page-buffer (current-buffer))
	  (arrival-time (current-time))
	  (silent w3m-message-silent))
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (w3m-clear-local-variables)
		  (w3m-retrieve url nil no-cache post-data referer handler)))
	(w3m--retrieve-and-render--handler-function
	 url silent page-buffer arrival-time charset type)))))

(defun w3m-show-error-information (url charset page-buffer)
  "Create and prepare the error information."
  (let ((case-fold-search t)
	(header (w3m-cache-request-header url))
	exit-status http-status errmsg)
    (with-current-buffer page-buffer
      (setq exit-status w3m-process-exit-status
	    http-status w3m-http-status))
    (setq errmsg
	  (concat
	   "<br><h1>Cannot retrieve URL: <a href=\""
	   url "\">" url "</a></h1>\n"
	   (cond
	    ((and exit-status (not (equal exit-status 0)))
	     (format "<br><br><h2>%s exits with the code %s</h2>\n"
		     w3m-command exit-status))
	    (http-status
	     (format "<br><br><h2>%s %s</h2>\n"
		     http-status
		     (or (cdr (assq http-status w3m-http-status-alist))
			 ""))))))
    (if (or (null header)
	    (string-match "\\`w3m: Can't load " header))
	(progn
	  (insert errmsg)
	  (unless http-status
	    (insert "\
<br><br>Something seems to be wrong with URL or this system.\n")
	    (when (string-match "\\`news:" url)
	      (insert "\
<br>Also verify the value of the <b>NNTPSERVER</b> environment variable\
 that should be the address of the <b>NNTP</b> server.\n")))
	  (save-restriction
	    (narrow-to-region (point) (point))
	    (when (w3m-cache-request-contents url)
	      (w3m-decode-encoded-contents
	       (setq charset (w3m-content-encoding url)))
	      (goto-char (point-min))
	      (insert "<br><br><hr><br>\n<h2>Contents</h2><br>\n"))))
      (goto-char (point-min))
      (or (search-forward "<body>" nil t)
	  (search-forward "<html>" nil t))
      (unless (bolp) (insert "\n"))
      (insert errmsg)
      (save-restriction
	(narrow-to-region (point) (point))
	(when (w3m-cache-request-contents url)
	  (w3m-decode-encoded-contents
	   (setq charset (w3m-content-encoding url)))
	  (goto-char (point-min))
	  (insert "<br><br><hr><br>\n")
	  (goto-char (point-max))))
      (unless (eobp) (insert "<br><br><hr><br>\n"))
      (when header
	(or (search-forward "</body>" nil t)
	    (search-forward "</html>" nil 'max))
	(unless (bolp) (insert "\n"))
	(insert "<br><br><hr><br><br><h2>Header information</h2><br>\n<pre>"
		header "</pre>\n"))))
  (w3m-create-page url "text/html" charset page-buffer)
  nil)

(defun w3m-show-redirection-error-information (url page-buffer)
  (erase-buffer)
  (insert
   (format "\n<br><h1>Cannot retrieve URL: %s</h1><br><br>%s"
	   (format "<a href=\"%s\">%s</a>" url url) "\
The number of redirections has exceeded a limit.  This may have<br>\n
happened due to the server side miss-configuration.  Otherwise,<br>\n
try increasing the limit, the value of <b>`w3m-follow-redirection'</b>.<br>\n"))
  (w3m-create-page url "text/html" "us-ascii" page-buffer))

(defun w3m-prepare-content (url type charset)
  "Prepare contents in the current buffer according to TYPE.
URL is assumed to be a place where the contents come from.  CHARSET is
passed to the filter function  corresponding to TYPE if it is
specified in the `w3m-content-type-alist' variable."
  (let ((filter (nth 3 (assoc type w3m-content-type-alist))))
    (cond
     ;; Filter function is specified.
     ((functionp filter) (funcall filter url type charset))
     ;; Equivalent type is specified.
     ((stringp filter) filter)
     ;; No filter is specified.
     ((not filter) type)
     ;; Failed.
     (t ""))))

(defun w3m-prepare-markdown-content (url type charset)
  (if w3m-markdown-converter
      (let ((coding-system-for-read 'utf-8)
	    (coding-system-for-write 'utf-8))
	(w3m-decode-buffer url charset type)
	(apply (function call-process-region)
	       (point-min)
	       (point-max)
	       (car w3m-markdown-converter)
	       t '(t nil) nil
	       (cdr w3m-markdown-converter))
	"text/html")
    "text/plain"))

(defun w3m-detect-xml-type (url type charset)
  "Check if the type of xml contents of URL is xhtml+xml.
If so return \"text/html\", otherwise \"text/plain\"."
  (with-temp-buffer
    (w3m-retrieve url)
    (w3m-decode-buffer url charset type)
    (goto-char (point-min))
    (setq case-fold-search t)
    (if (re-search-forward
	 "<[\t\n ]*html\\(?:\\(?:[\t\n ]+[^>]+\\)?>\\|[\t\n ]*>\\)"
	 nil t)
	"text/html"
      "text/plain")))

(defun w3m-create-text-page (url type charset page-buffer)
  (w3m-safe-decode-buffer url charset type)
  (setq w3m-current-url (if (w3m-arrived-p url)
			    (w3m-real-url url)
			  url)
	w3m-current-title
	(if (string= "text/html" type)
	    (let ((title (w3m-rendering-buffer charset)))
	      (setf (w3m-arrived-title url) title)
	      title)
	  (or (when (string-match "\\`about://\\(?:source\\|header\\)/" url)
		(w3m-arrived-title (substring url (match-end 0))))
	      (file-name-nondirectory (if (string-match "/\\'" url)
					  (directory-file-name url)
					url)))))
  (let ((result-buffer (current-buffer)))
    (with-current-buffer page-buffer
      (let ((inhibit-read-only t))
	(widen)
	(delete-region (point-min) (point-max))
	(insert-buffer-substring result-buffer)
	(goto-char (point-min))
	(w3m-copy-local-variables result-buffer)
	(set-buffer-file-coding-system w3m-current-coding-system)
	(when (string= "text/html" type) (w3m-fontify))
	'text-page))))

(defsubst w3m-image-page-displayed-p ()
  (and w3m-current-url
       (string-match "\\`image/" (w3m-content-type w3m-current-url))
       (eq (get-text-property (point-min) 'w3m-image-status) 'on)))

(defun w3m-create-image-page (url type _charset page-buffer)
  (when (w3m-image-type type)
    (with-current-buffer page-buffer
      (let ((inhibit-read-only t))
	(w3m-clear-local-variables)
	(setq w3m-current-url (w3m-real-url url)
	      w3m-current-title (file-name-nondirectory url))
	(widen)
	(delete-region (point-min) (point-max))
	(insert w3m-current-title)
	(w3m-add-face-property (point-min) (point-max) 'w3m-image)
	(w3m-add-text-properties (point-min) (point-max)
				 (list 'w3m-image url
				       'mouse-face 'highlight))
	(image-mode-setup-winprops)
	'image-page))))

(defun w3m--unsupported-display (page-buffer url type)
  "Internal function for `w3m-create-page'.
When the display does not support handling the mime-type, inform
the user and attempt to use an external program or just download
the url."
  (with-current-buffer page-buffer
    (setq w3m-current-url (if (w3m-arrived-p url)
			      (w3m-real-url url)
			    url)
	  w3m-current-title (file-name-nondirectory w3m-current-url))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "This display does not support %s:\n<%s>
Attempting external view or download..."
		      (if (string-match "\\`image/" type)
			  "image" type)
		      url))
      (center-region (point-min) (point))
      (goto-char (point-min))
      (insert-char ?\n (/ (- (window-height) 3) 2)))
    (goto-char (point-min))
    (w3m-external-view url)
    (w3m-process-stop page-buffer)
    (w3m-view-previous-page)
    'external-view))

(defun w3m--prompt-for-unknown-content-type (url type page-buffer)
  "Internal function for `w3m-create-page'.
Display raw contents, prompt the user for the mime-type to use, and
update buffer's local variables accordingly.  Return a cons of content
type and urls, where urls is a cons of a url to have been restored and
a url to download or external-view."
  (let ((cur (current-buffer))
	(mb enable-multibyte-characters)
	dots cont restore quit ourl)
    (with-temp-buffer
      (rename-buffer " *Raw Contents*" t)
      (set-buffer-multibyte mb)
      (save-window-excursion
	(pop-to-buffer (current-buffer))
	(delete-other-windows)
	(ding)
	;; Display the raw contents briefly.
	(sit-for 0)
	(setq truncate-lines t
	      dots (make-string (/ (- (window-width) 6) 2) ?.))
	(with-current-buffer cur
	  (cond ((< (count-lines (point-min) (point-max)) (window-height))
		 (setq cont (buffer-string)))
		((< (window-height) 10)
		 (goto-char (point-min))
		 (forward-line (max 0 (- (window-height) 2)))
		 (setq cont
		       (concat
			(buffer-substring (point-min) (point))
			"\n[" dots "snip" dots "]")))
		(t
		 (goto-char (point-min))
		 (forward-line (/ (- (window-height) 4) 2))
		 (setq cont (concat (buffer-substring (point-min) (point))
				    "\n[" dots "snip" dots "]\n\n"))
		 (goto-char (point-max))
		 (forward-line (/ (- 4 (window-height)) 2))
		 (setq cont (concat
			     cont
			     (buffer-substring (point) (point-max)))))))
	(insert cont)
	(goto-char (point-min))
	(condition-case nil
	    (let ((inhibit-quit nil))
	      (setq type (completing-read
			  (format "\
Content type for %s (%sjust type <RET> to download or external-view): "
				  (file-name-nondirectory url)
				  (if (zerop (length type)) ""
				    (concat "\"" type "\" is unknown; ")))
			  w3m-content-type-alist nil t))
	      (if (zerop (length type))
		  (setq restore t)
		(setf (w3m-arrived-content-type url) type)))
	  (quit ;; The user forced terminating the session with C-g.
	   (setq quit t)))))
    (when (or restore quit)
      (w3m-process-stop page-buffer)
      (with-current-buffer page-buffer
	(setq w3m-current-process nil)
	(cond (w3m-current-url
	       (w3m-redisplay-this-page nil t))
	      ((caar (w3m-history-backward))
	       (w3m-view-previous-page nil t))
	      (quit (w3m-delete-buffer)))
	(setq ourl w3m-current-url)))
    (if quit
	'quit
      (cons type (and ourl (cons ourl url))))))

(defun w3m-create-page (url type charset page-buffer)
  "Select a renderer or other handler for URL.
Choice is based upon content-type or mime-type TYPE."
  (let (download-url)
    ;; Select a content type.
    (when (or
	   (and (stringp type)
		(or (assoc type w3m-content-type-alist)
		    (w3m-image-type type)))
	   (let ((tem (w3m--prompt-for-unknown-content-type
		       url type page-buffer)))
	     (if (eq tem 'quit) ;; Terminated by C-g.
		 nil
	       (setq type (car tem))
	       ;; If the type is still unknown, i.e. a user gave up deciding,
	       ;; the above function w3m--prompt-* returns a url to have been
	       ;; re-visited to, and a url to download or external-view.
	       (when (consp (cdr tem))
		 (setq url (cadr tem)
		       download-url (cddr tem)))
	       t)))
      ;; Reset decoding status of this buffer.
      (setq w3m-current-coding-system nil)
      (setq type (w3m-prepare-content url type charset))
      (w3m-safe-decode-buffer url charset type)
      (setq charset (or charset w3m-current-content-charset))
      (when w3m-use-filter (w3m-filter url))
      (w3m-relationship-estimate url)
      ;; Create pages.
      (cond
       ((string-match "\\`text/" type)
	(w3m-create-text-page url type charset page-buffer))
       ((string-match "\\`image/" type)
	(if (display-images-p)
	    (w3m-create-image-page url type charset page-buffer)
	  (w3m--unsupported-display page-buffer url type)))
       ((member type w3m-doc-view-content-types)
	(if (not (display-images-p))
	    (w3m--unsupported-display page-buffer url type)
	  (with-current-buffer page-buffer
	    (setq w3m-current-url (if (w3m-arrived-p url)
				      (w3m-real-url url)
				    url)))
	  (w3m-doc-view url)))
       (t
	(with-current-buffer page-buffer
	  (setq w3m-current-url (if (w3m-arrived-p url)
				    (w3m-real-url url)
				  url)
		w3m-current-title (or (w3m-arrived-title w3m-current-url)
				      (file-name-nondirectory w3m-current-url)))
	  (w3m-external-view (or download-url url))
	  'external-view))))))

(defun w3m-relationship-estimate (url)
  "Estimate relationships between a page and others."
  (save-excursion
    (save-match-data
      (catch 'estimated
	(dolist (rule w3m-relationship-estimate-rules)
	  (when (apply (car rule) url (cdr rule))
	    (throw 'estimated t)))))))

(defun w3m-relationship-simple-estimate (url regexp &optional next previous
					     start contents)
  "Search relationships with given patterns
when the URL of the retrieved page matches the REGEXP."
  (when (string-match regexp url)
    (w3m-relationship-search-patterns url next previous start contents)))

(defun w3m-relationship-magicpoint-estimate (url)
  "Search relationships for pages generated by MagicPoint."
  (goto-char (point-max))
  (when (search-backward
	 "Generated by <A HREF=\"http://www.mew.org/mgp/\">MagicPoint</A>"
	 nil t)
    (goto-char (point-min))
    (w3m-relationship-search-patterns
     url
     (eval-when-compile
       (concat "<A HREF=" w3m-html-string-regexp ">\\[next&gt;\\]</A>"))
     (eval-when-compile
       (concat "<A HREF=" w3m-html-string-regexp ">\\[&lt;prev\\]</A>"))
     (eval-when-compile
       (concat "<A HREF=" w3m-html-string-regexp ">\\[&lt;&lt;start\\]</A>"))
     (eval-when-compile
       (concat "<A HREF=" w3m-html-string-regexp ">\\[index\\]</A>")))))

(defun w3m-relationship-oddmuse-estimate (url)
  (when (string-match "/wiki\\?search=.*" url)
    (goto-char (point-min))
    (and (re-search-forward "href=\"\\([^\"]+\\)\">Previous</a>" nil t)
	 (setq w3m-previous-url (match-string 1)))
    (and (re-search-forward "href=\"\\([^\"]+\\)\">Next</a>" nil t)
	 (setq w3m-next-url (match-string 1)))))

(defun w3m-relationship-slashdot-estimate (url)
  (goto-char (point-min))
  (when (and (string-match
	      "slashdot\\.org/\\(article\\|comments\\)\\.pl\\?"
	      url)
	     (search-forward "<div class=\"linkCommentPage\">" nil t))
    (let ((min (point)) (max (save-excursion (search-forward "</div>" nil t))))
      ;; move to the position of the current page indicator and then search
      ;; for the next and previous link within the current <div>
      (when (and max (re-search-forward "<b>\\(([0-9]+)\\)</b>" max t))
	(let ((re (concat "<a href=" w3m-html-string-regexp ">")))
	  (when (save-excursion (re-search-backward re min t))
	    (setq w3m-previous-url
		  (w3m-expand-url (w3m-decode-anchor-string
				   (or (match-string 2)
				       (match-string 3)
				       (match-string 1))))))
	  (when (re-search-forward re max t)
	    (setq w3m-next-url
		  (w3m-expand-url (w3m-decode-anchor-string
				   (or (match-string 2)
				       (match-string 3)
				       (match-string 1)))))))))))

(defun w3m-relationship-alc-estimate (url)
  ;; use filter
  (when (string-match "\\`http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8/" url)
    (when (re-search-forward
	   (concat "<a href=\\\"http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8/"
		   "\\(\\?pg=[0-9]+\\)\\\">前へ</a>")
	   nil t)
      (setq w3m-previous-url
	    (w3m-expand-url (match-string 1) url)))
    (when (re-search-forward
	   (concat "<a href=\\\"http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8/"
		   "\\(\\?pg=[0-9]+\\)\\\">次へ</a>")
	   nil t)
      (setq w3m-next-url
	    (w3m-expand-url (match-string 1) url)))
    (unless (or w3m-previous-url w3m-next-url)
      ;; no use filter
      (goto-char (point-min))
      (when (re-search-forward
	     "<a href='javascript:goPage(\"\\([0-9+]\\)\")'>前へ</a>"
	     nil t)
	(setq w3m-previous-url
	      (w3m-expand-url (format "?pg=%s" (match-string 1)) url)))
      (when (re-search-forward
	     "<a href='javascript:goPage(\"\\([0-9+]\\)\")'>次へ</a>"
	     nil t)
	(setq w3m-next-url
	      (w3m-expand-url (format "?pg=%s" (match-string 1)) url))))))

(defun w3m-relationship-search-patterns (url next previous
					     &optional start contents)
  "Search relationships with given patterns."
  (goto-char (point-min))
  (and next
       (re-search-forward next nil t)
       (setq w3m-next-url
	     (w3m-expand-url (w3m-decode-anchor-string (or (match-string 2)
							   (match-string 3)
							   (match-string 1)))
			     url))
       (goto-char (point-min)))
  (and previous
       (re-search-forward previous nil t)
       (setq w3m-previous-url
	     (w3m-expand-url (w3m-decode-anchor-string (or (match-string 2)
							   (match-string 3)
							   (match-string 1)))
			     url))
       (goto-char (point-min)))
  (and start
       (re-search-forward start nil t)
       (setq w3m-start-url
	     (w3m-expand-url (w3m-decode-anchor-string (or (match-string 2)
							   (match-string 3)
							   (match-string 1)))
			     url))
       (goto-char (point-min)))
  (and contents
       (re-search-forward contents nil t)
       (setq w3m-contents-url
	     (w3m-expand-url (w3m-decode-anchor-string (or (match-string 2)
							   (match-string 3)
							   (match-string 1)))
			     url))))

(defcustom w3m-anchor-list-filter-alist
  '(((".*" . "\\`https://[a-z]+\\.wikipedia\\.org/") . "\\`cite_.*[0-9]\\'")
    (".*" . "\\`mw-head\\'")
    (".*" . "\\`p-search\\'"))
  "Identification of \"junk\" anchor to be ignored in user searches.
The car of each element is a URL regexp, and the cdr is a regexp of
anchor names to prune.  If the car of each element is a cons cell,
its car is a regexp matching a URL to be filtered and its cdr is a
regexp matching a URL not to be filtered.  This feature was prompted
by the large number of useless anchors created by the very popular
media-wiki software used for sites such as wikipedia."
  :group 'w3m
  :type '(repeat
	  (cons
	   :format "%v" :indent 2
	   (group
	    :format "%v"
	    :match (lambda (_widget value)
		     (or (consp value) (stringp value)))
	    :value-to-internal (lambda (_widget value)
				 (if (consp value)
				     (if (consp (cdr value))
					 value
				       (list (car value) (cdr value)))
				   (list value)))
	    :value-to-external (lambda (_widget value)
				 (if (cdr value)
				     (apply #'cons value)
				   (car value)))
	    (regexp :format "Regexp matching url to be filtered: %v")
	    (checklist
	     :inline t
	     (regexp :format "Regexp matching url not to be filtered: %v")))
	   (regexp :format "Regexp matching anchor name: %v"))))

(defun w3m--filter-page-anchors (anchor-list)
  "Prune \"junk\" anchors from ANCHOR-LIST."
  (let (url-regexp)
    (dolist (filter w3m-anchor-list-filter-alist anchor-list)
      (when (if (consp (setq url-regexp (car filter)))
		(and (string-match (car url-regexp) w3m-current-url)
		     (not (string-match (cdr url-regexp) w3m-current-url)))
	      (string-match url-regexp w3m-current-url))
	(setq anchor-list
	      (cl-remove-if
	       (lambda (x)
		 (or (not (car x)) (string-match (cdr filter) (car x))))
	       anchor-list))))))

(defun w3m--get-page-anchors (&optional sub-sets sort-method)
  "Return list of page anchors, sorted by SORT-METHOD.
SUB-SETS defines from where to draw anchor information.  It defaults to
`all', but may also be `w3m-name-anchor' or `w3m-name-anchor2'.
SORT-METHOD defaults to `position', but may also be `name' or a function
that can be passed to `sort'."
  (let ((pos (point-min))
	anchor-list anchor2)
    ;; NOTE: w3m-name-anchor aggregates data from `w3m -half-dump'.
    (unless (eq sub-sets 'w3m-name-anchor2)
      (while (setq pos (next-single-property-change pos 'w3m-name-anchor))
	(push (cons (car (get-text-property pos 'w3m-name-anchor)) pos)
	      anchor-list))
      (setq pos (point-min)))
    (unless (eq sub-sets 'w3m-name-anchor)
      (while (setq pos (next-single-property-change pos 'w3m-name-anchor2))
	(setq anchor2 (car (get-text-property pos 'w3m-name-anchor2)))
	(unless (assoc anchor2 anchor-list)
	  (push (cons anchor2 pos) anchor-list))))
    (setq anchor-list (w3m--filter-page-anchors anchor-list))
    (sort anchor-list
	  (cond
	   ((or (not sort-method) (eq sort-method 'position))
	    (lambda (x y) (< (cdr x) (cdr y))))
	   ((eq sort-method 'name)
	    (lambda (x y) (string-lessp (downcase (car x)) (downcase (car y)))))
	   ((functionp sort-method) sort-method)
	   (t (error "Invalid arg: SORT-METHOD"))))))

(defun w3m-search-name-anchor (name &optional quiet no-record)
  "Navigate to HTML anchor NAME in the current buffer.
When called interactively, the user is prompted for a name and provided
a list of valid anchors.
Set QUIET to non-nil to suppress an error message.
Set NO-RECORD to non-nil to prevent the navigation from being pushed
onto the history stack."
  (interactive (list (completing-read
		      "Name: "
		      (w3m--get-page-anchors 'w3m-name-anchor2) nil t)
		     nil nil))
  (let ((anchor-list (w3m--get-page-anchors)) jump-to-pos)
    (if (not (assoc name anchor-list))
	(unless quiet
	  (w3m-message "No such anchor: %s" name))
      (setq jump-to-pos (cdr (assoc name anchor-list)))
      (when (and (not no-record)
		 (/= (point) jump-to-pos))
	(setq w3m-name-anchor-from-hist
	      (append (list 1 nil jump-to-pos (point))
		      (and (integerp (car w3m-name-anchor-from-hist))
			   (nthcdr (1+ (car w3m-name-anchor-from-hist))
				   w3m-name-anchor-from-hist)))))
      (goto-char jump-to-pos)
      (w3m-recenter))))

(defun w3m-parent-page-available-p ()
  (if (null w3m-current-url)
      nil
    (save-match-data
      (string-match "\\`[a-z]+://?[^/]+/." w3m-current-url))))

(defun w3m-url-savable-p ()
  "Return non-nil if the current page is able to be saved."
  (and w3m-current-url
       (or (not (string-match "\\`\\(?:about\\|file\\):"
			      w3m-current-url))
	   (string-match "\\`about://\\(?:header\\|source\\)/"
			 w3m-current-url))))

(defun w3m-view-parent-page (&optional count)
  "Attempt to move to the parent directory of the page currently displayed.
For instance, it will let you visit \"http://foo/bar/\" if you are currently
viewing \"http://foo/bar/baz\".
If COUNT is a integer, you will visit the parent directory to step up the COUNT.
If COUNT is zero, you will visit the top of this site."
  (interactive "p")
  (unless (integerp count)
    (setq count 1))
  (setq count (abs count))
  (w3m-history-store-position)
  (cond
   ((and w3m-current-url
	 (eq count 0)
	 (string-match "\\`[a-z]+:///?[^/]+/" w3m-current-url))
    (w3m-goto-url (match-string 0 w3m-current-url)))
   (w3m-start-url (w3m-goto-url w3m-start-url))
   (w3m-contents-url (w3m-goto-url w3m-contents-url))
   (w3m-current-url
    (let ((parent-url w3m-current-url))
      (catch 'loop
	(while (not (zerop count))
	  (setq count (1- count))
	  ;; Check whether http://foo/bar/ or http://foo/bar
	  (if (string-match "/\\'" parent-url)
	      (if (string-match "\\(.*\\)/[^/]+/\\'" parent-url)
		  ;; http://foo/bar/ -> http://foo/
		  (setq parent-url (concat (match-string 1 parent-url) "/")))
	    (if (string-match "\\(.*\\)/.+\\'" parent-url)
		;; http://foo/bar -> http://foo/
		(setq parent-url (concat (match-string 1 parent-url) "/"))))
	  ;; Ignore "http:/"
	  (cond
	   ((string-match "\\`[a-z]+:///?[^/]+/\\'" parent-url)
	    (throw 'loop t))
	   ((and parent-url
		 (string-match "\\`[a-z]+:/+\\'" parent-url))
	    (setq parent-url nil)
	    (throw 'loop nil)))))
      (if parent-url
	  (w3m-goto-url parent-url)
	(error "No parent page for: %s" w3m-current-url))))
   (t (error "w3m-current-url is not set"))))

(defun w3m-view-previous-page (&optional count no-store-pos)
  "Move back COUNT pages in the history.
If COUNT is a positive integer, move backward COUNT times in the
history.  If COUNT is a negative integer, moving forward is performed.
COUNT is treated as 1 by default if it is omitted.  NO-STORE-POS if it
is non-nil means not to store the window positions before going to the
previous page."
  (interactive "p")
  (unless w3m-current-url
    ;; This page may have not been registered in the history since an
    ;; accident has probably occurred, so we should skip the page.
    (if (integerp count)
	(when (> count 0)
	  (cl-decf count))
      (setq count 0)))
  (let ((index (car w3m-name-anchor-from-hist))
	pos)
    (if (and (integerp count)
	     (integerp index)
	     (< 0 (setq index (+ index count)))
	     (setq pos (nth index w3m-name-anchor-from-hist)))
	(progn
	  (when (and (= (point) pos)
		     (nth (1+ index) w3m-name-anchor-from-hist))
	    (setq index (1+ index)))
	  (goto-char (nth index w3m-name-anchor-from-hist))
	  (setcar w3m-name-anchor-from-hist index)
	  ;; Restore last position.
	  (w3m-history-restore-position))
      (let ((hist ;; Cons of a new history element and position pointers.
	     (if (integerp count)
		 (w3m-history-backward count)
	       (w3m-history-backward)))
	    ;; Inhibit sprouting of a new history.
	    (w3m-history-reuse-history-elements t))
	(if (caar hist)
	    (let ((w3m-prefer-cache t))
	      ;; Save last position.
	      (or no-store-pos (w3m-history-store-position))
	      (w3m-goto-url (caar hist) nil nil
			    (w3m-history-plist-get :post-data)
			    (w3m-history-plist-get :referer)
			    nil
			    (w3m-history-element (caddr hist) t))
	      ;; Set the position pointers in the history.
	      (setcar w3m-history (cdr hist))
	      ;; Restore last position.
	      (w3m-history-restore-position))
	  (if (and (equal w3m-current-url "about://cookie/")
		   (> (length (w3m-list-buffers t)) 1))
	      (w3m-delete-buffer)
	    (w3m-message "There's no more history")))))))

(defun w3m-view-next-page (&optional count)
  "Move forward COUNT pages in history.
If COUNT is a positive integer, move forward COUNT times in the
history.  If COUNT is a negative integer, moving backward is performed.
COUNT is treated as 1 by default if it is omitted."
  (interactive "p")
  (w3m-view-previous-page (if (integerp count) (- count) -1)))

(defun w3m-expand-path-name (file base)
  (let ((input (if (eq (elt file 0) ?/)
		   file
		 (concat base file)))
	(output ""))
    (save-match-data
      (while (string-match "\\`\\(?:\\.\\.?/\\)+" input)
	(setq input (substring input (match-end 0))))
      (while (not (zerop (length input)))
	(cond
	 ((string-match "\\`/\\.\\(?:/\\|\\'\\)" input)
	  (setq input (concat "/" (substring input (match-end 0)))))
	 ((string-match "\\`/\\.\\.\\(?:/\\|\\'\\)" input)
	  (setq input (concat "/" (substring input (match-end 0))))
	  (when (string-match "/?[^/]+\\'" output)
	    (setq output (substring output 0 (match-beginning 0)))))
	 ((string-match "\\`\\.\\.?\\'" input)
	  (setq input ""))
	 (t
	  (let ((end (and (string-match "\\`/[^/]*" input)
			  (match-end 0))))
	    (setq output
		  (concat output (substring input 0 end)))
	    (setq input
		  (substring input end))))))
      output)))

(defconst w3m-url-hierarchical-schemes
  '("http" "https" "ftp" "ftps" "file")
  "List of schemes which may have hierarchical parts.
This list is refered to by `w3m-expand-url' to keep backward
compatibility which is described in Section 5.2 of RFC 2396.")

(defconst w3m-buffer-local-url "buffer://")
(defun w3m-buffer-local-url-p (url)
  (save-match-data
    (string-match (concat "\\`" w3m-buffer-local-url) url)))

(defun w3m-expand-url (url &optional base)
  "Convert URL to the absolute address, and canonicalize it."
  (save-match-data
    (if base
	(if (progn
	      (w3m-string-match-url-components base)
	      (match-beginning 1))
	    (and (not (match-beginning 3))
		 (member (downcase (match-string 2 base))
			 w3m-url-hierarchical-schemes)
		 (setq base (concat
			     (substring base 0 (match-end 1))
			     "//"
			     (substring base (match-beginning 5)))))
	  (error "BASE must have a scheme part: %s" base))
      (setq base (or w3m-current-base-url
		     w3m-current-url
		     ;; Make url absolutely invalid.  See `w3m-url-valid'.
		     w3m-url-invalid-base)))
    (w3m-string-match-url-components url)
    ;; Remove an empty fragment part.
    (when (and (match-beginning 8)
	       (= (match-beginning 9) (length url)))
      (setq url (substring url 0 (match-beginning 8)))
      (w3m-string-match-url-components url))
    ;; Remove an empty query part.
    (when (and (match-beginning 6)
	       (= (match-beginning 7) (or (match-beginning 8)
					  (length url))))
      (setq url (concat (substring url 0 (match-beginning 6))
			(if (match-beginning 8)
			    (substring url (match-beginning 8))
			  ""))
	    base (progn (w3m-string-match-url-components base)
			(substring base 0 (match-beginning 6))))
      (w3m-string-match-url-components url))
    (cond
     ((match-beginning 1)
      ;; URL has a scheme part. => URL may have an absolute spec.
      (if (or (match-beginning 3)
	      (and (< (match-beginning 5) (length url))
		   (eq ?/ (aref url (match-beginning 5)))))
	  ;; URL has a net-location part or an absolute hierarchical
	  ;; part. => URL has an absolute spec.
	  url
	(let ((scheme (match-string 2 url)))
	  (if (and (member (downcase scheme) w3m-url-hierarchical-schemes)
		   (progn
		     (w3m-string-match-url-components base)
		     (equal scheme (match-string 2 base))))
	      (w3m-expand-url (substring url (match-end 1)) base)
	    url))))
     ((match-beginning 3)
      ;; URL has a net-location part. => The hierarchical part of URL
      ;; has an absolute spec.
      (w3m-string-match-url-components base)
      (concat (substring base 0 (match-end 1)) url))
     ((> (match-end 5) (match-beginning 5))
      (let ((path-end (match-end 5)))
	(if (string-equal base w3m-buffer-local-url)
	    (if (eq (aref url 0) ?#)
		(concat base url)
	      ;; Assume url omits the scheme, that is http.
	      (concat "http://" url))
	  (w3m-string-match-url-components base)
	  (concat
	   (substring base 0 (match-beginning 5))
	   (if (and (match-beginning 2)
		    (member (downcase (match-string 2 base))
			    w3m-url-hierarchical-schemes))
	       (w3m-expand-path-name
		(substring url 0 path-end)
		(or
		 ;; Avoid file name handlers; cf.
		 ;; <https://lists.gnu.org/archive/html/tramp-devel/2004-05/msg00016.html>
		 (let (file-name-handler-alist)
		   (file-name-directory (match-string 5 base)))
		 "/"))
	     (substring url 0 path-end))
	   (substring url path-end)))))
     ((match-beginning 6)
      ;; URL has a query part.
      (w3m-string-match-url-components base)
      (concat (substring base 0 (match-end 5)) url))
     (t
      ;; URL has only a fragment part.
      (w3m-string-match-url-components base)
      (concat (substring base 0 (match-beginning 8))
	      url)))))

(defun w3m-display-progress-message (url)
  "Show \"Reading URL...\" message in the middle of a buffer."
  (let ((indent (make-string (max 0 (/ (- (window-width) (length url) 11) 2))
			     ? )))
    (insert-char ?\n (max 0 (- (/ (window-height) 2) 3)))
    ;; (insert-char ?\n (max 0 (/ (1- (window-height)) 2)))
    (insert
     (substitute-command-keys
      (concat
       ;; indent "Reading " url " ...\n\n"
       indent "\
Reading " (w3m-url-readable-string (w3m-url-strip-authinfo url)) " ...\n\n"
       indent "\
`\\<w3m-mode-map>\\[w3m-process-stop]' to abort this operation, or\n"
       indent "\
`\\<w3m-mode-map>\\[w3m-search-new-session]' to perform a search\
 in a new buffer, or\n"
       indent "\
`\\<w3m-mode-map>\\[w3m-goto-url-new-session]' to visit a URL\
 in a new buffer, or\n"
       indent "do any emacs work in any other buffer, or just wait ... ")))
    (put-text-property (point-min) (point-max) 'w3m-progress-message t)
    (sit-for 0)))

(defun w3m-view-this-url (&optional arg new-session)
  "Display the page pointed to by the link under point.
If ARG is the number 2 or the list of the number 16 (you may produce
this by typing `C-u' twice) or NEW-SESSION is non-nil and the link is
an anchor, this function makes a copy of the current buffer in advance.
Otherwise, if ARG is non-nil, it forces to reload the url at point."
  (interactive (if (member current-prefix-arg '(2 (16)))
		   (list nil t)
		 (list current-prefix-arg nil)))
  ;; Store the current position in the history structure.
  (w3m-history-store-position)
  (let ((w3m-prefer-cache
	 (or w3m-prefer-cache
	     (and (stringp w3m-current-url)
		  (string-match "\\`about://\\(?:db-\\)?history/"
				w3m-current-url))))
	act url)
    (cond
     ((setq act (w3m-action))
      (let ((w3m-form-new-session new-session)
	    (w3m-form-download nil))
	(ignore w3m-form-new-session w3m-form-download)
	(eval act)))
     ((setq url (w3m-url-valid (w3m-anchor)))
      (if new-session
	  (w3m-goto-url-new-session url arg)
	(w3m-goto-url url arg)))
     ((w3m-url-valid (w3m-image))
      (if (display-images-p)
	  (w3m-toggle-inline-image)
	(w3m-view-image)))
     ((setq url (w3m-active-region-or-url-at-point 'never))
      (unless (eq 'quit (setq url (w3m-input-url nil url 'quit nil
						 'feeling-searchy 'no-initial)))
	(if new-session
	    (w3m-goto-url-new-session url arg)
	  (w3m-goto-url url arg))))
     (t (w3m-message "No URL at point")))))

(eval-and-compile
  (autoload 'mouse-set-point "mouse"))

(defun w3m-mouse-view-this-url (event &optional arg)
  "Follow the link under the mouse pointer."
  (interactive "e\nP")
  (mouse-set-point event)
  (w3m-view-this-url arg))

(defun w3m-open-all-links-in-new-session (start end &optional reload)
  "Open all http links between START and END as new sessions.
If the page looks like Google's search result and the START point is
the beginning of a line, only the links displayed in the beginning of
lines are picked up.  If RELOAD is non-nil, it forces to reload all
links.  If Transient Mark mode, deactivate the mark."
  (interactive "r\nP")
  (when (region-active-p)
    (deactivate-mark))
  (let ((buffer (current-buffer))
	(prev start)
	(url (w3m-url-valid (w3m-anchor start)))
	urls all)
    (when url
      (setq urls (list url)))
    (save-excursion
      (goto-char start)
      (setq all (not
		 (and (bolp)
		      w3m-current-url
		      (string-match "\\`https?://\\(?:[^/]+\\.\\)*google\\."
				    w3m-current-url))))
      (while (progn
	       (w3m-next-anchor)
	       (and (> (point) prev)
		    (< (point) end)))
	(setq prev (point))
	(when (and (setq url (w3m-url-valid (w3m-anchor)))
		   (string-match "\\`https?:" url)
		   (or all
		       (bolp)))
	  (push url urls))))
    (setq urls (nreverse urls))
    (while urls
      (setq url (car urls)
	    urls (cdr urls))
      (set-buffer buffer)
      (w3m-goto-url-new-session url reload))))

(defun w3m-view-this-url-new-session (&optional background)
  "Display the page of the link under point in a new buffer.
Open a new tab if you use tabs, i.e., `w3m-display-mode' is set to
`tabbed' or `w3m-use-tab' is set to a non-nil value.  If the region is
active, use the `w3m-open-all-links-in-new-session' command instead.

The buffer will get visible if BACKGROUND is nil or there is no other
emacs-w3m buffer regardless of BACKGROUND, otherwise (BACKGROUND is
non-nil) the buffer will be created but not appear to be visible.
BACKGROUND defaults to the value of `w3m-new-session-in-background',
but it could be inverted if called interactively with the prefix arg."
  (interactive)
  (let* ((w3m-new-session-in-background
	  (when (let (w3m-fb-mode) (ignore w3m-fb-mode)
		     (w3m-list-buffers t))
	    (if (w3m-interactive-p)
		(if current-prefix-arg
		    (not w3m-new-session-in-background)
		  w3m-new-session-in-background)
	      (or background w3m-new-session-in-background))))
	 (w3m-clear-display-while-reading
	  ;; Don't show the progress message for the background run.
	  (unless w3m-new-session-in-background
	    w3m-clear-display-while-reading)))
    (if (region-active-p)
	(call-interactively 'w3m-open-all-links-in-new-session)
      (w3m-view-this-url nil t))))

(defun w3m-mouse-view-this-url-new-session (event)
  "Follow the link under the mouse pointer in a new session."
  (interactive "e")
  (mouse-set-point event)
  (w3m-view-this-url nil t))

(defun w3m-submit-form (&optional new-session)
  "Submit the form at point."
  (interactive "P")
  (let ((submit (w3m-submit)))
    (if (and submit
	     w3m-current-url
	     (w3m-url-valid w3m-current-url)
	     (if w3m-submit-form-safety-check
		 (prog1 (y-or-n-p "Submit? ") (message nil))
	       t))
	(let ((w3m-form-new-session new-session)
	      (w3m-form-download nil))
	  (ignore w3m-form-new-session w3m-form-download)
	  (eval submit))
      (w3m-message "Can't submit form at this point"))))

(defun w3m-external-view (url &optional no-cache handler)
  (when (w3m-url-valid url)
    (w3m-process-do
	(type (w3m-content-type url no-cache handler))
      (if (not type)
	  (w3m-download url nil no-cache handler)
	(let* ((def (assoc type w3m-content-type-alist))
	       (method (nth 2 def)))
	  (unless method
	    (setq def (assoc (w3m-prepare-content url type nil)
			     w3m-content-type-alist)
		  method (nth 2 def)))
	  (when (and (consp method) (eq 'mailcap-mime-info (car method)))
	    (mailcap-parse-mailcaps)
	    (when (setq method (eval method))
	      (dolist (v (prog1 (split-string method) (setq method nil)))
		(push (cond ((string-equal "%s" v) 'file)
			    ((string-equal "%u" v) 'url)
			    (t v))
		      method))
	      (setcar (cddr def) (setq method (nreverse method)))))
	  (when (consp method)
	    (let ((result))
	      (setq method
		    (dolist (elem method (nreverse result))
		      (push (if (and (stringp elem)
				     (string-match "\\`[\"']?%s[\"']?\\'"
						   elem))
				'file
			      elem)
			    result)))))
	  (cond
	   ((not method)
	    (if (w3m-url-local-p url)
		(error "\
No method to view `%s' is registered. Use `w3m-edit-this-url'"
		       (file-name-nondirectory (w3m-url-to-file-name url)))
	      (w3m-download url nil no-cache handler)))
	   ((functionp method)
	    (funcall method url))
	   ((consp method)
	    (let ((command (w3m-which-command (car method)))
		  (arguments (cdr method))
		  (file (make-temp-name (expand-file-name
					 "w3mel"
					 w3m-external-view-temp-directory)))
		  (suffix (file-name-nondirectory url)))
	      (when (string-match "\\.[0-9A-Za-z]+\\'" suffix)
		(setq suffix (match-string 0 suffix))
		(when (< (length suffix) 5)
		  (setq file (concat file suffix))))
	      (cond
	       ((and command (memq 'file arguments))
		(let ((w3m-current-buffer (current-buffer)))
		  (w3m-process-do
		      (success (w3m-download url file no-cache handler))
		    (when success
		      (w3m-external-view-file command file url arguments)))))
	       (command
		(w3m-external-view-file command nil url arguments))
	       (t
		(w3m-download url nil no-cache handler)))))))))))

(defun w3m-external-view-file (command file url arguments)
  ;; The values of the arguments `file' and `url' will substitute the
  ;; symbols specified with the same names in `w3m-content-type-alist'.
  (let (proc)
    (unwind-protect
	(with-current-buffer
	    (generate-new-buffer " *w3m-external-view*")
	  (setq proc
		(apply 'start-process
		       "w3m-external-view"
		       (current-buffer)
		       command
		       (mapcar (lambda (x)
				 (pcase x ('file file) ('url url) (_ x)))
			       arguments)))
	  (w3m-message "Start %s..." (file-name-nondirectory command))
	  (set-process-sentinel
	   proc
	   (lambda (proc event)
	     (let ((buffer (process-buffer proc)))
	       (when (string-match "\\`\\(?:finished\\|exited\\)" event)
		 ;; Some program lies that the process has been finished
		 ;; even though it has not read the temp file yet, so
		 ;; it is necessary to delay deleting of the file.
		 (run-at-time 1 nil
			      (lambda (file buffer)
				(and file
				     (file-exists-p file)
				     ;; The file might be busy now.
				     (condition-case nil
					 (progn (delete-file file) t)
				       (error nil))
				     (buffer-name buffer)
				     (progn
				       (kill-buffer buffer)
				       (message ""))))
			      file buffer))))))
      (and file
	   (stringp file)
	   (file-exists-p file)
	   (unless (and (processp proc)
			(memq (process-status proc) '(run stop)))
	     (delete-file file))))))

(defun w3m-view-image ()
  "Display the image under point in the external viewer.
The viewer is defined in `w3m-content-type-alist' for every type of an
image."
  (interactive)
  (let ((url (w3m-url-valid (w3m-image))))
    (if url
	(w3m-external-view url)
      (w3m-message "No image at point"))))

(defun w3m-save-image ()
  "Save the image under point to a file.
The default name will be the original name of the image."
  (interactive)
  (let ((url (w3m-url-valid (w3m-image))))
    (if url
	(w3m-download url)
      (w3m-message "No image at point"))))

(defun w3m-external-view-this-url ()
  "Launch the external browser and display the link an point."
  ;; This command was listed in the menu bar and a link menu till
  ;; 2013-10-17.  As someone may still need it, don't delete it even
  ;; if emacs-w3m no longer uses.
  (interactive)
  (let ((url (w3m-url-valid (or (w3m-anchor) (w3m-image)))))
    (if url
	(w3m-external-view url)
      (w3m-message "No URL at point"))))

(defun w3m-external-view-current-url ()
  "Launch the external browser and display the current URL."
  ;; This command was listed in the menu bar till 2013-10-17.
  ;; As someone may still need it, don't delete it even if emacs-w3m
  ;; no longer uses.
  (interactive)
  (if w3m-current-url
      (w3m-external-view w3m-current-url)
    (w3m-message "No URL at this page")))

(defun w3m-view-url-with-external-browser (&optional url)
  "Launch the external browser and display the same web page.
If the cursor points to a link, it visits the url of the link instead
of the url currently displayed.  The browser is defined in
`w3m-content-type-alist' for every type of a url."
  ;; This command bound the M key and was listed in the tab menu till
  ;; 2013-10-17.  As someone may still need it, don't delete it even
  ;; if emacs-w3m no longer uses.
  (interactive (list (w3m-canonicalize-url
		      (w3m-input-url "URL to view externally: "
				     nil
				     (or (w3m-anchor)
					 (unless w3m-display-inline-images
					   (w3m-image))
					 w3m-current-url)
				     nil nil 'no-initial))))
  (when (w3m-url-valid url)
    (message "Browsing <%s>..." url)
    (w3m-external-view url)))

(defun w3m-view-url-with-browse-url (url)
  "Run `browse-url' to open URL."
  (interactive (list (let ((w3m-display-inline-images t))
		       (w3m-active-region-or-url-at-point t))))
  (if (and (stringp url)
	   (not (string-match "\\`about:" url)))
      (progn
	(w3m-message "Browsing %s..." url)
	(browse-url url))
    (w3m-message "No url at point")))

(defun w3m-download-this-url ()
  "Download the file or the page pointed to by the link under point."
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))) act)
    (cond
     ((w3m-url-valid url)
      (let ((pos (point-marker))
	    (curl w3m-current-url))
	(w3m-process-with-null-handler
	  (w3m-process-do
	      (success (w3m-download url nil nil handler))
	    (and success
		 (buffer-name (marker-buffer pos))
		 (with-current-buffer (marker-buffer pos)
		   (when (equal curl w3m-current-url)
		     (goto-char pos)
		     (w3m-refontify-anchor))))))))
     ((setq act (w3m-action))
      (let ((w3m-form-download t))
	(ignore w3m-form-download)
	(eval act)))
     (t
      (w3m-message "No URL at point")))))

(defun w3m-download-this-image ()
  "Download the image under point."
  (interactive)
  (let ((url (w3m-image)) act)
    (cond
     ((w3m-url-valid url)
      (let ((pos (point-marker))
	    (curl w3m-current-url))
	(w3m-process-with-null-handler
	  (w3m-process-do
	      (success (w3m-download url nil nil handler))
	    (and success
		 (buffer-name (marker-buffer pos))
		 (with-current-buffer (marker-buffer pos)
		   (when (equal curl w3m-current-url)
		     (goto-char pos)
		     (w3m-refontify-anchor))))))))
     ((setq act (w3m-action))
      (let ((w3m-form-download t))
	(ignore w3m-form-download)
	(eval act)))
     (t
      (w3m-message "No image at point")))))

(defun w3m-print-current-url ()
  "Display the current url in the echo area and put it into `kill-ring'."
  (interactive)
  (when w3m-current-url
    (let ((deactivate-mark nil))
      (kill-new (w3m-url-encode-string-2 w3m-current-url))
      (w3m-message "%s" (w3m-url-readable-string w3m-current-url)))))

(defvar message-truncate-lines)

(defun w3m-print-this-url (&optional interactive-p)
  "Display the url under point in the echo area and put it into `kill-ring'."
  (interactive (list t))
  (let ((deactivate-mark nil)
	(url (if interactive-p
		 (or (w3m-anchor)
		     (w3m-image)
		     (and (stringp w3m-current-url)
			  (let ((name (or (car (get-text-property
						(point) 'w3m-name-anchor))
					  (car (get-text-property
						(point)
						'w3m-name-anchor2)))))
			    (when name
			      (concat w3m-current-url "#" name)))))
	       (or (w3m-anchor (point)) (w3m-image (point)))))
	(alt (if interactive-p
		 (w3m-image-alt)
	       (w3m-image-alt (point))))
	(title (if interactive-p
		   (w3m-anchor-title)
		 (w3m-anchor-title (point))))
	(message-truncate-lines (or message-truncate-lines
				    (not interactive-p))))
    (when (or url interactive-p)
      (and url interactive-p (kill-new (w3m-url-encode-string-2 url)))
      (setq url (or (w3m-url-readable-string url)
		    (and (w3m-action) "There is a form")
		    "There is no url under point"))
      (w3m-message "%s" (cond
			 ((> (length alt) 0)
			  (concat alt ": " url))
			 ((> (length title) 0)
			  (let ((str (concat title " (" url ")")))
			    (if (or message-truncate-lines
				    (< (string-width str) (- (frame-width) 2)))
				;; Print a single line if the string fits
				;; to the frame width or is truncated.
				str
			      ;; Otherwise print the string in two lines.
			      (concat title "\n" url))))
			 (t
			  url))))))

(defun w3m-print-this-image-url (&optional interactive-p)
  "Display image url under point in echo area and put it into `kill-ring'."
  (interactive (list t))
  (let ((deactivate-mark nil)
	(url (if interactive-p
		 (w3m-image)
	       (w3m-image (point))))
	(alt (if interactive-p
		 (w3m-image-alt)
	       (w3m-image-alt (point)))))
    (when (or url interactive-p)
      (and url interactive-p (kill-new (w3m-url-encode-string-2 url)))
      (w3m-message "%s%s"
		   (if (zerop (length alt))
		       ""
		     (concat alt ": "))
		   (or (w3m-url-readable-string url)
		       (and (w3m-action) "There is a form")
		       "There is no image url under point")))))

(defmacro w3m-delete-all-overlays ()
  "Delete all momentary overlays."
  '(dolist (overlay (overlays-in (point-min) (point-max)))
     (if (overlay-get overlay 'w3m-momentary-overlay)
	 (delete-overlay overlay))))

(defun w3m-highlight-current-anchor-1 (seq)
  "Highlight an anchor in the line if the anchor sequence is the same as SEQ.
Return t if highlighting is successful."
  (let ((limit (point-at-eol))
	ov beg pos pseq)
    (save-excursion
      (beginning-of-line)
      (setq pos (point))
      (while (and pos
		  (< pos limit)
		  (not (eq seq (setq pseq (w3m-anchor-sequence pos)))))
	(setq pos (next-single-property-change pos 'w3m-anchor-sequence)))
      (when (and pos (< pos limit) (eq seq pseq))
	(setq beg pos)
	(setq pos (next-single-property-change pos 'w3m-anchor-sequence))
	(setq ov (make-overlay beg pos))
	(overlay-put ov 'face 'w3m-current-anchor)
	(overlay-put ov 'w3m-momentary-overlay t)
	(overlay-put ov 'evaporate t)
	t))))

(defun w3m-highlight-current-anchor ()
  "Highlight an anchor under point."
  (when (let ((ovs (overlays-at (point))) ov)
	  ;; If the anchor is already highlighted, it does nothing.
	  (or (null ovs)
	      (null (progn (while ovs
			     (if (overlay-get (car ovs) 'w3m-momentary-overlay)
				 (setq ov (car ovs)
				       ovs nil))
			     (setq ovs (cdr ovs)))
			   ov))))
    (w3m-delete-all-overlays)
    (save-excursion
      (let ((seq (w3m-anchor-sequence))
	    (pos (point)))
	(when (and seq
		   (w3m-highlight-current-anchor-1 seq)
		   (zerop (forward-line 1)))
	  (while (and (w3m-highlight-current-anchor-1 seq)
		      (zerop (forward-line 1))))
	  (goto-char pos)
	  (while (and (zerop (forward-line -1))
		      (w3m-highlight-current-anchor-1 seq))))))))

(defun w3m-edit-url (url)
  "Edit the source code of URL."
  (interactive (list (w3m-canonicalize-url (w3m-input-url))))
  (when (string-match "\\`about://\\(?:header\\|source\\)/" url)
    (setq url (substring url (match-end 0))))
  (catch 'found
    (dolist (pair w3m-edit-function-alist)
      (when (and (string-match (car pair) url)
		 (fboundp (cdr pair)))
	(throw 'found (funcall (cdr pair) url))))
    (funcall w3m-edit-function
	     (or (w3m-url-to-file-name url)
		 (call-interactively 'w3m-save-buffer)
		 (error "URL:%s is not a local file" url)))))

(defun w3m-edit-current-url ()
  "Edit the source code of the file that the current buffer is viewing."
  (interactive)
  (if w3m-current-url
      (w3m-edit-url w3m-current-url)
    (w3m-message "No URL")))

(defun w3m-edit-this-url ()
  "Edit the source code of the file linked from the anchor at point."
  (interactive)
  (let ((url (w3m-url-valid (w3m-anchor))))
    (if url
	(w3m-edit-url url)
      (w3m-message "No URL at point"))))

(defvar w3m-goto-anchor-hist nil)
(make-variable-buffer-local 'w3m-goto-anchor-hist)

(defun w3m-next-anchor (&optional arg)
  "Move point to the next anchor.  Return t if success otherwise nil."
  (interactive "p")
  (unless (eq last-command 'w3m-next-anchor)
    (setq w3m-goto-anchor-hist nil))
  (if (and arg (< arg 0))
      (w3m-previous-anchor (- arg))
    (unless arg (setq arg 1))
    (let ((noanchor (not (text-property-not-all (point-min) (point-max)
						'w3m-anchor-sequence nil)))
	  hseq pos next)
      (when (and (not (or noanchor (eobp)))
		 (or (and (setq hseq (w3m-anchor-sequence))
			  (setq pos (point)))
		     (and (setq pos (next-single-property-change
				     (point) 'w3m-anchor-sequence))
			  (if (setq hseq (w3m-anchor-sequence pos))
			      ;; Skip the anchor searched once.
			      (unless (memq hseq w3m-goto-anchor-hist)
				(setq arg (1- arg)))
			    (setq pos nil)))))
	(while (and (> arg 0) (< pos (point-max)))
	  (or (and (setq pos (next-single-property-change
			      pos 'w3m-anchor-sequence))
		   (or (setq next (w3m-anchor-sequence pos))
		       (and (setq pos (next-single-property-change
				       pos 'w3m-anchor-sequence))
			    (setq next (w3m-anchor-sequence pos))))
		   (or (eq hseq next) ;; Skip the anchor of the same ID.
		       ;; Skip the anchor searched once.
		       (memq next w3m-goto-anchor-hist)
		       (setq hseq next
			     arg (1- arg))))
	      (setq arg 0))))
      (if pos
	  (progn
	    (push hseq w3m-goto-anchor-hist)
	    (goto-char pos)
	    (w3m-horizontal-on-screen)
	    (w3m-print-this-url)
	    t)
	(setq w3m-goto-anchor-hist nil)
	(or (and (w3m-handle-non-anchor-buttons)
		 (goto-char (point-min))
		 (let ((button (next-button (point))))
		   (if button
		       (progn (goto-char (button-at button)) t)
		     (goto-char (point-min))
		     (and (ignore-errors (forward-button 1)) t))))
	    (if noanchor
		(progn (w3m-message "No anchor") nil)
	      (goto-char (point-min))
	      (w3m-next-anchor 1)))))))

(defun w3m-previous-anchor (&optional arg)
  "Move point to the previous anchor.  Return t if success otherwise nil."
  (interactive "p")
  (unless (eq last-command 'w3m-previous-anchor)
    (setq w3m-goto-anchor-hist nil))
  (if (and arg (< arg 0))
      (w3m-next-anchor (- arg))
    (unless arg (setq arg 1))
    (let ((noanchor (not (text-property-not-all (point-min) (point-max)
						'w3m-anchor-sequence nil)))
	  hseq pos prev next)
      (when (and (not (or noanchor (bobp)))
		 (or (and
		      (setq hseq (w3m-anchor-sequence)) ;; <- The current.
		      ;; Search for the previous anchor from the bottom of
		      ;; the current anchor lines because the top of the
		      ;; current anchor might stay above the previous anchor.
		      (prog1
			  t
			;; Go to the bottom.
			(setq pos (point)
			      next pos)
			(while (and
				(setq next (next-single-property-change
					    next 'w3m-anchor-sequence))
				(or (eq (w3m-anchor-sequence next) hseq)
				    (setq next (text-property-any
						next (point-max)
						'w3m-anchor-sequence hseq)))
				(setq pos next)))))
		     (and (or (w3m-anchor-sequence (setq pos (1- (point))))
			      (and (setq pos (previous-single-property-change
					      (point) 'w3m-anchor-sequence))
				   (setq pos
					 (or (previous-single-property-change
					      pos 'w3m-anchor-sequence)
					     (point-min)))))
			  (if (setq hseq (w3m-anchor-sequence pos))
			      ;; Skip the anchor searched once.
			      (unless (memq hseq w3m-goto-anchor-hist)
				(setq arg (1- arg)))
			    (setq pos nil)))))
	(while (and (> arg 0) (> pos (point-min)))
	  (or (and (setq pos (previous-single-property-change
			      pos 'w3m-anchor-sequence))
		   (or (setq prev (w3m-anchor-sequence pos))
		       (and (setq pos (or (previous-single-property-change
					   pos 'w3m-anchor-sequence)
					  (point-min)))
			    (setq prev (w3m-anchor-sequence pos))))
		   (or (eq hseq prev) ;; Skip the anchor of the same ID.
		       ;; Skip the anchor searched once.
		       (memq prev w3m-goto-anchor-hist)
		       (setq hseq prev
			     arg (1- arg))))
	      (setq arg 0))))
      (if pos
	  (progn
	    (push hseq w3m-goto-anchor-hist)
	    ;; Go to the beginning of the anchor.
	    (goto-char (or (text-property-any (point-min) pos
					      'w3m-anchor-sequence hseq)
			   pos))
	    (w3m-horizontal-on-screen)
	    (w3m-print-this-url)
	    t)
	(setq w3m-goto-anchor-hist nil)
	(or (and nil ;; Ignore non-anchor button because moving to it
		 ;; would be not so useful.
		 (w3m-handle-non-anchor-buttons)
		 (let ((button (previous-button (point))))
		   (if button
		       (progn (goto-char (button-at button)) t)
		     (goto-char (point-min))
		     (and (ignore-errors (forward-button 1)) t))))
	    (if noanchor
		(progn (w3m-message "No anchor") nil)
	      (goto-char (point-max))
	      (w3m-previous-anchor 1)))))))

(defun w3m-goto-next-form ()
  ;; Move the point to the end of the current form.
  (when (w3m-action (point))
    (goto-char (next-single-property-change (point) 'w3m-action)))
  ;; Find the next form.
  (or (w3m-action (point))
      (let ((pos (next-single-property-change (point) 'w3m-action)))
	(when pos
	  (goto-char pos)
	  t))))

(defun w3m-next-form (&optional arg)
  "Move the point to the next form."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-form w3m-previous-form)))
      (when (setq w3m-goto-anchor-hist (w3m-action (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-previous-form) w3m-goto-anchor-hist)
      (setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-previous-form (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-form)
	;; Make a search from the beginning of the buffer.
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-min))
	(w3m-goto-next-form))
      (setq arg (1- arg))
      (if (member (w3m-action (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-action (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
    (w3m-print-this-url)))

(defun w3m-goto-previous-form ()
  ;; Move the point to the beginning of the current form.
  (when (w3m-action (point))
    (goto-char (previous-single-property-change (1+ (point))
						'w3m-action)))
  ;; Find the previous form.
  (let ((pos (previous-single-property-change (point) 'w3m-action)))
    (if pos
	(goto-char
	 (if (w3m-action pos)
	     pos
	   (previous-single-property-change pos 'w3m-action))))))

(defun w3m-previous-form (&optional arg)
  "Move the point to the previous form."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-form w3m-previous-form)))
      (when (setq w3m-goto-anchor-hist (w3m-action (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-next-form) w3m-goto-anchor-hist)
      (setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-next-form (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-form)
	;; search from the end of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-max))
	(w3m-goto-previous-form))
      (setq arg (1- arg))
      (if (member (w3m-action (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-action (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
    (w3m-print-this-url)))

(defun w3m-goto-next-image ()
  ;; Move the point to the end of the current image.
  (when (w3m-image (point))
    (goto-char (next-single-property-change (point) 'w3m-image)))
  ;; Find the next form or image.
  (or (w3m-image (point))
      (let ((pos (next-single-property-change (point) 'w3m-image)))
	(when pos
	  (goto-char pos)
	  t))))

(defun w3m-next-image (&optional arg)
  "Move the point to the next image."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command
		  '(w3m-next-image w3m-previous-image)))
      (when (setq w3m-goto-anchor-hist (w3m-image (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-previous-image)
	       w3m-goto-anchor-hist)
      (setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-previous-image (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-image)
	;; Make a search for an image from the beginning of the buffer.
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-min))
	(w3m-goto-next-image))
      (setq arg (1- arg))
      (if (member (w3m-image (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-image (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
    (w3m-print-this-url)))

(defun w3m-goto-previous-image ()
  ;; Move the point to the beginning of the current image.
  (when (w3m-image (point))
    (goto-char (previous-single-property-change (1+ (point))
						'w3m-image)))
  ;; Find the previous form or image.
  (let ((pos (previous-single-property-change (point) 'w3m-image)))
    (if pos
	(goto-char
	 (if (w3m-image pos) pos
	   (previous-single-property-change pos 'w3m-image))))))

(defun w3m-previous-image (&optional arg)
  "Move the point to the previous image."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-image w3m-previous-image)))
      (when (setq w3m-goto-anchor-hist (w3m-image (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-next-image)
	       w3m-goto-anchor-hist)
      (setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-next-image (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-image)
	;; Make a search from the end of the buffer.
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-max))
	(w3m-goto-previous-image))
      (setq arg (1- arg))
      (if (member (w3m-image (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-image (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
    (w3m-print-this-url)))

(defun w3m-copy-buffer (&optional buffer new-name background empty last)
  "Copy an emacs-w3m BUFFER, and return the new buffer.

If BUFFER is nil, the current buffer is assumed.

If NEW-NAME is nil, a name is created based on the name of the current
buffer.  If BACKGROUND is non-nil, do not switch to the new buffer.

When called interactively, you will be prompted for NEW-NAME if and
only if a prefix argument is given, and BACKGROUND inherits the value
of `w3m-new-session-in-background'.

If EMPTY is non-nil, an empty buffer is created, but with the current
buffer's history and settings.

If LAST is non-nil, the new buffer will be buried as the final w3m
buffer; otherwise, it will be sequenced next to the current buffer."
  (interactive (list nil
		     (if current-prefix-arg (read-string "Name: "))
		     w3m-new-session-in-background))
  (unless buffer
    (setq buffer (current-buffer)))
  (unless new-name
    (setq new-name (buffer-name buffer)))
  (when (string-match "<[0-9]+>\\'" new-name)
    (setq new-name (substring new-name 0 (match-beginning 0))))
  (let (url coding images init-frames new)
    (save-current-buffer
      (set-buffer buffer)
      (setq url (or w3m-current-url
		    (car (w3m-history-element (cadar w3m-history))))
	    coding w3m-current-coding-system
	    images w3m-display-inline-images
	    init-frames (when (w3m-popup-frame-p)
			  (copy-sequence w3m-initial-frames)))
      (unless url
	(setq empty t))
      ;;
      (w3m-history-store-position)
      (set-buffer (setq new (w3m-generate-new-buffer new-name (not last))))
      ;; Make copies of `w3m-history' and `w3m-history-flat'.
      (w3m-history-copy buffer)
      (setq w3m-current-coding-system coding
	    w3m-initial-frames init-frames
	    w3m-display-inline-images
	    (if w3m-toggle-inline-images-permanently
		images
	      w3m-default-display-inline-images)))
    (cond
     (empty) ;; Don't leave from the current buffer.
     (t ;; Switch to the `new' buffer in which `w3m-goto-url' runs.
      (set-buffer new)
      ;; Render a page.
      (let ((positions (copy-sequence (car w3m-history)))
	    (w3m-history-reuse-history-elements t)
	    (w3m-prefer-cache t)
	    (w3m-clear-display-while-reading
	     ;; Don't show the progress message for the background run.
	     (unless (and background (w3m-interactive-p))
	       w3m-clear-display-while-reading)))
	(w3m-process-with-wait-handler
	  (w3m-goto-url url 'redisplay nil nil nil handler
			;; Pass the properties of the history elements,
			;; although it is currently always nil.
			(w3m-history-element (cadr positions))))
	(setcar w3m-history positions))
      (when (and background (not (get-buffer-window buffer)))
	(set-window-buffer (selected-window) buffer))))
    new))

(defvar w3m-previous-session-buffer nil
  "A buffer of the session having selected just before this session.
This will be the session to be selected after `w3m-delete-buffer'
deletes the current session.")
(make-variable-buffer-local 'w3m-previous-session-buffer)

(defun w3m-next-buffer (arg &optional buffer)
  "Turn ARG pages of emacs-w3m buffers ahead.
If BUFFER is specified, switch to it regardless of ARG."
  (interactive "p")
  (when (and (eq major-mode 'w3m-mode)
	     (or (and (buffer-live-p buffer)
		      (with-current-buffer buffer (eq major-mode 'w3m-mode)))
		 (progn
		   (unless arg (setq arg 1))
		   (when (/= arg 0)
		     (let* ((buffers (w3m-list-buffers))
			    (len (length buffers)))
		       (setq buffer (nth
				     (mod
				      (+ arg
					 (- len (length (memq (current-buffer)
							      buffers))))
				      len)
				     buffers)))))))
    (w3m-history-store-position)
    (let ((prev (current-buffer)))
      (switch-to-buffer buffer)
      (setq w3m-previous-session-buffer prev))
    (w3m-history-restore-position)
    (run-hooks 'w3m-select-buffer-hook)
    (w3m-select-buffer-update)))

(defun w3m-previous-buffer (arg)
  "Turn ARG pages of emacs-w3m buffers behind."
  (interactive "p")
  (w3m-next-buffer (- arg)))

(defun w3m-delete-buffer (&optional force)
  "Delete the current emacs-w3m buffer and switch to the previous one.

If there is only one emacs-w3m buffer, assume intent to terminate
the emacs-w3m session; the prefix argument FORCE will be passed
to the `w3m-quit' function. When in a tabbed display mode (see
`w3m-display-mode'), focus is returned to the buffer that
launched this buffer."
  (interactive "P")
  ;; Bind `w3m-fb-mode' to nil so that this function might not call
  ;; `w3m-quit' when there is only one buffer belonging to the selected
  ;; frame, but there are emacs-w3m buffers in other frames.
  (let* ((w3m-fb-mode nil)
	 (buffers (w3m-list-buffers t))
	 (num (length buffers))
	 cur buf bufs)
    (if (= 1 num)
	(w3m-quit force)
      (setq cur (current-buffer))
      (if w3m-use-tab
	  (progn
	    (select-window (or (get-buffer-window cur t) (selected-window)))
	    (w3m-next-buffer -1 w3m-previous-session-buffer))
	;; List buffers being shown in the other windows of the current frame.
	(save-current-buffer
	  (walk-windows (lambda (window)
			  (set-buffer (setq buf (window-buffer window)))
			  (when (and (eq major-mode 'w3m-mode)
				     (not (eq buf cur)))
			    (push buf bufs)))
			'no-minibuf))
	(cond ((= (1- num) (length bufs))
	       ;; All the other buffers are shown in the current frame.
	       (select-window (get-buffer-window (prog2
						     (w3m-next-buffer -1)
						     (current-buffer)
						   (delete-window)))))
	      (bufs
	       ;; Look for the buffer which is not shown in the current frame.
	       (setq buf nil)
	       (while (progn
			(w3m-next-buffer -1)
			(unless buf
			  (setq buf (current-buffer)))
			(memq (current-buffer) bufs)))
	       (when (memq buf bufs)
		 ;; Go to the buffer which is most suitable to be called
		 ;; the *previous* buffer.
		 (select-window (get-buffer-window buf))))
	      ((progn ;; List buffers being not shown anywhere.
		 (setq bufs nil)
		 (while buffers
		   (unless (get-buffer-window (setq buf (pop buffers)) t)
		     (push buf bufs)))
		 bufs)
	       (while (progn
			(w3m-next-buffer -1)
			(not (memq (current-buffer) bufs)))))
	      ((memq (selected-frame) w3m-initial-frames)
	       ;; Assume that this frame was created to show this buffer.
	       (if (one-window-p t)
		   (delete-frame)
		 (delete-window)))
	      (t
	       (if (>= num 2)
		   (w3m-next-buffer -1)
		 (unless (one-window-p t)
		   (delete-window))))))
      (with-temp-buffer
	(w3m-session-deleted-save (list cur))
	(w3m-process-stop cur)
	(w3m-idle-images-show-unqueue cur)
	(when w3m-use-form
	  (w3m-form-kill-buffer cur))
	(let ((ignore-window-parameters t))
	  (kill-buffer cur)))
      ;; A workaround to restore the window positions correctly when
      ;; this command is called by a mouse event.
      (run-at-time 0.1 nil #'w3m-history-restore-position)
      (run-hooks 'w3m-delete-buffer-hook)
      (w3m-session-crash-recovery-save)))
  (w3m-select-buffer-update)
  (unless w3m-fb-inhibit-buffer-selection
    (w3m-fb-select-buffer)))

(defun w3m-delete-buffer-if-empty (buffer)
  "Delete a newly created emacs-w3m buffer BUFFER if it seems unnecessary.
Some emacs-w3m commands create a buffer for the new session first, but
it may be useless if the command is invoked for visiting a local file
or a mail buffer.  This command will delete BUFFER if it is empty or
there is only a progress message.  It also deletes windows and frames
related to BUFFER."
  (with-current-buffer buffer
    (unless (or w3m-current-process
		w3m-current-url
		(not (or (zerop (buffer-size))
			 (and (get-text-property (point-min)
						 'w3m-progress-message)
			      (get-text-property (1- (point-max))
						 'w3m-progress-message)))))
      (w3m-delete-buffer t))))

(defun w3m-pack-buffer-numbers ()
  "Renumber suffixes of names of emacs-w3m buffers.
It aligns emacs-w3m buffers in order of *w3m*, *w3m<2>, *w3m*<3>,...
as if the folder command of MH performs with the -pack option."
  (interactive)
  (let ((count 1) number newname)
    (dolist (buffer (w3m-list-buffers))
      (setq number (w3m-buffer-number buffer))
      (when number
	(unless (eq number count)
	  (when (and (setq newname (w3m-buffer-set-number buffer count))
		     w3m-use-form)
	    (w3m-form-set-number buffer newname)))
	(cl-incf count)))))

(defun w3m-delete-other-buffers (&optional buffer)
  "Delete emacs-w3m buffers except for BUFFER or the current buffer."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (w3m-delete-frames-and-windows buffer)
  (let ((buffers (delq buffer (w3m-list-buffers t))))
    (w3m-delete-buffers buffers)))

(defun w3m-delete-left-tabs ()
  "Delete tabs on the left side of the current tab."
  (interactive)
  (let ((cbuf (current-buffer))
	bufs)
    (setq bufs (catch 'done
		 (dolist (buf (w3m-list-buffers))
		   (if (eq cbuf buf)
		       (throw 'done bufs)
		     (setq bufs (cons buf bufs))))))
    (when bufs
      (w3m-delete-buffers bufs))))

(defun w3m-delete-right-tabs ()
  "Delete tabs on the right side of the current tab."
  (interactive)
  (let ((bufs (w3m-righttab-exist-p)))
    (when bufs
      (w3m-delete-buffers bufs))))

(defun w3m-delete-buffers (buffers)
  "Delete emacs-w3m buffers."
  (let (buffer)
    (when buffers
      (w3m-session-deleted-save buffers))
    (while buffers
      (setq buffer (pop buffers))
      (w3m-process-stop buffer)
      (w3m-idle-images-show-unqueue buffer)
      (kill-buffer buffer)
      (when w3m-use-form
	(w3m-form-kill-buffer buffer))))
  (run-hooks 'w3m-delete-buffer-hook)
  (w3m-session-crash-recovery-save)
  (w3m-select-buffer-update)
  (w3m-force-window-update))

(defvar w3m-ctl-c-map nil
  "Sub-keymap used for the `C-c'-prefixed commands.

Note: keys should not be alphabet since `C-c LETTER' keys are reserved
for users.  See Info node `(elisp)Key Binding Conventions'.")
(unless w3m-ctl-c-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-h"  'w3m-history)
    (define-key map "\C-@"  'w3m-history-store-position)
    (define-key map [?\C- ] 'w3m-history-store-position)
    (define-key map "j"     'w3m-search-name-anchor)
    (define-key map "\C-e"  'w3m-goto-new-session-url)
    (define-key map "\C-v"  'w3m-history-restore-position)
    (define-key map "\C-t"  'w3m-copy-buffer)
    (define-key map "\C-p"  'w3m-previous-buffer)
    (define-key map "\C-n"  'w3m-next-buffer)
    (define-key map [?\C-,] 'w3m-tab-move-left)
    (define-key map [?\C-<] 'w3m-tab-move-left)
    (define-key map "<"     'w3m-tab-move-left)
    (define-key map [?\C-.] 'w3m-tab-move-right)
    (define-key map ">"     'w3m-tab-move-right)
    (define-key map [?\C->] 'w3m-tab-move-right)
    (define-key map "\C-w"  'w3m-delete-buffer)
    (define-key map "\M-w"  'w3m-delete-other-buffers)
    (define-key map "\M-l"  'w3m-delete-left-tabs)
    (define-key map "\M-r"  'w3m-delete-right-tabs)
    (define-key map "\C-s"  'w3m-select-buffer)
    (define-key map "\C-a"  'w3m-switch-buffer)
    (define-key map "\C-b"  'report-emacs-w3m-bug)
    (define-key map "\C-c"  'w3m-submit-form)
    (define-key map "\C-k"  'w3m-process-stop)
    (define-key map "\C-m"  'w3m-move-unseen-buffer)
    (setq w3m-ctl-c-map map)))

(defvar w3m-redisplay-map nil
  "Sub-keymap used for the `C'-prefixed redisplay commands.")
(unless w3m-redisplay-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'w3m-redisplay-with-content-type)
    (define-key map "c" 'w3m-redisplay-with-charset)
    (define-key map "C" 'w3m-redisplay-and-reset)
    (setq w3m-redisplay-map map)))

(defvar w3m-lnum-map nil
  "Sub-keymap used for the `L'-prefixed link numbering commands.")
(unless w3m-lnum-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'w3m-lnum-follow)
    (define-key map "F" 'w3m-lnum-goto)
    (define-key map "w" 'w3m-lnum-universal)
    (define-key map "I" 'w3m-lnum-view-image)
    (define-key map "\M-i" 'w3m-lnum-save-image)
    (define-key map "d" 'w3m-lnum-download-this-url)
    (define-key map "e" 'w3m-lnum-edit-this-url)
    (define-key map "t" 'w3m-lnum-toggle-inline-image)
    (define-key map "u" 'w3m-lnum-print-this-url)
    (define-key map "b" 'w3m-lnum-bookmark-add-this-url)
    (define-key map "]" 'w3m-lnum-zoom-in-image)
    (define-key map "[" 'w3m-lnum-zoom-out-image)
    (setq w3m-lnum-map map)))

(defvar w3m-lynx-like-map nil
  "Lynx-like keymap used in emacs-w3m buffers.")
;; `C-t' is a prefix key reserved to commands that do something in all
;; emacs-w3m buffers.  2006-05-18
(unless w3m-lynx-like-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'w3m-scroll-up-or-next-url)
    (define-key map "b" 'w3m-scroll-down-or-previous-url)
    (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
    (define-key map [delete] 'w3m-scroll-down-or-previous-url)
    (define-key map [?\S- ] 'w3m-scroll-down-or-previous-url)
    (define-key map "f" 'w3m-toggle-filtering)
    (define-key map "h" 'backward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'forward-char)
    (define-key map "J" 'w3m-scroll-up)
    (define-key map "K" 'w3m-scroll-down)
    (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [tab] 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
    (define-key map [backtab] 'w3m-previous-anchor)
    (define-key map [down] 'w3m-next-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map [up] 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [(shift return)] 'w3m-view-this-url-new-session)
    (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
    (define-key map [right] 'w3m-view-this-url)
    (if (featurep 'gtk)
	(progn
	  (define-key map [down-mouse-3] 'w3m-mouse-major-mode-menu)
	  (define-key map [drag-mouse-3] 'undefined)
	  ;; Don't use [mouse-3], which gets submenus not working in GTK Emacs.
	  (define-key map [mouse-3] 'undefined))
      (define-key map [mouse-3] 'w3m-mouse-major-mode-menu))
    (define-key map [mouse-2] 'w3m-mouse-view-this-url)
    ;; Support the mouse-1 trick.
    (define-key map [follow-link] 'mouse-face)
    (define-key map [S-mouse-2] 'w3m-mouse-view-this-url-new-session)
    (define-key map [left] 'w3m-view-previous-page)
    (define-key map "B" 'w3m-view-previous-page)
    (define-key map "N" 'w3m-view-next-page)
    (define-key map "^" 'w3m-view-parent-page)
    (define-key map "\M-d" 'w3m-download)
    (define-key map "d" 'w3m-download-this-url)
    (define-key map "u" 'w3m-print-this-url)
    (define-key map "I" 'w3m-view-image)
    (define-key map "\M-i" 'w3m-save-image)
    (define-key map "c" 'w3m-print-current-url)
    (define-key map "M" 'w3m-view-url-with-browse-url)
    (define-key map "G" 'w3m-goto-url-new-session)
    (define-key map "g" 'w3m-goto-url)
    (define-key map "\C-tt" 'w3m-create-empty-session)
    (define-key map "T" 'w3m-toggle-inline-images)
    (define-key map "\M-T" 'w3m-turnoff-inline-images)
    (define-key map "t" 'w3m-toggle-inline-image)
    (when (display-images-p)
      (define-key map "\M-[" 'w3m-zoom-out-image)
      (define-key map "\M-]" 'w3m-zoom-in-image))
    (define-key map "U" 'w3m-goto-url)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "V" 'w3m-bookmark-view-new-session)
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "\M-s" 'w3m-session-select)
    (define-key map "\M-S" 'w3m-session-save)
    (define-key map "r" 'w3m-redisplay-this-page)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "\C-tR" 'w3m-reload-all-pages)
    (define-key map "?" 'describe-mode)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "\M-k" 'w3m-cookie)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "\C-ta" 'w3m-bookmark-add-all-urls)
    (define-key map "+" 'w3m-antenna-add-current-url)
    (define-key map "]" 'w3m-next-form)
    (define-key map "[" 'w3m-previous-form)
    (define-key map "}" 'w3m-next-image)
    (define-key map "{" 'w3m-previous-image)
    (define-key map "H" 'w3m-gohome)
    (define-key map "A" 'w3m-antenna)
    (define-key map "W" 'w3m-weather)
    (define-key map "s" 'w3m-search)
    (define-key map "S" 'w3m-search-new-session)
    (define-key map "D" 'w3m-dtree)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map "." 'w3m-shift-left)
    (define-key map "," 'w3m-shift-right)
    (define-key map "\M-l" 'w3m-horizontal-recenter)
    (define-key map "\C-a" 'w3m-beginning-of-line)
    (define-key map "\C-e" 'w3m-end-of-line)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "=" 'w3m-view-header)
    (define-key map "E" 'w3m-edit-current-url)
    (define-key map "e" 'w3m-edit-this-url)
    (define-key map "|" 'w3m-pipe-source)
    (define-key map "\C-c" w3m-ctl-c-map)
    (define-key map "C" w3m-redisplay-map)
    (define-key map "L" w3m-lnum-map)
    (define-key map "\C-x\C-s" 'w3m-save-buffer)
    (setq w3m-lynx-like-map map)))

(defvar w3m-info-like-map nil
  "Info-like keymap used in emacs-w3m buffers.")
;; `C-t' is a prefix key reserved to commands that do something in all
;; emacs-w3m buffers.  2006-05-18
(unless w3m-info-like-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
    (define-key map [delete] 'w3m-scroll-down-or-previous-url)
    (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
    (define-key map [?\S- ] 'w3m-scroll-down-or-previous-url)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [tab] 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
    (define-key map [backtab] 'w3m-previous-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [(shift return)] 'w3m-view-this-url-new-session)
    (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
    (define-key map [mouse-2] 'w3m-mouse-view-this-url)
    ;; Support the mouse-1 trick.
    (define-key map [follow-link] 'mouse-face)
    (define-key map [S-mouse-2] 'w3m-mouse-view-this-url-new-session)
    (if (featurep 'gtk)
	(progn
	  (define-key map [down-mouse-3] 'w3m-mouse-major-mode-menu)
	  (define-key map [drag-mouse-3] 'undefined)
	  ;; Don't use [mouse-3], which gets submenus not working in GTK Emacs.
	  (define-key map [mouse-3] 'undefined))
      (define-key map [mouse-3] 'w3m-mouse-major-mode-menu))
    (define-key map " " 'w3m-scroll-up-or-next-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "\C-ta" 'w3m-bookmark-add-all-urls)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "+" 'w3m-antenna-add-current-url)
    (define-key map "A" 'w3m-antenna)
    (define-key map "b" 'w3m-scroll-down-or-previous-url)
    (define-key map "!" 'w3m-redisplay-with-content-type)
    (define-key map "d" 'w3m-download)
    (define-key map "D" 'w3m-download-this-url)
    (define-key map "e" 'w3m-edit-current-url)
    (define-key map "E" 'w3m-edit-this-url)
    (define-key map "f" 'w3m-toggle-filtering)
    (define-key map "g" 'w3m-goto-url)
    (define-key map "G" 'w3m-goto-url-new-session)
    (define-key map "\C-tt" 'w3m-create-empty-session)
    (define-key map "h" 'describe-mode)
    (define-key map "H" 'w3m-gohome)
    (define-key map "i" (if (display-images-p)
			    'w3m-toggle-inline-image
			  'w3m-view-image))
    (define-key map "I" 'w3m-toggle-inline-images)
    (define-key map "\M-I" 'w3m-turnoff-inline-images)
    (when (display-images-p)
      (define-key map "\M-[" 'w3m-zoom-out-image)
      (define-key map "\M-]" 'w3m-zoom-in-image))
    (define-key map "\M-i" 'w3m-save-image)
    (define-key map "l" 'w3m-view-previous-page)
    (define-key map "\C-l" 'recenter)
    (define-key map [(control L)] 'w3m-reload-this-page)
    (define-key map [(control t) (control L)] 'w3m-reload-all-pages)
    (define-key map "M" 'w3m-view-url-with-browse-url)
    (define-key map "n" 'w3m-view-next-page)
    (define-key map "N" 'w3m-namazu)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "\M-k" 'w3m-cookie)
    (define-key map "\M-s" 'w3m-session-select)
    (define-key map "\M-S" 'w3m-session-save)
    (define-key map "o" 'w3m-history)
    (define-key map "O" 'w3m-db-history)
    (define-key map "p" 'w3m-view-previous-page)
    (define-key map "P" 'undecided) ;; reserved for print-this-buffer.
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "r" 'w3m-redisplay-this-page)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "\C-tR" 'w3m-reload-all-pages)
    (define-key map "s" 'w3m-search)
    (define-key map "S" 'w3m-search-new-session)
    (define-key map "T" 'w3m-dtree)
    (define-key map "u" 'w3m-view-parent-page)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "V" 'w3m-bookmark-view-new-session)
    (define-key map "W" 'w3m-weather)
    (define-key map "y" 'w3m-print-current-url)
    (define-key map "Y" 'w3m-print-this-url)
    (define-key map "=" 'w3m-view-header)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "?" 'describe-mode)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map [(shift right)] 'w3m-shift-left)
    (define-key map [(shift left)] 'w3m-shift-right)
    (define-key map "\M-l" 'w3m-horizontal-recenter)
    (define-key map "\C-a" 'w3m-beginning-of-line)
    (define-key map "\C-e" 'w3m-end-of-line)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "^" 'w3m-view-parent-page)
    (define-key map "]" 'w3m-next-form)
    (define-key map "[" 'w3m-previous-form)
    (define-key map "}" 'w3m-next-image)
    (define-key map "{" 'w3m-previous-image)
    (define-key map "|" 'w3m-pipe-source)
    (define-key map "\C-c" w3m-ctl-c-map)
    (define-key map "C" w3m-redisplay-map)
    (define-key map "L" w3m-lnum-map)
    (define-key map "\C-x\C-s" 'w3m-save-buffer)
    (setq w3m-info-like-map map)))

(defun w3m-alive-p (&optional visible)
  "Return a buffer in which emacs-w3m is running.
If there is no emacs-w3m session, return nil.  If the optional VISIBLE
is non-nil, a visible emacs-w3m buffer is preferred.  The last visited
emacs-w3m buffer is likely to return if VISIBLE is omitted or there is
no visible buffer."
  (let* ((buffers (w3m-list-buffers t))
	 (buf (car buffers)))
    (if visible
	(progn
	  (setq visible nil)
	  (while (and (not visible)
		      buffers)
	    (when (get-buffer-window (car buffers) t)
	      (setq visible (car buffers)))
	    (setq buffers (cdr buffers)))
	  (or visible buf))
      buf)))

(defun w3m-quit (&optional force)
  "Quit emacs-w3m.

Quit browsing immediately if the prefix argument FORCE is
specified, otherwise prompt you for the confirmation.

See also `w3m-close-window'.

This command updates the `arrived URLs' database."
  (interactive "P")
  (let ((buffers (w3m-list-buffers t))
	(all-buffers (let ((w3m-fb-mode nil))
		       (w3m-list-buffers t))))
    (if (or (= (length buffers) (length all-buffers))
	    (prog1 (y-or-n-p "Kill emacs-w3m buffers on other frames? ")
	      (message nil)))
	(let ((w3m-fb-mode nil))
	  (when (or force
		    (prog1 (y-or-n-p "Do you want to exit emacs-w3m? ")
		      (message nil)))
	    (w3m-session-automatic-save)
	    (w3m-delete-frames-and-windows)
	    (sit-for 0) ;; Delete frames seemingly fast.
	    (dolist (buffer all-buffers)
	      (w3m-cancel-refresh-timer buffer)
	      (kill-buffer buffer)
	      (when w3m-use-form
		(w3m-form-kill-buffer buffer)))
	    (when w3m-use-form
	      (w3m-form-textarea-file-cleanup))
	    (w3m-select-buffer-close-window)
	    (w3m-cache-shutdown)
	    (w3m-arrived-shutdown)
	    (w3m-process-shutdown)
	    (when w3m-use-cookies
	      (w3m-cookie-shutdown))
	    (w3m-kill-all-buffer)))
      ;;; starting here is condition of fb-mode active and in use
      (w3m-session-automatic-save)
      (let (buf)
	(while (setq buf (get-buffer " *w3m buffers*"))
	  (delete-windows-on buf t)
	  (kill-buffer buf)))
      (w3m-fb-delete-frame-buffers)
      (display-buffer-same-window (other-buffer) nil))
    (remove-hook 'window-configuration-change-hook
		 #'w3m-redisplay-pages-automatically)
    (when (= emacs-major-version 26)
      (remove-hook 'window-size-change-functions
		   #'w3m-redisplay-pages-automatically))))

(defun w3m-close-window ()
  "Bury emacs-w3m buffers and close windows and frames.

The buffers remain, and the sessions continue to exist, so you
can return to them by either running `M-x w3m' or by switching to
one of the buffers.

See also `w3m-quit'."
  (interactive)
  (w3m-history-store-position)
  ;; `w3m-list-buffers' won't return all the emacs-w3m buffers if
  ;; `w3m-fb-mode' is turned on.
  (let* ((buffers (w3m-list-buffers t))
	 (bufs buffers)
	 buf windows window)
    (w3m-delete-frames-and-windows)
    (while bufs
      (setq buf (pop bufs))
      (w3m-cancel-refresh-timer buf)
      (bury-buffer buf))
    (while buffers
      (setq buf (pop buffers)
	    windows (get-buffer-window-list buf 'no-minibuf t))
      (while windows
	(setq window (pop windows))
	(set-window-buffer
	 window
	 (other-buffer buf nil (window-frame window))))))
  (w3m-select-buffer-close-window)
  ;; The current-buffer and displayed buffer are not necessarily the
  ;; same at this point; if they aren't bury-buffer will be a nop, and
  ;; we will infloop.
  (set-buffer (window-buffer (selected-window)))
  (while (eq major-mode 'w3m-mode)
    (bury-buffer)))

(unless w3m-mode-map
  (setq w3m-mode-map
	(if (eq w3m-key-binding 'info)
	    w3m-info-like-map
	  w3m-lynx-like-map)))

(defun w3m-mouse-major-mode-menu (event)
  "Pop up a W3M mode-specific menu of mouse commands."
  (interactive "e")
  (mouse-set-point event)
  (let* ((bmkitems (if w3m-bookmark-mode
		       (cdr w3m-bookmark-menu-items)
		     (car w3m-bookmark-menu-items)))
	 (bmkmenu (if w3m-bookmark-menu-items-pre
		      `(,@bmkitems
			"----"
			,@w3m-bookmark-menu-items-pre)
		    bmkitems)))
    (run-hooks 'menu-bar-update-hook)
    (popup-menu (delete nil
			`(,@w3m-rmouse-menubar
			  "----"
			  "----"
			  ,w3m-menubar
			  "----"
			  ,(cons "Bookmark" bmkmenu)
			  ,(when w3m-tab-menubar-make-items-preitems
			     (cons "Tab" w3m-tab-menubar-make-items-preitems))
			  ,(cons "Session"
				 (if w3m-session-menu-items-pre
				     (append w3m-session-menu-items
					     '("----")
					     w3m-session-menu-items-pre)
				   w3m-session-menu-items))))
		event)))

(defvar w3m-tab-button-menu-current-buffer nil
  "Internal variable used by `w3m-tab-button-menu'.")

(defvar w3m-tab-button-menu-commands
  (let ((manyp '(cdr (w3m-list-buffers)))
	(currentp 'w3m-tab-button-menu-current-buffer)
	(leftp '(and w3m-tab-button-menu-current-buffer
		     (w3m-lefttab-exist-p w3m-tab-button-menu-current-buffer)))
	(rightp '(and w3m-tab-button-menu-current-buffer
		      (w3m-righttab-exist-p
		       w3m-tab-button-menu-current-buffer)))
	(many2p '(and w3m-tab-button-menu-current-buffer
		      (cdr (w3m-list-buffers)))))
    `((w3m-goto-url-new-session
       ,(w3m-make-menu-item "新しいタブ" "New Tab")
       t ,w3m-new-session-in-background w3m-new-session-url)
      (w3m-copy-buffer
       ,(w3m-make-menu-item "タブを複製" "Copy Tab")
       ,currentp ,w3m-new-session-in-background)
      -
      (w3m-reload-this-page
       ,(w3m-make-menu-item "タブを再読み込み" "Reload Tab")
       ,currentp)
      (w3m-reload-all-pages
       ,(w3m-make-menu-item "すべてのタブを再読み込み" "Reload All Tabs")
       ,manyp)
      -
      (w3m-delete-buffer
       ,(w3m-make-menu-item "このタブを閉じる" "Close This Tab")
       ,currentp)
      -
      (w3m-delete-other-buffers
       ,(w3m-make-menu-item "他のタブをすべて閉じる" "Close Other Tabs")
       ,many2p)
      (w3m-delete-left-tabs
       ,(w3m-make-menu-item "左側のタブをすべて閉じる" "Close Left Tabs")
       ,leftp)
      (w3m-delete-right-tabs
       ,(w3m-make-menu-item "右側のタブをすべて閉じる" "Close Right Tabs")
       ,rightp)
      -
      (w3m-view-url-with-browse-url
       ,(w3m-make-menu-item "browse-url で開く" "View with browse-url")
       ,currentp ,w3m-new-session-in-background w3m-current-url)
      -
      (w3m-session-save
       ,(w3m-make-menu-item "すべてのタブを保存する" "Save All Tabs...")
       t)
      (w3m-session-select
       ,(w3m-make-menu-item "タブリストを選択する" "Select List of Tabs...")
       t)
      (w3m-bookmark-add-current-url
       ,(w3m-make-menu-item "このタブをブックマーク" "Bookmark This Tab...")
       ,currentp ,w3m-new-session-in-background)
      (w3m-bookmark-add-all-urls
       ,(w3m-make-menu-item
	 "すべてのタブをブックマーク" "Bookmark All Tabs..." )
       ,manyp)))
  "List of commands invoked by the tab button menu.
Each item is the symbol `-' which is a separator,
or a list which consists of the following elements:

0: a function.
1: a function description.
2: a Lisp form which returns non-nil if the item is active.
3: a flag specifying whether the buffer should be selected.
&rest: arguments passed to the function.")

(easy-menu-define
  w3m-tab-button-menu w3m-tab-map "w3m tab button menu."
  (cons nil (w3m-make-menu-commands w3m-tab-button-menu-commands)))

;; This function must be placed after `easy-menu-define'.
(defun w3m-tab-button-menu (event buffer)
  (select-window (posn-window (event-start event)))
  (setq w3m-tab-button-menu-current-buffer buffer)
  (popup-menu w3m-tab-button-menu))

(defun w3m-tab-button-menu2 (event _buffer)
  (select-window (posn-window (event-start event)))
  (setq w3m-tab-button-menu-current-buffer nil)
  (popup-menu w3m-tab-button-menu))

(unless w3m-link-map
  (setq w3m-link-map (make-sparse-keymap))
  (if (featurep 'gtk)
      (progn
	(define-key w3m-link-map [down-mouse-3] 'w3m-link-menu)
	(define-key w3m-link-map [drag-mouse-3] 'undefined)
	;; Don't use [mouse-3], which gets submenus not working in GTK Emacs.
	(define-key w3m-link-map [mouse-3] 'undefined))
    (define-key w3m-link-map [mouse-3] 'w3m-link-menu)))

(easy-menu-define w3m-link-menu w3m-link-map "w3m link menu."
  `("Link" ;; This cannot be omitted for at least MacOS.
    [,(w3m-make-menu-item "リンクをこのセッションで開く"
			  "Open Link in This Session")
     w3m-view-this-url (w3m-anchor (point))]
    [,(w3m-make-menu-item "リンクを新しいセッションで開く"
			  "Open Link in New Session")
     w3m-view-this-url-new-session (w3m-anchor (point))]
    [,(w3m-make-menu-item "リンクを browse-url で開く"
			  "Open Link using browse-url")
     w3m-view-url-with-browse-url (w3m-anchor (point))]
    "-"
    [,(w3m-make-menu-item "このリンクをブックマーク..."
			  "Bookmark This Link...")
     w3m-bookmark-add-this-url (w3m-anchor (point))]
    [,(w3m-make-menu-item "名前を付けてリンク先を保存..."
			  "Save Link As...")
     w3m-download-this-url (w3m-anchor (point))]
    [,(w3m-make-menu-item "名前を付けて画像を保存..."
			  "Save Image As...")
     w3m-download-this-image (w3m-image (point))]
    [,(w3m-make-menu-item "リンクの URL をコピー"
			  "Copy Link Location")
     w3m-print-this-url (w3m-anchor (point))]
    [,(w3m-make-menu-item "画像の URL をコピー"
			  "Copy Image Location")
     w3m-print-this-image-url (w3m-image (point))]))

(defun w3m-link-menu (event)
  "Pop up a link menu."
  (interactive "e")
  (mouse-set-point event)
  (popup-menu w3m-link-menu))

(defvar w3m-buffer-unseen nil)
(make-variable-buffer-local 'w3m-buffer-unseen)

(defun w3m-set-buffer-unseen ()
  (setq w3m-buffer-unseen t)
  (add-hook 'pre-command-hook 'w3m-set-buffer-seen nil t))

(defun w3m-set-buffer-seen ()
  (setq w3m-buffer-unseen nil)
  (remove-hook 'pre-command-hook 'w3m-set-buffer-seen t))

(defun w3m-move-unseen-buffer ()
  "Move to the next unseen buffer."
  (interactive)
  (when (eq major-mode 'w3m-mode)
    (let* ((bufs (w3m-list-buffers))
	   (right (memq (current-buffer) bufs))
	   unseen)
      (setq unseen
	    (catch 'unseen
	      (dolist (buf (append right bufs))
		(when (w3m-unseen-buffer-p buf)
		  (throw 'unseen buf)))))
      (if (not unseen)
	  (message "No unseen buffer.")
	(switch-to-buffer unseen)
	(run-hooks 'w3m-select-buffer-hook)
	(w3m-select-buffer-update)))))

(eval-when-compile (require 'mwheel))

(defcustom w3m-redisplay-pages-automatically-p t
  "If non-nil, redisplay pages when some operation changes the page width."
  :group 'w3m
  :type 'boolean)

(defun w3m-mode ()
  "Major mode for browsing web.

\\<w3m-mode-map>\
\\[w3m-view-this-url]	Display the page pointed to by the link under point.
	You may use the prefix arg `2' or\
 `\\[universal-argument] \\<universal-argument-map>\
\\[universal-argument-more]\\<w3m-mode-map>' to make a new session.
\\[w3m-mouse-view-this-url]	Follow the link under the mouse pointer.
	If w3m-use-form is t, `\\[w3m-view-this-url]' and\
 `\\[w3m-mouse-view-this-url]' enable you to enter forms.
	You may use the prefix arg `2' or\
 `\\[universal-argument] \\<universal-argument-map>\
\\[universal-argument-more]\\<w3m-mode-map>' to make a new session.
\\[w3m-view-this-url-new-session]	Display the page of the link\
 in a new session.
	If the region is active, visit all the links within the region.
\\[w3m-mouse-view-this-url-new-session]	Display the page of the link\
 in a new session by mouse.

\\[w3m-submit-form]	Submit the form at point.

\\[w3m-reload-this-page]	Reload the current page.
\\[w3m-reload-all-pages]	Reload all the pages.
\\[w3m-redisplay-this-page]	Redisplay the current page.
\\[w3m-redisplay-with-content-type]	Redisplay the page, specifying\
 a content type.
\\[w3m-redisplay-with-charset]	Redisplay the current page, specifying\
 a charset.
\\[w3m-redisplay-and-reset]	Redisplay the current page and reset\
 the user-specified charset and\n\tcontent type.

\\[w3m-next-anchor]	Move the point to the next anchor.
\\[w3m-previous-anchor]	Move the point to the previous anchor.
\\[w3m-next-form]	Move the point to the next form.
\\[w3m-previous-form]	Move the point to the previous form.
\\[w3m-next-image]	Move the point to the next image.
\\[w3m-previous-image]	Move the point to the previous image.

\\[w3m-view-previous-page]	Move back to the previous page in the history.
\\[w3m-view-next-page]	Move forward to the next page in the history.
\\[w3m-view-parent-page]	Attempt to move to the parent directory of\
 the page.

\\[w3m-goto-url]	Visit the web page.
\\[w3m-goto-url-new-session]	Visit the web page in a new session.
\\[w3m-goto-new-session-url]	Open page of which url is specified by\
 `w3m-new-session-url' in\n\ta new session.
\\[w3m-gohome]	Go to the Home page.
\\[w3m-view-url-with-browse-url]	Open a link using `browse-url'.
\\[w3m-delete-left-tabs]	Delete tabs on the left side of\
 the current tab.
\\[w3m-delete-right-tabs]	Delete tabs on the right side of\
 the current tab.
\\[w3m-tab-move-left]	Move this tab N (default 1) times to the left.
\\[w3m-tab-move-right]	Move this tab N (default 1) times to the right.

\\[w3m-download]	Download the URL.
\\[w3m-download-this-url]	Download the URL under point.

\\[w3m-view-image]	Display the image under point in the external viewer.
\\[w3m-save-image]	Save the image under point to a file.
\\[w3m-toggle-inline-image]	Toggle the visibility of an image under point.
\\[w3m-toggle-inline-images]	Toggle the visibility of all images.
\\[w3m-turnoff-inline-images]	Turn off to display all images.
\\[w3m-zoom-out-image]	Zoom in an image on the point.
\\[w3m-zoom-in-image]	Zoom out an image on the point.

\\[w3m-print-this-url]	Display the url under point and put it into\
 `kill-ring'.
\\[w3m-print-current-url]	Display the url of the current page and put\
 it into `kill-ring'.

\\[w3m-view-source]	Display the html source of the current page.
\\[w3m-view-header]	Display the header of the current page.
\\[w3m-edit-current-url]	Edit the local file displayed as the current\
 page.
\\[w3m-edit-this-url]	Edit the local file which is pointed to by URL under\
 point.
\\[w3m-cookie]	Display cookies and enable you to manage them.

\\[w3m-scroll-up-or-next-url]	Scroll up the current window, or go to the\
 next page.
\\[w3m-scroll-down-or-previous-url]	Scroll down the current window, or\
 go to the previous page.
\\[w3m-scroll-up]	Scroll the current window up one line (or lines\
 of which the number\n\tyou specify by the prefix argument).
\\[w3m-scroll-left]	Scroll to the left.
\\[w3m-scroll-right]	Scroll to the right.
\\[w3m-shift-left]	Shift to the left.
\\[w3m-shift-right]	Shift to the right.
\\[w3m-horizontal-recenter]	Recenter horizontally.
\\[w3m-beginning-of-line]	Go to the entire beginning of line, may be\
 accompanied by scrolling.
\\[w3m-end-of-line]	Go to the entire end of line, may be accompanied\
 by scrolling.

\\[next-line]	Next line.
\\[previous-line]	Previous line.
\\[forward-char]	Forward char.
\\[backward-char]	Backward char.

\\[goto-line]	Go to the line, specifying the line number (beginning with 1).
\\[w3m-history-store-position]	Mark the current position.
\\[w3m-history-restore-position]	Go to the last marked position.

\\[w3m-history]	Display the history of pages you have visited in the\
 session.
	If it is called with the prefix arg, it displays the arrived URLs.
\\[w3m-antenna]	Display the report of changes in web pages.
	If it is called with the prefix arg, it updates the report.
\\[w3m-antenna-add-current-url]	Add the current url to the antenna database.
\\[w3m-search]	Query to the search engine a word.
	To change the server, give any prefix argument to the command.
\\[w3m-search-new-session] Query to the search engine a word in a new session.
	To change the server, give any prefix argument to the command.
\\[w3m-weather]	Display a weather report.
	To change the local area, give any prefix argument to the command.
\\[w3m-dtree]	Display a directory tree.
	If the prefix arg is given, display files in addition to directories.
\\[w3m-namazu]	Search files with Namazu.
	To change the index, give any prefix argument to the command.
\\[w3m-pipe-source]	Pipe a page source to a shell command.

\\[w3m-bookmark-view]	Display the bookmark.
\\[w3m-bookmark-view-new-session]	Display the bookmark on a new session.
\\[w3m-bookmark-add-current-url]	Add a url of the current page to\
 the bookmark.
	If the prefix arg is given, the user will be prompted for the url.
\\[w3m-bookmark-add-all-urls]	Add urls of all pages being visited to\
 the bookmark.
\\[w3m-bookmark-add-this-url]	Add the url under point to the bookmark.

\\[w3m-create-empty-session]	Create an empty page as a new session and\
 visit it.
\\[w3m-copy-buffer]	Create a copy of the current page as a new session.
\\[w3m-next-buffer]	Turn the page of emacs-w3m buffers ahead.
\\[w3m-previous-buffer]	Turn the page of emacs-w3m buffers behind.
\\[w3m-move-unseen-buffer]	Move to the next unseen buffer.
\\[w3m-select-buffer]	Pop to the emacs-w3m buffers selection window up.
\\[w3m-switch-buffer]	Select one of emacs-w3m buffers at the current window.
\\[w3m-delete-buffer]	Delete the current emacs-w3m buffer.
\\[w3m-delete-other-buffers]	Delete emacs-w3m buffers except for the\
 current buffer.

\\[w3m]	Start browsing web with emacs-w3m.
\\[w3m-close-window]	Close all emacs-w3m windows, without deleteing\
 buffers.
\\[w3m-quit]	Exit browsing web.  All emacs-w3m buffers will be deleted.
\\[w3m-process-stop]	Try to stop internal processes of a page.

\\[describe-mode]	describe-mode.
\\[w3m-mouse-major-mode-menu]	Pop up a w3m-mode specific menu of mouse\
 commands.

\\[report-emacs-w3m-bug]	Send a bug report to the emacs-w3m team.
"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'w3m-mode)
  (setq mode-name "w3m")
  (use-local-map w3m-mode-map)
  ;; Force paragraph direction to be left-to-right.
  (set (make-local-variable 'bidi-paragraph-direction) 'left-to-right)
  (set (make-local-variable 'nobreak-char-display) nil)
  (setq	truncate-lines t
	w3m-display-inline-images w3m-default-display-inline-images)
  (when w3m-auto-show
    (set (make-local-variable 'auto-hscroll-mode) nil))
  (setq show-trailing-whitespace nil)
  (set (make-local-variable 'mwheel-scroll-up-function) #'w3m-scroll-up)
  (set (make-local-variable 'mwheel-scroll-down-function) #'w3m-scroll-down)
  (setq w3m-last-window-width (window-width))
  (when w3m-redisplay-pages-automatically-p
    (add-hook 'window-configuration-change-hook
	      #'w3m-redisplay-pages-automatically)
    (when (= emacs-major-version 26)
      (add-hook 'window-size-change-functions
		#'w3m-redisplay-pages-automatically)))
  (w3m-setup-toolbar)
  (w3m-setup-menu)
  (run-hooks 'w3m-mode-setup-functions)
  (run-mode-hooks 'w3m-mode-hook))

(defun w3m-scroll-up (&optional arg interactive-p)
  "Scroll the current window up ARG lines.
When called interactively, ARG defaults to 1."
  (interactive (list current-prefix-arg t))
  (scroll-up (or arg (and interactive-p 1))))

(defun w3m-scroll-up-or-next-url (arg)
  "Scroll the current window up ARG lines, or go to the next page.
\"Next page\" means the page that the current page defines with a
\"Next page\" link.  Note that the `w3m-relationship-estimate-rules'
variable and the related functions find it, so the feature will not
always necessarily work.
When a page visits only an image, run `image-scroll-up' instead."
  (interactive "P")
  (if (w3m-image-page-displayed-p)
      (image-scroll-up arg)
    (if (and w3m-next-url
	     (pos-visible-in-window-p (let ((cur (point)))
					(goto-char (point-max))
					(skip-chars-backward "\t\n\r ")
					(forward-line 1)
					(prog1
					    (point)
					  (goto-char cur)))))
	(let ((w3m-prefer-cache t))
	  (w3m-history-store-position)
	  (w3m-goto-url w3m-next-url))
      (w3m-scroll-up arg))))

(defun w3m-scroll-down (&optional arg interactive-p)
  "Scroll the current window down ARG lines.
When called interactively, ARG defaults to 1."
  (interactive (list current-prefix-arg t))
  (scroll-down (or arg (and interactive-p 1))))

(defun w3m-scroll-down-or-previous-url (arg)
  "Scroll the current window down ARG lines, or go to the previous page.
\"Previous page\" means the page that the current page defines with a
\"Previous page\" link.  Note that the `w3m-relationship-estimate-rules'
variable and the related functions find it, so the feature will not
always necessarily work.
When a page visits only an image, run `image-scroll-down' instead."
  (interactive "P")
  (if (w3m-image-page-displayed-p)
      (image-scroll-down arg)
    (if (and w3m-previous-url
	     (pos-visible-in-window-p (point-min)))
	(let ((w3m-prefer-cache t))
	  (w3m-history-store-position)
	  (w3m-goto-url w3m-previous-url))
      (w3m-scroll-down arg))))

(defvar w3m-current-longest-line nil
  "The length of the longest line in the window.")

(defun w3m-set-current-longest-line ()
  "Set the value of `w3m-current-longest-line'."
  (save-excursion
    (goto-char (window-start))
    (end-of-line)
    (setq w3m-current-longest-line 0)
    (let ((end (window-end)))
      (while (progn
	       (skip-chars-backward " ")
	       (setq w3m-current-longest-line
		     (max w3m-current-longest-line (current-column)))
	       (end-of-line 2)
	       (< (point) end))))))

(defun w3m-scroll-left (arg)
  "Scroll to the left.
If ARG (the prefix) is a number, scroll the window ARG columns.
Otherwise, it defaults to `w3m-horizontal-scroll-columns'."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) w3m-horizontal-scroll-columns))
  (if (w3m-image-page-displayed-p)
      (image-forward-hscroll arg)
    (when (if (memq last-command '(w3m-scroll-left w3m-shift-left))
	      (or (< (window-hscroll) w3m-current-longest-line)
		  (progn (ding) nil))
	    (w3m-set-current-longest-line)
	    (< (window-hscroll) w3m-current-longest-line))
      (w3m-horizontal-scroll 'left arg))))

(defun w3m-scroll-right (arg)
  "Scroll to the right.
If ARG (the prefix) is a number, scroll the window ARG columns.
Otherwise, it defaults to `w3m-horizontal-scroll-columns'."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) w3m-horizontal-scroll-columns))
  (if (w3m-image-page-displayed-p)
      (image-backward-hscroll arg)
    (if (zerop (window-hscroll))
	(when (memq last-command '(w3m-scroll-right w3m-shift-right))
	  (ding))
      (w3m-horizontal-scroll 'right arg))))

(defun w3m-shift-left (arg)
  "Shift to the left.  Shift means a fine level horizontal scrolling.
If ARG (the prefix) is a number, scroll the window ARG columns.
Otherwise, it defaults to `w3m-horizontal-shift-columns'."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) w3m-horizontal-shift-columns))
  (if (w3m-image-page-displayed-p)
      (image-forward-hscroll arg)
    (when (if (memq last-command '(w3m-scroll-left w3m-shift-left))
	      (or (< (window-hscroll) w3m-current-longest-line)
		  (progn (ding) nil))
	    (w3m-set-current-longest-line)
	    (< (window-hscroll) w3m-current-longest-line))
      (w3m-horizontal-scroll 'left arg))))

(defun w3m-shift-right (arg)
  "Shift to the right.  Shift means a fine level horizontal scrolling.
If ARG (the prefix) is a number, scroll the window ARG columns.
Otherwise, it defaults to `w3m-horizontal-shift-columns'."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) w3m-horizontal-shift-columns))
  (if (w3m-image-page-displayed-p)
      (image-backward-hscroll arg)
    (if (zerop (window-hscroll))
	(when (memq last-command '(w3m-scroll-right w3m-shift-right))
	  (ding))
      (w3m-horizontal-scroll 'right arg))))

(defvar w3m-horizontal-scroll-done nil)
(make-variable-buffer-local 'w3m-horizontal-scroll-done)
(defvar w3m-current-position '(-1 0 0))
(make-variable-buffer-local 'w3m-current-position)

(defun w3m-auto-show ()
  "Scroll horizontally so that the point is visible."
  (when (and truncate-lines
	     w3m-auto-show
	     (not w3m-horizontal-scroll-done)
	     (not (and (eq last-command this-command)
		       (or (eq (point) (point-min))
			   (eq (point) (point-max)))))
	     (or (memq this-command '(beginning-of-buffer end-of-buffer))
		 (and (symbolp this-command)
		      (string-match "\\`i?search-" (symbol-name this-command)))
		 (and (markerp (nth 1 w3m-current-position))
		      (markerp (nth 2 w3m-current-position))
		      (>= (point)
			  (marker-position (nth 1 w3m-current-position)))
		      (<= (point)
			  (marker-position (nth 2 w3m-current-position))))))
    (w3m-horizontal-on-screen))
  (setq w3m-horizontal-scroll-done nil))

(defun w3m-horizontal-scroll (direction ncol)
  "Scroll the window NCOL columns horizontally to DIRECTION.
DIRECTON should be the symbol `left' which specifies to scroll to the
left, or any other Lisp object meaning to scroll to the right.  NCOL
should be a number.  This function is a subroutine called by the
commands `w3m-scroll-left', `w3m-scroll-right', `w3m-shift-left' and
`w3m-shift-right'."
  (setq w3m-horizontal-scroll-done t)
  (let ((inhibit-point-motion-hooks t))
    (set-window-hscroll nil
			(max 0
			     (+ (window-hscroll)
				(if (eq direction 'left) ncol (- ncol)))))
    (let ((hs (window-hscroll)))
      (unless (and (>= (- (current-column) hs) 0)
		   (< (- (current-column) hs) (window-width)))
	(move-to-column (if (eq direction 'left)
			    hs
			  (+ hs (window-width) -2)))))))

(defun w3m-horizontal-on-screen ()
  "Scroll the window horizontally so that the current position is visible.
See the documentation for the `w3m-horizontal-scroll-division' variable
for details."
  (when w3m-auto-show
    (setq w3m-horizontal-scroll-done t)
    (let ((cc (current-column))
	  (hs (window-hscroll))
	  (ww (window-width))
	  (inhibit-point-motion-hooks t))
      (unless (and (>= (- cc hs) 0)
		   (< (+ (- cc hs) (if (eolp) 0 2)) ww))
	(set-window-hscroll
	 nil
	 (max 0 (- cc (if (> hs cc)
			  (/ ww w3m-horizontal-scroll-division)
			(* (/ ww w3m-horizontal-scroll-division)
			   (1- w3m-horizontal-scroll-division))))))))))

(defun w3m-horizontal-recenter (&optional arg)
  "Recenter horizontally.  With ARG, put the point on the column ARG.
If `truncate-lines' is nil, it does nothing besides resetting the
window's hscroll."
  (interactive "P")
  (if truncate-lines
      (progn
	(cond ((< (current-column) (window-hscroll))
	       (move-to-column (window-hscroll))
	       (setq arg 0))
	      ((>= (current-column) (+ (window-hscroll) (window-width)))
	       (move-to-column (+ (window-hscroll) (window-width) -2))
	       (setq arg -1))
	      ((listp arg)
	       (setq arg (car arg))))
	(set-window-hscroll
	 nil
	 (if (numberp arg)
	     (if (>= arg 0)
		 (max (- (current-column) arg) 0)
	       (let* ((home (point))
		      (inhibit-point-motion-hooks t)
		      (maxcolumn (prog2
				     (end-of-line)
				     (1- (current-column))
				   (goto-char home))))
		 (max (min (- (current-column)
			      (window-width)
			      arg
			      -2)
			   maxcolumn)
		      0)))
	   (max (- (current-column) (/ (window-width) 2) -1)
		0))))
    (set-window-hscroll nil 0)))

(defun w3m-recenter ()
  "Recenter according to `w3m-view-recenter'."
  (when (and w3m-view-recenter
	     (eq (window-buffer) (current-buffer)))
    (recenter (if (eq t w3m-view-recenter)
		  '(4)  ;; per "C-u C-l" to recenter in middle
		w3m-view-recenter)))) ;; otherwise an integer

(defun w3m-beginning-of-line (&optional arg)
  "Make the beginning of the line visible and move the point to there."
  (interactive "P")
  (if (w3m-image-page-displayed-p)
      (image-bol (or arg 1))
    (when (listp arg)
      (setq arg (car arg)))
    (set-window-hscroll nil 0)
    (beginning-of-line arg)))

(defun w3m-end-of-line (&optional arg)
  "Move the point to the end of the line and scroll the window left.
It makes the ends of upper and lower three lines visible.  If
`truncate-lines' is nil, it works identically as `end-of-line'."
  (interactive "P")
  (if (w3m-image-page-displayed-p)
      (image-eol (or arg 1))
    (if truncate-lines
	(progn
	  (when (listp arg)
	    (setq arg (car arg)))
	  (forward-line (1- (or arg 1)))
	  (let ((inhibit-point-motion-hooks t)
		home)
	    (end-of-line)
	    (setq home (point)
		  arg (current-column))
	    (dolist (n '(-3 -2 -1 1 2 3))
	      (forward-line n)
	      (end-of-line)
	      (setq arg (max (current-column) arg))
	      (goto-char home)))
	  (setq temporary-goal-column arg
		this-command 'next-line)
	  (set-window-hscroll nil (max (- arg (window-width) -2) 0)))
      (set-window-hscroll nil 0)
      (end-of-line arg))))

(defun w3m-pattern-uri-replace (uri format)
  "Create a new uri from URI matched by last search according to FORMAT."
  (replace-match format nil nil uri))

(defun w3m-uri-replace (uri)
  "Return the converted URI according to `w3m-uri-replace-alist'."
  (catch 'found-replacement
    (dolist (elem w3m-uri-replace-alist uri)
      (when (string-match (car elem) uri)
	(throw 'found-replacement
	       (cond
		((consp (cdr elem))
		 (apply (cadr elem) uri (cddr elem)))
		;; Rest conditions are inserted in order to keep
		;; backward compatibility.
		((functionp (cdr elem))
		 (funcall (cdr elem) uri))
		((stringp (cdr elem))
		 (w3m-pattern-uri-replace uri (cdr elem)))))))))

(defun w3m--goto-torrent-url (url)
  "Process `.torrent' links and `magnet:' protocol URLs.

This handler is currently hard-coded to require the external command-
line programs `transmission-daemon' and `transmission-remote', and to
recommend the external NCURSES program `transmission-remote-cli'.

The `transmission-daemon' program initiates a web interface on
`http://localhost:9091' from which one may view and manipulate
torrents; however, that interface requires javascript, so is
unavailable directly via `emacs-w3m'.  An alternative NCURSES
interface is available using `transmission-remote-cli', so if that
external program is available, this function concludes by starting
that external program in a dedicated `ansi-term' buffer, if one does
not already exist."
  ;; TODO: * don't hard-code for `transmission'
  ;;       * investigate options for using `deluge', `ktorrent', or others.
  (if (not (and (executable-find "transmission-daemon")
		(executable-find "transmission-remote")))
      (w3m-message "Missing executable for processing torrents.")
    (when url ;; sanity check
      (when (not (zerop (shell-command "pgrep -f transmission-daemon")))
	(shell-command (concat "transmission-daemon -w "
			       w3m-default-save-directory))
	(sit-for 1))
      (shell-command (concat "transmission-remote -a " url))
      (let ((buf-name "w3m-torrents")
	    (cmd      "transmission-remote-cli"))
	(when (and (executable-find cmd)
		   (not (get-buffer (concat "*" buf-name "*"))))
	  (ansi-term cmd buf-name)
	  (set-process-sentinel
	   (get-buffer-process (current-buffer))
	   (lambda (_process event)
	     (when (string-match "finished" event)
	       (kill-buffer))))
	  (bury-buffer))))))

(defun w3m-goto-mailto-url (url &optional post-data)
  (let ((before (nreverse (buffer-list)))
	comp info body buffers buffer function)
    (setq url (replace-regexp-in-string
	       "\n" "%0a" ;; `rfc2368-parse-mailto-url' replaces it with "\n".
	       (replace-regexp-in-string
		"\\(?:\r\\|%0[Dd]\\)+" ""
		(w3m-url-readable-string url))))
    (save-window-excursion
      (if (functionp w3m-mailto-url-function)
	  (funcall w3m-mailto-url-function url)
	(unless (and (setq comp (get mail-user-agent 'composefunc))
		     (functionp comp))
	  (error "No mail composing function for `%s'" mail-user-agent))
	(require 'rfc2368)
	(setq info (rfc2368-parse-mailto-url url)
	      body (assoc "Body" info)
	      info (delq body info)
	      body (delq nil (list (cdr body))))
	(when post-data
	  (setq body (nconc body (list (if (consp post-data)
					   (car post-data)
					 post-data)))))
	(apply comp
	       (append (mapcar
			(lambda (x)
			  (prog1
			      (cdr (assoc x info))
			    (setq info (delq (assoc x info) info))))
			'("To" "Subject"))
		       (list info)))))
    (setq buffers (nreverse (buffer-list)))
    (save-current-buffer
      (while buffers
	(setq buffer (car buffers)
	      buffers (cdr buffers))
	(unless (memq buffer before)
	  (set-buffer buffer)
	  (when (setq function
		      (cdr (assq major-mode
				 w3m-mailto-url-popup-function-alist)))
	    (setq buffers nil)))))
    (when function
      (let (same-window-buffer-names same-window-regexps mod)
	(let (display-buffer-alist)
	  (funcall function buffer))
	(when body
	  (setq mod (buffer-modified-p))
	  (goto-char (point-min))
	  (search-forward (concat "\n" (regexp-quote mail-header-separator)
				  "\n")
			  nil 'move)
	  (unless (bolp) (insert "\n"))
	  (while body
	    (insert (pop body))
	    (unless (bolp) (insert "\n")))
	  (set-buffer-modified-p mod))))))

(defun w3m-convert-ftp-url-for-emacsen (url)
  (or (and (string-match "\\`ftp://?\\([^/@]+@\\)?\\([^/]+\\)\\(?:/~/\\)?" url)
	   (concat "/"
		   (if (match-beginning 1)
		       (substring url (match-beginning 1) (match-end 1))
		     "anonymous@")
		   (substring url (match-beginning 2) (match-end 2))
		   ":"
		   (substring url (match-end 2))))
      (error "URL is strange")))

(defun w3m-file-directory-p (file)
  "Emulate the `file-directory-p' function for the remote file FILE."
  (when (file-exists-p file)
    (let (dirp (i 10))
      (catch 'loop
	(while (> i 0)
	  (setq dirp (car (file-attributes file)))
	  (if (stringp dirp)
	      (setq file (expand-file-name
			  dirp
			  (file-name-directory (directory-file-name file)))
		    i (1- i))
	    (throw 'loop dirp)))))))

(defun w3m-goto-ftp-url (url &optional filename)
  "Copy a remote file to the local system or run dired for ftp URLs.
If URL looks like a file, it will perform the copy.  Otherwise, it
will run `dired-other-window' using `ange-ftp' or `efs'.  Optional
FILENAME specifies the name of a local file.  If FILENAME is omitted,
this function will prompt user for it."
  (let ((ftp (w3m-convert-ftp-url-for-emacsen url))
	file)
    (if (or (string-equal "/" (substring ftp -1))
	    ;; `file-directory-p' takes a long time for remote files.
	    ;; `file-directory-p' returns t in Emacsen, anytime.
	    (w3m-file-directory-p ftp))
	(dired-other-window ftp)
      (setq file (file-name-nondirectory ftp))
      (unless filename
	(setq filename (w3m-read-file-name nil nil file)))
      (unless (file-writable-p (file-name-directory filename))
	(error "Permission denied, %s" (file-name-directory filename)))
      (when (or (not (file-exists-p filename))
		(if (file-writable-p filename)
		    (and (prog1
			     (y-or-n-p
			      (format "File(%s) already exists. Overwrite? "
				      filename))
			   (message nil))
			 (progn
			   (delete-file filename)
			   t))
		  (error "Permission denied, %s" filename)))
	(copy-file ftp filename)
	(message "Wrote %s" filename)))))

(unless w3m-doc-view-map
  (setq w3m-doc-view-map (make-sparse-keymap))
  (define-key w3m-doc-view-map "q" 'w3m-doc-view-quit))

(defun w3m-doc-view (url)
  "View PDF/PostScript/DVI files using `doc-view-mode'.

Where the document is displayed depends upon the `w3m-display-mode'."
  (let* ((basename (file-name-nondirectory (w3m-url-strip-query url)))
	 (regexp (concat "\\`" (regexp-quote basename) "\\(?:<[0-9]+>\\)?\\'"))
	 (buffers (buffer-list))
	 buffer data case-fold-search)
    (save-current-buffer
      (while buffers
	(setq buffer (pop buffers))
	(if (and (string-match regexp (buffer-name buffer))
		 (progn
		   (set-buffer buffer)
		   (eq major-mode 'doc-view-mode))
		 (equal buffer-file-name url))
	    (setq buffers nil)
	  (setq buffer nil))))
    (unless (prog1
		buffer
	      (unless buffer
		(setq buffer (generate-new-buffer basename)
		      data (buffer-string)))
	      (let ((pop-up-windows w3m-pop-up-windows)
		    (pop-up-frames w3m-pop-up-frames))
		(pop-to-buffer buffer)))
      (set-buffer-multibyte nil)
      (insert data)
      (set-buffer-modified-p nil)
      (setq buffer-file-name url)
      (doc-view-mode)
      (use-local-map w3m-doc-view-map)
      (set-keymap-parent w3m-doc-view-map doc-view-mode-map)
      'internal-view)))

(defun w3m-doc-view-quit (&optional kill)
  "Quit the `doc-view-mode' window that emacs-w3m launches.
With the prefix argument KILL, kill the buffer."
  (interactive "P")
  (cond (w3m-pop-up-frames
	 (when (prog1 (one-window-p t) (quit-window kill))
	   (delete-frame (selected-frame))))
	(w3m-pop-up-windows
	 (quit-window kill)
	 (unless (eq (next-window nil 'no-mini) (selected-window))
	   (delete-window)))))

(defun w3m-store-current-position ()
  "Memorize the current positions whenever every command starts.
The value will be held in the `w3m-current-position' variable.  This
function is designed as the hook function which is registered to
`pre-command-hook' by `w3m-buffer-setup'."
  (setq w3m-current-position (list (point)
				   (copy-marker (point-at-bol))
				   (copy-marker (point-at-eol)))))

(defun w3m-check-current-position ()
  "Run `w3m-after-cursor-move-hook' after invoking a command.
This function is designed as a hook function which is registered to
`post-command-hook' by `w3m-buffer-setup' for showing some infomations
when the command moves the point."
  (when (/= (point) (car w3m-current-position))
    ;; To bind `deactivate-mark' to nil protects the mark from being
    ;; deactivated.  `deactivate-mark' is set when any function modifies
    ;; a buffer, and it causes the deactivation of the mark.
    (let ((deactivate-mark nil))
      (run-hooks 'w3m-after-cursor-move-hook))))

(defun w3m-buffer-setup ()
  "Generate a new buffer, select it and set it up for emacs-w3m.
When the current buffer has already been prepared, it won't bother to
generate a new buffer."
  (unless (eq major-mode 'w3m-mode)
    (let ((buffer (w3m-alive-p t)))
      (set-buffer (or buffer (w3m-generate-new-buffer "*w3m*")))))
  ;; It may have been set to nil for viewing a page source or a header.
  (setq truncate-lines t)
  (add-hook 'pre-command-hook 'w3m-store-current-position nil t)
  (add-hook 'post-command-hook 'w3m-check-current-position nil t)
  (w3m-initialize-graphic-icons)
  (setq mode-line-buffer-identification
	`(,@(propertized-buffer-identification "%b") " "
	  (w3m-current-process
	   w3m-modeline-process-status-on
	   (w3m-current-ssl
	    (w3m-display-inline-images
	     w3m-modeline-ssl-image-status-on
	     w3m-modeline-ssl-status-off)
	    (w3m-display-inline-images
	     w3m-modeline-image-status-on
	     w3m-modeline-status-off)))
	  (w3m-show-graphic-icons-in-mode-line
	   (w3m-use-favicon
	    (w3m-favicon-image
	     w3m-modeline-favicon
	     w3m-modeline-separator)
	    w3m-modeline-separator)
	   w3m-modeline-separator)
	  (w3m-current-process
	   (:eval (if w3m-process-modeline-string
		      (concat
		       "Downloading "
		       (mapconcat #'cdr w3m-process-modeline-string " "))
		    "Loading..."))
	   (:eval (w3m-modeline-title))))))

(defvar w3m-modeline-title-string nil
  "Internal variable used to keep contents to be shown in the mode line.
This is a buffer-local variable.")
(make-variable-buffer-local 'w3m-modeline-title-string)

(defvar w3m-modeline-title-timer nil
  "Say time has not gone by after the mode line was updated last time.
It is used to control the `w3m-modeline-title' function running too
frequently, set by the function itself and cleared by a timer.")
(make-variable-buffer-local 'w3m-modeline-title-timer)

(defun w3m-modeline-title ()
  "Return a truncated title not to cut the right end of the mode line."
  (if w3m-use-title-buffer-name
      ""
    (when w3m-current-title
      (or (and w3m-modeline-title-timer w3m-modeline-title-string)
	  (prog2
	      (setq w3m-modeline-title-string w3m-current-title
		    w3m-modeline-title-timer t)
	      (let ((excess (- (string-width
				(condition-case nil
				    (format-mode-line mode-line-format 1)
				  (error "")))
			       (window-width)))
		    (tlen (string-width w3m-current-title)))
		(when (and (> excess 0)
			   (> tlen 3))
		  (setq w3m-modeline-title-string
			(concat (replace-regexp-in-string
				 "[\t ]+\\'" ""
				 (truncate-string-to-width
				  w3m-current-title (max (- tlen excess 3) 2)))
				"...")))
		w3m-modeline-title-string)
	    (run-at-time 0.5 nil
			 (lambda (buffer)
			   (when (buffer-live-p buffer)
			     (with-current-buffer buffer
			       (setq w3m-modeline-title-timer nil))))
			 (current-buffer)))))))

(defun w3m--buffer-busy-error ()
  "Uniform error handling for conition of a busy buffer.

Although operations are asynchronous, it makes sense that only
one GET operation can be performed at any one time in any single
buffer, so if the user tries to perform a second operation, a
helpful message is presented and the operation is aborted."
  (when w3m-current-process
    (error "%s"
	   (substitute-command-keys "This buffer is currently busy.
 `\\<w3m-mode-map>\\[w3m-process-stop]' to abort current operation,
 `\\<w3m-mode-map>\\[w3m-search-new-session]' to perform a search in a new buffer.
 `\\<w3m-mode-map>\\[w3m-goto-url-new-session]' to visit a URL in a new buffer."))))

(defun w3m--goto-url--handler-function (url reload _charset post-data referer
					    redisplay name reuse-history action
					    orig history-position)
  (with-current-buffer w3m-current-buffer
    (setq w3m-current-process nil)
    (if (not action)
	(w3m-history-push w3m-current-url
			  (list :title (or w3m-current-title "<no-title>")))
      (w3m-string-match-url-components w3m-current-url)
      (and (match-beginning 8)
	   (setq name (match-string 9 w3m-current-url)))
      (when (and name
		 (w3m-search-name-anchor name nil
					 (not (eq action 'cursor-moved))))
	(setf (w3m-arrived-time (w3m-url-strip-authinfo orig))
	      (w3m-arrived-time url))
	(unless (or w3m-message-silent
		    (not (eq this-command 'w3m-view-this-url-new-session))
		    (get-buffer-window w3m-current-buffer (selected-frame)))
	  (when (string-match "\\*w3m\\*<[0-9]+>\\'"
			      (setq name (buffer-name w3m-current-buffer)))
	    (setq name (match-string 0 name)))
	  (w3m-message "The content (%s) has been retrieved in %s" url name)
	  ;; Make the message sure to be shown for a while.
	  (run-at-time
	   0.5 nil
	   (lambda (orig) (setq w3m-after-cursor-move-hook orig))
	   (prog1
	       w3m-after-cursor-move-hook
	     (setq w3m-after-cursor-move-hook nil)))))
      (unless (eq action 'cursor-moved)
	(if (equal referer "about://history/")
	    ;; Don't sprout a new branch for
	    ;; the existing history element.
	    (let ((w3m-history-reuse-history-elements t))
	      (w3m-history-push w3m-current-url
				(list :title w3m-current-title))
	      ;; Fix the history position pointers.
	      (when history-position
		(setcar w3m-history
			(w3m-history-regenerate-pointers history-position))))
	  (let ((w3m-history-reuse-history-elements reuse-history)
		(position (when (eq 'reload reuse-history)
			    (cadar w3m-history))))
	    (w3m-history-push w3m-current-url (list :title w3m-current-title))
	    (when position
	      (w3m-history-set-current position))))
	(w3m-history-add-properties (list :referer referer
					  :post-data post-data))
	(unless w3m-toggle-inline-images-permanently
	  (setq w3m-display-inline-images w3m-default-display-inline-images))
	(when (and w3m-use-form reload)
	  (w3m-form-textarea-files-remove))
	(cond ((w3m-display-inline-images-p)
	       (and w3m-force-redisplay (sit-for 0))
	       (w3m-toggle-inline-images 'force reload))
	      ((and (display-images-p) (eq action 'image-page))
	       (and w3m-force-redisplay (sit-for 0))
	       (w3m-toggle-inline-image 'force reload)))))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (setq list-buffers-directory w3m-current-title)
    ;; must be `w3m-current-url'
    (setq default-directory (w3m-current-directory w3m-current-url))
    (w3m-buffer-name-add-title)
    (let ((real-url (if (w3m-arrived-p url)
			(or (w3m-real-url url) url)
		      url)))
      (run-hook-with-args 'w3m-display-functions real-url)
      (run-hook-with-args 'w3m-display-hook real-url))
    (w3m-select-buffer-update)
    (w3m-session-crash-recovery-save)
    (and w3m-current-url
	 (stringp w3m-current-url)
	 (or (string-match "\\`about://\\(?:header\\|source\\)/"
			   w3m-current-url)
	     (equal (w3m-content-type w3m-current-url) "text/plain"))
	 (setq truncate-lines nil))
    ;; restore position must call after hooks for localcgi.
    (when (or reload redisplay)
      (w3m-history-restore-position))
    (unless redisplay (w3m-set-buffer-unseen))
    (w3m-refresh-at-time)))

(defun w3m--goto-url--valid-url (url reload charset post-data referer handler
				     element background save-pos)
  "Main function called by `w3m-goto-url' for handling generic URLS."
  (setq url (w3m--url-strip-queries url))
  (w3m-buffer-setup)			; Setup buffer.
  (w3m-arrived-setup)			; Setup arrived database.
  (unless background
    (w3m-popup-buffer (current-buffer)))
  (w3m-cancel-refresh-timer (current-buffer))
  (w3m--buffer-busy-error)
  (w3m-process-stop (current-buffer))	; Stop all processes retrieving images.
  (w3m-idle-images-show-unqueue (current-buffer))
  ;; Store the current position in the history structure if SAVE-POS
  ;; is set or `w3m-goto-url' is called interactively.
  (when (or save-pos (w3m-interactive-p))
    (w3m-history-store-position))
  ;; Access url group
  (if (string-match "\\`group:" url)
      (let ((urls (mapcar 'w3m-url-decode-string
			  (split-string (substring url (match-end 0)) "&")))
	    buffers)
	(w3m-process-do
	    (type (save-window-excursion
		    (prog1
			(w3m-goto-url (pop urls))
		      (dotimes (_i (length urls))
			(push (w3m-copy-buffer nil nil nil 'empty t)
			      buffers))
		      (dolist (url (nreverse urls))
			(with-current-buffer (pop buffers)
			  (w3m-goto-url url))))))
	  type))
    ;; Retrieve the page.
    (let ((redisplay (eq reload 'redisplay))
	  (name)
	  (history-position (get-text-property (point)
					       'history-position))
	  (reuse-history w3m-history-reuse-history-elements))
      (setq url (w3m-url-strip-authinfo url)
	    reload (and (not redisplay) reload))
      (when w3m-current-forms
	;; Store the current forms in the history structure.
	(w3m-history-plist-put :forms w3m-current-forms))
      (let ((w3m-current-buffer (current-buffer)))
	(unless element
	  (setq element	(if (and (equal referer "about://history/")
				 history-position)
			    (w3m-history-element history-position t)
			  (if w3m-history-reuse-history-elements
			      (w3m-history-assoc url)))))
	;; Set current forms using the history structure.
	(when (setq w3m-current-forms
		    (when (and
			   ;; If reloading, ignore history.
			   (not reload)
			   ;; If post, ignore history.
			   (null post-data)
			   (or (w3m-cache-available-p url)
			       (w3m-url-local-p url)))
		      ;; Don't use `w3m-history-plist-get' here.
		      (plist-get (nthcdr 3 element) :forms)))
	  ;; Mark that the form is from history structure.
	  (setq w3m-current-forms (cons t w3m-current-forms)))
	(when (and post-data element)
	  ;; Remove processing url's forms from
	  ;; the history structure.
	  (w3m-history-set-plist (cadr element) :forms nil))
	;; local directory URL check
	(when (and (w3m-url-local-p url)
		   (file-directory-p (w3m-url-to-file-name url))
		   (setq url (file-name-as-directory url))
		   (eq w3m-local-directory-view-method 'w3m-dtree)
		   (string-match "\\`file:///" url))
	  (setq url (replace-match "about://dtree/" nil nil url)))
	;; Split body and fragments.
	(w3m-string-match-url-components url)
	(and (match-beginning 8)
	     (setq name (match-string 9 url)
		   url (substring url 0 (match-beginning 8))))
	(when (w3m-url-local-p url)
	  (unless (string-match "[^\000-\177]" url)
	    (setq url (w3m-url-decode-string url))))
	(w3m-process-do
	    (action
	     (if (and (not reload)
		      (not redisplay)
		      (stringp w3m-current-url)
		      (string= url w3m-current-url))
		 (progn
		   (w3m-refontify-anchor)
		   'cursor-moved)
	       (when w3m-name-anchor-from-hist
		 (w3m-history-plist-put
		  :name-anchor-hist
		  (append (list 1 nil)
			  (and (integerp (car w3m-name-anchor-from-hist))
			       (nthcdr (1+ (car w3m-name-anchor-from-hist))
				       w3m-name-anchor-from-hist)))))
	       (setq w3m-name-anchor-from-hist
		     (plist-get (nthcdr 3 element) :name-anchor-hist))
	       (let ((proc (w3m-retrieve-and-render
			    url reload charset post-data referer handler)))
		 (push proc w3m-current-process)
		 proc)))
	  (w3m--goto-url--handler-function
	   url reload charset post-data referer redisplay name reuse-history
	   action url history-position))))))

;;;###autoload
(defun w3m-goto-url (url &optional reload charset post-data referer handler
			 element background save-pos)
  "Visit World Wide Web pages in the current buffer.

This is the primitive function of `w3m'.

If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.
If it is a string, it makes this function request a body as if
the content-type is \"x-www-form-urlencoded\".  If it is a cons cell,
the car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], BACKGROUND, and SAVE-POS[2] are for
the internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.
See the `w3m-search' function and the variable `w3m-uri-replace-alist'.

Notes for the developers:
[1] ELEMENT is a history element which has already been registered in
the `w3m-history-flat' variable.  It is corresponding to URL to be
retrieved at this time, not for the url of the current page.

[2] SAVE-POS leads this function to save the current emacs-w3m window
configuration; i.e. to run `w3m-history-store-position'.
`w3m-history-store-position' should be called in a w3m-mode buffer, so
this will be convenient if a command that calls this function may be
invoked in other than a w3m-mode buffer."
  (interactive
   (list (unless (w3m--buffer-busy-error)
	   (w3m-input-url "Open URL in current buffer" nil nil nil
			  'feeling-searchy 'no-initial))
	 current-prefix-arg coding-system-for-read))
  (when (not post-data)
    (require 'w3m-search)
    (let* ((uri-replace ; based upon: `w3m-uri-replace'
	    (catch 'found-replacement
	      (dolist (elem w3m-uri-replace-alist)
		(when (string-match (car elem) url)
		  (throw 'found-replacement  elem)))))
	   (query (when uri-replace (substring url (match-end 0))))
	   engine-info)
      (when (and uri-replace query
		 (eq (nth 1 uri-replace) 'w3m-search-uri-replace))
	;; based upon `w3m-search-uri-replace' and `w3m-search-do-search'
	(setq engine-info (assoc (nth 2 uri-replace) w3m-search-engine-alist))
	(when (and engine-info (< 3 (length engine-info)))
	  (setq post-data
		(format (nth 3 engine-info)
			(w3m-search-escape-query-string query
							(nth 2 engine-info))))
	  (setq reload t)))))
  (setq url (w3m-canonicalize-url url))
  (set-text-properties 0 (length url) nil url)
  (unless (or (w3m-url-local-p url)
	      (string-match "\\`about:" url)
	      (let ((case-fold-search t))
		(string-match "\\`mailto:" url)))
    (w3m-string-match-url-components url)
    (setq url (concat (save-match-data
			(w3m-url-transfer-encode-string
			 (substring url 0 (match-beginning 8))))
		      (if (match-beginning 8)
			  (concat "#" (match-string 9 url))
			""))))
  (cond
   ;; process mailto: protocol
   ((string-match "\\`mailto:" url)
    (w3m-goto-mailto-url url post-data))
   ;; process torrents and their magnets
   ((or (string-match "\\`magnet:" url)
	(string-match "\\.torrent\\'" url))
    (w3m--goto-torrent-url url))
   ;; process ftp: protocol
   ((and w3m-use-ange-ftp
	 (string-match "\\`ftps?://" url)
	 (not (string= "text/html" (w3m-local-content-type url))))
    (w3m-goto-ftp-url url))
   ;; find-file directly
   ((condition-case nil
	(and (w3m-url-local-p url)
	     w3m-local-find-file-function
	     (let ((base-url (w3m-url-strip-fragment url))
		   (match (car w3m-local-find-file-regexps))
		   nomatch file)
	       (and (or (not match)
			(string-match match base-url))
		    (not (and (setq nomatch (cdr w3m-local-find-file-regexps))
			      (string-match nomatch base-url)))
		    (setq file (w3m-url-to-file-name base-url))
		    (file-exists-p file)
		    (not (file-directory-p file))
		    (prog1
			t
		      (funcall (if (functionp w3m-local-find-file-function)
				   w3m-local-find-file-function
				 (eval w3m-local-find-file-function))
			       file)))))
      (error nil)))
   ;; process buffer-local url
   ((w3m-buffer-local-url-p url)
    (let (file-part fragment-part)
      (w3m-string-match-url-components url)
      (setq file-part (concat (match-string 4 url)
			      (match-string 5 url))
	    fragment-part (match-string 9 url))
      (cond
       ((and (string= file-part "") fragment-part)
	(w3m-search-name-anchor fragment-part))
       ((not (string= file-part ""))
	(w3m-goto-url
	 (w3m-expand-url (substring url (match-beginning 4))
			 (concat "file://" default-directory))
	 reload charset post-data referer handler element background))
       (t (w3m-message "No URL at point")))))
   ((w3m-url-valid url)
    (w3m--goto-url--valid-url url reload charset post-data referer handler
			      element background save-pos))
   (t (w3m-message "Invalid URL: %s" url))))

(defun w3m-current-directory (url)
  "Return a directory used as the current directory in a page visiting URL.
See `w3m-default-directory'."
  (or (and url
	   (stringp url)
	   (let (file)
	     (if (string-match "\\`ftp://" url)
		 (progn
		   (setq file (w3m-convert-ftp-url-for-emacsen url))
		   (file-name-as-directory
		    (if (string-match "/\\`" file)
			file
		      (file-name-directory file))))
	       (and (setq file (w3m-url-to-file-name url))
		    (file-exists-p file)
		    (file-name-as-directory
		     (if (file-directory-p file)
			 file
		       (file-name-directory file)))))))
      (let (directory)
	(file-name-as-directory
	 (or (and (stringp w3m-default-directory)
		  (file-directory-p w3m-default-directory)
		  (expand-file-name w3m-default-directory))
	     (and (symbolp w3m-default-directory)
		  (boundp w3m-default-directory)
		  (setq directory (symbol-value w3m-default-directory))
		  (stringp directory)
		  (file-directory-p directory)
		  (expand-file-name directory))
	     (and (functionp w3m-default-directory)
		  (stringp (setq directory
				 (condition-case nil
				     (funcall w3m-default-directory url)
				   (error nil))))
		  (file-directory-p directory)
		  (expand-file-name directory))
	     w3m-profile-directory)))))

(defun w3m-refresh-at-time ()
  (when (and w3m-use-refresh w3m-current-refresh)
    (let ((seconds (car w3m-current-refresh))
	  (url (cdr w3m-current-refresh)))
      (setq seconds (max seconds w3m-refresh-minimum-interval))
      (if (= seconds 0)
	  (w3m-goto-url-with-timer url (current-buffer))
	(setq w3m-refresh-timer
	      (run-at-time seconds nil 'w3m-goto-url-with-timer url
			   (current-buffer)))))))

(defun w3m-goto-url-with-timer (url buffer)
  "Run the `w3m-goto-url' function by the refresh timer."
  (when (and (w3m-url-valid url) buffer
	     (if (stringp buffer) (get-buffer buffer) (buffer-name buffer)))
    (cond
     ((get-buffer-window buffer)
      (save-selected-window
	(pop-to-buffer buffer)
	(with-current-buffer buffer
	  (w3m-cancel-refresh-timer buffer)
	  (if (and w3m-current-url
		   (string= url w3m-current-url))
	      (w3m-reload-this-page t)
	    (w3m-goto-url url)))))
     ((buffer-live-p buffer)
      (let* ((cwin (selected-window))
	     (cbuf (window-buffer cwin)))
	(with-current-buffer buffer
	  (w3m-cancel-refresh-timer buffer)
	  (if (and w3m-current-url
		   (string= url w3m-current-url))
	      (w3m-reload-this-page t t)
	    (w3m-goto-url url nil
			  nil nil nil nil nil t)))
	(set-window-buffer cwin cbuf)))
     (t
      (with-current-buffer buffer
	(w3m-cancel-refresh-timer buffer))))))

(defun w3m-goto-new-session-url (&optional reload)
  "Open `w3m-new-session-url' in a new session."
  (interactive "P")
  (if (not (eq major-mode 'w3m-mode))
      (message "This command can be used in w3m mode only")
    (w3m-goto-url-new-session w3m-new-session-url reload)))

;;;###autoload
(defun w3m-goto-url-new-session (url &optional reload charset post-data
				     referer background)
  "Visit World Wide Web pages in a new buffer.
Open a new tab if you use tabs, i.e., `w3m-display-mode' is set to
`tabbed' or `w3m-use-tab' is set to a non-nil value.

The buffer will get visible if BACKGROUND is nil or there is no other
emacs-w3m buffer regardless of BACKGROUND, otherwise (BACKGROUND is
non-nil) the buffer will be created but not appear to be visible.
BACKGROUND defaults to the value of `w3m-new-session-in-background',
but it could be inverted if called interactively with the prefix arg."
  (interactive
   (list (w3m-input-url "Open URL in new buffer" nil
			(or (w3m-active-region-or-url-at-point)
			    w3m-new-session-url)
			nil 'feeling-searchy 'no-initial)
	 nil ;; reload
	 coding-system-for-read
	 nil ;; post-data
	 nil ;; referer
	 nil)) ;; background
  (setq background (when (let (w3m-fb-mode) (ignore w3m-fb-mode)
			      (w3m-list-buffers t))
		     (if (w3m-interactive-p)
			 (if current-prefix-arg
			     (not w3m-new-session-in-background)
			   w3m-new-session-in-background)
		       (or background w3m-new-session-in-background))))
  (let (buffer)
    (if (or (eq 'w3m-mode major-mode)
	    (and (setq buffer (w3m-alive-p))
		 (progn (w3m-popup-buffer buffer) t)))
	(progn
	  (w3m-history-store-position)
	  (setq buffer (w3m-copy-buffer nil "*w3m*" background 'empty t)))
      (setq buffer (w3m-generate-new-buffer "*w3m*")))
    (if background
	(set-buffer buffer)
      (cond ((and w3m-use-tab (eq 'w3m-mode major-mode))
	     (switch-to-buffer buffer))
	    ((w3m-popup-frame-p) (switch-to-buffer-other-frame buffer))
	    ((w3m-popup-window-p) (switch-to-buffer-other-window buffer))
	    (t (switch-to-buffer buffer)))
      (w3m-display-progress-message url))
    (w3m-goto-url
     url
     (or reload
	 ;; When new URL has `name' portion, (ir. a URI
	 ;; "fragment"), we have to goto the base url
	 ;; because generated buffer has no content at
	 ;; this moment.
	 (and
	  (w3m-string-match-url-components url)
	  (match-beginning 8)
	  'redisplay))
     charset post-data referer nil nil background)
    ;; Delete useless newly created buffer if it is empty.
    (w3m-delete-buffer-if-empty buffer)))

(defun w3m-move-point-for-localcgi (url)
  (when (and (w3m-url-local-p url)
	     (file-directory-p (w3m-url-to-file-name url))
	     (not (eq w3m-local-directory-view-method 'w3m-dtree))
	     (= (point-min) (point))
	     (w3m-search-name-anchor "current" 'quiet))
    (recenter (/ (window-height) 5))))

;;;###autoload
(defun w3m-gohome ()
  "Go to the Home page."
  (interactive)
  (unless w3m-home-page
    (error "You have to specify the value of `w3m-home-page'"))
  (w3m-goto-url w3m-home-page t nil nil nil nil nil nil t))

;;;###autoload
(defun w3m-create-empty-session ()
  "Create an empty page as a new session and visit it."
  (interactive)
  (w3m-goto-url-new-session "about:blank"))

(defun w3m-user-agent-site-specific (url)
  "Return a site-specific user-agent string.

Compares URL against the regexps of `w3m-user-agent-site-specific-alist'
and returns the corresponding user-agent string of the first match, or
NIL if none match."
  (let ((check-list w3m-user-agent-site-specific-alist) result entry)
    (while (and (not result) (setq entry (pop check-list)))
      (when (string-match (car entry) url)
	(setq result (cdr entry))))
    result))

(defun w3m-user-agent-change (&optional ua-string)
  "Return a user-agent string.

Prompt the user to select from entries in
`w3m-user-agent-default-alist', `w3m-user-agent-alist',
or the user may manually enter a custom user-agent string.

When called interactively, variables `w3m-user-agent' and
`w3m-add-user-agent' are updated, ie. the changes are permanent."
  (interactive "P")
  (when (not ua-string)
    (let ((ua-list (append w3m-user-agent-default-alist w3m-user-agent-alist)))
      (setq ua-string
	    (or
	     (cdr
	      (assoc
	       (setq ua-string
		     (completing-read
		      "Select a user-agent: "
		      `(lambda (string pred action)
			 (if (eq action 'metadata)
			     '(metadata (display-sort-function . identity))
			   (complete-with-action
			    action '(,@ua-list ("Don't send user agent" . ""))
			    string pred)))))
	       ua-list))
	     ua-string))))
  (when (string-equal ua-string "")
    (setq ua-string nil))
  (when (called-interactively-p 'interactive)
    ;; Does not work because we use temporary work buffers when
    ;; constructing the GET header (eg. `w3m-header-arguments')
    ;; (if (y-or-n-p "For this buffer only? ")
    ;;     (progn
    ;;       (make-local-variable 'w3m-add-user-agent)
    ;;       (make-local-variable 'w3m-user-agent))
    ;;   (kill-local-variable 'w3m-add-user-agent)
    ;;   (kill-local-variable 'w3m-user-agent))
    (if (not ua-string)
	(setq w3m-add-user-agent nil)
      (setq w3m-add-user-agent t)
      (setq w3m-user-agent ua-string)))
  ua-string)

(defun w3m-restore-tab-line ()
  "Restore tab-line if it is broken."
  (and w3m-use-tab w3m-use-tab-line (boundp 'tab-line-format)
       (not (equal tab-line-format '(:eval (w3m-tab-line))))
       (setq tab-line-format '(:eval (w3m-tab-line)))))

(defun w3m-reload-this-page (&optional arg background)
  "Reload the current page, disregarding the cached contents.
If the prefix arg ARG is given, it also clears forms and post data.

If the prefix arg is given twice, prompt the user to change the
user-agent string to be sent for the reload.

If the prefix arg is given three times, do both, ie. clear forms
and post data, AND prompt the user to change the user-agent
string to be sent for the reload."
  (interactive "P")
  (w3m-restore-tab-line)
  (if w3m-current-url
      (let* (;; Don't move the history position.
	     (w3m-history-reuse-history-elements 'reload)
	     (w3m-user-agent
	      (if (or (equal arg '(16)) (equal arg '(64)))
		  (w3m-user-agent-change)
		w3m-user-agent))
	     (w3m-add-user-agent
	      (if (or (equal arg '(16)) (equal arg '(64)))
		  w3m-user-agent
		w3m-add-user-agent))
	     post-data)
	(if (or (equal arg '(4)) (equal arg '(64)))
	    (progn
	      (w3m-history-remove-properties '(:forms nil :post-data nil))
	      (setq w3m-current-forms nil))
	  (when (and (setq post-data (w3m-history-plist-get :post-data))
		     (not (y-or-n-p "Repost form data? ")))
	    (setq post-data nil)))
	(w3m-history-store-position)
	(w3m-goto-url w3m-current-url 'reload nil post-data
		      (w3m-history-plist-get :referer)
		      nil
		      (w3m-history-element (cadar w3m-history) t)
		      background)
	(w3m-history-restore-position))
    (w3m-message "Can't reload this page")))

(defun w3m-reload-all-pages (&optional arg)
  "Reload all pages, disregarding the cached contents.
The prefix arg ARG is passed to `w3m-reload-this-page' as the 1st arg."
  (interactive "P")
  (save-window-excursion
    (dolist (buffer (w3m-list-buffers))
      (switch-to-buffer buffer)
      (w3m-reload-this-page arg))))

(defun w3m-redisplay-this-page (&optional arg no-store-pos)
  "Redisplay the current page.
If the prefix arg ARG is given, it toggles the visibility of images.
NO-STORE-POS if it is non-nil means not to store the window positions
before redisplaying."
  (interactive "P")
  (w3m-restore-tab-line)
  (if (null w3m-current-url)
      (w3m-message "Can't redisplay this page")
    (when arg
      (setq w3m-display-inline-images (not w3m-display-inline-images)))
    (let ((w3m-prefer-cache t)
	  (w3m-history-reuse-history-elements
	   ;; Don't move the history position.
	   'reload))
      (or no-store-pos (w3m-history-store-position))
      (w3m-goto-url w3m-current-url 'redisplay)
      (w3m-history-restore-position))))

(defun w3m-redisplay-and-reset (&optional arg)
  "Redisplay the current page and reset the user-specified values.
This function clears the charset and the content type which the user
specified for overriding the values of what the page requires.  The
prefix argument ARG is passed to the `w3m-redisplay-this-page'
function (which see)."
  (interactive "P")
  (if (null w3m-current-url)
      (w3m-message "Can't execute this page")
    (setf (w3m-arrived-content-type w3m-current-url) nil)
    (setf (w3m-arrived-content-charset
	   (if (string-match "\\`about://source/" w3m-current-url)
	       (substring w3m-current-url (match-end 0))
	     w3m-current-url))
	  nil)
    (w3m-redisplay-this-page arg)))

(defun w3m-redisplay-with-charset (&optional arg)
  "Redisplay the current page, specifying a charset.
If the user enters the empty string, the value which once was used for
decoding the page is used.  The prefix argument ARG is passed to the
`w3m-redisplay-this-page' function (which see)."
  (interactive "P")
  (if (null w3m-current-url)
      (w3m-message "Can't execute the command")
    (setf (w3m-arrived-content-charset
	   (if (string-match "\\`about://source/" w3m-current-url)
	       (substring w3m-current-url (match-end 0))
	     w3m-current-url))
	  (w3m-read-content-charset
	   (format "Content-charset (current %s, default reset): "
		   w3m-current-coding-system)))
    (w3m-redisplay-this-page arg)))

(defun w3m-redisplay-with-content-type (&optional arg)
  "Redisplay the current page, specifying a content type.
If the user enters the empty string, it uses the value which was
specified by the page's contents itself.  The prefix argument ARG is
passed to the `w3m-redisplay-this-page' function (which see)."
  (interactive "P")
  (if (null w3m-current-url)
      (w3m-message "Can't execute this page")
    (setf (w3m-arrived-content-type w3m-current-url)
	  (let ((type (completing-read
		       (format "Content-type (current %s, default reset): "
			       (or (w3m-arrived-content-type w3m-current-url)
				   (w3m-content-type w3m-current-url)))
		       w3m-content-type-alist nil t)))
	    (unless (string= type "") type)))
    (w3m-redisplay-this-page arg)))

(defun w3m-redisplay-pages-automatically (&optional _arg)
  "Redisplay pages when some operation changes the page width.
Note that the visibility of the same page, i.e., the same buffer,
displayed in the other unselected windows will also change unwantedly."
  ;; Don't care the page height change that often happens
  ;; if the echo area shows a message in two or more lines.
  (unless (eq (selected-window) (minibuffer-window))
    (let (pos bufsize buffer buffers)
      (cl-labels
	  ((re--center
	    (pos bufsize)
	    (goto-char (1+ (round (* (car pos) bufsize))))
	    (goto-char
	     (prog1 (line-beginning-position)
	       (forward-line (- (round (* (cdr pos) (1- (window-height))))))
	       (set-window-start nil (point)))))
	   (re--display
	    (window)
	    (save-selected-window
	      (select-window window)
	      (when (and (eq major-mode 'w3m-mode) (not w3m-current-process))
		(setq bufsize (buffer-size))
		(if (setq pos (cdr (assoc (setq buffer (window-buffer window))
					  buffers)))
		    (re--center pos bufsize)
		  (unless (eq w3m-last-window-width
			      (setq w3m-last-window-width
				    (window-width window)))
		    (setq pos
			  (if (zerop bufsize)
			      '(0 . 0)
			    (cons (/ (1- (line-beginning-position))
				     (float bufsize))
				  (/ (count-lines (window-start) (point))
				     (float (max 1 (1- (window-height))))))))
		    (w3m-condition-case nil
			(let ((w3m-message-silent t)
			      w3m-clear-display-while-reading)
			  (w3m-redisplay-this-page))
		      ;; Try to do reloading when redisplaying fails.
		      (error (w3m-reload-this-page)))
		    (re--center pos (buffer-size))
		    (push (cons buffer pos) buffers)))))))
	(re--display (selected-window))
	(walk-windows #'re--display 'ignore-minibuf (selected-frame))))))

(defun w3m-examine-command-line-args ()
  "Return a url when the `w3m' command is invoked from the command line.
The `w3m' Lisp command can be invoked even in the batch mode, e.g.,
``emacs -f w3m'' or ``emacs -f w3m url''.  This function is used in
the very case, it extracts a url string from the command line
arguments and passes it to the `w3m' command.  If a url is omitted, it
defaults to the value of `w3m-home-page' or \"about:\"."
  (let ((url (car command-line-args-left))
	(directives '("-f" "-funcall" "--funcall" "-e"))
	args)
    (if (and url (not (string-match "\\`-" url)))
	(progn
	  (setq command-line-args-left (cdr command-line-args-left))
	  (when (string-match "\\`[\t ]*\\'" url)
	    ;; emacs -f w3m '' ...
	    (setq url (or w3m-home-page "about:"))))
      (setq args (nthcdr (max (- (length command-line-args)
				 (length command-line-args-left)
				 2)
			      1)
			 command-line-args))
      (when (and (equal (cadr args) "w3m")
		 (member (car args) directives))
	(setq url (or w3m-home-page "about:"))))
    (unless
	(and command-line-args-left
	     (progn
	       (setq args (reverse command-line-args-left))
	       (while (and args
			   (not (and (setq args (cdr (member "w3m" args)))
				     (member (car args) directives)))))
	       args))
      (defalias 'w3m-examine-command-line-args (lambda nil)))
    ;; Inhibit the startup screen.
    (when url
      (let (fn)
	(unless inhibit-startup-screen
	  (setq inhibit-startup-screen t)
	  (setq fn (make-symbol "w3m-inhibit-startup-screen"))
	  (fset fn `(lambda nil
		      (setq inhibit-startup-screen nil)
		      (remove-hook 'window-setup-hook ',fn)
		      (fmakunbound ',fn)))
	  (add-hook 'window-setup-hook fn))))
    url))

;;;###autoload
(defun w3m (&optional url new-session interactive-p)
  "Visit World Wide Web pages using the external w3m command.

If no emacs-w3m session already exists: If POINT is at a url
string, visit that. Otherwise, if `w3m-home-page' is defined,
visit that. Otherwise, present a blank page. This behavior can be
over-ridden by setting variable `w3m-quick-start' to nil, in
which case you will always be prompted for a URL.

If an emacs-w3m session already exists: Pop to one of its windows
or frames. You can over-ride this behavior by setting
`w3m-quick-start' to nil, in order to always be prompted for a
URL.

In you have set `w3m-quick-start' to nil, but wish to over-ride
default behavior from the command line, either run this command
with a prefix argument or enter the empty string for the prompt.
In such cases, this command will visit a url at the point or,
lacking that, the URL set in variable `w3m-home-page' or, lacking
that, the \"about:\" page.

Any of five display styles are possible. See `w3m-display-mode'
for a description of those options.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function creates a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs calls this function as an interactive command
in the batch mode."
  (interactive
   (let ((url
	  ;; Emacs calls a Lisp command interactively even if it is
	  ;; in the batch mode.  If the following function returns
	  ;; a non-nil value, it means this function is called in
	  ;; the batch mode, and we don't treat it as what it is
	  ;; called interactively.
	  (w3m-examine-command-line-args))
	 new)
     (list
      ;; url
      (or url
	  (let ((default (or (w3m-url-at-point)
			     (if (w3m-alive-p) 'popup w3m-home-page))))
	    (setq new (if current-prefix-arg
			  default
			(w3m-input-url nil nil default w3m-quick-start
				       'feeling-searchy 'no-initial)))))
      ;; new-session
      (and w3m-make-new-session
	   (w3m-alive-p)
	   (not (eq new 'popup)))
      ;; interactive-p
      (not url))))
  (let ((nofetch (eq url 'popup))
	(alived (w3m-alive-p))
	(buffer (unless new-session (w3m-alive-p t)))
	(w3m-pop-up-frames (and interactive-p w3m-pop-up-frames))
	(w3m-pop-up-windows (and interactive-p w3m-pop-up-windows)))
    (unless (and (stringp url)
		 (> (length url) 0))
      (if buffer
	  (setq nofetch t)
	;; This command was possibly be called non-interactively or as
	;; the batch mode.
	(setq url (or (w3m-examine-command-line-args)
		      ;; Unlikely but this function was called with no url.
		      "about:")
	      nofetch nil)))
    (unless buffer
      ;; It means `new-session' is non-nil or there's no emacs-w3m buffer.
      ;; At any rate, we create a new emacs-w3m buffer in this case.
      (setq buffer (w3m-generate-new-buffer "*w3m*")))
    (w3m-popup-buffer buffer)
    (unless nofetch
      ;; `unwind-protect' is needed since a process may be terminated by C-g.
      (unwind-protect
	  (let* ((crash (and (not alived)
			     (w3m-session-last-crashed-session)))
		 (last (and (not alived)
			    (not crash)
			    (w3m-session-last-autosave-session))))
	    (w3m-goto-url url)
	    (when (or crash last)
	      (w3m-session-goto-session (or crash last))))
	;; Delete useless newly created buffer if it is empty.
	(w3m-delete-buffer-if-empty buffer)))))

(declare-function browse-url-interactive-arg "browse-url" (prompt))

;;;###autoload
(defun w3m-browse-url (url &optional new-session refresh-if-exists)
  "Ask emacs-w3m to browse URL.
When called interactively, URL defaults to the string existing around
the cursor position and looking like a url.  If the prefix argument is
given[1] or NEW-SESSION is non-nil, create a new emacs-w3m session.
If REFRESH-IF-EXISTS is non-nil, refresh the page if it already exists
but is older than the site.

[1] More precisely the prefix argument inverts the boolean logic of
`browse-url-new-window-flag' that defaults to nil."
  (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "Emacs-w3m URL: ")))
  (when (stringp url)
    (setq url (w3m-canonicalize-url url))
    (if new-session
	(w3m-goto-url-new-session url)
      (w3m-goto-url
       url
       ;; Reload the page if it is already visited, older than the site,
       ;; and REFRESH-IF-EXISTS is non-nil.
       (let (buffer)
	 (and refresh-if-exists
	      (setq buffer (w3m-alive-p t))
	      (string-equal url (with-current-buffer buffer w3m-current-url))
	      (w3m-time-newer-p (let ((w3m-message-silent t))
				  (w3m-last-modified url t))
				(w3m-arrived-last-modified url))))
       nil nil nil nil nil nil t))))

;;;###autoload
(defun w3m-find-file (file)
  "Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme."
  (interactive "fFilename: ")
  (w3m-goto-url (w3m-expand-file-name-as-url file)
		nil coding-system-for-read
		nil nil nil nil nil t))

(defun w3m-cygwin-path (path)
  "Convert PATH in the win32 style into the cygwin format.
ex. c:/dir/file => //c/dir/file"
  (if (string-match "\\`\\([A-Za-z]\\):" path)
      (replace-match "//\\1" nil nil path)
    path))

;;;###autoload
(defun w3m-region (start end &optional url charset)
  "Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset."
  (interactive (list (region-beginning)
		     (region-end)
		     (w3m-expand-file-name-as-url (or (buffer-file-name)
						      default-directory))))
  (w3m-arrived-setup)
  (w3m-process-stop (current-buffer))
  (w3m-clear-local-variables)
  (setq url (or url w3m-buffer-local-url)
	w3m-current-url url
	w3m-current-base-url url)
  (let ((inhibit-read-only t)
	(w3m-current-buffer (current-buffer))
	w3m-use-refresh)
    (save-restriction
      (narrow-to-region start end)
      (unless charset
	(setq charset (w3m-correct-charset (w3m-detect-meta-charset))))
      (setq w3m-current-coding-system
	    (if charset
		(w3m-charset-to-coding-system charset)
	      w3m-coding-system))
      (unless (eq buffer-undo-list t)
	;; Record text that of before being changed to the undo list.
	(setq buffer-undo-list
	      (nconc (list nil nil
			   (cons (buffer-substring start end) start))
		     buffer-undo-list)))
      (let ((buffer-undo-list t)) ;; Suspend undo recording.
	(setq w3m-current-title (w3m-rendering-buffer charset))
	(w3m-fontify)
	(goto-char (point-max))
	(skip-chars-backward "\t\n ")
	(delete-region (point) (point-max))
	(insert "\n"))
      (unless (eq buffer-undo-list t)
	;; Set the pointer for new text in the undo list.
	(setcar (cdr buffer-undo-list) (cons start (point-max))))
      (when (w3m-display-inline-images-p)
	(and w3m-force-redisplay (sit-for 0))
	(w3m-toggle-inline-images 'force)))))

;;;###autoload
(defun w3m-buffer (&optional url charset)
  "Render the current buffer.
See `w3m-region' for the optional arguments."
  (interactive (list (w3m-expand-file-name-as-url (or (buffer-file-name)
						      default-directory))))
  (w3m-region (point-min) (point-max) url charset))

;;; About:
(defun w3m-about (_url &rest _args)
  (insert "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">
<html>
<head><title>About emacs-w3m</title></head>
<body>
<center>
Welcome to <a href=\""
	  (if (equal w3m-language "Japanese")
	      "https://emacs-w3m.github.io/index-ja.html"
	    "https://emacs-w3m.github.io/index.html")
	  "\">\
<img src=\"about://emacs-w3m.gif\" alt=\"emacs-w3m\" width=\"83\"
height=\"14\"></a>!<br><br>
emacs-w3m is an interface program of
<a href=\"http://w3m.sourceforge.net/\">w3m</a>,
works on Emacs.
</center>
</body>
</html>")
  "text/html")

(defun w3m-view-source (&optional arg)
  "Display an html source of a page visited in the current buffer.
ARG should be a number (a non-numeric value is treated as `1') which
controls how much to decode a source.  A number larger than or equal
to 4 (which the `C-u' prefix produces) means don't decode.  The number
2 or 3 means decode normal text.  The number 1 means decodes `&#nnn;'
entities in 128..159 and 160 in addition to normal text (the default).
A number less than or equal to zero means also encode urls containing
non-ASCII characters."
  (interactive "p")
  (if w3m-current-url
      (let ((w3m-prefer-cache t)
	    (w3m-view-source-decode-level (if (numberp arg) arg 0))
	    (w3m-history-reuse-history-elements t))
	(w3m-history-store-position)
	(cond
	 ((string-match "\\`about://source/" w3m-current-url)
	  (w3m-goto-url (substring w3m-current-url (match-end 0))))
	 ((string-match "\\`about://header/" w3m-current-url)
	  (w3m-goto-url (concat "about://source/"
				(substring w3m-current-url (match-end 0)))))
	 (t
	  (w3m-goto-url  (concat "about://source/" w3m-current-url))))
	(w3m-history-restore-position)
	t) ; <-- an improvement, but wrong if the above failed (BORUCH)
    (w3m-message "Can't view page source")))

(defun w3m-make-separator ()
  (if (string= w3m-language "Japanese")
      (make-string (/ (w3m-display-width) 2)
		   (make-char 'japanese-jisx0208 40 44))
    (make-string (w3m-display-width) ?-)))

(defun w3m-about-header (url &optional _no-uncompress no-cache &rest _args)
  (when (string-match "\\`about://header/" url)
    (setq url (substring url (match-end 0)))
    (insert "Page Information\n"
	    "\nTitle:          " (or (w3m-arrived-title
				      (w3m-url-strip-authinfo url))
				     "")
	    "\nURL:            " url
	    "\nDocument Type:  " (or (w3m-content-type url) "")
	    "\nLast Modified:  "
	    (let ((time (w3m-last-modified url)))
	      (if time (current-time-string time) "")))

    (let (anchor anchor-title
		 image-url image-alt image-size)
      (with-current-buffer w3m-current-buffer
	(when (equal url w3m-current-url)
	  (setq anchor (w3m-anchor)
		anchor-title (w3m-anchor-title)
		image-url (w3m-image)
		image-alt (w3m-image-alt)
		image-size (w3m-get-text-property-around 'w3m-image-size))))
      (if anchor
	  (insert "\nCurrent Anchor: " anchor))
      (if anchor-title
	  (insert "\nAnchor Title:   " anchor-title))
      (if image-url
	  (insert "\nImage:      " image-url))
      (if image-alt
	  (insert "\nImage Alt:  " image-alt))
      (if image-size
	  (insert (format "\nImage Size: %sx%s"
			  (car image-size) (cdr image-size)))))

    (let ((ct (w3m-arrived-content-type url))
	  (charset (w3m-arrived-content-charset url))
	  (separator (w3m-make-separator))
	  (case-fold-search t)
	  header ssl beg)
      (when (or ct charset)
	(insert "\n\n" separator "\n\nModifier Information\n")
	(insert "\nDocument Content-Type:  " (or ct ""))
	(insert "\nDocument Charset:       " (or charset "")))
      (when (and (not (w3m-url-local-p url))
		 (setq header (condition-case nil
				  (or (unless no-cache
					(w3m-cache-request-header url))
				      (w3m-process-with-wait-handler
					(w3m-w3m-dump-head url handler)))
				(w3m-process-timeout nil))))
	(insert "\n\n" separator "\n\nHeader Information\n\n" header)
	(goto-char (point-min))
	(when (re-search-forward "^w3m-ssl-certificate: " nil t)
	  (setq beg (match-end 0))
	  (forward-line)
	  (while (and (not (eobp)) (looking-at "^[ \t]"))
	    (forward-line))
	  (setq ssl (buffer-substring beg (point)))
	  (delete-region beg (point))
	  (goto-char beg)
	  (insert "SSL\n")
	  (goto-char (point-max))
	  (insert separator "\n\nSSL Information\n\n")
	  (setq beg (point))
	  (insert ssl)
	  (goto-char beg)
	  (while (re-search-forward "^\t" nil t)
	    (delete-char -1)
	    (when (looking-at "Certificate:")
	      (insert "\n"))))))
    "text/plain"))

(defun w3m-view-header ()
  "Display the header of the current page."
  (interactive)
  (if w3m-current-url
      (let ((w3m-prefer-cache t)
	    (w3m-history-reuse-history-elements t)
	    (url (cond
		  ((string-match "\\`about://header/" w3m-current-url)
		   (substring w3m-current-url (match-end 0)))
		  ((string-match "\\`about://source/" w3m-current-url)
		   (let ((real-url (substring w3m-current-url (match-end 0))))
		     (unless (string-match "\\`about:" real-url)
		       (concat "about://header/" real-url))))
		  ((string-match "\\`about:" w3m-current-url)
		   nil)
		  (t
		   (concat "about://header/" w3m-current-url)))))
	(if url
	    (progn
	      (w3m-history-store-position)
	      (w3m-goto-url url)
	      (w3m-history-restore-position))
	  (w3m-message "Can't load a header for %s" w3m-current-url)))
    (w3m-message "Can't view page header")))

(defvar w3m-about-history-max-indentation '(/ (* (window-width) 2) 3)
  "*Number used to limit the identation level when showing a history.
This value is evaluated whenever a history page is displayed by the
`w3m-about-history' command.  So, it can be any s-expression returning
a number.")

(defvar w3m-about-history-indent-level 4
  "*Number used to specify the indentation level when showing a history.
A history page is invoked by the `w3m-about-history' command.")

(defun w3m-about-history (&rest _args)
  "Render the current buffer's tree-structured browsing history in HTML."
  ;; ARGS is not used.  It is necessary in order to >/dev/null
  ;; unnecessary arguments because this function is one of several
  ;; called by `w3m-about-retrieve' using a generically constructed
  ;; `funcall'.
  (let (start history current)
    (with-current-buffer w3m-current-buffer
      (setq history w3m-history-flat
	    current (cadar w3m-history)))
    (insert "\
<head><title>URL history</title></head><body>
<h1>List of all the links you have visited in this tab.</h1><pre>\n")
    (setq start (point))
    (when history
      (let ((form
	     (format
	      "%%0%dd"
	      (length
	       (number-to-string
		(apply 'max
		       (apply 'append
			      (mapcar
			       ;; Don't use `caddr' here, since it won't
			       ;; be substituted by the compiler macro.
			       (lambda (e)
				 (car (cdr (cdr e))))
			       history)))))))
	    (cur (current-buffer))
	    (margin (if (> w3m-about-history-indent-level 1)
			1
		      0))
	    (max-indent (condition-case nil
			    ;; Force the value to be a number or nil.
			    (+ 0 (eval w3m-about-history-max-indentation))
			  (error nil)))
	    (last-indent -1)
	    (sub-indent 0)
	    element url about title position bol indent)
	(while history
	  (setq element (pop history)
		url (car element)
		;; FIXME: an ad-hoc workaround to avoid illegal-type errors.
		about (or (not (stringp url))
			  (string-match w3m-history-ignored-regexp url))
		title (plist-get (cadr element) :title)
		position (caddr element))
	  (when url
	    (insert (format "h%s %d %d <a href=\"%s\">%s%s%s %s</a>\n"
			    (mapconcat (lambda (d) (format form d))
				       position
				       "-")
			    (/ (1- (length position)) 2)
			    (if (equal current position) 1 0)
			    url
			    (if about "&lt;" "")
			    (if (or (not title)
				    (string-equal "<no-title>" title)
				    (string-match "\\`[\t 　]*\\'" title))
				url
			      (w3m-encode-specials-string title))
			    (if about "&gt;" "")
			    position))))
	(sort-fields 0 start (point-max))
	(goto-char start)
	(while (not (eobp))
	  (setq bol (point))
	  (skip-chars-forward "^ ")
	  (setq indent (read cur)
		sub-indent (if (= indent last-indent)
			       (1+ sub-indent)
			     0)
		last-indent indent
		indent (+ (* w3m-about-history-indent-level indent)
			  sub-indent))
	  (when (prog1
		    (= (read cur) 1)
		  (delete-region bol (point))
		  (insert-char ?  (+ margin (if max-indent
						(min max-indent indent)
					      indent))))
	    (beginning-of-line)
	    (delete-char 1)
	    (insert "&gt;"))
	  (forward-line 1))))
    (insert "</pre></body>")
    "text/html"))

(defvar w3m-db-history-align-to-column nil)

(defun w3m-about-db-history (url &rest _args)
  "Render a flat chronological HTML list of all buffers' browsing history."
  ;; ARGS is not used. It is necessary in order to >/dev/null
  ;; unnecessary arguments because this function is one of several
  ;; called by `w3m-about-retrieve' using a generically constructed
  ;; `funcall'.
  (let* ((start 0)
	 (size 0)
	 (print-all t)
	 (width (- (w3m-display-width) (if (display-graphic-p) 18 19)))
	 (now (current-time))
	 (ellipsis "…")
	 title time alist prev next page total)
    (when (string-match "\\`about://db-history/\\?" url)
      (dolist (s (split-string (substring url (match-end 0)) "&"))
	(when (string-match "\\`\\(?:size\\|\\(start\\)\\)=" s)
	  (if (match-beginning 1)
	      (setq start (string-to-number (substring s (match-end 0))))
	    (setq size (string-to-number (substring s (match-end 0))))
	    (unless (zerop size) (setq print-all nil))))))
    (when w3m-arrived-db
      (maphash
       (lambda (_key sym)
	 (and sym
	      (setq url (symbol-name sym))
	      (not (string-match "#" url))
	      (not (string-match w3m-history-ignored-regexp url))
	      (push (cons url (w3m-arrived-time url)) alist)))
       w3m-arrived-db)
      (setq alist (sort alist
			(lambda (a b)
			  (w3m-time-newer-p (cdr a) (cdr b))))))
    (setq total (length alist))
    (setq alist (nthcdr start alist))
    (unless (zerop size)
      (when (> start 0)
	(setq prev
	      (format "about://db-history/?start=%d&size=%d"
		      (max 0 (- start size)) size)))
      (when (> (length alist) size)
	(setq next
	      (format "about://db-history/?start=%d&size=%d"
		      (+ start size) size)))
      (when (> total 0)
	(setq total (+ (/ total size) (if (> (% total size) 0) 1 0)))
	(setq page (1+ (/ start size)))))
    (insert "<html><head><title>URL history in DataBase</title>"
	    (if prev (format "<link rel=\"prev\" href=\"%s\">\n" prev) "")
	    (if next (format "<link rel=\"next\" href=\"%s\">\n" next) "")
	    (format "</head><body>\
<center><h1>Global URL history for all w3m buffers%s</h1></center>\n"
		    (if (and page total)
			(format " (page %d/%d)" page total) "")))
    (setq prev
	  (if (or prev next)
	      (setq next
		    (concat
		     "<table width=100%><tr>"
		     (if prev
			 (format "\
<td width=50%% align=\"left\">[<a href=\"%s\">Prev Page</a>]</td>" prev)
		       "<td width=50%%></td>")
		     (if next
			 (format "\
<td width=50%% align=\"right\">[<a href=\"%s\">Next Page</a>]</td>" next)
		       "<td width=50%%></td>")
		     "</tr></table>\n"))
	    ""))
    (if (null alist)
	(insert "<em>Nothing in DataBase.</em>\n")
      (insert prev "<table width=100% cellpadding=0>
<tr><td><h2>Title/URL</h2></td><td><h2>Time/Date</h2></td></tr>\n")
      (while (and alist (or (>= (cl-decf size) 0) print-all))
	(setq url (car (car alist))
	      time (cdr (car alist))
	      alist (cdr alist)
	      title (w3m-arrived-title url))
	(cond
	 ((or (null title) (string= "<no-title>" title))
	  (setq title
		(concat
		 "&lt;"
		 (if (> (string-width url) (- width 2))
		     (truncate-string-to-width url (- width 3) nil ?  ellipsis)
		   url)
		 "&gt")))
	 (t
	  (setq title
		(w3m-encode-specials-string
		 (if (> (string-width title) width)
		     (truncate-string-to-width title (1- width) nil ?  ellipsis)
		   title)))))
	(insert (format "<tr><td><nobr><a href=\"%s\">%s</a></nobr></td>"
			url title))
	(when time
	  (insert "<td>"
		  (if (<= (w3m-time-lapse-seconds time now)
			  64800) ;; = (* 60 60 18) 18hours.
		      (format-time-string "%H:%M:%S&nbsp;Today" time)
		    (format-time-string "%H:%M:%S&nbsp;%Y-%m-%d" time))
		  "</td>"))
	(insert "</tr>\n"))
      (insert "</table>"
	      (if next "\n<br>\n<hr>\n" "")
	      prev))
    (insert "</body></html>\n")
    (setq w3m-db-history-align-to-column width))
  "text/html")

(defun w3m-history-highlight-current-url (url)
  "Highlight the current url if it is a page for the history.
It does manage history position data as well."
  (when (string-equal "about://history/" url)
    (let ((inhibit-read-only t)
	  (buffer (current-buffer))
	  start)
      ;; Make history position data invisible.
      (goto-char (point-min))
      (w3m-next-anchor)
      (while (progn
	       (setq start (point))
	       ;; Extend href anchor.
	       (put-text-property (point-at-bol) start
				  'w3m-href-anchor
				  (get-text-property start 'w3m-href-anchor))
	       (re-search-forward " (\\(?:[0-9]+ \\)*[0-9]+)$" nil t))
	(goto-char (match-beginning 0))
	(put-text-property start (match-beginning 0)
			   'history-position (read buffer))
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(invisible t intangible t))
	(forward-char 2)
	(skip-chars-forward "\t "))
      ;; Highlight the current url.
      (goto-char (point-min))
      (when (search-forward "\n>" nil t)
	(w3m-next-anchor)
	(setq start (point))
	(end-of-line)
	(w3m-add-face-property start (point) 'w3m-history-current-url)
	(goto-char start)))
    (set-buffer-modified-p nil)))

(defcustom w3m-db-history-display-size
  (and (> w3m-keep-arrived-urls 500) 500)
  "Maximum number of arrived URLs which are displayed per page."
  :group 'w3m
  :type '(radio (const :tag "All entries are displayed in single page." nil)
		(integer :format "%t: %v\n")))

(defcustom w3m-history-in-new-buffer nil
  "Whether to display URL histories in the current buffer."
  :group 'w3m
  :type 'boolean)

(defun w3m-db-history-fix-indentation (url)
  "Fix wrong indentation that `w3m -halfdump' may produce in db history.
Time/Date columns might nevertheless not align depending on the fonts
especially on TTY."
  (when (string-match "\\`about://db-history/" url)
    (let ((regexp "\\( +\\)\\(?:[012][0-9]:[0-5][0-9]:[0-5][0-9] \
\\(?:20[1-9][0-9]-[01][0-9]-[0-3][0-9]\\|Today\\)\\|Time/Date\\) *$")
	  (inhibit-read-only t)
	  (aspace (propertize
		   " " 'display
		   `(space :align-to ,w3m-db-history-align-to-column)))
	  (inhibit-read-only t)
	  num)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (delete-region (match-beginning 1) (goto-char (match-end 1)))
	  (when (> (setq num (- w3m-db-history-align-to-column
				(current-column)))
		   0)
	    (insert-char ?  (1- num))
	    (insert aspace))))
      (set-buffer-modified-p nil))))

(defun w3m-db-history (&optional start size)
  "Display a flat chronological list of all buffers' browsing history.

This is a flat (not hierarchial) presentation of all URLs visited
by ALL w3m buffers, and includes a timestamp for when the URL was
visited.  The list is presented in reverse-chronological order,
i.e., most recent URL first.

START is a positive integer for the point in the history list at
which to begin displaying, where 0 is the most recent entry.

SIZE is the maximum number of arrived URLs which are displayed
per page.  Variable `w3m-db-history-display-size' sets the
default.  Use 0 to display the entire history on a single page.

If this function is called interactively with the prefix argument,
prompt a user for START and SIZE if the prefix argument is not a
number (i.e., `C-u').  Otherwise if the prefix argument is a number
(i.e., `C-u NUM'), use it as START and leave SIZE nil, that will be
overridden by `w3m-db-history-display-size' or 0."
  (interactive "P")
  (when (and (w3m-interactive-p) start (not (natnump start)))
    (setq start (read-number
		 "How far back in the history to start displaying: "
		 0)
	  size (read-number
		"How many entries per page (0 for all on one page): "
		(or w3m-db-history-display-size 0))))
  (or start (setq start 0))
  (or size (setq size (or w3m-db-history-display-size 0)))
  (let ((url (format "about://db-history/?start=%d&size=%d" start size)))
    (if w3m-history-in-new-buffer
	(w3m-goto-url-new-session url)
      (w3m-goto-url url nil nil nil nil nil nil nil t))))

(defun w3m-history (&optional arg)
  "Display the current buffer's browsing history tree.

If called with the prefix argument, display a flat chronological
list of ALL buffers' browsing history.

A buffer's history tree is a hierarchal presentation of all
URLs visited by the current buffer and its \"parents\", meaning
that if the buffer was spawned using a command such as
`w3m-goto-url-new-session', its history will include that of the
prior w3m buffer.

The flat chronological list is not hierarchial, but includes all
URLs visited by ALL w3m buffers, as well as a timestamp for when
the URL was visited. "
  (interactive "P")
  (if arg
      (w3m-db-history nil w3m-db-history-display-size)
    (if w3m-history-in-new-buffer
	(w3m-goto-url-new-session "about://history/")
      (w3m-goto-url "about://history/" nil nil nil nil nil nil nil t))))

(defun w3m-w32-browser-with-fiber (url)
  (let ((proc (start-process "w3m-w32-browser-with-fiber"
			     (current-buffer)
			     "fiber.exe" "-s"
			     (if (w3m-url-local-p url)
				 (w3m-url-to-file-name url)
			       url))))
    (set-process-filter proc 'ignore)
    (set-process-sentinel proc 'ignore)))

(defun w3m-pipe-source (&optional url command)
  "Pipe the page source of url URL in binary to a shell command COMMAND.
For the interactive use, URL defaults to that of a link at the point;
if there are both a link to a page and a link to an image at the point,
the link to a page is preferred unless the prefix argument is given."
  (interactive
   (let ((url (or (if current-prefix-arg
		      (or (w3m-image) (w3m-anchor))
		    (or (w3m-anchor) (w3m-image)))
		  (and w3m-current-url
		       (prog1
			   (y-or-n-p (format "Pipe <%s> ? " w3m-current-url))
			 (message nil))
		       w3m-current-url)))
	 command)
     (if (and (w3m-url-valid url)
	      (progn
		(setq command (read-string "Command: "))
		(not (string-match "\\`[\000-\040]*\\'" command))))
	 (list url command)
       (list 'none nil))))
  (cond ((eq url 'none) nil)
	((and (stringp url)
	      (w3m-url-valid url)
	      (stringp command)
	      (not (string-match "\\`[\000-\040]*\\'" command)))
	 (w3m-message "Pipe <%s> to \"| %s\"..." url command)
	 (with-temp-buffer
	   (set-buffer-multibyte nil)
	   (w3m-process-with-wait-handler
	     (w3m-retrieve (cond ((string-match "\\`about://source/" url)
				  url)
				 ((string-match "\\`about://header/" url)
				  (concat "about://source/"
					  (substring url (match-end 0))))
				 (t
				  (concat "about://source/" url)))))
	   (shell-command-on-region (point-min) (point-max) command nil)
	   (w3m-message "Pipe <%s> to \"| %s\"...done" url command)
	   (let ((buffer (get-buffer "*Shell Command Output*")))
	     (when (and buffer
			(not (zerop (buffer-size buffer))))
	       (display-buffer buffer)))))
	(t (error "Can't pipe page source"))))

;;; Interactive select buffer.
(defcustom w3m-select-buffer-horizontal-window t
  "Non-nil means split windows horizontally to open selection pop-up windows."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-select-buffer-window-ratio '(18 . 12)
  "The percentage of the selection window to the whole frame.
The car is used when splitting windows horizontally and the cdr is for
splitting windows vertically."
  :group 'w3m
  :type '(cons (integer :format "H: %v%%  " :size 1)
	       (integer :format "V: %v%%" :size 1)))

(defvar w3m-select-buffer-window nil)
(defconst w3m-select-buffer-message
  "n: next buffer, p: previous buffer, q: quit."
  "Help message used when the emacs-w3m buffers selection window is open.")

;; Why this function is here abruptly is because of
;; `w3m-select-buffer-horizontal-window' and `w3m-session-select-mode'.
(defun w3m-display-width ()
  "Return the maximum width which should display lines within the value."
  (if (< 0 w3m-fill-column)
      w3m-fill-column
    (+ (if (or (and w3m-select-buffer-horizontal-window
		    (get-buffer-window w3m-select-buffer-name))
	       ;; FIXME: that the session-select window is selected
	       ;; at this time would probably be due to a bug.
	       (with-current-buffer (window-buffer)
		 (eq major-mode 'w3m-session-select-mode)))
	   ;; Show a page as if there is no selection window.
	   (frame-width)
	 (window-width))
       (or w3m-fill-column -1))))

(defun w3m--setup-popup-window (toggle buffer-name nomsg)
  "Create a generic w3m popup window and its buffer.

TOGGLE toggles the position of the window between being below or
beside the main window."
  (let ((selected-window (selected-window)))
    (when toggle
      (setq w3m-select-buffer-horizontal-window
	    (not w3m-select-buffer-horizontal-window))
      (when (get-buffer-window buffer-name)
	(delete-windows-on buffer-name)))
    (unless (memq major-mode
		  '(w3m-mode w3m-select-buffer-mode w3m-session-select-mode))
      (let ((buffer (w3m-alive-p t)))
	(if buffer
	    (w3m-popup-buffer buffer)
	  (w3m-goto-url (or w3m-home-page "about:")))))
    (set-buffer (w3m-get-buffer-create buffer-name))
    (unless (eq nomsg 'update)
      (setq w3m-select-buffer-window selected-window))
    (let ((w (or (get-buffer-window buffer-name)
		 (split-window selected-window
			       (w3m-select-buffer-window-size)
			       w3m-select-buffer-horizontal-window))))
      (set-window-buffer w (current-buffer))
      (select-window w))))

(defun w3m-select-buffer (&optional toggle nomsg)
  "Pop-up an emacs-w3m buffers selection window.

Allows convenient switching between emacs-w3m buffers. With the
prefix-argument, toggles the position of the popup window between
being below or beside the main window.

The following command keys are available:

\\{w3m-select-buffer-mode-map}"
  (interactive "P")
  (let ((curbuf (current-buffer)))
    (w3m--setup-popup-window toggle w3m-select-buffer-name nomsg)
    (w3m-select-buffer-generate-contents curbuf)
    (w3m-select-buffer-mode)
    (or nomsg (w3m-message w3m-select-buffer-message))))

(defun w3m-select-buffer-update (&rest _args)
  (when (get-buffer-window w3m-select-buffer-name)
    (save-selected-window
      (w3m-select-buffer nil 'update)))
  (when w3m-use-tab
    (w3m-force-window-update)))

(defun w3m-select-buffer-generate-contents (curbuf)
  (let ((i 0)
	(inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (dolist (buffer (w3m-list-buffers))
      (put-text-property (point)
			 (progn
			   (insert (format "%d:%s %s\n" (cl-incf i)
					   (if (w3m-unseen-buffer-p buffer)
					       "(u)" "   ")
					   (w3m-buffer-title buffer)))
			   (point))
			 'w3m-select-buffer buffer))
    (skip-chars-backward " \t\r\f\n")
    (delete-region (point) (point-max))
    (set-buffer-modified-p nil)
    (goto-char (or (text-property-any (point-min) (point-max)
				      'w3m-select-buffer curbuf)
		   (point-min)))))

(defvar w3m-select-buffer-mode-map nil)
(unless w3m-select-buffer-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (substitute-key-definition
     'next-line 'w3m-select-buffer-next-line map global-map)
    (substitute-key-definition
     'previous-line 'w3m-select-buffer-previous-line map global-map)
    (substitute-key-definition
     'w3m-copy-buffer 'w3m-select-buffer-copy-buffer map w3m-mode-map)
    (substitute-key-definition
     'w3m-next-buffer 'w3m-select-buffer-next-line map w3m-mode-map)
    (substitute-key-definition
     'w3m-previous-buffer 'w3m-select-buffer-previous-line map w3m-mode-map)
    (substitute-key-definition
     'w3m-delete-buffer 'w3m-select-buffer-delete-buffer map w3m-mode-map)
    (substitute-key-definition
     'w3m-delete-other-buffers
     'w3m-select-buffer-delete-other-buffers map w3m-mode-map)
    (substitute-key-definition
     'w3m-scroll-up-or-next-url
     'w3m-select-buffer-show-this-line map w3m-mode-map)
    (substitute-key-definition
     'w3m-scroll-down-or-previous-url
     'w3m-select-buffer-show-this-line-and-down map w3m-mode-map)
    (substitute-key-definition
     'w3m-select-buffer 'w3m-select-buffer-toggle-style map w3m-mode-map)
    (define-key map " " 'w3m-select-buffer-show-this-line)
    (define-key map "g" 'w3m-select-buffer-recheck)
    (define-key map "j" 'w3m-select-buffer-next-line)
    (define-key map "k" 'w3m-select-buffer-previous-line)
    (define-key map "n" 'w3m-select-buffer-next-line)
    (define-key map "p" 'w3m-select-buffer-previous-line)
    (define-key map "\C-c\C-n" 'w3m-select-buffer-move-next)
    (define-key map "\C-c\C-p" 'w3m-select-buffer-move-previous)
    (define-key map "u" 'w3m-select-buffer-toggle-unseen)
    (define-key map "q" 'w3m-select-buffer-quit)
    (define-key map "h" 'w3m-select-buffer-show-this-line-and-switch)
    (define-key map "w" 'w3m-select-buffer-show-this-line-and-switch)
    (define-key map "\C-m" 'w3m-select-buffer-show-this-line-and-quit)
    (define-key map "\C-c\C-c" 'w3m-select-buffer-show-this-line-and-quit)
    (define-key map "\C-c\C-k" 'w3m-select-buffer-quit)
    (define-key map "\C-c\C-q" 'w3m-select-buffer-quit)
    (define-key map "\C-g" 'w3m-select-buffer-quit)
    (define-key map "?" 'describe-mode)
    (setq w3m-select-buffer-mode-map map)))

(defun w3m-select-buffer-mode ()
  "Major mode for switching emacs-w3m buffers using the buffer list.

\\<w3m-select-buffer-mode-map>\
\\[w3m-select-buffer-next-line]\
	Advance to next buffer on the list.
\\[w3m-select-buffer-previous-line]\
	Advance to previous buffer on the list.

\\[w3m-select-buffer-show-this-line-and-switch]\
	Switch to the selected buffer, leaving the list displayed.

\\[w3m-select-buffer-show-this-line]\
	Scroll the selected buffer forward one page.
\\[w3m-select-buffer-show-this-line-and-down]\
	Scroll the selected buffer backward one page.

\\[w3m-select-buffer-copy-buffer]\
	Create a copy of the selected buffer.

\\[w3m-select-buffer-move-next]\
	Move the selected buffer down the list.
\\[w3m-select-buffer-move-previous]\
	Move the selected buffer up the list.

\\[w3m-select-buffer-delete-buffer]\
	Delete the selected buffer.
\\[w3m-select-buffer-delete-other-buffers]\
	Delete all buffers on the list, except for the selected one.

\\[w3m-select-buffer-toggle-style]\
	Toggle the list style between horizontal and vertical.
\\[w3m-select-buffer-toggle-unseen]\
	Toggle the read/unread status of the selected buffer.

\\[w3m-select-buffer-recheck]\
	Refresh the list.

\\[w3m-select-buffer-show-this-line-and-quit]\
	Quit the buffers selection list.
\\[w3m-select-buffer-quit]\
	Quit the buffers selection list."
  (setq major-mode 'w3m-select-buffer-mode
	mode-name "w3m buffers"
	truncate-lines t
	buffer-read-only t)
  (use-local-map w3m-select-buffer-mode-map)
  (run-mode-hooks 'w3m-select-buffer-mode-hook))

(defun w3m-select-buffer-recheck ()
  "Do the roll call to all emacs-w3m buffers and regenerate the menu."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (w3m-select-buffer-generate-contents
   (window-buffer w3m-select-buffer-window))
  (w3m-select-buffer-show-this-line))

(defmacro w3m-select-buffer-current-buffer ()
  '(get-text-property (point-at-bol) 'w3m-select-buffer))

(defun w3m-select-buffer-show-this-line (&optional interactive-p)
  "Show the buffer on the current menu line or scroll it up."
  (interactive (list t))
  (let ((obuffer (and (window-live-p w3m-select-buffer-window)
		      (window-buffer w3m-select-buffer-window)))
	(buffer (w3m-select-buffer-current-buffer)))
    (unless buffer
      (error "No buffer at point"))
    (cond
     ((get-buffer-window buffer)
      (setq w3m-select-buffer-window (get-buffer-window buffer)))
     ((window-live-p w3m-select-buffer-window)
      ())
     ((one-window-p t)
      (setq w3m-select-buffer-window (selected-window))
      (select-window
       (split-window nil
		     (w3m-select-buffer-window-size)
		     w3m-select-buffer-horizontal-window)))
     (t (setq w3m-select-buffer-window (get-largest-window))))
    (set-window-buffer w3m-select-buffer-window buffer)
    (when (and interactive-p (eq obuffer buffer))
      (save-selected-window
	(pop-to-buffer buffer)
	(w3m-scroll-up-or-next-url nil)))
    (w3m-force-window-update w3m-select-buffer-window)
    (w3m-message w3m-select-buffer-message)
    buffer))

(defun w3m-select-buffer-show-this-line-and-down ()
  "Show the buffer on the current menu line or scroll it down."
  (interactive)
  (let ((obuffer (and (window-live-p w3m-select-buffer-window)
		      (window-buffer w3m-select-buffer-window)))
	(buffer (w3m-select-buffer-show-this-line)))
    (when (eq obuffer buffer)
      (save-selected-window
	(pop-to-buffer buffer)
	(w3m-scroll-down-or-previous-url nil)))))

(defun w3m-select-buffer-next-line (&optional n)
  "Move cursor vertically down N lines and show the buffer on the menu."
  (interactive "p")
  (when (if (or (not n) (> n 0))
	    (< (line-end-position) (point-max))
	  (> (line-beginning-position) (point-min)))
    (let ((c (current-column)))
      (forward-line n)
      (move-to-column c))
    (w3m-select-buffer-show-this-line)))

(defun w3m-select-buffer-previous-line (&optional n)
  "Move cursor vertically up N lines and show the buffer on the menu."
  (interactive "p")
  (w3m-select-buffer-next-line (- n)))

(defun w3m-select-buffer-move-next (&optional n event)
  "Move the current buffer down the list (ie. higher number).
Use the prefix argument to move N positions.
EVENT is an internal arg for mouse control."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-command-event))
  (with-current-buffer (w3m-select-buffer-current-buffer)
    (w3m-tab-move-right n event)))

(defun w3m-select-buffer-move-previous (&optional n event)
  "Move the current buffer up the list (ie. lower number).
Use the prefix argument to move N positions.
EVENT is an internal arg for mouse control."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-command-event))
  (with-current-buffer (w3m-select-buffer-current-buffer)
    (w3m-tab-move-right (- n) event)))

(defun w3m-select-buffer-copy-buffer ()
  "Create a copy of the buffer on the current menu line, and show it."
  (interactive)
  (w3m-select-buffer-show-this-line)
  (let ((window (selected-window)))
    (select-window (get-buffer-window (w3m-select-buffer-current-buffer)))
    ;; The selection buffer will be updated automatically because
    ;; `w3m-copy-buffer' calls `w3m-select-buffer-update' by way of
    ;; `w3m-goto-url'.
    (w3m-copy-buffer)
    (select-window window)))

(defun w3m-select-buffer-delete-buffer (&optional force)
  "Delete the buffer on the current menu line.
If only one emacs-w3m buffer exists, it is assumed that the function
was called to terminate the emacs-w3m session.  In this case, the
optional prefix argument FORCE can be set non-nil to exit the session
without prompting for confirmation."
  (interactive "P")
  (let* ((pos (point))
	 (buffer (w3m-select-buffer-current-buffer))
	 (show-next (eq (window-buffer w3m-select-buffer-window) buffer)))
    (if (= 1 (count-lines (point-min) (point-max)))
	(w3m-quit force)
      (w3m-process-stop buffer)
      (w3m-idle-images-show-unqueue buffer)
      (kill-buffer buffer)
      (when w3m-use-form
	(w3m-form-kill-buffer buffer))
      (run-hooks 'w3m-delete-buffer-hook)
      (w3m-select-buffer-generate-contents
       (w3m-select-buffer-current-buffer))
      (goto-char (min pos (point-max)))
      (beginning-of-line)
      (when show-next
	(w3m-select-buffer-show-this-line)))))

(defun w3m-select-buffer-delete-other-buffers ()
  "Delete emacs-w3m buffers except for the buffer on the current menu."
  (interactive)
  (w3m-select-buffer-show-this-line)
  (w3m-delete-other-buffers (w3m-select-buffer-current-buffer)))

(defun w3m-select-buffer-quit ()
  "Quit the buffers selection."
  (interactive)
  (if (one-window-p t)
      (set-window-buffer (selected-window)
			 (or (w3m-select-buffer-current-buffer)
			     (w3m-alive-p)))
    (let ((buf (or (w3m-select-buffer-current-buffer)
		   (w3m-alive-p)))
	  pop-up-frames)
      (pop-to-buffer buf)
      (and (get-buffer-window w3m-select-buffer-name)
	   (delete-windows-on w3m-select-buffer-name))))
  (w3m-set-buffer-seen))

(defun w3m-select-buffer-show-this-line-and-switch ()
  "Show the buffer on the menu and switch to the buffer."
  (interactive)
  (pop-to-buffer (w3m-select-buffer-show-this-line))
  (message nil))

(defun w3m-select-buffer-show-this-line-and-quit ()
  "Show the buffer on the menu and quit the buffers selection."
  (interactive)
  (w3m-select-buffer-show-this-line-and-switch)
  (and (get-buffer-window w3m-select-buffer-name)
       (delete-windows-on w3m-select-buffer-name)))

(defun w3m-select-buffer-close-window ()
  "Close the window which displays the buffers selection."
  (let ((window (get-buffer-window w3m-select-buffer-name)))
    (when window
      (if (one-window-p t)
	  (set-window-buffer window (other-buffer))
	(delete-window window)))))

(defun w3m-select-buffer-toggle-style()
  "Toggle the style of the selection between horizontal and vertical."
  (interactive)
  (w3m-select-buffer t))

(defun w3m-select-buffer-toggle-unseen ()
  "Toggle the read/unread status of a buffer."
  (interactive)
  (if (not (eq major-mode 'w3m-select-buffer-mode))
      (w3m-message "\
This command is only available from the buffer selection pop-up window.")
    (let ((pos (point)))
      (with-current-buffer (w3m-select-buffer-current-buffer)
	(if (w3m-unseen-buffer-p (current-buffer))
	    (w3m-set-buffer-seen)
	  (w3m-set-buffer-unseen)))
      (w3m-select-buffer-generate-contents (current-buffer))
      (goto-char pos))))

(defun w3m-select-buffer-window-size ()
  (if w3m-select-buffer-horizontal-window
      (- (window-width)
	 (/ (* (frame-width) (car w3m-select-buffer-window-ratio)) 100))
    (- (window-height)
       (/ (* (frame-height) (cdr w3m-select-buffer-window-ratio)) 100))))


;;; header line
;; (Where \"header line\" means the first line of a page buffer, not
;;  header-line that `header-line-format' controls.)
(defcustom w3m-use-header-line t
  "Non-nil means display a page location in the beginning of a window.
Insert a text in the page body if `w3m-use-tab' is non-nil, otherwise
use tab-line or heade-line according to `w3m-use-tab-line' to display
a location."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-header-line-title nil
  "Non-nil means display a page title in the beginning of a window.
This variable will be ignored if `w3m-use-tab' is non-nil (because
in such cases the tab line is used for the tab list), otherwise use
tab-line or heade-line according to `w3m-use-tab-line' to display
a title."
  :group 'w3m
  :type 'boolean)

(defface w3m-header-line-title
  '((((class color) (background light))
     (:foreground "Blue" :background "Gray90"))
    (((class color) (background dark))
     (:foreground "Cyan" :background "Gray20")))
  "Face used to highlight title when displaying location in the header line."
  :group 'w3m-face)
(put 'w3m-header-line-location-title 'face-alias 'w3m-header-line-title)

(defface w3m-header-line-content
  '((((class color) (background light))
     (:foreground "DarkGoldenrod" :background "Gray90"))
    (((class color) (background dark))
     (:foreground "LightGoldenrod" :background "Gray20")))
  "Face used to highlight url when displaying location in the header line."
  :group 'w3m-face)
(put 'w3m-header-line-location-content 'face-alias 'w3m-header-line-content)

(defface w3m-header-line-background
  '((((class color) (background light)) (:background "Gray90"))
    (((class color) (background dark)) (:background "Gray20")))
  "Face used to be background of the header line."
  :group 'w3m-face)

(defface w3m-error
  '((((class color) (background light)) (:foreground "Red1" :bold t))
    (((class color) (background dark))  (:foreground "Pink" :bold t))
    (t (:inverse-video t :bold t)))
  "Face used to highlight errors and to denote failure."
  :group 'w3m-face)

(defvar w3m-header-line-map nil)
(unless w3m-header-line-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map w3m-mode-map)
    (define-key map [mouse-2] 'w3m-goto-url)
    ;; Prevent tool-bar from being doubled under GNU Emacs.
    (define-key map [tool-bar] 'undefined)
    (setq w3m-header-line-map map)))

(defun w3m-header-line-insert ()
  "Put the header line into the current buffer."
  (when (and w3m-use-tab
	     w3m-use-header-line
	     w3m-current-url
	     (eq 'w3m-mode major-mode))
    (goto-char (point-min))
    (let ((ct (w3m-arrived-content-type w3m-current-url))
	  (charset (w3m-arrived-content-charset w3m-current-url)))
      (insert (format "Location%s: " (cond ((and ct charset) " [TC]")
					   (ct " [T]")
					   (charset " [C]")
					   (t "")))))
    (w3m-add-face-property (point-min) (point) 'w3m-header-line-title)
    (let ((start (point)))
      (insert (w3m-puny-decode-url
	       (if (string-match "[^\000-\177]" w3m-current-url)
		   w3m-current-url
		 (w3m-url-decode-string w3m-current-url
					w3m-current-coding-system
					"%\\([2-9a-f][0-9a-f]\\)"))))
      (w3m-add-face-property start (point) 'w3m-header-line-content)
      (w3m-add-text-properties
       start (point)
       `(mouse-face highlight keymap ,w3m-header-line-map
		    help-echo "mouse-2 prompts to input URL"))
      (setq start (point))
      (insert-char ?  (max
		       0
		       (- (if (and w3m-select-buffer-horizontal-window
				   (get-buffer-window w3m-select-buffer-name))
			      (frame-width)
			    (window-width))
			  (current-column) 1)))
      (w3m-add-face-property start (point) 'w3m-header-line-content)
      (unless (eolp)
	(insert "\n")))))

;;; w3m-minor-mode
(defcustom w3m-goto-article-function nil
  "Function used to visit an article pointed to by a given URL
in `w3m-minor-mode' buffer.  Normally, this option is used only
when you follow a link in an html article.  A function set to
this variable must take one argument URL, and should display the
specified page.  It may return the symbol `w3m-goto-url' when it
fails displaying the page.  In this case, either `w3m-goto-url'
or `w3m-goto-url-new-session' is employed to display the page."
  :group 'w3m
  :type '(radio (const :tag "Use emacs-w3m" nil)
		(function :value browse-url)))

(defun w3m-safe-view-this-url (&optional force)
  "View the URL of the link under point.
This command is quite similar to `w3m-view-this-url' except for
four differences: [1]don't handle forms, [2]don't consider URL-like
string under the cursor, [3]compare URL with `w3m-safe-url-regexp'
first to check whether it is safe, and [4]the arguments list differs;
the optional FORCE, if it is non-nil, specifies URL is safe.  You
should use this command rather than `w3m-view-this-url' when viewing
doubtful pages that might contain vicious forms.

This command makes a new emacs-w3m buffer if `w3m-make-new-session' is
non-nil and a user invokes this command in a buffer not being running
the `w3m-mode', otherwise use an existing emacs-w3m buffer."
  (interactive "P")
  (let ((w3m-pop-up-windows nil)
	(url (w3m-url-valid (w3m-anchor)))
	safe-regexp)
    (cond
     (url
      (setq safe-regexp (get-text-property (point) 'w3m-safe-url-regexp))
      (if (or (not safe-regexp)
	      (w3m-buffer-local-url-p url)
	      (string-match safe-regexp url)
	      force)
	  (unless (and (functionp w3m-goto-article-function)
		       (not (eq 'w3m-goto-url
				(funcall w3m-goto-article-function url))))
	    (if (and w3m-make-new-session
		     (not (eq major-mode 'w3m-mode)))
		(w3m-goto-url-new-session url)
	      (w3m-goto-url url)))
	(when (w3m-interactive-p)
	  (w3m-message "\
This link is considered to be unsafe; use the prefix arg to view anyway"))))
     ((w3m-url-valid (w3m-image))
      (if (display-images-p)
	  (if (w3m-interactive-p)
	      (call-interactively 'w3m-toggle-inline-image)
	    (w3m-toggle-inline-image force))
	(w3m-view-image)))
     (t (w3m-message "No URL at point")))))

(defun w3m-mouse-safe-view-this-url (event)
  "Perform the command `w3m-safe-view-this-url' by the mouse event."
  ;; Note: a command invoked by [mouse-N] cannot accept the prefix
  ;; argument since [down-mouse-N] eats it.
  (interactive "e")
  (mouse-set-point event)
  (let ((url (w3m-url-valid (or (w3m-anchor) (w3m-image)))))
    (if url
	(let ((safe-regexp (get-text-property (point) 'w3m-safe-url-regexp))
	      (use-dialog-box t))
	  (when (or (not safe-regexp)
		    (w3m-buffer-local-url-p url)
		    (string-match safe-regexp url)
		    (y-or-n-p "\
This link is considered to be unsafe; continue? "))
	    (w3m-safe-view-this-url t)))
      (w3m-message "No URL at point"))))

(defconst w3m-minor-mode-command-alist
  '((w3m-next-anchor)
    (w3m-previous-anchor)
    (w3m-next-image)
    (w3m-previous-image)
    (w3m-toggle-inline-image)
    (w3m-toggle-inline-images)
    (w3m-view-this-url . w3m-safe-view-this-url)
    (w3m-mouse-view-this-url . w3m-mouse-safe-view-this-url)
    (w3m-print-this-url))
  "Alist of commands and commands to be defined in `w3m-minor-mode-map'.
Each element looks like (FROM-COMMAND . TO-COMMAND); those keys which
are defined as FROM-COMMAND in `w3m-mode-map' are redefined as
TO-COMMAND in `w3m-minor-mode-map'.  When TO-COMMAND is nil,
FROM-COMMAND is defined in `w3m-minor-mode-map' with the same key in
`w3m-mode-map'.")

(defun w3m-make-minor-mode-keymap ()
  "Return a keymap used for `w3m-minor-mode'."
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair w3m-minor-mode-command-alist)
      (substitute-key-definition (car pair)
				 (or (cdr pair) (car pair))
				 keymap w3m-mode-map))
    ;; Inhibit the `widget-button-click' command when
    ;; `w3m-handle-non-anchor-buttons' is enabled.
    (define-key keymap [down-mouse-2] 'undefined)
    keymap))

(defvar w3m-minor-mode-map (w3m-make-minor-mode-keymap)
  "*Keymap used when `w3m-minor-mode' is active.")

(defcustom w3m-minor-mode-hook nil
  "Hook run after `w3m-minor-mode' initialization."
  :group 'w3m
  :type 'hook)

(defvar w3m-minor-mode nil "Non-nil if w3m minor mode is enabled.")
(make-variable-buffer-local 'w3m-minor-mode)
(unless (assq 'w3m-minor-mode minor-mode-alist)
  (push (list 'w3m-minor-mode " w3m") minor-mode-alist))
(unless (assq 'w3m-minor-mode minor-mode-map-alist)
  (push (cons 'w3m-minor-mode w3m-minor-mode-map) minor-mode-map-alist))

(defun w3m-minor-mode (&optional arg)
  "Minor mode to view text/html parts in articles."
  (interactive "P")
  (when (setq w3m-minor-mode
	      (if arg
		  (> (prefix-numeric-value arg) 0)
		(not w3m-minor-mode)))
    (run-hooks 'w3m-minor-mode-hook)))

(defcustom w3m-do-cleanup-temp-files nil
  "Whether to clean up temporary files when emacs-w3m shutdown."
  :group 'w3m
  :type 'boolean)

(defvar w3m-display-mode)
(defun w3m-display-mode (style)
  "Select how to display emacs-w3m buffers.

Refer to variable `w3m-display-mode' for details."
  (interactive
   (list
    (intern
     (let ((opts '("nil" "plain" "tabbed" "dual-pane" "frames" "tabbed-frames"))
	   (def (symbol-name w3m-display-mode)))
       (completing-read "Display mode: " opts nil t nil 'opts def)))))
  (setq w3m-display-mode style)
  (let ((vals (pcase style
		;; (w3m-use-tab w3m-pop-up-windows w3m-pop-up-frames)
		('nil		nil)
		('plain		'(nil nil nil))
		('tabbed	'(t   nil nil))
		('dual-pane	'(nil t   nil))
		('frames	'(nil nil t  ))
		('tabbed-frames	'(t   nil t  )))))
    (when vals
      (setq w3m-use-tab (car vals)
	    w3m-pop-up-windows (cadr vals)
	    w3m-pop-up-frames (caddr vals))))
  (if (and w3m-use-tab w3m-use-tab-line)
      (progn
	(require 'tab-line nil t)
	(when (boundp 'tab-line-exclude-modes)
	  (add-to-list 'tab-line-exclude-modes 'w3m-mode)))
    (when (boundp 'tab-line-exclude-modes)
      (setq tab-line-exclude-modes
	    (delq 'w3m-mode tab-line-exclude-modes)))))

(defcustom w3m-display-mode (pcase (list (and w3m-use-tab t)
					 (and w3m-pop-up-windows t)
					 (and w3m-pop-up-frames t))
			      ('(nil nil nil) 'plain)
			      ('(t   nil nil) 'tabbed)
			      ('(nil t   nil) 'dual-pane)
			      ('(t   t   nil) 'tabbed)
			      ('(nil nil t  ) 'frames)
			      ('(t   nil t  ) 'tabbed-frames)
			      ('(nil t   t  ) 'frames)
			      ('(t   t   t  ) 'tabbed-frames))
  "How to display emacs-w3m buffers.

There exist five display modes for emacs-w3m when called
interactively: Plain, Tabbed, Dual-Pane, Dedicated-Frames, and
Tabbed-Dedicated-Frames. When emacs-w3m is run in batch mode or
non-interactively, only the Plain or Tabbed display modes are
available. In the past, these modes had been set by a combination
of three variables, `w3m-use-tab', `w3m-pop-up-windows' and
`w3m-pop-up-frames', but can now be set with this single setting
`w3m-display-mode'. When this variable is set, the other three
are ignored.

PLAIN: Each emacs-w3m buffer exists on its own and can appear in
any window in any frame, although a reasonable attempt will be
made to re-use an existing window. There is no tab bar. In the
past, this had been set by the combination: `w3m-use-tab' nil,
`w3m-pop-up-windows' nil, and `w3m-pop-up-frames' nil.

TABBED: A reasonable attempt is made to keep only one window
displaying emacs-w3m buffers. That window has a cliackable tab
bar along the top. Users can manually subvert this by explicitly
opening an emacs-w3m buffer in any number of other windows. In
the past, this had been set by the combination: `w3m-use-tab' t
`w3m-pop-up-windows' ignored, and `w3m-pop-up-frames' nil.

DUAL-PANE: Once more than one emacs-w3m buffer exists, a
reasonable attempt is made to present emacs-w3m in two windows on
the same frame. Any action to open a new emacs-w3m buffer, such
as `w3m-goto-url-new-session' or `w3m-search-new-session'
displays the new buffer in the unfocused pane, and transfers
focus there. In the past, this had been set by the combination:
`w3m-use-tab' nil `w3m-pop-up-windows' t, and `w3m-pop-up-frames'
nil.

DEDICATED-FRAMES: Each new emacs-w3m buffer is opened in a new
single-window frame. In the past, this had been set by the
combination: `w3m-use-tab' nil `w3m-pop-up-windows' ignored, and
`w3m-pop-up-frames' t.

TABBED-DEDICATED-FRAMES: Each new emacs-w3m buffer is opened in
the same window of the frame from which it was spawned, and is
not easily visible to emacs-w3m buffers associated with other
frames. The window includes a clickable tab bar along the top. In
the past, this had been set by the combination: `w3m-use-tab' t
`w3m-pop-up-windows' ignored, and `w3m-pop-up-frames' t."
  :type '(radio (const :format "Nil " nil)
		(const :format "Plain " plain)
		(const :format "Tabbed " tabbed)
		(const :format "Dual-pane\n" dual-pane)
		(const :format "Dedicated Frames " frames)
		(const :format "Tabbed Dedicated Frames" tabbed-frames))
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (w3m-display-mode value)))

(defun w3m-cleanup-temp-files ()
  (when w3m-do-cleanup-temp-files
    (dolist (f (directory-files w3m-profile-directory t
				"\\`w3m\\(cache\\|el\\|src\\|tmp\\)" t))
      (delete-file f))))

(defun w3m-show-form-hint ()
  "Show sending form hint when the cursor is in a form."
  (when w3m-use-form (w3m-form-unexpand-form))
  (let ((keys (where-is-internal 'w3m-submit-form)))
    (when (and (w3m-submit (point)) keys)
      (if (get-text-property (point) 'w3m-form-readonly)
	  (if (memq (car (w3m-action))
		    '(w3m-form-input w3m-form-input-textarea))
	      (progn
		(w3m-form-expand-form)
		(w3m-message
		 "This form is not editable; type `c' to copy the contents"))
	    (w3m-message "This form is not accessible"))
	(w3m-message "Press %s to send the current form"
		     (key-description (car keys)))))))

;;; Fix melpa installation.
(defvar package-user-dir)
(eval-and-compile
  (defun w3m-fix-melpa-installation (&optional _arg)
    "Generate w3m-load.el if missing so as to fix melpa installation."
    (remove-hook 'after-load-functions #'w3m-fix-melpa-installation)
    (let (dir autoloads archive timestamp hash file)
      ;; Look for the directory where this file is installed.  Note that
      ;; `load-history' referenced here is updated after this file is loaded,
      ;; so this function should run by way of the `after-load-functions' hook.
      (let ((hists load-history) hist)
	(while (and (not dir) (setq hist (car (pop hists)))
		    (string-match "/w3m\\.el\\(?:c\\|\\.[-0-9_a-z]+\\)?\\'"
				  hist)
		    (setq dir (substring hist 0 (match-beginning 0))))))
      (when (and dir
		 (file-exists-p ;; Verify if this file comes from melpa.
		  (setq autoloads (expand-file-name "w3m-autoloads.el" dir)))
		 (require 'package)
		 (file-exists-p (setq archive
				      (expand-file-name
				       "archives/melpa/archive-contents"
				       package-user-dir)))
		 (with-temp-buffer
		   (ignore-errors
		     (insert-file-contents archive)
		     ;; Look for timestamp and revision hash.
		     (let ((def (cdr (assq 'w3m (read (current-buffer))))))
		       (setq timestamp (apply #'format "%s.%s" (aref def 0))
			     hash (cdr (assq :commit (aref def 4)))))))
		 (stringp hash)
		 (string-match "\\`\\([0-9a-z]\\{7\\}\\)[0-9a-z]+\\'" hash)
		 (setq hash (match-string 1 hash)))
	(setq file (expand-file-name "w3m-load.el" dir))
	(or (string-match (concat "/w3m-" (regexp-quote timestamp) "\\'") dir)
	    (and (string-match "/w3m-[0-9]\\{8\\}\\.[0-9]\\{1,4\\}\\'" dir)
		 (display-warning
		  'w3m "You may want to update the emacs-w3m installation")))
	(unless (and (file-exists-p file)
		     (or (boundp 'emacs-w3m-git-revision)
			 (and (load file t t)
			      (boundp 'emacs-w3m-git-revision))))
	  (if (and (file-writable-p dir)
		   (file-writable-p autoloads))
	      ;; Modify w3m-autoloads.el.
	      (let ((buffer (find-file-noselect autoloads)))
		(with-current-buffer buffer
		  (goto-char (point-min))
		  (if (re-search-forward
		       "^(defconst emacs-w3m-git-revision \"\\([^\"]+\\)"
		       nil t)
		      (replace-match hash nil nil nil 1)
		    (when (re-search-forward "^;; Local\040Variables:"
					     nil 'move)
		      (goto-char (match-beginning 0)))
		    (insert "(defconst emacs-w3m-git-revision \"" hash "\"\n"
			    "  \"Git revision string of this package.\")\n\n"
			    "(provide 'w3m-load)\n")
		    (unless (eobp) (insert "\n")))
		  (let ((save-silently t) message-log-max)
		    (save-buffer 0)))
		(kill-buffer buffer)
		(add-name-to-file autoloads file t)
		(message "Created w3m-load.el in %s/"
			 (abbreviate-file-name dir))
		(load file nil t))
	    (defconst emacs-w3m-git-revision hash
	      "Git revision string of this package.")))))))

(defvar byte-compile-current-file)
(eval-and-compile
  (if (featurep 'w3m)
      ;; Try to create w3m-load.el when performing `package-reinstall'
      ;; in the case w3m.elc is already loaded.
      (let ((cur (and (boundp 'byte-compile-current-file)
		      byte-compile-current-file)))
	(when (and (stringp cur)
		   (string-match "/w3m\\.el[^/]*\\'" cur))
	  (let ((load-history (cons (list cur) load-history)))
	    (w3m-fix-melpa-installation))))
    ;; This hook will be removed by `w3m-fix-melpa-installation' itself.
    (add-hook 'after-load-functions #'w3m-fix-melpa-installation)))

(provide 'w3m)

(unless noninteractive
  (when w3m-init-file
    (if (string-match "\\.el\\'" w3m-init-file)
	(or (load (concat w3m-init-file "c") t t t)
	    (load w3m-init-file t t t))
      (load w3m-init-file t t))))

(run-hooks 'w3m-load-hook)

;;; w3m.el ends here
