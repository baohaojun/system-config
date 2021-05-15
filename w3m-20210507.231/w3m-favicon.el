;;; w3m-favicon.el --- utilities for handling favicon in emacs-w3m -*- lexical-binding: t -*-

;; Copyright (C) 2001-2005, 2007, 2009, 2011, 2017, 2018, 2019
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
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

;;; Code:

(require 'w3m-image)

(defvar w3m-current-buffer)
(defvar w3m-current-url)
(defvar w3m-favicon-image)
(defvar w3m-icon-data)
(defvar w3m-message-silent)
(defvar w3m-modeline-favicon)
(defvar w3m-profile-directory)
(defvar w3m-use-favicon)
(defvar w3m-work-buffer-name)
(defvar w3m-work-buffer-list)

(declare-function w3m-expand-url "w3m" (url &optional base))
(declare-function w3m-load-list "w3m" (file &optional coding-system))
(declare-function w3m-message "w3m" (&rest args))
(declare-function w3m-retrieve "w3m" (url &optional no-uncompress no-cache
					  post-data referer handler))
(declare-function w3m-save-list "w3m"
		  (file list &optional coding-system escape-ctl-chars))
(declare-function w3m-url-readable-string "w3m" (url))

(defcustom w3m-favicon-size nil
  "Size of favicon.  The value should be `(WIDTH . HEIGHT)' or nil.
Where WIDTH and HEIGHT are positive integers; both or any of them can
be omitted."
  :group 'w3m
  :type '(radio (const :tag "Not specified" nil)
		(cons :format "%v"
		      (integer :format "Width: %v " :value 16 :size 8)
		      (integer :format "Height: %v " :value 16 :size 8))))

(defconst w3m-favicon-name "favicon.ico"
  "The favicon name.")

(add-hook 'w3m-display-functions 'w3m-favicon-setup)

(defcustom w3m-favicon-use-cache-file nil
  "If non-nil, use favicon cache file."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-favicon-cache-file nil
  "Filename of saving favicon cache.
It defaults to the file named \".favicon\" under the directory specified
by the `w3m-profile-directory' variable."
  :group 'w3m
  :type '(radio (const :format "Not specified\n")
		(file :format "%t: %v")))

(defcustom w3m-favicon-cache-expire-wait (* 30 24 60 60)
  "The cache will be expired after specified seconds passed since retrieval.
If this variable is nil, never expired."
  :group 'w3m
  :type 'integer)

(defcustom w3m-favicon-type (when (boundp 'image-types)
			      (catch 'det
				(dolist (type '(gif png pbm svg xpm bmp) nil)
				  (if (image-type-available-p type)
				      (throw 'det type)))))
  "Image type of favicon."
  :group 'w3m
  :type '(radio
	  :convert-widget w3m-widget-type-convert-widget
	  ;; The variable `image-types' will not be available
	  ;; if Emacs was built without the graphics stuff.
	  (if (boundp 'image-types)
	      (let ((types (delq 'imagemagick
				 (delq 'postscript
				       (copy-sequence image-types)))))
		(nconc (mapcar (lambda (x)
				 `(const :format "%v  " ,x))
			       (butlast types))
		       `((const ,(car (last types))))))
	    '((const :match (lambda (widget value) t) nil)))))

(defcustom w3m-space-before-favicon " "
  "String of space char(s) to be put in front of favicon in the mode-line.
It may be better to use two or more spaces if you are using oblique or
italic font in the modeline."
  :group 'w3m
  :type 'string)

(defcustom w3m-favicon-convert-args nil
  "List of additional arguments passed to ImageMagick's convert program.
Args that are always passed to convert in addition to this value are:

(\"-geometry\" \"WIDTHxHEIGHT\" \"fromTYPE:temp-file\" \"toTYPE:-\")

Args might also contain (\"-transparent\" \"COLOR\") in the beginning."
  :group 'w3m
  :type `(repeat (group :format "%v" :inline t
			:match-inline
			(lambda (widget vals)
			  (if (and (eq (aref (car vals) 0) ?-)
				   (cdr vals)
				   (not (eq (aref (nth 1 vals) 0) ?-)))
			      (cons (list (car vals) (nth 1 vals))
				    (nthcdr 2 vals))
			    (cons (list (car vals)) (cdr vals))))
			(string :format "Arg: %v " :value "-")
			(group :format "%v" :inline t
			       (checklist :inline t
					  (string :format "Value: %v"))))))

(defcustom w3m-favicon-default-background nil
  "Color name used as transparent color of favicon image.
Nil means to use the background color of the Emacs frame.  The null
string \"\" is special, that will be replaced with the background color
of the tab line or the mode line on which the favicon is displayed."
  :group 'w3m
  :type '(radio (string :format "Color: %v"
			:match (lambda (widget value)
				 (and (stringp value) (> (length value) 0))))
		(const :tag "Use the background color of the Emacs frame" nil)
		(const :tag "Null string" "")))

(defvar w3m-favicon-type-alist '((pbm . ppm))
  "A cons of what image type Emacs says and its real type generally told.")

(defvar w3m-favicon-cache-data nil
  "A list of favicon cache (internal variable).
Each information is a list whose elements are:

 0. URL
 1. (RAW_DATA . TYPE)
 2. DATE when the RAW_DATA was retrieved
 3. IMAGE

Where IMAGE highly depends on the Emacs version and is not saved in
the cache file.")

(put 'w3m-modeline-favicon 'risky-local-variable t)
(make-variable-buffer-local 'w3m-modeline-favicon)
(make-variable-buffer-local 'w3m-favicon-image)

(defmacro w3m-favicon-cache-p (url)
  "Say whether the favicon data for URL has been chached."
  `(assoc ,url w3m-favicon-cache-data))

(defmacro w3m-favicon-cache-favicon (url)
  "Pull out the favicon image corresponding to URL from the cache."
  `(nth 3 (assoc ,url w3m-favicon-cache-data)))

(defmacro w3m-favicon-cache-retrieved (url)
  "Return the time when the favicon data for URL was retrieved."
  `(nth 2 (assoc ,url w3m-favicon-cache-data)))

(defmacro w3m-favicon-set-image (image)
  "Set IMAGE to `w3m-favicon-image' and `w3m-modeline-favicon'."
  `(when (setq w3m-favicon-image ,image)
     (set 'w3m-modeline-favicon
	  (list ""
		'w3m-space-before-favicon
		(propertize "  " 'display w3m-favicon-image)
		(propertize " " 'display '(space :width 0.5))))))

(defun w3m-favicon-setup (url)
  "Set up the favicon data in the current buffer.  The buffer-local
variable `w3m-favicon-image' will be set to non-nil value when the
favicon is ready."
  (w3m-favicon-set-image nil)
  (when (and w3m-use-favicon
	     w3m-current-url
	     (display-images-p)
	     (image-type-available-p w3m-favicon-type))
    (let (icon)
      (cond
       ((and (string-match "\\`about://\\([^/]+\\)/" url)
	     (setq icon (intern-soft (concat "w3m-about-" (match-string 1 url)
					     "-favicon"))))
	(with-current-buffer w3m-current-buffer
	  (w3m-favicon-set-image
	   (w3m-favicon-convert
	    (base64-decode-string (symbol-value icon)) 'ico))))
       ((or (string-match "\\`\\(https?\\|file\\)://" url)
	    (and (string-match "\\`about://\\(?:header\\|source\\)/https?://"
			       url)
		 (setq url (substring url 15))))
	(if w3m-icon-data
	    (w3m-favicon-retrieve (car w3m-icon-data) (cdr w3m-icon-data)
				  w3m-current-buffer)
	  (w3m-favicon-retrieve (w3m-expand-url (concat "/" w3m-favicon-name)
						url)
				'ico w3m-current-buffer)))))))

(defun w3m-favicon-convert (data type)
  "Convert the favicon DATA in TYPE to the favicon image and return it."
  (when (or (not (eq type 'ico))
	    ;; Is it really in the ico format?
	    (and (>= (length data) 4)
		 (string-equal "\x00\x00\x01\x00" (substring data 0 4)))
	    ;; Some icons named favicon.ico are animated GIFs.
	    (and (>= (length data) 5)
		 (member (substring data 0 5) '("GIF87" "GIF89"))
		 (setq type 'gif)))
    (let ((height (or (cdr w3m-favicon-size) (frame-char-height)))
	  bg args img)
      ;; Examine the transparent color of the image.
      (when (and w3m-imagick-identify-program
		 (equal w3m-favicon-default-background ""))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert data)
	  (let ((coding-system-for-read 'raw-text)
		(coding-system-for-write 'binary))
	    (condition-case nil
		(call-process-region (point-min) (point-max)
				     w3m-imagick-identify-program
				     t t nil "-verbose" (format "%s:-" type))
	      (error)))
	  (goto-char (point-min))
	  (setq case-fold-search t)
	  (while (and (not bg)
		      (re-search-forward "^ *Transparent +color: *\
\\([^\n ]+\\(?: +[^\n ]+\\)*\\)" nil t))
	    (when (string-match "\\`none\\'" (setq bg (match-string 1)))
	      (setq bg nil)))))
      (setq args (list "-geometry"
		       (format "%dx%d"
			       (or (car w3m-favicon-size) height) height)))
      ;; "-transparent" should precede the other arguments.
      (setq args (nconc (when bg (list "-transparent" bg))
			args
			w3m-favicon-convert-args))
      (setq img (apply
		 #'w3m-imagick-convert-data
		 data (symbol-name type)
		 (symbol-name (or (cdr (assq w3m-favicon-type
					     w3m-favicon-type-alist))
				  w3m-favicon-type))
		 args))
      (when img
	(create-image img w3m-favicon-type t
		      :ascent 'center
		      :background w3m-favicon-default-background)))))

(declare-function zlib-available-p "decompress.c")
(declare-function zlib-decompress-region "decompress.c"
		  (start end &optional allow-partial))

(defun w3m-favicon-retrieve (url type target)
  "Retrieve favicon from URL and convert it to image as TYPE in TARGET.
TYPE is a symbol like `ico' and TARGET is a buffer where the image is
stored in the `w3m-favicon-image' buffer-local variable."
  (if (and (w3m-favicon-cache-p url)
	   (or (null w3m-favicon-cache-expire-wait)
	       (< (- (float-time)
		     (float-time (w3m-favicon-cache-retrieved url)))
		  w3m-favicon-cache-expire-wait)))
      (with-current-buffer target
	(w3m-favicon-set-image (w3m-favicon-cache-favicon url)))
    (let ((silent w3m-message-silent))
      (w3m-process-with-null-handler
	(w3m-process-do-with-temp-buffer
	    (ok (w3m-retrieve url 'raw nil nil nil handler))
	  (let ((w3m-message-silent silent)
		idata image)
	    (if (and ok
		     ;; Some broken servers provides empty contents.
		     (>= (buffer-size) 4))
		(progn
		  (when (string-equal "\037\213" (buffer-substring 1 3))
		    (if (and (fboundp 'zlib-available-p) (zlib-available-p))
			(zlib-decompress-region (point-min) (point-max))
		      (let ((coding-system-for-read 'binary)
			    (coding-system-for-write 'binary))
			(shell-command-on-region (point-min) (point-max)
						 "gzip -d" nil t))))
		  (setq idata (buffer-string)
			image (w3m-favicon-convert idata type)))
	      (w3m-message "Reading %s...done (no favicon)"
			   (w3m-url-readable-string url)))
	    (with-current-buffer target
	      (w3m-favicon-set-image image)
	      (push (list url idata (current-time) w3m-favicon-image)
		    w3m-favicon-cache-data)))))))
  ;; Emacs frame needs to be redisplayed to make favicon come out.
  (w3m-force-window-update-later target 1))

(defun w3m-favicon-save-cache-file ()
  "Save the cached favicon data into the local file."
  (when w3m-favicon-use-cache-file
    (w3m-save-list (or w3m-favicon-cache-file
		       (expand-file-name ".favicon" w3m-profile-directory))
		   (delq nil (mapcar (lambda (elem)
				       (when (= (length elem) 4)
					 (butlast elem)))
				     w3m-favicon-cache-data))
		   'binary)))

(defun w3m-favicon-load-cache-file ()
  "Load the cached favicon data from the local file."
  (when (and w3m-favicon-use-cache-file
	     (null w3m-favicon-cache-data))
    (let ((cache (w3m-load-list
		  (or w3m-favicon-cache-file
		      (expand-file-name ".favicon" w3m-profile-directory))
		  'binary))
	  elem data image)
      (while cache
	(setq elem (car cache)
	      cache (cdr cache)
	      data (cadr elem))
	(when (stringp data)
	  (setcar (cdr elem) (setq data (cons data 'ico))))
	(when (setq image (condition-case nil
			      (w3m-favicon-convert (car data) (cdr data))
			    (error nil)))
	  (push (nconc elem (list image)) w3m-favicon-cache-data))))))

(add-hook 'w3m-arrived-setup-functions 'w3m-favicon-load-cache-file)
(add-hook 'w3m-arrived-shutdown-functions 'w3m-favicon-save-cache-file)

(provide 'w3m-favicon)

;;; w3m-favicon.el ends here
