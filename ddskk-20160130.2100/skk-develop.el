;;; skk-develop.el --- support SKK developper -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars)
  (require 'tar-util)
  (require 'url))

(eval-when-compile
  (defvar skk-exserv-list))

;;;###autoload
(defun skk-submit-bug-report ()
  "SKK のバグレポートを書くメールバッファを用意する。
mail-user-agent を設定することにより好みのメールインターフェイスを使用すること
ができる。例えば、Wanderlust を使用したい場合は下記のように設定する。

    \(setq mail-user-agent 'wl-user-agent\) "
  (interactive)
  (require 'reporter)
  (when (skk-y-or-n-p
	 "SKK についてのバグレポートを書きますか？ "
	 "Do you really want to write a bug report on SKK? ")
    (reporter-submit-bug-report
     skk-ml-address
     (concat (skk-version 'with-codename)
	     ", "
	     (cond
	      ((or (and (boundp 'skk-servers-list)
			skk-servers-list)
		   (or (and (boundp 'skk-server-host)
			    skk-server-host)
		       (getenv "SKKSERVER"))
		   ;; refer to DEFAULT_JISYO when skk-server-jisyo is nil.
		   ;;(or (and (boundp 'skk-server-jisyo) skk-server-jisyo)
		   ;;    (getenv "SKK_JISYO"))))
		   )
	       (require 'skk-server)
	       (concat "skkserv; "
		       (skk-server-version)
		       (when (getenv "SKKSERVER")
			 (concat ",\nSKKSERVER; "
				 (getenv "SKKSERVER")))
		       (when (getenv "SKKSERV")
			 (concat ", SKKSERV; "
				 (getenv "SKKSERV")))))
	      ((and (boundp 'skk-exserv-list)
		    skk-exserv-list)
	       (require 'skk-exserv)
	       (skk-server-version))))
     (let ((base (list 'window-system
		       'isearch-mode-hook
		       'isearch-mode-end-hook
		       'skk-auto-okuri-process
		       'skk-auto-start-henkan
		       'skk-egg-like-newline
		       'skk-henkan-okuri-strictly
		       'skk-henkan-strict-okuri-precedence
		       'skk-kakutei-early
		       'skk-process-okuri-early
		       'skk-search-prog-list
		       'skk-share-private-jisyo
		       'skk-use-viper)))
       (when (boundp 'skk-server-host)
	 (setq base (append base '(skk-server-host))))
       (when (boundp 'skk-server-prog)
	 (setq base (append base '(skk-server-prog))))
       (when (boundp 'skk-servers-list)
	 (setq base (append base '(skk-servers-list))))
       (when (boundp 'skk-exserv-list)
	 (setq base (append base '(skk-exserv-list))))
       base)))
  (message ""))

(defvar skk-get-files '("SKK-JISYO.JIS2.gz"
			"SKK-JISYO.JIS2004.gz"
			"SKK-JISYO.JIS3_4.gz"
			"SKK-JISYO.L.gz"
			"SKK-JISYO.assoc.gz"
			"SKK-JISYO.edict.tar.gz"
			"SKK-JISYO.fullname.gz"
			"SKK-JISYO.geo.gz"
			"SKK-JISYO.itaiji.gz"
			"SKK-JISYO.jinmei.gz"
			"SKK-JISYO.law.gz"
			"SKK-JISYO.lisp.gz"
			"SKK-JISYO.mazegaki.gz"
			"SKK-JISYO.okinawa.gz"
			"SKK-JISYO.propernoun.gz"
			"SKK-JISYO.pubdic+.gz"
			"SKK-JISYO.station.gz"
			"zipcode.tar.gz")
  "")

(defun skk-get-delete-files (dir)
  "DIR."
  (let ((files (cons "edict_doc.txt" skk-get-files))
	p)
    (dolist (filename files)
      (dolist (file (list filename
			  (replace-regexp-in-string ".gz" "" filename)
			  (replace-regexp-in-string ".tar.gz" "" filename)))
	(setq p (expand-file-name file dir))
	(when (file-exists-p p)
	  (cond ((null (car (file-attributes p)))
		 (delete-file p))
		(t
		 (delete-directory p)	; GNU Emacs22 まで
;;;		 (delete-directory p t)	; GNU Emacs23 から
		 )))))))

(defun skk-get-mkdir (dir)
  "DIR."
  (if (file-exists-p dir)
      (skk-get-delete-files dir)
    (make-directory dir t)))

(defun skk-get-download (dir)
  "DIR."
  (let ((url "http://openlab.ring.gr.jp/skk/dic/")
	fn)
    (dolist (f skk-get-files)
      (setq fn (expand-file-name f dir))
      (unless (file-exists-p fn)
	(url-copy-file (format "%s%s" url f) fn)))))

(defun skk-get-generate-gzip-d (dir)
  "即席 gzip -d"
  (and (not (executable-find "gzip"))
       (eq system-type 'windows-nt)
       (not (file-exists-p (expand-file-name "gzip-d.ps1" dir)))
       (skk-get-generate-gzip-d-1 dir)))

(defun skk-get-generate-gzip-d-1 (dir)
  "要 powershell"
  (with-temp-buffer
    (insert "$infile = $args[0]" 10)
    (insert "$outfile = ( $infile -replace '\.gz$','' )" 10)
    (insert "$input = New-Object System.IO.FileStream $inFile, ( [IO.FileMode]::Open ), ( [IO.FileAccess]::Read ), ( [IO.FileShare]::Read )" 10)
    (insert "$gzipStream = New-Object System.IO.Compression.GzipStream $input, ( [IO.Compression.CompressionMode]::Decompress )" 10)
    (insert "$output = New-Object System.IO.FileStream $outFile, ( [IO.FileMode]::Create ), ( [IO.FileAccess]::Write ), ( [IO.FileShare]::None )" 10)
    (insert "$buffer = New-Object byte[](1024)" 10)
    (insert "while( $true ) {" 10)
    (insert "  $read = $gzipstream.Read( $buffer, 0, 1024 )" 10)
    (insert "  if ( $read -le 0 ) {break}" 10)
    (insert "  $output.Write( $buffer, 0, $read )" 10)
    (insert "}" 10)
    (insert "$input.Close()" 10)
    (insert "$gzipStream.Close()" 10)
    (insert "$output.Close()" 10)
    (write-region (point-min) (point-max)
		  (expand-file-name "gzip-d.ps1" dir))))

(defun skk-get-expand-gzip (dir)
  "DIR."
  (let* ((ps (convert-standard-filename (expand-file-name "gzip-d.ps1" dir)))
	 (cmd (cond ((executable-find "gzip")
		    "gzip -d")
		    ((file-exists-p ps)
		     (message "skk-get: Use powershell version of the simple gzip.")
		     (format "powershell -executionpolicy remotesigned %s" ps))
		   (t
		    (error "skk-get: gzip command could not be found. Aborts.")))))
    (dolist (f (directory-files dir t ".gz"))
      (let ((fn (convert-standard-filename f)))
	(message "skk-get: expand %s..." fn)
	(shell-command (format "%s %s" cmd fn))
	(when (file-exists-p fn)
	  (delete-file fn))))))

(defun skk-get-expand-tar (dir)
  "DIR."
  ;; (let (fn)
  ;;   (dolist (f (directory-files dir t "tar"))
  ;;     (setq fn (convert-standard-filename f))
  ;;     (shell-command (format "tar -xf %s -C %s && rm %s"
  ;; 			     fn dir fn))))
  (let ((list '(("SKK-JISYO.edict.tar" . "SKK-JISYO.edict")
		("zipcode.tar"         . "SKK-JISYO.zipcode")
		("zipcode.tar"         . "SKK-JISYO.office.zipcode"))))
    (dolist (c list)
      (tar-salvage-file (expand-file-name (car c) dir)
			(cdr c)
			(expand-file-name (cdr c) dir)))))

;;;###autoload
(defun skk-get (dir)
  "DIR."
  (interactive "Dskk-get directory: " )
  (let ((jisyo-dir (if dir
			(expand-file-name dir)
		     (expand-file-name skk-get-jisyo-direcroty))))
    (skk-get-mkdir jisyo-dir)
    (skk-get-download jisyo-dir)
    (skk-get-generate-gzip-d jisyo-dir)
    (skk-get-expand-gzip jisyo-dir)
    (skk-get-expand-tar jisyo-dir))
  (message "skk-get...done")
  nil)

;;;###autoload
(add-hook
 'before-init-hook
 (lambda ()
   (eval-after-load "font-lock"
     ;; `lisp-font-lock-keywords-2' is an alias for `lisp-el-font-lock-keywords-2'.
     ;; `lisp-font-lock-keywords-2' is obsolete since 24.4;
     ;;                             use `lisp-el-font-lock-keywords-2' instead.
     '(set (if (boundp 'lisp-el-font-lock-keywords-2)
	       'lisp-el-font-lock-keywords-2
	     'lisp-font-lock-keywords-2)
	    (nconc
	     (list (list (concat "(\\(\\(skk-\\)?def\\("
				 ;; Function type declarations.
				 "\\(un-cond\\|subst-cond\\|advice\\|"
				 "macro-maybe\\|alias-maybe\\|un-maybe\\)\\|"
				 ;; Variable type declarations.
				 "\\(var\\|localvar\\)"
				 "\\)\\)\\>"
				 ;; Any whitespace and defined object.
				 "[ \t'\(]*"
				 "\\(\\sw+\\)?")
			 '(1 font-lock-keyword-face)
			 '(6 (cond ((match-beginning 4) font-lock-function-name-face)
				   ((match-beginning 5) font-lock-variable-name-face))
			     nil t)))

	     (list (list (concat "("
				 (regexp-opt '("skk-save-point"
					       "skk-with-point-move"
					       "skk-loop-for-buffers")
					     t)
				 "\\>")
			 '(1 font-lock-keyword-face)))

	     (list (list "(\\(skk-error\\)\\>"
			 '(1 font-lock-warning-face)))

	     (symbol-value (if (boundp 'lisp-el-font-lock-keywords-2)
			       'lisp-el-font-lock-keywords-2
			     'lisp-font-lock-keywords-2))

	     )))
   ;;
   (put 'skk-deflocalvar 'doc-string-elt 3)
   (put 'skk-defadvice 'doc-string-elt 3)
   ))

(provide 'skk-develop)

;;; skk-develop.el ends here
