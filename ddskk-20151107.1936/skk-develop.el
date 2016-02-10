;;; skk-develop.el --- support SKK developper -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-develop.el,v 1.33 2013/10/15 12:49:32 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2013/10/15 12:49:32 $

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
  (require 'skk-vars))

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
