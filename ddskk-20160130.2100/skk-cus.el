;;; skk-cus.el --- SKK の簡単かすたまいず試作品 -*- coding: iso-2022-jp -*-

;; Copyright (C) 2001 SKK Development Team

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

;; SKK 起動後 M-x skk-customize で設定する。

;;; Code:

(eval-when-compile
  (require 'skk-macs))

(require 'skk-vars)
(require 'cus-edit)
(require 'custom)

(defvar skk-custom-params nil)
(defvar skk-custom-alist nil)
(defvar skk-custom-buffer-original nil)
(defvar skk-custom-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map [mouse-1] 'widget-move-and-invoke)
    map))

(defconst skk-cus-params-visual
  '((skk-use-face
     (const :tag "変換中に色をつける" t) "")
    (skk-use-color-cursor
     (const :tag "カーソルに色をつける" t) "")
    (skk-japanese-message-and-error
     (const :tag "メッセージは日本語で通知する" t) "")
    (skk-verbose
     (const :tag "冗長なメッセージを表示する" t) "")
    (skk-show-japanese-menu
     (const :tag "メニューバーを日本語で表示する" t) "")
    (skk-show-annotation
     (radio :tag "変換時に註釈を表示する？"
	    (const :tag "常に表示" t)
	    (const :tag "候補一覧では非表示" (not list))
	    (const :tag "ミニバッファでは非表示" (not minibuf))
	    (const :tag "候補一覧とミニバッファでは非表示"
		   (not list minibuf))
	    (const :tag "非表示" nil))
     "")
    (skk-show-inline
     (radio :tag "候補一覧をインライン表示する？"
	    (const :tag "縦に並べて表示" vertical)
	    (const :tag "一列に表示" t)
	    (const :tag "インライン表示しない" nil))
     "")
    (skk-show-tooltip
     (const :tag "候補一覧・注釈をツールティップで表示する" t) "")
    (skk-show-candidates-always-pop-to-buffer
     (const :tag "候補一覧を別ウインドウを用意して表示する" t) "")))

(defconst skk-cus-params-ui
  '((skk-egg-like-newline
     (const :tag "▼モードでの Return [Enter] キーは確定のみで改行はしない" t)
     "")
    (skk-kakutei-early
     (const :tag "明示的な確定を省略可能にする" t) "")
    (skk-delete-implies-kakutei
     (const :tag "▼モードで BS を押したら確定する" t) "")
    (skk-auto-insert-paren
     (const :tag "閉括弧を自動的に挿入する" t) "")))

(defconst skk-cus-params-henkan
  '((skk-auto-start-henkan
     (const :tag "特定の文字の入力時に自動的に変換を開始する" t) "")
    (skk-henkan-okuri-strictly
     (const :tag "送り仮名が厳密に正しい候補のみ表示する" t) "")
    (skk-henkan-strict-okuri-precedence
     (const :tag "送り仮名が厳密に正しい候補を優先して表示する" t) "")
    (skk-check-okurigana-on-touroku
     (radio :tag "辞書登録時の余計な送り仮名の自動処理は？"
	     (const :tag "自動処理する" auto)
	     (const :tag "ユーザの指示による" ask)
	     (const :tag "自動処理しない" nil))
     "")
    (skk-j-mode-function-key-usage
     (radio :tag "かなモードでファンクションキーを使う？"
	    (const :tag "特殊変換に割り当てる" conversion)
	    (const :tag "使わない" nil))
     "")))

(defconst skk-cus-params-search
  '((skk-use-look
     (const :tag "補完の時に look コマンドを使う" t) "")
    (skk-auto-okuri-process
     (const :tag "送りなし変換で送りあり候補も検索する" t) "")
    (skk-use-numeric-conversion
     (const :tag "数値変換機能を使う" t) "")))

(defconst skk-cus-params-input
  '((skk-use-jisx0201-input-method
     (const :tag "半角カナを入力可能にする" t) "")))

(defconst skk-cus-params-misc
  '((skk-share-private-jisyo
     (const :tag "複数の SKK が個人辞書を共有する" t) "")
    (skk-show-icon
     (const :tag "SKK のアイコンを表示する" t) "")
    (skk-preload
     (const :tag "SKK をあらかじめロードして初回起動を高速にする" t) "")))

(defun skk-custom-mode ()
  (kill-all-local-variables)
  (setq major-mode 'skk-custom-mode
	mode-name "SKK の設定")
  (use-local-map skk-custom-map)
  (when (and (facep 'custom-button-face)
	     (facep 'custom-button-pressed-face))
    (set (make-local-variable 'widget-button-face)
	 'custom-button-face)
    (set (make-local-variable 'widget-button-pressed-face)
	 'custom-button-pressed-face)
    (set (make-local-variable 'widget-mouse-face)
	 'custom-button-pressed-face))
  (when (and (boundp 'custom-raised-buttons)
	     (symbol-value 'custom-raised-buttons))
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))
  (run-hooks 'skk-custom-mode-hook))

(defun skk-cus-info (params)
  (delq nil
	(mapcar (lambda (el)
		  (let ((val (symbol-value (car el))))
		    (if val
			(cons (car el) val)
		      nil)))
		params)))

;;;###autoload
(defun skk-customize-group-skk ()
  (interactive)
  (customize-group 'skk))

;;;###autoload
(defun skk-customize ()
  (interactive)
  (dolist (param (append skk-cus-params-visual
			 skk-cus-params-ui
			 skk-cus-params-henkan
			 skk-cus-params-search
			 skk-cus-params-input
			 skk-cus-params-misc))
    (let ((var (car param)))
      (when (and (eq 'const (caadr param))
		 (symbol-value var))
	(set var t))))
  (setq skk-custom-buffer-original (current-buffer))
  (let (
	(visual (mapcar (lambda (entry)
			  `(cons :format "%v%h\n"
				 :doc ,(nth 2 entry)
				 (const :format "" ,(nth 0 entry))
				 ,(nth 1 entry)))
			skk-cus-params-visual))
	(ui (mapcar (lambda (entry)
		      `(cons :format "%v%h\n"
			     :doc ,(nth 2 entry)
			     (const :format "" ,(nth 0 entry))
			     ,(nth 1 entry)))
		    skk-cus-params-ui))
	(henkan (mapcar (lambda (entry)
			  `(cons :format "%v%h\n"
				 :doc ,(nth 2 entry)
				 (const :format "" ,(nth 0 entry))
				 ,(nth 1 entry)))
			skk-cus-params-henkan))
	(search (mapcar (lambda (entry)
			  `(cons :format "%v%h\n"
				 :doc ,(nth 2 entry)
				 (const :format "" ,(nth 0 entry))
				 ,(nth 1 entry)))
		       skk-cus-params-search))
	(input (mapcar (lambda (entry)
			 `(cons :format "%v%h\n"
				:doc ,(nth 2 entry)
				(const :format "" ,(nth 0 entry))
				,(nth 1 entry)))
		       skk-cus-params-input))
	(misc (mapcar (lambda (entry)
			`(cons :format "%v%h\n"
			       :doc ,(nth 2 entry)
			       (const :format "" ,(nth 0 entry))
			       ,(nth 1 entry)))
		       skk-cus-params-misc))
	(info (append
	       (skk-cus-info skk-cus-params-visual)
	       (skk-cus-info skk-cus-params-ui)
	       (skk-cus-info skk-cus-params-henkan)
	       (skk-cus-info skk-cus-params-search)
	       (skk-cus-info skk-cus-params-input)
	       (skk-cus-info skk-cus-params-misc))))
    (kill-buffer (get-buffer-create "*SKK の基本設定*"))
;;     (switch-to-buffer (get-buffer-create "*SKK の基本設定*"))
    (set-window-buffer (selected-window)
		       (get-buffer-create "*SKK の基本設定*"))
    (set-buffer "*SKK の基本設定*")

    (skk-custom-mode)
    (widget-insert "SKK の基本設定。終わったら ")
    (widget-create 'push-button
		   :tag "done"
		   :help-echo "終わったらボクを押して。"
		   :action 'skk-customize-done)
    (widget-insert " を押してください。\n\n")
    (widget-insert "注意: いくつかの設定は再起動が必要です。\n\n")
    (setq skk-custom-params
	  (list
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "表示に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@visual))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "基本的なユーザ・インターフェース"
				:format "%t:\n%h%v"
				:doc ""
				,@ui))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "変換に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@henkan))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "辞書検索に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@search))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "入力方式に関する設定"
				:format "%t:\n%h%v"
				:doc ""
				,@input))
	   (widget-create 'group
			  :value info
			  `(set :inline t
				:greedy t
				:tag "その他の設定"
				:format "%t:\n%h%v"
				:doc ""
				,@misc))))
    (use-local-map skk-custom-map)
    (local-set-key "q" 'bury-buffer)
    (widget-setup)
    (goto-char (point-min))))

(defun skk-customize-done (&rest args)
  (interactive)
  (setq skk-custom-alist nil)
  (dolist (group skk-custom-params)
    (setq skk-custom-alist (append skk-custom-alist
				   (widget-value group))))
  (dolist (param (append skk-cus-params-visual
			 skk-cus-params-ui
			 skk-cus-params-henkan
			 skk-cus-params-search
			 skk-cus-params-input
			 skk-cus-params-misc))
    (unless (assq (car param) skk-custom-alist)
      (push (cons (car param) nil) skk-custom-alist)))
  (skk-cus-set)
  (bury-buffer)
  (unless (eq skk-custom-buffer-original (current-buffer))
;;      (switch-to-buffer skk-custom-buffer-original))
    (set-window-buffer (selected-window)
		       (get-buffer skk-custom-buffer-original)))
  (skk-adjust-user-option))

;;;###autoload
(defun skk-cus-setup ()
  ;; obsolete
  (when (and (boundp 'skk-custom-file)
	     (ignore-errors
	       (file-readable-p (symbol-value 'skk-custom-file))))
    (let* ((old-name (expand-file-name (symbol-value 'skk-custom-file)))
	   (new-name (concat old-name ".not_used")))
      (load-file old-name)
      (copy-file old-name new-name 'ok-if-already-exists)
      (delete-file old-name))
    (skk-cus-set)))

;;;###autoload
(defun skk-cus-set (&optional alist)
  (unless alist
    (setq alist skk-custom-alist))
  (dolist (param alist)
    (let ((variable (car param))
	  (value (cdr param)))
      (funcall (or (get variable 'custom-set) 'set-default) variable value)
      (put variable 'saved-value (list (custom-quote value)))
      (custom-push-theme 'theme-value variable 'user 'set
			 (custom-quote value))
      (put variable 'customized-value nil)
      (put variable 'customized-variable-comment nil)))
  (custom-save-all)
  ;;
  (when (eq alist skk-custom-alist)
    (setq skk-custom-alist nil)))

(provide 'skk-cus)

;;; skk-cus.el ends here
