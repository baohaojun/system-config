;;; skk-annotation.el --- SKK annotation 関連プログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Copyright (C) 2000-2010  SKK Development Team <skk@ring.gr.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method
;; Created: Oct. 27, 2000.

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

;; これは、SKK 個人辞書に付けたアノテーション (註釈) を活用するプログ
;; ラムです。
;;
;; <HOW TO USE>
;;
;;   (setq skk-show-annotation t)
;;
;; と ~/.skk に書きましょう。辞書の候補に `;' から始まる文字列があれば、
;; その該当の候補が変換されてバッファに出力された際に `;' 以降をその候
;; 補のアノテーションとしてエコーエリアに表示します。
;;
;;   (setq skk-annotation-show-as-message nil)
;;
;; と ~/.skk に書いた場合は、other-window を一時的に開いてアノテーショ
;; ンを表示します。other-window はその候補について確定するか、その候補
;; の選択を止める (次の候補を選択したり、quit したり) すると自動的に閉
;; じられます。
;;
;; SKK では 5 番目の候補以降は候補一覧 (リスト) として表示されるため、ア
;; ノテーションは候補と一緒に纏めで表示されます。
;;
;; ある単語にアノテーションを付けたいときは、確定した直後に同じバッファ
;; で
;;
;;   M-x skk-annotation-add
;;
;; と実行しましょう。アノテーションを編集するバッファが開いて、カレント
;; バッファになりますので、そこへアノテーションを付けましょう。
;; 1 行である必要はありませんが、複数行のアノテーションを付けるとエコー
;; エリアに表示したときに全体が見えなくなります。
;; また、`;' の文字自体は入れる必要はありません。
;; 今までに既に付けていたアノテーションがあれば編集バッファが表示され
;; たときにそのアノテーションが prefix 的に出力されます。既存のアノテー
;; ションも含めて編集して下さい。バッファの先頭行を除いて全ての行が新
;; しいアノテーションとして上書きされます。
;; 編集が終わったら C-c C-c しましょう。
;;
;; 上記の動作でユーザが付けたアノテーションを「ユーザアノテーション」
;; と呼びます。ユーザアノテーションは、
;;
;;   「きかん /期間/機関;*機関投資家/基幹;*基幹業務/」
;;
;; のように `;' の直後に `*' の文字が自動的に振られます。これはユーザ
;; が独自に付けたアノテーションであることを示します (`*' の文字は変換
;; 時には表示されません)。
;;
;; 一方、共有辞書に元々付けられているアノテーションを「システムアノテー
;; ション」と呼び、これは `;' の直後に `*' の文字を伴ないません。
;; <例>
;;    「いぜん /以前;previous/依然;still/」
;;
;; ユーザアノテーションとシステムアノテーションを区別することで、ユー
;; ザアノテーションだけを表示したり、あるいはその逆を行うことが可能
;; です。`skk-annotation-function' に表示したいアノテーションを
;; non-nil と判定する関数を書きましょう。こんな感じです。
;;
;;   (setq skk-annotation-function
;;         (lambda (annotation) (eq (aref annotation 0) ?*)))
;;
;; 上記の例では、アノテーションの先頭が `*' で始まる「ユーザアノテーショ
;; ン」の場合に t を返しますので、ユーザアノテーションだけを表示します。
;;
;; M-x skk-annotation-add したものの、結局アノテーションを付けずに置き
;; たいときは、
;;
;;   M-x skk-annotation-kill
;;
;; して下さい。
;;
;; また、最後に確定した候補についてのアノテーションを取り去りたいとき
;; は、
;;
;;   M-x skk-annotation-remove
;;
;; して下さい。
;;
;; Viper 対策はまだ行っていません。~/.viper に次のように書いて下さい。
;; (viper-harness-minor-mode "skk-annotation")
;;
;; <lookup.el からのアノテーション>
;;
;; 以下の設定を ~/.skk にすると関数 skk-lookup-get-content からアノテー
;; ションが取得されます。
;;
;; (setq skk-annotation-lookup-lookup t)
;;
;; <Apple OS X 「辞書」サービスからのアノテーション>
;;
;; Mac の辞書アプリ (Dictionary.app) では標準で国語辞典など利用できます。
;; このうち優先順位の高い辞書からアノテーションを取得する機能が利用できま
;; す。現状では辞書の検索順は Dictionary.app の環境設定で設定する必要があ
;; ります。
;;
;; この機能を利用する場合は以下の設定を ~/.skk に記述してください。
;; 
;; (setq skk-annotation-lookup-DictionaryServices t)
;;
;; この機能は Carbon Emacs 22 または Cocoa Emacs 23 以降でテストされていま
;; す。ただし Carbon Emacs 22 では変換操作が速すぎる場合に辞書サービスから受け
;; とった文字列のデコードに失敗することがあるようです。
;;
;; <Wikipedia アノテーション>
;;
;; ▼モードにて C-i をタイプすると、表示中の候補を Wikipedia/Wiktionary
;; の項目から探し，見つかった場合は、内容の抜粋をアノテーションとして表示
;; します。この機能は Emacs 22 以上でテストされています。
;;
;; <旧い SKK からの移行>
;;
;; この項はアノテーション機能がない旧い SKK (DDSKK 11.2 以前または SKK
;; 10.62 以前) から最新のものに移行する場合の注意事項です。
;;
;; アノテーションはセパレータとして `;' を使用しているため、`;' の文字
;; を含んだ候補は、eval すると `;' になる Lisp 式として quote し辞書候
;; 補に収める必要があります。
;;
;; まだアノテーション機能を一度も使用していない個人辞書については、以下
;; の S 式を評価した後、
;;
;;   (defun skk-annotation-update-jisyo-format ()
;;     (interactive)
;;     (skk-setup-jisyo-buffer)
;;     (let ((min skk-okuri-ari-min) (max skk-okuri-ari-max))
;;       (skk-annotation-update-jisyo-format-1 min max)
;;       (setq min skk-okuri-nasi-min
;;	     max (point-max))
;;       (skk-annotation-update-jisyo-format-1 min max)))
;;
;;   (defun skk-annotation-update-jisyo-format-1 (min max)
;;     (let (candidate)
;;       (goto-char min)
;;       (while (re-search-forward "\\/\\([^\n/]*;[^\n/]*\\)\\/" max t nil)
;;	 (setq candidate (buffer-substring-no-properties
;;			  (match-beginning 1) (match-end 1)))
;;	 (delete-region (match-beginning 1) (match-end 1))
;;	 (goto-char (match-beginning 1))
;;	 (insert
;;	  (concat "(concat \""
;;		  (mapconcat
;;		   (function
;;		    (lambda (c)
;;		      (if (eq c ?\;)
;;			  "\\073"
;;			(char-to-string c))))
;;		   (append candidate nil) "")
;;		  "\")")))))
;;
;; 個人辞書を読みこみ、辞書を読み込んだバッファで
;;
;;   M-x skk-annotation-update-jisyo-format
;;
;; することでこの作業を行うことができます。
;;
;; 但し、既にアノテーションが付けられている場合は、このアノテーション
;; 自体も候補と区別できずに quote されてしまいますので、ご注意下さい
;; (今のところ手作業で quote されないように退避するなどしか方法はあり
;; ません)。

;;; Code:

(eval-and-compile
  (require 'skk-macs)
  (require 'skk-vars)

  (autoload 'skk-lookup-get-content "skk-lookup")
  (autoload 'run-python "python")
  (autoload 'python-send-command "python")
  (autoload 'python-send-string "python")
  (autoload 'python-check-comint-prompt "python")
  (autoload 'python-proc "python")
  (autoload 'html2text "html2text")
  (autoload 'html2text-delete-tags "html2text")
  (autoload 'url-hexify-string "url-util")
  (autoload 'url-retrieve "url"))

(eval-when-compile
  (require 'compile)
  (require 'comint)
  (defvar python-buffer)
  (defvar python-shell-prompt-regexp)
  (defvar python-shell-buffer-name)
  (defvar mule-version)
  (defvar html2text-remove-tag-list)
  (defvar html2text-format-tag-list))

(when (eval-when-compile (featurep 'xemacs))
  (require 'skk-xemacs))

(unless skk-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'skk-annotation-save-and-quit)
    (define-key map "\C-c\C-k" 'skk-annotation-kill)
    (setq skk-annotation-mode-map map)))

(unless (assq 'skk-annotation-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(skk-annotation-mode " annotation")
			       minor-mode-alist)))

(when (and (boundp 'minor-mode-map-alist)
	   (not (assq 'skk-annotation-mode-map minor-mode-map-alist)))
  (setq minor-mode-map-alist
	(cons (cons 'skk-annotation-mode skk-annotation-mode-map)
	      minor-mode-map-alist)))

;; functions.
(defsubst skkannot-in-normal-buffer-p ()
  (not (or skk-isearch-switch (skk-in-minibuffer-p))))

(defsubst skkannot-clear-msg ()
  (unless (skkannot-in-normal-buffer-p)
    (message nil)))

(defsubst skkannot-dict-exec-find ()
  (ignore-errors (executable-find skk-annotation-dict-program)))

(defun skkannot-check-lookup ()
  (unless (and (locate-library "lookup")
	       (locate-library "skk-lookup")
	       (boundp 'lookup-search-agents)
	       (symbol-value 'lookup-search-agents))
    (setq skk-annotation-other-sources
	  (delq 'lookup.el skk-annotation-other-sources))))

(defun skk-annotation-erase-buffer ()
  (let ((inhibit-read-only t)
	buffer-read-only)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)))

(defun skk-annotation-insert (annotation)
  "`skk-annotation-buffer' に ANNOTATION を挿入する"
  (with-current-buffer (get-buffer-create skk-annotation-buffer)
    (skk-annotation-erase-buffer)
    (setq buffer-read-only nil)
    (insert annotation)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

;;;###autoload
(defun skk-annotation-get (annotation)
  (cond ((string= annotation "")
	 "")
	((eq (aref annotation 0) ?*)
	 (substring annotation 1))
	(t
	 annotation)))

(defun skkannot-sit-for (seconds &optional listing-p)
  (condition-case nil
      (sit-for seconds)
    (quit
     (with-current-buffer skkannot-buffer-origin
       (cond
	(listing-p
	 (skk-escape-from-show-candidates 0))
	(t
	 (keyboard-quit)))))))

;;; アノテーション表示機能
;;;###autoload
(defun skk-annotation-find-and-show (pair)
  "各種リソースからアノテーションを取得し表示する。"
  (skkannot-check-lookup)
  (skkannot-clear-msg)
  ;;
  (let ((word (car-safe pair))
	(note (cdr-safe pair))
	(srcs skk-annotation-other-sources)
	list)
    (when skk-annotation-first-candidate
      (setq skkannot-remaining-delay skk-annotation-delay
	    skk-annotation-first-candidate nil))
    (when (and (skkannot-in-normal-buffer-p)
	       word
	       (or skk-annotation-lookup-lookup
		   skk-annotation-lookup-DictionaryServices
		   (and skk-annotation-lookup-dict
			(skkannot-dict-exec-find))))
      ;; Mac の「辞書」あるいは dict (外部プログラム) の設定があれば
      ;; SKK 辞書のアノテーションより優先させる
      (setq note nil)
      (when skk-annotation-lookup-lookup
	(setq note (skk-lookup-get-content word)))
      (when (and (null note)
		 (eq system-type 'darwin)
		 skk-annotation-lookup-DictionaryServices)
	(catch '辞書
	  (setq note (skk-annotation-lookup-DictionaryServices word))))
      (when (and (null note)
		 skk-annotation-lookup-dict
		 (skkannot-dict-exec-find))
	(catch 'dict
	  (setq note (skk-annotation-lookup-dict word))
	  ;; 余裕があれば次候補の意味を先読み
	  (dotimes (i (min (length skk-henkan-list) 4))
	    (add-to-list 'list (nth i skk-henkan-list) t))
	  (when list
	    (dolist (el list)
	      (setq el (car (skk-treat-strip-note-from-word el)))
	      (unless (equal word el)
		(skk-annotation-preread-dict el))))))
      (unless note
	(setq note (cdr-safe pair))))
    (when (and word (not note))
      ;; Wikipedia などその他のリソースからのキャッシュがあれば
      ;; それを表示する。
      (unless skkannot-cached-srcs
	(while srcs
	  (unless (memq (car srcs) '(lookup.el))
	    (add-to-list 'skkannot-cached-srcs (car srcs) t))
	  (setq srcs (cdr srcs))))
      (setq note (car (skkannot-cache word skkannot-cached-srcs))))
    ;;
    (setq skkannot-buffer-origin (current-buffer))
    (cond
     ((or (<= skkannot-remaining-delay 0)
	  (skkannot-sit-for skkannot-remaining-delay))
      (setq skkannot-remaining-delay 0)
      (skk-annotation-show (or note "") word
			   (unless skk-annotation-lookup-lookup
			     skkannot-cached-srcs)))
     (t
      (setq skkannot-remaining-delay skk-annotation-delay)
      (skk-annotation-show "" word (unless skk-annotation-lookup-lookup
				     skkannot-cached-srcs))))))

;;;###autoload
(defun skk-annotation-show (annotation &optional word sources)
  (when (and (not skk-kakutei-flag)
	     (or (not skk-annotation-function)
		 (funcall skk-annotation-function annotation)))
    (setq annotation (skk-annotation-get annotation))
    (let ((notes (mapcar #'skk-eval-string (split-string annotation ";")))
	  (inhibit-wait skk-isearch-switch))
      (setq annotation (skk-eval-string annotation))
      (unless (string= annotation "")
	(setq inhibit-wait (skk-annotation-show-2 annotation)))
      ;; 注釈の表示はここまでだが、ここでユーザが注釈の内容をコピーしたり
      ;; して利用できるようにする。
      (unless inhibit-wait
	(skk-annotation-wait-for-input annotation notes word sources)))))

(defun skk-annotation-show-2 (annotation)
  (let (inhibit-wait)
    (cond (skk-isearch-switch
	   ;; do nothing
	   (setq inhibit-wait t))
	  ((and (not (skk-annotation-display-p 'minibuf))
		(skk-in-minibuffer-p))
	   ;; do nothing
	   (setq inhibit-wait t))
	  ((and window-system skk-show-tooltip)
	   (skk-tooltip-show-at-point annotation 'annotation))
	  ((and skk-annotation-show-as-message
		(skkannot-in-normal-buffer-p))
	   (skk-annotation-show-as-message annotation))
	  (t
	   (skk-annotation-show-buffer annotation)))
    inhibit-wait))

(defun skk-annotation-show-buffer (annotation)
  (condition-case nil
      (save-window-excursion
	(let ((minibuf-p (skk-in-minibuffer-p))
	      event window)
	  (skk-annotation-insert annotation)
	  (cond (minibuf-p
		 (if (setq window (get-buffer-window (skk-minibuffer-origin)))
		     (select-window window)
		   (other-window 1))
		 (unless (eq (next-window) (selected-window))
		   (delete-other-windows)))
		(t
		 (split-window-vertically)))
	  ;;
	  (skk-fit-window (display-buffer skk-annotation-buffer))
	  (when minibuf-p
	    (select-window (minibuffer-window)))
	  ;;
	  (skk-annotation-message 'annotation)
	  ;;
	  (setq event (next-command-event))
	  (when (skk-key-binding-member (skk-event-key event)
					'(key-board-quit
					  skk-kanagaki-bs
					  skk-kanagaki-esc)
					skk-j-mode-map)
	    (signal 'quit nil))
	  (skk-unread-event event)))
    (quit
     ;; skk-previous-candidate へ
     (skk-reset-henkan-count 0))))

(defun skk-annotation-show-as-message (annotation)
  (message "%s" annotation))

;;;###autoload
(defun skk-annotation-display-p (test)
  ;; TEST は 'list 又は 'minibuf
  (skkannot-clear-msg)
  ;;
  (cond ((null skk-show-annotation)
	 nil)
	((and (listp skk-show-annotation)
	      (eq (car skk-show-annotation) 'not)
	      ;; (not ...)
	      (memq test skk-show-annotation))
	 ;; (not list), (not minibuf) or (not list minibuf)
	 nil)
	(t
	 ;; non-nil
	 t)))

;;;###autoload
(defun skk-annotation-toggle-display-p ()
  (interactive)
  (cond ((null skk-show-annotation)
	 ;; do nothing
	 nil)
	((and (listp skk-show-annotation)
	      (eq (car skk-show-annotation) 'not))
	 ;; (not ...)
	 (cond ((memq 'list skk-show-annotation)
		(if (eq (length skk-show-annotation) 2)
		    ;; (not list) -> t  i.e. turn on
		    (setq skk-show-annotation t)
		  ;; (not list minibuf) -> (not minibuf)
		  (setq skk-show-annotation '(not minibuf))))
	       (t
		;; (not minibuf) -> (not list minibuf)  i.e. turn off
		(setq skk-show-annotation '(not list minibuf)))))
	(t
	 ;; non-nil -> (not list)  i.e. turn off
	 (setq skk-show-annotation '(not list)))))

;;; アノテーション操作機能
(defun skk-annotation-setup ()
  (let ((skk-henkan-key (skk-get-last-henkan-datum 'henkan-key))
	(skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
	(cand (car (skk-get-last-henkan-datum 'henkan-list)))
	word)
    (unless cand
      (setq skk-henkan-key
	    (read-from-minibuffer "Midasi: "))
      (when (string= skk-henkan-key "")
	(skk-error "アノテーションする単語がありません"
		   "No word to be annotated"))
      (when (string-match "\\cj\\([a-z]+\\)$"
			  skk-henkan-key)
	(setq skk-okuri-char (match-string 1 skk-henkan-key)
	      ;; 送りあり変換を指定すると
	      ;; skk-henkan-okurigana の指定に困る。
	      skk-henkan-okurigana ""))
      (setq cand
	    (prog1
		(skk-henkan-in-minibuff)
	      (setq skk-kakutei-flag nil))))
    ;; この時点では skk-num-list は既に nil
    ;; ミニバッファから対象を指定した場合には consp にならない
    (when (consp cand)
      (setq cand (car cand)))
    (setq word (car (skk-treat-strip-note-from-word cand)))
    (when (and (string-match "[0-9]" skk-henkan-key)
	       (or (string-match "#[0-9]" word)
		   (skk-lisp-prog-p word)))
      (setq skk-henkan-key
	    (skk-num-compute-henkan-key skk-henkan-key)))
    (setq skk-annotation-target-data
	  (list skk-henkan-key
		skk-okuri-char
		cand))
    ;; 意図を理解してないが、skk-kakutei-initialize のほうが適切な気も
    (skk-kakutei)))

;;;###autoload
(defun skk-annotation-add (&optional no-previous-annotation)
  "最後に確定した語に annotation を付ける。
既に annotation が付けられていれば、それを編集バッファに出力する。
NO-PREVIOUS-ANNOTATION を指定 (\\[Universal-Argument] \\[skk-annotation-add])
すると、既に付けられている annotation を編集バッファに出力しない。"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (let* ((plist (append
		   '(intangible t read-only t)
		   (if (featurep 'xemacs)
		       '(start-closed t end-open t)
		     '(front-sticky t rear-nonsticky t))))
	   (wholestring (nth 2 skk-annotation-target-data))
	   (realword (if (and wholestring
			      (string-match ";\\*?" wholestring))
			 (substring wholestring 0 (match-beginning 0))
		       wholestring))
	   (annotation (if (and realword
				(string-match ";\\*?" wholestring))
			   (substring wholestring (match-end 0))
			 nil)))
      (setq skk-annotation-original-window-configuration
	    (current-window-configuration))
      (delete-other-windows)
;;       (split-window-vertically)
;;       (other-window 1)
;;       (switch-to-buffer (get-buffer-create skk-annotation-buffer))
      (pop-to-buffer skk-annotation-buffer)

      (setq buffer-read-only nil
	    skk-annotation-mode t)
      (skk-annotation-erase-buffer)
      (insert
       (format "\
;; Add a note to word `%s' (this line will not be added to the note.)
"
	       realword))
      (add-text-properties (point-min) (1- (point)) plist)
      (when (and (not no-previous-annotation)
		 annotation)
	(insert annotation))
      (run-hooks 'skk-annotation-mode-hook)
      (message "%s to save edits, %s to just kill this buffer"
	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-save-and-quit
					     skk-annotation-mode-map)
			  ", ")

	       (mapconcat 'key-description
			  (where-is-internal 'skk-annotation-kill
					     skk-annotation-mode-map)
			  ", ")))))

(defun skk-annotation-save-and-quit (&optional quiet)
  "最後に確定した語に annotation を付けて annotation バッファを閉じる。"
  ;; called in the annotation buffer.
  (interactive "P")
  (let (annotation)
    (save-match-data
      (with-current-buffer (get-buffer-create skk-annotation-buffer)
	(goto-char (point-min))
	(when (looking-at ";; Add a note to word") ; 中途半端
	  (forward-line 1)
	  (beginning-of-line))
	(setq annotation (buffer-substring-no-properties
			  (point) (point-max)))
	(when (string-match "^[\t\n 　]+" annotation)
	  (setq annotation (substring annotation (match-end 0))))
	(when (string-match "[\t\n 　]+$" annotation)
	  (setq annotation (substring annotation 0 (match-beginning 0))))
	(when (string= annotation "")
	  (setq annotation nil))
	(setq annotation (skk-quote-char annotation))))
    (if annotation
	(skk-annotation-last-word-1
	 (lambda (beg end)
	   (goto-char beg)
	   (when (re-search-forward ";[^/]*" end t)
	     (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char end)
	   (insert ";*" annotation)))
      ;; 削除した時
      (let ((old-annotation
	     (cdr (skk-treat-strip-note-from-word
		   (nth 2 skk-annotation-target-data)))))
	(when (and old-annotation
		   (yes-or-no-p
		    (format (if skk-japanese-message-and-error
				"既存のアノテーション `%s' を削除しますか？ "
			      "Delete old annotation `%s' ? ")
			    (skk-annotation-get old-annotation))))
	  (skk-annotation-last-word-1
	   (lambda (beg end)
	     (goto-char beg)
	     (when (re-search-forward ";[^/]*" end t)
	       (delete-region (match-beginning 0) (match-end 0))))))))
    (skk-annotation-erase-buffer)
    (kill-buffer (current-buffer))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (when annotation
      (unless quiet
	(message "%s" "Added annotation")))))

(defun skk-annotation-kill ()
  "annotation を付けずに annotation バッファを kill する。"
  ;; called in the annotation buffer.
  (interactive)
  (skk-annotation-erase-buffer)
  (kill-buffer (current-buffer))
  (set-window-configuration
   skk-annotation-original-window-configuration))

;;;###autoload
(defun skk-annotation-remove ()
  "最後に確定した語から annotation を取り去る。"
  (interactive)
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (when (yes-or-no-p
	   (format (if skk-japanese-message-and-error
		       "%s についてのアノテーションを削除しますか？ "
		     "Really delete annotation for %s? ")
		   (nth 2 skk-annotation-target-data)))
      (skk-annotation-last-word-1
       (lambda (beg end)
	 (goto-char beg)
	 (when (re-search-forward ";[^/]*" end t)
	   (delete-region (match-beginning 0) (match-end 0))))))))

(defun skk-annotation-last-word-1 (function)
  ;; funcall FUNCTION with BEG and END where BEG and END are markers.
  (let ((inhibit-quit t)
	(jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(word (nth 2 skk-annotation-target-data))
	(beg (make-marker))
	(end (make-marker))
	(eol (make-marker))
	pattern)
    (when (buffer-live-p jisyo-buffer)
      (save-match-data
	(with-current-buffer jisyo-buffer
	  (goto-char (if (nth 1 skk-annotation-target-data)
			 skk-okuri-ari-min
		       skk-okuri-nasi-min))
	  (when (re-search-forward
		    (concat "^\\("
			    (regexp-quote (car skk-annotation-target-data))
			    "\\) /")
		    (if (nth 1 skk-annotation-target-data)
			skk-okuri-ari-max nil)
		    t nil)
	    (goto-char (match-beginning 1))
	    (set-marker eol (line-end-position))
	    (when (string-match ";" word)
	      (setq word (substring word 0 (match-beginning 0))))
	    (when (re-search-forward
		   (concat "/\\(" word "\\)\\(;[^/]*\\)*/")
		   eol t nil)
	      (set-marker beg (match-beginning 1))
	      (set-marker end (or (match-end 2) (match-end 1)))
	      (funcall function beg end)
	      (when (nth 1 skk-annotation-target-data)
		(goto-char end)
		;; skip other candidates that has not a okuirigana.
		(search-forward "/[" eol t nil)
		(setq pattern (concat "/\\(" word "\\)\\(;[^/]*\\)*/"))
		(while (re-search-forward pattern eol t nil)
		  (set-marker beg (match-beginning 1))
		  (set-marker end (or (match-end 2)
				      (match-end 1)))
		  (funcall function beg end)))
	      (set-marker beg nil)
	      (set-marker end nil)
	      (set-marker eol nil))))))))

;;;###autoload
(defun skk-annotation-quote (&optional quiet)
  "最後に確定した語に含まれる `;' を候補の一部として quote する。"
  (interactive "P")
  (skk-kakutei)
  (skk-annotation-setup)
  (let (candidate)
    (skk-annotation-last-word-1
     (lambda (beg end)
       (goto-char beg)
       (setq candidate (buffer-substring-no-properties beg end))
       (when (string-match ";" candidate)
	 (delete-region beg end)
	 (insert (skk-quote-semicolon candidate))
	 (unless quiet
	   (message "%s" "Quoted")))))))

;;; アノテーション UI 拡張機能
(defun skk-annotation-wait-for-input (annotation notes &optional word sources)
  "アノテーション表示時にキー入力を捕捉する。
キー入力の内容によってアノテーションのコピー、情報源 URL のブラウズ、または
別の情報源からの意味取得を行う。"
  (let* ((copy-command (key-binding skk-annotation-copy-key))
	 (browse-command (key-binding skk-annotation-browse-key))
	 (list (list copy-command browse-command))
	 event key command urls note cache char digit exit)
    (while (and (not exit)
		list
		(or (memq this-command
			  '(skk-annotation-wikipedia-region-or-at-point
			    skk-annotation-lookup-region-or-at-point))
		    (eq skk-henkan-mode 'active))
		(if digit
		    t
		  (skk-annotation-message (if (and annotation
						   (> (length annotation) 0))
					      'annotation
					    nil)))
		(condition-case nil
		    (progn
		      (setq event (next-command-event)
			    key (skk-event-key event)
			    command (key-binding
				     (if (featurep 'xemacs) event key)))
		      ;; Return value of the following expression is important.
		      (or (memq command list)
			  (eq command 'digit-argument)
			  (memq command
				'(skk-annotation-wikipedia-region-or-at-point
				  skk-annotation-lookup-region-or-at-point))
			  (equal (key-description key)
				 (key-description
				  skk-annotation-wikipedia-key))))
		  (quit
		   (when (eval-when-compile (and (featurep 'xemacs)
						 (= emacs-major-version 21)
						 (= emacs-minor-version 4)))
		     ;; workaround for XEmacs 21.4
		     (keyboard-quit)))))
      (cond ((eq command copy-command)
	     (setq list (delq copy-command list))
	     (unless (equal annotation "")
	       (kill-new (substring-no-properties annotation))
	       (skk-message "現在の注釈をコピーしました"
			    "Copying the current note...done")
	       (setq event nil
		     digit nil
		     char  nil)
	       (skk-annotation-show-2 annotation)))
	    ((eq command browse-command)
	     (setq list (delq browse-command list))
	     (setq urls nil)
	     (when (and word (setq cache (skkannot-cache word sources)))
	       (let ((url (cond
			   ((string= (cdr cache) "dict")
			    (format "dict:///%s" word))
			   ((string= (cdr cache) "lookup.el")
			    (list 'lookup-word word))
			   (t
			    (apply #'skkannot-generate-url
				   "http://%s.org/wiki/%s"
				   (cdr (split-string (cdr cache) " " t)))))))
		 (when url
		   (setq urls (cons url urls)))))
	     (unless (equal annotation "")
	       (cond
		(urls
		 (dolist (url urls)
		   (cond ((consp url)
			  (setq exit t)
			  (apply (car url) (cdr url)))
			 (t
			  (browse-url url))))
		 (skk-message "注釈のソースをブラウズしています..."
			      "Browsing originals for the current notes..."))
		(t
		 (skk-message "注釈のソースが見つかりません"
			      "No originals found for the current notes")))
	       (setq event nil
		     digit nil
		     char  nil)
	       (unless exit
		 (skk-annotation-show-2 annotation))))
	    ((eq command 'digit-argument)
	     (setq char  (cond ((featurep 'xemacs)
				key)
			       ((integerp event)
				event)
			       (t
				(get event 'ascii-character)))
		   digit (- (logand char ?\177) ?0)
		   event nil))
	    ((or (equal (key-description key)
			(key-description skk-annotation-wikipedia-key))
		 (memq command
		       '(skk-annotation-wikipedia-region-or-at-point
			 skk-annotation-lookup-region-or-at-point)))
	     (setq sources
		   (if (and digit
			    (> digit 0)
			    (<= digit
				(length skk-annotation-other-sources)))
		       (list (nth (1- digit)
				  skk-annotation-other-sources))
		     skk-annotation-other-sources))
	     (setq event nil
		   digit nil
		   char  nil)
	     (when word
	       (setq note (skk-annotation-wikipedia word sources)))
	     (cond ((null note)
		    (setq note annotation))
		   (t
		    (setq annotation note)))
	     (unless (equal note "")
	       (add-to-list 'list browse-command)
	       (add-to-list 'list copy-command)
	       (skk-annotation-show-2 (or note annotation))))
	    (t
	     (setq list nil))))
    (when event
      (skk-unread-event event))))

;;;###autoload
(defun skk-annotation-message (&optional situation)
  (when (and skk-verbose (skkannot-in-normal-buffer-p))
    (unless skk-annotation-wikipedia-message
      (let ((key (key-description skk-annotation-wikipedia-key))
	    (string "")
	    list new)
	(when (equal key "TAB")
	  (setq key "C-i"))
	(setq list
	      (delete ""
		      (split-string
		       (dotimes (i (length skk-annotation-other-sources) string)
			 (setq string
			       (format "%s[C-%d %s]%s  "
				       string
				       (1+ i)
				       key
				       (nth i skk-annotation-other-sources))))
		       "  ")))
	(dolist (x list)
	  (let* ((y (split-string x "]"))
		 (s1 (car y))
		 (s2 (nth 1 y)))
	    (setq new (concat new
			      (propertize (concat s1 "]") 'face
					  'skk-verbose-kbd-face)
			      s2 " "))))
	(setq skk-annotation-wikipedia-message
	      (concat (propertize "{どれを参照?}" 'face
				  'skk-verbose-intention-face)
		      new))))
    ;;
    (unless skk-annotation-message
      (let ((key-copy (or (key-description skk-annotation-copy-key)
			  "未定義"))
	    (key-wiki (or (key-description skk-annotation-wikipedia-key)
			  "未定義"))
	    (key-browse (or (key-description skk-annotation-browse-key)
			    "未定義"))
	    list new)
	(when (equal key-wiki "TAB")
	  (setq key-wiki "C-i"))
	(setq list
	      (split-string
	       (format "[%s]コピー  [%s]ブラウズ  [%s]デフォルトのソースを参照"
		       key-copy key-browse key-wiki) "  "))
	(dolist (x list)
	  (let* ((y (split-string x "]"))
		 (s1 (car y))
		 (s2 (nth 1 y)))
	    (setq new (concat new
			      (propertize (concat s1 "]") 'face
					  'skk-verbose-kbd-face)
			      s2 " "))))
	(setq skk-annotation-message
	      (concat (propertize "{アノテーション}" 'face
				  'skk-verbose-intention-face)
		      new))))
    ;;
    (condition-case nil
	(cond ((eq situation 'annotation)
	       (if (sit-for skk-verbose-wait)
		   (let ((i 0))
		     (catch 'loop
		       (while (< i 20)
			 (message "%s" skk-annotation-message)
			 (unless (sit-for skk-verbose-message-interval)
			   (throw 'loop nil))
			 (message "%s" skk-annotation-wikipedia-message)
			 (unless (sit-for skk-verbose-message-interval)
			   (throw 'loop nil))
			 (setq i (1+ i))))
		     (message nil))
		 nil))
	      ;;
	      (t
	       (when (sit-for skk-verbose-wait)
		 (message "%s" skk-annotation-wikipedia-message))))
      (quit
       (when (eq skk-henkan-mode 'active)
	 (skk-reset-henkan-count 0)))))
  ;; 常に t を返す
  t)

;;;###autoload
(defun skk-annotation-lookup-region-or-at-point (&optional prefix-arg
							   start end)
  "選択領域またはポイント位置の単語を辞書で調べる。
辞書としては lookup.el、Apple OS X の辞書サービス、Wikipedia/Wikitionary などが
利用される。

領域が選択されていなければ単語の始めと終わりを推測して調べる。

調べた結果を `skk-annotation-show-as-message' が Non-nil であればエコーエリア
に、nil であれば別 window に表示する。"
  (interactive (cons (prefix-numeric-value current-prefix-arg)
		     (cond
		      ((skk-region-active-p)
		       (list (region-beginning) (region-end)))
		      ((eq skk-henkan-mode 'on)
		       (list (marker-position skk-henkan-start-point)
			     (point)))
		      (t
		       ;; dummy
		       (list 1 1)))))
  (skkannot-check-lookup)
  (skkannot-clear-msg)
  ;;
  (let ((word (if (and (= start 1) (= end 1))
		  ;; region が active でないときは，ポイントにある
		  ;; 単語を推測する
		  (thing-at-point 'word)
		(buffer-substring-no-properties start end)))
	(sources
	 (if (and current-prefix-arg
		  (> prefix-arg 0)
		  (<= prefix-arg (length skk-annotation-other-sources)))
	     (list (nth (1- prefix-arg) skk-annotation-other-sources))
	   skk-annotation-other-sources))
	note)
    (when (and word
	       (> (length word) 0))
      (setq note (or (car (skkannot-cache word sources))
		     (skk-annotation-wikipedia word sources)))
      (skk-annotation-show (or note "") word sources))))


;;; Apple OS X 辞書サービス関連機能
(defsubst skkannot-DictServ-command (word)
  (format skkannot-DictServ-cmd-format-str word "%" "%"))

(defsubst skkannot-dict-buffer-format (word)
  "dict の内容を格納するバッファのフォーマット。"
  (format "  *skk dict %s" word))

(declare-function comint-send-string "comint")
(defun skkannot-py-send-string (string)
  "Evaluate STRING in inferior Python process."
  (require 'comint)
  (let ((proc (get-buffer-process skkannot-py-buffer)))
    (comint-send-string proc string)
    (unless (string-match "\n\\'" string)
      ;; Make sure the text is properly LF-terminated.
      (comint-send-string proc "\n"))
    (when (string-match "\n[ \t].*\n?\\'" string)
      ;; If the string contains a final indented line, add a second newline so
      ;; as to make sure we terminate the multiline instruction.
      (comint-send-string proc "\n"))))

(eval-and-compile
  (defsubst skkannot-emacs-24_3-or-later ()
    (or (> emacs-major-version 24)
	(and (= emacs-major-version 24)
	     (>= emacs-minor-version 3)))))

(defun skkannot-py-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (cond
   ((eval-when-compile (skkannot-emacs-24_3-or-later))
    (with-current-buffer (process-buffer (or proc (python-proc)))
      (save-excursion
	(save-match-data
	  (re-search-backward (concat python-shell-prompt-regexp " *\\=")
			      nil t)))))
   (t
    (python-check-comint-prompt))))

(declare-function compilation-forget-errors "compile")
(defun skkannot-py-send-command (command)
  "Like `skkannot-py-send-string' but resets `compilation-shell-minor-mode'."
  (when (or (eval-when-compile (and (featurep 'emacs)
				    (= emacs-major-version 22)))
	    (skkannot-py-check-comint-prompt (get-buffer-process
					      skkannot-py-buffer))
	    t)
    (with-current-buffer skkannot-py-buffer
      (goto-char (point-max))
      (compilation-forget-errors)
      (skkannot-py-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

;;;###autoload
(defun skk-annotation-start-python (&optional wait)
  "OS X の「辞書」を利用するために python を起動する。"
  (require 'python)
  (cond
   ((buffer-live-p skkannot-py-buffer)
    skkannot-py-buffer)
   (t
    ;; python + readline で UTF-8 の入力をするために LANG の設定が必要。
    (let* ((env (getenv "LANG"))
	   python-buffer orig-py-buffer)
      (unless (eval-when-compile (skkannot-emacs-24_3-or-later))
	;; Emacs 24.2 or earlier
	(setq orig-py-buffer (default-value 'python-buffer)))
      (unless (equal env "Ja_JP.UTF-8")
	(setenv "LANG" "ja_JP.UTF-8"))
      (save-window-excursion
	(run-python skk-annotation-python-program t t))
      (when (eval-when-compile (skkannot-emacs-24_3-or-later))
	;; Emacs 24.3 or later
	(setq python-buffer (get-buffer (format "*%s*" python-shell-buffer-name))
	      orig-py-buffer (default-value 'python-buffer)))
      (setenv "LANG" env)
      (with-current-buffer python-buffer
	(rename-buffer "  *skk python")
	(setq-default python-buffer orig-py-buffer)
	(setq python-buffer orig-py-buffer)
	(setq skkannot-py-buffer (current-buffer))
	;;
	(font-lock-mode 0)
	(set-buffer-multibyte t)
	(skk-process-kill-without-query (get-buffer-process (current-buffer)))
	(set-buffer-file-coding-system
	 (if (eval-when-compile (<= emacs-major-version 22))
	     'emacs-mule
	   'utf-8-emacs))
	(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
	;;
	(skkannot-py-send-command "import DictionaryServices")
	(cond ((and wait (skkannot-sit-for 1.0))
	       (setq skkannot-remaining-delay
		     (- skkannot-remaining-delay 1.0)))
	      (wait
	       (throw '辞書 nil))
	      (t
	       nil))
	;;
	skkannot-py-buffer)))))

(defun skkannot-DictServ-cache (word truncate)
  "OS X の「辞書」からこれまでに取得済のアノテーションを探す。"
  (let (success pt)
    (skk-save-point
     (goto-char (point-max))
     (cond
      ((re-search-backward (regexp-quote
			    (format " %s in DictionaryServices" word))
			   nil t)
       (forward-line 1)
       (beginning-of-line)
       (setq pt (point))
       (setq success (re-search-forward "^>>> " nil t))
       (unless success
	 (unless (skkannot-sit-for skk-annotation-loop-interval truncate)
	   (unless truncate
	     (throw '辞書 nil)))
	 (setq success (re-search-forward "^>>> " nil t)))
       (cond
	(success
	 (forward-line  -1)
	 (end-of-line)
	 (when (>= pt (point))
	   (forward-line 1)
	   (end-of-line))
	 (when (eq (point-at-bol) (point-at-eol))
	   (forward-line -1)
	   (end-of-line))
	 (buffer-substring-no-properties pt (point)))
	(t
	 nil)))
      (t
       nil)))))

;;;###autoload
(defun skk-annotation-lookup-DictionaryServices (word &optional truncate force)
  "python を介して DictionaryServices を利用しアノテーションを取得する。
オプション引数 TRUNCATE が non-nil の場合は候補一覧用に短いアノテーション
に絞りこむ。"
  (when (or skk-annotation-lookup-DictionaryServices force)
    (skk-annotation-start-python (not truncate))
    (let ((command (skkannot-DictServ-command word))
	  (process (get-buffer-process skkannot-py-buffer))
	  (loopint skk-annotation-loop-interval)
	  output)
      (with-current-buffer skkannot-py-buffer
	(unless (setq output (skkannot-DictServ-cache word truncate))
	  (goto-char (point-max))
	  (skkannot-py-send-command command)
	  (cond ((and (skkannot-sit-for loopint truncate)
		      (not truncate))
		 (setq skkannot-remaining-delay
		       (- skkannot-remaining-delay loopint)))
		(t
		 (unless truncate
		   (throw '辞書 nil))))
	  (accept-process-output process loopint)
	  (goto-char (point-max))
	  (setq output (or (skkannot-DictServ-cache word truncate)
			   "None")))
	(unless (string-match skkannot-py-none-regexp output)
	  (cond
	   (truncate
	    (with-temp-buffer
	      (set-buffer-multibyte t)
	      (insert output)
	      (goto-char (point-min))
	      (when (re-search-forward "[;,.、。；｜]" nil t)
		(beginning-of-line)
		(buffer-substring (point) (match-beginning 0)))))
	   (t
	    output)))))))

;;; 外部プログラムからのアノテーション取得機能
(defun skkannot-start-dict-process (buffer word)
  "dict のプロセスを起動する。"
  (let ((process-connection-type nil)
	(word (encode-coding-string word skk-annotation-dict-coding-system))
	process)
    (make-local-variable 'coding-system-for-read)
    (setq coding-system-for-read 'undecided)
    (prog1
	(setq process (apply #'start-process (buffer-name buffer) buffer
			     skk-annotation-dict-program
			     (append skk-annotation-dict-program-arguments
				     (list word))))
      (skk-process-kill-without-query process))))

;;;###autoload
(defun skk-annotation-preread-dict (word &optional nowait)
  "dict のプロセスを起動する。先読みのために用いる。"
  (let ((buffer (get-buffer-create (skkannot-dict-buffer-format word)))
	(text "")
	(loopint 0.01))
    (setq skkannot-buffer-origin (current-buffer))
    (with-current-buffer buffer
      (setq text (buffer-string))
      (when (string= text "")
	(unless (get-process (buffer-name buffer))
	  (skkannot-start-dict-process buffer word)
	  (unless nowait
	    (cond ((skkannot-sit-for loopint)
		   (setq skkannot-remaining-delay
			 (- skkannot-remaining-delay loopint)))
		  (t
		   (throw 'dict nil)))))))))

;;;###autoload
(defun skk-annotation-lookup-dict (word &optional truncate)
  "dict のプロセスを必要なら起動し、結果を調べる。
意味が取得できた場合には結果を文字列として返す。"
  (let ((buffer (get-buffer-create (skkannot-dict-buffer-format word)))
	(text "")
	(no-user-input t)
	(loopint skk-annotation-loop-interval)
	process)
    (setq skkannot-buffer-origin (current-buffer))
    (with-current-buffer buffer
      (setq text (buffer-string))
      (when (string= text "")
	(unless (setq process (get-process (buffer-name buffer)))
	  (setq process (skkannot-start-dict-process buffer word)))
	(cond
	 (truncate
	  (while (eq (process-status process) 'run)
	    (sleep-for loopint))
	  (sleep-for loopint))
	 (t
	  (while (and no-user-input
		      (eq (process-status process) 'run))
	    (when (setq no-user-input (skkannot-sit-for loopint truncate))
	      (setq skkannot-remaining-delay
		    (- skkannot-remaining-delay loopint))))
	  (if no-user-input
	      (sleep-for 0.01)
	    (throw 'dict nil)))))
      (goto-char (point-max))
      (forward-line -1)
      (cond
       ((looking-at "^Process .+ finished$")
	(cond
	 (truncate
	  (goto-char (point-min))
	  (cond
	   ((re-search-forward "[、。]" nil t)
	    (beginning-of-line)
	    (setq text (buffer-substring (point) (match-beginning 0))))
	   (t
	    (erase-buffer)
	    (insert " ")
	    (setq text ""))))
	 (t
	  (forward-line -1)
	  (setq text (buffer-substring (point-min) (point))))))
       (t
	(erase-buffer)
	(insert " ")
	(setq text ""))))
    (unless (string= text "")
      text)))

;;; Wiktionary/Wikipedia アノテーション
;;;###autoload
(defun skk-annotation-wikipedia (word &optional sources)
  "Wiktionary/Wikipedia の WORD に相当する記事からアノテーションを取得する。"
  (let ((sources (or sources skk-annotation-other-sources))
	source
	words
	(string "")
	(note nil))
    ;; sources に指定された順番に参照する
    (if (catch 'skkannot-wikipedia-suspended
	  (save-match-data
	    (while (and (or (not note) (equal note ""))
			sources)
	      (setq source (car sources))
	      (cond
	       ((memq source '(dict DictionaryServices
				    en.wiktionary ja.wiktionary))
		(setq words (list word))
		(when (skk-ascii-char-p (aref word 0))
		  (cond
		   ((skk-lower-case-p (aref word 0))
		    ;; foo -> Foo, FOO
		    (setq words (append words (list (upcase-initials word))))
		    (if (>= (length word) 2)
			(setq words (append words (list (upcase word))))))
		   ((and (>= (length word) 2)
			 (skk-lower-case-p (aref word 1)))
		    ;; Foo -> foo, FOO
		    (setq words (append words (list (downcase word) (upcase word)))))
		   (t
		    ;; FOO -> foo, Foo
		    (setq words (append words (list (downcase word))))
		    (if (>= (length word) 2)
			(setq words (append words
					    (list (upcase-initials word)))))))))
	       (t
		(setq words (list (upcase-initials word)))
		(if (>= (length word) 2)
		    (setq words (append words (list (upcase word)))))))
	      (while (and (not note) words)
		(setq note (skk-annotation-wikipedia-1 (car words) source t))
		(sleep-for 0.01) ; これがないと止まることあり
		(setq words (cdr words))
		(when (equal note "")
		  (setq note nil)))
	      ;;
	      (setq string (format (if (string= "" string)
				       "%s%s"
				     "%s/%s")
				   string source))
	      (setq sources (cdr sources)))
	    (unless note
	      (message "%s に項目がありません" string)))
	  nil)
	;; ダウンロードが中断されたとき
	(progn
	  (message "%s の転送が中断されました" source)
	  nil)
      ;;
      note)))

(defun skkannot-wikipedia-clean-sup (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'underline)
  (save-excursion
    (goto-char p2)
    (insert "^"))
  (html2text-delete-tags p1 p2 (1+ p3) (1+ p4)))

(defun skkannot-wikipedia-clean-sub (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'underline)
  (save-excursion
    (goto-char p2)
    (insert "_"))
  (html2text-delete-tags p1 p2 (1+ p3) (1+ p4)))

(defun skk-annotation-wikipedia-1 (word source &optional preserve-case)
  "Wiktionary/Wikipedia の WORD に相当する記事を実際にダウンロードして調べる。
該当ページ (html) をダウンロードする機能は Emacs に付属の URL パッケージに依
る。"
  (cond
   ((eq source 'lookup.el)
    (skk-lookup-get-content word nil))
   ((eq source '辞書)
    (catch '辞書
      (skk-annotation-lookup-DictionaryServices word nil t)))
   ((eq source 'dict)
    (catch 'dict (skk-annotation-lookup-dict word)))
   (t
    (require 'html2text)
    (require 'url)
    ;;
    (setq word (skkannot-wikipedia-normalize-word word source preserve-case))
    ;;
    (let ((cache-buffer (format "  *skk %s %s" source word))
	  ;; html2text が正しく扱えない tag は以下のリストに指定する
	  (html2text-remove-tag-list
	   (append '("a" "span" "table" "tr" "td" "h2" "h3" "h4" "h5" "small"
		     "code")
		   html2text-remove-tag-list))
	  (html2text-format-tag-list
	   (append '(("sup" . skkannot-wikipedia-clean-sup)
		     ("sub" . skkannot-wikipedia-clean-sub))
		   html2text-format-tag-list))
	  (url-retrieve-func #'url-retrieve)
	  buf buffer)
      (if (get-buffer cache-buffer)
	  (with-current-buffer cache-buffer
	    (buffer-string))
	;; キャッシュがない場合
	(setq buffer (funcall url-retrieve-func
			      (skkannot-generate-url
			       "http://%s.org/wiki/%s"
			       source word)
			      #'skkannot-wikipedia-retrieved
			      (list (list source))))
	(while (not buf)
	  (setq buf (catch 'skkannot-wikipedia-retrieved
		      (condition-case nil
			  (sleep-for 0.01)
			((error quit)
			 (kill-buffer buffer)
			 (throw 'skkannot-wikipedia-suspended
				source))))))
	(when (and (setq buffer buf)
		   (buffer-live-p buffer))
	  (skkannot-wikipedia-format-buffer source buffer
						  cache-buffer)))))))

(defun skkannot-wikt-preferred-lang-regexp (lang)
  (let ((head "<h2>.*<span class=\"mw-headline\".+>\\(<a href=.+>\\)?\\(")
	(tail "\\)\\(</a>\\)?</span></h2>"))
    (concat head lang tail)))

(defun skkannot-wikt-find-preferred-langs (wiktlang)
  (let ((langs (cdr (assoc wiktlang
			   skk-annotation-wiktionary-preferred-lang-alist)))
	(pt (point))
	regexp)
    (catch 'found
      (dolist (lang langs)
	(setq regexp (skkannot-wikt-preferred-lang-regexp lang))
	(if (re-search-forward regexp nil t)
	    (throw 'found t)
	  (goto-char pt))))))

(defun skkannot-wikipedia-format-buffer (source buffer cache-buffer)
  "html の余計な要素を除去し、html2text の機能を用いて整形する。"
  (let ((html2text-remove-tag-list
	 (append '("a" "span" "table" "tr" "td" "h2" "h3" "h4" "h5" "small"
		   "code")
		 html2text-remove-tag-list))
	(html2text-format-tag-list
	 (append '(("sup" . skkannot-wikipedia-clean-sup)
		   ("sub" . skkannot-wikipedia-clean-sub))
		 html2text-format-tag-list))
	note aimai continue nop point top pt1 pt2 btag etag end)
    (with-current-buffer buffer
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (when (> (buffer-size) 0)
	(when (get-buffer cache-buffer)
	  (kill-buffer cache-buffer))
	(rename-buffer cache-buffer)
	;; 要らない部分を消す
	(cond
	 ;; ja.wiktionary
	 ((eq source 'ja.wiktionary)
	  (goto-char (point-min))
	  (if (save-excursion
		(re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
				   nil t))
	      ;; 項目がない場合
	      (erase-buffer)
	    (search-forward "<!-- start content -->" nil t)
	    (delete-region (point-min) (point))
	    ;;
	    (goto-char (point-min))
	    (when (or (skkannot-wikt-find-preferred-langs "ja")
		      (re-search-forward
		       skkannot-ja-wiktionary-lang-regexp
		       nil t))
	      (save-excursion
		(goto-char (match-end 2))
		(insert ", "))
	      (delete-region (point-min) (match-beginning 0))
	      (setq top (point))
	      (when (re-search-forward skkannot-ja-wiktionary-lang-regexp
				       nil t)
		(delete-region (setq pt1 (match-beginning 0)) (point-max))))
	    ;;
	    (setq point top)
	    (goto-char (point-min))
	    ;; ja.wiktionary の書式が en.wiktionary ほど整っていないので
	    ;; workaround
	    (unless
		(save-excursion
		  (re-search-forward
		   skkannot-ja-wiktionary-part-of-speech-regexp
		   nil t))
	      (setq point pt1))
	    ;;
	    (while (re-search-forward
		    skkannot-ja-wiktionary-part-of-speech-regexp
		    nil t)
	      (setq nop t)
	      (save-match-data
		(when (looking-at "</h3>")
		  (delete-region (match-beginning 0) (match-end 0))))
	      (goto-char (match-beginning 0))
	      (delete-region (or point (point-min)) (point))
	      (cond
	       ((re-search-forward "<\\(ol\\|dl\\)>" nil t)
		(setq btag (match-string 0)
		      etag (if (string= btag "<ol>")
			       "</ol>"
			     "</dl>")
		      point nil
		      pt1 (point)
		      pt2 nil)
		(while (and (not point)
			    (search-forward etag nil t))
		  (setq pt2 (point))
		  (goto-char pt1)
		  (if (and (search-forward btag nil t)
			   (< (point) pt2))
		      (progn
			(goto-char pt2)
			(setq pt1 (point)))
		    (setq point pt2)
		    (goto-char point))))
	       (t
		(goto-char (match-end 0))
		(when (search-forward "</p>" nil t)
		  (setq point (point))))))
	    ;;
	    (when point
	      (delete-region point (point-max)))
	    ;; (用例などを除く -- 除かないほうがいい？)
	    ;; ja.wiktionary は en.wiktionary と全く統一された書き方には
	    ;; なっていないので、ul を除くと情報がほとんど残らない場合が
	    ;; ある
	    (skkannot-wikipedia-remove-nested "<ul>" "</ul>")
	    (skkannot-wikipedia-remove-nested "<dl>" "</dl>")
	    (skkannot-wikipedia-remove-nested "<table[^<]*>"
						    "</table>")
	    (skkannot-wikipedia-remove-nested "\
<div class=\"\\(infl-table\\|thumb.+\\)\"[^<]*>" "</div>" "<div[^<]*>")
	    ;;
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<span.*>\\[<a.+>編集</a>\\]</span>"
		    nil t)
	      (replace-match ""))))
	 ;; en.wiktionary
	 ((eq source 'en.wiktionary)
	  (goto-char (point-min))
	  (if (save-excursion
		(re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
				   nil t))
	      ;; 項目がない場合
	      (erase-buffer)
	    (search-forward "<!-- start content -->" nil t)
	    (delete-region (point-min) (point))
	    ;;
	    (goto-char (point-min))
	    (when (or (skkannot-wikt-find-preferred-langs "en")
		      (re-search-forward
		       skkannot-en-wiktionary-lang-regexp
		       nil t))
	      (save-excursion
		(goto-char (match-end 2))
		(insert ", "))
	      (delete-region (point-min) (match-beginning 0))
	      (setq top (point))
	      (when (re-search-forward
		     skkannot-en-wiktionary-lang-regexp
		     nil t)
		(delete-region (match-beginning 0) (point-max))))
	    ;;
	    (setq point top)
	    (goto-char (point-min))
	    (while (re-search-forward
		    skkannot-en-wiktionary-part-of-speech-regexp
		    nil t)
	      (setq nop t)
	      (save-match-data
		(when (looking-at "</h3>")
		  (delete-region (match-beginning 0) (match-end 0))))
	      (goto-char (match-beginning 0))
	      (delete-region (or point (point-min)) (point))
	      (cond
	       ((re-search-forward "<\\(ol\\|dl\\)>" nil t)
		(setq btag (match-string 0)
		      etag (if (string= btag "<ol>")
			       "</ol>"
			     "</dl>")
		      point nil
		      pt1 (point)
		      pt2 nil)
		(while (and (not point)
			    (search-forward etag nil t))
		  (setq pt2 (point))
		  (goto-char pt1)
		  (if (and (search-forward btag nil t)
			   (< (point) pt2))
		      (progn
			(goto-char pt2)
			(setq pt1 (point)))
		    (setq point pt2)
		    (goto-char point))))
	       (t
		(goto-char (match-end 0))
		(when (search-forward "</p>" nil t)
		  (setq point (point))))))
	    ;;
	    (when point
	      (delete-region point (point-max)))
	    ;; (用例などを除く -- 除かないほうがいい？)
	    (skkannot-wikipedia-remove-nested "<ul>" "</ul>")
	    (skkannot-wikipedia-remove-nested "<dl>" "</dl>")
	    (skkannot-wikipedia-remove-nested "<table[^<]*>"
						    "</table>")
	    (skkannot-wikipedia-remove-nested "\
<div class=\"\\(infl-table\\|thumb.+\\)\"[^<]*>" "</div>" "<div[^<]*>")
	    (skkannot-wikipedia-remove-nested "\
<span class=\"interProject\">" "</span>")
	    ;; Wikipedia への案内を除く
	    (goto-char (point-min))
	    (while (re-search-forward "\
\\(<a href=\"/wiki/Wikipedia\" title=\"Wikipedia\">\\)?\
Wikipedia\\(</a>\\)? has an article on:$" nil t)
	      (save-excursion
		(goto-char (match-beginning 0))
		(beginning-of-line)
		(setq point (point)))
	      (forward-line 2)
	      (end-of-line)
	      (delete-region point (point)))
	    ;;
	    (goto-char (point-min))
	    (while (re-search-forward "\
<span.*>\\(\\[<a.+>edit</a>\\]\\|Inflection\\)</span>"
				      nil t)
	      (replace-match ""))))
	 ;; wikipedia
	 ((memq source '(ja.wikipedia simple.wikipedia en.wikipedia))
	  (goto-char (point-min))
	  (if (save-excursion
		(re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
				   nil t))
	      ;; 項目がない場合
	      (erase-buffer)
	    (setq aimai
		  (save-excursion
		    (re-search-forward "\
wgCategories.+\\(曖昧さ回避\\|[Dd]isambiguation\\).+$" nil t)))
	    ;; <span> を除去する
	    (setq point nil)
	    (goto-char (point-min))
	    (while (re-search-forward "\
<span class=\"\\(.+audiolink.+\\|editsection\\)\".*>" nil t)
	      (setq point (match-beginning 0))
	      (goto-char point)
	      (search-forward "</span>" nil t)
	      (delete-region point (point))
	      (goto-char point))
	    ;; <big> を除去する
	    (goto-char (point-min))
	    (while (re-search-forward "<p><big>.+</big></p>" nil t)
	      (replace-match ""))
	    ;; &#160; を処理
	    (goto-char (point-min))
	    (while (re-search-forward "&#160;" nil t)
	      (replace-match " "))
	    ;; <br /> を除去する
	    (goto-char (point-min))
	    (while (re-search-forward "<p>.+\\(<br />\\)$" nil t)
	      (replace-match "" nil nil nil 1))
	    ;; xxx > xxx > xxx ... を除去する
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<p>.+</a> &gt; \\(<a.+>\\|<b>\\).+</p>" nil t)
	      (replace-match ""))
	    ;; <script> を除去
	    (skkannot-wikipedia-remove-nested "<script.*>" "</script>")
	    ;; <table> を除去
	    (skkannot-wikipedia-remove-nested "<table.*>" "</table>")
	    ;;
	    (goto-char (point-min))
	    (when (or (when (re-search-forward
			     "<p>\\(<br />\n\\|[^\n]*\\)?<b>[^\n]+</b>[^\n]+"
			     nil t)
			(goto-char (match-beginning 0))
			(if (and (save-excursion
				   (re-search-forward "</p>" nil t))
				 (string-match
				  (cond
				   ((eq source 'ja.wikipedia)
				    "。\\|．")
				   (t
				    "\\."))
				  (buffer-substring (point)
						    (match-beginning 0))))
			    t
			  (setq point (point)
				continue t)
			  nil))
		      (when (progn
			      (goto-char (point-min))
			      (re-search-forward "<\\(u\\|o\\)l>" nil t))
			(goto-char (if continue
				       point
				     (match-beginning 0)))
			(setq nop t)))
	      (delete-region (point-min) (point))
	      (goto-char (point-min))
	      ;;
	      (cond
	       ((or aimai nop)
		(setq pt1 (if (re-search-forward "<\\(u\\|o\\)l>" nil t)
			      (match-end 0)
			    nil)
		      pt2 nil)
		(while (and (not end)
			    (re-search-forward "</\\(u\\|o\\)l>"
					       nil t))
		  (setq pt2 (match-end 0))
		  (save-excursion
		    (goto-char (or pt1 (1+ (point-min))))
		    (when (re-search-forward "<\\(u\\|o\\)l>"
					     nil t)
		      (setq pt1 (match-end 0))))
		  (when (or (null pt1)
			    (> pt1 pt2))
		    (setq end t))))
	       (t
		(re-search-forward "</p>" nil t)))
	      (delete-region (point) (point-max))))))
	;;
	(setq point nil)
	(when (> (buffer-size) 0)
	  (html2text)
	  (goto-char (point-min))
	  (cond
	   ((memq source '(ja.wiktionary en.wiktionary))
	    ;; wiktionary の整形結果は空行だらけになる...
	    (goto-char (point-min))
	    (while (re-search-forward "\n[\n]+" nil t)
	      (replace-match "\n"))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (beginning-of-line)
	      (setq point (point))
	      (forward-line 1)
	      (fill-region point (point))))
	   (t
	    (while (looking-at "^[ \t]*$")
	      (kill-line 1))
	    (cond ((or aimai nop)
		   (while (not (eobp))
		     (beginning-of-line)
		     (setq point (point))
		     (forward-line 1)
		     (fill-region point (point))))
		  (t
		   (fill-paragraph nil)))))
	  ;;
	  (when aimai
	    (insert (if (eq source 'ja.wikipedia)
			"\n(曖昧さ回避のページ)"
		      "\n(Disambiguation page)")))
	  ;;
	  (goto-char (point-max))
	  (while (and (looking-at "^$")
		      (not (string= "" (buffer-string))))
	    (delete-char -1))
	  ;;
	  (when (and (not (equal (buffer-string) ""))
		     (not (get-text-property 1 'face)))
	    (put-text-property 1 2 'face 'default))
	  (setq note (buffer-string)))))
    ;;
    (cond ((stringp note)
	   (if (equal note "")
	       nil
	     note))
	  (t
	   nil))))

(defun skkannot-wikipedia-remove-nested (btag etag &optional ibtag)
  "<dl> <ul> <table> などの入れ子構造を除去する。"
  (unless ibtag
    (setq ibtag btag))
  (let (point pt1 pt2 orig-btag)
    (setq point nil)
    (goto-char (point-min))
    (while (re-search-forward btag nil t)
      (setq point (match-beginning 0))
      (goto-char point)
      (cond
       ((not (search-forward etag nil t))
	(delete-region point (match-end 0))
	(goto-char (point-min)))
       (t
	(setq pt2 (match-end 0))
	(goto-char (1+ point))
	(cond
	 ((not (re-search-forward ibtag nil t))
	  (delete-region point pt2)
	  (when orig-btag
	    (setq btag      orig-btag
		  orig-btag nil))
	  (goto-char (point-min)))
	 (t
	  (setq pt1 (match-beginning 0))
	  (cond
	   ((< pt2 pt1)
	    (delete-region point pt2)
	    (setq point nil)
	    (when orig-btag
	      (setq btag      orig-btag
		    orig-btag nil))
	    (goto-char (point-min)))
	   (t
	    (unless orig-btag
	      (setq orig-btag btag
		    btag      ibtag))
	    (goto-char pt1))))))))))

(defun skkannot-wikipedia-retrieved (&rest args)
  (cond ((or (member "deleted\n" (assq 'error (memq :error (car args))))
	     (< (buffer-size) 7)
	     (not (skkannot-wikipedia-test-html-tag)))
	 ;; 不完全な retrieval においても STATUS が nil となることがあるので
	 ;; ここで調整する。
	 (kill-buffer (current-buffer))
	 (ignore-errors
	   (throw 'skkannot-wikipedia-suspended (cadr args))))
	(t
	 (throw 'skkannot-wikipedia-retrieved (current-buffer)))))

(defun skkannot-wikipedia-test-html-tag ()
  ;; html データが最後の </html> タグを持つことを確認する
  (goto-char (point-min))
  (when (re-search-forward "^Content-Encoding: gzip$" nil t)
    ;; html が gzip 圧縮で送られて来た場合
    (unless (fboundp 'url-handle-content-transfer-encoding)
      ;; Emacs 24.3 or earlier
      (let ((gzip (executable-find "gzip")))
	(unless gzip
	  (error "この内容を表示するには %s が必要です" "gzip"))
	(while (and (not (looking-at "^\n"))
		    (not (eobp)))
	  (forward-line 1))
	(forward-line 1)
	(when (< (point) (point-max))
	  (let ((coding-system-for-write 'binary))
	    (call-process-region (point) (point-max) gzip t t t "-cd"))))))
  (goto-char (point-max))
  (search-backward "</html>" nil t))

;;;###autoload
(defalias 'skk-annotation-wikipedia-region-or-at-point
  'skk-annotation-lookup-region-or-at-point)

(defun skkannot-generate-url (format-string &rest args)
  (condition-case nil
      (require 'url-util)
    (error
     (error "%s" "新しい URL パッケージが必要です")))
  (if (skkannot-url-installed-p)
      (apply #'format format-string
	     (mapcar (lambda (element)
		       (if (stringp element)
			   (url-hexify-string element)
			 element))
		     args))
    (error "%s" "URL パッケージまたは Mule-UCS が利用できません")))

(defun skkannot-wikipedia-normalize-word (word &optional method preserve-case)
  ;; スペースは %20 ではなく、アンダースコアに変換する
  (replace-regexp-in-string
   " " "_"
   (cond
    ((memq method '(ja.wiktionary en.wiktionary))
     (if (and (not preserve-case)
	      (> (length word) 1)
	      (skk-ascii-char-p (aref word 0))
	      (skk-lower-case-p (aref word 1)))
	 ;; 二文字めが lower case なら downcase
	 (downcase word)
       ;; 一文字だったら元の case
       ;; 二文字めが upper case なら元の case
       ;; 英語以外は未対応
       word))
    ((eq method 'upcase-initials)
     (upcase-initials word))
    (t
     (if (> (length word) 1)
	 (concat (vector (upcase (aref word 0)))
		 (substring word 1))
       word)))))

(defun skkannot-url-installed-p ()
  (when (eq skkannot-url-installed-p 'untested)
    ;; GNU Emacs 22 以降以外で URL パッケージをテストする
    (cond
     ((and (featurep 'xemacs)
	   (= emacs-major-version 21)
	   (= emacs-minor-version 4)
	   (not (featurep 'un-define)))
      ;; XEmacs 21.4 で Mule-UCS もない場合
      (setq skkannot-url-installed-p nil))
     (t
      ;; Emacs 21 と XEmacs
      (defadvice url-hexify-string (around multibyte-char activate)
	(setq ad-return-value
	      (mapconcat (lambda (byte)
			   (if (memq byte url-unreserved-chars)
			       (char-to-string byte)
			     (format "%%%02x" byte)))
			 (if (multibyte-string-p (ad-get-arg 0))
			     (encode-coding-string (ad-get-arg 0) 'utf-8)
			   (ad-get-arg 0))
			 "")))
      ;;
      (setq skkannot-url-installed-p t))))
  ;;
  skkannot-url-installed-p)

;;; 各種アノテーション・ソースのキャッシュ管理
;;;###autoload
(defun skkannot-cache (word &optional sources)
  (let ((sources (or sources skk-annotation-other-sources))
	(word (skkannot-wikipedia-normalize-word word 'en.wiktionary))
	(cword (skkannot-wikipedia-normalize-word word))
	(ccword (skkannot-wikipedia-normalize-word word 'upcase-initials)))
    (catch 'found
      (while sources
	(let* ((source (pop sources))
	       (ccache-buffer (if (equal word cword)
				  nil
				(format "  *skk %s %s" source cword)))
	       (cccache-buffer (if (or (equal word ccword)
				       (equal cword ccword))
				   nil
				 (format "  *skk %s %s" source ccword)))
	       (cache-buffer (format "  *skk %s %s" source word))
	       string)
	  (cond
	   ((eq source 'lookup.el)
	    (setq string (skk-lookup-get-content word))
	    (if (or (null string)
		    (string= string ""))
		nil
	      (throw 'found
		     (cons string "lookup.el"))))
	   ((eq source '辞書)
	    (setq string
		  (catch '辞書
		    (skk-annotation-lookup-DictionaryServices word)))
	    (if (or (null string)
		    (string= string ""))
		nil
	      (throw 'found
		     (cons string "dict"))))
	   ((eq source 'dict)
	    (setq string (catch 'dict (skk-annotation-lookup-dict word)))
	    (if (or (null string)
		    (string= string ""))
		nil
	      (throw 'found
		     (cons string "dict"))))
	   (t
	    (setq string
		  (if (and ccache-buffer
			   (get-buffer ccache-buffer))
		      ;; Word word
		      (with-current-buffer (get-buffer ccache-buffer)
			(buffer-string))
		    ""))
	    (if (> (length string) 0)
		(throw 'found (cons string ccache-buffer))
	      (setq string
		    (if (and cccache-buffer
			     (get-buffer cccache-buffer))
			;; Word Word
			(with-current-buffer (get-buffer cccache-buffer)
			  (buffer-string))
		      ""))
	      (if (> (length string) 0)
		  (throw 'found (cons string cccache-buffer))
		(setq string
		      (if (get-buffer cache-buffer)
			  ;; word word
			  (with-current-buffer (get-buffer cache-buffer)
			    (buffer-string))
			""))
		(if (string= string "")
		    nil
		  (throw 'found (cons string cache-buffer))))))))))))

(provide 'skk-annotation)

;;; skk-annotation.el ends here
