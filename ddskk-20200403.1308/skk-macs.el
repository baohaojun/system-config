;;; skk-macs.el --- Macros and inline functions commonly used in SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999-2010 SKK Development Team <skk@ring.gr.jp>
;; Copyright (C) 1993-2000 Free Software Foundation, Inc.

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Macros and inline functions commonly used in SKK

;;; Code:

(require 'advice)
(require 'skk)

;;;; macros

(defmacro skk-defadvice (function &rest everything-else)
  "Defines a piece of advice for FUNCTION (a symbol).
This is like `defadvice', but warns if FUNCTION is a subr command and advice
doesn't give arguments of `interactive'. See `interactive' for details."
  (let ((origfunc (and (fboundp function)
                       (if (ad-is-advised function)
                           (ad-get-orig-definition function)
                         (symbol-function function))))
        interactive)
    (unless
        (or (not origfunc)
            (not (subrp origfunc))
            (memq function ; XXX possibilly Emacs version dependent
                  ;; built-in commands which do not have interactive specs.
                  '(abort-recursive-edit
                    bury-buffer
                    delete-frame
                    delete-window
                    exit-minibuffer)))
      ;; check if advice definition has a interactive call or not.
      (setq interactive
            (cond
             ((and (stringp (nth 1 everything-else)) ; have document
                   (eq 'interactive (car-safe (nth 2 everything-else))))
              (nth 2 everything-else))
             ((eq 'interactive (car-safe (nth 1 everything-else)))
              (nth 1 everything-else))))
      (cond
       ((and (commandp origfunc)
             (not interactive))
        (message
         "\
*** WARNING: Adding advice to subr %s\
 without mirroring its interactive spec ***"
         function))
       ((and (not (commandp origfunc))
             interactive)
        (setq everything-else (delq interactive everything-else))
        (message
         "\
*** WARNING: Deleted interactive call from %s advice\
 as %s is not a subr command ***"
         function function))))
    `(defadvice ,function ,@everything-else)))

;;;###autoload
(put 'skk-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec skk-defadvice defadvice)

(defmacro skk-save-point (&rest body)
  `(let ((skk-save-point (point-marker)))
     (unwind-protect
         (progn
           ,@body)
       (goto-char skk-save-point)
       (skk-set-marker skk-save-point nil))))

(def-edebug-spec skk-save-point t)

(defmacro skk-message (japanese english &rest arg)
  "メッセージを表示する。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil で
あれば ENGLISH をエコーエリアに表示する。
 ARG は `message' 関数の第２引数以降の引数として渡される。"
  (append
   (if arg
       `(message (if skk-japanese-message-and-error
                     ,japanese
                   ,english))
     `(message "%s" (if skk-japanese-message-and-error
                        ,japanese
                      ,english)))
   arg))

(defmacro skk-error (japanese english &rest arg)
  "メッセージを表示して、エラーを発生させる。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil で
あれば ENGLISH をエコーエリアに表示し、エラーを発生させる。
 ARG は `error' 関数の第２引数以降の引数として渡される。"
  (append
   (if arg
       `(error (if skk-japanese-message-and-error
                   ,japanese
                 ,english))
     `(error "%s" (if skk-japanese-message-and-error
                      ,japanese
                    ,english)))
   arg))

(defmacro skk-yes-or-no-p (japanese english)
  "ユーザに yes-or-no を質問し、答えが yes だったら t を返す。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil であれ
ば ENGLISH を PROMPT として `yes-or-no-p' を実行する。
`yes-or-no-p' の引数 PROMPT が複雑に入れ込んでいる場合は `skk-yes-or-no-p' を
使うよりもオリジナルの `yes-or-no-p' を使用した方がコードが複雑にならない場合
がある。"
  `(yes-or-no-p (if skk-japanese-message-and-error
                    ,japanese ,english)))

(defmacro skk-y-or-n-p (japanese english)
  "ユーザに \"y or n\" を質問し、答えが \"y\" だったら t を返す。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil であれ
ば ENGLISH を PROMPT として `y-or-n-p' を実行する。"
  `(y-or-n-p (if skk-japanese-message-and-error
                 ,japanese ,english)))

(defmacro skk-set-marker (marker position &optional buffer)
  "マーカ MARKER を BUFFER の POSITION に移動する。
BUFFER のディフォルト値はカレントバッファである。
MARKER が nil だったら、新規マーカーを作って代入する。"
  `(progn
     (if (not ,marker)
         (setq ,marker (make-marker)))
     (set-marker ,marker ,position ,buffer)))

(defmacro skk-with-point-move (&rest form)
  "ポイントを移動するがフックを実行してほしくない場合に使う。"
  `(unwind-protect
       (progn
         ,@form)
     (setq skk-previous-point (point))))

(def-edebug-spec skk-with-point-move t)

(defmacro skk-face-on (object start end face &optional priority)
  `(let ((inhibit-quit t))
     (if (not (overlayp ,object))
         (progn
           (setq ,object (make-overlay ,start ,end))
           (when ,priority
             (overlay-put ,object 'priority ,priority))
           (overlay-put ,object 'face ,face)
           ;;(overlay-put (, object) 'evaporate t)
           )
       (move-overlay ,object ,start ,end))))

;;;###autoload
(put 'skk-loop-for-buffers 'lisp-indent-function 1)
(defmacro skk-loop-for-buffers (buffers &rest body)
  "BUFFERS が指定する各バッファに移動して BODY を実行する。"
  `(save-current-buffer
     (dolist (buf ,buffers)
       (when (buffer-live-p buf)
         (set-buffer buf)
         ,@body))))

(defmacro skk-delete-overlay (list)
  ;; skk-dcomp-multiple-hide と skk-inline-hide を統合した。
  `(when ,list
     (dolist (o ,list)
       (delete-overlay o))
     (setq ,list nil)))

(defmacro skk-help-make-usage (symbol arglist)
  (cond ((fboundp 'help--make-usage)
         ;; GNU Emacs 25.1 から
         `(help--make-usage ,symbol ,arglist))

        (t
         ;; GNU Emacs 24.1 まで
         `(help-make-usage ,symbol ,arglist))))

;;; functions.

(defmacro string-to-int-list (string)
  `(mapcar #'identity ,string))

(defun event-to-character (event)
  "Return the character approximation to the given event object.
If the event isn't a keypress, this returns nil."
  (cond
   ((symbolp event)
    ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
    (let ((mask (get event 'event-symbol-element-mask)))
      (if mask
          (let ((base (get (car mask) 'ascii-character)))
            (if base
                (logior base (cadr mask)))))))
   ((integerp event)
    event)))

(defun cancel-undo-boundary ()
  "Cancel undo boundary."
  (if (and (consp buffer-undo-list)
           (null (car buffer-undo-list)))
      (setq buffer-undo-list (cdr buffer-undo-list))))

;; For GNU Emacs.
(defun skk-color-cursor-display-p ()
  (and (skk-find-window-system)
       (fboundp 'x-display-color-p)
       (x-display-color-p)))

(defun skk-char-to-unibyte-string (char)
  (ignore-errors
    (cond
     ;; Warning: `string-make-unibyte' is an obsolete function (as of 26.1).
     ;;          use `encode-coding-string'.
     ((eval-when-compile (>= emacs-major-version 26))
      (encode-coding-string (char-to-string char) 'iso-8859-1))

     ;;  GNU Emacs 25 まで
     (t
      (string-make-unibyte (char-to-string char))))))

(defun skk-ascii-char-p (char)
  (eq (char-charset char skk-charset-list) 'ascii))

(defun skk-jisx0208-p (char)
  (eq (char-charset char skk-charset-list) 'japanese-jisx0208))

(defun skk-jisx0213-p (char)
  (memq (char-charset char skk-charset-list)
        '(japanese-jisx0213-1
          japanese-jisx0213.2004-1
          japanese-jisx0213-2)))

(defun skk-split-char (ch)
  ;; http://mail.ring.gr.jp/skk/200908/msg00006.html
  ;; C の split-char() と同様の機能だが、char-charset() の呼出しにおいて
  ;; 文字集合の選択肢を skk-charset-list に含まれるものに制限する。
  ;; これは例えば、japanese-jisx0208 の文字が unicode-bmp に属する、
  ;; と判定されるような状況を回避する。
  (let* ((charset (char-charset ch skk-charset-list))
         (code (encode-char ch charset))
         (dimension (charset-dimension charset))
         val)
    (while (> dimension 0)
      (setq val (cons (logand code 255) ;; 0xFF
                      val))
      (setq code (lsh code -8))
      (setq dimension (1- dimension)))
    (cons charset val)))

(defun skk-indicator-to-string (indicator &optional no-properties)
  "SKK インジケータ型オブジェクト INDICATOR を文字列に変換する。"
  (if no-properties
      (with-temp-buffer
        (insert indicator)
        (buffer-substring-no-properties (point-min) (point-max)))
    indicator))

(defun skk-mode-string-to-indicator (mode string)
  "文字列 STRING を SKK インジケータ型オブジェクトに変換する。"
  (if (and window-system
           (not (eq mode 'default)))
      (apply 'propertize string
             (cdr (assq mode skk-emacs-property-alist)))
    string))

(defun skk-face-proportional-p (face)
  (or (face-equal face 'variable-pitch)
      (eq (face-attribute face :inherit) 'variable-pitch)))

(defun skk-event-key (event)
  "イベント EVENT を発生した入力の情報を取得する。"
  (let ((char (event-to-character event))
        keys)
    (if char
        (vector char)
      (setq keys (recent-keys))
      (vector (aref keys (1- (length keys)))))))

(put 'skk-bind-last-command-char 'lisp-indent-function 1)
(defmacro skk-bind-last-command-char (char &rest body)
  (let ((variable 'last-command-event))
    `(let ((,variable ,char))
       (progn
         ,@body))))

;;; version independent

(defsubst skk-char-octet (ch &optional n)
  (or (nth (if n
               (1+ n)
             1)
           (skk-split-char ch))
      0))

(defun skk-cursor-set (&optional color force)
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  (when (or skk-use-color-cursor
            force)
    (skk-cursor-set-1 color)))

(defun skk-cursor-off ()
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  (when skk-use-color-cursor
    (skk-cursor-off-1)))

(defun skk-modify-indicator-alist (mode string)
  (setcdr (assq mode skk-indicator-alist)
          (cons string (skk-mode-string-to-indicator mode string))))

(defun skk-update-modeline (&optional mode string)
  (unless mode
    (setq mode 'default))
  ;;
  (when string
    (skk-modify-indicator-alist mode string))
  ;;
  (let ((indicator (cdr (assq mode skk-indicator-alist))))
    (setq skk-modeline-input-mode
          (if (eq skk-status-indicator 'left)
              (cdr indicator)
            (car indicator)))
    (force-mode-line-update)))

;; ツリーにアクセスするためのインターフェース
(defun skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
        prefix
        (if (string= nextstate "")
            nil
          nextstate)
        kana
        branch-list))

(defsubst skk-get-prefix (tree)
  (nth 1 tree))

(defsubst skk-get-nextstate (tree)
  (nth 2 tree))

(defsubst skk-set-nextstate (tree nextstate)
  (when (string= nextstate "")
    (setq nextstate nil))
  (setcar (nthcdr 2 tree) nextstate))

(defsubst skk-get-kana (tree)
  (nth 3 tree))

(defsubst skk-set-kana (tree kana)
  (setcar (nthcdr 3 tree) kana))

(defsubst skk-get-branch-list (tree)
  (nth 4 tree))

(defsubst skk-set-branch-list (tree branch-list)
  (setcar (nthcdr 4 tree) branch-list))

;; tree procedure for skk-kana-input.
(defsubst skk-add-branch (tree branch)
  (skk-set-branch-list tree (cons branch (skk-get-branch-list tree))))

(defsubst skk-select-branch (tree char)
  (assq char (skk-get-branch-list tree)))

(defun skk-erase-prefix (&optional clean)
  "`skk-echo' が non-nil であれば現在表示されている `skk-prefix' を消す。
オプショナル引数の CLEAN が指定されると、変数としての `skk-prefix' を空文字に、
`skk-current-rule-tree' を nil に初期化する。"
  ;; かな文字の入力がまだ完成していない場合にこの関数が呼ばれたときなどは
  ;; 表示されている skk-prefix は削除したいが、変数としての skk-prefix は
  ;; null 文字にしたくない。
  (when (overlayp skk-prefix-overlay)
    (condition-case nil
        (delete-overlay skk-prefix-overlay)
      (error
       (skk-set-marker skk-kana-start-point nil)
       (setq skk-prefix ""
             skk-current-rule-tree nil))))
  (when clean
    (setq skk-prefix ""
          skk-current-rule-tree nil))) ; fail safe

(defun skk-kana-cleanup (&optional force)
  (let ((data (cond
               ((and skk-current-rule-tree
                     (null (skk-get-nextstate skk-current-rule-tree)))
                (skk-get-kana skk-current-rule-tree))
               (skk-kana-input-search-function
                (car (funcall skk-kana-input-search-function)))))
        kana)
    (when (or force data)
      (skk-erase-prefix 'clean)
      (setq kana (if (functionp data)
                     (funcall data nil)
                   data))
      (when (consp kana)
        (setq kana (if skk-katakana
                       (car kana)
                     (cdr kana))))
      (when (stringp kana)
        (skk-insert-str kana))
      (skk-set-marker skk-kana-start-point nil)
      (or data t)))) ; skk-prefix に対応するデータがあったならそれを返す

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion
       (require 'skk-num)
       skk-num-list))

(defsubst skk-file-exists-and-writable-p (file)
  (and (setq file (expand-file-name file))
       (file-exists-p file)
       (file-writable-p file)))

(defsubst skk-share-private-jisyo-p ()
  (vectorp skk-jisyo-update-vector))

(defsubst skk-lower-case-p (char)
  "CHAR が小文字のアルファベットであれば、t を返す。"
  (and (<= ?a char)
       (>= ?z char)))

(defsubst skk-downcase (char)
  "連想リスト `skk-downcase-alist' に該当あれば (assq)、要素(子リスト)の cdr を返す。
該当なければ 関数 `downcase'の結果を返す。
関数 `skk-set-henkan-point' から呼ばれている。"
  (or (cdr (assq char skk-downcase-alist))
      (downcase char)))

(defun skk-mode-off ()
  (setq skk-mode nil
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  ;; initialize
  (skk-update-modeline)
  (skk-cursor-off)
  (skk-remove-skk-pre-command))

(defun skk-j-mode-on (&optional katakana)
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode t
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana katakana)
  (skk-setup-keymap)
  (skk-update-modeline (if skk-katakana
                           'katakana
                         'hiragana))
  (skk-cursor-set))

(defun skk-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode t
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'latin)
  (skk-cursor-set))

(defun skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode t
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'jisx0208-latin)
  (skk-cursor-set))

(defun skk-abbrev-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode t
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; skk-abbrev-mode は一時的な ascii 文字による変換なので、変換後は元の
        ;; 入力モード (かなモードかカナモード) に戻ることが期待される。
        ;; skk-katakana は minor-mode フラグではなく、skk-j-mode マイナーモード
        ;; の中でこのフラグにより入力文字を決定するポインタを変更するだけなので
        ;; skk-abbrev-mode マイナーモード化するのに skk-katakana フラグを初期化
        ;; しなければならない必然性はない。
        ;; sub mode of skk-j-mode.
        ;;skk-katakana nil
        )
  (skk-setup-keymap)
  (skk-update-modeline 'abbrev)
  (skk-cursor-set))

(defsubst skk-in-minibuffer-p ()
  "カレントバッファがミニバッファであれば t を返す。"
  (eq (current-buffer) (window-buffer (minibuffer-window))))

(defun skk-screen-column ()
  "スクリーン行から得たカーソル位置の桁数を返す。
テキスト行（改行文字で区切られたテキスト）がウィンドウ幅を越えて折り返して表示
されている場合にも対応する。"
  (- (current-column)
     (save-excursion
       (vertical-motion 0)      ;スクリーン行の行頭に移動する
       (current-column))))      ;↑この結果、スクリーン行の行頭なのか
                                        ;テキスト行の行頭なのか

(defun skk-move-to-screen-column (col)
  "スクリーン行から見た COL 桁位置にポイントを移動する。
テキスト行（改行文字で区切られたテキスト）がウィンドウ幅を越えて折り返して表示
されている場合にも対応するが、改行文字を越える移動は行わない。"
  (move-to-column (+ (current-column)   ;テキスト行から見た桁数
                     (- col (skk-screen-column))))
  (skk-screen-column))

(defun skk-max-string-width (list)
  "LIST の各要素の中から、最長のコラム幅 (string-width) を返す。"
  (apply 'max (mapcar 'string-width list)))

(defun skk-insert-prefix (&optional char)
  "`skk-echo' が non-nil であればカレントバッファに `skk-prefix' を挿入する。"
  (when skk-echo
    ;; skk-prefix はアンドゥの対象とならないので
    ;; バッファに挿入される必要がない。
    (setq skk-prefix-overlay (make-overlay (point) (point)))
    (let ((prefix (or char skk-prefix)))
      (when (and skk-use-face (not skk-henkan-mode))
        (setq prefix
              (propertize prefix 'face
                          (cond ((and skk-j-mode (not skk-katakana))
                                 'skk-prefix-hiragana-face)
                                (skk-katakana
                                 'skk-prefix-katakana-face)
                                (skk-jisx0201-mode
                                 'skk-prefix-jisx0201-face)))))
      (overlay-put skk-prefix-overlay 'after-string prefix))))

(defsubst skk-string-lessp-in-coding-system (str1 str2 coding-system)
  (string< (encode-coding-string str1 coding-system)
           (encode-coding-string str2 coding-system)))

(defun skk-string< (str1 str2)
  "STR1 と STR2 とを比較する。
内部コードが emacs-mule でないなど `stringp' の返り値が異なる Emacs に
対して emacs-mule の encoded string に変換して比較する。
比較の結果 str1 < str2 ならば t を返す。"
  ;; mule-version が 6.0 以上の GNU Emacs は Emacs with coding system utf-8-emacs
  ;;   (emacs-version) => "GNU Emacs 28.0.50"
  ;;   (stringp "漢") => t
  ;;   (encode-coding-string "漢" 'emacs-mule) => "\222\264\301"
  (skk-string-lessp-in-coding-system str1 str2 'emacs-mule))

(defsubst skk-string<= (str1 str2)
  "STR1 と STR2 とを比較して、`string<' か `string=' であれば、t を返す。"
  (or (skk-string< str1 str2)
      (string= str1 str2)))

(defsubst skk-do-auto-fill ()
  "`auto-fill-function' に値が代入されていれば、それをコールする。"
  (when auto-fill-function
    (funcall auto-fill-function)))

(defsubst skk-current-input-mode ()
  (cond (skk-abbrev-mode 'abbrev)
        (skk-latin-mode 'latin)
        (skk-jisx0208-latin-mode 'jisx0208-latin)
        (skk-katakana 'katakana)
        (skk-j-mode 'hiragana)))

(defsubst skk-get-current-candidate-1 (&optional count)
  (setq count (or count skk-henkan-count))
  (when (> 0 count)
    (skk-error "候補を取り出すことができません"
               "Cannot get current candidate"))
  ;; (nth -1 '(A B C)) は、A を返すので、負でないかどうかチェックする。
  (nth count skk-henkan-list))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>    ::= nil | (<tree> . <branch-list>)
;; <tree>         ::= (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         ::= (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         ::= <英小文字>
;; <nextstate>    ::= <英小文字文字列> | nil

(defsubst skk-make-raw-arg (arg)
  (cond ((eql arg '1) nil)
        ((eql arg '-1) '-)
        (t (if (numberp arg)
               (list arg)
             nil))))

(defsubst skk-unread-event (event)
  "Unread single EVENT."
  (setq unread-command-events
        (nconc unread-command-events (list event))))

(defsubst skk-get-last-henkan-datum (key)
  (cdr (assq key skk-last-henkan-data)))

(defun skk-put-last-henkan-data (alist)
  (let (e)
    (dolist (kv alist)
      (if (setq e (assq (car kv) skk-last-henkan-data))
          (setcdr e (cdr kv))
        (push (cons (car kv) (cdr kv))
              skk-last-henkan-data)))))

(defun skk-find-coding-system (code)
  "CODE が、Emacs が解釈する coding-system シンボル表現であればそのまま返し、
文字列であれば連想リスト `skk-coding-system-alist' を用いてシンボル表現へ変換する。
これら以外（nil を含む）であればシンボル euc-jis-2004 を返す。"
  (cond ((and code (coding-system-p code))
         code)

        ((and code (stringp code))
         (cdr (assoc code skk-coding-system-alist)))

        (t
         (cdr (assoc "euc" skk-coding-system-alist)))))

(defsubst skk-lisp-prog-p (string)
  "STRING が Lisp プログラムであれば、t を返す。"
  (let ((l (length string)))
    (and (> l 2)
         (eq (aref string 0) ?\()
         ;; second character is ascii or not.
         (skk-ascii-char-p (aref string 1))
         (eq (aref string (1- l)) ?\))      ; この行、もう不要かも
         (ignore-errors
           (= l (cdr (read-from-string string)))))))

(defun skk-eval-string (string)
  "Eval STRING as a lisp program and return the result."
  (cond ((string-match ";" string)
         (concat (skk-eval-string (substring string 0 (match-beginning 0)))
                 (substring string (match-beginning 0) (match-end 0))
                 (skk-eval-string (substring string (match-end 0)))))
        ((skk-lisp-prog-p string)
         (let (func face)
           ;; (^_^;) のような文字列に対し、read-from-string を呼ぶと
           ;; エラーになるので、ignore-errors で囲む。
           (ignore-errors
             (setq func (car (read-from-string string)))
             (when (and (listp func)
                        (functionp (car func)))
               (setq face (get-text-property 0 'face string))
               (setq string (if face
                                (propertize (format "%s" (eval func))
                                            'face face)
                              (format "%s" (eval func))))))
           string))
        (t
         string)))

;;;; from dabbrev.el.  Welcome!
;; 判定間違いを犯す場合あり。要改良。
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)))

(defun skk-quote-char-1 (word alist)
  (mapconcat (lambda (char)
               (or (cdr (assq char alist))
                   (char-to-string char)))
             ;; 文字列を対応する char のリストに分解する。
             (append word nil) ""))

(defun skk-key-binding-member (key commands &optional map)
  "入力 KEY が発動するコマンドが、COMMANDS に含まれれば non-nil を返す。
MAP は入力が書かれているキーマップを指定するが、指定されなければ
`skk-j-mode-map' を参照する。
この関数は、入力 KEY が `lookup-key' で探せない形式でありうる場合に用いる。"
  (unless map
    (setq map skk-j-mode-map))
  (let (keys)
    (dolist (command commands)
      (setq keys (nconc keys
                        (where-is-internal command map))))
    (member (key-description key)
            (mapcar #'key-description
                    keys))))

(defun skk-update-minor-mode-map-alist (mode map)
  (let ((element (assq mode minor-mode-map-alist)))
    (if element
        (setcdr element map)
      (add-to-list 'minor-mode-map-alist (cons mode map)))))

;; Functions from alist.el (APEL)
(defun skk-put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defun skk-del-alist (key alist)
  "Delete an element whose car equals KEY from ALIST.
Return the modified ALIST."
  (let ((pair (assoc key alist)))
    (if pair
        (delq pair alist)
      alist)))

(defun skk-remove-alist (symbol key)
  "Delete an element whose car equals KEY from the alist bound to SYMBOL."
  (and (boundp symbol)
       (set symbol (skk-del-alist key (symbol-value symbol)))))

(defun skk-reset-henkan-count (count)
  ;; ▽モードに戻るときは 0
  ;; ▼モードのまま候補一覧の手前に戻るときは 4
  (setq skk-henkan-count count)
  (skk-unread-event (aref (car (where-is-internal
                                'skk-previous-candidate
                                skk-j-mode-map))
                          0)))

(defun skk-escape-from-show-candidates (count)
  ;; skk-henkan まで一気に throw する。
  (skk-reset-henkan-count count)
  (throw 'unread nil))

(defun skk-nunion (x y)
  "X と Y の和集合を作る。
等しいかどうかの比較は、`equal' で行われる。X に Y を破壊的に連接する。"
  (cond
   ((null x)
    y)
   ((null y)
    x)
   (t
    (save-match-data
      (let ((list2 y) list1 origlist1 e1 e2)
        (while list2
          (setq list1 (cons nil x)
                e2 (car list2)
                origlist1 list1)
          (catch 'found
            (while (setq e1 (cadr list1))
              (cond
               ((equal e1 e2)
                (throw 'found nil))
               ((and (stringp e1)
                     (stringp e2)
                     (string-match ";" e1))
                (setq e1 (substring e1 0 (match-beginning 0)))
                (when (or (equal e1 e2)
                          (and
                           (string-match ";" e2)
                           (equal (substring e2 0 (match-beginning 0))
                                  e1)))
                  (throw 'found nil))))
              (setq list1 (cdr list1)))
            (setcdr list1 (list e2))
            (setq x (cdr origlist1)))
          (setq list2 (cdr list2)))
        x)))))

(defun skk-splice-in (org offset spliced)
  ;; ORG := '(A B C), SPLICED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (unless (> offset 0)
      (error "%s" "Cannot splice in!"))
    (setq tmp (nthcdr (1- offset) org)
          tail (cdr tmp))
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail
                    (nconc spliced tail)
                  spliced))
    org))

(defun skk-detach-extent (object)
  (when (overlayp object)
    (delete-overlay object)))

(defun skk-time-difference (a b)
  ;; from type-break.el.  Welcome!
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a))))


(provide 'skk-macs)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-macs.el ends here
