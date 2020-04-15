;;; skk.el --- Daredevil SKK (Simple Kana to Kanji conversion program) -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988-1997 Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999-2010 SKK Development Team <skk@ring.gr.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
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

;; SKK-MODE is a mode for inputting Japanese to a current buffer which is
;; composed of four minor modes described below.

;;      +---------------+-------- skk-mode -----+--------------------+
;;      |               |                       |                    |
;;      |               |                       |                    |
;;  skk-j-mode   skk-latin-mode   skk-jisx0208-latin-mode   skk-abbrev-mode
;;                  ASCII               JISX0208 LATIN         ABBREVIATION
;; (C-j wakes up skk-j-mode)      (ZEN'KAKU EIMOJI)
;;
;; skk-j-mode-map               skk-jisx0208-latin-mode-map
;;              skk-latin-mode-map                        skk-abbrev-mode-map

;; skk-katakana: nil
;;   HIRAKANA

;; skk-katakana: t
;;   KATAKANA

;;; Code:

(eval-when-compile ; shut up compiler warning.
  (defvar enable-character-translation)
  (defvar epoch::version)
  (defvar jka-compr-compression-info-list)
  (defvar message-log-max)
  (defvar migemo-isearch-enable-p)
  (defvar minibuffer-local-ns-map)
  (defvar self-insert-after-hook)
  (defvar skk-rdbms-private-jisyo-table)
  (defvar this-command-char))

;; Daredevil SKK 13.1 は APEL 10.7 を要求する。
;; APEL 10.7 は主に install.el の機能のために要求されている。
;; APEL 10.6 はセキュリティ上の重大な欠陥に対応しているため、必須である。
;; make-temp-file() の欠陥については関数 skk-save-jisyo-original() のコメ
;; ントを参照。

(eval-when-compile
  (require 'cl-lib))

;; Emacs standard library.
(require 'advice)
(require 'easymenu)

(eval-and-compile
  ;; SKK common.


  ;; skk の起動時、*-autoloads は下記の２種類のうちどちらかに限って存在する。
  ;; MELPA経由の場合、 autoload は package.el によって管理されるため、
  ;; skk-autoloadsがロードできない場合、単に無視する。
  ;;                | make でインストール     | MELPA 経由でインストール
  ;;   ファイル名     | skk-autoloads.el     | ddskk-autoloads.el
  ;;   provide 宣言  | あり (SKK-MK が生成)   | なし
  (require 'skk-autoloads nil 'noerror)

  (provide 'skk)                        ; workaround for recursive require
  (require 'skk-vars)
  (require 'skk-macs)
  (require 'skk-emacs)

  ;; Shut up, compiler.
  (autoload 'skk-kanagaki-initialize "skk-kanagaki") ; nicola/
  (autoload 'skk-rdbms-count-jisyo-candidates "skk-rdbms"))

;;;###autoload
(defun skk-mode (&optional arg)
  "日本語入力モード。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
負の引数を与えると SKK モードから抜ける。

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is
\"かな\".  Lowercase romaji inputs are automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana \(mode line
indicator \"カナ\"\) input submodes.

`l' is used to enter ASCII submode \(mode line indicator \"SKK\"\).
Uppercase `L' enters JISX0208 latin \(wide ASCII\) submode \(mode line
indicator \"全英\"\).  `\C-j' returns to hiragana submode from either
ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words \(eg, nouns\) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading \"▽\" indicates that
kanji conversion is in progress.  After entering the reading, press
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => \"▽べんり\", and pressing space produces \"▼便利\" \(the
solid triangle indicates that conversion is in progress\).  Backspace
steps through the candidate list in reverse.

A candidate can be accepted by pressing `\C-j', or by entering a
self-inserting character.  \(Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.\)

Inflected words \(verbs and adjectives\), like non-inflected words, begin
input with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> \"▼強い\".  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed \(pressing space is not necessary\).  Space and
backspace are used to step forward and backward through the list of
candidates.

For more information, see the `skk' topic in Info.  \(Japanese only.\)

A tutorial is available in Japanese or English via \\[skk-tutorial].
Use a prefix argument to choose the language.  The default is system-
dependent."
  (interactive "P")
  (setq skk-mode (cond ((null arg)
                        (not skk-mode))
                       ;; - は -1 に変換される。
                       ((> (prefix-numeric-value arg) 0)
                        t)
                       (t
                        nil)))
  (if (not skk-mode)
      ;; exit skk-mode
      (skk-mode-exit)
    ;; enter into skk-mode.
    (unless skk-mode-invoked
      ;; enter `skk-mode' for the first time in this session.
      (skk-mode-invoke))
    ;; 以下は skk-mode に入るたびに毎回コールされるコード。
    (skk-create-file (skk-jisyo)
                     "空の SKK 個人辞書を作りました"
                     "I have created an empty SKK Jisyo file for you")
    (skk-require-module)
    ;; To terminate kana input.
    (skk-add-skk-pre-command)
    (add-hook 'post-command-hook 'skk-after-point-move nil 'local)
    (skk-search-ja-dic-maybe t)
    (skk-j-mode-on)
    (run-hooks 'skk-mode-hook)))

;;;###autoload
(defun skk-auto-fill-mode (&optional arg)
  "日本語入力モード。自動折り返し機能付き。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
正の引数を与えると、強制的に `auto-fill-mode' 及び SKK モードに入る。
負の引数を与えると `auto-fill-mode' 及び SKK モードから抜ける。"
  (interactive "P")
  (let ((auto-fill
         (cond ((null arg)
                (not auto-fill-function))
               ((> (prefix-numeric-value arg) 0)
                t))))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (run-hooks 'skk-auto-fill-mode-hook)))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "個人辞書を保存せずに Emacs を終了する。"
  (interactive "P")
  ;; format を引数に持たせた場合は、skk-yes-or-no-p を使うとかえって冗長にな
  ;; る。
  (when (yes-or-no-p
         (if skk-japanese-message-and-error
             "個人辞書を保存せずに Emacs を終了します。良いですか？ "
           "Do you really wish to kill Emacs without saving Jisyo? "))
    (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
      (remove-hook 'kill-emacs-hook 'skk-save-jisyo)
      (when buff
        (set-buffer buff)
        (set-buffer-modified-p nil)
        (kill-buffer buff))
      (save-buffers-kill-emacs query))))

(defun skk-restart ()
  "`skk-init-file' の再ロード及び各種再設定の後 SKK モードを起動する。
各種ユーザ変数をいったん初期化するので、~/.emacs.d/init.el で SKK の設定をしている場合は
使うべきでない。
`skk-kakutei-key' の変更には対応できていない。"
  (interactive)
  (skk-save-jisyo)
  (setq skk-jisyo-update-vector nil)
  (when (featurep 'skk-server)
    (skk-disconnect-server))
  (setq skk-server-disable-completion nil)
  (kill-local-variable 'skk-rule-tree)
  (setq skk-rule-tree nil)
  ;; いっそ unload-feature とかしたほうがいいのかもしれない。
  ;; skk-kakutei-key に関しては minibuffer-local-map などの処理も。
  (mapatoms (lambda (sym)
              ;; skk-user-directory, skk-init-file 以外の defcustom で宣言
              ;; された変数のうち、saved-value を持たないものを再初期化。
              ;; 他にも除外すべき変数がないか要検討。
              (when (and (string-match "^skk-" (symbol-name sym))
                         (not (memq sym '(skk-user-directory skk-init-file)))
                         (not (plist-member (symbol-plist sym) 'saved-value))
                         (plist-member (symbol-plist sym) 'standard-value))
                (set-default sym
                             (eval (car (get sym 'standard-value)))))))
  (dolist (feature '(skk-act skk-azik))
    (when (featurep feature)
      (unload-feature feature)))
  (let (skk-mode-invoked)
    (skk-mode 1)))

(defun skk-require-module ()
  ;; skk-mode に入るたびに実行される
  (when skk-use-viper
    (require 'skk-viper))
  (when (or skk-servers-list
            skk-server-host
            (getenv "SKKSERVER"))
    (require 'skk-server))
  (when (featurep 'skk-server)
    (skk-adjust-search-prog-list-for-server-search))
  (when skk-auto-okuri-process
    (require 'skk-auto)
    (skk-adjust-search-prog-list-for-auto-okuri))
  (when skk-use-look
    (require 'skk-look))
  (when (featurep 'skk-jisx0201)
    (setq skk-use-jisx0201-input-method t))
  (when (or skk-dcomp-activate
            skk-dcomp-multiple-activate)
    (require 'skk-dcomp))
  (when skk-sticky-key
    (require 'skk-sticky))
  (when skk-show-mode-show
    (require 'skk-dcomp)
    (require 'skk-show-mode))
  (when skk-use-search-web
    (require 'skk-search-web)))

(defun skk-mode-exit ()
  (let ((skk-mode t)
        skk-show-mode-show)
    (skk-kakutei))
  (skk-mode-off)
  (skk-remove-skk-pre-command)
  (remove-hook 'post-command-hook
               'skk-after-point-move
               'local)
  (skk-update-modeline)

  (dolist (b `("*候補*" "*SKK annotation*" "*単漢字*"
               " *数値変換タイプ*" ,skk-list-chars-buffer-name))
    (when (get-buffer b)
      (kill-buffer b))))

(defun skk-mode-invoke ()
  ;; 一番最初の skk 起動時のみ実行される
  (when skk-user-directory
    (make-directory skk-user-directory t))
  (skk-compile-init-file-maybe)
  (skk-cus-setup)
  (load skk-init-file t)
  (skk-adjust-user-option)
  (skk-setup-modeline)
  (when (default-value 'skk-share-private-jisyo)
    (skk-setup-shared-private-jisyo))
  (when skk-keep-record
    (skk-create-file skk-record-file
                     "SKK の統計情報用ファイルを作りました"
                     "I have created an SKK record file for you"))
  (skk-setup-auto-paren) ; necessary to call before compiling skk-rule-tree.
  ;; SKK 拡張入力機能の設定
  (cond (skk-use-act
         ;; 拡張ローマ字入力 ACT
         (require 'skk-act))
        (skk-use-azik
         ;; 拡張ローマ字入力 AZIK
         (require 'skk-azik))
        ((featurep 'skk-tutcdef)
         ;; TUT-code
         (require 'skk-tutcode))
        (skk-use-kana-keyboard
         ;; 仮名入力 (日本語旧 JIS または親指シフト)
         (skk-kanagaki-initialize)))
  (skk-emacs-prepare-menu)
  (skk-setup-charset-list)
  (skk-setup-delete-selection-mode)
  (when skk-annotation-lookup-DictionaryServices
    (skk-annotation-start-python))
  (setq skk-mode-invoked t))

;;; setup
(defun skk-setup-shared-private-jisyo ()
  ;; vector の長さは、`skk-save-jisyo' が失敗しない前提ならば
  ;; skk-jisyo-save-count と同じでよい。エラーその他で失敗する可能性を
  ;; 考慮すると、それより少し長い方が問題が起こりにくい。
  ;; (以下の具体的な数字には根拠はない)
  (setq skk-jisyo-update-vector
        (make-vector (max (* 2 skk-jisyo-save-count)
                          (+ 20 skk-jisyo-save-count))
                     nil))
  (setq skk-emacs-id
        (concat (system-name) ":"
                (number-to-string (emacs-pid)) ":"
                (mapconcat 'int-to-string (current-time) "") ":"))
  (skk-create-file skk-emacs-id-file nil nil 384) ; 0600
  (with-temp-buffer
    (insert-file-contents skk-emacs-id-file)
    (insert skk-emacs-id "\n")
    (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)))

(defun skk-setup-charset-list ()
  (setq skk-charset-list
        (delq nil (mapcar (lambda (c)
                            (when (charsetp c)
                              c))
                          '(ascii
                            japanese-jisx0208
                            japanese-jisx0213-1
                            japanese-jisx0213-2
                            japanese-jisx0213.2004-1
                            katakana-jisx0201
                            latin-jisx0201
                            japanese-jisx0212
                            japanese-jisx0208-1978)))))

(defun skk-setup-keymap ()
  "SKK のキーを設定する。"
  (cond (skk-j-mode
         (skk-define-j-mode-map)
         (skk-setup-j-mode-map-options))
        (skk-latin-mode
         (skk-define-latin-mode-map)
         (skk-setup-latin-mode-map-options))
        (skk-jisx0208-latin-mode
         (skk-define-jisx0208-latin-mode-map)
         (skk-setup-jisx0208-latin-mode-map-options))
        (skk-abbrev-mode
         (skk-define-abbrev-mode-map)
         (skk-setup-abbrev-mode-map-options)))
  (skk-define-minibuffer-maps)
  ;;
  (unless skk-rule-tree
    (setq skk-rule-tree (skk-compile-rule-list skk-rom-kana-base-rule-list
                                               skk-rom-kana-rule-list))))

(defun skk-define-menu (map)
  "SKK のプルダウンメニューを定義する。"
  (easy-menu-define skk-menu map "Menu used in SKK mode." skk-menu-items))

(defun skk-define-j-mode-map ()
  "キーマップ `skk-j-mode-map' を定義する。"
  (unless (keymapp skk-j-mode-map)
    (setq skk-j-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-j-mode skk-j-mode-map)
    (skk-update-minor-mode-map-alist 'skk-jisx0201-mode skk-j-mode-map))
  (unless (eq (lookup-key skk-j-mode-map "a") 'skk-insert)
    (dotimes (i 95)         ;from " " to "~".
      (define-key skk-j-mode-map (skk-char-to-unibyte-string (+ 32 i))
        'skk-insert))
    (skk-define-menu skk-j-mode-map)))

(defun skk-define-latin-mode-map ()
  "キーマップ `skk-latin-mode-map' を定義する。"
  (unless (keymapp skk-latin-mode-map)
    (setq skk-latin-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-latin-mode skk-latin-mode-map)
    (skk-define-menu skk-latin-mode-map)))

(defun skk-define-jisx0208-latin-mode-map ()
  "キーマップ `skk-jisx0208-latin-mode-map' を定義する。"
  (unless (keymapp skk-jisx0208-latin-mode-map)
    (setq skk-jisx0208-latin-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-jisx0208-latin-mode
                                     skk-jisx0208-latin-mode-map))
  (unless (eq (lookup-key skk-jisx0208-latin-mode-map "a")
              'skk-jisx0208-latin-insert)
    (dotimes (i 128)
      (when (aref skk-jisx0208-latin-vector i)
        (define-key skk-jisx0208-latin-mode-map
          (skk-char-to-unibyte-string i)
          'skk-jisx0208-latin-insert)))
    (define-key skk-jisx0208-latin-mode-map "\C-q" 'skk-toggle-characters)
    (skk-define-menu skk-jisx0208-latin-mode-map)))

(defun skk-define-abbrev-mode-map ()
  "キーマップ `skk-abbrev-mode-map' を定義する。"
  (unless (keymapp skk-abbrev-mode-map)
    (setq skk-abbrev-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-abbrev-mode skk-abbrev-mode-map)
    (dotimes (i 95)
      (define-key skk-abbrev-mode-map (skk-char-to-unibyte-string (+ 32 i))
        'skk-abbrev-insert))
    (define-key skk-abbrev-mode-map "," 'skk-abbrev-comma)
    (define-key skk-abbrev-mode-map "." 'skk-abbrev-period)
    (define-key skk-abbrev-mode-map "\C-q" 'skk-toggle-characters)
    (skk-define-menu skk-abbrev-mode-map)))

(defun skk-define-minibuffer-maps ()
  (unless (eq (lookup-key minibuffer-local-map skk-kakutei-key) 'skk-kakutei)
    (define-key minibuffer-local-map skk-kakutei-key #'skk-kakutei)
    (define-key minibuffer-local-completion-map skk-kakutei-key #'skk-kakutei)
    (define-key minibuffer-local-ns-map skk-kakutei-key #'skk-kakutei)))

(skk-define-abbrev-mode-map)
(skk-define-latin-mode-map)
(skk-define-jisx0208-latin-mode-map)
(skk-define-j-mode-map)

(defun skk-setup-j-mode-map-options ()
  (unless (eq (lookup-key skk-j-mode-map
                          (skk-char-to-unibyte-string skk-try-completion-char))
              'skk-insert)
    (when (vectorp skk-kakutei-key)
      (define-key skk-j-mode-map skk-kakutei-key #'skk-kakutei))
    (define-key skk-j-mode-map
      (skk-char-to-unibyte-string skk-try-completion-char)
      #'skk-insert)

    ;; Workaround for key translation.
    (when (eq skk-try-completion-char 9)
      ;; tab キーは <tab> の定義が無ければ TAB の定義が割り当てられる。
      ;; Org-mode などは <tab> を定義するので，SKK の方でも <tab> を定義
      ;; する必要がある。
      (define-key skk-j-mode-map [(tab)] #'skk-comp-wrapper))

    ;; comp と dcomp での前候補へ戻る動作を Shift TAB でも可能とする。
    (when skk-previous-completion-use-backtab
      (define-key skk-j-mode-map skk-previous-completion-backtab-key
        #'skk-previous-comp-maybe))
    ;;
    (unless (featurep 'skk-kanagaki)
      (dolist (key skk-previous-candidate-keys)
        (define-key skk-j-mode-map key #'skk-previous-candidate)))
    ;;
    (when skk-use-jisx0201-input-method
      ;; This command is autoloaded.
      (define-key skk-j-mode-map "\C-q" #'skk-toggle-katakana))
    (unless skk-use-viper
      (define-key skk-j-mode-map
        (skk-char-to-unibyte-string skk-start-henkan-with-completion-char)
        #'skk-comp-start-henkan)
      (define-key skk-j-mode-map
        (skk-char-to-unibyte-string skk-backward-and-set-henkan-point-char)
        #'skk-backward-and-set-henkan-point))
    (skk-setup-delete-backward-char)
    (skk-setup-undo)
    ;;
    (when (eq skk-j-mode-function-key-usage 'conversion)
      (define-key skk-j-mode-map [f1] #'skk-start-henkan-prog-1)
      (define-key skk-j-mode-map [f2] #'skk-start-henkan-prog-2)
      (define-key skk-j-mode-map [f3] #'skk-start-henkan-prog-3)
      (define-key skk-j-mode-map [f4] #'skk-start-henkan-prog-4)
      (define-key skk-j-mode-map [f5] #'skk-start-henkan-prog-5)
      (define-key skk-j-mode-map [f6] #'skk-start-henkan-prog-6)
      (define-key skk-j-mode-map [f7] #'skk-start-henkan-prog-7)
      (define-key skk-j-mode-map [f8] #'skk-start-henkan-prog-8)
      (define-key skk-j-mode-map [f9] #'skk-start-henkan-prog-9)
      (define-key skk-j-mode-map [f10] #'skk-start-henkan-prog-0))))

(defun skk-setup-latin-mode-map-options ()
  (unless (eq (lookup-key skk-latin-mode-map skk-kakutei-key) 'skk-kakutei)
    (define-key skk-latin-mode-map skk-kakutei-key #'skk-kakutei)))

(defun skk-setup-jisx0208-latin-mode-map-options ()
  (unless (eq (lookup-key skk-jisx0208-latin-mode-map skk-kakutei-key)
              'skk-kakutei)
    (define-key skk-jisx0208-latin-mode-map skk-kakutei-key #'skk-kakutei)
    (unless skk-use-viper
      (define-key skk-jisx0208-latin-mode-map
        (skk-char-to-unibyte-string skk-backward-and-set-henkan-point-char)
        #'skk-backward-and-set-henkan-point))))

(defun skk-setup-abbrev-mode-map-options ()
  (unless (eq (lookup-key skk-abbrev-mode-map skk-kakutei-key) 'skk-kakutei)
    (define-key skk-abbrev-mode-map skk-kakutei-key #'skk-kakutei)
    (define-key skk-abbrev-mode-map
      (skk-char-to-unibyte-string skk-start-henkan-char)
      #'skk-start-henkan)
    (define-key skk-abbrev-mode-map
      (skk-char-to-unibyte-string skk-try-completion-char)
      #'skk-try-completion)
    (unless skk-use-viper
      (define-key skk-abbrev-mode-map
        (skk-char-to-unibyte-string skk-start-henkan-with-completion-char)
        #'skk-comp-start-henkan))))

(defun skk-make-indicator-alist ()
  "SKK インジケータ型オブジェクトを用意し、連想リストにまとめる。"
  (skk-emacs-prepare-modeline-properties)
  (let ((mode-string-list '(skk-latin-mode-string
                            skk-hiragana-mode-string
                            skk-katakana-mode-string
                            skk-jisx0208-latin-mode-string
                            skk-abbrev-mode-string
                            skk-jisx0201-mode-string))
        mode string base)
    (save-match-data
      (cons
       (cons 'default
             (cons "" (skk-mode-string-to-indicator 'default "")))
       (mapcar (lambda (symbol)
                 (setq mode (prin1-to-string symbol))
                 (string-match "skk-\\([-a-z0-9]+\\)-mode-string" mode)
                 (setq mode (intern (match-string-no-properties 1 mode)))
                 (setq string (symbol-value symbol))
                 ;; 本来ならこのようにユーザ変数を加工するのはおかしいが、
                 ;; 移行期の処置として暫定的に行なう。
                 (cond
                  ((string-match "^ +" string)
                   ;; minor-mode setting
                   (setq base (substring string (match-end 0))))
                  ((string-match "^--" string)
                   ;; mode-line left setting
                   (setq base (substring string (match-end 0)))
                   (when (string-match "::*$" base)
                     (setq base (substring base 0 (match-beginning 0)))))
                  (t
                   (setq base string)))
                 (cons mode
                       (cons (concat " " base)
                             (skk-make-indicator-alist-1 mode base))))
               mode-string-list)))))

(defun skk-make-indicator-alist-1 (mode base)
  (skk-mode-string-to-indicator 'mode-line
                                (concat "--" base
                                        (cond ((skk-face-proportional-p 'mode-line)
                                               ":")
                                              ((memq mode '(latin abbrev))
                                               "::")
                                              (t
                                               ":")))))

(defun skk-setup-modeline ()
  "モードラインへのステータス表示を準備する。"
  (setq skk-indicator-alist (skk-make-indicator-alist))
  (cl-case skk-status-indicator
    (left
     (unless (and mode-line-format
                  (memq 'skk-modeline-input-mode (default-value 'mode-line-format)))
       (setq-default mode-line-format
                     (append '("" skk-modeline-input-mode)
                             (default-value 'mode-line-format))))
     (skk-loop-for-buffers (buffer-list)
       (when (and (consp mode-line-format)
                  (local-variable-p 'mode-line-format)
                  (null (memq 'skk-modeline-input-mode mode-line-format)))
         (setq mode-line-format
               (append '("" skk-modeline-input-mode) mode-line-format))))
     (when skk-icon
       (unless (memq 'skk-icon (default-value 'mode-line-format))
         (setq-default mode-line-format
                       (append '("" skk-icon)
                               (default-value 'mode-line-format))))
       (skk-loop-for-buffers (buffer-list)
         (when (and (consp mode-line-format)
                    (local-variable-p 'mode-line-format)
                    (null (memq 'skk-icon mode-line-format)))
           (setq mode-line-format (append '("" skk-icon) mode-line-format)))))
     (force-mode-line-update t))
    ;;
    (t
     (when (and (consp mode-line-format)
                (equal (car mode-line-format) "")
                (eq 'skk-modeline-input-mode (nth 1 mode-line-format)))
       ;; for skk-restart.
       (setq-default mode-line-format
                     (nthcdr 2 mode-line-format)))

     (skk-loop-for-buffers (buffer-list)
       (when (and (consp mode-line-format)
                  (equal (car mode-line-format) "")
                  (eq 'skk-modeline-input-mode (nth 1 mode-line-format)))
         ;; for skk-restart.
         (setq mode-line-format (nthcdr 2 mode-line-format))))
     (setq-default skk-modeline-input-mode "")
     (add-minor-mode 'skk-mode 'skk-modeline-input-mode))))

(defun skk-setup-emulation-commands (commands emulation)
  (let ((map (if (and (boundp 'overriding-local-map)
                      (keymapp 'overriding-local-map))
                 overriding-local-map
               (current-global-map))))
    (dolist (command commands)
      (dolist (key (where-is-internal command map))
        (define-key skk-abbrev-mode-map key emulation)
        (define-key skk-j-mode-map key emulation)))))

(defun skk-setup-delete-backward-char ()
  "「後退」系のキーにコマンド `skk-delete-backward-char' を割当てる。"
  (skk-setup-emulation-commands
   skk-delete-backward-char-commands 'skk-delete-backward-char))

(defun skk-setup-undo ()
  "「やり直し」系のキーにコマンド `skk-undo' を割当てる。"
  (skk-setup-emulation-commands skk-undo-commands 'skk-undo))

(defun skk-setup-verbose-messages ()
  (unless skk-henkan-on-message
    (let ((list
           (split-string
            (apply 'format
                   ;; fotmat#STRING
                   (cl-case skk-j-mode-function-key-usage
                     (conversion
                      "\
\[F5]%s  [F6]%s  [F7]%s  [F8]%s  [F9]%s  [F10]%s")
                     (t
                      "\
\[C-5 SPC]%s  [C-6 SPC]%s  [C-7 SPC]%s  [C-8 SPC]%s  [C-9 SPC]%s  [C-0 SPC]%s"))
                   ;; format#OBJECTS
                   (mapcar
                    (lambda (x)
                      (cdr
                       (assoc (caar (symbol-value
                                     (intern-soft
                                      (format "skk-search-prog-list-%d"
                                              x))))
                              '((skk-search-tankanji . "単漢字")
                                (skk-search-identity . "無変換")
                                (skk-search-katakana . "カタカナ")
                                (skk-search-hankaku-katakana . "半角カナ")
                                (skk-search-jisx0208-romaji . "全角ローマ")
                                (skk-search-romaji . "ローマ")
                                (nil . "未定義"))
                              )))
                    '(5 6 7 8 9 0))) "  "))
          new) ; END varlist
      ;; BODY
      (dolist (x list)
        (let* ((y (split-string x "]"))
               (s1 (car y))
               (s2 (nth 1 y)))
          (setq new (concat new
                            (propertize (concat s1 "]") 'face
                                        'skk-verbose-kbd-face)
                            s2 " "))))
      (setq skk-henkan-on-message new))
    ))

(defun skk-compile-init-file-maybe ()
  "必要なら `skk-init-file' をバイトコンパイルする。
`skk-byte-compile-init-file' が non-nil の場合で、`skk-init-file' をバイトコ
ンパイルしたファイルが存在しないか、そのバイトコンパイル済ファイルより
`skk-init-file' の方が新しいときは、`skk-init-file' をバイトコンパイルする。

`skk-byte-compile-init-file' が nil の場合で、`skk-init-file' をバイトコンパ
イルしたファイルより `skk-init-file' の方が新しいときは、そのバイトコンパイ
ル済ファイルを消す。"
  (save-match-data
    (let* ((file (expand-file-name skk-init-file))
           (init-file (if (file-exists-p (concat file ".el"))
                          (concat file ".el")
                        file))
           (elc (concat init-file
                        (if (string-match "\\.el$" init-file)
                            "c"
                          ".elc"))))
      (if skk-byte-compile-init-file
          (when (and (file-exists-p init-file)
                     (or (not (file-exists-p elc))
                         (file-newer-than-file-p init-file elc)))
            (save-window-excursion ; for keep window configuration.
              (skk-message "%s をバイトコンパイルします"
                           "Byte-compile %s"
                           skk-init-file)
              (sit-for 2)
              (byte-compile-file init-file)))
        (when (and (file-exists-p init-file)
                   (file-exists-p elc)
                   (file-newer-than-file-p init-file elc))
          (delete-file elc))))))

(defun skk-delete-selection-wrapper ()
  (unless (and (member (char-to-string last-command-event)
                       (mapcar (lambda (x) (car x)) skk-auto-paren-string-alist))
               skk-use-auto-enclose-pair-of-region)
    (delete-region (point) (mark))
    (if (and overwrite-mode
             (eq this-command 'self-insert-command))
        (let ((overwrite-mode nil))
          (self-insert-command
           (prefix-numeric-value current-prefix-arg))
          (setq this-command 'ignore)))))

(defun skk-setup-delete-selection-mode ()
  "Delete Selection モードのための設定をする。
Delete Selection モードが SKK を使った日本語入力に対しても機能するように
セットアップする。"
  (let ((funcs '(skk-current-kuten
                 skk-current-touten
                 skk-input-by-code-or-menu
                 skk-today))
        (supersede-funcs '(skk-delete-backward-char)))
    (unless (get 'skk-insert 'delete-selection)
      (dolist (func funcs)
        (put func 'delete-selection t))
      (dolist (func supersede-funcs)
        (put func 'delete-selection 'supersede))
      (put 'skk-insert 'delete-selection 'skk-delete-selection-wrapper))))

(defun skk-setup-auto-paren ()
  (when (and skk-auto-insert-paren
             skk-auto-paren-string-alist)
    ;; skk-auto-paren-string-alist の中から、
    ;; skk-special-midashi-char-list の要素に関連するものを取り除く。
    (dolist (s (mapcar #'char-to-string
                       skk-special-midashi-char-list))
      (skk-remove-alist 'skk-auto-paren-string-alist s))

    (when (memq t (mapcar (lambda (e)
                            (skk-ascii-char-p (string-to-char (car e))))
                          skk-auto-paren-string-alist))
      (let (rulealst str)
        (setq rulealst (nconc (mapcar (lambda (e)
                                        (nth 2 e))
                                      skk-rom-kana-rule-list)
                              (mapcar (lambda (e)
                                        (nth 2 e))
                                      skk-rom-kana-base-rule-list)))
        (dolist (cell skk-auto-paren-string-alist)
          (setq str (car cell))
          (when (and (skk-ascii-char-p (string-to-char str))
                     ;; 出力文字が入っているセルを調べて、いずれかの
                     ;; キーにバインドされていないかどうかを確認する。
                     (null (assoc str rulealst))
                     (null (rassoc str rulealst))
                     ;; 割り付けようとしているキーが、何か他の出力文字に
                     ;; バインドされていないかどうかを確認する。
                     (null (assoc str skk-rom-kana-base-rule-list))
                     (null (assoc str skk-rom-kana-rule-list)))
            ;; skk-auto-paren-string-alist の各要素の car の文字が
            ;; ascii char である場合は、skk-rom-kana-rule-list,
            ;; skk-rom-kana-base-rule-list にその文字を書き込む。
            ;; 本来は ascii char は skk-rom-kana-rule-list,
            ;; skk-rom-kana-base-rule-list に書く必要がない
            ;; --- skk-emulate-original-mapによる入力が行なわれる ---
            ;; が、skk-auto-paren-string-alist に指定された対になる
            ;; 文字の挿入のためには、キーとなる文字を書いておく必要が
            ;; ある。
            (setq skk-rom-kana-rule-list
                  (cons (list str nil str)
                        skk-rom-kana-rule-list))))))))

(defun skk-setup-minibuffer ()
  "カレントバッファの入力モードに従ってミニバッファの入力モードを設定する。"
  (cl-case skk-minibuffer-origin-mode
    (hiragana
     (skk-j-mode-on))
    (katakana
     (skk-j-mode-on t))
    (abbrev
     (skk-abbrev-mode-on))
    (latin
     (skk-latin-mode-on))
    (jisx0208-latin
     (skk-jisx0208-latin-mode-on))))

;;;###autoload
(defun skk-setup-jisyo-buffer ()
  "SKK の辞書バッファを用意する。
`skk-jisyo' の辞書バッファで、
 (1)空バッファであれば、新しくヘッダーを作り、
 (2)辞書エントリがある既存の辞書バッファならば、ヘッダーが正しいかどうかを
    チェックする。"

  ;; skk-okuri-ari-min と skk-okuri-nasi-min の位置を変更した。
  ;;                       ↓ 新しい skk-okuri-ari-min
  ;;   ;; okuri-ari entries.
  ;;   ← 以前の skk-okuri-ari-min
  ;;
  ;;   ↓ skk-okuri-ari-max ↓ 新しい skk-okuri-nasi-min
  ;;   ;; okuri-nasi entries.
  ;;   ← 以前の skk-okuri-nasi-min
  ;;
  ;;
  ;; 変更前の位置であれば、下記のような空辞書の場合、
  ;;
  ;;   ;; okuri-ari entries.
  ;;   ;; okuri-nasi entries.
  ;;
  ;; skk-okuri-ari-min と skk-okuri-ari-max のマーカーが重なってしまい、
  ;; skk-okuri-ari-min の位置に挿入した候補が skk-okuri-ari-max のマーカー
  ;; を後方に押しやらない。
  ;;
  ;; case-fold-search は、辞書バッファでは常に nil。
  (save-match-data
    (when (zerop (buffer-size))
      ;; 空バッファだったら、ヘッダーのみ挿入。
      (insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n"))
    (goto-char (point-min))
    (unless (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
      (skk-error "送りありエントリのヘッダーがありません！"
                 "Header line for okuri-ari entries is missing!"))
    ;; 固定ポイントなので、(point) で十分。
    (setq skk-okuri-ari-min (point))
    (unless (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
      (skk-error "送りなしエントリのヘッダーがありません！"
                 "Header line for okuri-nasi entries is missing!"))
    (beginning-of-line)
    ;; 共有辞書なら固定ポイントでも良いのだが、辞書バッファで編集を行
    ;; なったときのことを配慮してマーカーにしておく。
    (setq skk-okuri-ari-max (point-marker))
    (forward-line 1)
    (backward-char 1)
    (setq skk-okuri-nasi-min (point-marker))))

;;;###autoload
(defun skk-emulate-original-map (arg)
  "キー入力に対して Emacs のオリジナルのキー割り付けでコマンドを実行する。"
  (let ((prefix-arg arg)
        (keys (skk-command-key-sequence (this-command-keys) this-command)))
    (when keys ; If key is nil, the command may have been invoked by M-x.
      (let (skk-mode
            skk-latin-mode
            skk-j-mode
            skk-abbrev-mode
            skk-jisx0208-latin-mode
            skk-jisx0201-mode
            command)
        ;; have to search key binding after binding 4 minor mode flags to nil.
        (setq command (key-binding keys))
        (unless command
          (setq keys (lookup-key function-key-map keys))
          (when keys
            (setq command (key-binding keys))))
        ;; avoid recursive calling of skk-emulate-original-map.
        (unless (eq command this-command)
          (setq this-command command)
          (unless (or (memq command skk-undo-commands)
                      (memq command skk-delete-backward-char-commands))
            (skk-cancel-undo-boundary))
          ;; if no bindings are found, call `undefined'.  it's
          ;; original behaviour.
          (command-execute (or command
                               #'undefined)))))))

(defun skk-command-key-sequence (key command)
  "KEY から universal arguments を取り除き、COMMAND を実行するキーを返す。
`execute-extended-command' によってコマンドが実行された場合は、nil を返す。"
  (while (not (or (zerop (length key))
                  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (unless (zerop (length key))
    key))

;;;###autoload
(defun skk-adjust-user-option ()
  "ユーザオプションの不整合を調整する。"
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  ;; 両立できないオプションを調整する。
  (when skk-process-okuri-early
    ;; skk-process-okuri-early の値が non-nil であるときに下記の値が non-nil
    ;; であれば正常に動かないのでこの変数の優先順位を高くした。
    (setq skk-kakutei-early nil
          skk-auto-okuri-process nil
          skk-henkan-okuri-strictly nil
          skk-henkan-strict-okuri-precedence nil))
  (unless skk-jisyo-save-count
    ;; 現在の実装では、個人辞書のオートセーブ無しでは個人辞書の共有はできない
    ;; ことになっている。
    (setq skk-share-private-jisyo nil)))

;;;###autoload
(defun skk-latin-mode (arg)
  "SKK のモードをアスキーモードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-latin-mode-on)
  nil)

;;;###autoload
(defun skk-jisx0208-latin-mode (arg)
  "SKK のモードを全英モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0208-latin-mode-on)
  nil)

(defun skk-abbrev-mode (arg)
  "ascii 文字をキーにした変換を行うための入力モード。"
  (interactive "*P")
  (cl-case skk-henkan-mode
    (active
     (skk-kakutei))
    (on
     (skk-error "既に▽モードに入っています" "Already in ▽ mode")))
  (let (skk-dcomp-activate)
    (skk-set-henkan-point-subr))
  (skk-abbrev-mode-on)
  nil)

;;;###autoload
(defun skk-toggle-characters (arg)
  "■モード、▼モードで、かなモードとカナモードをトグルで切り替える。
▽モードでは `skk-henkan-start-point' (▽の直後) とカーソルの間の文字列につい
て、ひらがなとカタカナを入れ替える。"
  (interactive "P")
  (let ((show skk-show-mode-enable))    ;チュートリアル実行中のみ nil
    (cond
     ((eq skk-henkan-mode 'on)      ;▽モード
      (let (char-type)
        (skk-save-point
         (goto-char skk-henkan-start-point)
         (while (and (>= skk-save-point (point))
                     ;; (not (eobp))
                     (or
                      ;; "ー" では文字種別が判別できないので、ポイントを進める。
                      (looking-at "ー")
                      (eq 'unknown (setq char-type (skk-what-char-type)))))
           (forward-char 1)))
        (skk-henkan-skk-region-by-func
         (cl-case char-type
           (hiragana #'skk-katakana-region)
           (katakana #'skk-hiragana-region)
           (jisx0208-latin #'skk-latin-region)
           (ascii #'skk-jisx0208-latin-region))
         ;; `skk-katakana-region' の引数 VCONTRACT または
         ;; `skk-hiragana-region' の引数 VEXPAND を与える。
         (memq char-type '(hiragana katakana))))
      (setq show nil))
     ((and (skk-in-minibuffer-p)
           (not skk-j-mode))
      ;; ミニバッファへの初突入時。
      (skk-j-mode-on))
     (t
      (setq skk-katakana (not skk-katakana))))
    (skk-kakutei)
    (when skk-j-mode
      (let ((skk-show-mode-show show))
        (skk-j-mode-on skk-katakana))))
  nil)

(defun skk-misc-for-picture ()
  "`picture-mode' へ入ったときに SKK 起動前の状態に戻す。
`edit-picture-hook' に `add-hook' して使用する。"
  ;; 既存のバッファを picture mode にしたとき、picture-mode 関数は
  ;; kill-all-local-variables 関数を呼ばないので、SKK 関連のバッファローカル
  ;; 変数が元のバッファの値のままになってしまう。そこで、picture mode に入った
  ;; ときにフックを利用してこれらのバッファローカル変数を kill する。
  ;; RMS は picture-mode で kill-all-local-variables 関数を呼ばないのは、バグ
  ;; ではない、と言っていた。
  ;;
  ;; picture-mode で SKK を使用し漢字入力をした場合に、BS で全角文字が消せない
  ;; のは、SKK の不具合ではなく、picture.el の問題 (move-to-column-force 関数
  ;; の中で使用している move-to-column で全角文字を無視したカラム数が与えられ
  ;; たときにカーソル移動ができないから) である。消したい文字にポイントを合わ
  ;; せ、C-c C-d で一文字ずつ消すしか方法はない。
  (when skk-mode
    (skk-kill-local-variables)))

(defun skk-kill-local-variables ()
  "SKK 関連のバッファローカル変数を無効にする。"
  (skk-mode -1)
  (let (v vstr)
    (dolist (lv (buffer-local-variables))
      (setq v (car lv)
            vstr (prin1-to-string v))
      (when (and (> (length vstr) 3)
                 (string= "skk-" (substring vstr 0 4)))
        (kill-local-variable v)))))

;;;; kana inputting functions
;;;###autoload
(defun skk-insert (&optional arg prog-list-number)
  "SKK の文字入力を行なう。"
  (interactive "p")
  (unless prog-list-number
    (setq prog-list-number current-prefix-arg))
  (barf-if-buffer-read-only)
  (skk-with-point-move
   (let ((ch last-command-event))
     (cond ((and skk-henkan-mode
                 (memq ch skk-special-midashi-char-list))
            (if (= skk-henkan-start-point (point))
                ;; `▽' に続く `>' では変換処理を開始しない
                (progn
                  (set 'last-command-event ?>)
                  (skk-kana-input arg))
              ;; 接頭辞・接尾辞の処理。
              (skk-process-prefix-or-suffix arg)))

           ;; start writing a midasi key.
           ((and (memq ch skk-set-henkan-point-key)
                 (or skk-okurigana
                     (not (skk-get-prefix skk-current-rule-tree))
                     (not (skk-select-branch skk-current-rule-tree ch))))
            ;; normal pattern
            ;; skk-set-henkan-point -> skk-kana-input.
            (setq skk-insert-keysequence (string ch))
            (skk-set-henkan-point arg))

           ;; start conversion.
           ((and skk-henkan-mode        ; ▽モード or ▼モード
                 (eq ch skk-start-henkan-char)) ; SPC
            (skk-start-henkan arg prog-list-number))

           ;; just input kana.
           ((not (eq skk-henkan-mode 'on)) ; not ▽モード
            (skk-kana-input arg))

           ;; for completion.
           ;; コンプリーション関連の関数は skk-rom-kana-base-rule-list の中に押
           ;; し込め、skk-kana-input の中から制御すべき。
           ;; 但し、TAB は self-insert-command ではないので、skk-j-mode-map の
           ;; キーマップで substitute-key-definition しても skk-insert にバイン
           ;; ドできない。skk-j-mode-map で 直接 "\t" を skk-insert にバインド
           ;; して、completion と skk-current-kuten/skk-current-touten をコント
           ;; ロールするコマンド名を skk-rom-kana-base-rule-list に書けば良いか
           ;; も。
           ;; でも、skk-comp と skk-current-kuten/skk-current-touten のコントロ
           ;; ールがハードコーディングされるのはまずいかも (skk-comp は使っても
           ;; skk-current-kuten/skk-current-touten は使わない、という人がいるか
           ;; も)。
           ((and (eq skk-henkan-mode 'on) ; ▽モード
                 (eq ch skk-try-completion-char))
            (skk-comp (or prog-list-number ; C-u TAB で補完キーを初期化
                          (not (eq last-command 'skk-comp-do)))))

           ;;
           ((and (eq skk-henkan-mode 'on) ; ▽モード
                 (memq ch (list skk-next-completion-char
                                skk-previous-completion-char))
                 (eq last-command 'skk-comp-do))
            (skk-comp-previous/next ch))

           ;; just input Kana.
           (t
            (setq skk-insert-keysequence (concat skk-insert-keysequence (string ch)))
            (skk-kana-input arg)))
     ;; verbose message
     (skk-henkan-on-message))))

(defun skk-process-prefix-or-suffix (&optional arg)
  "接頭辞または接尾辞の入力を開始する。
これは、普通 `skk-special-midashi-char-list' に指定された文字の入力があった場
合に非対話的に呼び出されるが、対話的に呼出すことも可能である。"
  ;; SKK 10 までは、> < ? の 3 つについて扱いが平等でなかった。Daredevil SKK
  ;; 11 以降では、辞書における表現を > で統一することにより、3 者の扱いを平等
  ;; にし、なおかつ、このコマンドが文字キーでない入力により呼ばれたときにも接
  ;; 尾辞・ 接頭辞入力ができるようにする。
  (interactive "*p")
  (cl-case skk-henkan-mode
    (active
     ;; 接尾辞のための処理
     (skk-kakutei)
     (let (skk-kakutei-history)
       (skk-set-henkan-point-subr))
     (insert-and-inherit ?>))
    (on
     ;; 接頭辞の処理
     (skk-kana-cleanup 'force)
     (insert-and-inherit ?>)
     (skk-set-marker skk-henkan-end-point (point))
     (setq skk-henkan-count 0)
     (setq skk-henkan-key (buffer-substring-no-properties
                           skk-henkan-start-point (point))
           skk-prefix "")
     (setq skk-after-prefix t)
     (skk-henkan))
    (t
     (if last-command-event
         ;; `skk-insert' から呼ばれる場合には、このケースはない。
         (let ((i (prefix-numeric-value arg))
               (str (skk-char-to-unibyte-string last-command-event)))
           (while (> i 0)
             (skk-insert-str str)
             (setq i (1- i))))
       nil))))

(defun skk-kana-input (&optional arg)
  "かな文字の入力を行うルーチン。"
  ;; Message-Id: <19980610190611B.sakurada@kuis.kyoto-u.ac.jp>
  ;; From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
  ;; Date: Wed, 10 Jun 1998 19:06:11 +0900 (JST)
  ;;
  ;; 新しい skk-kana-input は, 簡単に説明すると,
  ;; あらかじめルールを木の形に表現しておいて, 入力を見
  ;; て木を辿り, それ以上辿れなくなったらその節に登録さ
  ;; れている仮名を入力する. というしくみです.
  ;;
  ;; 例えば, n a t のみからなる以下のルール
  ;;
  ;;     a  → あ
  ;;     n  → ん
  ;;     nn → ん
  ;;     na → な
  ;;     ta → た
  ;;     tt → っ (次状態 t)
  ;;
  ;; をルール木に変換すると,
  ;;
  ;;                 ／/＼
  ;;               ／ /   ＼
  ;;           a ／  / t    ＼ n
  ;;           ／   /         ＼
  ;;          あ   ・           ん
  ;;             ／|           / ＼
  ;;         a ／  | t      a /    ＼ n
  ;;         ／    |         /       ＼
  ;;       た     っ        な        ん
  ;;          (次状態 "t")
  ;;
  ;; という形になります.
  ;;
  ;; 初期状態(木の根)で `a' を入力すると, 木の根から
  ;; 「あ」に移動します. 次にどのような入力が来ても,
  ;; それより下に辿れないので, 「あ」を出力して根に戻ります.
  ;; ルールに次状態が設定されている場合は, 設定されてい
  ;; る文字列をキューに戻してから根に戻ります.
  ;;
  ;; 初期状態で `n' を入力すると, 「ん」に移動します.
  ;; 次に `a' または `n' が入力されればそれより下に辿れる
  ;; ので次の入力を見るまでまだ出力しません.
  ;; 次に `t' が入力された場合は, `t' では下に辿れないので,
  ;; 「ん」を出力して `t' をキューに戻します.
  ;;
  ;; ここで, 初期状態, 現状態をそれぞれ skk-rule-tree,
  ;; skk-current-rule-tree で表し.
  ;; 木を下に辿る, という操作は, skk-select-branch を
  ;; 用いて,
  ;;
  ;;   (skk-select-branch rule-tree ?a)
  ;;
  ;; のようにします. 現状態に設定されているかな(("ア". "あ")など),
  ;; 次状態("t" など)は, それぞれ skk-get-kana,
  ;; skk-get-nextstate で取ります.
  ;; don't echo key strokes in the minibuffer.
  (let ((echo-keystrokes 0)
        (queue (list last-command-event)))
    (while queue
      (if (not (skk-get-prefix skk-current-rule-tree))
          (progn
            (skk-set-marker skk-kana-start-point (point))
            (setq skk-current-rule-tree skk-rule-tree))
        (skk-erase-prefix))
      (setq skk-prefix (concat (skk-get-prefix skk-current-rule-tree)
                               (skk-char-to-unibyte-string last-command-event)))
      (let ((next (skk-select-branch skk-current-rule-tree (car queue)))
            data)
        (cond
         (next
          ;; can go down SKK-CURRENT-RULE-TREE
          (cond
           ((skk-get-branch-list next)
            ;; NEXT have at least one branch
            (when (and (eq skk-henkan-mode 'active)
                       skk-kakutei-early
                       (not skk-process-okuri-early))
              (skk-kakutei)
              (skk-set-marker skk-kana-start-point (point)))
            (setq queue (cdr queue)
                  skk-current-rule-tree next))
           (t
            ;; NEXT does not have any branch (i.e. NEXT is a leaf)
            (setq data (skk-get-kana next)
                  queue (nconc (string-to-list (skk-get-nextstate next))
                               (cdr queue))
                  skk-current-rule-tree nil))))
         (t
          ;; can not go down SKK-CURRENT-RULE-TREE
          (let ((d (skk-get-kana skk-current-rule-tree)))
            (cond
             (d
              ;; SKK-CURRENT-RULE-TREE have a roma->kana rule
              (setq data d
                    queue (nconc (string-to-list
                                  (skk-get-nextstate skk-current-rule-tree))
                                 queue)
                    skk-current-rule-tree nil))
             (t
              ;; SKK-CURRENT-RULE-TREE does not have any roma->kana rule
              (let ((dd (when skk-kana-input-search-function
                          (funcall skk-kana-input-search-function))))
                (cond
                 (dd
                  (setq data (car dd)
                        queue (nconc (string-to-list (cdr dd))
                                     (cdr queue))
                        skk-current-rule-tree nil))
                 ((eq skk-current-rule-tree skk-rule-tree)
                  ;; typo on the root of tree
                  (setq queue nil
                        skk-current-rule-tree nil))
                 (t
                  ;; otherwise move to root of the tree, and redo
                  (setq skk-current-rule-tree nil)))))))))
        (cond
         ((not data)
          (if skk-current-rule-tree
              (progn
                ;;(digit-argument arg)
                ;; う〜ん、よう分からん。とりあえず。
                (unless skk-isearch-message
                  (setq prefix-arg arg))
                (setq skk-prefix (skk-get-prefix skk-current-rule-tree))
                (skk-insert-prefix skk-prefix))
            ;;(skk-kana-cleanup 'force)
            (when (eq skk-henkan-mode 'active)
              (skk-kakutei))
            (setq skk-prefix "")
            (unless (or queue
                        (and (not (eq this-command 'skk-insert))
                             skk-henkan-mode))
              (skk-emulate-original-map (skk-make-raw-arg arg)))))
         (t
          ;;(skk-cancel-undo-boundary)
          (setq skk-prefix "")
          (when (functionp data)
            (setq data (funcall data (skk-make-raw-arg arg))))
          (when (stringp (if (consp data)
                             (car data)
                           data))
            (let* ((str (if (consp data)
                            (if skk-katakana
                                (car data)
                              (cdr data))
                          data))
                   (pair (when skk-auto-insert-paren
                           (cdr (assoc str skk-auto-paren-string-alist))))
                   (count0 arg)
                   (count1 arg)
                   (inserted 0)
                   region-str)
              (when (and (eq skk-henkan-mode 'active) ;▼モード
                         skk-kakutei-early
                         (not skk-process-okuri-early))
                (skk-kakutei))
              ;; arg は保存しておかないと、0 になってしまい、queue が
              ;; たまっていて再度ここへやって来たときに、文字入力が
              ;; できなくなる。
              (skk-cancel-undo-boundary)
              (when (and (use-region-p)
                         skk-use-auto-enclose-pair-of-region
                         pair)
                (setq region-str (buffer-substring (region-beginning)
                                                   (region-end)))
                (delete-region (region-beginning) (region-end)))
              (while (> count0 0)
                (skk-insert-str str)
                (setq count0 (1- count0)))
              (when pair
                (when region-str
                  (insert region-str))
                (while (> count1 0)
                  (unless (string= pair
                                   (skk-char-to-unibyte-string (following-char)))
                    (setq inserted (1+ inserted))
                    (skk-insert-str pair))
                  (setq count1 (1- count1)))
                (unless (zerop inserted)
                  (backward-char inserted))
                (when region-str
                  (skip-chars-forward pair)))
              (when (and skk-okurigana
                         (null queue))
                (skk-set-okurigana)))))))
      ;; XXX I don't know how skk-isearch-message works....
      (when skk-isearch-message
        (skk-isearch-message)))))

;;; tree procedure (ツリーにアクセスするためのインターフェース)
(defun skk-search-tree (tree char-list)
  "TREE の根から先端へ CHAR-LIST に従って辿る。
成功した場合は nil と 結果の木の組を返し、失敗した場合は辿れなかった
CHAR-LIST の残りと辿れなくなった節点の木の組を返す。"
  (catch 'return
    (let (next char rest)
      (while char-list
        (setq char (car char-list)
              rest (cdr char-list)
              next (skk-select-branch tree char))
        (if next
            (setq tree next
                  char-list rest)
          (throw 'return (cons char-list tree))))
      (cons nil tree))))

(defun skk-add-rule (tree rule)
  (let* ((prefix (nth 0 rule))
         (l (length prefix))
         (result (skk-search-tree tree (string-to-list prefix)))
         (rest (car result))
         (addpoint (cdr result)))
    (while rest
      (let ((addtree (skk-make-rule-tree
                      (car rest)
                      (if (vectorp prefix)
                          prefix
                        (substring prefix 0 (1+ (- l (length rest)))))
                      nil nil nil)))
        (skk-add-branch addpoint addtree)
        (setq addpoint addtree
              rest (cdr rest))))
    (skk-set-nextstate addpoint (nth 1 rule))
    (skk-set-kana addpoint (nth 2 rule))))

(defun skk-delete-rule (tree string)
  "入力 STRING に対するルールをルール木 TREE から削除する。"
  (catch 'return
    (let ((char-list (string-to-list string))
          (cutpoint tree)
          ;; TREE の根から出る枝が1本しかない場合
          ;; のために一応初期化しておく
          (cuttree (car (skk-get-branch-list tree)))
          next)
      (while char-list
        (setq next (skk-select-branch tree (car char-list))
              char-list (cdr char-list))
        (if next
            (if (> (length (skk-get-branch-list tree)) 1)
                (setq cutpoint tree ; 枝が2本以上の時 cutpoint cuttree
                      cuttree next  ; を update
                      tree next)
              (setq tree next))
          (throw 'return nil)))
      (skk-set-branch-list cutpoint
                           (delq cuttree (skk-get-branch-list cutpoint))))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>  := nil | (<tree> . <branch-list>)
;; <tree>         := (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         := (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         := <英小文字>
;; <nextstate>    := <英小文字文字列> | nil
;;;###autoload
(defun skk-compile-rule-list (&rest rule-lists)
  "rule list を木の形にコンパイルする。"
  (let ((tree (skk-make-rule-tree nil "" nil nil nil))
        key)
    (dolist (rule-list rule-lists)
      (dolist (rule rule-list)
        (setq key (car rule))
        (ignore-errors
          (when (symbolp key)
            (setq key (eval key))
            (when (stringp key)
              (setq rule (cons key (cdr rule)))))
          (unless (or (not (stringp key))
                      (string-match "\\w" key)
                      (eq (key-binding key) 'self-insert-command))
            (define-key skk-j-mode-map key 'skk-insert)))
        (when (stringp key)
          (skk-add-rule tree rule))))
    tree))

(defun skk-insert-str (str)
  "バッファに STR を挿入する。
必要であれば `skk-auto-start-henkan' をコールする。
必要であれば `self-insert-after-hook' をコールする。
`overwrite-mode' であれば、適切に上書きする。"
  (insert-and-inherit str)
  (if (eq skk-henkan-mode 'on)
      ;; ▽モード
      (when (and skk-auto-start-henkan
                 (not skk-okurigana))
        (skk-auto-start-henkan str))
    ;; ▽モード以外
    (when (and (boundp 'self-insert-after-hook)
               self-insert-after-hook)
      (funcall self-insert-after-hook
               (- (point) (length str))
               (point)))
    (when overwrite-mode
      (skk-del-char-with-pad (skk-ovwrt-len str))))
  ;; SKK 9.6 ではこのタイミングで fill が行われていたが、SKK 10 では行われてい
  ;; なかった。
  (when (and skk-j-mode
             (not skk-henkan-mode))
    (skk-do-auto-fill)))

(defun skk-ovwrt-len (str)
  "上書きして良い長さを返す。"
  (min (string-width (buffer-substring-no-properties (point)
                                                     (line-end-position)))
       (string-width str)))

(defun skk-del-char-with-pad (length)
  "長さ LENGTH の文字を消去する。
調整のため、必要であれば、末尾にスペースを挿入する。"
  (let ((p (point))
        (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (buffer-substring-no-properties (point) p))))
    (delete-region p (point))
    (unless (= length len)
      (insert-and-inherit " ")
      (backward-char 1))))

(defun skk-cancel-undo-boundary ()
  ;; skk-insert, skk-jisx0208-latin-insert で連続して入力さ
  ;; れた 20 文字を 1 回のアンドゥの対象とする。`20' は
  ;; keyboard.c に定められたマジックナンバー。Mule-2.3 添付
  ;; の egg.el を参考にした。
  (cond
   ((and (< skk-self-insert-non-undo-count 20)
         (memq last-command
               '(skk-insert
                 skk-jisx0208-latin-insert
                 skk-kakutei-henkan
                 ;; SKK abbrev モードでは、アスキー文字入力が Emacs オリジナ
                 ;; ルの self-insert-command により行われているので、
                 ;; skk-self-insert-non-undo-count をインクリメントすること
                 ;; ができないので、アンドゥをエミュレートできない。
                 ;; しかも、カンマやピリオドを挿入した時点で、
                 ;; skk-abbrev-comma や skk-abbrev-period を使うことになるの
                 ;; で (self-insert-command 以外のコマンドを使ってしまうので)、
                 ;; オリジナルのアンドゥの機能も損なってしまう。
                 ;; しかし現実問題としては、SKK abbrev モードは省略形としての
                 ;; 見出し語を挿入するためのモードであるので、長い見出し語を
                 ;; 挿入することはあまりなく、問題も小さいと考えられる。
                 ;;skk-abbrev-comma
                 ;;skk-abbrev-period
                 self-insert-command)))
    (cancel-undo-boundary)
    (when (null skk-current-rule-tree)
      ;; まだかな文字が完成していないときは、undo count をインクリメント
      ;; しない。
      (setq skk-self-insert-non-undo-count
            (1+ skk-self-insert-non-undo-count))))
   (t
    (setq skk-self-insert-non-undo-count 1))))

(defun skk-set-okurigana ()
  "見出し語から `skk-henkan-okurigana', `skk-henkan-key' の各値をセットする。"
  (cancel-undo-boundary)
  ;;(and skk-katakana (skk-hiragana-region skk-henkan-start-point (point)))
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
   (goto-char skk-okurigana-start-point)
   (unless (eq (following-char) ?*)
     (insert-and-inherit "*")))
  (setq skk-henkan-okurigana (buffer-substring-no-properties
                              (1+ skk-okurigana-start-point)
                              (point)))
  (setq skk-henkan-key (concat (buffer-substring-no-properties
                                skk-henkan-start-point
                                skk-henkan-end-point)
                               (or (skk-okurigana-prefix
                                    (if skk-katakana
                                        (skk-katakana-to-hiragana
                                         skk-henkan-okurigana)
                                      skk-henkan-okurigana))
                                   skk-okuri-char))
        skk-prefix "")
  (when skk-katakana
    (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)
          skk-henkan-okurigana
          (skk-katakana-to-hiragana skk-henkan-okurigana)))
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil))

(defun skk-set-char-before-as-okurigana (&optional no-sokuon)
  "ポイントの直前の文字を送り仮名と見倣して、変換を開始する。
ただし、 もうひとつ前の文字が促音だった場合には、 それ以降を送り仮名と見倣す。"
  (interactive)
  (let ((pt1 (point))
        pt2 okuri sokuon)
    (setq okuri (skk-save-point
                 (backward-char 1)
                 (buffer-substring-no-properties (setq pt2 (point))
                                                 pt1)))
    (when okuri
      (unless no-sokuon
        (setq sokuon (skk-save-point
                      (backward-char 2)
                      (buffer-substring-no-properties (point)
                                                      pt2)))
        (unless (member sokuon '("っ" "ッ"))
          (setq sokuon nil)))
      ;;
      (skk-save-point
       (backward-char (if sokuon 2 1))
       (skk-set-marker skk-okurigana-start-point (point)))
      (setq skk-okuri-char (skk-okurigana-prefix okuri))
      (unless skk-current-search-prog-list
        (setq skk-current-search-prog-list skk-search-prog-list))
      (skk-set-okurigana))))

;;; other inputting functions
(defun skk-toggle-kutouten ()
  "句読点の種類をトグルで変更する。"
  (interactive)
  (setq skk-kutouten-type (cl-case skk-kutouten-type
                            (jp 'en)
                            (en 'jp-en)
                            (jp-en 'en-jp)
                            (t 'jp)))
  (when (called-interactively-p 'interactive)
    (skk-message "読点: `%s'  句点: `%s'"
                 "Touten: `%s'  Kuten: `%s'"
                 (skk-current-touten nil)
                 (skk-current-kuten nil))))

(defun skk-current-kuten (arg)      ;句点(。)
  ;; just ignore arg.
  (if (symbolp skk-kutouten-type)
      (cadr (assq skk-kutouten-type skk-kuten-touten-alist))
    (car skk-kutouten-type)))

(defun skk-current-touten (arg)     ;読点(、)
  ;; just ignore arg.
  (if (symbolp skk-kutouten-type)
      (cdr (cdr (assq skk-kutouten-type skk-kuten-touten-alist)))
    (cdr skk-kutouten-type)))

(defun skk-auto-kutouten (arg)
  (let ((output (cdr (assq last-command-event
                           (list (list ?- "-" "−" "ー")
                                 (list ?, "," "，" (skk-current-touten nil))
                                 (list ?. "." "．" (skk-current-kuten nil))))))
        (chr (char-before (point))))
    (cond ((null chr)           ;point-min 対策
           (nth 2 output))
          ((and (<= ?0 chr) (>= ?9 chr) skk-use-auto-kutouten)
           (nth 0 output))
          ((and (<= ?０ chr) (>= ?９ chr) skk-use-auto-kutouten)
           (nth 1 output))
          (t
           (nth 2 output)))))

(defun skk-abbrev-insert (arg)
  (interactive "*p")
  (self-insert-command arg))

(defun skk-abbrev-period (arg)
  "SKK abbrev モードで見出し語の補完中であれば、次の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-comp-do)
       (progn
         (setq this-command 'skk-comp-do)
         (skk-comp-do nil))
     (skk-emulate-original-map arg))))

(defun skk-abbrev-comma (arg)
  "SKK abbrev モードで見出し語の補完中であれば、直前の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-comp-do)
       (progn
         (setq this-command 'skk-comp-do)
         (skk-comp-previous))
     (skk-emulate-original-map arg))))

(defun skk-jisx0208-latin-insert (arg)
  "全英文字をカレントバッファに挿入する。
`skk-jisx0208-latin-vector' をテーブルとして、最後に入力されたキーに対応する
文字を挿入する。
`skk-auto-insert-paren' の値が non-nil の場合で、`skk-auto-paren-string-alist'
に対応する文字列があるときは、その対応する文字列 (かっこ類) を自動的に挿入す
る。"
  (interactive "p")
  (barf-if-buffer-read-only)
  (skk-with-point-move
   (let* ((str (aref skk-jisx0208-latin-vector last-command-event))
          (arg2 arg)
          (pair-str (if skk-auto-insert-paren
                        (cdr (assoc str skk-auto-paren-string-alist))
                      nil))
          (pair-str-inserted 0)
          region-str)
     (if (not str)
         (skk-emulate-original-map arg)
       (skk-cancel-undo-boundary)
       (when (and (use-region-p)
                  skk-use-auto-enclose-pair-of-region
                  pair-str)
         (setq region-str (buffer-substring (region-beginning)
                                            (region-end)))
         (delete-region (region-beginning) (region-end)))
       (while (> arg 0)
         (skk-insert-str str)
         (setq arg (1- arg)))
       (when pair-str
         (when region-str
           (insert region-str))
         (while (> arg2 0)
           (unless (string= pair-str (char-to-string (following-char)))
             (setq pair-str-inserted (1+ pair-str-inserted))
             (skk-insert-str pair-str))
           (setq arg2 (1- arg2)))
         (unless (zerop pair-str-inserted)
           (backward-char pair-str-inserted))
         (when region-str
           (skip-chars-forward pair-str)))))))

(defun skk-delete-backward-char (arg)
  "▼モードで `skk-delete-implies-kakutei' なら直前の文字を消して確定する。
▼モードで `skk-delete-implies-kakutei' が nil だったら前候補を表示する。
▽モードで`▽'よりも前のポイントで実行すると確定する。
確定入力モードで、かなプレフィックスの入力中ならば、かなプレフィックスを消す。"
  (interactive "*P")
  (skk-with-point-move
   (let ((count (prefix-numeric-value arg)))
     (cond
      ((eq skk-henkan-mode 'active)
       (if (and (not skk-delete-implies-kakutei)
                (= (+ skk-henkan-end-point (length skk-henkan-okurigana))
                   (point)))
           (skk-previous-candidate)
         ;; overwrite-mode で、ポイントが全角文字に囲まれていると
         ;; きに delete-backward-char を使うと、全角文字は消すが半
         ;; 角文字分しか backward 方向にポイントが戻らない (Emacs
         ;; 19.31 にて確認)。変換中の候補に対しては
         ;; delete-backward-char で必ず全角文字 1 文字分 backward
         ;; 方向に戻った方が良い。
         (if overwrite-mode
             (progn
               (backward-char count)
               (delete-char count arg))
           (skk-emulate-original-map arg))
         ;; XXX assume skk-prefix has no multibyte chars.
         (if (> (length skk-prefix) count)
             (setq skk-prefix (substring skk-prefix
                                         0 (- (length skk-prefix) count)))
           (setq skk-prefix ""))
         (when (>= skk-henkan-end-point (point))
           (if (eq skk-delete-implies-kakutei 'dont-update)
               (let ((skk-update-jisyo-function #'ignore))
                 (skk-kakutei))
             (skk-kakutei)))))
      ((and skk-henkan-mode
            (>= skk-henkan-start-point (point))
            (not (skk-get-prefix skk-current-rule-tree)))
       (setq skk-henkan-count 0)
       (skk-kakutei))
      ;; 入力中の見出し語に対しては delete-backward-char で
      ;; 必ず全角文字 1文字分 backward 方向に戻った方が良い。
      ((and skk-henkan-mode overwrite-mode)
       (backward-char count)
       (delete-char count arg))
      (t
       (skk-delete-okuri-mark)
       (if (skk-get-prefix skk-current-rule-tree)
           (skk-erase-prefix 'clean)
         (skk-set-marker skk-kana-start-point nil)
         (skk-emulate-original-map arg)))))))

;;; henkan routines
(defun skk-henkan (&optional prog-list-number)
  "カナを漢字変換するメインルーチン。"
  (let (mark prototype new-word pair kakutei-henkan)
    (if (string= skk-henkan-key "")
        (skk-kakutei)
      ;; we use mark to go back to the correct position after henkan
      (unless (eobp)
        (setq mark (skk-save-point (forward-char 1)
                                   (point-marker))))

      (unless (eq skk-henkan-mode 'active) ;▼モード以外なら。つまり一発目を含む
        (skk-change-marker)
        (setq skk-current-search-prog-list
              (cond ((and (integerp prog-list-number)
                          (<= 0 prog-list-number)
                          (<= prog-list-number 9))
                     (or (symbol-value (intern (format "skk-search-prog-list-%d"
                                                       prog-list-number)))
                         skk-search-prog-list))
                    (t
                     skk-search-prog-list))))

      ;; skk-henkan-1 の中からコールされる skk-henkan-show-candidates
      ;; から throw される。ここでキャッチした場合は、?x がストリームに
      ;; 戻されているので、この関数を出て、skk-previous-candidate へゆく。
      (catch 'unread
        (cond ((setq prototype (skk-henkan-1))
               (setq new-word prototype))
              ((setq prototype (skk-henkan-in-minibuff))
               (setq new-word (skk-quote-semicolon prototype))))
        (setq kakutei-henkan skk-kakutei-flag)
        (when new-word
          (setq pair (skk-insert-new-word new-word))))

      (skk-delete-overlay skk-inline-overlays)
      (if mark
          (progn
            (goto-char mark)
            ;; 参照されていないマーカーは、Garbage Collection がコールされたと
            ;; きに回収されるが、それまでの間、テキストのどこかを指していると、
            ;; テキストのアップデートの際にそのマーカー値を更新する必要がある
            ;; ので、どこも指さないようにする。
            (skk-set-marker mark nil)
            (backward-char 1))
        (goto-char (point-max)))

      (when (and skk-show-annotation
                 (not kakutei-henkan))
        (skk-annotation-find-and-show pair)))))

(defun skk-exit-henkan-in-minibuff ()
  (when (and (numberp skk-henkan-in-minibuff-nest-level)
             (= (1- (minibuffer-depth)) skk-henkan-in-minibuff-nest-level))
    (setq skk-henkan-in-minibuff-nest-level nil)))

(defun skk-henkan-1 ()
  "`skk-henkan' のサブルーチン。"
  (let (new-word)
    (cl-case skk-henkan-count
      ;; 一発目の SPC 打鍵（つまり▽→SPC→▼のとき）
      (0
       (let ((prog-list-length (when (numberp skk-kakutei-search-prog-limit)
                                 (length skk-current-search-prog-list))))
         (while (and skk-current-search-prog-list
                     (not new-word))
           (setq skk-henkan-list (skk-nunion skk-henkan-list
                                             (skk-search)))
           (skk-henkan-list-filter)
           (setq new-word (skk-get-current-candidate)))
         ;; 変換候補が一つしか無い時の確定変換用チェック
         (when (and (or (eq skk-kakutei-when-unique-candidate t)
                        (cond (skk-abbrev-mode
                               (and (listp skk-kakutei-when-unique-candidate)
                                    (memq 'abbrev
                                          skk-kakutei-when-unique-candidate)))
                              (skk-henkan-okurigana
                               (and (listp skk-kakutei-when-unique-candidate)
                                    (memq 'okuri-ari
                                          skk-kakutei-when-unique-candidate)))
                              (t
                               ;; 送り無しのみを特別扱いしていた古い仕様に対応
                               (or (eq 'okuri-nasi
                                       skk-kakutei-when-unique-candidate)
                                   (memq 'okuri-nasi
                                         skk-kakutei-when-unique-candidate)))))
                    (= (length skk-henkan-list) 1)
                    (not skk-undo-kakutei-flag))
           (while (and skk-current-search-prog-list
                       (or (not (numberp skk-kakutei-search-prog-limit))
                           (< (- prog-list-length
                                 (length skk-current-search-prog-list))
                              skk-kakutei-search-prog-limit))
                       (<= (length skk-henkan-list) 1))
             (setq skk-henkan-list (skk-nunion skk-henkan-list
                                               (skk-search)))
             (skk-henkan-list-filter))
           (when (and (= (length skk-henkan-list) 1)
                      (or (not (numberp skk-kakutei-search-prog-limit))
                          (<= (- prog-list-length
                                 (length skk-current-search-prog-list))
                              skk-kakutei-search-prog-limit)))
             (setq skk-kakutei-henkan-flag t)))
         ;; skk-henkan-list-filter を通した後は念の為に再取得
         (setq new-word (skk-get-current-candidate))
         (when (and new-word
                    (not skk-undo-kakutei-flag)
                    skk-kakutei-henkan-flag)
           ;; found the unique candidate in kakutei jisyo
           (setq this-command 'skk-kakutei-henkan
                 skk-kakutei-flag t))))

      ;; 二発目以降の SPC 打鍵（つまり▼で連続 SPC 打鍵）
      (t
       (setq new-word (skk-get-current-candidate))
       (unless new-word
         ;; 新しい候補を見つけるか、skk-current-search-prog-list が空にな
         ;; るまで skk-search を連続してコールする。
         (while (and skk-current-search-prog-list
                     (not new-word))
           (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search)))
           (skk-henkan-list-filter)
           (setq new-word (skk-get-current-candidate))))
       (when (and new-word
                  (> skk-henkan-count (- skk-show-candidates-nth-henkan-char 2) ))
         ;; show candidates in minibuffer
         (setq new-word (skk-henkan-show-candidates)))))
    new-word))

(defun skk-get-current-candidate (&optional noconv)
  (let ((candidate (skk-get-current-candidate-1)))
    (cond ((not (and (skk-numeric-p)
                     (consp candidate)))
           candidate)
          (noconv
           (car candidate))
          (t
           (cdr candidate)))))

(defun skk-henkan-list-filter ()
  (when (skk-numeric-p)
    (skk-num-uniq)
    (skk-num-multiple-convert))
  (when (and (featurep 'jisx0213)   ;Mule-UCS
             skk-jisx0213-prohibit)
    (skk-jisx0213-henkan-list-filter)))

(defun skk-henkan-show-candidates ()
  "変換した候補群をエコーエリアに表示する。"
  (skk-save-point
   (let* ((max-candidates skk-henkan-number-to-display-candidates)
          (candidate-keys ; 表示用のキーリスト
           (mapcar (lambda (c)
                     (when (or (memq c '(?\C-g skk-start-henkan-char))
                               (skk-key-binding-member
                                (skk-char-to-unibyte-string c)
                                '(skk-previous-candidate)))
                       (skk-error "`%s' に無効なキーが指定されています"
                                  "Illegal key in `%s'"
                                  "skk-henkan-show-candidates-keys"))
                     (skk-char-to-unibyte-string (upcase c)))
                   skk-henkan-show-candidates-keys))
          key-num-alist ; 候補選択用の連想リスト
          (key-num-alist1 ; key-num-alist を組み立てるための作業用連想リスト。
           ;; 逆さまにしておいて、表示する候補の数が少なかったら先
           ;; 頭から幾つか削る。
           (let (alist)
             (dotimes (i max-candidates alist)
               (push (cons (nth i skk-henkan-show-candidates-keys)
                           i)
                     alist))))
          (nth-henkan (- skk-show-candidates-nth-henkan-char 1))
          (loop 0)
          inhibit-quit
          (echo-keystrokes 0)
          henkan-list
          new-one
          reverse
          n)
     ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
     ;; skk-henkan-key に何故か Overlay がかかってしまう。
     (when skk-use-face
       (skk-henkan-face-off))
     (delete-region skk-henkan-start-point
                    skk-henkan-end-point)
     (while loop
       (cond
        (reverse
         (setq loop (1- loop)
               henkan-list (nthcdr (+ nth-henkan (* loop max-candidates))
                                   skk-henkan-list)
               reverse nil))

        (skk-exit-show-candidates
         ;; 候補が尽きてしまって、skk-henkan-show-candidates ->
         ;; skk-henkan-in-minibuff -> skk-henkan
         ;; -> skk-henkan-show-candidates の順で、再びこの関数が呼ばれ
         ;; たときは、ここで henkan-list と loop を計算する。
         (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
               loop (car skk-exit-show-candidates))
         (setq skk-exit-show-candidates nil))

        (t
         ;; skk-henkan-show-candidates-keys の最終のキーに対応する候補
         ;; が出てくるまでサーチを続ける。
         (while (and skk-current-search-prog-list
                     (null (nthcdr (+ nth-henkan max-candidates (* loop max-candidates))
                                   skk-henkan-list)))
           ;; 新規に候補が得られた時のみ skk-henkan-list-filter を呼ぶ。
           ;; skk-look や skk-server-completion-search を利用した郵便番号からの
           ;; 変換時などのように、数値を含み且つ大量の候補がある時の速度改善。
           (let ((cands (skk-search)))
             (when cands
               (setq skk-henkan-list
                     (skk-nunion skk-henkan-list
                                 cands))
               (skk-henkan-list-filter))))
         (setq henkan-list (nthcdr (+ nth-henkan (* loop max-candidates))
                                   skk-henkan-list))))
       (save-window-excursion
         (setq n (skk-henkan-show-candidate-subr candidate-keys henkan-list))
         (when (> n 0)
           (condition-case nil
               (let* ((event (read-event))
                      (char (event-to-character event))
                      (key (skk-event-key event))
                      num)
                 ;; clear out candidates in echo area
                 (message "")
                 (if (and (null char)
                          (null key))
                     (skk-unread-event event)
                   (setq key-num-alist (nthcdr (- max-candidates n)
                                               key-num-alist1))
                   (when (and key-num-alist
                              char)
                     (setq num (cdr (or (assq char
                                              key-num-alist)
                                        (assq (if (skk-lower-case-p char)
                                                  (upcase char)
                                                (downcase char))
                                              key-num-alist)))))
                   (cond
                    (num
                     (setq skk-henkan-count (+ nth-henkan (* loop max-candidates) num))
                     (setq new-one (nth num henkan-list)
                           skk-kakutei-flag t
                           loop nil))

                    ((or (eq char skk-start-henkan-char) ; SPC
                         (skk-key-binding-member key
                                                 '(skk-nicola-self-insert-rshift)
                                                 skk-j-mode-map))
                     ;;
                     (if (or skk-current-search-prog-list
                             (nthcdr max-candidates henkan-list))
                         (setq loop (1+ loop))
                       ;; 候補が尽きた。この関数から抜ける。
                       (let ((last-showed-index (+ nth-henkan (* loop max-candidates))))
                         (setq skk-exit-show-candidates
                               ;; cdr 部は、辞書登録に入る前に最後に表示し
                               ;; た候補群の中で最初の候補を指すインデクス
                               (cons loop last-showed-index))
                         ;; 辞書登録に入る。skk-henkan-count は
                         ;; skk-henkan-list の最後の候補の次 (存在しない
                         ;; --- nil)を指す。
                         (setq skk-henkan-count (+ last-showed-index n))
                         (setq loop nil))))

                    ((eq char skk-force-registration-mode-char)
                     (let ((last-showed-index (+ nth-henkan (* loop max-candidates))))
                       (setq skk-exit-show-candidates
                             ;; cdr 部は、辞書登録に入る前に最後に表示し
                             ;; た候補群の中で最初の候補を指すインデクス
                             (cons loop last-showed-index))
                       (setq skk-henkan-count last-showed-index)
                       (setq loop nil)))

                    ((eq char skk-show-candidates-toggle-display-place-char)
                     (setq skk-show-candidates-always-pop-to-buffer
                           (not skk-show-candidates-always-pop-to-buffer)))

                    ((skk-key-binding-member key
                                             '(skk-previous-candidate
                                               skk-delete-backward-char
                                               skk-undo)
                                             skk-j-mode-map)
                     (cl-case loop
                       (0
                        ;; skk-henkan-show-candidates を呼ぶ前の
                        ;; 状態に戻す。
                        (skk-escape-from-show-candidates nth-henkan))
                       (t
                        ;; 一つ前の候補群をエコーエリアに表示する。
                        (setq reverse t))))

                    ((eq char skk-annotation-toggle-display-char)
                     (skk-annotation-toggle-display-p))

                    ((skk-key-binding-member key skk-quit-commands
                                             skk-j-mode-map)
                     ;;
                     (signal 'quit nil))

                    (t
                     (skk-message "`%s' は無効なキーです！"
                                  "`%s' is not valid here!"
                                  (or (key-description key)
                                      (key-description char)))
                     (sit-for 1)))))
             (quit
              ;; skk-previous-candidate へ
              (skk-escape-from-show-candidates 0)))))) ; end of while loop
     ;;
     (or (cdr-safe new-one)
         new-one))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  "候補群を表示する関数。
KEYS と CANDIDATES を組み合わせて７の倍数個の候補群 (候補数が
満たなかったらそこで打ち切る) の文字列を作り、インライン、ツールティップ、
エコーエリア又は候補バッファに表示する。"
  (let* ((max-candidates skk-henkan-number-to-display-candidates)
         (workinglst (skk-henkan-candidate-list candidates max-candidates))
         str tooltip-str message-log-max)
    (when workinglst
      (dotimes (i (length workinglst))
        (let ((cand (if (consp (nth i workinglst))
                        (cdr (nth i workinglst))
                      (nth i workinglst)))
              (key (concat (propertize (nth i keys) 'face
                                       'skk-henkan-show-candidates-keys-face)
                           ":")))
          (when (and (zerop (% i skk-henkan-number-to-display-candidates)) ; 各列の最初の候補
                     (not (zerop i)))
            (setq str (concat str "\n")))
          (setq str (concat str
                            (if (zerop (% i skk-henkan-number-to-display-candidates)) "" "  ")
                            key cand)
                tooltip-str (concat tooltip-str key cand "\n"))))
      (setq str (concat str (propertize
                             (format "  [残り %d%s]"
                                     (- (length candidates)
                                        (length workinglst))
                                     (make-string
                                      (length skk-current-search-prog-list)
                                      ?+))
                             'face 'skk-henkan-rest-indicator-face))
            tooltip-str (concat tooltip-str
                                (format "[残り %d%s]"
                                        (- (length candidates)
                                           (length workinglst))
                                        (make-string
                                         (length skk-current-search-prog-list)
                                         ?+))))
      (cond
       ;; (1) 現在のバッファの中に表示する (インライン表示)
       ((and skk-show-inline
             (not skk-isearch-switch)
             (not (skk-in-minibuffer-p)))
        (skk-inline-show str skk-inline-show-face tooltip-str max-candidates))

       ;; (2) tooptip で表示する
       ((and window-system
             skk-show-tooltip
             (not (eq (symbol-function 'skk-tooltip-show-at-point) 'ignore)))
        (when (and skk-henkan-rest-indicator)
          (let* ((body (substring tooltip-str 0 (string-match "\\[残り" tooltip-str)))
                 (rest (substring tooltip-str (- (length body) (length tooltip-str)))))
            (setq tooltip-str (concat body
                                      (make-string (- (car (skk-tooltip-max-tooltip-size))
                                                      (string-width rest) 3)
                                                   ? )
                                      rest))))
        (funcall skk-tooltip-function tooltip-str))

       ;; (3) エコーエリアを使う
       ((and (not skk-show-candidates-always-pop-to-buffer)
             (> (frame-width) (skk-max-string-width (split-string str "\n"))))
        (when skk-henkan-rest-indicator
          (let* ((body (substring str 0 (string-match "  \\[残り" str)))
                 (rest (substring str (- (length body) (length str)))))
            (setq str (concat body
                              (make-string (- (frame-width)
                                              (string-width str) 1) ? )
                              rest))))
        (message "%s" str))

       ;; (4) 一時バッファを pop up して使う
       (t
        (skk-henkan-show-candidates-buffer str keys))))
    ;; 表示する候補数を返す。
    (length workinglst)))

(defun skk-check-treat-candidate-appearance-function ()
  (when (or (eq skk-annotation-lookup-lookup 'always)
            (eq skk-annotation-lookup-DictionaryServices 'always))
    ;; Apple OS X の辞書サービスを候補一覧でも使う場合は、
    ;; `skk-treat-candidate-appearance-function' を調節する必要あり
    (setq skk-treat-candidate-appearance-function
          'skk-treat-candidate-sample2))
  ;;
  (functionp skk-treat-candidate-appearance-function))

(defun skk-treat-candidate-appearance (candidate listing-p)
  (funcall skk-treat-candidate-appearance-function candidate listing-p))

(defun skk-henkan-candidate-list (candidates max)
  ;; CANDIDATES の先頭の max 個のみのリストを返す。
  (let ((count 0) e sep note v)
    (while (> max count)
      (setq e (nth count candidates))
      (setq sep  nil
            note nil)
      (when (and (skk-numeric-p) (consp e))
        (setq e (cdr e)))
      (cond
       (e
        ;; まだ候補が残っている場合
        (when (skk-check-treat-candidate-appearance-function)
          ;; skk-treat-candidate-appearance-function によってユーザは
          ;; 任意に候補文字列と注釈文字列を加工・修飾することができる。
          ;; ユーザが返す値は cons cell または文字列となる。
          (let ((value (save-match-data
                         ;; 候補一覧表示の際は
                         ;; skk-treat-candidate-appearance-function の
                         ;; 第 2 引数を non-nil とする。
                         (skk-treat-candidate-appearance e 'list))))
            (cond
             ((consp value)
              ;; 返り値が cons cell だった場合
              (setq e (car value))
              (cond
               ((consp (cdr value))
                ;; (候補 . (セパレータ . 注釈))
                ;; 注釈は既にセパレータ抜き
                (setq sep  (cadr value)
                      note (cddr value)))
               ((string-match "^;" (cdr value))
                ;; (候補 . 注釈)
                ;; 注釈はまだセパレータを含んでいる
                (setq sep  (substring (cdr value) 0 1)
                      note (substring (cdr value) 1)))
               (t
                ;; (候補 . 注釈)
                ;; 注釈は既にセパレータを除去しているものと
                ;; 判断する
                (setq sep  ";"
                      note (cdr value)))))
             (t
              ;; 返り値が文字列だった場合
              (setq e    value
                    note nil)))))
        ;; ユーザが注釈表示を加工済みの場合はもう注釈の処理はしない。
        (when (and (not (stringp note))
                   (string-match ";" e))
          (setq sep  (substring e (match-beginning 0) (match-end 0))
                note (substring e (match-end 0))
                e    (substring e 0 (match-beginning 0))))
        ;; ユーザが望む注釈の表示形式に沿って注釈を加工する。
        (cond ((not skk-show-annotation)
               ;; 「表示しない」
               (setq sep  nil
                     note nil))
              ((skk-annotation-display-p 'list)
               ;; 「表示する」
               ;; そのまま
               )
              (t
               ;; 「候補一覧では表示しない」
               ;; annotation の存在だけを知らせる。
               (setq note nil)))
        ;; 候補一覧表示では候補と注釈を一括して表示するので
        ;; ここで結合しておく。
        (setq e (concat (skk-eval-string e)
                        sep
                        (when note
                          (skk-eval-string (skk-annotation-get note)))))
        ;; 全ての加工処理終わり。変数にセットする。
        (setq v     (cons e v)
              count (1+ count)))
       (t
        ;; 候補が尽きた場合
        (setq count max))))
    ;; 候補を出現順にソートし直して返す。
    (nreverse v)))

(defun skk-henkan-show-candidates-buffer (str keys)
  ;; エコーエリアの代わりに一時バッファを pop up して候補一覧を表示する。
  (let ((buff (get-buffer-create "*候補*"))
        (case-fold-search t))
    (with-current-buffer buff
      (erase-buffer)
      (insert (if skk-henkan-show-candidates-buffer-decor
                  (funcall skk-henkan-show-candidates-buffer-decor str)
                str))
      (goto-char (point-min))
      ;; 1 候補に 1 行を割り当てる。
      (forward-char 2)
      (while (re-search-forward
              (concat "  "
                      (mapconcat 'identity keys ":\\|  ")
                      ":\\|"
                      "  \\[残り [0-9]+\\(\\++\\)?\\]") nil t)
        (goto-char (match-beginning 0))
        (delete-char 2)
        (insert "\n"))
      (goto-char (point-min))

      ;; 右端に余白を設ける
      (while (and (move-to-column (- (frame-width) 2))
                  (not (eobp))
                  (>= (frame-width) (current-column)))
        (unless (eolp)
          ;; 禁則処理
          (unless (member (char-to-string (char-before))
                          skk-auto-start-henkan-keyword-list)
            (backward-char 1))
          (insert "\n  ")
          (forward-line -1))
        (forward-line 1))

      ;; [残り 99++] を右端へ
      (when skk-henkan-rest-indicator
        (let ((col (progn (goto-char (point-max))
                          (skk-screen-column))))
          (beginning-of-line)
          (insert-char 32 (- (frame-width) col 1))))

      ;;候補バッファの背景色
      (when skk-candidate-buffer-background-color
        (unless skk-candidate-buffer-background-color-odd
          (setq skk-candidate-buffer-background-color-odd
                skk-candidate-buffer-background-color))
        (goto-char (point-min))
        (let ((background-color skk-candidate-buffer-background-color))
          (while (null (eobp))
            (let* ((eol (save-excursion (end-of-line)
                                        (unless (eobp) (forward-char))
                                        (point)))
                   (bol (progn (beginning-of-line)
                               (point)))
                   (ovl (make-overlay bol eol)))
              (when (or (string-match "[ASDFJKL]:" (buffer-substring bol (+ 2 bol)))
                        (string-match "\\[残り [0-9]+\\(\\++\\)?\\]"
                                      (buffer-substring bol eol)))
                (setq background-color
                      (if (string= background-color skk-candidate-buffer-background-color)
                          skk-candidate-buffer-background-color-odd
                        skk-candidate-buffer-background-color)))
              (overlay-put ovl 'face `(:background ,background-color :extend t)))
            (forward-line))))

      (goto-char (point-min)))

    (let ((minibuf-p (skk-in-minibuffer-p))
          (window (get-buffer-window (skk-minibuffer-origin))))
      (when minibuf-p
        (if window
            (select-window window)
          (other-window 1)))
      (unless (eq (next-window) (selected-window))
        ;; *候補* バッファを見易くする。
        ;; `save-window-excursion' の中なので大丈夫なはず。
        (and skk-candidate-buffer-delete-other-windows
             (delete-other-windows)))
      (save-selected-window
        (pop-to-buffer buff)
        (let ((lines (count-lines (point-min) (point-max))))
          ;; window-height() includes mode-line
          (when (> lines (1- (window-height)))
            (enlarge-window (- lines (1- (window-height))))))
        (unless (pos-visible-in-window-p)
          (recenter '(1)))
        (fit-window-to-buffer)
        (apply 'set-window-fringes (if skk-candidate-buffer-display-fringes
                                       skk-candidate-buffer-fringe-width
                                     '(nil 0 0))))
      (when minibuf-p
        (select-window (minibuffer-window))))))

;;;###autoload
(defun skk-henkan-in-minibuff ()
  "辞書登録モードに入り、登録した単語の文字列を返す。"
  (unless (numberp skk-henkan-in-minibuff-nest-level)
    (setq skk-henkan-in-minibuff-nest-level (minibuffer-depth)))
  (when (and window-system skk-show-tooltip)
    (tooltip-hide))
  (when skk-show-inline
    (skk-inline-show "↓辞書登録中↓" 'skk-jisyo-registration-badge-face))
  (save-match-data
    (let ((enable-recursive-minibuffers t)
          (depth (- (1+ (minibuffer-depth)) skk-henkan-in-minibuff-nest-level))
          ;; XEmacs では次の変数が再帰的ミニバッファの可否に影響する。
          minibuffer-max-depth
          ;; From skk-henkan()
          ;; we use mark to go back to the correct position after henkan
          (mark (unless (eobp)
                  (skk-save-point
                   (forward-char 1)
                   (point-marker))))
          skk-isearch-message   ; 変換中に isearch message が出ないようにする
          orglen new-one pair)
      (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
      (add-hook 'minibuffer-setup-hook 'skk-add-skk-pre-command)
      (save-window-excursion
        (skk-show-num-type-info)
        (condition-case nil
            (setq new-one (read-from-minibuffer
                           (format "%s辞書登録%s %s: "
                                   (make-string depth ?\[)
                                   (make-string depth ?\])
                                   (or (and (skk-numeric-p)
                                            (skk-num-henkan-key))
                                       (if skk-okuri-char
                                           (skk-compute-henkan-key2)
                                         skk-henkan-key)))
                           (when (and (not skk-okuri-char)
                                      skk-read-from-minibuffer-function)
                             (funcall skk-read-from-minibuffer-function))))
          (quit
           (skk-delete-overlay skk-inline-overlays)
           (setq new-one ""))))
      (when (and skk-check-okurigana-on-touroku
                 ;; 送りあり変換でも skk-okuri-char だけだと判断できない。
                 skk-henkan-okurigana new-one)
        (setq new-one (skk-remove-redundant-okurigana new-one)))
      (cond
       ((string= new-one "")
        (skk-delete-overlay skk-inline-overlays)
        (if skk-exit-show-candidates
            ;; エコーエリアに表示した候補が尽きて辞書登録に入ったが、空文字
            ;; 列が登録された場合。最後にエコーエリアに表示した候補群を再表
            ;; 示する。
            (progn
              (setq skk-henkan-count (cdr skk-exit-show-candidates))
              (skk-henkan))
          ;; skk-henkan-show-candidates に入る前に候補が尽きた場合
          (setq skk-henkan-count (1- skk-henkan-count))
          (if (= skk-henkan-count -1)
              ;; 送りありの変換で辞書登録に入り、空文字を登録した後、その
              ;; まま再度送りなしとして変換した場合は
              ;; skk-henkan-okurigana, skk-okuri-char の値を nil にしなけ
              ;; れば、それぞれの値に古い送り仮名が入ったままで検索に失敗
              ;; する。
              (progn
                (setq skk-henkan-okurigana nil
                      skk-okurigana nil
                      skk-okuri-char nil)
                (skk-change-marker-to-white))
            ;; 辞書登録に入る直前の候補に注釈がある可能性を考え、再表示する。
            ;;   skk-insert-new-word(), skk-henkan-candidate-list() 内の
            ;;   注釈加工処理を独立した関数にして、
            ;;   それを利用するようにしたほうが良さそう。
            (setq pair (skk-insert-new-word (skk-get-current-candidate)))
            ;; From skk-henkan()
            ;; 送りあり変換の際の point の位置を、辞書登録モードに入る前の
            ;; 位置に戻す。
            (if mark
                (progn
                  (goto-char mark)
                  (skk-set-marker mark nil)
                  (backward-char 1))
              (goto-char (point-max)))
            ;;
            (when skk-show-annotation
              (skk-annotation-find-and-show pair)))))
       (t
        (when (string-match "[ 　]+$" new-one)
          (setq new-one (substring new-one 0 (match-beginning 0))))
        (setq skk-henkan-list (nconc skk-henkan-list
                                     (list new-one)))
        (when (skk-numeric-p)
          (setq orglen (length skk-henkan-list))
          (skk-num-convert skk-henkan-count)
          (setq new-one (cdr (skk-get-current-candidate-1))))
        (when (or (not orglen)
                  (= orglen (length skk-henkan-list)))
          (setq skk-kakutei-flag t))
        (setq skk-henkan-in-minibuff-flag t
              skk-touroku-count (1+ skk-touroku-count))))
      ;; (nth skk-henkan-count skk-henkan-list) が nil だから辞書登録に
      ;; 入っている。skk-henkan-count をインクリメントする必要はない。
      ;; new-one が空文字列だったら nil を返す。
      (unless (string= new-one "")
        (setq skk-jisyo-updated t)  ; skk-update-jisyo で参照
        new-one))))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana が non-nil なら skk-henkan-key から、かつて
  ;; skk-henkan-key2 と呼ばれていたものを作る。
  ;; skk-henkan-key2 とは、「漢字部分の読み + "*" + 送り仮名」の形式の文字列を
  ;; 言う。
  (when skk-henkan-okurigana
    (save-match-data
      (string-match "[a-z]+$" skk-henkan-key)
      (concat (substring skk-henkan-key 0 (match-beginning 0))
              "*"
              skk-henkan-okurigana))))

(defun skk-remove-redundant-okurigana (word)
  "辞書に登録される候補の持つ余計な送り仮名を取り除く。

送りありの登録をするとき、送り仮名を消してから [RET] を押さなければ正しく登録
できない。 そこで、ユーザが間違えて送り仮名を消し忘れていないかどうか、 SKK
の側でチェックできる範囲についてはユーザの確認を取る。

`skk-check-okurigana-on-touroku' を non-nil に設定している場合のみ有効。
auto に設定するとユーザに確認しない。
変換が行われたバッファで実行される。ミニバッファ、辞書バッファではない。"
  (save-match-data
    (let* ((len (length word))
           (str1 (when (< 0 len)
                   (substring word (1- len) len)))
           (str2 (when (< 1 len)
                   (substring word (- len 2) (1- len))))
           (str (if (and str2
                         (string-match "^[ぁ-ん]$" str2))
                    (concat str2 str1)
                  str1)))
      (when (and str
                 (string-match "^[ぁ-ん]$" str1)
                 (or (eq skk-check-okurigana-on-touroku 'auto)
                     (skk-y-or-n-p
                      (format "%s: `%s' を除いて登録しますか？ "
                              word str)
                      (format "%s: Remove `%s' when register? "
                              word str))))
        ;; ユーザの指示に従い送り仮名を取り除く。
        (message "")
        (setq word (substring word
                              0
                              (if (string-match "^[ぁ-ん]$" str2)
                                  (- len 2)
                                (1- len)))))))
  ;;
  word)

(defun skk-show-num-type-info ()
  "数値変換エントリの辞書登録時に変換タイプの案内を表示する。"
  (when (and skk-show-num-type-info
             skk-use-numeric-conversion
             (string-match "[0-9]" skk-henkan-key))
    (let ((buff (get-buffer-create " *数値変換タイプ*")))
      (with-current-buffer buff
        (erase-buffer)
        (insert "\
#0 無変換                 e.g. 12
#1 全角数字               e.g. １２
#2 漢数字で位取りあり     e.g. 五五〇〇
#3 漢数字で位取りなし     e.g. 五千五百
#4 数値再変換
#5 金額表記               e.g. 壱阡九百九拾伍
#9 将棋棋譜用"))
      ;; skk-henkan-show-candidates-buffer からひっぱってきたコード
      (let ((minibuf-p (skk-in-minibuffer-p))
            (window (get-buffer-window (skk-minibuffer-origin))))
        (when minibuf-p
          (if window
              (select-window window)
            (other-window 1)))
        (unless (eq (next-window) (selected-window))
          (delete-other-windows))
        (save-selected-window
          (pop-to-buffer buff)
          (fit-window-to-buffer)
          (unless (pos-visible-in-window-p)
            (recenter '(1))))
        (when minibuf-p
          (select-window (minibuffer-window)))))))

;;;###autoload
(defun skk-previous-candidate (&optional arg)
  "▼モードであれば、一つ前の候補を表示する。
▼モード以外ではカレントバッファにタイプした文字を挿入する。
確定辞書による確定の直後に呼ぶと確定をアンドゥし、見出しに対する次候補を表示する。
最後に確定したときの候補はスキップされる。"
  (interactive "*p")
  (skk-with-point-move
   (cond
    ((not (eq skk-henkan-mode 'active))
     (if (not (eq last-command 'skk-kakutei-henkan))
         (when (and last-command-event
                    (characterp last-command-event))
           (skk-kana-input arg))
       (skk-undo-kakutei-subr)))
    ((string= skk-henkan-key "")
     nil)
    (t
     (let ((mark (unless (eobp)
                   (skk-save-point
                    (forward-char 1)
                    (point-marker))))
           pair)
       (skk-save-point
        (cl-case skk-henkan-count
          (0
           (when skk-okuri-char
             ;; roman prefix for okurigana should be removed.
             (setq skk-henkan-key (substring skk-henkan-key 0 -1)))
           (when skk-katakana
             (setq skk-henkan-key
                   (skk-hiragana-to-katakana skk-henkan-key)))
           (setq skk-henkan-count -1)
           (setq skk-henkan-in-minibuff-flag nil
                 skk-henkan-list nil
                 skk-henkan-okurigana nil
                 skk-okuri-char nil
                 skk-okuri-index-min -1
                 skk-okuri-index-max -1
                 skk-okurigana nil
                 skk-prefix "")
           (when (skk-numeric-p)
             (skk-num-initialize))
           ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert され
           ;; る skk-henkan-key に何故か Overlay がかかってしまう。
           (when skk-use-face
             (skk-henkan-face-off))
           (delete-region skk-henkan-start-point skk-henkan-end-point)
           (goto-char skk-henkan-end-point)
           (insert-and-inherit skk-henkan-key)
           (skk-change-marker-to-white))
          (t
           (setq skk-henkan-count (1- skk-henkan-count))
           (setq pair (skk-insert-new-word (skk-get-current-candidate))))))
       ;;
       (if mark
           (progn
             (goto-char mark)
             (skk-set-marker mark nil)
             (backward-char 1))
         (goto-char (point-max)))
       ;;
       (when skk-show-annotation
         (skk-annotation-find-and-show pair))
       ;;
       (when (and skk-abbrev-mode
                  (= skk-henkan-count -1))
         (skk-abbrev-mode-on)))))
   ;; verbose message
   (skk-henkan-on-message)))

(defun skk-undo (&optional arg)
  "`undo' の機能を、SKK との整合性を考えて調節する。"
  (interactive "*P")
  (cond ((skk-get-prefix skk-current-rule-tree)
         (skk-kana-cleanup 'force))
        ((eq skk-henkan-mode 'active)
         (skk-previous-candidate))
        ((eq skk-henkan-mode 'on)
         (if (= (point)
                (marker-position skk-henkan-start-point))
             (skk-kakutei arg)
           (forward-char -1)
           (delete-char 1)))
        (t
         (skk-emulate-original-map arg))))

(defun skk-insert-new-word (word)
  "見出し語を消し、その場所へ変換結果の文字列を挿入する。"
  (save-match-data
    (let (retval note face)
      (while (and (setq retval (skk-treat-new-word (or retval word)))
                  (stringp retval))
        ;; nextword-or-retval に文字列が代入された場合 (`skk-ignore-dic-word'
        ;; によって前の候補がスキップされた場合) はその文字列を新しい候補として
        ;; 処理を繰り返す。文字列以外が代入されたらループを終了する。
        ())
      (when (consp retval)
        (setq word (car retval)
              note (cdr retval)))
      (unless (eq retval 'none)
        ;; 挿入すべき候補がある。
        (when skk-use-face
          (skk-henkan-face-off))
        (delete-region skk-henkan-start-point skk-henkan-end-point)
        (goto-char skk-henkan-start-point)
        ;; word を挿入する前にその face 属性を除去する。ただし，除去する前に
        ;; face 属性を変数に退避する。この face 属性は次の候補表示に使用され
        ;; る。
        (setq face (get-text-property 0 'face word))
        (set-text-properties 0 (length word) nil word)
        (insert-and-inherit word)
        (skk-set-marker skk-henkan-end-point (point))
        (when skk-use-face
          (skk-henkan-face-on face))
        (when skk-insert-new-word-function
          (funcall skk-insert-new-word-function))
        (when skk-kakutei-flag
          ;; `skk-ignore-dic-word' 内で辞書登録モードに入った場合。
          (skk-kakutei))
        (cons word note)))))

(defun skk-treat-new-word (word)
  "`skk-insert-new-word' のサブルーチン。挿入すべき単語を装飾する。
ループ内でよばれる。処理中に `skk-ignore-dic-word' が呼ばれた場合はそこから
catch する。catch した値が文字列ならば、それを返す (word をそれに置き換えて
再度呼び出される)。 catch しなかった場合は挿入すべき文字列が得られる。この場合
は注釈と文字列の cons cell を返す。"
  (let (note)
    (catch 'next-word
      ;; ループ 1 回目では next-word は nil。
      ;; `skk-ignore-dic-word' により現在の word が skip され、
      ;; 新しい語が返ってきた場合、next-word に文字列がセットされている。
      ;; この場合 word を next-word に置き換えて処理をやり直す。

      ;; 候補一覧で選択された候補を挿入する際は、
      ;; skk-kakutei-flag が t になっている。この場合は装飾
      ;; してもしかたない。
      (when (and (not skk-kakutei-flag)
                 (skk-check-treat-candidate-appearance-function))
        ;; skk-treat-candidate-appearance-function によって
        ;; ユーザは任意に候補文字列と注釈文字列を加工・修飾
        ;; することができる。
        ;; ユーザが返す値は cons cell または文字列となる。
        (save-match-data
          (let ((value (skk-treat-candidate-appearance word nil)))
            (if (consp value)
                ;; 返り値が cons cell だった場合
                (setq word (car value)
                      note (cond
                            ((consp (cdr value))
                             ;; (候補 . (セパレータ . 注釈))
                             ;; 注釈は既にセパレータ抜き
                             (cddr value))
                            ((string-match "^;" (cdr value))
                             ;; (候補 . 注釈)
                             ;; 注釈はまだセパレータを含んで
                             ;; いる
                             (substring (cdr value) (match-end 0)))
                            (t
                             ;; (候補 . 注釈)
                             ;; 注釈は既にセパレータを除去して
                             ;; いるものと判断する
                             (cdr value))))
              ;; 返り値が文字列だった場合
              (setq word value)))))
      ;; ユーザの意図によって注釈が既に設定されて
      ;; いる場合はSKK の方では対処しない。
      (when (and (not (stringp note))
                 (string-match ";" word))
        (setq note (substring word (match-end 0))
              word (substring word 0 (match-beginning 0))))
      ;; word の方が S 式の文字列だったら、それを評価した
      ;; 文字列を返す。
      ;; note の方も S 式の場合がありうるが、それの評価は
      ;; skk-annotation がやってくれる。
      (setq word (skk-eval-string word))
      (cons word note))))

;;;###autoload
(defun skk-treat-strip-note-from-word (word)
  "変換候補の文字列 WORD を、候補そのものと注釈とに分割して cons cell を返す。
候補そのものと注釈とのセパレータは \";\" である必要がある。
分割のルールは以下のとおり。

  \"word\" --> (\"word\" . nil)
  \"word;\" --> (\"word\" . \"\")
  \"word;note\" --> (\"word\" . \"note\")
"(when word
   (save-match-data
     (let (cand note)
       (if (string-match ";" word)
           (setq cand (substring word 0 (match-beginning 0))
                 note (substring word (match-end 0)))
         (setq cand word))
       (cons cand note)))))

;;;###autoload
(defun skk-kakutei (&optional arg word)
  "現在表示されている語で確定し、辞書を更新する。
カレントバッファで SKK モードになっていなかったら SKK モードに入る。
オプショナル引数の WORD を渡すと、現在表示されている候補とは無関係に
WORD で確定する。"
  ;; read only でエラーになるようにすると read only バッファで SKK が起動でき
  ;; なくなる。
  (interactive "P")
  (let ((inhibit-quit t)
        converted kakutei-word)
    (when skk-henkan-mode       ;'on or 'active
      (cl-case skk-henkan-mode
        (active             ;▼モード
         (setq kakutei-word
               ;; 確定辞書の語で確定したときは、辞書にその語を書き込む必要もな
               ;; いし、更新する必要もないと思っていたが、補完を行うときは、
               ;; 個人辞書を参照する (確定辞書は参照しない) ので、多少資源と時
               ;; 間を無駄にしても、個人辞書に確定辞書のエントリを書き込んで更
               ;; 新もしておく。
               (or word (skk-get-current-candidate 'noconv)))
         (when (and kakutei-word
                    (skk-update-jisyo-p kakutei-word))
           (skk-update-jisyo kakutei-word)
           ;; 接尾辞・接頭辞に関する処理
           (cond
            ((not skk-learn-combined-word)
             ;; ユーザが希望しない限り何の処理もしない。
             (setq skk-after-prefix nil))
            ((and skk-after-prefix
                  (not (string-match "^[^\000-\177]+>$" skk-henkan-key)))
             ;; このバッファにおいて、接頭辞に続く入力が進行中。
             (let* ((history (cdr skk-kakutei-history))
                    (list1 (car skk-kakutei-history)) ; (りよう 利用)
                    (list2 (catch 'list ; (さい> 再)
                             (while history
                               (if (eq (nth 2 list1) (nth 2 (car history)))
                                   ;; 同じバッファだったら
                                   (throw 'list (car history))
                                 (setq history (cdr history))))))
                    (list1-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list1))))
                    (list2-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list2))))
                    skk-henkan-key comb-word)
               (when (and (stringp list2-word)
                          (string-match "^[^\000-\177]+>$" (car list2))
                          (skk-save-point
                           (ignore-errors
                             (goto-char (- skk-henkan-start-point
                                           (length list1-word)))
                             (looking-at list2-word))))
                 (setq skk-henkan-key
                       (concat (substring (car list2)
                                          0
                                          (1- (length (car list2))))
                               (car list1)) ; さいりよう
                       comb-word (concat list2-word list1-word)) ; 再利用
                 (skk-update-jisyo comb-word))
               (setq skk-after-prefix nil)))
            ((and (stringp (caar skk-kakutei-history))
                  (string-match "^>[^\000-\177]+$" (caar skk-kakutei-history)))
             ;; 今回の確定が接尾辞だった場合、前回の確定と今回の接尾辞を
             ;; 合わせた語を辞書登録する。
             (let* ((history (cdr skk-kakutei-history))
                    (list1 (car skk-kakutei-history)) ; (>てき 的)
                    (list2 (catch 'list ; (かんどう 感動)
                             (while history
                               (if (eq (nth 2 list1) (nth 2 (car history)))
                                   ;; 同じバッファだったら
                                   (throw 'list (car history))
                                 (setq history (cdr history))))))
                    (list1-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list1))))
                    (list2-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list2))))
                    skk-henkan-key comb-word)
               (when (stringp list2-word)
                 (setq skk-henkan-key
                       (concat (car list2)
                               (substring (car list1) 1)) ; かんどうてき
                       comb-word (concat list2-word list1-word)) ; 感動的
                 (skk-update-jisyo comb-word)))))
           ;;
           (when (skk-numeric-p)
             (setq converted (skk-get-current-candidate))
             (skk-num-update-jisyo kakutei-word converted))))
        (t
         ;; ▽モードで確定した場合。便宜的に現在のポイントまでを見出し語を扱い
         ;; して履歴を更新する。
         (when (and (> skk-kakutei-history-limit 0)
                    (< skk-henkan-start-point (point))
                    (skk-save-point
                     (goto-char skk-henkan-start-point)
                     (eq (skk-what-char-type) 'hiragana)))
           (skk-update-kakutei-history
            (buffer-substring-no-properties
             skk-henkan-start-point (point))))))
      (when (and window-system skk-show-tooltip)
        (tooltip-hide))
      (when skk-mode
        (skk-kakutei-cleanup-buffer)
        (when (and skk-undo-kakutei-word-only
                   (consp buffer-undo-list))
          ;; ▽モードに移ってから現在までの undo 情報から先頭以外の
          ;; nil を削除する
          (setq buffer-undo-list
                (cons (car buffer-undo-list)
                      (nconc
                       (delq nil (cdr (butlast buffer-undo-list
                                               (length skk-last-buffer-undo-list))))
                       skk-last-buffer-undo-list))))
        ;; KAKUTEI-WORD などの情報が必要であれば、skk-last-henkan-data
        ;; から得られる。必要なデータがそれらの変数に限定されないので、
        ;; 引数にしない。
        (when skk-kakutei-end-function
          (funcall skk-kakutei-end-function))
        (skk-kakutei-initialize
         (if (skk-numeric-p)
             (cons kakutei-word converted)
           kakutei-word))))

    (skk-do-auto-fill)
    (when (and skk-undo-kakutei-return-previous-point
               (numberp skk-undo-kakutei-previous-point)
               (numberp skk-undo-kakutei-previous-length)
               (markerp skk-henkan-end-point)
               (markerp skk-henkan-start-point))
      (goto-char (+ skk-undo-kakutei-previous-point
                    (if (>= skk-undo-kakutei-previous-point
                            (1- (+ skk-henkan-start-point
                                   skk-undo-kakutei-previous-length)))
                        (- skk-henkan-end-point
                           skk-henkan-start-point
                           skk-undo-kakutei-previous-length)
                      0))))
    (setq skk-undo-kakutei-previous-point nil
          skk-undo-kakutei-previous-length nil)
    (cond
     ((not skk-mode)
      (skk-mode 1))
     (skk-undo-kakutei-prev-state
      (cond ((cdr (assq 'skk-latin-mode skk-undo-kakutei-prev-state))
             (skk-latin-mode-on))
            ((cdr (assq 'skk-jisx0208-latin-mode skk-undo-kakutei-prev-state))
             (skk-jisx0208-latin-mode-on))
            ;; skk-mode がオフの状態に戻そうとすると
            ;; `skk-mode-exit' 内で再度 `skk-kakutei' を呼ぶた
            ;; め無限ループになってしまう
            ;;      ((not (cdr (assq 'skk-mode skk-undo-kakutei-prev-state)))
            ;;       (skk-mode -1))
            ((cdr (assq 'skk-j-mode skk-undo-kakutei-prev-state))
             ;; M-x skk-undo-kakutei で skk-abbrev-mode に戻った際、
             ;; 確定後に skk-j-mode に戻るためには以下が必要。
             (skk-j-mode-on skk-katakana)))
      (setq skk-undo-kakutei-prev-state nil))
     ((not (or skk-j-mode skk-jisx0201-mode))
      (skk-j-mode-on skk-katakana))))
  nil)

(defun skk-update-jisyo-p (word)
  "WORD が個人辞書に登録されるべきか否かを判定する。
変数 `skk-search-excluding-word-pattern-function' が関数であれば、WORD を
引数にしてその関数を実行し、戻り値が non-nil であれば `skk-update-jisyo-p' は
 nil を返す。
変数 `skk-search-excluding-word-pattern-function' が関数のリストであれば、
 WORD を引数にしてそれぞれの関数を実行し、そのうちのひとつでも non-nil を
返せば `skk-update-jisyo-p' は nil を返す。

変数 `skk-search-excluding-word-pattern-function' の docstring も参照のこと。"
  (save-match-data
    (not (run-hook-with-args-until-success
          'skk-search-excluding-word-pattern-function word))))

(defun skk-kakutei-cleanup-buffer ()
  "確定直後のバッファを整形する。"
  (when skk-okurigana
    ;; 確定変換のときにここにくる。
    ;; 再変換の際に送り仮名関連フラグが clear されているとよろしくないので
    ;; `skk-delete-okuri-mark' の NOCLEAR をセットして呼ぶ。
    (skk-delete-okuri-mark t))
  (skk-delete-henkan-markers)
  (when (and (boundp 'self-insert-after-hook)
             self-insert-after-hook)
    (funcall self-insert-after-hook
             skk-henkan-start-point (point)))
  (when overwrite-mode
    (skk-del-char-with-pad (skk-ovwrt-len (buffer-substring-no-properties
                                           skk-henkan-start-point (point))))))

(defun skk-kakutei-initialize (&optional kakutei-word)
  "確定時に変数の初期化とアンドゥのための変数の保存を行う。"
  (when (and kakutei-word
             (or (consp kakutei-word)
                 (not (string= kakutei-word ""))))
    (setq skk-kakutei-count (1+ skk-kakutei-count))
    ;; skk-undo-kakutei のために最後の変換のデータを保存する。
    (skk-put-last-henkan-data
     (list (cons 'henkan-key skk-henkan-key)
           (cons 'okuri-char skk-okuri-char)
           (cons 'henkan-okurigana skk-henkan-okurigana)
           (cons 'henkan-list
                 ;; 確定した語を先頭にする。
                 (cons kakutei-word
                       (delete kakutei-word skk-henkan-list)))
           (cons 'henkan-buffer (current-buffer))
           (cons 'henkan-point
                 (let ((hpoint (skk-get-last-henkan-datum 'henkan-point)))
                   (if hpoint
                       (set-marker hpoint (point))
                     (point-marker))))
           (cons 'abbrev-mode skk-abbrev-mode)
           ;; (eq last-command 'skk-kakutei-henkan) でポータブルに確認でき
           ;; るのであえていらないか。
           ;; (cons 'kakutei-henkan (eq this-command 'skk-kakutei-henkan))
           ;; 上記以外の henkan data を skk-last-henkan-data に残したかったら、
           ;; skk-kakutei-end-function を利用する。
           )))
  (setq skk-henkan-count -1)
  (setq skk-exit-show-candidates nil)
  (setq skk-abbrev-mode nil
        skk-henkan-in-minibuff-flag nil
        skk-henkan-key nil
        skk-henkan-list nil
        skk-henkan-okurigana nil
        skk-henkan-mode nil
        skk-kakutei-flag nil
        skk-kakutei-henkan-flag nil
        skk-undo-kakutei-flag nil
        skk-okuri-char nil
        skk-okuri-index-min -1
        skk-okuri-index-max -1
        ;; skk-prefix ""
        ))

(defun skk-undo-kakutei ()
  "一番最後の確定をアンドゥし、見出し語に対する候補を表示する。
最後に確定したときの候補はスキップされる。
他に候補がないときは、エコーエリアでの辞書登録に入る。"
  (interactive)
  (let (jmsg emsg)
    (cond ((eq last-command 'skk-undo-kakutei)
           (setq jmsg "確定アンドゥは連続使用できません"
                 emsg "Cannot undo kakutei repeatedly"))
          ((eq skk-henkan-mode 'active)
           (setq jmsg "▼モードでは確定アンドゥできません"
                 emsg "Cannot undo kakutei in ▼ mode"))
          ((or (not (skk-get-last-henkan-datum 'henkan-key))
               (string= (skk-get-last-henkan-datum 'henkan-key) "")
               (null skk-henkan-end-point))
           ;; skk-henkan-key may be nil or "".
           (setq jmsg "アンドゥデータがありません"
                 emsg "Lost undo data")))
    (if jmsg
        (skk-message jmsg emsg)
      (skk-with-point-move
       (condition-case nil
           (skk-undo-kakutei-subr)
         ;; skk-undo-kakutei から途中で抜けた場合は、各種フラグを初期化して
         ;; おかないと次の動作をしようとしたときにエラーになる。
         ((error quit)
          (skk-kakutei)))))))

(defun skk-undo-kakutei-subr ()
  (let ((end (if (skk-get-last-henkan-datum 'henkan-okurigana)
                 (+ (length (skk-get-last-henkan-datum
                             'henkan-okurigana))
                    skk-henkan-end-point)
               skk-henkan-end-point)))
    (setq skk-henkan-mode 'active
          skk-current-search-prog-list
          (let ((prog-list-number current-prefix-arg))
            (cond
             ((and (integerp prog-list-number)
                   (<= 0 prog-list-number)
                   (<= prog-list-number 9))
              (let ((list (symbol-value
                           (intern
                            (format "skk-search-prog-list-%d" prog-list-number)))))
                (or list skk-search-prog-list)))
             (t
              skk-search-prog-list)))
          skk-undo-kakutei-flag t)
    ;; get henkan data back from skk-last-henkan-data.
    (setq skk-henkan-key (skk-get-last-henkan-datum 'henkan-key)
          skk-henkan-list (list (car (skk-get-last-henkan-datum 'henkan-list)))
          skk-henkan-okurigana (skk-get-last-henkan-datum
                                'henkan-okurigana)
          skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
    (setq skk-undo-kakutei-prev-state
          (list (cons 'skk-mode skk-mode)
                (cons 'skk-abbrev-mode skk-abbrev-mode)
                (cons 'skk-latin-mode skk-latin-mode)
                (cons 'skk-j-mode skk-j-mode)
                (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode)
                (cons 'skk-jisx0201-mode skk-jisx0201-mode)
                (cons 'skk-katakana skk-katakana)))
    (cond ((skk-get-last-henkan-datum 'abbrev-mode)
           (skk-abbrev-mode-on))
          ((or (not skk-mode) skk-latin-mode skk-jisx0208-latin-mode)
           (skk-j-mode-on)))
    (when (and skk-undo-kakutei-return-previous-point
               (markerp skk-henkan-end-point)
               (markerp skk-henkan-start-point))
      (setq skk-undo-kakutei-previous-point (point)
            skk-undo-kakutei-previous-length (- skk-henkan-end-point
                                                skk-henkan-start-point)))
    (when skk-use-numeric-conversion
      (setq skk-num-list (skk-get-last-henkan-datum 'skk-num-list)))
    (when (>= (point-max) end)
      ;; 最後の変換部分のテキストを消す。送り仮名を把握しているのなら
      ;; (skk-process-okuri-early が non-nil なら送り仮名を把握できない)、
      ;; 送り仮名を含めた部分までを消す。
      (delete-region skk-henkan-start-point end))
    (when skk-undo-kakutei-word-only
      (setq skk-last-buffer-undo-list buffer-undo-list))
    (goto-char skk-henkan-start-point)
    (insert-and-inherit "▼")
    (skk-set-marker skk-henkan-start-point (point))
    (cond
     (skk-okuri-char
      ;; 送りあり
      (insert-and-inherit (substring skk-henkan-key 0
                                     (1- (length skk-henkan-key))))
      (skk-set-marker skk-henkan-end-point (point))
      (when skk-henkan-okurigana
        (insert-and-inherit skk-henkan-okurigana)))
     (t
      (insert-and-inherit skk-henkan-key)
      (skk-set-marker skk-henkan-end-point (point))))
    (skk-message "確定アンドゥ！"
                 "Undo kakutei!")
    (setq skk-henkan-count 1)
    (skk-henkan)))

(defun skk-set-henkan-point (&optional arg)
  "変換を開始するポイントをマークし、対応する `skk-prefix' か母音を入力する。"
  (let* ((last-char (skk-downcase last-command-event))
         (normal (not (eq last-char last-command-event)))
         (sokuon (if (string= skk-prefix (skk-char-to-unibyte-string last-char))
                     (/= last-char ?o)
                   nil))
         (henkan-active (eq skk-henkan-mode 'active)))
    (cond
     ((not (eq skk-henkan-mode 'on))
      (if normal
          (skk-set-henkan-point-subr)
        (when skk-henkan-mode
          (skk-set-henkan-point-subr))
        (if henkan-active
            (skk-emulate-original-map arg)
          ;; What's to be here?
          ;;(skk-insert arg)
          )))
     ((not normal)
      ;; special char
      (insert-and-inherit last-char)
      (skk-set-marker skk-henkan-end-point (point))
      (setq skk-henkan-count 0)
      (setq skk-henkan-key (buffer-substring-no-properties
                            skk-henkan-start-point (point))
            skk-prefix "")
      (skk-henkan))
     ;; prepare for the processing of okurigana if not skk-okurigana
     ;; and the preceding character is not a numeric character.
     ;; if the previous char is a special midashi char or a
     ;; numeric character, we assume that the user intended to type the
     ;; last-command-char in lower case.
     ((and (or
            ;; for KAnji, KanJIru
            (not (skk-get-prefix skk-current-rule-tree))
            (if (/= skk-kana-start-point skk-henkan-start-point)
                (prog1
                    t
                  (unless sokuon ; for TaSSi or TasSi
                    (skk-kana-cleanup))) ; for NEko
              nil))
           (not skk-okurigana)
           (or (= skk-henkan-start-point (point))
               (let ((p (char-before)))
                 (not (or
                       ;; previous char is a special midashi char
                       (memq p skk-special-midashi-char-list)
                       ;; previous char is an ascii numeric char
                       (and (<= ?0 p)
                            (<= p ?9))
                       ;; previous char is a JIS X 0208 numeric char
                       (and (skk-jisx0208-p p)
                            (= (skk-char-octet p 0) 35) ;?#
                            (<= 48 (skk-char-octet p 1)) ; ?0
                            (<= (skk-char-octet p 1) 57))  ; ?9
                       )))))
      (cond
       (skk-process-okuri-early
        (skk-set-marker skk-henkan-end-point (point))
        (let ((char (skk-char-to-unibyte-string last-char)))
          (setq skk-okuri-char
                (or (cdr (assoc char skk-okuri-char-alist))
                    char)))
        (cond
         (sokuon
          (setq skk-henkan-key
                (concat (buffer-substring-no-properties
                         skk-henkan-start-point
                         skk-kana-start-point)
                        (if skk-katakana "ッ" "っ")
                        skk-henkan-okurigana))
          (skk-erase-prefix)
          (insert-and-inherit (if skk-katakana "ッ " "っ "))
          (setq skk-prefix "")
          (setq skk-henkan-count 0)
          (skk-henkan)
          (delete-char -2))
         (t
          (setq skk-henkan-key (concat
                                (buffer-substring-no-properties
                                 skk-henkan-start-point
                                 (point))
                                skk-okuri-char))
          (insert-and-inherit " ")
          (setq skk-prefix "")
          (setq skk-henkan-count 0)
          (skk-henkan)
          (delete-char -1)))
        ;; we set skk-kana-start-point here, since the marker may no
        ;; longer point at the correct position after skk-henkan.
        (skk-set-marker skk-kana-start-point (point)))
       ((/= skk-henkan-start-point (point))
        (when sokuon
          (skk-erase-prefix 'clean)
          (insert-and-inherit (if skk-katakana "ッ" "っ")))
        (cond
         ((and (not sokuon)
               (skk-get-prefix skk-current-rule-tree)
               normal
               (eq (char-before) (string-to-char skk-prefix)))
          ;; SKK の仕様にない例外の処理。
          ;; 例えば，ユーザが 「歩く」を挿入したくて "AruKu" と打つべき
          ;; ところを "ArukU" と打ってしまった場合。この場合 SKK 側で
          ;; どう処理するべきか、決まっていない。ここでは仮の処置として、
          ;; "AruKu" と同様の変換をするようにしておく。
          (setq skk-okuri-char nil
                skk-okurigana nil
                normal nil)
          (set 'last-command-event last-char)
          (let ((skk-dcomp-activate nil))
            (skk-kana-input arg))
          (skk-set-char-before-as-okurigana))
         (t
          (when (and skk-dcomp-activate
                     (skk-dcomp-marked-p))
            ;; 新しい marker を set する前に  skk-dcomp の marker をクリア
            ;; しておく。
            (skk-dcomp-before-kakutei))
          (skk-set-marker skk-okurigana-start-point (point))
          (insert-and-inherit "*")
          (skk-set-marker skk-kana-start-point (point))
          (setq skk-okuri-char (skk-char-to-unibyte-string last-char)
                skk-okurigana t)))))))
    (when normal
      (set 'last-command-event last-char)
      (skk-kana-input arg))))

;;;###autoload
(defun skk-henkan-on-message ()
  (condition-case nil
      (when (and skk-verbose
                 (not (or skk-isearch-switch
                          (skk-in-minibuffer-p)))
                 (eq skk-henkan-mode 'on)
                 (< (marker-position skk-henkan-start-point) (point))
                 (sit-for skk-verbose-wait))
        (skk-setup-verbose-messages)
        (message "%s" skk-henkan-on-message))
    (quit
     (keyboard-quit)))
  nil)

;;;###autoload
(defun skk-start-henkan (arg &optional prog-list-number)
  "▽モードでは漢字変換を開始する。▼モードでは次の候補を表示する。
▽モードでカナモードのまま漢字変換を開始した場合は、見出し語を平仮名に
変換してから漢字変換を開始する。見出し語を変換せずにそのまま漢字変換を
行いたければ、\\[universal-argument] SPC とタイプする。"
  (interactive "*p")
  (unless prog-list-number
    (setq prog-list-number current-prefix-arg))
  (skk-with-point-move
   (cancel-undo-boundary)
   (if (eq skk-henkan-mode 'active) ;▼モード
       (progn
         (setq skk-henkan-count (1+ skk-henkan-count))
         (skk-henkan))
     ;; ▽モード
     (save-match-data
       (let (pos)
         (skk-kana-cleanup 'force)
         (when (skk-get-prefix skk-current-rule-tree)
           ;; Never.  `skk-erase-prefix' called by `skk-kana-cleanup'
           ;; initializes `skk-prefix'.
           (skk-error "フィックスされていない skk-prefix があります"
                      "Have unfixed skk-prefix"))
         (setq pos (point))
         (when (< pos skk-henkan-start-point)
           (skk-error "カーソルが変換開始地点より前にあります"
                      "Henkan end point must be after henkan start point"))
         (setq skk-henkan-key (buffer-substring-no-properties
                               skk-henkan-start-point pos))
         (when (and skk-katakana
                    ;; C-u を使っていない場合
                    ;; prog-list-number は skk-insert() における current-prefix-arg
                    (not (and prog-list-number (listp prog-list-number))))
           (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)))
         (when (and skk-okurigana
                    (string-match "\\* *$" skk-henkan-key))
           (skk-error "空の送り仮名で漢字を登録しようとしています"
                      "No okurigana!"))
         (if skk-allow-spaces-newlines-and-tabs
             ;; skk-henkan-key の中の "[ \n\t]+" を完全に取り除く。
             (while (string-match "[ \n\t]+" skk-henkan-key)
               (setq skk-henkan-key
                     (concat (substring skk-henkan-key 0 (match-beginning 0))
                             (substring skk-henkan-key (match-end 0)))))
           (skk-save-point
            (beginning-of-line)
            (when (> (point) skk-henkan-start-point)
              (skk-error "変換キーに改行が含まれています"
                         "Henkan key may not contain a new line character")))
           ;; 最初のスペースで skk-henkan-key をカットするだけ。
           (setq skk-henkan-key (substring skk-henkan-key
                                           0
                                           (string-match " "
                                                         skk-henkan-key))))
         (skk-set-marker skk-henkan-end-point pos)
         (setq skk-henkan-count 0)
         (setq skk-annotation-first-candidate t)
         (skk-henkan prog-list-number)
         (when (and skk-abbrev-mode
                    (eq skk-henkan-mode 'active))
           ;; こうしておかないと変換後、次に入力される文字もまた
           ;; SKK abbrev-mode 入力になってしまう。
           (skk-j-mode-on skk-katakana)
           (setq skk-abbrev-mode t)))))))

(defun skk-auto-start-henkan (str)
  "STR が `skk-auto-start-henkan-keyword-list' の要素と一致 (member) する場合に変換を開始する。
関数 `skk-insert-str' の実行中、変数 `skk-auto-start-henkan' が non-nil のときにこの関数がコールされる。"
  (when (member str skk-auto-start-henkan-keyword-list)
    (skk-save-point
     (backward-char 1)
     (when (> (point) skk-henkan-start-point)
       (let ((skk-prefix ""))
         (skk-start-henkan (prefix-numeric-value current-prefix-arg)))))))

(defun skk-backward-and-set-henkan-point (arg)
  "ポイントの直前にある文字列の先頭に変換開始ポイントを示す \"▽\" を付ける。
カーソルの直前にある文字 (スペース文字、タブ文字、長音を表わす「ー」 は無条件
にスキップされる) を `skk-what-char-type' にて判別し、同種の文字列をひとかたま
りとして後方へスキップする。
但し、ひらがなの場合は「を」の直前で、カタカナの場合は「ヲ」の直前で止まる。
\\[universal-argument] ARG で ARG を与えると、その文字分だけ戻って同じ動作を行う。"
  (interactive "*P")
  (if (not skk-mode)
      (skk-emulate-original-map arg)
    (catch 'exit1
      (skk-save-point
       ;; とりあえず最初の SPC, TAB, 全角 SPC だけジャンプする。
       (skip-chars-backward " \t　")
       ;; 引数あり。
       (cond
        (arg
         (if (not skk-allow-spaces-newlines-and-tabs)
             (backward-char (prefix-numeric-value arg))
           (setq arg (prefix-numeric-value arg))
           (while (> arg 0)
             (skip-chars-backward " \t　")
             (if (bolp)
                 ;; 行頭だったら一行前の行末まで戻るが、arg は減らさない。
                 (backward-char 1)
               (backward-char 1)
               (setq arg (1- arg))))))
        (t
         ;; 引数なし。
         (let ((limit
                (if (not skk-allow-spaces-newlines-and-tabs)
                    (skk-save-point (beginning-of-line) (point))
                  (point-min)))
               ;; ＿￣＾¨｀´゜゛！？；：・．，。
               (unknown-chars-regexp
                (if skk-allow-spaces-newlines-and-tabs
                    "[ 　\n\tー〃ゞゝヾヽ]"
                  "[　ー〃ゞゝヾヽ]"))
               type p)
           (save-match-data
             (skk-save-point
              (backward-char 1)
              (while (and (> (point) limit)
                          ;; unknown-chars-regexp では文字種別が判別できないの
                          ;; で、その文字列が続く限りポイントをバッファの先頭
                          ;; 方向へ戻す。
                          (looking-at unknown-chars-regexp))
                (backward-char 1))
              (setq type (skk-what-char-type))
              (when (eq type 'unknown)
                (throw 'exit1 nil))
              (skk-backward-and-set-henkan-point-1 type)
              (setq p (point))
              (when skk-allow-spaces-newlines-and-tabs
                (while (and (> (point) limit) (bolp))
                  ;; 1 行上の行末へ。
                  (backward-char 1)
                  ;; ポイントが判別できない文字種別の上にある間は
                  ;; backward 方向へポイントを戻す。
                  ;;(while (and (> (point) limit)
                  ;;            (looking-at unknown-chars-regexp))
                  ;;  (backward-char 1))
                  (when ;;(or
                      (> 0 (skk-backward-and-set-henkan-point-1 type))
                    ;;(eq (skk-what-char-type) type))
                    (setq p (point)))))))
           (goto-char p)
           (skip-chars-forward unknown-chars-regexp))))
       (skk-set-henkan-point-subr)))))

(defun skk-backward-and-set-henkan-point-1 (type)
  "`skk-backward-and-set-henkan-point' のサブルーチン。
TYPE (文字の種類) に応じた文字をスキップしてバッファの先頭方向へ戻る。"
  (skip-chars-backward
   (cl-case type
     (hiragana
      ;; "を" の前で止まった方が便利？
      "ヽヾゝゞ〃ーんぁ-ゑ")
     (katakana
      ;; "ヲ" の前で止まった方が便利？
      "ヽヾゝゞ〃ーンァ-ヱ")
     (jisx0208-latin
      "　-ｚ")
     (ascii
      " -~"))))

(defun skk-what-char-type ()
  "現在のポイントにある文字の種類を判別する。
文字の種類に応じて、次のいずれかのシンボルを返す。
'hiragana 'katakana 'jisx0208-latin 'ascii 'unknown"
  (save-match-data
    (cond ((looking-at "[ぁ-ん]")
           'hiragana)
          ((looking-at "[ァ-ヶヽヾ]")
           'katakana)
          ;; "ー" を除外している ("ー" は "〇" と "―" の間に入っている)。
          ((looking-at "[　-〇―-ｚ]")
           'jisx0208-latin)
          ((looking-at "[ -~]")
           'ascii)
          (t
           'unknown))))

;;;###autoload
(defun skk-set-henkan-point-subr (&optional arg)
  "かなを入力した後で、ポイントに変換開始のマーク (▽) を付ける。
この関数は `skk-set-henkan-point' の内部関数としても使用されている。"
  (interactive "*P")
  (skk-with-point-move
;;;    (unless skk-undo-kakutei-word-only
;;;      (skk-cancel-undo-boundary))
   (if skk-henkan-mode
       (skk-kakutei)
     (skk-kana-cleanup));; XXX
   (when skk-undo-kakutei-word-only
     (setq skk-last-buffer-undo-list buffer-undo-list))
   (if (not (skk-get-prefix skk-current-rule-tree))
       (insert-and-inherit "▽")
     (skk-erase-prefix)
     (insert-and-inherit "▽")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix))
   (setq skk-henkan-mode 'on)
   (setq skk-henkan-end-point nil)
   (skk-set-marker skk-henkan-start-point (point)))
  nil)

(defun skk-change-marker ()
  "\"▽\"を\"▼\"に変える。`skk-henkan-mode' を active にする。"
  (skk-save-point
   (goto-char (1- skk-henkan-start-point))
   (unless (looking-at "▽")
     (skk-kakutei)
     (skk-error "▽がありません"
                "It seems that you have deleted ▽"))
   (cancel-undo-boundary)
   (let ((buffer-undo-list t))
     (insert-and-inherit "▼")
     (delete-char 1))
   (setq skk-henkan-mode 'active)))

(defun skk-change-marker-to-white ()
  "\"▼\"を\"▽\"に変える。`skk-henkan-mode' を on にする。"
  (skk-save-point
   (goto-char (1- skk-henkan-start-point))
   (cancel-undo-boundary)
   (if (looking-at "▼")
       (let ((buffer-undo-list t))
         (insert-and-inherit "▽")
         (delete-char 1))
     (goto-char skk-henkan-start-point)
     (insert-and-inherit "▽")
     (skk-set-marker skk-henkan-start-point (point))
     (skk-message "▼がありません"
                  "It seems that you have deleted ▼"))
   (setq skk-henkan-end-point nil)
   (setq skk-henkan-mode 'on)))

(defun skk-delete-henkan-markers (&optional nomesg)
  "変換時にカレントバッファに表われる \"▽\", \"▼\" マークを消す。"
  (when (marker-position skk-henkan-start-point)
    (save-match-data
      (skk-save-point
       (goto-char (1- skk-henkan-start-point))
       (cond
        ((eq skk-henkan-mode 'active)
         (when skk-use-face
           (skk-henkan-face-off))
         (if (looking-at "▼")
             (delete-char 1)
           (unless nomesg
             (skk-message "▼がありません"
                          "It seems that you have deleted ▼"))))
        ((looking-at "▽")
         (delete-char 1))
        ((not nomesg)
         (skk-message "▽がありません"
                      "It seems that you have deleted ▽")))))))

(defun skk-delete-okuri-mark (&optional noclear)
  "送り仮名関連フラグを消す。
送り仮名入力中にカレントバッファに表われる `*' マークを消し、
NOCLEAR が nil であれば送り仮名関連フラグを nil にセットする。"
  (when (and skk-okurigana
             skk-okurigana-start-point
             (markerp skk-okurigana-start-point)
             (marker-position skk-okurigana-start-point))
    (skk-save-point
     (when (eq ?* (char-after skk-okurigana-start-point))
       (delete-region skk-okurigana-start-point
                      (1+ skk-okurigana-start-point))))
    (setq skk-okurigana nil)
    (unless noclear
      (setq skk-okuri-char nil
            skk-henkan-okurigana nil))))

;; "[F7] カタカナ" のような簡単な変換機能を割り当てるためのコマンド定義
(defun skk-start-henkan-prog-null-handler (arg)
  (condition-case nil
      (skk-emulate-original-map arg)
    (error
     (let ((key (this-command-keys)))
       (when (keymapp (let (skk-j-mode)
                        (key-binding key)))
         (define-key skk-j-mode-map key nil)
         (skk-unread-event (if (vectorp key)
                               (aref key 0)
                             (string-to-char key))))))))

(defun skk-start-henkan-prog-i (i arg)
  (cond ((and skk-henkan-mode
              (symbol-value (intern (format "skk-search-prog-list-%d" i))))
         (when (eq skk-henkan-mode 'active)
           (let ((skk-verbose-wait 0))
             (skk-henkan-inactivate)))
         (skk-start-henkan arg i))
        (t
         (skk-start-henkan-prog-null-handler arg))))

(eval-when-compile
  (defmacro skk-define-start-henkan-progs ()
    (let (list)
      (dotimes (i 10)
        (setq list
              (nconc
               list
               `((defun ,(intern (format "skk-start-henkan-prog-%d" i)) (arg)
                   (interactive "*p")
                   (skk-start-henkan-prog-i ,i arg))))))
      (cons 'progn list))))

(skk-define-start-henkan-progs)

;;; jisyo related functions
(defun skk-purge-from-jisyo (&optional arg)
  "▼モードで現在の候補を辞書バッファから消去する。"
  (interactive "*P")
  (skk-with-point-move
   (cond
    ((not (eq skk-henkan-mode 'active))
     (skk-emulate-original-map arg))
    ((and (eq skk-henkan-mode 'active)
          (not (string= skk-henkan-key ""))
          (yes-or-no-p
           (format
            (if skk-japanese-message-and-error
                "%s /%s/%sを辞書から削除します。良いですか？ "
              "Really purge \"%s /%s/%s\"? ")
            skk-henkan-key
            (skk-get-current-candidate)
            (cond
             ((not (and skk-henkan-okurigana
                        (or skk-henkan-okuri-strictly
                            skk-henkan-strict-okuri-precedence)))
              " ")
             (skk-japanese-message-and-error
              (format " (送り仮名: %s) " skk-henkan-okurigana))
             (t
              (format " (okurigana: %s) " skk-henkan-okurigana))))))
     ;; skk-henkan-start-point から point まで削除してしまっても、変換直後
     ;; に (カーソルを動かすことなく) skk-purge-from-jisyo を呼べば問題ない
     ;; が、カーソルが違う場所へ移動していた場合は、削除すべきでないものま
     ;; で削除してしまう可能性がある。そこで、送り仮名があればその長さを含
     ;; めた end を求め、今回の変換に関連した個所だけを正確に切り取るように
     ;; する。
     (let ((end (if skk-henkan-okurigana
                    (+ (length skk-henkan-okurigana)
                       skk-henkan-end-point)
                  skk-henkan-end-point))
           (word (skk-get-current-candidate)))
       (skk-update-jisyo word 'purge)
       ;; Emacs 19.28 だと Overlay を消しておかないと、次に insert される
       ;; skk-henkan-key に何故か Overlay がかかってしまう。
       (when skk-use-face
         (skk-henkan-face-off))
       (delete-region skk-henkan-start-point end)
       (skk-change-marker-to-white)
       (skk-kakutei)))))
  nil)

;;;###autoload
(defun skk-save-jisyo (&optional quiet)
  "SKK の辞書バッファをセーブする。
オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを
出さない。"
  (interactive "P")
  ;; skk.el 以外で提供される辞書セーブ機能を利用できるように関数を funcall する
  ;; 形にしておく。
  (unless noninteractive
    ;; emacs の終了時に個人辞書を保存するため、skk.el (5190 行あたり) で
    ;; kill-emacs-hook に #'skk-save-jisyo を add-hook している。
    ;; skk-mode に入るたびに個人辞書の存在を確認している（なければ自動生成）が、
    ;; 何らかの原因で個人辞書が存在しない状態で emacs を終了しようとすると
    ;; skk-save-jisyo-original (skk-get-jisyo-buffer) 内のエラーで emacs を終了できない事態となる。
    (when (file-exists-p (skk-jisyo))
      (funcall skk-save-jisyo-function quiet))))

;;;###autoload
(defun skk-save-jisyo-original (&optional quiet)
  "SKK の辞書バッファをセーブする。
オプショナル引数 QUIET が non-nil であれば、辞書セーブ時のメッセージを出さない。"
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (not (and jisyo-buffer
                  (buffer-modified-p jisyo-buffer)))
        (unless quiet
          (skk-message "SKK 辞書を保存する必要はありません"
                       "No need to save SKK jisyo")
          (sit-for 1))
      ;;
      (with-current-buffer jisyo-buffer
        (when (skk-share-private-jisyo-p)
          (lock-buffer (skk-jisyo))
          (when (skk-jisyo-is-shared-p)
            (skk-update-shared-jisyo)))
        (let ((inhibit-quit t)
              (tempo-file (make-temp-file "skk")))
          (unless quiet
            (skk-message "SKK 辞書を保存しています..."
                         "Saving SKK jisyo..."))
          (skk-save-jisyo-as tempo-file)
          (skk-check-size-and-do-save-jisyo tempo-file)
          ;; 辞書のセーブに成功して初めて modified フラグを nil にする。
          (cond
           ((skk-share-private-jisyo-p)
            (skk-init-shared-jisyo)
            ;; `set-buffer-modified-p' は不要な lock を解除する。ただし、
            ;; バッファとファイル名が関連付けられている必要がある。
            (let ((buffer-file-name (expand-file-name (skk-jisyo)))
                  (buffer-file-truename (file-truename (skk-jisyo))))
              (set-buffer-modified-p nil)))
           (t
            (set-buffer-modified-p nil)))
          (unless quiet
            (skk-message "SKK 辞書を保存しています...完了！"
                         "Saving SKK jisyo...done")
            (sit-for 1))))))
  (setq skk-update-jisyo-count 0))

(defun skk-init-shared-jisyo ()
  (fillarray skk-jisyo-update-vector nil)
  (with-temp-buffer
    (insert skk-emacs-id "\n")
    (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)))

(defun skk-jisyo-is-shared-p ()
  (and (file-exists-p skk-emacs-id-file)
       (with-temp-buffer
         (insert-file-contents skk-emacs-id-file)
         (goto-char (point-min))
         ;; 個人辞書が他の emacs 上の skk により更新されたかをチェック
         (not (search-forward skk-emacs-id nil t)))))

(defun skk-update-shared-jisyo ()
  "個人辞書が共有されている場合に、新しい情報に更新する。
現在の辞書バッファの内容を消去して、他の Emacs 上の SKK が更新した
`skk-jisyo' を読み込む。"
  (erase-buffer)
  (insert-file-contents (skk-jisyo))
  (skk-setup-jisyo-buffer)
  ;; skk-jisyo-update-vector にしたがってバッファを更新する。
  (let ((index 0)
        (len (length skk-jisyo-update-vector))
        list skk-henkan-key)
    (while (and (< index len)
                (setq list (aref skk-jisyo-update-vector index)))
      ;; skk-update-jisyo-1, skk-search-jisyo
      ;; で参照される skk-henkan-key をセットする
      (when (setq skk-henkan-key (car list))
        (skk-update-jisyo-1 (nth 1 list)                          ;okurigana
                            (nth 2 list)                          ;word
                            (skk-search-jisyo (nth 1 list) 0 'delete) ;old-words-list
                            (nth 3 list)))                            ;purge
      (setq index (1+ index)))))

(defun skk-save-jisyo-as (file)
  (save-match-data
    (let (buffer-read-only)
      (goto-char (point-min))
      (unless (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
        (skk-error
         "\
送りありエントリのヘッダーがありません！ SKK 辞書のセーブを中止します"
         "\
Header line for okuri-ari entries is missing!  Stop saving SKK jisyo"))
      (unless (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
        (skk-error
         "\
送りなしエントリのヘッダーがありません ！ SKK 辞書のセーブを中止します"
         "\
Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo")))
    (let ((coding-system-for-write (skk-find-coding-system (skk-jisyo t)))
          jka-compr-compression-info-list)
      (write-region 1 (point-max) file nil 'nomsg))))

(defun skk-check-size-and-do-save-jisyo (new-file)
  (skk-bind-last-command-char nil
    (let ((new-size (nth 7 (file-attributes new-file)))
          old-size
          ;; yes-or-no-p に回答し、newline すると、this-command が変ってしまう。
          this-command this-command-char last-command)
      (when (zerop new-size)
        (delete-file new-file)
        (skk-error "SKK 辞書が空になっています！ 辞書のセーブを中止します"
                   "Null SKK jisyo!  Stop saving jisyo"))
      (cond
       ((or (not skk-compare-jisyo-size-when-saving)
            ;; 旧辞書とのサイズを比較しない。
            (progn
              ;; (1)skk-jisyo がないか、
              ;; (2)new-file と skk-jisyo が同一のサイズか
              ;;    (skk-(aux-)large-jisyo から新規の単語を読み込まなかったり、
              ;;    新規単語の登録を行わなかった場合はサイズが同じ)、
              ;; (3)new-file の方が大きい
              ;; 場合 (上記の 3 通りであればいずれも正常)。
              (setq old-size (nth 7 (file-attributes (skk-jisyo))))
              (or (not old-size)
                  (>= new-size old-size))))
        (skk-make-new-jisyo new-file))

       ((skk-yes-or-no-p
         (format "%s が %dbytes 小さくなりますが、セーブして良いですか？"
                 (skk-jisyo) (- old-size new-size))
         (format "New %s will be %dbytes smaller.  Save anyway?"
                 (skk-jisyo) (- old-size new-size)))
        ;; とにかくセーブ。
        (skk-make-new-jisyo new-file))

       (t
        ;; セーブとり止め。
        (delete-file new-file)
        (with-output-to-temp-buffer "*SKK warning*"
          (if skk-japanese-message-and-error
              (princ "\
セーブしようとする辞書のサイズが元のサイズよりも小さくなってしまうため、
セーブを中止しました。辞書のサイズが小さくなった原因には、例えば、

    (a) M-x skk-purge-from-jisyo を実行した。

    (b) ~/.skk-jisyo の文字コードとは異なる文字コードで \" *.skk-jisyo*\"
       バッファが保存されようとしている。

    (c) \" *.skk-jisyo*\" バッファを自分で編集した。

などがあります。(a) と (b) の場合は異常ではありません。
\(c) の場合は編集の内容によります。原因を慎重に確認してから辞書を保存するこ
とをお勧めします。

元の辞書を再び読み込むには、

    M-x skk-reread-private-jisyo

を実行して下さい。")
            (princ "\
Saving your private dictionary has been canceled, since the size of the
dictionary will be smaller.  The following cases should be considered:

   (a) You executed M-x skk-purge-from-jisyo,

   (b) The coding system SKK tried to save \" *.skk-jisyo*\" buffer in
       is different from that of ~/.skk-jisyo.

   (c) You have edited \" *.skk-jisyo*\" buffer manually.

Either the case (a) or (b) is not strange.  Probability of the case (c)
depends on how you edited the buffer.  Anyway, it is strongly recommended
that you check each of the cases above and save the dictionary carefully.

If you want to restore the dictionary from your drive, try

    M-x skk-reread-private-jisyo
")))
        (skk-error "SKK 辞書のセーブを中止しました！"
                   "Stop saving SKK jisyo!"))))))

(defun skk-make-new-jisyo (tempo-file)
  "TEMPO-FILE を新規の `skk-jisyo' にする。
`skk-backup-jisyo' が non-nil だったらバックアップ辞書を作る。"
  (if skk-backup-jisyo
      (progn
        (when (file-exists-p skk-backup-jisyo)
          (delete-file skk-backup-jisyo))
        (rename-file (skk-jisyo) skk-backup-jisyo))
    (delete-file (skk-jisyo)))
  (rename-file tempo-file (skk-jisyo) 'ok-if-already-exists))

;;;###autoload
(defun skk-reread-private-jisyo (&optional force)
  "バッファに読み込んだ個人辞書を破棄し、ファイルからバッファへ再読み込みする。
オプショナル引数の FORCE が non-nil であれば、破棄の確認をしない。"
  (interactive "P")
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (when (and buf
               (or force
                   (skk-yes-or-no-p
                    "未セーブの個人辞書を破棄しますか？ "
                    "Discard your unsaved private JISYO? ")))
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (kill-buffer buf))
      (unless (skk-get-jisyo-buffer skk-jisyo 'nomsg)
        (skk-error "個人辞書を再読み込みすることができません！"
                   "Cannot reread private JISYO!")))))

(defun skk-record-jisyo-data ()
  "個人辞書に関する統計情報を `skk-record-file' に保存する。"
  (unless (or (not skk-keep-record)
              (> 1 skk-kakutei-count))
    (with-temp-file skk-record-file
      (insert-file-contents skk-record-file)
      (goto-char (point-min))
      (insert (format
               "%s  登録: %3d  確定: %4d  確定率: %3d%%  語数:%6d\n"
               (current-time-string)
               skk-touroku-count
               skk-kakutei-count
               (/ (* 100 (- skk-kakutei-count skk-touroku-count))
                  skk-kakutei-count)
               (cond
                ((featurep 'skk-rdbms)
                 ;; RDBMS を使えばもっと興味深い統計が取れるかもしれない
                 ;; が、とりあえず語数だけ数えて入れておく。
                 (skk-rdbms-count-jisyo-candidates
                  skk-rdbms-private-jisyo-table))
                (skk-count-private-jisyo-candidates-exactly
                 (skk-count-jisyo-candidates
                  (expand-file-name (skk-jisyo))))

                ;; 1 行 1 候補とみなす。
                (t
                 (with-current-buffer (skk-get-jisyo-buffer
                                       skk-jisyo 'nomsg)
                   (- (count-lines (point-min) (point-max))
                      2))))))
      (when (integerp skk-keep-record)
        (setq selective-display nil)
        (widen)
        (goto-char (point-min))
        (forward-line skk-keep-record)
        (delete-region (point) (point-max))))
    (setq skk-touroku-count 0
          skk-kakutei-count 0)))

(defun skk-count-jisyo-candidates (file-or-table)
  "SKK 辞書の候補数を数える。"
  (interactive
   (list (cond
          ((eq skk-count-jisyo-candidates-function
               'skk-count-jisyo-candidates-original)
           (read-file-name
            (format "Jisyo file: (default: %s) " (skk-jisyo))
            default-directory (skk-jisyo) 'confirm))
          ((eq skk-count-jisyo-candidates-function
               'skk-rdbms-count-jisyo-candidates)
           ;; データベースファイルを直接ファイル名で指定できる
           ;; permission がない場合が多いよね...。
           ;;(read-file-name
           ;; (format "Jisyo table: (default: %s) "
           ;;    skk-rdbms-private-jisyo-table))
           skk-rdbms-private-jisyo-table))))
  ;; mule@emacs19.31 だと下記のようにすると (`ァ' が原因のよう) 何故か
  ;; default-directory の末尾に改行が付く。
  ;; 通常は気が付かないが、rsz-mini.el を使って resize-minibuffer-mode を
  ;; non-nil にしていると不要な 2 行目が出現する。
  ;; (interactive "f辞書ファイル: ")
  (let ((count (funcall skk-count-jisyo-candidates-function
                        file-or-table)))
    (if (called-interactively-p 'interactive)
        (message (if (= count 1)
                     "%d candidate"
                   "%d candidates")
                 count)
      count)))

(defun skk-count-jisyo-candidates-original (file)
  "SKK 辞書の候補数を数える。
`[' と `]' に囲まれた送り仮名毎のブロック内は数えない。"
  (let ((orig-buffer (find-buffer-visiting file)))
    (with-current-buffer (let ((find-file-visit-truename t))
                           (or orig-buffer (find-file-noselect file)))
      (save-match-data
        (let ((count 0)
              (min (point-min))
              (max (and (called-interactively-p 'interactive) (point-max)))
              (interactive-p (called-interactively-p 'interactive)))
          (goto-char min)
          (unless (and
                   ;; こちらは skk-save-point を使わず、ポイントを移動させる。
                   (re-search-forward "^;; okuri-ari entries.$" nil t nil)
                   (skk-save-point
                    (re-search-forward "^;; okuri-nasi entries.$" nil t nil)))
            (skk-error "このファイルは SKK 辞書ではありません"
                       "This file is not an SKK dictionary"))
          (beginning-of-line)
          (while (looking-at ";")
            (forward-line 1)
            (beginning-of-line))
          (search-forward " " nil t)
          (while (search-forward "/" nil t)
            (cond ((or (eolp)
                       (looking-at "\\["))
                   (forward-line 1)
                   (beginning-of-line)
                   (while (looking-at ";")
                     (forward-line 1)
                     (beginning-of-line))
                   (search-forward " " nil t))
                  (t
                   (setq count (1+ count))))
            (when interactive-p
              (message "Counting jisyo candidates...%3d%% done"
                       (/ (* 100 (- (point) min)) max))))
          (prog1
              count
            (unless orig-buffer
              (kill-buffer (current-buffer)))))))))

;;;###autoload
(defun skk-create-file (file &optional japanese english modes)
  "FILE がなければ、FILE という名前の空ファイルを作る。
オプショナル引数の JAPANESE/ENGLISH を指定すると、ファイル作成後そのメッセージ
をエコーエリアに表示する。"
  (let ((file (expand-file-name file)))
    (if (file-exists-p file)
        (when modes
          (set-file-modes file modes))
      (write-region 1 1 file nil 0)
      (when modes
        (set-file-modes file modes))
      (when (or japanese english)
        (message "%s"
                 (if skk-japanese-message-and-error
                     japanese
                   english))
        (sit-for 3)))))

;;;###autoload
(defun skk-get-jisyo-buffer (file &optional nomsg)
  "FILE を開いて SKK 辞書バッファを作り、バッファを返す。
辞書バッファには `skk-jisyo-code' が適用される (nil であれば euc) が、FILE に (\"path/to/file\" . CODING-SYSTEM) のコンスセルも指定できる。
オプショナル引数の NOMSG を指定するとファイル読み込みの際のメッセージを表示しな
い。"
  (when file
    (let* ((inhibit-quit t)
           (code (skk-find-coding-system (cond ((consp file)
                                                (cdr file))
                                               ((string= file (skk-jisyo))
                                                (skk-jisyo t))
                                               (t
                                                skk-jisyo-code))))
           (file (or (car-safe file)
                     file))
           (enable-character-translation
            (not (memq code '(euc-japan shift_jis junet))))
           (buf-name (concat " *"
                             (file-name-nondirectory file)
                             "*"))
           (buf (get-buffer buf-name)))
      ;; 辞書バッファとしてオープンされているなら、何もしない。
      (unless (buffer-live-p buf)
        (setq buf (get-buffer-create buf-name))
        (setq file (expand-file-name file))
        (with-current-buffer buf
          (buffer-disable-undo)
          (auto-save-mode -1)
          ;; ワーキングバッファのモードラインはアップデートされない？
          ;;(make-local-variable 'line-number-mode)
          ;;(make-local-variable 'column-number-mode)
          ;;(setq column-number-mode nil
          ;;      line-number-mode nil)
          (setq buffer-read-only nil
                case-fold-search nil
                ;; buffer-file-name を nil にしておくと M-x compile など
                ;; 内部で save-some-buffers をコールしているコマンドを
                ;; 使ったときでもセーブするかどうかを尋ねてこなくなる。
                ;; buffer-file-name file
                ;; cache-long-line-scans nil
                ;; dabbrev のサーチとなるバッファにならないように存在し
                ;; ないモード名にしておく。実害のある副作用はないはず。
                major-mode 'skk-jisyo-mode
                mode-name "SKK dic")
          (unless nomsg
            (skk-message "SKK 辞書 %s をバッファに読み込んでいます..."
                         "Inserting contents of %s ..."
                         (file-name-nondirectory file)))
          (condition-case nil
              (let ((coding-system-for-read code)
                    format-alist)
                (insert-file-contents file))
            (error
             (when (buffer-live-p buf)
               (kill-buffer buf))
             (skk-error "`%s'を読み込めません" "Cannot load `%s'." file)))
          (unless nomsg
            (skk-message "SKK 辞書 %s をバッファに読み込んでいます...完了！"
                         "Inserting contents of %s ...done"
                         (file-name-nondirectory file)))
          (skk-setup-jisyo-buffer)
          (set-buffer-modified-p nil)))
      buf)))

;;;###autoload
(defun skk-search ()
  "`skk-current-search-prog-list' の各要素（検索プログラム）を順に評価する。
いずれかの要素が候補を見つけた時点で終了する。"
  ;; 個人辞書で候補が見つかればそれを返す（L 辞書まで見にいく訳ではない）
  (let (l prog)
    (while (and (null l)
                skk-current-search-prog-list)
      (setq prog (car skk-current-search-prog-list))
      (setq l (if (and skk-use-numeric-conversion
                       (string-match "[0-9]" skk-henkan-key)
                       (skk-numeric-program-p prog))
                  ;; -- 12.2.1 からの変更 --
                  ;; 数値変換時に、非数値変換も同時に検索して候補に
                  ;; 含める。
                  (skk-nunion (let (skk-use-numeric-conversion)
                                (eval prog))
                              (eval prog))
                (let (skk-use-numeric-conversion)
                  (eval prog))))
      (setq skk-current-search-prog-list (cdr skk-current-search-prog-list)))
    (setq skk-search-state (list skk-henkan-key prog l))
    l))

(defun skk-search-state ()
  (interactive)
  (with-output-to-temp-buffer "*skk search state*"
    (with-current-buffer standard-output
      (insert (format "skk-henkan-key: %s\n" (nth 0 skk-search-state))
              (format "skk-search-prog: %s\n" (nth 1 skk-search-state))
              (format "skk-search() result: %s\n\n" (nth 2 skk-search-state)))
      (when (equal (nth 1 skk-search-state)
                   '(skk-search-extra-jisyo-files))
        (mapconcat #'(lambda (x)
                       (insert (format "%s\n" x)))
                   skk-search-ex-state "")))))

(defun skk-numeric-program-p (program)
  "辞書検索プログラム PROGRAM が数値変換有効かどうか判定する。
もしプログラムが `skk-non-numeric-prog-list' に指定されていたら nil を返す。
さもなければ non-nil を返す。"
  (not (or (memq (car program) skk-non-numeric-prog-list)
           (member program skk-non-numeric-prog-list))))

(defun skk-search-jisyo-file (file limit &optional nomsg)
  "SKK 辞書フォーマットの FILE で `skk-henkan-key' をキーにして検索する。
検索領域が LIMIT 以下になるまでバイナリサーチを行い、その後リニアサーチを行う。
LIMIT が 0 であれば、リニアサーチのみを行う。
辞書がソートされていない場合は LIMIT を 0 とする必要がある。
オプショナル引数の NOMSG が non-nil であれば `skk-get-jisyo-buffer' の
メッセージを出力しない。

FILE には辞書ファイルだけでなく、
  (辞書ファイル . コーディングシステム)
のペアも受けつける。コーディングシステムは `skk-jisyo-code' と
同様に指定する。"
  (skk-search-jisyo-buf (skk-get-jisyo-buffer file nomsg)
                        limit))

(defun skk-search-extra-jisyo-files ()
  ;; skk-search-prog-list の要素のひとつ
  (setq skk-search-ex-state nil)
  (let (candidates words)
    (dolist (file skk-extra-jisyo-file-list)
      (setq words (skk-search-jisyo-file file 10000))
      (when words
        (add-to-list 'skk-search-ex-state (cons file words)))
      (setq candidates (nconc candidates words)))
    candidates))

(defun skk-search-itaiji ()
  ;; skk-search-prog-list の要素として使用する
  (and (= (length skk-henkan-key) 1)
       skk-itaiji-jisyo
       (skk-search-jisyo-file skk-itaiji-jisyo 0 t)))

(defun skk-search-server (file limit &optional nomsg)
  "辞書サーバを使用して `skk-henkan-key' をキーにして検索する。
辞書サーバが使用できないときは、FILE をバッファに読み込んで検索する。
LIMIT と NOMSG は辞書サーバが使用できないときのみ有効。
これらの引数については `skk-search-jisyo-file' を参照のこと。"
  (if (or skk-server-host
          skk-servers-list)
      (skk-search-server-1 file limit)
    ;; 辞書サーバが利用可能でなければ file を検索する。
    ;; 引数 file は通常 `skk-aux-large-jisyo' が指定される。
    (when (and (stringp file)
               (file-readable-p file))
      (skk-search-jisyo-file file limit nomsg))))

(defun skk-okuri-search ()
  "見出し語を送り仮名を含むものとして検索する。
例えば、`skk-auto-okuri-process' が non-nil ならば \"Uresii\" のように送り仮
名も含めてタイプしても送りありの \"嬉しい\" を探し出す。"
  (when skk-auto-okuri-process
    (skk-okuri-search-1)))

(defun skk-search-jisyo-buf (buf limit)
  "バッファを BUF に移動して、そこを辞書として検索する。"
  (when (buffer-live-p buf)
    ;; skk-henkan-key と skk-henkan-okurigana はカレントバッファの
    ;; ローカル値なので、あらかじめ取得。
    (let ((okurigana (or skk-henkan-okurigana
                         skk-okuri-char))
          (midasi (if skk-use-numeric-conversion
                      (skk-num-compute-henkan-key skk-henkan-key)
                    skk-henkan-key))
          (henkan-buffer (current-buffer))
          words-list)
      (with-current-buffer buf
        (setq skk-henkan-key midasi
              words-list (skk-search-jisyo okurigana limit))
        (skk-select-words-from-list words-list
                                    henkan-buffer
                                    midasi
                                    okurigana)))))

(defun skk-search-jisyo (okurigana limit &optional delete)
  "カレントバッファを辞書として検索する。
この関数の戻り値は、`skk-henkan-key' をキーとして `skk-compute-henkan-lists' を
評価した値である。
DELETE が non-nil であれば `skk-henkan-key' にマッチするエントリを削除する。"

  ;; (let ((skk-henkan-key "ほかん"))
  ;;   (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
  ;;     (skk-search-jisyo nil 0)))
  ;; => (("補間" "補完" "保管") nil nil nil)

  ;; (let ((skk-henkan-key "うごk"))
  ;;   (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
  ;;     (skk-search-jisyo "く" 0)))
  ;; => (("動") ("[く") ("動") ("]"))

  (let ((key (concat "\n" skk-henkan-key " /"))
        min max size p)
    (save-match-data
      ;; skk-okuri-ari-min と skk-okuri-ari-max は辞書バッファのローカル値。
      (if okurigana
          (setq min skk-okuri-ari-min
                max skk-okuri-ari-max)
        (setq min skk-okuri-nasi-min
              max (point-max)))
      (when (> limit 0)
        ;; 二分探索
        (while (> (setq size (- max min)) limit)
          (goto-char (+ min (/ size 2)))
          (beginning-of-line)
          (setq p (point))
          (if (= p min)
              (setq max min)    ; return
            (let ((p-is-further
                   ;; 送りありなら逆順に比較する。
                   (if okurigana
                       (skk-string< (buffer-substring-no-properties
                                     p (1- (search-forward  " ")))
                                    skk-henkan-key)
                     (skk-string< skk-henkan-key
                                  (buffer-substring-no-properties
                                   p (1- (search-forward " ")))))))
              (if p-is-further
                  (setq max p)
                (setq min p))))))
      (goto-char min)
      ;; key が検索開始地点にあった場合でも検索可能なように一文字戻る。
      ;; key の先頭部分に "\n" が含まれていることに注意。
      (unless (bobp)
        (backward-char 1))
      ;; case-fold-search は、辞書バッファでは常に nil。
      (when (search-forward key max 'noerror)
        (prog1
            (skk-compute-henkan-lists okurigana)
          (when delete
            (beginning-of-line)
            (delete-region (point)
                           (progn
                             (forward-line 1)
                             (point)))))))))

(defun skk-select-words-from-list (list buffer midasi okurigana)
  "`skk-search-jisyo' が返した候補リストから現在要求されている候補を選びだす。"
  (when list
    (let ((words
           (cond
            ((and okurigana
                  skk-henkan-okuri-strictly)
             ;; 送り仮名が同一の候補のみを返す。
             (nth 2 list))
            ((and okurigana
                  skk-henkan-strict-okuri-precedence)
             ;; 送り仮名が同一の候補のうしろに、
             ;; その他の候補をつけてかえす。
             (skk-nunion (nth 2 list)
                         (car list)))
            (t
             (car list)))))
      (dolist (function skk-search-end-function)
        (setq words (funcall function buffer midasi okurigana words)))
      words)))

;;;###autoload
(defun skk-compute-henkan-lists (okurigana)
  "辞書候補群を４つのリストに分解する。
詳しくは、この関数のコメントを参照。"
  ;; 送りなし (例えば、辞書エントリ "てんさい /転載/天災/天才/" の処理)
  ;;   words1 := ("転載" "天災" "天才") == 全候補群
  ;;   words2 := nil
  ;;   words3 := nil
  ;;   words4 := nil
  ;;
  ;; (with-temp-buffer
  ;;   (insert "てんさい /転載/天災/天才/")
  ;;   (goto-char (point-min))
  ;;   (search-forward " /")
  ;;   (skk-compute-henkan-lists ""))
  ;; => (("転載" "天災" "天才") nil nil nil)

  ;; 送りあり (例えば、「泣く」の変換を行った場合の、辞書エントリ
  ;;           "なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/" の処理)
  ;;   words1 := ("亡" "無" "鳴" "泣")  == 漢字部分の全候補群
  ;;   words2 := ("[く")               == 他の送り仮名を使う漢字候補群 (あれ
  ;;                                       ば) + 今回の変換の送り仮名部分
  ;;   words3 := ("無" "鳴" "泣")       == 今回の変換の送り仮名を使う可能性の
  ;;                                       ある全漢字候補群
  ;;   words4 := ("]" "[き" "亡" "]")   == 他の送り仮名を使う漢字候補群 (残
  ;;                                       り。あれば)
  ;;
  ;; (with-temp-buffer
  ;;   (insert "なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/")
  ;;   (goto-char (point-min))
  ;;   (search-forward " /")
  ;;   (skk-compute-henkan-lists "く"))
  ;; => (("亡" "無" "鳴" "泣") ("[く") ("無" "鳴" "泣") ("]" "[き" "亡" "]"))
  ;;
  ;;   * "[" は直後に続くひらがなを送り仮名に持つ漢字の候補群の始まりを表し、
  ;;     "]" は、該当の送り仮名グループの終りを示す。

  ;; この関数は、変換時と、確定直後の辞書のアップデート時の２度呼ばれる
  ;; (変換時に検索を行った辞書が、skk-jisyo とは限らないので、２度計算せざる
  ;; を得ない)。
  ;;
  ;; 変換時は、
  ;; o skk-henkan-okuri-strictly が non-nil であれば、計算結果の words3 を、
  ;; o skk-henkan-okuri-strictly が nil であって
  ;;   - skk-henkan-strict-okuri-precedence が non-nil あれば (skk-nunion words3 words1) を
  ;;   - skk-henkan-strict-okuri-precedence が nil の場合は words1 を
  ;; 取り出す。
  (cond
   ((not okurigana)
    (list (split-string (buffer-substring-no-properties (point) (1- (line-end-position)))
                        "/")
          nil nil nil))
   (t
    (save-match-data
      (let ((stage 1) q1 q2 q3 q4
            (okuri-key (concat "\[" okurigana))
            item headchar)

        (while (not (eolp))
          (setq item (buffer-substring-no-properties (point) (1- (search-forward "/")))
                headchar (if (string= item "")
                             (identity 0)
                           (aref item 0)))
          (cond ((and (eq headchar ?\[)
                      (<= stage 2))
                 (setq item (skk-compute-henkan-lists-sub-adjust-okuri item okuri-key))
                 (if (string= item okuri-key)
                     (progn
                       (setq q2 (cons item q2))
                       (setq stage 3))
                   (setq stage 2)
                   (setq q2 (cons item q2))))
                ((= stage 1)
                 (setq q1 (cons item q1)))
                ((= stage 2)
                 (setq q2 (cons item q2)))
                ((= stage 3)
                 (if (eq headchar ?\])  ; ?\]
                     (progn
                       (setq stage 4)
                       (setq q4 (cons item q4)))
                   (setq q3 (cons item q3))))
                ((= stage 4)
                 (setq q4 (cons item q4)))))

        (list (nreverse q1)       ; words1
              (nreverse q2)       ; words2
              (nreverse q3)       ; words3
              (nreverse q4))))))) ; words4

(defun skk-compute-henkan-lists-sub-adjust-okuri (item &optional okuri-key)
  ;; Yet to be elucidated.
  item)

;;;###autoload
(defun skk-remove-duplicates (list)
  "LIST から、重複する要素を除外したリストを返す。"
  (let (new)
    (dolist (x list)
      (or (member x new)
          (setq new (cons x new))))
    (nreverse new)))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  "SKK 辞書フォーマットの FILE から検索する。
候補を見つけた場合は、大域変数 `skk-kakutei-henkan-flag' に non-nil を代入する。
引数については `skk-search-jisyo-file' を参照。

利用する場合は `skk-search-prog-list' の先頭に配する事。"
  (setq skk-kakutei-henkan-flag (skk-search-jisyo-file file limit nomsg)))

;;;###autoload
(defun skk-update-jisyo (word &optional purge)
  (funcall skk-update-jisyo-function word purge)
  (when (and skk-save-jisyo-instantly
             (or skk-jisyo-updated  ; skk-henkan-in-minibuff で setq
                 purge))
    (skk-save-jisyo 'quiet)
    (setq skk-jisyo-updated nil)))

;;;###autoload
(defun skk-update-jisyo-original (word &optional purge)
  "次の変換時に WORD が最初の候補になるように、個人辞書を更新する。
PURGE が non-nil で WORD が共有辞書にある候補なら `skk-ignore-dic-word'
関数でクォートした候補を個人辞書に作り、次の変換から出力しな
いようにする。
WORD が共有辞書になければ、個人辞書の辞書エントリから削除する。"
  ;;
  ;; 個人辞書のエントリ
  ;;         ;; okuri-ari entries.
  ;;  変で   でt /出/[て/出/]/[た/出/]/
  ;;  換昇   つi /付/[い/付/]/
  ;;  順順   けs /消/[す/消/]/[し/消/]/[せ/消/]/[さ/消/]/
  ;;   ↓    かえs /返/[し/返/]/[す/返/]/[さ/返/]/[せ/返/]/
  ;;         ...
  ;;         ...
  ;;         ながs /長/流/[し/流/]/[さ/長/]/[そ/流/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; skk-auto-okuri-process が non-nil のときに、skk-okuri-search は見出し語の
  ;; 長い順に候補を返す必要がある。
  ;; skk-okuri-search が、見付けた候補を見出し語をキーとして昇順にソートして返
  ;; すため、個人辞書のソートは必要でない。
  ;; よって、最後に変換したものを skk-okuri-ari-min の位置に挿入する。
  ;;
  (let* ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
         (cand (car (skk-treat-strip-note-from-word word)))
         (midasi (if (and (skk-numeric-p)
                          (or (string-match "#[0-9]" cand)
                              (skk-lisp-prog-p cand)))
                     (skk-num-compute-henkan-key skk-henkan-key)
                   skk-henkan-key))
         (henkan-buffer (and skk-update-end-function
                             (current-buffer))))
    ;; 入力履歴を更新する。
    ;; 送りあり入力は省略し、送りなし入力のみ履歴をとる。
    (unless skk-henkan-okurigana
      (skk-update-kakutei-history midasi word))

    (when jisyo-buffer
      (let ((inhibit-quit t)
            buffer-read-only old-words-list okurigana)
        (when (> skk-okuri-index-min -1)
          (setq word (skk-remove-common word)
                ;; skk-henkan-key は skk-remove-common によって
                ;; 変更されている可能性がある。
                midasi skk-henkan-key))
        (setq okurigana (or skk-henkan-okurigana
                            skk-okuri-char))
        (with-current-buffer jisyo-buffer
          ;; 既存エントリを検索後消去する。挿入すべき候補が words1 に 1 つ
          ;; しかなく、word と同じ文字であっても、いったん消してそのエント
          ;; リを min ポイントに移動させなければならない。これは、読みの補
          ;; 完を行うときに、 min ポイントから見出しを探すため、新しい見出
          ;; しほど、min ポイントに近いところになければならないからである。
          (setq skk-henkan-key midasi
                old-words-list (skk-search-jisyo okurigana 0 'delete))
          (skk-update-jisyo-1 okurigana word old-words-list purge)
          ;; 複数の emacs で SKK が起動されているときに個人辞書を整合的に
          ;; 更新するために確定の動作を記録する。
          (when (and (skk-share-private-jisyo-p)
                     (< skk-jisyo-save-count (length skk-jisyo-update-vector)))
            (aset skk-jisyo-update-vector skk-update-jisyo-count
                  (list midasi okurigana word purge)))
          (dolist (function skk-update-end-function)
            (funcall function henkan-buffer midasi okurigana word purge))
          (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
          ;; skk-share-private-jisyo が non-nil のときは skk-jisyo-save-count
          ;; も non-nil であることを前提とする
          (when (and skk-jisyo-save-count
                     (<= skk-jisyo-save-count skk-update-jisyo-count))
            ;; auto save.
            (skk-save-jisyo 'quiet)))))))

(defun skk-update-jisyo-1 (okurigana word old-words-list purge)
  "個人辞書に新しいエントリを挿入する。
既存エントリから計算した words[1-4] の値と、今回の変換の結果 word とを
結合して、新たなエントリを計算し、挿入する。"
  ;; 入力となる OLD-WORDS-LIST は、関数 `skk-search-jisyo' が出力したものである。

  (let ((words1 (car   old-words-list))
        (words2 (nth 1 old-words-list))
        (words3 (nth 2 old-words-list))
        (words4 (nth 3 old-words-list)))
    (cond ((not purge)
           ;; words1 を更新
           (if skk-jisyo-fix-order
               (if (consp words1)
                   ;; 新規の時は、他の同音語の後に追加
                   (unless (member word words1)
                     (setcdr (last words1) (cons word nil)))
                 ;; 今回の読みの語、そのものが新規
                 (setq words1 (cons word nil)))
             ;; words1 の先頭の候補を word にする。
             (setq words1 (cons word (delete word words1)))))

          ;; 送りなし、もしくは skk-henkan-okuri-strictly と
          ;; skk-henkan-strict-okuri-precedence が nil の場合。
          (t
           ;; words1 を purge。共用辞書にある候補だったら、
           ;; skk-ignore-dic-word でクォートして次の変換から出力
           ;; しないようにする。共用辞書にない文字列は word を消す。
           (setq words1 (if (skk-public-jisyo-has-word-p okurigana word)
                            (skk-compose-ignore-word words1 word)
                          (delete word words1)))))

    (when words1 ;; words1 が null であれば、もう何もすることはない。
      (goto-char (if okurigana
                     skk-okuri-ari-min
                   skk-okuri-nasi-min))
      (insert "\n" skk-henkan-key " /")
      ;; words1 -- 全候補群 (送りなしの場合) 、または
      ;;           全候補群の漢字部分 (送りありの場合)
      (insert (skk-update-jisyo-2 words1) "/")

      (when okurigana
        ;; words2 以降の候補群を処理するのは、送りありの場合のみ。
        ;; 先に挿入すべき候補群を計算、調整する。
        (cond (words3
               (cond ((not purge)
                      (if skk-jisyo-fix-order
                          (unless (member word words3)
                            (setcdr (last words3) (cons word nil)))
                        (setq words3 (cons word (delete word words3)))))
                     (t
                      (setq words3 (delete word words3))
                      (when (null words3)
                        ;; words3 として挿入するものが全くなければ、"/[く/]/" のよ
                        ;; うな送り仮名のみの候補を作らないようにする (必要で
                        ;; あれば、words2 の最後方と) words4 の先頭の "]" を削除。
                        (let* ((len (length words2))
                               (last2 (cl-case len
                                        (0 nil)
                                        (1 (list nil (car words2)))
                                        (t (nthcdr (- (length words2) 2)
                                                   words2)))))
                          ;; words2 の最後方は常に "[送り仮名" とは限らない。
                          (when (and last2
                                     (string= (nth 1 last2) (concat "[" okurigana)))
                            (cl-case len
                              (1 (setq words2 nil))
                              (t (setcdr last2 nil))))
                          ;; words4 の先頭は常に "]"。
                          (setq words4 (cdr words4)))))))
              (t
               ;; words3 が null であれば
               (unless (or skk-process-okuri-early
                           purge)
                 ;; skk-process-okuri-early が non-nil なら送り仮名が分らないので
                 ;; 何もしない。-- 今回使用した送り仮名がわからないまま変換してい
                 ;; るので、全ての候補が words2 に入っている -- words3, words4 は
                 ;; null。
                 ;; words3 として挿入するものが全くなければ、何もしない -- words3
                 ;; が purge 前から null なら、words2 の末尾は "[" でないし、
                 ;; words4 は null だから words[234] の操作は不要。
                 (setq words2 (nconc words2
                                     (list (concat "[" okurigana)))
                       words3 (list word)
                       ;; purge 前から words3 が null だったのだから
                       ;; words4 も null。
                       words4 (list "]"))))))
      (when words2
        ;; words2 -- 今回使用しなかった送り仮名を使う漢字の候補群
        ;;         + "["
        ;;         + 今回使用した送り仮名 (送り仮名のみ。その送り
        ;;           仮名を使用する漢字の候補群は、words3 に含まれる)
        (insert (skk-update-jisyo-2 words2) "/")

        ;; words2 が null なら words3 も null。
        (when words3
          ;; words3 -- 今回使用した送り仮名を使う全漢字候補
          (insert (skk-update-jisyo-2 words3) "/"))

        ;; purge で words3 が null になった場合は words4 が残っている
        ;; ときがある。
        (when words4
          ;; words4 -- "]" + 他の送り仮名を使う全漢字候補
          ;; (words2 の残り)。
          (insert (skk-update-jisyo-2 words4) "/"))))))

(defun skk-update-jisyo-2 (words)
  (mapconcat #'skk-quote-char
             words
             "/"))

;;;###autoload
(defun skk-quote-char (word)
  "WORD を辞書エントリとして正しい形に整形する。
辞書形式の制限から、辞書エントリ内に含めてはならない文字が WORD の中にあれば、
評価したときにその文字となるような Lisp コードを返す。"
  (save-match-data
    (cond ((and word
                (string-match "[/\n\r\"]" word)
                ;; we should not quote WORD if it is a symbolic expression
                (not (skk-lisp-prog-p word))
                ;; has an annotation
                (not (string-match ";" word)))
           (format "(concat \"%s\")"
                   (skk-quote-char-1 word (cdr skk-quote-char-alist))))
          (t
           word))))

;;;###autoload
(defun skk-quote-semicolon (word)
  "WORD を辞書エントリとして正しい形に整形する。
`skk-quote-char' と似ているが、註釈と関係ないセミコロン (;) を処理する点が
異なる。"
  ;; `save-match-data' は要らない。
  (cond ((string-match ";" word)
         (format "(concat \"%s\")"
                 (skk-quote-char-1 word skk-quote-char-alist)))
        (t
         word)))

(defun skk-public-jisyo-has-word-p (okurigana word)
  "共有辞書が  WORD を持っているかどうか調べる。
共有辞書が MIDASHI 及びそれに対応する 候補 WORD を持っていれば、 non-nil を返
す。個人辞書のバッファで実行される。"
  (let (fn
        skk-henkan-okuri-strictly
        skk-henkan-strict-okuri-precedence)
    (when okurigana
      (setq skk-henkan-okurigana okurigana))
    ;; skkserv を使う設定になっていたら、skk-server.el をロードする。
    (when (or skk-servers-list
              skk-server-host
              (getenv "SKKSERVER"))
      (require 'skk-server))
    (setq fn (funcall skk-public-jisyo-to-be-searched-function))
    ;;
    (and fn
         (member word (eval fn)))))

(defun skk-public-jisyo-to-be-searched-original ()
  "`skk-search-prog-list' の中から、一番大きな共有辞書を使うプログラムを返す。"
  (let (fn)
    (when (and (featurep 'skk-server)
               (or skk-servers-list
                   skk-server-host))
      (setq fn (assq 'skk-search-server skk-search-prog-list)))
    ;; skk-search-server から始まるリストがなければ、とにかく大きい辞書を引数
    ;; にしている skk-search-jisyo-file プログラムを探す。
    (when (and (not fn)
               (or skk-aux-large-jisyo
                   skk-large-jisyo))
      (let ((spl skk-search-prog-list)
            cell)
        (while (setq cell (car spl))
          (if (and (eq (car cell) 'skk-search-jisyo-file)
                   (memq (nth 1 cell) '(skk-aux-large-jisyo skk-large-jisyo)))
              (setq fn cell
                    spl nil)
            (setq spl (cdr spl))))))
    fn))

(defun skk-compose-ignore-word (words &optional add)
  "無視すべき候補をまとめる。
WORDS の中に `skk-ignore-dic-word' 関数でクォートした候補があれば、一つの候補
にまとめる。
オプショナル引数の ADD が指定されていたら、ADD を含めた `skk-ignore-dic-word'
候補群を作る。
新しい `skk-ignore-dic-word' 候補を car に、それ以外の候補を cdr にしたセル
\(リスト)を返す。"
  (let (l arg e)
    (when add
      (setq words (delete add words)))
    (setq l words)
    (save-match-data
      (while l
        (setq e (car l)
              l (cdr l))
        (when (string-match "(skk-ignore-dic-word +\\([^\)]+\\))"
                            e)
          (setq arg (concat arg
                            (substring e
                                       (1+ (match-beginning 1))
                                       (1- (match-end 1)))
                            "\" \"")
                words (delq e words))))
      (setq arg (cond
                 ((not add)
                  ;; 末尾の " \"" を切り落とす。
                  (substring arg 0 -2))
                 (arg
                  (concat arg
                          (skk-compose-ignore-word-sub-quote-char
                           add)))
                 (t
                  add)))
      (cons (format "(skk-ignore-dic-word \"%s\")"
                    (if (equal arg add)
                        (skk-compose-ignore-word-sub-quote-char arg)
                      arg))
            words))))

(defun skk-compose-ignore-word-sub-quote-char (str)
  "`skk-compose-ignore-word' の下位ルーチン。
SKK 辞書の候補として正しい形に整形する。"
  (cond
   ((string-match "[/\n\r\";]" str)
    (let ((alist (if (string-match ";" str)
                     skk-quote-char-alist
                   (cdr skk-quote-char-alist))))
      (skk-quote-char-1 str alist)))
   (t
    str)))

(defun skk-search-katakana-maybe ()
  (when skk-search-katakana
    (skk-search-katakana (eq skk-search-katakana 'jisx0201-kana))))

(defun skk-search-katakana (&optional jisx0201-kana)
  "見出し語をカタカナにして、リストにして返す。
これは `skk-search-prog-list' に追加されるべき機能で、変換キーを単純にカ
タカナに変換したものを候補として返す。
一般的な FEP は単純にカタカナに変換したものが候補に現れるものが多いが、
そのような挙動が好みの場合にはこの関数を用いるとよい。"
  (unless (or skk-henkan-okurigana
              (string-match ">$" skk-henkan-key) ; 接頭辞
              (string-match "^>" skk-henkan-key)) ; 接尾辞
    (let ((key skk-henkan-key)
          char words)
      (with-temp-buffer
        (insert key)
        ;; 接頭辞・接尾辞の入力だったら ">" を消しておく。
        (goto-char (1- (point)))    ;
        (when (looking-at ">")      ;(looking-back ">")
          (delete-char 1))
        (goto-char (point-min))
        (when (looking-at ">")
          (delete-char 1))
        ;;
        (while (and (not (eobp))
                    ;; "ー" では文字種が判別できないので point を進める。
                    (or (looking-at "ー")
                        (eq 'unknown (setq char (skk-what-char-type)))))
          (forward-char 1))
        (when (eq char 'hiragana)
          (skk-katakana-region (point-min) (point-max) t)
          (setq words (list (buffer-string))))
        (when (and jisx0201-kana
                   (or (eq char 'hiragana)
                       (string-match "ー" key)))
          (skk-katakana-to-jisx0201-region (point-min) (point-max))
          (setq words (nconc words (list (buffer-string))))))
      words)))

(defun skk-search-hankaku-katakana ()
  "見出し語を半角カナにして、リストにして返す。"
  (cdr (skk-search-katakana t)))

(defun skk-search-romaji (&optional jisx0208)
  "変換キーをローマ字に変換した候補を返す。"
  (cond ((executable-find "kakasi")
         (unless (or skk-henkan-okurigana
                     (string-match ">$" skk-henkan-key) ; 接頭辞
                     (string-match "^>" skk-henkan-key)) ; 接尾辞
           (let ((key skk-henkan-key)
                 words chars)
             (with-temp-buffer
               (insert key)
               ;; 接頭辞・接尾辞の入力だったら ">" を消しておく。
               (goto-char (1- (point)))
               (when (looking-at ">")
                 (delete-char 1))
               (goto-char (point-min))
               (when (looking-at ">")
                 (delete-char 1))
               ;;
               (while (not (eobp))
                 (add-to-list 'chars (skk-what-char-type))
                 (forward-char 1))
               (when (memq 'hiragana chars)
                 (skk-romaji-region (point-min) (point-max))
                 (setq words (list (buffer-string))))
               (when (and jisx0208 words)
                 (skk-jisx0208-latin-region (point-min) (point-max))
                 (setq words (nconc words (list (buffer-string))))))
             words)))

        (skk-insert-keysequence
         (list skk-insert-keysequence))))

(defun skk-search-jisx0208-romaji ()
  "見出し語を全角ローマ字に変換して、リストにして返す。"
  (cdr (skk-search-romaji t)))

(defun skk-search-upcase (&optional initial)
  "変換キーの ascii 小文字を大文字に変換した候補を返す。
この関数は `skk-abbrev-mode' 専用。"
  (if skk-abbrev-mode
      (list (if initial
                (upcase-initials skk-henkan-key)
              (upcase skk-henkan-key)))
    nil))

(defun skk-search-identity (&rest args)
  "変換キーをそのまま候補として返す。
この関数は送りなし変換専用。"
  (if skk-henkan-okurigana
      nil
    (list (identity skk-henkan-key))))

(defun skk-search-function-usage ()
  "Emacs Lisp 関数の usage を返す。"
  (unless skk-henkan-okurigana
    (let* ((symbol (intern (format "%s" skk-henkan-key)))
           def doc usage arglist result)
      (when (fboundp symbol)
        (setq def (symbol-function symbol)
              doc (documentation symbol)
              usage (help-split-fundoc doc symbol)
              arglist (help-function-arglist symbol))
        (cond (usage
               (setq result (car usage)))

              ((listp arglist)
               (setq result (format "%S" (skk-help-make-usage symbol arglist))))

              ((stringp arglist)
               (setq result arglist))

              ((let ((fun symbol))
                 (while (and (symbolp fun)
                             (setq fun (symbol-function fun))
                             (not (setq usage (help-split-fundoc
                                               (documentation fun)
                                               symbol)))))
                 usage)
               (setq result (car usage)))

              ((or (stringp def)
                   (vectorp def))
               (setq result (format "\nMacro: %s" (format-kbd-macro def)))))

        (when result
          (list (format "(quote %s)" result)))))))

;;;###autoload
(defun skk-search-progs (key &optional prog-list remove-note)
  ;; prog-list が省略された時は skk-search-prog-list の全てが対象
  ;; もし引数をさらに追加するような事があれば
  ;;   okuri-nasi   送り有りのチェックをパス
  ;;   allow-duplicate  skk-nunion でなく nconc を使う
  ;; あたりか
  (save-match-data
    (let ((skk-henkan-key key)
          skk-henkan-okurigana
          skk-okuri-char
          skk-auto-okuri-process
          words)
      ;; 混ぜ書きな人って送り有り変換するのかな  \cj のほうがいい?
      (when (string-match "[ぁ-ん][a-z]$" key)
        (setq skk-henkan-okurigana ""
              skk-okuri-char (substring key (1- (length key)))))
      (ignore-errors
        (dolist (form (or prog-list
                          skk-search-prog-list))
          (dolist (word (eval form))
            (when (and remove-note
                       (string-match ";" word))
              (setq word (substring word 0 (match-beginning 0))))
            (setq words (skk-nunion words (list word))))))
      words)))

(defun skk-search-sagyo-henkaku-maybe ()
  (when skk-search-sagyo-henkaku
    (skk-search-sagyo-henkaku nil (eq skk-search-sagyo-henkaku 'anything))))

(defun skk-search-sagyo-henkaku (&optional okuri-list anything)
  "見出し語をサ行変格活用の動詞とみなして、送りあり候補を検索する。"
  (unless okuri-list
    (setq okuri-list '("さ" "し" "す" "せ")))
  (when (and skk-henkan-okurigana
             (or (member skk-henkan-okurigana okuri-list)
                 anything))
    (skk-search-progs (substring skk-henkan-key
                                 0
                                 (1- (length skk-henkan-key))))))

(defun skk-search-ja-dic-maybe (&optional check)
  ;; `skk-search-prog-list' の一要素として作用するほか、
  ;; skk-mode に入るたび check で評価される。
  (unless (or (and (stringp skk-large-jisyo)
                   (file-readable-p skk-large-jisyo))
              (and (stringp skk-aux-large-jisyo)
                   (file-readable-p skk-aux-large-jisyo))
              (and (stringp skk-cdb-large-jisyo)
                   (file-readable-p skk-cdb-large-jisyo))
              skk-server-host
              skk-inhibit-ja-dic-search)
    (if check
        (skk-message "辞書として leim/ja-dic を使います"
                     "Use leim/ja-dic as dictionary")
      (skk-search-ja-dic))))

(defun skk-search-with-suffix ()
  (unless (or skk-henkan-okurigana
              (get 'skk-search-with-suffix 'active))
    (let ((i 1)
          (len (length skk-henkan-key))
          key suf-key words suffixes list)
      (put 'skk-search-with-suffix 'active t)
      (while (< i len)
        (setq key (substring skk-henkan-key 0 i)
              suf-key (substring skk-henkan-key i))
        (setq words (skk-search-progs key nil 'remove)
              suffixes (skk-search-progs (concat ">" suf-key) nil 'remove))
        (when (and words suffixes)
          (dolist (word words)
            (dolist (suffix suffixes)
              (setq list (nconc list (list (concat word suffix)))))))
        (setq i (1+ i)))
      (put 'skk-search-with-suffix 'active nil)
      list)))

(defun skk-katakana-region (start end &optional vcontract)
  "領域のひらがなをカタカナに変換する。
オプショナル引数の VCONTRACT が non-nil であれば、\"う゛\" を \"ヴ\" に変換す
る。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (when vcontract
    (skk-search-and-replace start end "う゛"
                            (lambda (matched)
                              nil "ヴ")))
  (skk-search-and-replace start end "[ぁ-ん]+"
                          (lambda (matched)
                            (skk-hiragana-to-katakana matched))))

(defun skk-hiragana-region (start end &optional vexpand)
  "領域のカタカナをひらがなに変換する。
オプショナル引数の VEXPAND が non-nil であれば、\"ヴ\" を \"う゛\" に変換する。
引数の START と END は数字でもマーカーでも良い。
\"ヵ\" と \"ヶ\" は変更されない。この 2 つの文字は対応するひらがながないので、
カタカナとしては扱われない。"
  (interactive "*r\nP")
  (when vexpand
    (skk-search-and-replace start end "ヴ"
                            (lambda (matched)
                              nil "う゛")))
  (skk-search-and-replace start end "[ァ-ン]+"
                          (lambda (matched)
                            (skk-katakana-to-hiragana matched))))

(defun skk-jisx0208-latin-region (start end)
  "領域の ascii 文字を対応する全角英文字に変換する。"
  (interactive "*r")
  (skk-search-and-replace start end "[ -~]"
                          (lambda (matched)
                            (aref skk-default-jisx0208-latin-vector
                                  (string-to-char matched)))))

(defun skk-latin-region (start end)
  "領域の全角英文字を対応する ascii 文字に変換する。"
  (interactive "*r")
  (skk-search-and-replace start end "\\cj"
                          (lambda (matched)
                            (or (skk-jisx0208-to-ascii matched)
                                matched))))

;;;###autoload
(defun skk-search-and-replace (start end regexp func)
  (let (matched replace)
    (save-match-data
      (skk-save-point
       ;; END may be changed when length of MATCHED and one of REPLACE
       ;; are different.
       (setq end (set-marker (make-marker) end))
       (goto-char start)
       (while (re-search-forward regexp end 'noerror)
         (let ((beg0 (match-beginning 0))
               (end0 (match-end 0)))
           (setq matched (buffer-substring-no-properties beg0 end0)
                 replace (funcall func matched))
           (goto-char beg0)
           ;; firstly insert a new string, secondly delete an old string to save
           ;; the cursor position.
           (insert-and-inherit replace)
           (delete-region (+ beg0 (length replace))
                          (+ end0 (length replace)))))
       (set-marker end nil)))))

;;;###autoload
(defun skk-jisx0208-to-ascii (string)
  (require 'japan-util)
  (let ((char (get-char-code-property (string-to-char string)
                                      'ascii)))
    (if char
        (char-to-string char)
      nil)))

;;;###autoload
(defun skk-henkan-skk-region-by-func (func &optional arg)
  "`skk-henkan-start-point' と `skk-henkan-end-point' の間の文字列を変換する。
変換可能かどうかのチェックをした後に ARG を引数として FUNC を適用し、
`skk-henkan-start-point' と `skk-henkan-end-point' の間の文字列を変換する。"
  (skk-with-point-move
   (cl-case skk-henkan-mode
     (active                ;▼モード
      nil)
     (on                ;▽モード
      (skk-set-marker skk-henkan-end-point (point))
      (when (and (> skk-kakutei-history-limit 0)
                 (< skk-henkan-start-point (point))
                 (skk-save-point
                  (goto-char skk-henkan-start-point)
                  (eq (skk-what-char-type) 'hiragana)))
        (skk-update-kakutei-history
         (buffer-substring-no-properties
          skk-henkan-start-point (point))))
      ;; 変換可能かどうかの最終チェック
      (when (skk-get-prefix skk-current-rule-tree)
        (skk-error "入力途中の仮名プレフィックスがあります"
                   "There remains a kana prefix"))

      (when (< (point) skk-henkan-start-point)
        (skk-error "カーソルが変換開始地点より前にあります"
                   "Henkan end point must be after henkan start point"))

      (when (and (not skk-allow-spaces-newlines-and-tabs)
                 (skk-save-point
                  (beginning-of-line)
                  (> (point) skk-henkan-start-point)))
        (skk-error "変換キーに改行が含まれています"
                   "Henkan key may not contain a line feed"))
      ;;
      (apply func skk-henkan-start-point skk-henkan-end-point
             (if arg (list arg) nil))
      (skk-kakutei))
     (t
      (skk-emulate-original-map arg)))))

;;;###autoload
(defun skk-hiragana-to-katakana (hiragana)
  (let ((diff (- ?ア ?あ)))
    (mapconcat (lambda (e)
                 (if (and (<= ?ぁ e) (>= ?ん e))
                     (char-to-string (+ e diff))
                   (char-to-string e)))
               (string-to-int-list hiragana) "")))

;;;###autoload
(defun skk-katakana-to-hiragana (katakana)
  (let ((diff (- ?ア ?あ)))
    (mapconcat (lambda (e)
                 (if (and (<= ?ァ e) (>= ?ン e))
                     (char-to-string (- e diff))
                   (char-to-string e)))
               (string-to-int-list katakana) "")))

;;;###autoload
(defun skk-henkan-face-on (&optional face)
  "SKK の face 属性を ON にする。
`skk-use-face' が non-nil の場合、`skk-henkan-start-point' と
`skk-henkan-end-point' の間の face 属性を `skk-henkan-face' の値に変更する。"
  ;; SKK 9.4 より Text Properties を使用するのを止めて、Overlays を使用するよ
  ;; うにした (egg.el, canna.el, wnn-egg.el を参考にした)。
  ;; Overlays は、テキストの一部ではないので、バッファから文字を切り出してもコ
  ;; ピーの対象にならないし、アンドゥ時も無視されるので、変換された候補の表示
  ;; を一時的に変更するには Text Properties よりも好都合である。
  (unless face
    (setq face skk-henkan-face))
  (when (and face
             (marker-position skk-henkan-start-point)
             (marker-position skk-henkan-end-point))
    (setq skk-henkan-overlay nil)
    (skk-face-on skk-henkan-overlay
                 skk-henkan-start-point skk-henkan-end-point
                 face skk-henkan-overlay-priority)))

;;;###autoload
(defun skk-henkan-face-off ()
  "SKK の face 属性を OFF にする。
`skk-henkan-start-point' と `skk-henkan-end-point' の間の表示を変更している
`skk-henkan-overlay' を消す。"
  (when skk-henkan-face
    (skk-detach-extent skk-henkan-overlay)))

(defun skk-make-face (face)
  "新しい FACE を作成する。
FACE は「前景色」又は「前景色 + スラッシュ + 背景色」の形式で指定する。"
  ;; hilit-lookup-face-create のサブセット。tutorial で色付けを行う場合でも
  ;; hilit19 に依存せずとりあえず face を自前で作ることができるように、という
  ;; 目的で作ったもので、簡単な色付けしかできない。あまり賢くはない。複雑な
  ;; face を作りたい人は hilit-lookup-face-create 等を使って下さい。
  (unless (car (memq face (face-list)))
    (save-match-data
      (let* ((list (split-string (symbol-name face) "/"))
             (bg (nth 1 list)))
        (setq face (make-face face))
        (set-face-foreground face (car list))
        (when bg
          (set-face-background face bg)))))
  face)

;; skk-auto.el, skk-rdbms.el の両方で使うので、skk-auto.el より移動した。
(defun skk-remove-common (word)
  "WORD の送り仮名以外の部分を返す。
`skk-henkan-key' と WORD の間に共通の送り仮名を取り除き、送り仮名以外の部分
の文字列を返す。`skk-henkan-key' と `skk-henkan-okurigana' の値をセットする。
例えば、WORD == 持ってきた であれば、`skk-henkan-key' := もt ,
`skk-henkan-okurigana' := って , WORD := 持 のように分解し、WORD を返す。
`skk-auto-okuri-process' の値が non-nil であるときにこの関数を使用する。
変換が行われたバッファでコールされる (辞書バッファではない)。"
  (when (and (not (skk-numeric-p))
             (not skk-abbrev-mode)
             (or skk-henkan-in-minibuff-flag
                 (and (<= skk-okuri-index-min skk-henkan-count)
                      (<= skk-henkan-count skk-okuri-index-max))))
    (let ((midasi skk-henkan-key)
          (midasi-len (length skk-henkan-key))
          (word-len (length word))
          (cont t)
          char pos pos2
          midasi-tail word-tail new-word okuri-first
          new-skk-okuri-char new-skk-henkan-key)
      (when (and (>= midasi-len 2) (>= word-len 2))
        ;; check if both midasi and word end with the same ascii char.
        (when (and (skk-ascii-char-p (aref midasi (1- midasi-len)))
                   (eq (aref midasi (1- midasi-len))
                       (aref word (1- word-len))))
          ;; if so chop off the char from midasi and word.
          ;; assume size of an ASCII char is always 1.
          (setq midasi (substring midasi 0 -1)
                midasi-len (1- midasi-len)
                word (substring word 0 -1)
                word-len (1- word-len)))
        (setq midasi-tail (substring midasi (1- midasi-len)
                                     midasi-len)
              word-tail (substring word (1- word-len)
                                   word-len))
        (when (and (string= midasi-tail word-tail)
                   (or (and (skk-string<= "ぁ" midasi-tail)
                            (skk-string<= midasi-tail "ん"))
                       (member midasi-tail '("、" "。" "，" "．"))))
          ;; 見出し語と単語との末尾が同一のかな文字の場合。
          ;; 送りなしを送りありへ
          (setq pos (1- word-len)
                new-word new-skk-henkan-key)
          (while (and cont (> pos 0))
            (setq char (substring word (1- pos) pos))
            (if (and (skk-string<= "亜" char)
                     (skk-string<= char "瑤"))
                ;; char is the right-most Kanji
                (setq cont nil)
              (setq pos (1- pos))))
          (setq pos2 (- midasi-len (- word-len pos)))
          ;; check if midasi and word has the same tail of length
          (when (string= (substring midasi pos2 midasi-len)
                         (substring word pos word-len))
            (setq okuri-first (substring word pos (1+ pos)))
            (setq skk-henkan-okurigana
                  (if (and (string= okuri-first "っ")
                           (<= (+ pos 2) word-len))
                      ;; in this case okuriga consits of two
                      ;; characters, e.g., 「残った」
                      (substring word pos (+ pos 2))
                    okuri-first))
            (setq new-word (substring word 0 pos)
                  new-skk-okuri-char (skk-okurigana-prefix
                                      skk-henkan-okurigana)
                  new-skk-henkan-key (concat
                                      (substring midasi 0 pos2)
                                      new-skk-okuri-char))
            (let (inhibit-quit) ; allow keyboard quit
              (cond
               ((not skk-henkan-in-minibuff-flag)
                (setq word new-word
                      skk-henkan-key new-skk-henkan-key))
               ;; 辞書登録モードで登録された場合。
               ;; ask if register as okuri-ari word.
               ((y-or-n-p
                 (format
                  (if skk-japanese-message-and-error
                      "%s /%s/ を送りあり候補として登録しますか？ "
                    "Shall I register this as okuri-ari word: %s /%s/ ? ")
                  new-skk-henkan-key new-word))
                (setq word new-word
                      skk-okuri-char new-skk-okuri-char
                      skk-henkan-key new-skk-henkan-key))
               (t
                (setq skk-henkan-okurigana nil
                      skk-okuri-char nil)
                (message "")))))))))
  ;; 分解した word (送り仮名部分を除いたもの) を返す。
  word)

(defun skk-okurigana-prefix (okurigana)
  (let ((headchar (substring okurigana 0 1)))
    (cond ((string= headchar "ん")
           "n")
          ((not (and (skk-string<= "ぁ" headchar)
                     (skk-string<= headchar "ん")))
           nil)
          ((and (string= headchar "っ")
                (not (string= okurigana "っ")))
           (aref skk-kana-rom-vector
                 ;; assume the character is hiragana of JIS X 0208.
                 (- (skk-char-octet
                     (string-to-char (substring okurigana 1 2))
                     1)
                    33)))
          (t
           (aref skk-kana-rom-vector
                 (- (skk-char-octet (string-to-char headchar)
                                    1)
                    33))))))

(defun skk-update-kakutei-history (midasi &optional word)
  "変数 `skk-kakutei-history' を更新する。
この履歴は skk-comp.el において利用される。"
  (cond
   ((<= skk-kakutei-history-limit 0)
    (setq skk-kakutei-history nil))
   (t
    (setq skk-kakutei-history (cons (list midasi word (current-buffer))
                                    skk-kakutei-history))
    (when (> (length skk-kakutei-history)
             skk-kakutei-history-limit)
      (setcdr (nthcdr (1- skk-kakutei-history-limit)
                      skk-kakutei-history)
              nil)))))

;;; functions for hooks.
(defun skk-after-point-move ()
  (when (and (not (and skk-previous-point
                       (= skk-previous-point (point))))
             (skk-get-prefix skk-current-rule-tree))
    (skk-with-point-move
     (skk-erase-prefix 'clean))))

(defun skk-pre-command ()
  (when (and (memq last-command '(skk-insert skk-previous-candidate))
             (null (memq this-command skk-kana-cleanup-command-list)))
    (skk-kana-cleanup t)))

;;;###autoload
(defun skk-remove-minibuffer-setup-hook (&rest args)
  ;; Remove all args from minibuffer-setup-hook.
  (dolist (hook args)
    (remove-hook 'minibuffer-setup-hook hook)))

(defun skk-add-skk-pre-command ()
  (add-hook 'pre-command-hook 'skk-pre-command nil 'local))

(defun skk-remove-skk-pre-command ()
  (remove-hook 'pre-command-hook 'skk-pre-command 'local))

(add-hook 'edit-picture-hook #'skk-misc-for-picture 'append)
(add-hook 'kill-emacs-hook #'skk-record-jisyo-data)
;; add 'skk-save-jisyo only to remove easily.
(add-hook 'kill-emacs-hook #'skk-save-jisyo)
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (skk-remove-skk-pre-command)
            (skk-remove-minibuffer-setup-hook 'skk-j-mode-on
                                              'skk-setup-minibuffer
                                              'skk-add-skk-pre-command)
            (skk-exit-henkan-in-minibuff)))

;;;###autoload
(defun skk-preload ()
  "Emacs 起動時にあらかじめ SKK を呼ぶことで SKK の応答を速くする。
先読みの対象になるのは以下。
1. skk.el と関連するいくつかのファイル (初回起動時の遅延を緩和)
2. 共有辞書群 (初変換時の遅延を緩和)
変数 `skk-preload' が non-nil のとき `after-init-hook' から呼ばれる。
そのため Emacs の起動時間が長くなる点には注意を要する。"
  (with-temp-buffer
    (skk-mode 1))
  (dolist (item skk-search-prog-list)
    (when (eq (car item) 'skk-search-jisyo-file)
      (skk-preload-jisyo (cadr item))))
  (dolist (item skk-extra-jisyo-file-list)
    (skk-preload-jisyo item))
  (when skk-annotation-lookup-DictionaryServices
    (skk-annotation-start-python)))

(defun skk-preload-jisyo (jisyo)
  (cond
   ((eq jisyo 'skk-jisyo)
    (setq jisyo nil))
   ;;
   ((symbolp jisyo)
    (setq jisyo (symbol-value jisyo))
    (unless (and (stringp jisyo)
                 (file-readable-p jisyo))
      (setq jisyo nil)))
   ;;
   ((and (listp jisyo)
         (memq (car jisyo) '(cons quote)))
    (setq jisyo (ignore-errors (eval jisyo)))
    (unless (and (consp jisyo)
                 (stringp (car jisyo))
                 (file-readable-p (car jisyo)))
      (setq jisyo nil))))
  (when jisyo
    (skk-get-jisyo-buffer jisyo 'nomsg)))

(defun skk-toggle-isearch-mode (&optional arg)
  "skk-isearch を利用するかどうかをトグルで変更する。
変数 `skk-isearch-mode-enable' の値を nil/t トグルで変更する。
変数 `migemo-isearch-enable-p' が bound されていれば、
`skk-isearch-mode-enable' の値と逆の値をセットする。"
  (interactive)
  (setq skk-isearch-mode-enable
        (cond ((null arg)
               (not skk-isearch-mode-enable))
              ((> (prefix-numeric-value arg) 0)
               t)))
  (when (boundp 'migemo-isearch-enable-p)
    (if skk-isearch-mode-enable
        (setq migemo-isearch-enable-p nil)
      (setq migemo-isearch-enable-p t)))
  (message "SKK isearch is %s" (if skk-isearch-mode-enable
                                   "enabled"
                                 "disabled")))

(defun skk-henkan-inactivate ()
  "▼モードをキャンセルして▽モードに戻る。文字列は維持する。"
  (setq skk-henkan-count 0)
  (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
      (let ((count (length skk-henkan-okurigana)))
        (skk-previous-candidate nil)
        ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
        (delete-char (- 0 count)))
    (skk-previous-candidate nil)))

(defun skk-henkan-off-by-quit ()
  "▽モードをキャンセルして■モードに戻る。文字列は破棄する。"
  (if (memq 'skk-comp-do (list last-command this-command))
      (skk-with-point-move
       (delete-region skk-henkan-start-point (point))
       (insert skk-comp-key)
       (when skk-verbose
         (setq last-command 'keyboard-quit))
       (setq this-command 'keyboard-quit))
    (skk-erase-prefix 'clean)
    (delete-region skk-henkan-start-point
                   (if (> (point) skk-henkan-start-point)
                       (point)
                     skk-previous-point))
    (skk-kakutei)))

;;; cover to original functions.
(skk-defadvice keyboard-quit (around skk-ad activate preactivate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ `keyboard-quit' と同じ動作をする。"
  (if (not skk-mode)
      ad-do-it
    (cond
     ((eq skk-henkan-mode 'active)
      (skk-henkan-inactivate))
     ((eq skk-henkan-mode 'on)
      (skk-henkan-off-by-quit))
     (t
      (if (skk-get-prefix skk-current-rule-tree)
          (skk-erase-prefix 'clean)
        ad-do-it)))))

(skk-defadvice abort-recursive-edit (around skk-ad activate preactivate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ `abort-recursive-edit' と同じ動作をする。"
  ;; subr command but no arg.
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer 'skk-add-skk-pre-command)
  (if (not skk-mode)
      ad-do-it
    (cond
     ((eq skk-henkan-mode 'active)
      (skk-henkan-inactivate))
     ((eq skk-henkan-mode 'on)
      (skk-henkan-off-by-quit))
     (t
      (if (skk-get-prefix skk-current-rule-tree)
          (skk-erase-prefix 'clean)
        ad-do-it)))))

(defadvice newline (around skk-ad activate)
  "`skk-egg-like-newline' が non-nil であれば、確定のみ行い、改行しない。"
  (if (not (or skk-j-mode
               skk-jisx0201-mode
               skk-abbrev-mode))
      ad-do-it
    (let (;;(arg (ad-get-arg 0))
          ;; `skk-kakutei' を実行すると `skk-henkan-mode' の値が
          ;; 無条件に nil になるので、保存しておく必要がある。
          (no-newline (and skk-egg-like-newline
                           skk-henkan-mode))
          (auto-fill-function (if (called-interactively-p 'interactive)
                                  auto-fill-function
                                nil)))
      ;; fill されても nil が帰ってくる :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)))
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) によって行が折り返されたら
      ;;      ;; arg を 1 つ減らす。
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)))))
      (when skk-mode
        (skk-kakutei))
      (undo-boundary)
      (unless no-newline
        ad-do-it))))

(defadvice newline-and-indent (around skk-ad activate)
  "`skk-egg-like-newline' が non-nil であれば、確定のみ行い、改行しない。"
  (if (not (or skk-j-mode
               skk-jisx0201-mode
               skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline
                           skk-henkan-mode))
          (auto-fill-function (if (called-interactively-p 'interactive)
                                  auto-fill-function
                                nil)))
      (when skk-mode
        (skk-kakutei))
      (undo-boundary)
      (unless no-newline
        ad-do-it))))

(skk-defadvice exit-minibuffer (around skk-ad activate)
  ;; subr command but no arg.
  "`skk-egg-like-newline' が non-nil であれば、確定のみ行い、改行しない。"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer 'skk-add-skk-pre-command)
  (if (not (or skk-j-mode
               skk-jisx0201-mode
               skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline
                           skk-henkan-mode)))
      (when skk-mode
        (skk-kakutei))
      (unless no-newline
        ad-do-it))))

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK のバッファローカル変数を無効にし、`picture-mode-exit' をコールする。
`picture-mode' から出たときにそのバッファで SKK を正常に動かすための処理。"
  (when skk-mode
    (skk-kill-local-variables)))

(defadvice undo (before skk-ad activate)
  "SKK モードが on なら `skk-self-insert-non-undo-count' を初期化する。"
  (when skk-mode
    (setq skk-self-insert-non-undo-count 0)))

(defadvice next-line (before skk-ad activate)
  (when (eq skk-henkan-mode 'active)
    (skk-kakutei)))

(defadvice previous-line (before skk-ad activate)
  (when (eq skk-henkan-mode 'active)
    (skk-kakutei)))

(defadvice backward-kill-sentence (before skk-ad activate)
  ;; C-x DEL
  ;; どのような動作をするべきか未決定
  (when skk-mode
    (skk-kakutei)))

(defmacro skk-wrap-newline-command (cmd)
  "[return]キーに割り当てられているであろうコマンド (CMD) をラップして、
skk の動作と整合させる。
 [return]キーにコマンドを割り当てているメジャーモードで skk を使うと、skk が
`skk-kakutei' を呼び出す機会がないために変換を確定できず `▼' がバッファに
残ってしまうという問題がある。

本マクロを用いると、変換を確定してから (`skk-kakutei' を実行してから) CMD 本
体を実行するように CMD をラップする。"
  `(defadvice ,cmd (around skk-ad activate compile)
     (cond (skk-henkan-mode
            (skk-kakutei)
            (unless skk-egg-like-newline
              ad-do-it))
           (t
            ad-do-it))))

(skk-wrap-newline-command comint-send-input)
(skk-wrap-newline-command ielm-return)
(skk-wrap-newline-command rcirc-send-input)
(skk-wrap-newline-command widget-field-activate)
(skk-wrap-newline-command org-insert-heading)
(skk-wrap-newline-command org-return)

;; hooks.

;;;###autoload
(add-hook 'after-init-hook
          (lambda ()
            (when (and (symbol-value 'init-file-user)
                       skk-preload)
              (skk-preload)
              (message "SKK preload...done"))
            (when window-system
              (ccc-setup)))
          t)

(add-hook 'kill-buffer-hook
          ;; SKK の▼モードだったら、確定してからバッファをキルする。
          (lambda ()
            (when (and skk-mode
                       skk-henkan-mode)
              (skk-kakutei))))

(run-hooks 'skk-load-hook)

(provide 'skk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk.el ends here
