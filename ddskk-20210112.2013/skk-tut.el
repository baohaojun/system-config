;;; skk-tut.el --- SKK tutorial -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000, 2001, 2002
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999-2010  SKK Development Team

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
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

;;; Code:

(require 'skk)

(eval-and-compile
  (autoload 'skk-nicola-setup-tutorial "skk-nicola"))

(eval-when-compile
  (defvar skk-kanagaki-keyboard-type)
  (defvar skk-kanagaki-state))

;; internal variables and constants.
;; prefix should be `skktut-'.
(defvar skk-tut-section-face 'skk-tut-section-face)
(defvar skk-tut-do-it-face 'skk-tut-do-it-face)
(defvar skk-tut-question-face 'skk-tut-question-face)
(defvar skk-tut-key-bind-face 'skk-tut-key-bind-face)
(defvar skk-tut-hint-face 'skk-tut-hint-face)

(defconst skktut-adviced-alist
  '((skk-abbrev-mode . before)
    (skk-insert . before)
    (skk-kakutei . before)
    (skk-mode . before)
    (skk-create-file . around)
    (skk-save-jisyo-original . around)
    (skk-get-jisyo-buffer . around))
  "SKK チュートリアルで advice が付けられる関数と advice class のエーリスト。")

(defvar skktut-question-numbers nil "SKK チュートリアルの問題数。")

(defconst skktut-tut-jisyo "~/skk-tut-jisyo"
  "SKK チュートリアル用の辞書。")

(defconst skktut-init-variables-alist
  '((skk-cursor-abbrev-color . "royalblue")
    (skk-abbrev-mode-string . " aあ")
    (skk-allow-spaces-newlines-and-tabs . t)
    (skk-auto-fill-mode-hook . nil)
    (skk-auto-insert-paren . nil)
    (skk-auto-okuri-process . nil)
    (skk-auto-start-henkan . nil)
    (skk-byte-compile-init-file . nil)
    (skk-comp-load-hook . nil)
    (skk-compare-jisyo-size-when-saving . nil)
    (skk-count-jisyo-candidates-function
     . 'skk-count-jisyo-candidates-original)
    (skk-count-private-jisyo-candidates-exactly . nil)
    (skk-date-ad . 1)
    (skk-cursor-default-color . (cdr (assq 'cursor-color
                                           (frame-parameters (selected-frame)))))
    (skk-delete-implies-kakutei . t)
    (skk-delete-okuri-when-quit . nil)
    (skk-downcase-alist . nil)
    (skk-echo . t)
    (skk-egg-like-newline . nil)
    (skk-gadget-load-hook . nil)
    (skk-henkan-face . 'highlight)
    (skk-henkan-okuri-strictly . nil)
    (skk-henkan-overlay-priority . 600)
    (skk-henkan-show-candidates-keys . '(?a ?s ?d ?f ?j ?k ?l))
    (skk-henkan-number-to-display-candidates . 7)
    (skk-show-candidates-always-pop-to-buffer . nil)
    (skk-henkan-strict-okuri-precedence . nil)
    (skk-previous-candidate-keys
     . (if (skk-tut-nicola-p)
           '("\C-p")
         '("\C-p" "x")))
    (skk-hiragana-mode-string . " かな")
    (skk-init-file . "")
    (skk-input-by-code-menu-keys1 . '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y))
    (skk-input-by-code-menu-keys2
     . '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u))
    (skk-japanese-message-and-error . nil)
    (skk-cursor-jisx0208-latin-color . "gold")
    (skk-jisx0208-latin-mode-string . " 全英")
    (skk-jisx0208-latin-vector . skk-default-jisx0208-latin-vector)
    (skk-jisyo . "~/skk-tut-jisyo")
    (skk-jisyo-save-count . nil)
    (skk-update-jisyo-count . 0)
    (skk-kakutei-early . t)
    (skk-kakutei-key . "\C-j")
    (skk-kana-input-search-function
     . (lambda ()
         (save-match-data
           (when (and (string-match
                       "^h\\([bcdfghjklmnpqrstvwxz]\\)$"
                       skk-prefix)
                      (member (char-to-string (preceding-char))
                              '("お" "オ")))
             (cons '("オ" . "お") (match-string 1 skk-prefix))))))
    (skk-katakana-mode-string . " カナ")
    (skk-kcode-load-hook . nil)
    (skk-keep-record . nil)
    (skk-kuten-touten-alist . '((jp . ("。" . "、"))))
    (skk-kutouten-type . 'jp)
    (skk-cursor-latin-color . "ivory4")
    (skk-latin-mode-string . " SKK")
    (skk-load-hook . nil)
    (skk-mode-hook . nil)
    (skk-next-completion-char . ?.)
    (skk-number-style . 1)
    (skk-okuri-char-alist . nil)
    (skk-previous-completion-char . ?,)
    (skk-process-okuri-early . nil)
    (skk-public-jisyo-has-entry-p-function
     . 'skk-public-jisyo-has-entry-p-original)
    (skk-rom-kana-base-rule-list
     .
     '(("a" nil ("ア" . "あ")) ("bb" "b" ("ッ" . "っ"))
       ("ba" nil ("バ" . "ば")) ("be" nil ("ベ" . "べ"))
       ("bi" nil ("ビ" . "び")) ("bo" nil ("ボ" . "ぼ"))
       ("bu" nil ("ブ" . "ぶ")) ("bya" nil ("ビャ" . "びゃ"))
       ("bye" nil ("ビェ" . "びぇ")) ("byi" nil ("ビィ" . "びぃ"))
       ("byo" nil ("ビョ" . "びょ")) ("byu" nil ("ビュ" . "びゅ"))
       ("cc" "c" ("ッ" . "っ")) ("cha" nil ("チャ" . "ちゃ"))
       ("che" nil ("チェ" . "ちぇ")) ("chi" nil ("チ" . "ち"))
       ("cho" nil ("チョ" . "ちょ")) ("chu" nil ("チュ" . "ちゅ"))
       ("cya" nil ("チャ" . "ちゃ")) ("cye" nil ("チェ" . "ちぇ"))
       ("cyi" nil ("チィ" . "ちぃ")) ("cyo" nil ("チョ" . "ちょ"))
       ("cyu" nil ("チュ" . "ちゅ")) ("dd" "d" ("ッ" . "っ"))
       ("da" nil ("ダ" . "だ")) ("de" nil ("デ" . "で"))
       ("dha" nil ("デャ" . "でゃ")) ("dhe" nil ("デェ" . "でぇ"))
       ("dhi" nil ("ディ" . "でぃ")) ("dho" nil ("デョ" . "でょ"))
       ("dhu" nil ("デュ" . "でゅ")) ("di" nil ("ヂ" . "ぢ"))
       ("do" nil ("ド" . "ど")) ("du" nil ("ヅ" . "づ"))
       ("dya" nil ("ヂャ" . "ぢゃ")) ("dye" nil ("ヂェ" . "ぢぇ"))
       ("dyi" nil ("ヂィ" . "ぢぃ")) ("dyo" nil ("ヂョ" . "ぢょ"))
       ("dyu" nil ("ヂュ" . "ぢゅ")) ("e" nil ("エ" . "え"))
       ("ff" "f" ("ッ" . "っ")) ("fa" nil ("ファ" . "ふぁ"))
       ("fe" nil ("フェ" . "ふぇ")) ("fi" nil ("フィ" . "ふぃ"))
       ("fo" nil ("フォ" . "ふぉ")) ("fu" nil ("フ" . "ふ"))
       ("fya" nil ("フャ" . "ふゃ")) ("fye" nil ("フェ" . "ふぇ"))
       ("fyi" nil ("フィ" . "ふぃ")) ("fyo" nil ("フョ" . "ふょ"))
       ("fyu" nil ("フュ" . "ふゅ")) ("gg" "g" ("ッ" . "っ"))
       ("ga" nil ("ガ" . "が")) ("ge" nil ("ゲ" . "げ"))
       ("gi" nil ("ギ" . "ぎ")) ("go" nil ("ゴ" . "ご"))
       ("gu" nil ("グ" . "ぐ")) ("gya" nil ("ギャ" . "ぎゃ"))
       ("gye" nil ("ギェ" . "ぎぇ")) ("gyi" nil ("ギィ" . "ぎぃ"))
       ("gyo" nil ("ギョ" . "ぎょ")) ("gyu" nil ("ギュ" . "ぎゅ"))
       ;;("h" "" ("オ" . "お"))
       ("ha" nil ("ハ" . "は")) ("he" nil ("ヘ" . "へ"))
       ("hi" nil ("ヒ" . "ひ")) ("ho" nil ("ホ" . "ほ"))
       ("hu" nil ("フ" . "ふ")) ("hya" nil ("ヒャ" . "ひゃ"))
       ("hye" nil ("ヒェ" . "ひぇ")) ("hyi" nil ("ヒィ" . "ひぃ"))
       ("hyo" nil ("ヒョ" . "ひょ")) ("hyu" nil ("ヒュ" . "ひゅ"))
       ("i" nil ("イ" . "い")) ("jj" "j" ("ッ" . "っ"))
       ("ja" nil ("ジャ" . "じゃ")) ("je" nil ("ジェ" . "じぇ"))
       ("ji" nil ("ジ" . "じ")) ("jo" nil ("ジョ" . "じょ"))
       ("ju" nil ("ジュ" . "じゅ")) ("jya" nil ("ジャ" . "じゃ"))
       ("jye" nil ("ジェ" . "じぇ")) ("jyi" nil ("ジィ" . "じぃ"))
       ("jyo" nil ("ジョ" . "じょ")) ("jyu" nil ("ジュ" . "じゅ"))
       ("kk" "k" ("ッ" . "っ")) ("ka" nil ("カ" . "か"))
       ("ke" nil ("ケ" . "け")) ("ki" nil ("キ" . "き"))
       ("ko" nil ("コ" . "こ")) ("ku" nil ("ク" . "く"))
       ("kya" nil ("キャ" . "きゃ")) ("kye" nil ("キェ" . "きぇ"))
       ("kyi" nil ("キィ" . "きぃ")) ("kyo" nil ("キョ" . "きょ"))
       ("kyu" nil ("キュ" . "きゅ")) ("mm" "c" ("ッ" . "っ"))
       ("ma" nil ("マ" . "ま")) ("me" nil ("メ" . "め"))
       ("mi" nil ("ミ" . "み")) ("mo" nil ("モ" . "も"))
       ("mu" nil ("ム" . "む")) ("mya" nil ("ミャ" . "みゃ"))
       ("mye" nil ("ミェ" . "みぇ")) ("myi" nil ("ミィ" . "みぃ"))
       ("myo" nil ("ミョ" . "みょ")) ("myu" nil ("ミュ" . "みゅ"))
       ("n" nil ("ン" . "ん")) ("n'" nil ("ン" . "ん"))
       ("na" nil ("ナ" . "な")) ("ne" nil ("ネ" . "ね"))
       ("ni" nil ("ニ" . "に")) ("nn" nil ("ン" . "ん"))
       ("no" nil ("ノ" . "の")) ("nu" nil ("ヌ" . "ぬ"))
       ("nya" nil ("ニャ" . "にゃ")) ("nye" nil ("ニェ" . "にぇ"))
       ("nyi" nil ("ニィ" . "にぃ")) ("nyo" nil ("ニョ" . "にょ"))
       ("nyu" nil ("ニュ" . "にゅ")) ("o" nil ("オ" . "お"))
       ("pp" "p" ("ッ" . "っ")) ("pa" nil ("パ" . "ぱ"))
       ("pe" nil ("ペ" . "ぺ")) ("pi" nil ("ピ" . "ぴ"))
       ("po" nil ("ポ" . "ぽ")) ("pu" nil ("プ" . "ぷ"))
       ("pya" nil ("ピャ" . "ぴゃ")) ("pye" nil ("ピェ" . "ぴぇ"))
       ("pyi" nil ("ピィ" . "ぴぃ")) ("pyo" nil ("ピョ" . "ぴょ"))
       ("pyu" nil ("ピュ" . "ぴゅ")) ("rr" "r" ("ッ" . "っ"))
       ("ra" nil ("ラ" . "ら")) ("re" nil ("レ" . "れ"))
       ("ri" nil ("リ" . "り")) ("ro" nil ("ロ" . "ろ"))
       ("ru" nil ("ル" . "る")) ("rya" nil ("リャ" . "りゃ"))
       ("rye" nil ("リェ" . "りぇ")) ("ryi" nil ("リィ" . "りぃ"))
       ("ryo" nil ("リョ" . "りょ")) ("ryu" nil ("リュ" . "りゅ"))
       ("ss" "s" ("ッ" . "っ")) ("sa" nil ("サ" . "さ"))
       ("se" nil ("セ" . "せ")) ("sha" nil ("シャ" . "しゃ"))
       ("she" nil ("シェ" . "しぇ")) ("shi" nil ("シ" . "し"))
       ("sho" nil ("ショ" . "しょ")) ("shu" nil ("シュ" . "しゅ"))
       ("si" nil ("シ" . "し")) ("so" nil ("ソ" . "そ"))
       ("su" nil ("ス" . "す")) ("sya" nil ("シャ" . "しゃ"))
       ("sye" nil ("シェ" . "しぇ")) ("syi" nil ("シィ" . "しぃ"))
       ("syo" nil ("ショ" . "しょ")) ("syu" nil ("シュ" . "しゅ"))
       ("tt" "t" ("ッ" . "っ")) ("ta" nil ("タ" . "た"))
       ("te" nil ("テ" . "て")) ("tha" nil ("テァ" . "てぁ"))
       ("the" nil ("テェ" . "てぇ")) ("thi" nil ("ティ" . "てぃ"))
       ("tho" nil ("テョ" . "てょ")) ("thu" nil ("テュ" . "てゅ"))
       ("ti" nil ("チ" . "ち")) ("to" nil ("ト" . "と"))
       ("tsu" nil ("ツ" . "つ")) ("tu" nil ("ツ" . "つ"))
       ("tya" nil ("チャ" . "ちゃ")) ("tye" nil ("チェ" . "ちぇ"))
       ("tyi" nil ("チィ" . "ちぃ")) ("tyo" nil ("チョ" . "ちょ"))
       ("tyu" nil ("チュ" . "ちゅ")) ("u" nil ("ウ" . "う"))
       ("vv" "v" ("ッ" . "っ")) ("va" nil ("ヴァ" . "う゛ぁ"))
       ("ve" nil ("ヴェ" . "う゛ぇ")) ("vi" nil ("ヴィ" . "う゛ぃ"))
       ("vo" nil ("ヴォ" . "う゛ぉ")) ("vu" nil ("ヴ" . "う゛"))
       ("ww" "w" ("ッ" . "っ")) ("wa" nil ("ワ" . "わ"))
       ("we" nil ("ウェ" . "うぇ")) ("wi" nil ("ウィ" . "うぃ"))
       ("wo" nil ("ヲ" . "を")) ("wu" nil ("ウ" . "う"))
       ("xx" "x" ("ッ" . "っ")) ("xa" nil ("ァ" . "ぁ"))
       ("xe" nil ("ェ" . "ぇ")) ("xi" nil ("ィ" . "ぃ"))
       ("xka" nil ("ヵ" . "か")) ("xke" nil ("ヶ" . "け"))
       ("xo" nil ("ォ" . "ぉ")) ("xtsu" nil ("ッ" . "っ"))
       ("xtu" nil ("ッ" . "っ")) ("xu" nil ("ゥ" . "ぅ"))
       ("xwa" nil ("ヮ" . "ゎ")) ("xwe" nil ("ヱ" . "ゑ"))
       ("xwi" nil ("ヰ" . "ゐ")) ("xya" nil ("ャ" . "ゃ"))
       ("xyo" nil ("ョ" . "ょ")) ("xyu" nil ("ュ" . "ゅ"))
       ("yy" "y" ("ッ" . "っ")) ("ya" nil ("ヤ" . "や"))
       ("ye" nil ("イェ" . "いぇ")) ("yo" nil ("ヨ" . "よ"))
       ("yu" nil ("ユ" . "ゆ")) ("zz" "z" ("ッ" . "っ"))
       ("z," nil "‥") ("z-" nil "〜") ("z." nil "…")
       ("z/" nil "・") ("z[" nil "『") ("z]" nil "』")
       ("za" nil ("ザ" . "ざ")) ("ze" nil ("ゼ" . "ぜ"))
       ("zh" nil "←") ("zi" nil ("ジ" . "じ"))
       ("zj" nil "↓") ("zk" nil "↑") ("zl" nil "→")
       ("zo" nil ("ゾ" . "ぞ")) ("zu" nil ("ズ" . "ず"))
       ("zya" nil ("ジャ" . "じゃ")) ("zye" nil ("ジェ" . "じぇ"))
       ("zyi" nil ("ジィ" . "じぃ")) ("zyo" nil ("ジョ" . "じょ"))
       ("zyu" nil ("ジュ" . "じゅ")) ("." nil skk-current-kuten)
       ("," nil skk-current-touten) ("-" nil "ー")
       (":" nil "：") (";" nil "；") ("?" nil "？")
       ("[" nil "「") ("]" nil "」") ("l" nil skk-latin-mode)
       ("q" nil skk-toggle-characters) ("L" nil skk-jisx0208-latin-mode)
       ("Q" nil skk-set-henkan-point-subr)
       ("X" nil skk-purge-from-jisyo) ("/" nil skk-abbrev-mode)
       ("$" nil skk-display-code-for-char-at-point)
       ("\C-j" nil skk-kakutei)
       ("@" nil skk-today) ("\\" nil skk-input-by-code-or-menu)))
    (skk-rom-kana-rule-list . '(("hh" "h" ("ッ" . "っ"))))
    (skk-save-jisyo-function . 'skk-save-jisyo-original)
    (skk-search-excluding-word-pattern-function . nil)
    (skk-search-prog-list . '((skk-search-jisyo-buf
                               (get-buffer skktut-jisyo-buffer) 0)))
    (skk-set-henkan-point-key
     . '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z))
    (skk-share-private-jisyo . nil)
    (skk-special-midashi-char-list . '(?> ?< ??))
    (skk-start-henkan-char . ?\040)
    (skk-show-candidates-nth-henkan-char . 5)
    (skk-try-completion-char . ?\011)
    (skk-update-jisyo-function . 'skktut-update-jisyo)
    (skk-use-color-cursor . (and window-system
                                 (fboundp 'x-display-color-p)
                                 (x-display-color-p)))
    (skk-use-face . window-system)
    (skk-use-look . nil)
    (skk-use-numeric-conversion . t)
    (skk-use-rdbms . nil)
    (skk-use-relation . nil)
    (skk-use-viper . nil)
    (skk-show-tooltip . nil)
    (skk-show-inline . nil)
    (skk-show-annotation . nil)
    (skk-kcode-charset . (if (featurep 'jisx0213) ; Mule-UCS
                             (quote japanese-jisx0208)
                           skk-kcode-charset))
    (skk-kcode-method . 'code-or-char-list)
    (skk-dcomp-activate . nil)
    (skk-dcomp-multiple-activate . nil)
    (skk-read-from-minibuffer-function . nil)
    (skk-verbose . nil)
    (debug-on-error . nil)
    (skk-show-mode-show . nil)
    (skk-show-mode-enable . nil)

    ;; not user variables but to be localized.
    (skk-insert-new-word-function . nil)
    (skk-mode-invoked . t)
    (skk-rule-tree
     .
     (if (and skk-use-kana-keyboard
              (memq skk-kanagaki-keyboard-type '(oasys
                                                 nicola-jis nicola-us nicola-dvorak
                                                 omelet-jis omelet-us omelet-dvorak))
              (eq skk-kanagaki-state 'kana)
              skktut-nicola-tut-file)
         skk-kanagaki-rule-tree
       (skk-compile-rule-list skk-rom-kana-base-rule-list
                              skk-rom-kana-rule-list))))
  "skk.el のユーザー変数のリスト。")

(defvar skktut-nicola-tut-file "NICOLA-SKK.tut"
  "NICOLA-DDSKK のためのチュートリアルファイル名。
ディレクトリ名は含まない。")

(defvar skktut-japanese-tut nil
  "Non-nil であれば、チュートリアルが日本語であることを示す。")
(defvar skktut-right-answer nil "正解の文字列。")
(defvar skktut-question-count 1 "チュートリアルの現在の問題番号。")
(defvar skktut-tutorial-end nil "チュートリアルの終了を示すフラグ。")
(defvar skktut-working-buffer " *skk-tutorial*")
(defvar skktut-question-buffer "*問*")
(defvar skktut-answer-buffer "*答*")
(defvar skktut-jisyo-buffer " *skk-tut-jisyo*")
(defvar skktut-original-window-configuration nil)
(defvar skktut-working-window-configuration nil)
(defvar skktut-original-local-map nil)
(defvar skktut-skk-mode-on nil
  "Non-nil なら、tutorial を起動時に SKK が既に起動されていたことを示す。")

(defvar skktut-latin-mode-map nil
  "SKK チュートリアル ASCII モードキーマップ。")

(unless skktut-latin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'skk-kakutei)
    (setq skktut-latin-mode-map map)))

(defvar skktut-j-mode-map nil
  "SKK チュートリアルかな/カナモードキーマップ。")

(unless skktut-j-mode-map
  (let ((map (make-sparse-keymap))
        (c 32))
    (while (< c 127)
      (define-key map (skk-char-to-unibyte-string c) 'skk-insert)
      (setq c (1+ c)))
    (setq skktut-j-mode-map map)))

(defvar skktut-jisx0208-latin-mode-map nil
  "SKK チュートリアル全角英数字モードキーマップ。")

(unless skktut-jisx0208-latin-mode-map
  (let ((map (make-sparse-keymap))
        (i 0))
    (while (< i 128)
      (if (aref skk-jisx0208-latin-vector i)
          (define-key map (skk-char-to-unibyte-string i) 'skk-jisx0208-latin-insert))
      (setq i (1+ i)))
    (define-key map "\C-j" 'skk-kakutei)
    (setq skktut-jisx0208-latin-mode-map map)))

(defvar skktut-abbrev-mode-map nil
  "SKK チュートリアル Abbrev モードキーマップ。")

(unless skktut-abbrev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "," 'skk-abbrev-comma)
    (define-key map "." 'skk-abbrev-period)
    (define-key map "\C-q" 'skk-toggle-characters)
    (define-key map "\C-j" 'skk-kakutei)
    (define-key map " " 'skk-start-henkan)
    (define-key map "\t" 'skk-try-completion)
    (setq skktut-abbrev-mode-map map)))

;; -- macros
(defmacro skktut-message (japanese english &rest arg)
  ;; skktut-japanese-tut が non-nil だったら JAPANESE を nil であれば ENGLISH
  ;; をエコーエリアに表示する。
  ;; ARG は message 関数の第２引数以降の引数として渡される。
  (append `(message (if skktut-japanese-tut ,japanese ,english))
          arg))

(defmacro skktut-error (japanese english &rest arg)
  ;; skktut-japanese-tut が non-nil だったら JAPANESE を nil であれば ENGLISH
  ;; をエコーエリアに表示し、エラーを発生させる。
  ;; ARG は error 関数の第２引数以降の引数として渡される。
  (append `(error (if skktut-japanese-tut ,japanese ,english))
          arg))

(defmacro skktut-yes-or-no-p (japanese english)
  `(yes-or-no-p (if skktut-japanese-tut ,japanese ,english)))

;; advices.
(defadvice skk-create-file (around skktut-ad disable))

(defadvice skk-save-jisyo-original (around skktut-ad disable))

(defadvice skk-abbrev-mode (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (when (> 12 skktut-question-count)
    (skktut-error "このキーはまだ使えません"
                  "Cannot use this key yet")))

(defadvice skk-insert (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (when (and (memq last-command-event skk-set-henkan-point-key)
             (> 12 skktut-question-count))
    (skktut-error "かな/カナモードでは、英大文字はまだ使えません"
                  "Cannot use upper case character in kana/katakana mode")))

(defadvice skk-kakutei (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (when (and (called-interactively-p 'interactive)
             (= skktut-question-count 1))
    (skktut-error "このキーはまだ使えません"
                  "Cannot use this key yet")))

(defadvice skk-mode (before skktut-ad disable)
  "SKK チュートリアル用アドバイス付。"
  (when (and (called-interactively-p 'interactive)
             (= skktut-question-count 1))
    (skktut-error "このキーはまだ使えません"
                  "Cannot use this key yet")))

(defadvice skk-get-jisyo-buffer (around skktut-ad disable)
  (cond ((string= (skk-jisyo) skktut-tut-jisyo)
         (setq ad-return-value (get-buffer skktut-jisyo-buffer)))
        (t
         ad-do-it)))

;; hooks
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and (called-interactively-p 'interactive)
                       (member (buffer-name (current-buffer))
                               (list skktut-working-buffer
                                     skktut-question-buffer
                                     skktut-answer-buffer
                                     skktut-jisyo-buffer)))
              (skk-tutorial-quit 'now))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (buffer-live-p (get-buffer skktut-jisyo-buffer))
              (skk-tutorial-quit 'now))))

;; interactive commands. prefix should be `skk-tutorial'.
;;;###autoload
(defun skk-tutorial (&optional query-language)
  "Start SKK tutorial.
You can select English version by \\[universal-argument] \\[skk-tutorial]."
  (interactive "P")
  ;;
  (unless skk-mode-invoked
    ;; SKK を起動せずにいきなり
    ;; skk-tutorial を実行したときに skk-jisyo バッファが作られないので
    ;; skk-setup-jisyo-buffer でエラーとなり、Emacs の終了ができなく
    ;; なるので SKK モードを一度起こしておく。
    ;; (追記: モード行の設定などにも SKK を起動しておくことは必要なので、
    ;; とりあえず、なるべく早いタイミングに変更します。)
    (skk-mode 1)
    (skk-mode -1))
  ;;
  (when (and skk-use-kana-keyboard
             (memq skk-kanagaki-keyboard-type
                   '(oasys
                     nicola-jis nicola-us nicola-dvorak
                     omelet-jis omelet-us omelet-dvorak))
             (eq skk-kanagaki-state 'kana)
             skktut-nicola-tut-file)
    (skk-nicola-setup-tutorial)
    (setq skk-tut-file (expand-file-name skktut-nicola-tut-file
                                         (file-name-directory skk-tut-file))))
  ;;
  (let ((file skk-tut-file))
    (when (and (null skk-tut-current-lang)
               skk-tut-lang)
      (setq skk-tut-current-lang skk-tut-lang))
    (when query-language
      (setq skk-tut-current-lang (completing-read "Language: "
                                                  skk-tut-file-suffix-alist))
      (message "Language for the tutorial is %s until you exit Emacs"
               skk-tut-current-lang))
    (when skk-tut-current-lang
      (setq file (skk-tut-find-tut-file file)))
    (unless (file-exists-p (expand-file-name file))
      (error "No file found as %s" file))
    (let ((inhibit-quit t))
      (skktut-pre-setup-tutorial)
      (skktut-setup-jisyo-buffer)
      (skktut-setup-working-buffer file)
      (skktut-setup-question-buffer)
      (skktut-setup-answer-buffer)
      (skktut-enable-advice)
      (skktut-enable-tutmap)
      (add-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
      (skktut-make-windows))
    (skktut-setup-delete-backward-char)))

(defun skk-tut-nicola-p ()
  (equal skk-tut-file (expand-file-name skktut-nicola-tut-file
                                        (file-name-directory skk-tut-file))))

(defun skk-tut-find-tut-file (file)
  (cond
   ((skk-tut-nicola-p)
    skk-tut-file)
   (t
    (when (string-match "SKK\\.tut\\.\\w" file)
      (setq file (substring file 0 (- (match-end 0) 2))))
    (concat file (cdr (assoc skk-tut-current-lang
                             skk-tut-file-suffix-alist))))))

(defun skk-tutorial-again (&optional now)
  "ユーザの同意があれば SKK チュートリアルを最初からやり直す。
\\[universal-argument] \\[skk-tutorial-again] すると直ちにやり直す。"
  (interactive "P")
  (when (or now
            (skktut-yes-or-no-p
             "最初から Tutorial をやり直します。よろしいですね？ "
             "Quit tutorial and start from question 1 again? "))
    (skk-tutorial-quit 'now)
    (skk-tutorial)))

(defun skk-tutorial-quit (&optional now)
  "ユーザの同意がある場合 SKK チュートリアルをやめる。
\\[universal-argument] \\[skk-tutorial-quit] すると直ちにやめる。"
  (interactive "P")
  (when (or now
            (skktut-yes-or-no-p
             "本当にチュートリアルをやめますか？ "
             "Really quit tutorial? "))
    (let ((inhibit-quit t))
      (delete-other-windows)
      ;; 再度チュートリアルを使えるように、内部変数を初期化しておく。
      (setq skktut-japanese-tut nil
            skktut-question-count 1
            skktut-right-answer nil
            skktut-tutorial-end nil)
      (remove-hook 'minibuffer-setup-hook
                   'skktut-localize-and-init-variables)
      (skktut-disable-tutmap)
      (skktut-disable-advice)
      (with-current-buffer skktut-jisyo-buffer
        (set-buffer-modified-p nil)
        (kill-buffer skktut-jisyo-buffer))
      (kill-buffer skktut-working-buffer)
      (kill-buffer skktut-answer-buffer)
      (kill-buffer skktut-question-buffer)
      (set-window-configuration skktut-original-window-configuration)
      ;; チュートリアル起動直前に開いていたバッファで、skk-mode を起動して
      ;; いたら、その状態にして、チュートリアルを終了する。
      (unless skktut-skk-mode-on
        (skk-mode -1)))))

;; the following commands are also interactive, but users may not call
;; them by name.  So prefix should be `skktut-'.
(defun skktut-next-question ()
  (interactive)
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (save-match-data
    (let (user-ans)
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (skip-chars-backward " \t")
        (setq user-ans (buffer-substring-no-properties
                        (point-min) (point))))
      (when (string-match "^>* *" user-ans)
        (setq user-ans (substring user-ans (match-end 0))))
      (if (not (string= skktut-right-answer user-ans))
          (progn
            (skktut-message "答が違います。もう一度やってみて下さい"
                            "Wrong.  Try again")
            (ding))
        (setq skktut-question-count (1+ skktut-question-count))
        ;; buffer independent.
        (skktut-get-question-page skktut-question-count)
        (if (>= skktut-question-count
                (1+ skktut-question-numbers))
            (skk-tutorial-quit 'now)
          (skktut-next-answer-buffer))))))

(defun skktut-skip-question (arg)
  (interactive "p")
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (skktut-erase-buffer)
  (setq skktut-question-count (+ skktut-question-count arg))
  (cond ((> 1 skktut-question-count)
         (setq skktut-question-count 1))
        ;; overrun
        ((> skktut-question-count skktut-question-numbers)
         (setq skktut-question-count skktut-question-numbers))
        ((and (>= skktut-question-count 3)
              (not skk-j-mode))
         (skk-mode 1)))
  ;; buffer independent.
  (skktut-get-question-page skktut-question-count)
  (if skktut-tutorial-end
      (skk-tutorial-quit 'now)
    (skktut-next-answer-buffer)))

;; internal functions.  prefix should be `skktut-'.
(defun skktut-make-windows ()
  ;; Make window fill its frame.
  (delete-other-windows)
  (split-window-vertically)
  (let ((height (window-height (selected-window))))
    (unless (< 22 height)
      ;; Keep at least 23 lines for the upper window.
      (enlarge-window (- 23 height))))
  ;;   (other-window 1)
  ;; make it selected window and current buffer.
  ;;   (switch-to-buffer skktut-answer-buffer)
  (pop-to-buffer skktut-answer-buffer)
  ;; not make it current buffer but visible.
  (display-buffer skktut-question-buffer)
  (setq skktut-working-window-configuration
        (current-window-configuration)))

(defun skktut-enable-advice ()
  (dolist (e skktut-adviced-alist)
    (ad-enable-advice (car e) (cdr e) 'skktut-ad)
    (ad-activate (car e))))

(defun skktut-disable-advice ()
  (dolist (e skktut-adviced-alist)
    (ad-disable-advice (car e) (cdr e) 'skktut-ad)
    (ad-activate (car e))))

(defun skktut-enable-tutmap ()
  (let ((inhibit-quit t))
    (skk-update-minor-mode-map-alist 'skk-latin-mode skktut-latin-mode-map)
    (skk-update-minor-mode-map-alist 'skk-abbrev-mode skktut-abbrev-mode-map)
    (skk-update-minor-mode-map-alist 'skk-j-mode skktut-j-mode-map)
    (skk-update-minor-mode-map-alist 'skk-jisx0208-mode skktut-jisx0208-latin-mode-map)
    (define-key skktut-j-mode-map "\C-p" 'skk-previous-candidate)
    (unless (skk-tut-nicola-p)
      (define-key skktut-j-mode-map "x" 'skk-previous-candidate))
    (define-key skktut-j-mode-map "\C-j" 'skk-insert)
    (define-key skktut-j-mode-map "\t" 'skk-insert)
    ;; for minor-mode-map-alist localized by Viper.
    (when (and (featurep 'viper)
               (local-variable-p 'minor-mode-map-alist))
      (setq-default minor-mode-map-alist minor-mode-map-alist))))

(defun skktut-disable-tutmap ()
  (let ((inhibit-quit t)
        (minor-mode-list '(skk-abbrev-mode
                           skk-latin-mode
                           skk-j-mode
                           skk-jisx0208-latin-mode))
        minor-mode e)
    (while minor-mode-list
      (setq minor-mode (car minor-mode-list)
            minor-mode-list (cdr minor-mode-list))
      ;; fail safe.
      (while (setq e (assq minor-mode minor-mode-map-alist))
        (setq minor-mode-map-alist (delq e minor-mode-map-alist))))
    (skk-update-minor-mode-map-alist 'skk-latin-mode skk-latin-mode-map)
    (skk-update-minor-mode-map-alist 'skk-abbrev-mode skk-abbrev-mode-map)
    (skk-update-minor-mode-map-alist 'skk-j-mode skk-j-mode-map)
    (skk-update-minor-mode-map-alist 'skk-jisx0208-mode skk-jisx0208-latin-mode-map))
  ;; for minor-mode-map-alist localized by Viper.
  (when (default-value skk-use-viper)
    (skk-viper-normalize-map)))

(defun skktut-pre-setup-tutorial ()
  (setq skktut-original-window-configuration (current-window-configuration)
        skktut-skk-mode-on skk-mode
        skktut-question-count 1))

(defun skktut-update-jisyo (word &optional purge)
  (let ((skk-share-private-jisyo nil)
        (skk-jisyo-update-vector nil)
        (skk-update-jisyo-count 0))
    (skk-update-jisyo-original word purge)))

(defun skktut-setup-jisyo-buffer ()
  ;; setup skktut-tut-jisyo buffer.
  (with-current-buffer (get-buffer-create skktut-jisyo-buffer)
    (buffer-disable-undo (current-buffer))
    (skktut-localize-and-init-variables)
    (setq case-fold-search nil)
    (insert "\
;; okuri-ari entries.
ほっs /欲/
つかt /使/
たっs /達/
しt /知/
うごk /動/
;; okuri-nasi entries.
Cyrillic /А/Б/В/Г/Д/Е/Ё/Ж/З/И/Й/К/Л/М/Н/О/П/Р/С/Т/У/Ф/\
Х/Ц/Ч/Ш/Щ/Ъ/Ы/Ь/Э/Ю/Я/
Greek /Α/Β/Γ/Δ/Ε/Ζ/Η/Θ/Ι/Κ/Λ/Μ/Ν/Ξ/Ο/Π/Ρ/Σ/Τ/Υ/Φ/Χ/Ψ/\
Ω/
cyrillic /а/б/в/г/д/е/ё/ж/з/и/й/к/л/м/н/о/п/р/с/т/у/ф/\
х/ц/ч/ш/щ/ъ/ы/ь/э/ю/я/
greek /α/β/γ/δ/ε/ζ/η/θ/ι/κ/λ/μ/ν/ξ/ο/π/ρ/σ/τ/υ/φ/χ/ψ/\
ω/
tutorial /チュートリアル/
いち /一/
いちおく /一億/
おおさか /大阪/
おく /億/
かな /仮名/
かんじ /漢字/幹事/監事/
がく /学/
がくしゅう /学習/
き /基/記/気/木/帰/
きごう /記号/・/゛/゜/´/｀/¨/￣/ヽ/ヾ/ゝ/ゞ/〃/仝/々/〆/〇/―/‐/‖/…/‥/\
“/”/〔/〕/〈/〉/《/》/『/』/【/】/±/×/÷/≠/≦/≧/∞/∴/♂/♀/°/′/″/℃/\
￥/¢/£/§/☆/★/○/●/◎/◇/◆/□/■/△/▲/▽/▼/※/〒/→/←/↑/↓/〓/
きょうと /京都/
こう /口/
こうべ /神戸/
ご /五/互/伍/午/呉/吾/娯/後/御/悟/梧/檎/瑚/碁/語/誤/護/醐/
さい /細/最/再/
さいしょ /最初/
さいとう /斎藤/
さとう /佐藤/
しゅう /習/終/
しゅうりょう /終了/
しょ /書/
じ /辞/字/
じしょ /辞書/地所/
じん /人/
じんこう /人口/
せん /選/洗/
せんたく /選択/洗濯/
そう /走/
だい /大/第/代/
たく /択/濯/
てき /的/敵/滴/適/摘/
とう /東/登/
とうほく /東北/
とうろく /登録/
どう /動/
にゅうりょく /入力/
ひこうき /飛行機/
へんかん /変換/
ほく /北/
みょう /名/
みょうじ /名字/
ようい /容易/用意/
りょう /了/
ろく /録/
")
    (skk-setup-jisyo-buffer)))

(defun skktut-setup-working-buffer (file)
  (save-match-data
    (let (sexp)
      (set-buffer (get-buffer-create skktut-working-buffer))
      (buffer-disable-undo (current-buffer))
      (skktut-localize-and-init-variables)
      (skktut-erase-buffer) ; fail safe.
      (insert-file-contents file)
      ;; Count how many questions there are.
      (setq skktut-question-numbers -1)
      (goto-char (point-min))
      (while (re-search-forward "^----$" nil t)
        (setq skktut-question-numbers (1+ skktut-question-numbers)))
      ;;
      (goto-char (point-min))
      (setq skktut-japanese-tut (looking-at ";; SKK Japanese"))
      (while (re-search-forward "^>> \\((.+)\\)$" nil t nil)
        (setq sexp (buffer-substring-no-properties (match-beginning 1)
                                                   (match-end 1)))
        (delete-region (match-beginning 1) (match-end 1))
        ;; insert evaluated string instead of lisp program.
        (insert (eval (car (read-from-string sexp)))))
      (goto-char (point-min))
      (when skk-tut-use-face
        (skktut-colored)))))

(defun skktut-setup-question-buffer ()
  (with-current-buffer (get-buffer-create skktut-question-buffer)
    (buffer-disable-undo (current-buffer))
    (skktut-erase-buffer) ; fail safe.
    (setq buffer-read-only t)
    (skktut-get-question-page skktut-question-count)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question)))

(defun skktut-setup-answer-buffer ()
  (with-current-buffer (get-buffer-create skktut-answer-buffer)
    ;; users may use undo.
    ;; (buffer-disable-undo (current-buffer))
    ;; skktut-answer-buffer の skk.el の変数をバッファローカル化し、初期化す
    ;; る。
    (skktut-localize-and-init-variables)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question)
    (auto-fill-mode -1)
    (skktut-next-answer-buffer)))

(defun skktut-localize-and-init-variables ()
  ;; ユーザーが skk.el の変数をカスタマイズしている可能性があるので、カレント
  ;; バッファの skk.el の変数をバッファローカル化し、初期化する。
  (let ((alist skktut-init-variables-alist)
        v)
    (dolist (cell alist)
      (setq v (car cell))
      (make-local-variable v)
      (set v (eval (cdr cell))))))

(defun skktut-erase-buffer ()
  (let ((inhibit-read-only t)
        buffer-read-only)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)))

(defun skktut-colored ()
  ;; face を Text Property にしておくとテキストをコピーしたときに一緒にコピーで
  ;; きるので好都合。
  (while (re-search-forward "▼\\([^】 ぁ-んァ-ン]+\\)" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1) 'face
                       'highlight))
  (goto-char (point-min))
  (while (re-search-forward "^==.+==$" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-section-face))
  (goto-char (point-min))
  (while (re-search-forward "^!!.+" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-do-it-face))
  (goto-char (point-min))
  (while (re-search-forward "^>> \\(.+\\)$" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face skk-tut-question-face))
  (unless skktut-japanese-tut
    (goto-char (point-min))
    (while (re-search-forward "Hint: .*$" nil t nil)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face skk-tut-hint-face))))

(defun skktut-next-answer-buffer ()
  (save-match-data
    (with-current-buffer skktut-question-buffer
      ;; first get right answer in `skktut-question-buffer'.
      (goto-char (point-max))
      (search-backward "\n>>")
      (forward-char 1)
      (setq skktut-right-answer
            (buffer-substring-no-properties (+ 3 (point))
                                            (line-end-position)))
      (goto-char (point-min)))
    ;; not to save point.
    (let ((cbuf (current-buffer))
          p)
      (unwind-protect
          (let ((plist (cons 'rear-nonsticky
                             '(t intangible t read-only t))))
            ;; secondary make a new answer buffer.
            (set-buffer skktut-answer-buffer)
            (skktut-erase-buffer)
            (insert ">> \n\n")
            (add-text-properties (point-min) (- (point) 2) plist)
            (setq p (point))
            (insert
             (let ((next " `\\[skktut-next-question]'")
                   (quit " `\\[skk-tutorial-quit]'")
                   (skip " `\\[skktut-skip-question]'")
                   (sow  " `\\[scroll-other-window]'"))
               (substitute-command-keys
                (if skktut-japanese-tut
                    (concat
                     "* 答ができたら" next "\n"
                     "* 途中でやめるには" quit "\n"
                     (if (/= skktut-question-count skktut-question-numbers)
                         (concat "* スキップするには" skip "\n"))
                     "* 【問】をスクロールするには" sow)
                  (concat
                   "* For next question" next "\n"
                   "* to quit " quit "\n"
                   (if (/= skktut-question-count skktut-question-numbers)
                       (concat "* to skip this question" skip "\n"))
                   "* to scroll question window" sow)))))
            (when skk-tut-use-face
              (put-text-property p (point) 'face skk-tut-key-bind-face))
            (add-text-properties p (point) plist)
            (goto-char (+ (point-min) 3)))
        (set-buffer cbuf)))))

(defun skktut-get-question-page (page)
  (save-excursion
    (save-match-data
      (if (>= skktut-question-count
              (1+ skktut-question-numbers))
          ;; No more need to search for S expressions in the working buffer.
          ;; It's time to finish.
          (skktut-end-tutorial)
        ;; Tutorial continues.
        (set-buffer skktut-working-buffer)
        (let (pos str)
          (goto-char (point-min))
          (search-forward "--\n" nil t page)
          (when (looking-at ";") ; lisp program exists.
            (forward-char 3)
            (setq pos (point))
            (end-of-line)
            (skk-save-point
             (eval-region pos (point) nil))
            (forward-char 1))
          (unless skktut-tutorial-end
            (setq pos (point))
            (search-forward "\n>>")
            (end-of-line)
            (setq str (buffer-substring pos (point)))
            (set-buffer skktut-question-buffer)
            (skktut-erase-buffer)
            (let (buffer-read-only)
              (insert str)
              (setq mode-line-buffer-identification
                    (concat "ＳＫＫチュートリアル: ［問 "
                            (number-to-string page)
                            "］ （残り "
                            (number-to-string
                             (- skktut-question-numbers page))
                            "問）"))
              (set-buffer-modified-p nil)
              (force-mode-line-update 'all))))))))

;; The following two functions are tricky, since they are executed by
;; `eval-region' in skktut-working-buffer.
(defun skktut-today ()
  (save-restriction
    (save-match-data
      (let (p)
        (widen)
        (search-forward "\n>> ")
        (when (re-search-forward "「.*」" (line-end-position) t)
          (delete-region (match-beginning 0) (match-end 0)))
        (setq p (point))
        (insert (concat "「きょうは、" (skk-current-date) "です。」"))
        (narrow-to-region (point-min) (point))
        (when skk-tut-use-face
          (put-text-property p (point) 'face skk-tut-question-face))))))

(defun skktut-end-tutorial ()
  ;;  (switch-to-buffer skktut-question-buffer)
  (pop-to-buffer skktut-question-buffer)
  (delete-other-windows)
  (skktut-erase-buffer)
  (let (buffer-read-only)
    (goto-char (point-min))
    (insert
     (format
      (if skktut-japanese-tut
          "\
SKK チュートリアルはこれで終りです。

%s に関する質問、コメント、bug report 等は

https://github.com/skk-dev 迄お送りください。

!! 最後に <return> キーを押してください。"

        "\
Now we end the SKK tutorial.

Please post comments, questions and bug reports on %s to:

https://github.com/skk-dev

!! Hit <return> key when you are ready.")
      (skk-version t)))

    (when skk-tut-use-face
      (save-match-data
        (goto-char (point-min))
        (re-search-forward "^!!.+" nil t nil)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face skk-tut-do-it-face)))

    (while (not skktut-tutorial-end)
      (ignore-errors
        (let* ((event (read-event))
               (char (event-to-character event)))
          (skktut-message "<return> キーを押してください"
                          "Hit <return> key")
          (if (and char (eq ?\C-m char))
              (setq skktut-tutorial-end t)))))
    (message nil)))

(defun skktut-setup-delete-backward-char ()
  (let ((commands '(backward-delete-char-untabify
                    backward-delete-char
                    backward-or-forward-delete-char
                    delete-backward-char
                    picture-backward-clear-column
                    ;; following two are SKK adviced.
                    ;;viper-del-backward-char-in-insert
                    ;;vip-del-backward-char-in-insert
                    ))
        (map (if (and (boundp 'overriding-local-map)
                      (keymapp 'overriding-local-map))
                 overriding-local-map
               (current-global-map)))
        keys)
    (dolist (command commands)
      (setq keys (where-is-internal command map))
      (dolist (key keys)
        (define-key skktut-abbrev-mode-map
          key 'skk-delete-backward-char)
        (define-key skktut-j-mode-map
          key 'skk-delete-backward-char)))))

(defun skktut-user-name ()
  (let ((name (user-full-name)))
    (if (not (or (string= name "")
                 ;; non-ASCII characters
                 (save-match-data
                   (string-match "[^\000-\177]" name))))
        name
      (setq name (user-login-name))
      (if (not (or (string= name "")
                   ;; non-ASCII characters
                   (save-match-data
                     (string-match "[^\000-\177]" name))))
          name
        "nanasi"))))

(provide 'skk-tut)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-tut.el ends here
