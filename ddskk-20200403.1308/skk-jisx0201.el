;;; skk-jisx0201.el --- JIS X 0201 ｶﾅ, Roman -*- coding: iso-2022-7bit -*-

;; Copyright (C) 1999-2007  SKK Development Team <skk@ring.gr.jp>

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method
;; Created: Oct. 30, 1999.

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

;; <簡単な説明>

;; ~/.skk に

;;   (setq skk-use-jisx0201-input-method t)

;; と書くことでインストールされます。使い方は以下のようになります。

;;   ◎カタカナモードにおいて、
;;     ・"C-q" で全角カナモードと半角カナモードを切りかえます。

;;   ◎ひらがな/カタカナ両モード内での▽モードにおいて、
;;     ・"C-q" を押すと､見出し語として入力されたひらがな/カタカナをﾊﾝｶｸｶﾀｶﾅに変
;;       換します。

;; `skk-jisx0201-roman-rule-list' に JISX0201.1976 Japanese Roman
;; (latin-jisx0201) の文字列を定義しています。ただし GNU Emacs 23 以降、ファ
;; イル保存時に JIS X 0201 Roman と ASCII の区別がされなくなったようです (円
;; 記号およびオーバーラインを除く)。したがってこのファイルの現行版では、これ
;; らの 2 文字以外は ASCII の文字が定義されています。

;; ｶﾅ と roman を切り替える機能 `skk-toggle-jisx0201' にはキー定義していませ
;; ん。

;; <業務連絡>

;; GNU Emacs 20.3 〜 22 でこのファイルを編集する場合は、ファイルを開く前に

;;   (setq standard-translation-table-for-decode (make-translation-table nil))

;; を評価してください。

;;; Code:

(require 'skk)

(require 'japan-util)

;; 諸般の事情により skk-vars.el に入れるべきでない変数
(defvar skk-jisx0201-base-rule-list
  '(("a" nil "ｱ")
    ("bb" "b" "ｯ") ("ba" nil "ﾊﾞ") ("be" nil "ﾍﾞ")
    ("bi" nil "ﾋﾞ") ("bo" nil "ﾎﾞ") ("bu" nil "ﾌﾞ") ("bya" nil "ﾋﾞｬ")
    ("bye" nil "ﾋﾞｪ") ("byi" nil "ﾋﾞｨ") ("byo" nil "ﾋﾞｮ") ("byu" nil "ﾋﾞｭ")
    ("cc" "c" "ｯ") ("cha" nil "ﾁｬ") ("che" nil "ﾁｪ") ("chi" nil "ﾁ")
    ("cho" nil "ﾁｮ") ("chu" nil "ﾁｭ") ("cya" nil "ﾁｬ") ("cye" nil "ﾁｪ")
    ("cyi" nil "ﾁｨ") ("cyo" nil "ﾁｮ") ("cyu" nil "ﾁｭ")
    ("dd" "d" "ｯ") ("da" nil "ﾀﾞ") ("de" nil "ﾃﾞ") ("dha" nil "ﾃﾞｬ")
    ("dhe" nil "ﾃﾞｪ") ("dhi" nil "ﾃﾞｨ") ("dho" nil "ﾃﾞｮ") ("dhu" nil "ﾃﾞｭ")
    ("di" nil "ﾁﾞ") ("do" nil "ﾄﾞ") ("du" nil "ﾂﾞ") ("dya" nil "ﾁﾞｬ")
    ("dye" nil "ﾁﾞｪ") ("dyi" nil "ﾁﾞｨ") ("dyo" nil "ﾁﾞｮ") ("dyu" nil "ﾁﾞｭ")
    ("e" nil "ｴ")
    ("ff" "f" "ｯ") ("fa" nil "ﾌｧ") ("fe" nil "ﾌｪ") ("fi" nil "ﾌｨ")
    ("fo" nil "ﾌｫ") ("fu" nil "ﾌ") ("fya" nil "ﾌｬ") ("fye" nil "ﾌｪ")
    ("fyi" nil "ﾌｨ") ("fyo" nil "ﾌｮ") ("fyu" nil "ﾌｭ") ("gg" "g" "ｯ")
    ("ga" nil "ｶﾞ") ("ge" nil "ｹﾞ") ("gi" nil "ｷﾞ") ("go" nil "ｺﾞ")
    ("gu" nil "ｸﾞ") ("gya" nil "ｷﾞｬ") ("gye" nil "ｷﾞｪ") ("gyi" nil "ｷﾞｨ")
    ("gyo" nil "ｷﾞｮ") ("gyu" nil "ｷﾞｭ")
    ("ha" nil "ﾊ") ("he" nil "ﾍ") ("hi" nil "ﾋ") ("ho" nil "ﾎ")
    ("hu" nil "ﾌ") ("hya" nil "ﾋｬ") ("hye" nil "ﾋｪ") ("hyi" nil "ﾋｨ")
    ("hyo" nil "ﾋｮ") ("hyu" nil "ﾋｭ") ("i" nil "ｲ")
    ("jj" "j" "ｯ") ("ja" nil "ｼﾞｬ") ("je" nil "ｼﾞｪ") ("ji" nil "ｼﾞ")
    ("jo" nil "ｼﾞｮ") ("ju" nil "ｼﾞｭ") ("jya" nil "ｼﾞｬ") ("jye" nil "ｼﾞｪ")
    ("jyi" nil "ｼﾞｨ") ("jyo" nil "ｼﾞｮ") ("jyu" nil "ｼﾞｭ")
    ("kk" "k" "ｯ") ("ka" nil "ｶ") ("ke" nil "ｹ") ("ki" nil "ｷ")
    ("ko" nil "ｺ") ("ku" nil "ｸ") ("kya" nil "ｷｬ") ("kye" nil "ｷｪ")
    ("kyi" nil "ｷｨ") ("kyo" nil "ｷｮ") ("kyu" nil "ｷｭ")
    ("mm" "c" "ｯ") ("ma" nil "ﾏ") ("me" nil "ﾒ") ("mi" nil "ﾐ")
    ("mo" nil "ﾓ") ("mu" nil "ﾑ") ("mya" nil "ﾐｬ") ("mye" nil "ﾐｪ")
    ("myi" nil "ﾐｨ") ("myo" nil "ﾐｮ") ("myu" nil "ﾐｭ")
    ("n" nil "ﾝ") ("n'" nil "ﾝ") ("na" nil "ﾅ") ("ne" nil "ﾈ")
    ("ni" nil "ﾆ") ("nn" nil "ﾝ") ("no" nil "ﾉ") ("nu" nil "ﾇ")
    ("nya" nil "ﾆｬ") ("nye" nil "ﾆｪ") ("nyi" nil "ﾆｨ") ("nyo" nil "ﾆｮ")
    ("nyu" nil "ﾆｭ")
    ("o" nil "ｵ")
    ("pp" "p" "ｯ") ("pa" nil "ﾊﾟ") ("pe" nil "ﾍﾟ") ("pi" nil "ﾋﾟ")
    ("po" nil "ﾎﾟ") ("pu" nil "ﾌﾟ") ("pya" nil "ﾋﾟｬ") ("pye" nil "ﾋﾟｪ")
    ("pyi" nil "ﾋﾟｨ") ("pyo" nil "ﾋﾟｮ") ("pyu" nil "ﾋﾟｭ")
    ("rr" "r" "ｯ") ("ra" nil "ﾗ") ("re" nil "ﾚ") ("ri" nil "ﾘ")
    ("ro" nil "ﾛ") ("ru" nil "ﾙ") ("rya" nil "ﾘｬ") ("rye" nil "ﾘｪ")
    ("ryi" nil "ﾘｨ") ("ryo" nil "ﾘｮ") ("ryu" nil "ﾘｭ")
    ("ss" "s" "ｯ") ("sa" nil "ｻ") ("se" nil "ｾ") ("sha" nil "ｼｬ")
    ("she" nil "ｼｪ") ("shi" nil "ｼ") ("sho" nil "ｼｮ") ("shu" nil "ｼｭ")
    ("si" nil "ｼ") ("so" nil "ｿ") ("su" nil "ｽ") ("sya" nil "ｼｬ")
    ("sye" nil "ｼｪ") ("syi" nil "ｼｨ") ("syo" nil "ｼｮ") ("syu" nil "ｼｭ")
    ("tt" "t" "ｯ") ("ta" nil "ﾀ") ("te" nil "ﾃ") ("tha" nil "ﾃｧ")
    ("the" nil "ﾃｪ") ("thi" nil "ﾃｨ") ("tho" nil "ﾃｮ") ("thu" nil "ﾃｭ")
    ("ti" nil "ﾁ") ("to" nil "ﾄ") ("tsu" nil "ﾂ") ("tu" nil "ﾂ")
    ("tya" nil "ﾁｬ") ("tye" nil "ﾁｪ") ("tyi" nil "ﾁｨ") ("tyo" nil "ﾁｮ")
    ("tyu" nil "ﾁｭ")
    ("u" nil "ｳ")
    ("vv" "v" "ｯ") ("va" nil "ｳﾞｧ") ("ve" nil "ｳﾞｪ") ("vi" nil "ｳﾞｨ")
    ("vo" nil "ｳﾞｫ") ("vu" nil "ｳﾞ")
    ("ww" "w" "ｯ") ("wa" nil "ﾜ") ("we" nil "ｳｪ") ("wi" nil "ｳｨ")
    ("wo" nil "ｦ") ("wu" nil "ｳ")
    ("xx" "x" "ｯ") ("xa" nil "ｧ") ("xe" nil "ｪ") ("xi" nil "ｨ")
    ("xka" nil "ｶ") ("xke" nil "ｹ") ("xo" nil "ｫ") ("xtsu" nil "ｯ")
    ("xtu" nil "ｯ") ("xu" nil "ｩ") ("xwa" nil "ﾜ") ("xwe" nil "ｪ")
    ("xwi" nil "ｨ") ("xya" nil "ｬ") ("xyo" nil "ｮ") ("xyu" nil "ｭ")
    ("yy" "y" "ｯ") ("ya" nil "ﾔ") ("ye" nil "ｲｪ") ("yo" nil "ﾖ")
    ("yu" nil "ﾕ")
    ("zz" "z" "ｯ") ("z," nil "‥") ("z-" nil "〜") ("z." nil "…")
    ("z/" nil "･") ("z[" nil "『") ("z]" nil "』") ("za" nil "ｻﾞ")
    ("ze" nil "ｾﾞ") ("zh" nil "←") ("zi" nil "ｼﾞ") ("zj" nil "↓")
    ("zk" nil "↑") ("zl" nil "→") ("zo" nil "ｿﾞ") ("zu" nil "ｽﾞ")
    ("zya" nil "ｼﾞｬ") ("zye" nil "ｼﾞｪ") ("zyi" nil "ｼﾞｨ") ("zyo" nil "ｼﾞｮ")
    ("zyu" nil "ｼﾞｭ")
    ("," nil "､") ("." nil "｡") ("-" nil "ｰ") (":" nil ":") (";" nil ";")
    ("?" nil "?") ("[" nil "｢") ("]" nil "｣")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-katakana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    (skk-kakutei-key nil skk-kakutei))
  "*SKK JISX0201 モードのベースのルール。")

(defvar skk-jisx0201-roman-rule-list
  '(("!" nil "!") ("\"" nil "\"") ("#" nil "#") ("$" nil "$") ("%" nil "%")
    ("&" nil "&") ("'" nil "'") ("\(" nil "(") ("\)" nil ")") ("*" nil "*")
    ("+" nil "+") ("," nil ",") ("-" nil "-") ("." nil ".") ("/" nil "/")
    ("0" nil "0") ("1" nil "1") ("2" nil "2") ("3" nil "3") ("4" nil "4")
    ("5" nil "5") ("6" nil "6") ("7" nil "7") ("8" nil "8") ("9" nil "9")
    (":" nil ":") (";" nil ";") ("<" nil "<") ("=" nil "=") (">" nil ">")
    ("?" nil "?") ("@" nil "@")
    ("A" nil "A") ("B" nil "B") ("C" nil "C") ("D" nil "D") ("E" nil "E")
    ("F" nil "F") ("G" nil "G") ("H" nil "H") ("I" nil "I") ("J" nil "J")
    ("K" nil "K") ("L" nil "L") ("M" nil "M") ("N" nil "N") ("O" nil "O")
    ("P" nil "P") ("Q" nil "Q") ("R" nil "R") ("S" nil "S") ("T" nil "T")
    ("U" nil "U") ("V" nil "V") ("W" nil "W") ("X" nil "X") ("Y" nil "Y")
    ("Z" nil "Z")
    ("[" nil "[") ("\\" nil "\\") ("]" nil "]") ("^" nil "^") ("_" nil "_")
    ("`" nil "`")
    ("a" nil "a") ("b" nil "b") ("c" nil "c") ("d" nil "d") ("e" nil "e")
    ("f" nil "f") ("g" nil "g") ("h" nil "h") ("i" nil "i") ("j" nil "j")
    ("k" nil "k") ("l" nil "l") ("m" nil "m") ("n" nil "n") ("o" nil "o")
    ("p" nil "p") ("q" nil "q") ("r" nil "r") ("s" nil "s") ("t" nil "t")
    ("u" nil "u") ("v" nil "v") ("w" nil "w") ("x" nil "x") ("y" nil "y")
    ("z" nil "z")
    ("{" nil "{") ("|" nil "|") ("}" nil "}") ("~" nil "~"))
  "*SKK JISX0201 モードの Roman のルール。")

(defvar skk-jisx0201-rule-list
  '(("\(" nil "(")
    ("{" nil "{"))
  "*SKK JISX0201 モードの追加のルール。")

(setq skk-jisx0201-base-rule-tree
      (skk-compile-rule-list skk-jisx0201-base-rule-list
                             skk-jisx0201-rule-list))
(setq skk-jisx0201-roman-rule-tree
      (skk-compile-rule-list skk-jisx0201-roman-rule-list))

;; Hooks.

;; inline functions.
(defsubst skk-jisx0201-mode-on (&optional arg)
  "SKK JIS X 0201 (カナ) モードを起動する。"
  (make-local-variable 'skk-rule-tree)
  (setq skk-mode t
        skk-jisx0201-mode t
        skk-jisx0201-roman arg
        skk-rule-tree (if arg
                          skk-jisx0201-roman-rule-tree
                        skk-jisx0201-base-rule-tree)
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-katakana nil)
  (skk-update-modeline 'jisx0201)
  (skk-cursor-set))

;; Pieces of advice.
(defadvice skk-mode (before skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil)
  (kill-local-variable 'skk-rule-tree))

(defadvice skk-kakutei (around skk-jisx0201-ad activate)
  (let ((jisx0201 skk-jisx0201-mode))
    ad-do-it
    (when jisx0201
      (skk-jisx0201-mode-on skk-jisx0201-roman))))

(defadvice skk-latin-mode (before skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil)
  (kill-local-variable 'skk-rule-tree))

(defadvice skk-jisx0208-latin-mode (before skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil)
  (kill-local-variable 'skk-rule-tree))

(defadvice skk-abbrev-mode (before skk-jisx0201-ad activate)
  (setq skk-jisx0201-mode nil)
  (kill-local-variable 'skk-rule-tree))

(defadvice skk-set-okurigana (around skk-jisx0201-ad activate)
  "半角カナの送り仮名を正しく取得する。"
  (cond
   (skk-jisx0201-mode
    (skk-save-point
     (goto-char skk-okurigana-start-point)
     (when (eq ?* (following-char))
       (delete-char 1))
     (skk-jisx0201-zenkaku-region skk-henkan-start-point
                                  skk-okurigana-start-point))
    ;;
    (let ((pt1 (point))
          pt2 okuri sokuon)
      (setq okuri
            (skk-save-point
             (backward-char 1)
             (buffer-substring-no-properties
              (setq pt2 (point)) pt1)))
      (cond
       ((member okuri '("ﾞ" "ﾟ"))
        (setq okuri
              (concat (skk-save-point
                       (backward-char 2)
                       (buffer-substring-no-properties
                        (point) pt2))
                      okuri))
        (setq sokuon t))
       (okuri
        (setq sokuon
              (skk-save-point
               (backward-char 2)
               (buffer-substring-no-properties
                (point) pt2)))
        (unless (member sokuon '("ｯ"))
          (setq sokuon nil))))
      ;;
      (when okuri
        (skk-save-point
         (backward-char (if sokuon 2 1))
         (skk-set-marker skk-okurigana-start-point (point)))
        (setq skk-okuri-char (skk-okurigana-prefix
                              (skk-katakana-to-hiragana
                               (skk-jisx0201-zenkaku okuri))))
        ;;
        (let ((skk-katakana t))
          ad-do-it))))
   (t
    ad-do-it)))

(defadvice skk-insert (around skk-jisx0201-ad activate)
  "SKK JIS X 0201 モードの文字入力を行う。"
  (cond
   (skk-jisx0201-mode
    (let ((arg (ad-get-arg 0))
          (ch last-command-event))
      (cond
       ((or (and (not skk-jisx0201-roman)
                 (memq ch skk-set-henkan-point-key)
                 (or skk-okurigana
                     (not (skk-get-prefix skk-current-rule-tree))
                     (not (skk-select-branch
                           skk-current-rule-tree ch))))
            (and skk-henkan-mode
                 (memq ch skk-special-midashi-char-list)))
        ad-do-it)
       ;;
       ((and skk-henkan-mode
             (eq ch skk-start-henkan-char))
        (skk-kana-cleanup 'force)
        (unless (or skk-okurigana
                    skk-okuri-char)
          (let ((jisx0201 (buffer-substring-no-properties
                           skk-henkan-start-point
                           (point)))
                jisx0208)
            (when (and jisx0201
                       (setq jisx0208
                             (skk-jisx0201-zenkaku jisx0201)))
              (insert-before-markers jisx0208)
              (delete-region skk-henkan-start-point
                             (- (point) (length jisx0208))))))
        (let ((skk-katakana t))
          (skk-start-henkan arg))
        (skk-cursor-set))
       ;;
       (skk-jisx0201-roman
        (let (skk-set-henkan-point-key)
          ad-do-it))
       ;;
       (t
        ad-do-it))))
   ;;
   (t
    ad-do-it)))

(defadvice skk-search-sagyo-henkaku (before skk-jisx0201-set-okuri activate)
  "SKK JIS X 0201 モードでは送り仮名を半角カナにする。"
  (when skk-jisx0201-mode
    (ad-set-arg 0 '("ｻ" "ｼ" "ｽ" "ｾ"))))

;; functions.
;;;###autoload
(defun skk-jisx0201-mode (arg)
  "SKK のモードを JIS X 0201 モードに変更する。"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0201-mode-on))

(defun skk-toggle-jisx0201 (arg)
  "半角カナモードとローマ字モードを切り替える。"
  (interactive "P")
  (cond
   ((eq skk-henkan-mode 'on)
    (skk-jisx0201-henkan arg))
   (skk-jisx0201-roman
    (setq skk-rule-tree skk-jisx0201-base-rule-tree
          skk-jisx0201-roman nil))
   (t
    (setq skk-rule-tree skk-jisx0201-roman-rule-tree
          skk-jisx0201-roman t))))

(defun skk-jisx0201-string-conversion (str func)
  (with-temp-buffer
    (insert str)
    (funcall func 1 (point))
    (buffer-string)))

(defun skk-jisx0201-zenkaku (str)
  "STR の JIS X 0201 カナ文字を対応する JIS X 0208 の文字で置き換える。"
  (skk-jisx0201-string-conversion str #'skk-jisx0201-zenkaku-region))

(defun skk-jisx0201-hankaku (str)
  "STR の JIS X 0208 文字を対応する JIS X 0201 カナの文字で置き換える。"
  (skk-jisx0201-string-conversion str #'japanese-hankaku-region))

;;;###autoload
(defun skk-toggle-katakana (arg)
  (interactive "P")
  (cond
   ((eq skk-henkan-mode 'on)
    (skk-jisx0201-henkan arg))
   (skk-jisx0201-mode
    (when (eq skk-henkan-mode 'active)
      (skk-kakutei))
    (setq skk-jisx0201-mode nil)
    (skk-j-mode-on)
    (kill-local-variable 'skk-rule-tree))
   (t
    (when (eq skk-henkan-mode 'active)
      (skk-kakutei))
    (skk-jisx0201-mode-on))))

(defun skk-jisx0201-zenkaku-region (start end)
  (japanese-zenkaku-region start end 'katakana-only))

(defun skk-jisx0201-henkan (arg)
  "▽モードであれば、領域のひらがな/カタカナを ﾊﾝｶｸｶﾀｶﾅ に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (skk-henkan-skk-region-by-func #'skk-jisx0201-region arg))

(defun skk-jisx0201-region (start end)
  "領域のひらがな/カタカナを ﾊﾝｶｸｶﾀｶﾅ に変換する。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (setq end (set-marker (make-marker) end))
  (skk-hiragana-to-jisx0201-region start end)
  (skk-katakana-to-jisx0201-region start end)
  (set-marker end nil))

;;;###autoload
(defun skk-hiragana-to-jisx0201-region (start end)
  (skk-search-and-replace
   start end
   "[ぁ-ん。、・ー゛゜]+"
   (lambda (matched)
     (save-match-data
       (skk-jisx0201-hankaku matched)))))

;;;###autoload
(defun skk-katakana-to-jisx0201-region (start end)
  (skk-search-and-replace
   start end
   "[ァ-ヴ。、・ー゛゜]+"
   (lambda (matched)
     (save-match-data
       (skk-jisx0201-hankaku matched)))))

(provide 'skk-jisx0201)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-jisx0201.el ends here
