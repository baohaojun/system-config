;;; skk-look.el --- UNIX look command interface for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 1998, 1999, 2000 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

;; <How to work>
;; ~/.skk か ~/.emacs.d/init.el で `skk-use-look' を t にセットしてこれを
;; 評価して下さい。その後 skk-mode を立ち上げるか、M-x skk-restart すると、
;; 下記のような芸当が可能になります。

;; (1)英単語を補完ができます。

;;    ▽abstr(TAB) ---> ▽abstract

;;    通常の補完機能同様、`.' で次の補完候補、`,' でひとつ前の補完候補に
;;    移動できます。

;;    SKK 形式の英和辞書があれば、ここから SPC を押して英和変換ができますね。

;; (2)英単語をあいまいに変換して取り出すことができます。

;;    ▽abstr* (SPC) ---> ▼abstract

;;    見出し語にアスタリスク (`*') を入れるのをお忘れなく。

;;    確定すると、`abstr*' を見出し語、`abstract' を候補とするエントリが個人辞
;;    書に追加されます。このようなエントリを追加したくない場合は、
;;    ユーザー変数、`skk-search-excluding-word-pattern-function' を適切に
;;    設定することで、これを実現することができます。詳しくは、
;;    `skk-search-excluding-word-pattern-function' のドキュメントをご覧下さい。

;; (3)(2)で変換した後、更に再帰的な英和変換を行うことができます。

;;    まず、`skk-look-recursive-search' の値を non-nil にセットして下さ
;;    い。Emacs/SKK を再起動する必要はありません。

;;    すると、例えば、

;;    ▽abstr* (SPC)

;;      ---> ▼abstract (SPC) -> ▼アブストラクト (SPC) -> ▼抽象 (SPC)
;;        -> ▼abstraction (SPC) -> ▼アブストラクション

;;    このように英単語 + その英単語を見出し語にした候補の「セット」を変換
;;    結果として出力することができます。

;;    この際、`skk-look-expanded-word-only' の値が non-nil であれば、再帰
;;    検索に成功した英単語の「セット」だけを出力することができます (再帰
;;    検索で検出されなかった英単語は無視して出力しません) 。

;;    もちろん、SKK 辞書に

;;       abstract /アブストラクト/抽象/
;;       abstraction /アブストラクション/

;;    というエントリがあることを前提としています。edict を SKK 辞書形式に
;;    変換すると良いですね。

;; 動作を確認した look は、Slackware 3.5 に入っていた、man page に
;; `BSD Experimental June 14, 1993' と記載のあるもの (バージョン情報がない)
;; です。オプションの指定などが異なる look があれば、ご一報下さい。
;; よろしくお願いいたします。

;; <Dictionary>
;; ftp://ftp.u-aizu.ac.jp:/pub/SciEng/nihongo/ftp.cc.monash.edu.au/
;; に置いてある edict を利用すると手軽に英和辞書ができます。

;;   % jgawk -f skk-10/lisp/look/edict2skk.awk edict > temp
;;   % skkdic-expr temp | skkdic-sort > SKK-JISYO.E2J
;;   % rm temp

;; できた SKK-JISYO.E2J の利用方法は色々ありますが、

;;   % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L\
;;     | skkdic-sort > SKK-JISYO.L

;; などとして、SKK-JISYO.L とマージして使うのが手軽です。

;; <Motivation>
;; このプログラムは、eWnn for Linux/FreeBSD の広告に類似の機能紹介があったのを
;; 見て、「こんな機能なら SKK 上にすぐインプリメントできるさ」と思うとたまらく
;; なって書いてしまいました。eWnn に負けるな、SKK!

;; 昔、Seiichi Namba <sn@asahi-net.email.ne.jp> さんと一緒に Emacs Lisp で
;; look interface を書いたことがあるのですが、今回はその際の経験を生かすことが
;; できました。難波さんに感謝いたします。

;;; Code:

(require 'skk)

(eval-when-compile
  ;; shut up compiler warnings.
  (defvar ispell-process)
  (defvar ispell-filter)
  (defvar ispell-filter))

(eval-and-compile
  (autoload 'ispell-accept-buffer-local-defs "ispell")
  (autoload 'ispell-parse-output "ispell"))

(when (and skk-look-command
           (null (member '(skk-look)
                         (default-value 'skk-search-prog-list))))
  (let ((pl (default-value 'skk-search-prog-list))
        (n 0)
        dic mark)
    (while pl
      (setq dic (car pl))
      (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
          (setq mark n
                pl nil)
        (setq pl (cdr pl)
              n (1+ n))))
    (cond
     (mark
      (skk-splice-in (default-value 'skk-search-prog-list)
                     (1+ mark)
                     '((skk-look))))
     (t
      (setq-default skk-search-prog-list
                    (skk-nunion
                     (default-value 'skk-search-prog-list)
                     '((skk-look))))))))

;; program
;;;###autoload
(defun skk-look (&optional conversion-arguments not-abbrev-only expand-null)
  "UNIX look コマンドを利用して変換する。
SKK abbrev モードにて、英文字 + アスタリスクで uncompleted spelling を指定する。
詳しくは skk-look.el ファイルのコメントや Info を参照の事。
CONVERSION-ARGUMENTS は `skk-look-conversion-arguments' を
一時的に置き換えたい時に指定する。
デフォルトでは SKK abbrev モードのみで有効な機能だが、
NOT-ABBREV-ONLY を指定する事で常に有効となる。
EXPAND-NULL を指定すると、入力が \"*\" のみの時は
words ファイルにある全ての見出しが対象となる。
`skk-look-recursive-search', `skk-look-expanded-word-only',
`skk-look-use-ispell' を一時的に変更したい場合には
`let' により束縛して使う事。"
  (when (and (not (memq skk-use-look '(nil completion)))
             (or not-abbrev-only
                 skk-abbrev-mode)
             (or expand-null
                 (not (string= skk-henkan-key "*")))
             (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
                 ?*))
    (let* ((skk-look-conversion-arguments (or conversion-arguments
                                              skk-look-conversion-arguments))
           (substr (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
           (v (if (and (not (memq skk-look-use-ispell '(nil completion)))
                       (> (length substr) 0))
                  (skk-look-ispell substr 'conversion)
                (skk-look-1 substr 'conversion)))
           skk-henkan-key
           skk-use-look
           v2 v3)
      (cond
       ((not skk-look-recursive-search)
        v)
       (t
        (dolist (key v)
          (let ((skk-current-search-prog-list
                 (copy-sequence skk-search-prog-list)))
            (setq skk-henkan-key key)
            (while skk-current-search-prog-list
              (setq v3 (let (skk-use-numeric-conversion)
                         (skk-search))
                    v2 (if (or (not skk-look-expanded-word-only)
                               v3)
                           (skk-nunion v2 (cons key v3))
                         v2)))))
        v2)))))

(defun skk-look-1 (word situation)
  ;; core search engine
  (let ((format-string (cond ((eq situation 'conversion)
                              skk-look-conversion-arguments)
                             ((eq situation 'completion)
                              skk-look-completion-arguments)
                             (t
                              "%s")))
        args preargs postargs)
    (if (string= format-string "%s")
        (setq args (list word))
      (save-match-data
        (when (string-match "%s" format-string)
          (setq preargs (substring format-string 0 (match-beginning 0))
                postargs (substring format-string (match-end 0))
                args (append
                      (delete "" (split-string preargs " "))
                      (list word)
                      (delete "" (split-string postargs " ")))))))
    (with-temp-buffer
      (when (and (zerop (apply #'call-process skk-look-command nil t nil args))
                 (> (buffer-size) 0))
        (delete word (split-string (buffer-substring-no-properties
                                    (point-min) (1- (point-max)))
                                   "\n"))))))

;;;###autoload
(defun skk-look-completion (&optional completion-arguments not-abbrev-only expand-null)
  "look コマンドを利用して補完候補を得る。
COMPLETION-ARGUMENTS は `skk-look-completion-arguments' を
一時的に置き換えたい時に指定する。
デフォルトでは SKK abbrev モードのみで有効な機能だが、
NOT-ABBREV-ONLY を指定する事で常に有効となる。
EXPAND-NULL を指定すると、入力が空である時に
words ファイルにある全ての見出しを返す。
`skk-look-use-ispell' を一時的に変更したい場合には
`let' により束縛して使う事。"
  (let* ((numericp (and skk-use-numeric-conversion
                        (save-match-data
                          (string-match "[0-9０-９]" skk-comp-key))))
         (conv-key (and numericp
                        (skk-num-compute-henkan-key skk-comp-key)))
         (comp-key (or conv-key skk-comp-key))
         word)
    (when (and (not (memq skk-use-look '(nil conversion)))
               (or not-abbrev-only
                   skk-abbrev-mode)
               (or expand-null
                   (not (string= comp-key ""))))
      (let ((skk-look-completion-arguments (or completion-arguments
                                               skk-look-completion-arguments)))
        (when skk-comp-first
          ;; look は複数の候補を吐くので、一旦貯めておいて、
          ;; 一つずつ complete する。
          (setq skk-look-completion-words
                (if (and (not (memq skk-look-use-ispell '(nil conversion)))
                         (> (length comp-key) 0))
                    (skk-look-ispell comp-key 'completion)
                  (skk-look-1 comp-key 'completion))))
        (setq word (pop skk-look-completion-words))
        (when (and skk-comp-use-prefix
                   (not (string= skk-comp-prefix "")))
          (save-match-data
            (let ((regexp-key (concat "^"
                                      (regexp-quote comp-key)
                                      (skk-comp-get-regexp skk-comp-prefix))))
              (while (and word
                          (not (string-match regexp-key word)))
                (setq word (pop skk-look-completion-words))))))
        (when word
          (if numericp
              (concat skk-comp-key
                      (substring word (length comp-key)))
            word))))))

;;;###autoload
(defun skk-look-ispell (word &optional situation)
  (require 'ispell)
  (ispell-accept-buffer-local-defs)
  (message "")
  (process-send-string ispell-process "%\n") ;put in verbose mode
  (process-send-string ispell-process (concat "^" word "\n"))
  (while (progn
           (accept-process-output ispell-process)
           (not (string= "" (car ispell-filter)))))
  (setq ispell-filter (cdr ispell-filter)) ; remove extra \n
  (let ((poss (when (and ispell-filter
                         (listp ispell-filter))
                ;; 1: t for an exact match.
                ;; 2: A string containing the root word matched via suffix
                ;;    removal.
                ;; 3: A list of possible correct spellings of the format:
                ;;    (ORIGINAL-WORD OFFSET MISS-LIST GUESS-LIST)
                ;;    ORIGINAL-WORD is a string of the possibly misspelled
                ;;    word.
                ;;    OFFSET is an integer giving the line offset of the word.
                ;;    MISS-LIST and GUESS-LIST are possibly null lists of
                ;;    guesses and misses.
                ;; 4: Nil when an error has occurred."
                (or (ispell-parse-output (car ispell-filter))
                    'error)))
        ret var)
    (setq ispell-filter nil)
    (cond
     ((eq poss 'error)
      (skk-message "ispell process でエラーが発生しました"
                   "error in ispell process")
      (sit-for 1)
      (message "")
      nil)
     ((or (eq poss t)
          ;; root word に対して skk-look-1 かけちゃおうか？
          ;; でもちっとも補完ぢゃなくなっちまいますね... (^^;;。
          (stringp poss)
          (null (or (nth 2 poss) (nth 3 poss))))
      (skk-look-1 word situation))
     (t
      (setq var (nconc (nth 2 poss) (nth 3 poss)))
      (dolist (key var)
        ;; call look command by each candidate put out by ispell.
        (setq ret (skk-nunion ret (cons key (skk-look-1 key situation)))))
      (delete word (skk-nunion (skk-look-1 word situation) ret))))))

(provide 'skk-look)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-look.el ends here
