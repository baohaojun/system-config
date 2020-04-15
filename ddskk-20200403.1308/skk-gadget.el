;;; skk-gadget.el --- 実行変換のためのプログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

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

;; 2014.12.30 SKK-JISYO.lisp
;;   SKK-JISYO.L からプログラム実行変換を使用しているエントリを抜き出して
;;   SKK-JISYO.lisp とした。ただし、concat 関数に限っては SKK-JISYO.L に
;;   残している。 http://mail.ring.gr.jp/skk/201412/msg00095.html

;; プログラム実行変換とは
;; ======================
;; 送り仮名のない辞書の変換の候補に Emacs Lisp のコードが書いてあれば、SKK
;; はそのコードを Lisp のプログラムとして実行し、その結果の文字列を画面に挿
;; 入する。例えば、辞書に

;;         now /(current-time-string)/

;; という行があるとき、`/now ' とタイプすれば画面には現在の時刻が表示され、
;; `▼Fri Apr 10 11:41:43 1992' のようになる。

;; ここで使える Lisp のコードは
;;   o 文字列を返すこと
;;   o 改行を含まないこと
;; に限られる。

;; このファイルは実行変換プログラムを集めたものである。

;; skk-gadget.el の `gadget' は「上手く工夫した道具」の意味。「色々飛び出す
;; 気のきいたおもちゃ箱」というような意味で名付けられた。
;; 余談だが、X Window で使用される `Widget' という言葉は、`window'+`gadget'
;; から作られた造語らしい。


;; 辞書エントリ today の解説
;; ======================
;; today /(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) )/

;; 1. lambda() 全体を引数として skk-current-date() を実行する

;; 2. skk-current-date() は、
;;   a. skk-current-date-1() => ("2013" "Jan" "29" "Tue" "22" "59" "50")
;;   b. format               => nil
;;   c. (not skk-date-ad)    => t=西暦, nil=元号
;;   d. and-time             => nil
;;  を引数として lambda() を funcall する

;; 3. すなわち、lambda() 自身の引数 `date-information' `format' `gengo' `and-time' は、
;;  上記の a 〜 d である

;; 4. 最終的に skk-default-current-date() が実行される
;;   e. date-information  [date-information] ... ("2013" "Jan" "29" "Tue" "22" "59" "50")
;;   f. nil               [format]           ... %s年%s月%s日(%s)%s時%s分%s秒
;;   g. 3                 [num-type]         ... 変換タイプ => 漢数字
;;   h. 'gengo            [gengo]            ... non-nil => 平成二十五年
;;                                                   nil => 二千十三年
;;   i. 0                 [gengo-index]      ...
;;   j. 0                 [month-alist-index] ...
;;   k. 0                 [dayofweek-alist-index] ...
;;   opt. and-time

;; 5. *scratch* で評価してみる
;;   (skk-default-current-date
;;     '("2013" "Jan" "29" "Tue" "22" "59" "50") nil 3 t 0 0 0)  [C-j]
;;   => "平成二十五年一月二十九日(火)"
;;      これ↑が、発端である skk-current-date() の戻り値である

;;; Code:

(require 'skk)

(eval-when-compile
  (require 'cl-lib))

;; -- programs
;;;###autoload
(defun skk-current-date (&optional pp-function format and-time)
  "`current-time-string' の出力を加工し、現在の日時 \(string\) を返す。
オプショナル引数の PP-FUNCTION を指定すると、
  `skk-current-date-1' の返り値、 FORMAT 、 AND-TIME
を引数にして PP-FUNCTION を `funcall' する。
PP-FUNCTION が nil の場合は `skk-default-current-date-function' を
`funcall' する。
FORMAT は `format' の第１引数の様式 \(string\) による出力指定テンプレート。
AND-TIME \(boolean\) を指定すると時刻も返す。
`skk-today' と `skk-clock' のサブルーチンである。"
  (funcall (if pp-function
               pp-function
             skk-default-current-date-function)
           (skk-current-date-1) format (not skk-date-ad) and-time))

(defun skk-current-date-1 (&optional specified-time)
  "`current-time-string' の出力を加工し、日付・時刻情報をリストにして返す。
\(year month day day-of-week hour minute second\)
\(\"2013\" \"Jan\" \"29\" \"Tue\" \"22\" \"41\" \"11\"\)

オプショナル引数の SPECIFIED-TIME は `current-time-string' の docstring
を参照のこと。"
  (cl-multiple-value-bind (dow month day time year)
      (split-string (current-time-string specified-time))
    (append (list year month day dow)
            (split-string time ":"))))

;;;###autoload
(defun skk-default-current-date
    (date-information
     format num-type gengo gengo-index month-alist-index dayofweek-alist-index
     &optional and-time)
  "\
日付情報の標準的な出力をする他、ユーザにある程度のカスタマイズ機能を提供する。
この関数の引数でカスタマイズできない出力を希望する場合は、
`skk-default-current-date-function' に自前の関数を指定する。

DATE-INFORMATION は

  \(year month day day-of-week hour minute second\)

の形式のリスト。各要素は文字列。`skk-current-date-1' の出力を使用。

FORMAT は `format' の第１引数の様式による出力形態を指定する文字列。
  nil であれば \"%s年%s月%s日\(%s\)%s時%s分%s秒\" \(もしくは
  \"%s年%s月%s日\(%s\)\" が使われる。

NUM-TYPE \(number\) は
  0 -> 無変換
  1 -> 全角数字へ変換
  2 -> 漢数字へ変換 \(位取りなし\)
  3 -> 漢数字へ変換 \(位取りをする\)
  4 -> その数字そのものをキーにして辞書を再検索
  5 -> 漢数字 \(手形などで使用する文字を使用\)へ変換 \(位取りをする\)
  9 -> 将棋で使用する数字 \(\"３四\" など\) に変換

GENGO は元号表示するかどうか \(boolean\)。

GENGO-INDEX は `skk-gengo-alist' の各要素の cadr を 0 とする index
 \(number\)。nil であれば `current-time-string' の出力のまま無変換。

MONTH-ALIST-INDEX は `skk-month-alist' の各要素の cadr を 0 とする
 index \(number\)。nil であれば `current-time-string' の出力のまま無変換。

DAYOFWEEK-ALIST-INDEX は `skk-day-of-week-alist' の各要素の cadr を
 0 とする index \(number\)。nil であれば `current-time-string' の出力のま
ま無変換。

AND-TIME は時刻も表示するかどうか \(boolean\)。"
  (cl-multiple-value-bind (year month day day-of-week hour minute second v)
      date-information
    (when gengo
      (setq v (skk-ad-to-gengo-1
               (string-to-number year) nil
               (string-to-number (nth 0 (cdr (assoc month skk-month-alist))))
               (string-to-number day))))
    (setq year (if gengo
                   (concat (if gengo-index
                               (nth gengo-index (car v))
                             (caar v))
                           (if (numberp (cdr v))
                               (skk-num-exp (number-to-string (cdr v))
                                            num-type)
                             (cdr v)))
                 (skk-num-exp year num-type)))
    (when month-alist-index
      (setq month (skk-num-exp (nth month-alist-index
                                    (cdr (assoc month skk-month-alist)))
                               num-type)))
    (setq day (skk-num-exp day num-type))
    (when dayofweek-alist-index
      (setq day-of-week (nth dayofweek-alist-index
                             (cdr (assoc day-of-week skk-day-of-week-alist)))))
    (when and-time
      (setq hour (skk-num-exp hour num-type))
      (setq minute (skk-num-exp minute num-type))
      (setq second (skk-num-exp second num-type)))
    (if and-time
        (format (or format "%s年%s月%s日(%s)%s時%s分%s秒")
                year month day day-of-week hour minute second)
      (format (or format "%s年%s月%s日(%s)") year month day day-of-week))))

;;;###autoload
(cl-defun skk-relative-date (pp-function format and-time &key (yy 0) (mm 0) (dd 0))
  "`skk-current-date' の拡張版。PP-FUNCTION, FORMAT, AND-TIME は `skk-current-date' を参照のこと。
実行例
 (skk-relative-date) => \"平成25年2月03日(日)\"
 (skk-relative-date (lambda (arg) body) nil nil :dd -1) => \"平成25年2月02日(土)\"
 (skk-relative-date (lambda (arg) body) nil nil :mm -1) => \"平成25年1月03日(木)\"
 (skk-relative-date (lambda (arg) body) nil nil :yy  2) => \"平成27年2月03日(火)\"
"
  (let ((specified-time (cl-multiple-value-bind (sec min hour day month year dow dst zone)
                            (decode-time)
                          (encode-time sec min hour
                                       (+ day   dd)
                                       (+ month mm)
                                       (+ year  yy)))))
    (funcall (if pp-function
                 pp-function
               skk-default-current-date-function)
             (skk-current-date-1 specified-time)
             format (not skk-date-ad) and-time)))

(defun skk-today-execute-char ()
  (let (char)
    (dolist (list (nth 4 skk-rule-tree))
      (when (memq 'skk-today list)
        (setq char (char-to-string (car list)))))
    char))

;;;###autoload
(defun skk-today (arg)
  "\
`current-time-string' の出力を加工し、現在の日時を表す文字列を作り、挿入
する。実質的に「today エントリの呼び出し」だけなので、カスタマイズは個人
辞書の today エントリによる。"
  (interactive "p")
  (if (and (eq skk-henkan-mode 'on) ;▽モード
           (equal (this-command-keys) (skk-today-execute-char))
           (< skk-henkan-start-point (point)))
      (this-command-keys)
    ;;
    (unless skk-mode
      (skk-mode 1))
    (skk-set-henkan-point-subr)
    (insert "today")
    (skk-start-henkan arg)))

;;;###autoload
(defun skk-clock (&optional kakutei-when-quit time-signal)
  "ミニバッファにデジタル時計を表示する。
quit すると、その時点の日時を候補として挿入する。
quit したときに起動してからの経過時間をミニバッファに表示する。
interactive に起動する他、\"clock /(skk-clock)/\" などのエントリを SKK の辞書
に加え、\"/clock\"+ SPC で変換することによっても起動可。\\[keyboard-quit] で止まる。
実行変換で起動した場合は、\\[keyboard-quit] した時点の時点の日時を挿入する。

オプショナル引数の KAKUTEI-WHEN-QUIT が non-nil であれば \\[keyboard-quit] したときに確定する。
オプショナル引数の TIME-SIGNAL が non-nil であれば、NTT の時報風に ding する。
それぞれ \"clock /(skk-clock nil t)/\" のようなエントリを辞書に挿入すれば良い。
`skk-date-ad' と `skk-number-style' によって表示方法のカスタマイズが可能。"
  (interactive "*")
  (let ((start (current-time))
        end
        mes
        expr1 expr2
        sec
        snd)
    (cond
     ((or (not skk-number-style)
          (eq skk-number-style 0))
      (setq expr1 "[789]秒"
            expr2 "0秒"))
     ((or (eq skk-number-style t)
          ;; skk-number-style に 数字と t 以外の non-nil 値を入れている場
          ;; 合、= を使うと Wrong type argument: number-or-marker-p, xxxx
          ;; になってしまう。
          (eq skk-number-style 1))
      (setq expr1 "[７８９]秒"
            expr2 "０秒"))
     (t
      (setq expr1 "[七八九]秒"
            expr2 "〇秒")))

    (save-match-data
      (condition-case nil
          (let (case-fold-search
                inhibit-quit
                visible-bell)
            (while (not quit-flag)
              (setq mes (skk-current-date nil nil t)
                    sec 0)
              (message "%s    Hit any key to quit" mes)
              (when time-signal
                (cond
                 ((string-match expr1 mes)
                  ;; [7890] のように正規表現を使わず、7 だけで全てのマシンが
                  ;; 着いてゆけば良いのだが...。丁度この関数実行時に Garbage
                  ;; collection が呼ばれても表示される数字が飛ぶ場合がある。
                  (ding))
                 ((string-match expr2 mes)
                  ;; 0 だけ「ポ〜ン」といきたいところですが、マシンによっ
                  ;; て差がある。
                  ;; 386SX 25Mhz + Mule-2.x だと「ピッ、ピッ」という感じ。
                  ;; 付いてゆくのが非常に辛い。68LC040 33Mhz + NEmacs だと
                  ;; 「ピピッ」となり、音のタイミングは良いのだが、とき
                  ;; どき 1 秒分ついていけなくなる。Pentium 90Mhz +
                  ;; Mule-2.xだと「ピッ」という単音になってしまう... (;_;)。
                  (if snd
                      (ding)
                    (ding)
                    (unless (sit-for (setq sec (+ sec 0.16))
                                     'nodisplay)
                      (read-event)
                      (signal 'quit nil))
                    (ding)))))
              (unless (sit-for (- 1 sec) 'nodisplay)
                (read-event)
                (signal 'quit nil))))
        (quit
         (prog2
             (setq end (current-time))
             (skk-current-date nil nil t)
           (when kakutei-when-quit
             (setq skk-kakutei-flag t))
           (message "経過時間: %s 秒"
                    (skk-time-difference start end))))))))

;;;###autoload
(defun skk-ad-to-gengo (gengo-index &optional divider tail not-gannen)
  ;; 西暦を元号に変換する。オプショナル引数の DIVIDER が指定されていれば、年号
  ;; と数字の間に、TAIL が指定されていれば、数字の末尾に、それぞれの文字列を連
  ;; 結する。
  ;; 辞書見出し例;
  ;; せいれき#ねん /(skk-ad-to-gengo 0 nil "年")/(skk-ad-to-gengo 0 " " " 年")/
  (let ((v (skk-ad-to-gengo-1 (string-to-number
                               (car skk-num-list))
                              not-gannen)))
    (concat (nth gengo-index (car v))
            divider
            (if (not (stringp (cdr v)))
                (number-to-string (cdr v))
              (cdr v))
            tail)))

;;;###autoload
(defun skk-ad-to-gengo-1 (ad &optional not-gannen month day)
  ;; AD is a number and NOT-GANNEN is a boolean optional
  ;; arg.
  ;; return a cons cell of which car is a Gengo list
  ;; gotten from `skk-gengo-alist', and cdr is a number
  ;; of year.
  ;; if NOT-GANNEN is non-nil and calculated year is 1,
  ;; return a value of which cdr is "元" (string).
  (when (>= 1866 ad)
    (skk-error "分りません" "Unknown year"))
  (cons (cond ((or (< ad 1912) (and (= ad 1912) month (< month 7))
                   (and (= ad 1912) month (= month 7) day (< day 30)))
               (setq ad (- ad 1867))
               (cdr (assq 'meiji skk-gengo-alist)))
              ((or (< ad 1926) (and (= ad 1926) month (< month 12))
                   (and (= ad 1926) month (= month 12) day (< day 25)))
               (setq ad (- ad 1911))
               (cdr (assq 'taisho skk-gengo-alist)))
              ((or (< ad 1989)
                   (and (= ad 1989) month (= month 1) day (< day 8)))
               (setq ad (- ad 1925))
               (cdr (assq 'showa skk-gengo-alist)))
              ((or (< ad 2019) (and (= ad 2019) month (< month 5)))
               (setq ad (- ad 1988))
               (cdr (assq 'heisei skk-gengo-alist)))
              (t
               (setq ad (- ad 2018))
               (cdr (assq 'reiwa skk-gengo-alist))))
        (cond (not-gannen ad)
              ((= ad 1) "元")
              (t ad))))

;;;###autoload
(defun skk-gengo-to-ad (&optional head tail)
  ;; 元号を西暦に変換する。オプショナル引数の HEAD, TAIL が指定されてい
  ;; れば、その文字列を先頭、末尾に連結する。
  ;; 辞書見出し例;
  ;;   しょうわ#ねん /(skk-gengo-to-ad "" "年")/(skk-gengo-to-ad "" " 年")/\
  ;;   (skk-gengo-to-ad "西暦" "年")/(skk-gengo-to-ad "西暦" " 年")/
  (save-match-data
    (when (string-match (car skk-num-list)
                        skk-henkan-key)
      (let ((v (skk-gengo-to-ad-1
                (substring skk-henkan-key 0 (match-beginning 0))
                (string-to-number (car skk-num-list)))))
        (when v
          (concat head
                  (number-to-string v)
                  tail))))))

;;;###autoload
(defun skk-gengo-to-ad-1 (gengo number)
  ;; GENGO is a string and NUMBER is a number.
  ;; return a year (number) equal to GENGO-NUMBER.
  (+ number
     (cond
      ((eq number 0)
       (skk-error "0 年はあり得ない"
                  "Cannot convert 0 year"))
      ((member gengo '("れいわ" "令和"))
       2018)
      ((member gengo '("へいせい" "平成"))
       1988)
      ((member gengo '("しょうわ" "昭和"))
       1925)
      ((member gengo '("たいしょう" "大正"))
       1911)
      ((member gengo '("めいじ" "明治"))
       1867)
      (t
       (skk-error "判別不能な元号です！"
                  "Unknown Gengo!")))))

;;;###autoload
(defun skk-calc (operator)
  ;; 2 つの引数を取って operator の計算をする。
  ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
  ;; skk-calc に渡す。
  ;; 辞書見出し例; #*# /(skk-calc '*)/
  (number-to-string (apply operator
                           (mapcar 'string-to-number
                                   skk-num-list))))

;;;###autoload
(defun skk-plus ()
  ;; 辞書見出し例; #+#+# /(skk-plus)/
  (skk-calc '+))

;;;###autoload
(defun skk-minus ()
  (skk-calc '-))

;;;###autoload
(defun skk-times ()
  (skk-calc '*))

;;;###autoload
(defun skk-ignore-dic-word (&rest no-show-list)
  ;; 共用辞書に登録されている、違っている/気に入らない変換を出さないようにす
  ;; る。
  ;; 辞書見出し例;
  ;;   るすばん /留守番/(skk-ignore-dic-word "留守電")/
  ;;   かくてい /(skk-ignore-dic-word "確定")/
  (let (new-word)
    ;; skk-ignore-dic-word 自身のエントリを消す。消すべき候補は
    ;; skk-henkan-list から直接抽出しているので delete ではなく delq で十分。
    (setq skk-henkan-list (delq (nth skk-henkan-count skk-henkan-list)
                                skk-henkan-list))
    ;; 全候補を skk-henkan-list に入れる。
    (while skk-current-search-prog-list
      (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))))
    ;; 不要な候補を捨てる。
    (while no-show-list
      (setq skk-henkan-list (delete (car no-show-list) skk-henkan-list)
            no-show-list (cdr no-show-list)))
    ;; カレントの候補 (skk-ignore-dic-word 自身のエントリ) を消したので、
    ;; skk-henkan-count は次の候補を指している。
    (setq new-word (or (nth skk-henkan-count skk-henkan-list)
                       (skk-henkan-in-minibuff)))
    ;; 候補がないとき。
    (unless new-word
      ;; 空文字列が登録されたら辞書登録の前の状態に戻す。
      ;; (nth -1 '(A B C)) は、A を返すので、n が負の数でないことをチェック
      ;; しておく必要がある。
      (if (> skk-henkan-count 0)
          (setq skk-henkan-count (- skk-henkan-count 1)
                new-word (nth skk-henkan-count skk-henkan-list))
        ;; (1- skk-henkan-count) == -1 になる。▽モードに戻す。
        (throw 'next-word 'none)))
    ;;
    (throw 'next-word new-word)))

;;;###autoload
(defun skk-henkan-face-off-and-remove-itself ()
  ;; skk-insert-new-word-function にセットするための関数。カレントバッファの
  ;; 変換部分が Overlay の face 属性によって表示が変更されているのを戻し、その
  ;; 後自分自身を skk-insert-new-word-function から取り除く自爆関数。
  (skk-henkan-face-off)
  (setq skk-insert-new-word-function nil))

;;;###autoload
(defun skk-gadget-units-conversion (unit-from number unit-to)
  "`skk-units-alist'を参照し、換算を行う。
NUMBER について UNIT-FROM から UNIT-TO への換算を行う。"
  (let ((v (assoc unit-to
                  (cdr (assoc unit-from skk-units-alist)))))
    (when v
      (concat (number-to-string (* number (cdr v)))
              (car v)))))

(run-hooks 'skk-gadget-load-hook)

(provide 'skk-gadget)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-gadget.el ends here
