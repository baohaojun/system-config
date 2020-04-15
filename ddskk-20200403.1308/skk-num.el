;;; skk-num.el --- 数値変換のためのプログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000, 2001, 2002
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

;;; Code:

(require 'skk)

(defsubst skk-num-get-suuji (expression alist)
  (cdr (assq expression alist)))

(defsubst skk-num-int-p (num)
  (not (string-match "\\.[0-9]" num)))

;;;###autoload
(defun skk-num-compute-henkan-key (key)
  "KEY の中の連続する数字を現わす文字列を \"#\" に置き換えた文字列を返す。
\"12\" や \"０９\" など連続する数字を 1 つの \"#\" に置き換えることに注意。
置き換えた数字を `skk-num-list' の中にリストの形で保存する。
例えば、KEY が \"へいせい7ねん12がつ\" であれば、\"へいせい#ねん#がつ\"
と変換し、`skk-num-list' に (\"7\" \"12\") というリストを代入する。
辞書の見出し語の検索に使用する。"
  (let ((numexp (if skk-num-convert-float
                    "[0-9]+\\(\\.[0-9]+\\)?"
                  "[0-9]+")))
    ;;(setq skk-noconv-henkan-key key)
    (save-match-data
      ;; 全角数字を ascii 数字に変換する。
      (while (string-match "[０-９]" key)
        (let ((zen-num (match-string 0 key)))
          (setq key (concat (substring key 0 (match-beginning 0))
                            (skk-jisx0208-to-ascii zen-num)
                            (substring key (match-end 0))))))
      ;; 位取りの "," を除去する。
      (while (string-match "[0-9]\\(,\\)[0-9]" key)
        (setq key (concat (substring key 0 (match-beginning 1))
                          (substring key (match-end 1)))))
      ;; 重複を避ける。
      (when (string-match numexp key)
        (setq skk-num-list nil))
      ;; ascii 数字を "#" に置き換え、その数字を skk-num-list の中に保存。
      (while (string-match numexp key)
        (setq skk-num-list (nconc skk-num-list (list (match-string 0 key)))
              key (concat (substring key 0 (match-beginning 0))
                          "#"
                          (substring key (match-end 0)))))))
  key)

;;;###autoload
(defun skk-num-convert (index)
  "INDEX が指す `skk-henkan-list' の要素を数値変換のために加工する。
`skk-henkan-list' の INDEX が指している候補 \(数値変換キーの)\ を
  \"#2\" -> \(\"#2\" .\"一\"\)
のように変換する。"
  (let ((key (skk-get-current-candidate-1 index))
        convlist current)
    (unless (consp key)
      (setq convlist (skk-num-convert-1 key))
      (cond
       ((null convlist)
        nil)
       ;; CONV-LIST の全要素が文字列。
       ((null (memq t (mapcar 'listp convlist)))
        (setq current (mapconcat 'identity convlist ""))
        (if (skk-get-current-candidate-1)
            ;; ("A" "#2" "C") -> ("A" ("#2" ."一") "C")
            (setcar (nthcdr index skk-henkan-list)
                    (cons key current))
          (setq skk-henkan-list
                (nconc skk-henkan-list (list (cons key current))))))
       ;; #4
       (t
        (let ((l (mapcar (lambda (e) (cons key e))
                         (skk-num-flatten-list convlist))))
          (setq current (cdar l))
          (if (and (> index -1)
                   (nth index skk-henkan-list))
              (progn
                (setcar (nthcdr index skk-henkan-list) (car l))
                (setq skk-henkan-list (skk-splice-in
                                       skk-henkan-list
                                       (1+ index)
                                       (cdr l))))
            (setq skk-henkan-list (nconc skk-henkan-list l))
            (skk-num-uniq))))))))

(defun skk-num-convert-1 (key)
  "KEY を `skk-num-list' に従い変換する。
変換後の文字列のパーツを順にならべたリストを返す。例えば
  KEY ::= `平成#0年', return ::= (\"平成\" \"13\" \"年\")"
  (unless (or (not key)
              (consp key))
    (let ((numexp (if skk-num-convert-float
                      ;; "." を含める意図は?
                      "#[.0-9]+" "#[0-9]+"))
          (n 0)
          (workkey key)
          num convnum string convlist beg)
      (save-match-data
        (while (and
                ;; 具体的な数値を保持しているリストを参照する。
                (setq num (nth n skk-num-list))
                (setq beg (string-match numexp workkey)))
          (setq convnum     ; 数値変換された部分の文字列
                ;; 具体的な数字を変換タイプに従い変換する。
                (skk-num-exp
                 num
                 (string-to-number
                  (substring workkey (1+ beg) (match-end 0))))
                ;; 処理された数値キーまでの prefix 文字列
                string (substring workkey 0 beg)
                ;; 未処理の文字列
                workkey (substring workkey (match-end 0))
                n (1+ n))
          ;; 変換された文字と数値変換に関係のない無変換の文字を並べたリスト
          (setq convlist (nconc convlist (list string convnum))))
        (delete "" (nconc convlist (list workkey)))))))

;;;###autoload
(defun skk-num-multiple-convert (&optional count)
  (let ((index skk-henkan-count))
    (catch 'break
      (while (nth index skk-henkan-list)
        (when (and count (> 0 count))
          (throw 'break nil))
        (skk-num-convert index)
        (setq index (1+ index))
        (when count
          (setq count (1- count)))))
    (skk-num-uniq)))

(defun skk-num-rawnum-exp (string)
  (setq string (skk-num-rawnum-exp-1
                string "[０-９][〇一九五三四七二八六]" "#9" 0))
  (setq string (skk-num-rawnum-exp-1
                string "\\(^\\|[^#0-9]\\)\\([0-9]+\\)" "#0" 2))
  (setq string (skk-num-rawnum-exp-1
                string "[０-９]+" "#1" 0))
  (setq string (skk-num-rawnum-exp-1
                string "\\([〇一九五三四七二八六十][十百千万億兆京]\\)+"
                "#3" 0))
  ;; (mapcar 'char-to-string
  ;;         (sort
  ;;          '(?一 ?二 ?三 ?四 ?五 ?六 ?七 ?八 ?九 ?〇) '<))
  ;;   --> ("〇" "一" "九" "五" "三" "四" "七" "二" "八" "六")
  ;;
  ;; [〇-九] という正規表現が使えないので、生のままつっこんでおく。
  (skk-num-rawnum-exp-1 string "[〇一九五三四七二八六]+" "#2" 0))

(defun skk-num-rawnum-exp-1 (string key type place)
  (save-match-data
    (while (string-match key string)
      (setq string (concat (substring string 0 (match-beginning place))
                           type
                           (substring string (match-end place)))))
    string))

(defun skk-num-flatten-list (list)
  "与えられたリストの各要素から組み合せ可能な文字列の連接を作る。
結果はリストで返す。例えば
  ((\"A\" \"B\") \"1\" (\"X\" \"Y\")) -> (\"A1X\" \"A1Y\" \"B1X\" \"B1Y\")"
  (let ((dst (car list)))
    (unless (listp dst)
      (setq dst (list dst)))
    (dolist (elt (cdr list))
      (setq dst
            (cond
             ((consp elt)
              (apply #'nconc
                     (mapcar (lambda (str0)
                               (mapcar (lambda (str1)
                                         (concat str0 str1))
                                       elt))
                             dst)))
             (t
              (mapcar (lambda (str0)
                        (concat str0 elt))
                      dst)))))
    dst))

;;;###autoload
(defun skk-num-exp (num type)
  "ascii 数字 (string) の NUM を TYPE に従って変換した文字列を返す。
TYPE は下記の通り。
0 -> 無変換
1 -> 全角数字へ変換
2 -> 漢数字 (位取りあり) へ変換。例;1024 -> 一〇二四
3 -> 漢数字 (位取りなし) へ変換。例;1024 -> 千二十四
4 -> その数字そのものをキーにして辞書を再検索
5 -> 漢数字 (手形などで使用する文字を使用) へ変換
8 -> 桁区切りへ変換 (1,234,567)
9 -> 将棋で使用する数字 (\"３四\" など) へ変換"
  (save-match-data
    (let ((fun (cdr (assq type skk-num-type-alist))))
      (when fun
        (funcall fun num)))))

(defun skk-num-jisx0208-latin (num)
  "ascii 数字の NUM を全角数字の文字列に変換し、変換後の文字列を返す。
例えば \"45\" を \"４５\" に変換する。"
  (let ((candidate
         (mapconcat (lambda (c)
                      (skk-num-get-suuji c skk-num-alist-type1))
                    num "")))
    (unless (string= candidate "")
      candidate)))

(defun skk-num-type2-kanji (num)
  "ascii 数字 NUM を漢数字の文字列 (位取りあり) に変換し、変換後の文字列を返す。
例えば、\"1024\" を \"一〇二四\" に変換する。"
  (save-match-data
    (when (skk-num-int-p num)
      (let ((candidate
             (mapconcat (lambda (c)
                          (skk-num-get-suuji
                           c
                           skk-num-alist-type2))
                        num "")))
        (unless (string= candidate "")
          candidate)))))

(defun skk-num-type3-kanji (num)
  "ascii 数字 NUM を漢数字の文字列 (位取りなし) に変換し、変換後の文字列を返す。
例えば \"1024\" を \"千二十四\" に変換する。"
  (save-match-data
    (when (skk-num-int-p num)
      ;; 小数点を含まない数
      (skk-num-to-kanji num 'type3))))

(defun skk-num-type5-kanji (num)
  "ascii 数字 NUM を漢数字の文字列に変換し、変換後の文字列を返す。
例えば \"1021\" を \"壱阡弐拾壱\" に変換する。"
  (save-match-data
    (when (skk-num-int-p num)
      ;; 小数点を含まない数
      (skk-num-to-kanji num 'type5))))

(defun skk-num-to-kanji (num type &optional alist)
  "NUM を TYPE の形式の漢数字にする。位などを表す漢字は ALIST から取得する。"
  (let ((len (length num))
        (i 0)
        char v num1 v1)
    (unless alist
      (setq alist
            (symbol-value
             (intern (format "skk-num-alist-%s" type)))))
    ;; 「千京」までは出力する。
    (when (> len 20)
      (skk-error "位が大きすぎます！" "Too big number!"))
    (setq num (append num nil))
    (cond
     ((<= len 4)
      (while (setq char (car num))
        ;; 位:   一  十  百  千
        ;; len:   1   2   3   4
        (cond
         ((= len 1)
          ;; 位を表わす漢数字以外の漢数字。
          (unless (eq char ?0)
            ;; 一の位で 0 でない数。
            (setq v (concat v (skk-num-get-suuji char alist)))))
         (t
          ;; 位を表わす漢数字以外の漢数字。
          (unless (or (and (eq type 'type3)
                           (memq char '(?0 ?1)))
                      (and (eq type 'type5)
                           (eq char ?0)))
            ;; type3 のときは、十の位以上で、かつ 0, 1 以外の数字。
            ;; type5 のときは、十の位以上で、かつ 0 以外の数字。
            (setq v (concat v (skk-num-get-suuji char alist))))
          ;; 位を表わす漢数字。
          (when (and (not (eq char ?0)) (< 1 len))
            (setq v
                  (concat
                   v
                   (skk-num-get-suuji
                    (cond ((eq len 2) 'ju) ; 十
                          ((eq len 3) 'hyaku) ; 百
                          (t 'sen)) ; 千
                    alist))))))
        (setq len (1- len) num (cdr num))))
     (t
      (setq num (nreverse num))
      (while num
        (setq num1 nil)
        (while (and (< (length num1) 4) num)
          (setq num1 (cons (car num) num1)
                num (cdr num)))
        (when num1
          (setq v1 (skk-num-to-kanji num1 type alist))
          (when (string= v1 (skk-num-get-suuji ?0 alist))
            (setq v1 ""))
          (when (and (eq type 'type3) (eq i 1) (equal v1 "千"))
            ;; 日本語では「千億」という表現はときに使われるが、「千万」という表
            ;; 現はまず使われないので、「一千万」に直す。
            (setq v1 (concat "一" v1)))
          (setq
           v
           (concat
            v1
            (when v1
              (skk-num-get-suuji
               (cond ((eq i 0) ?\ )
                     ((eq i 1) 'man) ; 万
                     ((eq i 2) 'oku) ; 億
                     ((eq i 3) 'cho) ; 兆
                     ((eq i 4) 'kei)) ; 京
               alist))
            v)))
        (setq i (1+ i)))))
    ;;
    (or v
        (skk-num-get-suuji ?0 alist))))

(defun add-number-grouping (number &optional separator places)
  ;; http://www.emacswiki.org/cgi-bin/wiki/AddCommasToNumbers
  ;; and 玉野健一 <suzu@a7m3.jp>
  "Add commas to NUMBER and return it as a string.
    Optional SEPARATOR is the string to use to separate groups.
    It defaults to a comma.
    PLACES is the number of places of a group.
    It defaults to three."
  (let ((num (number-to-string number))
        (op (or separator
                ","))
        (pl (or (if (< places 1) 3 places)
                3)))
    (while (string-match (concat "\\(.*[0-9]\\)\\([0-9]\\{"
                                 (number-to-string pl)
                                 "\\}.*\\)")
                         num)
      (setq num (concat (match-string 1 num)
                        op
                        (match-string 2 num))))
    num))

(defun skk-num-grouping (num)
  "ascii 数字の NUM を桁区切りへ変換し、変換後の文字列を返す。
例えば \"1234567\" を \"1,234,567\" へ変換する。
区切る記号は `skk-num-grouping-separator' で、区切る桁数は `skk-num-grouping-places' で指定する。"
  (add-number-grouping (string-to-number num)     ; number
                       skk-num-grouping-separator ; `,'
                       skk-num-grouping-places    ; 3
                       ))

(defun skk-num-shogi (num)
  "ascii 数字の NUM を将棋で使用される数字表記に変換する。
例えば \"34\" を \"３四\" に変換する。"
  (save-match-data
    (when (and (= (length num) 2)
               (skk-num-int-p num))
      (let ((candidate
             (concat (skk-num-get-suuji (aref num 0) skk-num-alist-type1)
                     (skk-num-get-suuji (aref num 1) skk-num-alist-type2))))
        (unless (string= candidate "")
          candidate)))))

(defun skk-num-recompute (num)
  "#4 の見出しに対し `skk-henkan-key' に代入された数字そのものを再度検索する。"
  (let (result)
    (setq skk-num-recompute-key num)
    (with-temp-buffer
      ;; カレントバッファのバッファローカル変数に影響を及ぼさないよう、
      ;; ワーキングバッファへ一旦逃げる
      (let ((skk-current-search-prog-list skk-search-prog-list)
            (skk-henkan-key num)
            ;; カレントの変換は送りなし (skk-henkan-okurigana と
            ;; skk-okuri-char はいずれも nil) だが、別バッファ
            ;;  (work バッファ) に入っているので、念のため、nil を
            ;; 入れておく。
            skk-henkan-okurigana skk-okuri-char skk-use-numeric-conversion)
        (while skk-current-search-prog-list
          (setq result (skk-nunion result (skk-search))))))
    ;; ここで temp-buffer を出て変換を行っているカレントバッファに戻る
    ;; (バッファローカル値である skk-henkan-list を操作したいため)。
    (cond
     ((not result)
      ;; 変換できなかったら元の数字をそのまま返しておく。
      num)
     ((null (cdr result));;(= (length result) 1)
      (car result))
     (t
      result))))

;;;###autoload
(defun skk-num-uniq ()
  (when (and skk-num-uniq
             skk-henkan-list)
    (save-match-data
      (let ((n1 -1)
            n2
            e1 e2 e3
            ;; 1 つでも 2 桁以上の数字があれば、#2 と #3 では uniq しない。
            (type2and3 (> 2 (apply 'max (mapcar 'length skk-num-list))))
            type2 type3
            index2 index3
            head2 head3
            tail2 tail3
            case-fold-search)
        (while (setq n1 (1+ n1) e1 (nth n1 skk-henkan-list))
          ;; cons cell でなければ skk-nunion で処理済みなので、重複はない。
          (when (consp e1)
            (setq skk-henkan-list (delete (car e1) skk-henkan-list)
                  skk-henkan-list (delete (cdr e1) skk-henkan-list)))
          (when (and skk-num-recompute-key (consp e1))
            ;; ("#4" . "xxx") を含む候補が skk-henkan-list の中にある。
            (setq n2 -1)
            (while (setq n2 (1+ n2) e2 (nth n2 skk-henkan-list))
              (when (and (not (= n1 n2)) (consp e2)
                         ;; 例えば ("#4" . "一") と ("#2" . "一") が
                         ;; 並存している場合。
                         (string= (cdr e1) (cdr e2)))
                (setq skk-henkan-list (delq e2 skk-henkan-list)))))
          (when type2and3
            ;; 1 桁の数字を変換する際に、skk-henkan-list に #2 エントリと #3
            ;; エントリがあれば、#2 もしくは #3 エントリのうち、より後方にある
            ;; ものを消す。
            (setq e3 (if (consp e1) (car e1) e1))
            ;; e3 は "#2" のように数値変換を示す文字列のみとは限らないので、
            ;; member は使えない。
            (cond ((string-match "#2" e3)
                   (setq type2 e1
                         index2 n1
                         head2 (substring e3 0 (match-beginning 0))
                         tail2 (substring e3 (match-end 0))))
                  ((string-match "#3" e3)
                   (setq type3 e1
                         index3 n1
                         head3 (substring e3 0 (match-beginning 0))
                         tail3 (substring e3 (match-end 0)))))))
        (when (and type2and3 type2 type3
                   ;; 数値変換を示す文字列 "#[23]" の前後の文字列も同一のと
                   ;; きのみ uniq を行う。
                   (string= head2 head3) (string= tail2 tail3))
          (if (> index2 index3)
              ;; "#3" の方が前にある。
              (setq skk-henkan-list (delq type2 skk-henkan-list))
            ;; 変数 type[23] の値は、skk-henkan-list から直接抽出したも
            ;; のだから delete でなく、delq で十分。
            (setq skk-henkan-list (delq type3 skk-henkan-list))))))))

;;;###autoload
(defun skk-num-initialize ()
  "`skk-use-numeric-conversion' 関連の変数を初期化する。"
  (setq skk-last-henkan-data (skk-put-alist 'num-list skk-num-list skk-last-henkan-data)
        skk-num-list nil
        skk-num-recompute-key nil))

;;;###autoload
(defun skk-num-henkan-key ()
  "適切な変換キーを返す。
type4 の数値再変換が行われたときは、数値自身を返し、それ以外の数値変換
では、`skk-henkan-key' の数値を \"#\" で置き換えたキーを返す。"
  (or skk-num-recompute-key
      (skk-num-compute-henkan-key skk-henkan-key)))

;;;###autoload
(defun skk-num-update-jisyo (noconvword word &optional purge)
  "数字自身を見出し語として辞書をアップデートする。"
  (when (and skk-num-recompute-key
             (save-match-data (string-match "#4" noconvword)))
    (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
      (let ((skk-henkan-key skk-num-recompute-key)
            skk-use-numeric-conversion)
        ;;(message "%S" skk-num-recompute-key)
        (skk-update-jisyo word purge)))))

;;;###autoload
(defun skk-num (str)
  "数字を `skk-number-style' の値に従い変換する。
`skk-current-date' のサブルーチン。"
  (mapconcat (lambda (c)
               (cond
                ((or (< ?9 c) (< c 0))
                 nil)
                ((or (not skk-number-style)
                     (and (numberp skk-number-style)
                          (= skk-number-style 0)))
                 (char-to-string c))
                ((or (eq skk-number-style t)
                     (and (numberp skk-number-style)
                          (= skk-number-style 1)))
                 (cdr (assq c skk-num-alist-type1)))
                (t
                 (cdr (assq c skk-num-alist-type2)))))
             str ""))

(defadvice skk-kakutei-initialize (after skk-num-ad activate)
  (when (skk-numeric-p)
    (skk-num-initialize)))

(run-hooks 'skk-num-load-hook)

(provide 'skk-num)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-num.el ends here
