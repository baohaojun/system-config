;;; skk-kakasi.el --- KAKASI 関連プログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1998, 1999, 2000, 2001
;;   NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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

;; skk-kakasi.el は KAKASI を SKK の中から使うインターフェイスです。KAKASI は、
;; 高橋裕信さん <hironobu@trc.rwcp.or.jp> による、「漢字かなまじり文をひらが
;; な文やローマ字文に変換することを目的として作成したプログラムと辞書の総称」
;; です。私自身がニュースやメールを読んでいて、日常読みが分らなくて恥ずかしい
;; 思いをすることが多いので、逆引きをしたくて作りました。

;; KAKASI は、

;;   ftp://kakasi.namazu.org/pub/kakasi/

;; にあり anonymous ftp で入手できます。

;; 素晴しいプログラム KAKASI をお作りになった高橋さんに感謝いたします。

;;; Code:

(require 'skk)

(let ((euc (cdr (assoc "euc" skk-coding-system-alist))))
  (modify-coding-system-alist 'process "kakasi" (cons euc euc)))

;;;; FUNCTIONS
;;;###autoload
(defun skk-gyakubiki-region (start end &optional all)
  "領域の漢字、送り仮名を全てひらがなに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}"
  (interactive "*r\nP")
  (let ((str (skk-gyakubiki-1 start end all)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-gyakubiki-and-henkan (start end)
  "領域の漢字をひらがなに変換し、そのひらがなを見出し語としてかな漢字変換を実行する。"
  (interactive "*r")
  (let ((str (skk-gyakubiki-1 start end nil)))
    (delete-region start end)
    (skk-set-henkan-point-subr)
    (insert-and-inherit str)
    (skk-start-henkan nil)))

;;;###autoload
(defun skk-gyakubiki-message (start end &optional all)
  "領域の漢字、送り仮名を全てひらがなに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}"
  (interactive "r\nP")
  (let ((str (skk-gyakubiki-1 start end all)))
    (save-match-data
      (when (string-match "^[ 　\t]+" str)
        ;; 先頭の空白を取り除く。
        (setq str (substring str (match-end 0)))))
    (message "%s" str)))

;;;###autoload
(defun skk-gyakubiki-katakana-region (start end &optional all)
  "領域の漢字、送り仮名を全てカタカナに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}"
  (interactive "*r\nP")
  (let ((str (skk-gyakubiki-1 start end all 'katakana)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-gyakubiki-katakana-message (start end &optional all)
  "領域の漢字、送り仮名を全てカタカナに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}"
  (interactive "r\nP")
  (let ((str (skk-gyakubiki-1 start end all 'katakana)))
    (save-match-data
      (when (string-match "^[ 　\t]+" str)
        ;; 先頭の空白を取り除く。
        (setq str (substring str (match-end 0)))))
    (message "%s" str)))

(defun skk-gyakubiki-1 (start end all &optional katakana)
  ;; skk-gyakubiki-* のサブルーチン。
  ;; オプショナル引数の KATAKANA が non-nil であれば、カタカナへ変換する。
  (let ((arg (if katakana '("-JK") '("-JH"))))
    (when skk-allow-spaces-newlines-and-tabs
      (setq arg (cons "-c" arg)))
    (when all
      (setq arg (cons "-p" arg)))
    (skk-kakasi-region start end arg)))

;;;###autoload
(defun skk-hurigana-region (start end &optional all)
  "領域の漢字に全てふりがなを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    \"中島\" -> \"中島[{なかしま|なかじま}]\""
  (interactive "*r\nP")
  (let ((str (skk-hurigana-1 start end all)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-hurigana-message (start end &optional all)
  "領域の漢字に全てふりがなを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    \"中島\" -> \"中島[{なかしま|なかじま}]\""
  (interactive "r\nP")
  (message "%s" (skk-hurigana-1 start end all)))

;;;###autoload
(defun skk-hurigana-katakana-region (start end &optional all)
  "領域の漢字に全てフリガナを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    \"中島\" -> \"中島[{ナカシマ|ナカジマ}]\""
  (interactive "*r\nP")
  (let ((str (skk-hurigana-1 start end all 'katakana)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-hurigana-katakana-message (start end &optional all)
  "領域の漢字に全てフリガナを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    \"中島\" -> \"中島[{ナカシマ|ナカジマ}]\""
  (interactive "r\nP")
  (message "%s" (skk-hurigana-1 start end all 'katakana)))

(defun skk-hurigana-1 (start end all &optional katakana)
  ;; skk-hurigana-* のサブルーチン。
  ;; オプショナル引数の KATAKANA が non-nil であれば、カタカナへ変換する。
  (let ((arg (if katakana
                 '("-JK" "-f")
               '("-JH" "-f"))))
    (when skk-allow-spaces-newlines-and-tabs
      (setq arg (cons "-c" arg)))
    (when all
      (setq arg (cons "-p" arg)))
    (skk-kakasi-region start end arg)))

;;;###autoload
(defun skk-romaji-region (start end)
  "領域の漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換する。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

`skk-romaji-*-by-hepburn' が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。"
  (interactive "*r")
  (let ((arg '("-Ha" "-Ka" "-Ja" "-Ea" "-ka" "-s"))
        str)
    (when skk-allow-spaces-newlines-and-tabs
      (setq arg (cons "-c" arg)))
    (unless skk-romaji-*-by-hepburn
      (setq arg (cons "-rk" arg)))
    (setq str (skk-kakasi-region start end arg))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-romaji-message (start end)
  "領域の漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換し、エコーする。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

`skk-romaji-*-by-hepburn' が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。"
  (interactive "r")
  (let ((arg '("-Ha" "-Ka" "-Ja" "-Ea" "-ka" "-s")))
    (when skk-allow-spaces-newlines-and-tabs
      (setq arg (cons "-c" arg)))
    (unless skk-romaji-*-by-hepburn
      (setq arg (cons "-rk" arg)))
    (message "%s" (skk-kakasi-region start end arg))))

(defun skk-kakasi-region (start end arglist)
  ;; START と END 間の領域に対し kakasi コマンドを適用する。ARGLIST を
  ;; kakasi の引数として渡す。kakasi の出力を返す。
  (unless (or skk-use-kakasi
              skk-kakasi-command)
    (skk-error
     "KAKASI がインストールされていないか、使用しない設定(%s)になっています"
     "KAKASI was not installed, or %s is nil"
     "skk-use-kakasi"))
  ;; 複数辞書対応
  (dolist (jisyo skk-gyakubiki-jisyo-list)
    (setq arglist (append arglist (list (expand-file-name jisyo)))))

  (let ((str (buffer-substring-no-properties start end)))
    ;; 頻度情報を使って何かおもしろい使い方ができるかな？  現状では使って
    ;; いない。
    ;;(hindo-file (skk-make-temp-file "skkKKS"))
    (with-temp-buffer
      ;; current buffer が read-only のときに current buffer で call-process
      ;; を呼ぶと destination buffer を別に指定していてもエラーになるので、リー
      ;; ジョンの文字列をワークバッファに退避する。
      (insert str)
      (unless (and (eq (apply 'call-process-region
                              (point-min) (point)
                              skk-kakasi-command
                              ;; kakasi-2.2.5.hindo.diff が当っていると
                              ;; 標準エラー出力に頻度情報が出力される。
                              'delete-original-text
                              ;;(list t hindo-file)
                              '(t nil)
                              nil (cons "-ieuc" (cons "-oeuc" arglist)))
                       0)
                   (> (buffer-size) 0))
        (skk-error "変換できません"
                   "Cannot convert!"))
      (buffer-string))))

(run-hooks 'skk-kakasi-load-hook)

(provide 'skk-kakasi)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-kakasi.el ends here
