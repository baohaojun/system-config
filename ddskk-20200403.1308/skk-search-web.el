;;; skk-search-web.el --- Google サジェストなどを利用したかな漢字変換 -*- coding: iso-2022-jp -*-

;; Copyright (C) 2010, 2011 HAMANO Kiyoto <khiker.mail@gmail.com>
;; Copyright (C) 2011 Tsuyoshi Kitamoto <tsuyoshi.kitamoto@gmail.com>

;; Author: HAMANO Kiyoto <khiker.mail@gmail.com>
;; Maintainer: Tsuyoshi Kitamoto <tsuyoshi.kitamoto@gmail.com>
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

;; 使用方法を２つ説明します。好みの方を使用してください。

;; (1) かな漢字変換の候補に Google サジェストを利用する
;;     skk-search-prog-list の最後方に skk-search-web() を置くことにより、
;;     個人辞書や共有辞書に登録されていない見出し語を Google サジェスト
;;     します。

;;     (add-to-list 'skk-search-prog-list
;;                  '(skk-search-web 'skk-google-suggest)
;;                  t)

;; (2) 辞書登録モードへの突入時の初期値に Google サジェストを利用する
;;     (setq skk-read-from-minibuffer-function
;;           (lambda ()
;;             (car (skk-google-suggest skk-henkan-key))))

;; 上記例で示した関数 skk-google-suggest は skk-google-cgi-api-for-japanese-input
;; に置き換え可能です。
;;     (add-to-list 'skk-search-prog-list
;;              '(skk-search-web 'skk-google-cgi-api-for-japanese-input)
;;                  t)

;;     (setq skk-read-from-minibuffer-function
;;           (lambda ()
;;             (car (skk-google-cgi-api-for-japanese-input skk-henkan-key))))

;; SKK Dynamic Completion と併用することも可能です。
;; (add-to-list 'skk-completion-prog-list '(skk-comp-google) t)

;;; Test:

;; (let ((skk-henkan-key "emacs"))
;;   (skk-search-web 'skk-google-suggest))
;; => ("emacs" "emacs コマンド" "emacs windows" "emacs 使い方" "emacs 文字コード" ...)

;;; 謝辞
;;    もともとのオリジナルは HAMANO Kiyoto <khiker.mail@gmail.com> さんが
;;    書いた記事 http://d.hatena.ne.jp/khiker/20100128/google_suggest です。
;;    改変、公開を快く承諾していただいた HAMANO Kiyoto さんに感謝します。

;;; Code:

(require 'url-http)
(require 'json)

(eval-when-compile
  (defvar skk-henkan-key)
  (defvar skk-comp-key)
  (defvar skk-comp-first))

(defun skk-url-retrieve (url coding-system)
  "URL を取得する。戻り値は decode-coding-string である."
  (let (buf p)
    (unwind-protect
        (progn
          (setq buf (let (
                          ;; (url-request-extra-headers '(("" . "")
                          ;;                      ("" . "")))
                          (url-request-method "GET")
                          (url-max-redirextions 0))
                      (url-retrieve-synchronously url))) ; return BUFFER contain data
          (when (setq p (url-http-symbol-value-in-buffer
                         'url-http-end-of-headers buf))
            (with-current-buffer buf
              (decode-coding-string (buffer-substring (1+ p)
                                                      (point-max))
                                    coding-system))))
      (when buf
        (kill-buffer buf)))))


(defun skk-google-cgi-api-for-japanese-input (word)
  "Google CGI API for Japanese Input を利用したかな漢字変換.
http://www.google.co.jp/ime/cgiapi.html
戻り値は、候補群のリスト."
  (let* ((jsonp (skk-url-retrieve
                 (concat "http://www.google.com/transliterate"
                         "?langpair=ja-Hira|ja"
                         "&text="
                         (url-hexify-string (encode-coding-string (concat word ",")
                                                                  'utf-8)))
                 'utf-8))
         (json (json-read-from-string jsonp)) ; [["みだしご" ["候補a" "候補b" "候補c"]]]
         (ary (aref (aref json 0) 1))         ; ["候補a" "候補b" "候補c"]
         list)
    (dotimes (i (length ary))
      (setq list (cons (aref ary i)
                       list)))
    (nreverse list)))


(defun skk-google-suggest (word)
  "Google サジェストを利用したかな漢字変換."
  ;; http://labs.google.com/intl/ja/suggestfaq.html (404 not found)

  (with-temp-buffer
    (insert (skk-url-retrieve
             (concat "http://clients1.google.co.jp/complete/search"
                     "?hl=ja"
                     "&cp=2"
                     "&output=toolbar" ; xml レスポンス
                     "&q=" (url-hexify-string (encode-coding-string word 'utf-8)))
             'sjis))
    (goto-char (point-min))
    (let (list)
      (while (re-search-forward "suggestion data=\"\\([^>]*\\)\"" nil t)
        (setq list (cons (buffer-substring (match-beginning 1)
                                           (match-end 1))
                         list)))
      (nreverse list))))

(defun skk-wikipedia-suggest (word)
  (let* ((jsonp (skk-url-retrieve
                 (concat "http://ja.wikipedia.org/w/api.php"
                         "?action=opensearch"
                         "&format=json"
                         "&search=" (url-hexify-string
                                     (encode-coding-string word 'utf-8)))
                 'utf-8))
         (json (json-read-from-string jsonp))
         ;; ["みだしご" ["候補a" "候補b" "候補c"]]
         ;; ※「みだしご」が漢字であれば様々な候補が得られるが、
         ;;   平仮名だとあまりヒットしない。そのため、かな漢字
         ;;   変換の用途には不向きかも。
         (ary (aref json 1))
         list)
    (dotimes (i (length ary))
      (setq list (cons (aref ary i)
                       list)))
    (nreverse list)))


(defun skk-search-web (function)
  (funcall function skk-henkan-key))

;; skk-comp, skk-dcomp,
(defvar skk-comp-google-candidates nil)

(defun skk-comp-google ()
  ;; Howto use
  ;; (add-to-list 'skk-completion-prog-list '(skk-comp-google) t)
  (unless (string= skk-comp-key "")
    (when skk-comp-first
      (setq skk-comp-google-candidates (skk-comp-google-make-candidates)))
    (prog1
        (car skk-comp-google-candidates)
      (setq skk-comp-google-candidates (cdr skk-comp-google-candidates)))))

(defun skk-comp-google-make-candidates ()
  (let ((key (car (split-string skk-comp-key "*" t))))
    (skk-google-suggest key)))

(provide 'skk-search-web)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-search-web.el ends here
