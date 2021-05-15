;;; skk-search-web.el --- Google $B%5%8%'%9%H$J$I$rMxMQ$7$?$+$J4A;zJQ49(B -*- coding: iso-2022-jp -*-

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

;; $B;HMQJ}K!$r#2$D@bL@$7$^$9!#9%$_$NJ}$r;HMQ$7$F$/$@$5$$!#(B

;; (1) $B$+$J4A;zJQ49$N8uJd$K(B Google $B%5%8%'%9%H$rMxMQ$9$k(B
;;     skk-search-prog-list $B$N:G8eJ}$K(B skk-search-web() $B$rCV$/$3$H$K$h$j!"(B
;;     $B8D?M<-=q$d6&M-<-=q$KEPO?$5$l$F$$$J$$8+=P$78l$r(B Google $B%5%8%'%9%H(B
;;     $B$7$^$9!#(B

;;     (add-to-list 'skk-search-prog-list
;;                  '(skk-search-web 'skk-google-suggest)
;;                  t)

;; (2) $B<-=qEPO?%b!<%I$X$NFMF~;~$N=i4|CM$K(B Google $B%5%8%'%9%H$rMxMQ$9$k(B
;;     (setq skk-read-from-minibuffer-function
;;           (lambda ()
;;             (car (skk-google-suggest skk-henkan-key))))

;; $B>e5-Nc$G<($7$?4X?t(B skk-google-suggest $B$O(B skk-google-cgi-api-for-japanese-input
;; $B$KCV$-49$(2DG=$G$9!#(B
;;     (add-to-list 'skk-search-prog-list
;;              '(skk-search-web 'skk-google-cgi-api-for-japanese-input)
;;                  t)

;;     (setq skk-read-from-minibuffer-function
;;           (lambda ()
;;             (car (skk-google-cgi-api-for-japanese-input skk-henkan-key))))

;; SKK Dynamic Completion $B$HJ;MQ$9$k$3$H$b2DG=$G$9!#(B
;; (add-to-list 'skk-completion-prog-list '(skk-comp-google) t)

;;; Test:

;; (let ((skk-henkan-key "emacs"))
;;   (skk-search-web 'skk-google-suggest))
;; => ("emacs" "emacs $B%3%^%s%I(B" "emacs windows" "emacs $B;H$$J}(B" "emacs $BJ8;z%3!<%I(B" ...)

;;; $B<U<-(B
;;    $B$b$H$b$H$N%*%j%8%J%k$O(B HAMANO Kiyoto <khiker.mail@gmail.com> $B$5$s$,(B
;;    $B=q$$$?5-;v(B http://d.hatena.ne.jp/khiker/20100128/google_suggest $B$G$9!#(B
;;    $B2~JQ!"8x3+$r2w$/>5Bz$7$F$$$?$@$$$?(B HAMANO Kiyoto $B$5$s$K46<U$7$^$9!#(B

;;; Code:

(require 'url-http)
(require 'json)

(eval-when-compile
  (defvar skk-henkan-key)
  (defvar skk-comp-key)
  (defvar skk-comp-first))

(defun skk-url-retrieve (url coding-system)
  "URL $B$r<hF@$9$k!#La$jCM$O(B decode-coding-string $B$G$"$k(B."
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
  "Google CGI API for Japanese Input $B$rMxMQ$7$?$+$J4A;zJQ49(B.
http://www.google.co.jp/ime/cgiapi.html
$BLa$jCM$O!"8uJd72$N%j%9%H(B."
  (let* ((jsonp (skk-url-retrieve
                 (concat "http://www.google.com/transliterate"
                         "?langpair=ja-Hira|ja"
                         "&text="
                         (url-hexify-string (encode-coding-string (concat word ",")
                                                                  'utf-8)))
                 'utf-8))
         (json (json-read-from-string jsonp)) ; [["$B$_$@$7$4(B" ["$B8uJd(Ba" "$B8uJd(Bb" "$B8uJd(Bc"]]]
         (ary (aref (aref json 0) 1))         ; ["$B8uJd(Ba" "$B8uJd(Bb" "$B8uJd(Bc"]
         list)
    (dotimes (i (length ary))
      (setq list (cons (aref ary i)
                       list)))
    (nreverse list)))


(defun skk-google-suggest (word)
  "Google $B%5%8%'%9%H$rMxMQ$7$?$+$J4A;zJQ49(B."
  ;; http://labs.google.com/intl/ja/suggestfaq.html (404 not found)

  (with-temp-buffer
    (insert (skk-url-retrieve
             (concat "http://clients1.google.co.jp/complete/search"
                     "?hl=ja"
                     "&cp=2"
                     "&output=toolbar" ; xml $B%l%9%]%s%9(B
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
         ;; ["$B$_$@$7$4(B" ["$B8uJd(Ba" "$B8uJd(Bb" "$B8uJd(Bc"]]
         ;; $B"(!V$_$@$7$4!W$,4A;z$G$"$l$PMM!9$J8uJd$,F@$i$l$k$,!"(B
         ;;   $BJ?2>L>$@$H$"$^$j%R%C%H$7$J$$!#$=$N$?$a!"$+$J4A;z(B
         ;;   $BJQ49$NMQES$K$OIT8~$-$+$b!#(B
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
