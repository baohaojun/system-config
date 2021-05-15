;;; skk-kakasi.el --- KAKASI $B4XO"%W%m%0%i%`(B -*- coding: iso-2022-jp -*-

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

;; skk-kakasi.el $B$O(B KAKASI $B$r(B SKK $B$NCf$+$i;H$&%$%s%?!<%U%'%$%9$G$9!#(BKAKASI $B$O!"(B
;; $B9b66M5?.$5$s(B <hironobu@trc.rwcp.or.jp> $B$K$h$k!"!V4A;z$+$J$^$8$jJ8$r$R$i$,(B
;; $B$JJ8$d%m!<%^;zJ8$KJQ49$9$k$3$H$rL\E*$H$7$F:n@.$7$?%W%m%0%i%`$H<-=q$NAm>N!W(B
;; $B$G$9!#;d<+?H$,%K%e!<%9$d%a!<%k$rFI$s$G$$$F!"F|>oFI$_$,J,$i$J$/$FCQ$:$+$7$$(B
;; $B;W$$$r$9$k$3$H$,B?$$$N$G!"5U0z$-$r$7$?$/$F:n$j$^$7$?!#(B

;; KAKASI $B$O!"(B

;;   ftp://kakasi.namazu.org/pub/kakasi/

;; $B$K$"$j(B anonymous ftp $B$GF~<j$G$-$^$9!#(B

;; $BAG@2$7$$%W%m%0%i%`(B KAKASI $B$r$*:n$j$K$J$C$?9b66$5$s$K46<U$$$?$7$^$9!#(B

;;; Code:

(require 'skk)

(let ((euc (cdr (assoc "euc" skk-coding-system-alist))))
  (modify-coding-system-alist 'process "kakasi" (cons euc euc)))

;;;; FUNCTIONS
;;;###autoload
(defun skk-gyakubiki-region (start end &optional all)
  "$BNN0h$N4A;z!"Aw$j2>L>$rA4$F$R$i$,$J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    $BCfEg(B -> {$B$J$+$7$^(B|$B$J$+$8$^(B}"
  (interactive "*r\nP")
  (let ((str (skk-gyakubiki-1 start end all)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-gyakubiki-and-henkan (start end)
  "$BNN0h$N4A;z$r$R$i$,$J$KJQ49$7!"$=$N$R$i$,$J$r8+=P$78l$H$7$F$+$J4A;zJQ49$r<B9T$9$k!#(B"
  (interactive "*r")
  (let ((str (skk-gyakubiki-1 start end nil)))
    (delete-region start end)
    (skk-set-henkan-point-subr)
    (insert-and-inherit str)
    (skk-start-henkan nil)))

;;;###autoload
(defun skk-gyakubiki-message (start end &optional all)
  "$BNN0h$N4A;z!"Aw$j2>L>$rA4$F$R$i$,$J$KJQ498e!"%(%3!<$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    $BCfEg(B -> {$B$J$+$7$^(B|$B$J$+$8$^(B}"
  (interactive "r\nP")
  (let ((str (skk-gyakubiki-1 start end all)))
    (save-match-data
      (when (string-match "^[ $B!!(B\t]+" str)
        ;; $B@hF,$N6uGr$r<h$j=|$/!#(B
        (setq str (substring str (match-end 0)))))
    (message "%s" str)))

;;;###autoload
(defun skk-gyakubiki-katakana-region (start end &optional all)
  "$BNN0h$N4A;z!"Aw$j2>L>$rA4$F%+%?%+%J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    $BCfEg(B -> {$B%J%+%7%^(B|$B%J%+%8%^(B}"
  (interactive "*r\nP")
  (let ((str (skk-gyakubiki-1 start end all 'katakana)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-gyakubiki-katakana-message (start end &optional all)
  "$BNN0h$N4A;z!"Aw$j2>L>$rA4$F%+%?%+%J$KJQ498e!"%(%3!<$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    $BCfEg(B -> {$B%J%+%7%^(B|$B%J%+%8%^(B}"
  (interactive "r\nP")
  (let ((str (skk-gyakubiki-1 start end all 'katakana)))
    (save-match-data
      (when (string-match "^[ $B!!(B\t]+" str)
        ;; $B@hF,$N6uGr$r<h$j=|$/!#(B
        (setq str (substring str (match-end 0)))))
    (message "%s" str)))

(defun skk-gyakubiki-1 (start end all &optional katakana)
  ;; skk-gyakubiki-* $B$N%5%V%k!<%A%s!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B KATAKANA $B$,(B non-nil $B$G$"$l$P!"%+%?%+%J$XJQ49$9$k!#(B
  (let ((arg (if katakana '("-JK") '("-JH"))))
    (when skk-allow-spaces-newlines-and-tabs
      (setq arg (cons "-c" arg)))
    (when all
      (setq arg (cons "-p" arg)))
    (skk-kakasi-region start end arg)))

;;;###autoload
(defun skk-hurigana-region (start end &optional all)
  "$BNN0h$N4A;z$KA4$F$U$j$,$J$rIU$1$k!#(B
$BNc$($P!"(B
   \"$BJQ49A0$N4A;z$NOF$K(B\" -> \"$BJQ49A0(B[$B$X$s$+$s$^$((B]$B$N4A;z(B[$B$+$s$8(B]$B$NOF(B[$B$o$-(B]$B$K(B\"

$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    \"$BCfEg(B\" -> \"$BCfEg(B[{$B$J$+$7$^(B|$B$J$+$8$^(B}]\""
  (interactive "*r\nP")
  (let ((str (skk-hurigana-1 start end all)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-hurigana-message (start end &optional all)
  "$BNN0h$N4A;z$KA4$F$U$j$,$J$rIU$1!"%(%3!<$9$k!#(B
$BNc$($P!"(B
   \"$BJQ49A0$N4A;z$NOF$K(B\" -> \"$BJQ49A0(B[$B$X$s$+$s$^$((B]$B$N4A;z(B[$B$+$s$8(B]$B$NOF(B[$B$o$-(B]$B$K(B\"

$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    \"$BCfEg(B\" -> \"$BCfEg(B[{$B$J$+$7$^(B|$B$J$+$8$^(B}]\""
  (interactive "r\nP")
  (message "%s" (skk-hurigana-1 start end all)))

;;;###autoload
(defun skk-hurigana-katakana-region (start end &optional all)
  "$BNN0h$N4A;z$KA4$F%U%j%,%J$rIU$1$k!#(B
$BNc$($P!"(B
   \"$BJQ49A0$N4A;z$NOF$K(B\" -> \"$BJQ49A0(B[$B%X%s%+%s%^%((B]$B$N4A;z(B[$B%+%s%8(B]$B$NOF(B[$B%o%-(B]$B$K(B\"

$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    \"$BCfEg(B\" -> \"$BCfEg(B[{$B%J%+%7%^(B|$B%J%+%8%^(B}]\""
  (interactive "*r\nP")
  (let ((str (skk-hurigana-1 start end all 'katakana)))
    (delete-region start end)
    (goto-char start)
    (insert-and-inherit str)))

;;;###autoload
(defun skk-hurigana-katakana-message (start end &optional all)
  "$BNN0h$N4A;z$KA4$F%U%j%,%J$rIU$1!"%(%3!<$9$k!#(B
$BNc$($P!"(B
   \"$BJQ49A0$N4A;z$NOF$K(B\" -> \"$BJQ49A0(B[$B%X%s%+%s%^%((B]$B$N4A;z(B[$B%+%s%8(B]$B$NOF(B[$B%o%-(B]$B$K(B\"

$B%*%W%7%g%J%k0z?t$N(B ALL $B$,(B non-nil $B$J$i$P!"J#?t$N8uJd$,$"$k>l9g$O!"(B\"{}\" $B$G$/(B
$B$/$C$FI=<($9$k!#(B
$BNc$($P!"(B
    \"$BCfEg(B\" -> \"$BCfEg(B[{$B%J%+%7%^(B|$B%J%+%8%^(B}]\""
  (interactive "r\nP")
  (message "%s" (skk-hurigana-1 start end all 'katakana)))

(defun skk-hurigana-1 (start end all &optional katakana)
  ;; skk-hurigana-* $B$N%5%V%k!<%A%s!#(B
  ;; $B%*%W%7%g%J%k0z?t$N(B KATAKANA $B$,(B non-nil $B$G$"$l$P!"%+%?%+%J$XJQ49$9$k!#(B
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
  "$BNN0h$N4A;z!"$R$i$,$J!"%+%?%+%J!"A41QJ8;z$rA4$F%m!<%^;z$KJQ49$9$k!#(B
$BJQ49$K$O!"%X%\%s<0$rMQ$$$k!#(B
$BNc$($P!"(B
   \"$B4A;z$+$J:.$8$jJ8$r%m!<%^;z$KJQ49(B\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

`skk-romaji-*-by-hepburn' $B$,(B nil $B$G$"$l$P!"%m!<%^;z$X$NJQ49MM<0$r71Na<0$KJQ99$9(B
$B$k!#Nc$($P!"(B\"$B$7(B\" $B$O%X%\%s<0$G$O(B \"shi\" $B$@$,!"71Na<0$G$O(B \"si\" $B$H$J$k!#(B"
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
  "$BNN0h$N4A;z!"$R$i$,$J!"%+%?%+%J!"A41QJ8;z$rA4$F%m!<%^;z$KJQ49$7!"%(%3!<$9$k!#(B
$BJQ49$K$O!"%X%\%s<0$rMQ$$$k!#(B
$BNc$($P!"(B
   \"$B4A;z$+$J:.$8$jJ8$r%m!<%^;z$KJQ49(B\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

`skk-romaji-*-by-hepburn' $B$,(B nil $B$G$"$l$P!"%m!<%^;z$X$NJQ49MM<0$r71Na<0$KJQ99$9(B
$B$k!#Nc$($P!"(B\"$B$7(B\" $B$O%X%\%s<0$G$O(B \"shi\" $B$@$,!"71Na<0$G$O(B \"si\" $B$H$J$k!#(B"
  (interactive "r")
  (let ((arg '("-Ha" "-Ka" "-Ja" "-Ea" "-ka" "-s")))
    (when skk-allow-spaces-newlines-and-tabs
      (setq arg (cons "-c" arg)))
    (unless skk-romaji-*-by-hepburn
      (setq arg (cons "-rk" arg)))
    (message "%s" (skk-kakasi-region start end arg))))

(defun skk-kakasi-region (start end arglist)
  ;; START $B$H(B END $B4V$NNN0h$KBP$7(B kakasi $B%3%^%s%I$rE,MQ$9$k!#(BARGLIST $B$r(B
  ;; kakasi $B$N0z?t$H$7$FEO$9!#(Bkakasi $B$N=PNO$rJV$9!#(B
  (unless (or skk-use-kakasi
              skk-kakasi-command)
    (skk-error
     "KAKASI $B$,%$%s%9%H!<%k$5$l$F$$$J$$$+!";HMQ$7$J$$@_Dj(B(%s)$B$K$J$C$F$$$^$9(B"
     "KAKASI was not installed, or %s is nil"
     "skk-use-kakasi"))
  ;; $BJ#?t<-=qBP1~(B
  (dolist (jisyo skk-gyakubiki-jisyo-list)
    (setq arglist (append arglist (list (expand-file-name jisyo)))))

  (let ((str (buffer-substring-no-properties start end)))
    ;; $BIQEY>pJs$r;H$C$F2?$+$*$b$7$m$$;H$$J}$,$G$-$k$+$J!)(B  $B8=>u$G$O;H$C$F(B
    ;; $B$$$J$$!#(B
    ;;(hindo-file (skk-make-temp-file "skkKKS"))
    (with-temp-buffer
      ;; current buffer $B$,(B read-only $B$N$H$-$K(B current buffer $B$G(B call-process
      ;; $B$r8F$V$H(B destination buffer $B$rJL$K;XDj$7$F$$$F$b%(%i!<$K$J$k$N$G!"%j!<(B
      ;; $B%8%g%s$NJ8;zNs$r%o!<%/%P%C%U%!$KB`Hr$9$k!#(B
      (insert str)
      (unless (and (eq (apply 'call-process-region
                              (point-min) (point)
                              skk-kakasi-command
                              ;; kakasi-2.2.5.hindo.diff $B$,Ev$C$F$$$k$H(B
                              ;; $BI8=`%(%i!<=PNO$KIQEY>pJs$,=PNO$5$l$k!#(B
                              'delete-original-text
                              ;;(list t hindo-file)
                              '(t nil)
                              nil (cons "-ieuc" (cons "-oeuc" arglist)))
                       0)
                   (> (buffer-size) 0))
        (skk-error "$BJQ49$G$-$^$;$s(B"
                   "Cannot convert!"))
      (buffer-string))))

(run-hooks 'skk-kakasi-load-hook)

(provide 'skk-kakasi)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-kakasi.el ends here
