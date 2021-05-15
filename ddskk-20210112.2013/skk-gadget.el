;;; skk-gadget.el --- $B<B9TJQ49$N$?$a$N%W%m%0%i%`(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

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

;; 2014.12.30 SKK-JISYO.lisp
;;   SKK-JISYO.L $B$+$i%W%m%0%i%`<B9TJQ49$r;HMQ$7$F$$$k%(%s%H%j$rH4$-=P$7$F(B
;;   SKK-JISYO.lisp $B$H$7$?!#$?$@$7!"(Bconcat $B4X?t$K8B$C$F$O(B SKK-JISYO.L $B$K(B
;;   $B;D$7$F$$$k!#(B http://mail.ring.gr.jp/skk/201412/msg00095.html

;; $B%W%m%0%i%`<B9TJQ49$H$O(B
;; ======================
;; $BAw$j2>L>$N$J$$<-=q$NJQ49$N8uJd$K(B Emacs Lisp $B$N%3!<%I$,=q$$$F$"$l$P!"(BSKK
;; $B$O$=$N%3!<%I$r(B Lisp $B$N%W%m%0%i%`$H$7$F<B9T$7!"$=$N7k2L$NJ8;zNs$r2hLL$KA^(B
;; $BF~$9$k!#Nc$($P!"<-=q$K(B

;;         now /(current-time-string)/

;; $B$H$$$&9T$,$"$k$H$-!"(B`/now ' $B$H%?%$%W$9$l$P2hLL$K$O8=:_$N;~9o$,I=<($5$l!"(B
;; `$B"'(BFri Apr 10 11:41:43 1992' $B$N$h$&$K$J$k!#(B

;; $B$3$3$G;H$($k(B Lisp $B$N%3!<%I$O(B
;;   o $BJ8;zNs$rJV$9$3$H(B
;;   o $B2~9T$r4^$^$J$$$3$H(B
;; $B$K8B$i$l$k!#(B

;; $B$3$N%U%!%$%k$O<B9TJQ49%W%m%0%i%`$r=8$a$?$b$N$G$"$k!#(B

;; skk-gadget.el $B$N(B `gadget' $B$O!V>e<j$/9)IW$7$?F;6q!W$N0UL#!#!V?'!9Ht$S=P$9(B
;; $B5$$N$-$$$?$*$b$A$cH"!W$H$$$&$h$&$J0UL#$GL>IU$1$i$l$?!#(B
;; $BM>CL$@$,!"(BX Window $B$G;HMQ$5$l$k(B `Widget' $B$H$$$&8@MU$O!"(B`window'+`gadget'
;; $B$+$i:n$i$l$?B$8l$i$7$$!#(B


;; $B<-=q%(%s%H%j(B today $B$N2r@b(B
;; ======================
;; today /(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) )/

;; 1. lambda() $BA4BN$r0z?t$H$7$F(B skk-current-date() $B$r<B9T$9$k(B

;; 2. skk-current-date() $B$O!"(B
;;   a. skk-current-date-1() => ("2013" "Jan" "29" "Tue" "22" "59" "50")
;;   b. format               => nil
;;   c. (not skk-date-ad)    => t=$B@>Nq(B, nil=$B859f(B
;;   d. and-time             => nil
;;  $B$r0z?t$H$7$F(B lambda() $B$r(B funcall $B$9$k(B

;; 3. $B$9$J$o$A!"(Blambda() $B<+?H$N0z?t(B `date-information' `format' `gengo' `and-time' $B$O!"(B
;;  $B>e5-$N(B a $B!A(B d $B$G$"$k(B

;; 4. $B:G=*E*$K(B skk-default-current-date() $B$,<B9T$5$l$k(B
;;   e. date-information  [date-information] ... ("2013" "Jan" "29" "Tue" "22" "59" "50")
;;   f. nil               [format]           ... %s$BG/(B%s$B7n(B%s$BF|(B(%s)%s$B;~(B%s$BJ,(B%s$BIC(B
;;   g. 3                 [num-type]         ... $BJQ49%?%$%W(B => $B4A?t;z(B
;;   h. 'gengo            [gengo]            ... non-nil => $BJ?@.Fs==8^G/(B
;;                                                   nil => $BFs@i==;0G/(B
;;   i. 0                 [gengo-index]      ...
;;   j. 0                 [month-alist-index] ...
;;   k. 0                 [dayofweek-alist-index] ...
;;   opt. and-time

;; 5. *scratch* $B$GI>2A$7$F$_$k(B
;;   (skk-default-current-date
;;     '("2013" "Jan" "29" "Tue" "22" "59" "50") nil 3 t 0 0 0)  [C-j]
;;   => "$BJ?@.Fs==8^G/0l7nFs==6eF|(B($B2P(B)"
;;      $B$3$l",$,!"H/C<$G$"$k(B skk-current-date() $B$NLa$jCM$G$"$k(B

;;; Code:

(require 'skk)

(eval-when-compile
  (require 'cl-lib))

;; -- programs
;;;###autoload
(defun skk-current-date (&optional pp-function format and-time)
  "`current-time-string' $B$N=PNO$r2C9)$7!"8=:_$NF|;~(B \(string\) $B$rJV$9!#(B
$B%*%W%7%g%J%k0z?t$N(B PP-FUNCTION $B$r;XDj$9$k$H!"(B
  `skk-current-date-1' $B$NJV$jCM!"(B FORMAT $B!"(B AND-TIME
$B$r0z?t$K$7$F(B PP-FUNCTION $B$r(B `funcall' $B$9$k!#(B
PP-FUNCTION $B$,(B nil $B$N>l9g$O(B `skk-default-current-date-function' $B$r(B
`funcall' $B$9$k!#(B
FORMAT $B$O(B `format' $B$NBh#10z?t$NMM<0(B \(string\) $B$K$h$k=PNO;XDj%F%s%W%l!<%H!#(B
AND-TIME \(boolean\) $B$r;XDj$9$k$H;~9o$bJV$9!#(B
`skk-today' $B$H(B `skk-clock' $B$N%5%V%k!<%A%s$G$"$k!#(B"
  (funcall (if pp-function
               pp-function
             skk-default-current-date-function)
           (skk-current-date-1) format (not skk-date-ad) and-time))

(defun skk-current-date-1 (&optional specified-time)
  "`current-time-string' $B$N=PNO$r2C9)$7!"F|IU!&;~9o>pJs$r%j%9%H$K$7$FJV$9!#(B
\(year month day day-of-week hour minute second\)
\(\"2013\" \"Jan\" \"29\" \"Tue\" \"22\" \"41\" \"11\"\)

$B%*%W%7%g%J%k0z?t$N(B SPECIFIED-TIME $B$O(B `current-time-string' $B$N(B docstring
$B$r;2>H$N$3$H!#(B"
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
$BF|IU>pJs$NI8=`E*$J=PNO$r$9$kB>!"%f!<%6$K$"$kDxEY$N%+%9%?%^%$%:5!G=$rDs6!$9$k!#(B
$B$3$N4X?t$N0z?t$G%+%9%?%^%$%:$G$-$J$$=PNO$r4uK>$9$k>l9g$O!"(B
`skk-default-current-date-function' $B$K<+A0$N4X?t$r;XDj$9$k!#(B

DATE-INFORMATION $B$O(B

  \(year month day day-of-week hour minute second\)

$B$N7A<0$N%j%9%H!#3FMWAG$OJ8;zNs!#(B`skk-current-date-1' $B$N=PNO$r;HMQ!#(B

FORMAT $B$O(B `format' $B$NBh#10z?t$NMM<0$K$h$k=PNO7ABV$r;XDj$9$kJ8;zNs!#(B
  nil $B$G$"$l$P(B \"%s$BG/(B%s$B7n(B%s$BF|(B\(%s\)%s$B;~(B%s$BJ,(B%s$BIC(B\" \($B$b$7$/$O(B
  \"%s$BG/(B%s$B7n(B%s$BF|(B\(%s\)\" $B$,;H$o$l$k!#(B

NUM-TYPE \(number\) $B$O(B
  0 -> $BL5JQ49(B
  1 -> $BA43Q?t;z$XJQ49(B
  2 -> $B4A?t;z$XJQ49(B \($B0L<h$j$J$7(B\)
  3 -> $B4A?t;z$XJQ49(B \($B0L<h$j$r$9$k(B\)
  4 -> $B$=$N?t;z$=$N$b$N$r%-!<$K$7$F<-=q$r:F8!:w(B
  5 -> $B4A?t;z(B \($B<j7A$J$I$G;HMQ$9$kJ8;z$r;HMQ(B\)$B$XJQ49(B \($B0L<h$j$r$9$k(B\)
  9 -> $B>-4}$G;HMQ$9$k?t;z(B \(\"$B#3;M(B\" $B$J$I(B\) $B$KJQ49(B

GENGO $B$O859fI=<($9$k$+$I$&$+(B \(boolean\)$B!#(B

GENGO-INDEX $B$O(B `skk-gengo-alist' $B$N3FMWAG$N(B cadr $B$r(B 0 $B$H$9$k(B index
 \(number\)$B!#(Bnil $B$G$"$l$P(B `current-time-string' $B$N=PNO$N$^$^L5JQ49!#(B

MONTH-ALIST-INDEX $B$O(B `skk-month-alist' $B$N3FMWAG$N(B cadr $B$r(B 0 $B$H$9$k(B
 index \(number\)$B!#(Bnil $B$G$"$l$P(B `current-time-string' $B$N=PNO$N$^$^L5JQ49!#(B

DAYOFWEEK-ALIST-INDEX $B$O(B `skk-day-of-week-alist' $B$N3FMWAG$N(B cadr $B$r(B
 0 $B$H$9$k(B index \(number\)$B!#(Bnil $B$G$"$l$P(B `current-time-string' $B$N=PNO$N$^(B
$B$^L5JQ49!#(B

AND-TIME $B$O;~9o$bI=<($9$k$+$I$&$+(B \(boolean\)$B!#(B"
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
        (format (or format "%s$BG/(B%s$B7n(B%s$BF|(B(%s)%s$B;~(B%s$BJ,(B%s$BIC(B")
                year month day day-of-week hour minute second)
      (format (or format "%s$BG/(B%s$B7n(B%s$BF|(B(%s)") year month day day-of-week))))

;;;###autoload
(cl-defun skk-relative-date (pp-function format and-time &key (yy 0) (mm 0) (dd 0))
  "`skk-current-date' $B$N3HD%HG!#(BPP-FUNCTION, FORMAT, AND-TIME $B$O(B `skk-current-date' $B$r;2>H$N$3$H!#(B
$B<B9TNc(B
 (skk-relative-date) => \"$BJ?@.(B25$BG/(B2$B7n(B03$BF|(B($BF|(B)\"
 (skk-relative-date (lambda (arg) body) nil nil :dd -1) => \"$BJ?@.(B25$BG/(B2$B7n(B02$BF|(B($BEZ(B)\"
 (skk-relative-date (lambda (arg) body) nil nil :mm -1) => \"$BJ?@.(B25$BG/(B1$B7n(B03$BF|(B($BLZ(B)\"
 (skk-relative-date (lambda (arg) body) nil nil :yy  2) => \"$BJ?@.(B27$BG/(B2$B7n(B03$BF|(B($B2P(B)\"
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
`current-time-string' $B$N=PNO$r2C9)$7!"8=:_$NF|;~$rI=$9J8;zNs$r:n$j!"A^F~(B
$B$9$k!#<B<AE*$K!V(Btoday $B%(%s%H%j$N8F$S=P$7!W$@$1$J$N$G!"%+%9%?%^%$%:$O8D?M(B
$B<-=q$N(B today $B%(%s%H%j$K$h$k!#(B"
  (interactive "p")
  (if (and (eq skk-henkan-mode 'on) ;$B"&%b!<%I(B
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
  "$B%_%K%P%C%U%!$K%G%8%?%k;~7W$rI=<($9$k!#(B
quit $B$9$k$H!"$=$N;~E@$NF|;~$r8uJd$H$7$FA^F~$9$k!#(B
quit $B$7$?$H$-$K5/F0$7$F$+$i$N7P2a;~4V$r%_%K%P%C%U%!$KI=<($9$k!#(B
interactive $B$K5/F0$9$kB>!"(B\"clock /(skk-clock)/\" $B$J$I$N%(%s%H%j$r(B SKK $B$N<-=q(B
$B$K2C$(!"(B\"/clock\"+ SPC $B$GJQ49$9$k$3$H$K$h$C$F$b5/F02D!#(B\\[keyboard-quit] $B$G;_$^$k!#(B
$B<B9TJQ49$G5/F0$7$?>l9g$O!"(B\\[keyboard-quit] $B$7$?;~E@$N;~E@$NF|;~$rA^F~$9$k!#(B

$B%*%W%7%g%J%k0z?t$N(B KAKUTEI-WHEN-QUIT $B$,(B non-nil $B$G$"$l$P(B \\[keyboard-quit] $B$7$?$H$-$K3NDj$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B TIME-SIGNAL $B$,(B non-nil $B$G$"$l$P!"(BNTT $B$N;~JsIw$K(B ding $B$9$k!#(B
$B$=$l$>$l(B \"clock /(skk-clock nil t)/\" $B$N$h$&$J%(%s%H%j$r<-=q$KA^F~$9$l$PNI$$!#(B
`skk-date-ad' $B$H(B `skk-number-style' $B$K$h$C$FI=<(J}K!$N%+%9%?%^%$%:$,2DG=!#(B"
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
      (setq expr1 "[789]$BIC(B"
            expr2 "0$BIC(B"))
     ((or (eq skk-number-style t)
          ;; skk-number-style $B$K(B $B?t;z$H(B t $B0J30$N(B non-nil $BCM$rF~$l$F$$$k>l(B
          ;; $B9g!"(B= $B$r;H$&$H(B Wrong type argument: number-or-marker-p, xxxx
          ;; $B$K$J$C$F$7$^$&!#(B
          (eq skk-number-style 1))
      (setq expr1 "[$B#7#8#9(B]$BIC(B"
            expr2 "$B#0IC(B"))
     (t
      (setq expr1 "[$B<7H,6e(B]$BIC(B"
            expr2 "$B!;IC(B")))

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
                  ;; [7890] $B$N$h$&$K@55,I=8=$r;H$o$:!"(B7 $B$@$1$GA4$F$N%^%7%s$,(B
                  ;; $BCe$$$F$f$1$PNI$$$N$@$,(B...$B!#CzEY$3$N4X?t<B9T;~$K(B Garbage
                  ;; collection $B$,8F$P$l$F$bI=<($5$l$k?t;z$,Ht$V>l9g$,$"$k!#(B
                  (ding))
                 ((string-match expr2 mes)
                  ;; 0 $B$@$1!V%]!A%s!W$H$$$-$?$$$H$3$m$G$9$,!"%^%7%s$K$h$C(B
                  ;; $B$F:9$,$"$k!#(B
                  ;; 386SX 25Mhz + Mule-2.x $B$@$H!V%T%C!"%T%C!W$H$$$&46$8!#(B
                  ;; $BIU$$$F$f$/$N$,Hs>o$K?I$$!#(B68LC040 33Mhz + NEmacs $B$@$H(B
                  ;; $B!V%T%T%C!W$H$J$j!"2;$N%?%$%_%s%0$ONI$$$N$@$,!"$H$-(B
                  ;; $B$I$-(B 1 $BICJ,$D$$$F$$$1$J$/$J$k!#(BPentium 90Mhz +
                  ;; Mule-2.x$B$@$H!V%T%C!W$H$$$&C12;$K$J$C$F$7$^$&(B... (;_;)$B!#(B
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
           (message "$B7P2a;~4V(B: %s $BIC(B"
                    (skk-time-difference start end))))))))

;;;###autoload
(defun skk-ad-to-gengo (gengo-index &optional divider tail not-gannen)
  ;; $B@>Nq$r859f$KJQ49$9$k!#%*%W%7%g%J%k0z?t$N(B DIVIDER $B$,;XDj$5$l$F$$$l$P!"G/9f(B
  ;; $B$H?t;z$N4V$K!"(BTAIL $B$,;XDj$5$l$F$$$l$P!"?t;z$NKvHx$K!"$=$l$>$l$NJ8;zNs$rO"(B
  ;; $B7k$9$k!#(B
  ;; $B<-=q8+=P$7Nc(B;
  ;; $B$;$$$l$-(B#$B$M$s(B /(skk-ad-to-gengo 0 nil "$BG/(B")/(skk-ad-to-gengo 0 " " " $BG/(B")/
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
  ;; return a value of which cdr is "$B85(B" (string).
  (when (>= 1866 ad)
    (skk-error "$BJ,$j$^$;$s(B" "Unknown year"))
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
              ((= ad 1) "$B85(B")
              (t ad))))

;;;###autoload
(defun skk-gengo-to-ad (&optional head tail)
  ;; $B859f$r@>Nq$KJQ49$9$k!#%*%W%7%g%J%k0z?t$N(B HEAD, TAIL $B$,;XDj$5$l$F$$(B
  ;; $B$l$P!"$=$NJ8;zNs$r@hF,!"KvHx$KO"7k$9$k!#(B
  ;; $B<-=q8+=P$7Nc(B;
  ;;   $B$7$g$&$o(B#$B$M$s(B /(skk-gengo-to-ad "" "$BG/(B")/(skk-gengo-to-ad "" " $BG/(B")/\
  ;;   (skk-gengo-to-ad "$B@>Nq(B" "$BG/(B")/(skk-gengo-to-ad "$B@>Nq(B" " $BG/(B")/
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
       (skk-error "0 $BG/$O$"$jF@$J$$(B"
                  "Cannot convert 0 year"))
      ((member gengo '("$B$l$$$o(B" "$BNaOB(B"))
       2018)
      ((member gengo '("$B$X$$$;$$(B" "$BJ?@.(B"))
       1988)
      ((member gengo '("$B$7$g$&$o(B" "$B><OB(B"))
       1925)
      ((member gengo '("$B$?$$$7$g$&(B" "$BBg@5(B"))
       1911)
      ((member gengo '("$B$a$$$8(B" "$BL@<#(B"))
       1867)
      (t
       (skk-error "$BH=JLITG=$J859f$G$9!*(B"
                  "Unknown Gengo!")))))

;;;###autoload
(defun skk-calc (operator)
  ;; 2 $B$D$N0z?t$r<h$C$F(B operator $B$N7W;;$r$9$k!#(B
  ;; $BCm0U(B: '/ $B$O0z?t$H$7$FEO$;$J$$$N$G(B (defalias 'div '/) $B$J$I$H$7!"JL$N7A$G(B
  ;; skk-calc $B$KEO$9!#(B
  ;; $B<-=q8+=P$7Nc(B; #*# /(skk-calc '*)/
  (number-to-string (apply operator
                           (mapcar 'string-to-number
                                   skk-num-list))))

;;;###autoload
(defun skk-plus ()
  ;; $B<-=q8+=P$7Nc(B; #+#+# /(skk-plus)/
  (skk-calc '+))

;;;###autoload
(defun skk-minus ()
  (skk-calc '-))

;;;###autoload
(defun skk-times ()
  (skk-calc '*))

;;;###autoload
(defun skk-ignore-dic-word (&rest no-show-list)
  ;; $B6&MQ<-=q$KEPO?$5$l$F$$$k!"0c$C$F$$$k(B/$B5$$KF~$i$J$$JQ49$r=P$5$J$$$h$&$K$9(B
  ;; $B$k!#(B
  ;; $B<-=q8+=P$7Nc(B;
  ;;   $B$k$9$P$s(B /$BN1<iHV(B/(skk-ignore-dic-word "$BN1<iEE(B")/
  ;;   $B$+$/$F$$(B /(skk-ignore-dic-word "$B3NDj(B")/
  (let (new-word)
    ;; skk-ignore-dic-word $B<+?H$N%(%s%H%j$r>C$9!#>C$9$Y$-8uJd$O(B
    ;; skk-henkan-list $B$+$iD>@\Cj=P$7$F$$$k$N$G(B delete $B$G$O$J$/(B delq $B$G==J,!#(B
    (setq skk-henkan-list (delq (nth skk-henkan-count skk-henkan-list)
                                skk-henkan-list))
    ;; $BA48uJd$r(B skk-henkan-list $B$KF~$l$k!#(B
    (while skk-current-search-prog-list
      (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))))
    ;; $BITMW$J8uJd$r<N$F$k!#(B
    (while no-show-list
      (setq skk-henkan-list (delete (car no-show-list) skk-henkan-list)
            no-show-list (cdr no-show-list)))
    ;; $B%+%l%s%H$N8uJd(B (skk-ignore-dic-word $B<+?H$N%(%s%H%j(B) $B$r>C$7$?$N$G!"(B
    ;; skk-henkan-count $B$O<!$N8uJd$r;X$7$F$$$k!#(B
    (setq new-word (or (nth skk-henkan-count skk-henkan-list)
                       (skk-henkan-in-minibuff)))
    ;; $B8uJd$,$J$$$H$-!#(B
    (unless new-word
      ;; $B6uJ8;zNs$,EPO?$5$l$?$i<-=qEPO?$NA0$N>uBV$KLa$9!#(B
      ;; (nth -1 '(A B C)) $B$O!"(BA $B$rJV$9$N$G!"(Bn $B$,Ii$N?t$G$J$$$3$H$r%A%'%C%/(B
      ;; $B$7$F$*$/I,MW$,$"$k!#(B
      (if (> skk-henkan-count 0)
          (setq skk-henkan-count (- skk-henkan-count 1)
                new-word (nth skk-henkan-count skk-henkan-list))
        ;; (1- skk-henkan-count) == -1 $B$K$J$k!#"&%b!<%I$KLa$9!#(B
        (throw 'next-word 'none)))
    ;;
    (throw 'next-word new-word)))

;;;###autoload
(defun skk-henkan-face-off-and-remove-itself ()
  ;; skk-insert-new-word-function $B$K%;%C%H$9$k$?$a$N4X?t!#%+%l%s%H%P%C%U%!$N(B
  ;; $BJQ49ItJ,$,(B Overlay $B$N(B face $BB0@-$K$h$C$FI=<($,JQ99$5$l$F$$$k$N$rLa$7!"$=$N(B
  ;; $B8e<+J,<+?H$r(B skk-insert-new-word-function $B$+$i<h$j=|$/<+Gz4X?t!#(B
  (skk-henkan-face-off)
  (setq skk-insert-new-word-function nil))

;;;###autoload
(defun skk-gadget-units-conversion (unit-from number unit-to)
  "`skk-units-alist'$B$r;2>H$7!"49;;$r9T$&!#(B
NUMBER $B$K$D$$$F(B UNIT-FROM $B$+$i(B UNIT-TO $B$X$N49;;$r9T$&!#(B"
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
