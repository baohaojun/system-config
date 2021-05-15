;;; skk-sticky.el --- Improved sticky-shift on SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 2008  IRIE Tetsuya

;; Author: IRIE Tetsuya <irie@t.email.ne.jp>
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

;; [$B35MW(B]
;; SKK $B$G$ODL>o!"JQ493+;O$*$h$SAw$j3+;O$N0LCV$rBgJ8;z$G;XDj$7$^$9$,!"$3(B
;; $B$l$rG$0U$N%-!<$G;XDj$G$-$k$h$&$K$7!"(Bsticky-shift $B%i%$%/$JA`:n$r2DG=(B
;; $B$K$7$^$9!#(B

;; ~/.skk.el $B$K(B

;;   (setq skk-sticky-key ";")

;; $B$H=q$$$F$*$/$H(B ";" $B%-!<$GJQ490LCV$,;XDj$G$-$k$h$&$K$J$j$^$9!#Nc$($P(B
;; $B!VM-$k!W$,(B

;;   "; a ; r u"

;; $B$G$bF~NO$G$-!"%7%U%H%-!<$r2!$9I,MW$,$J$/$J$j$^$9!#A`:n>e$O(B

;;   (info "(skk) Q.3-4 $B:8<j$N>.;X$r(B SHIFT $B$G9s;H$7$?$/$"$j$^$;$s!#(B")

;; $B$J$I$K$"$kDL>o$N(B sticky-shift $B$HJQ$o$j$^$;$s$,!"2hLLI=<($O(B

;;   $BBG80(B  $BDL>o$N(Bsticky  skk-sticky
;;    ;      $BJQ2=$J$7(B      $B"&(B
;;    a      $B"&$"(B          $B"&$"(B
;;    ;      $B"&$"(B          $B"&$"(B*
;;    r      $B"&$"(B*r        $B"&$"(B*r

;; $B$HA+0\$7$^$9!#(Bskk-sticky $B$O(B ";" $B$r2!$7$?;~E@$GI=<($,JQ2=$9$k$N$G<c43(B
;; $BJ,$+$j0W$/$J$k$+$H;W$$$^$9!#(B

;; [$B@_DjJ}K!(B]
;; $B%-!<$N@_DjJ}K!$O3dEv$F$k%-!<$N<oN`$K$h$C$F0[$J$j$^$9!#(B

;; 1. $BI=<(2DG=$J%-!<(B

;;   ";" $B$J$I$NI=<($,2DG=$J%-!<$N>l9g$O(B

;;     (setq skk-sticky-key ";")

;;   $B$N$h$&$K(B string $B$r@_Dj$7$F2<$5$$!#(B`skk-sticky-key' $B$K@_Dj$7$?J8;z(B
;;   $B$=$N$b$N$rF~NO$7$?$$>l9g$O(B2$B2sB3$1$FBG$D$HF~NO$G$-$^$9!#(B

;; 2. $BI=<($5$l$J$$%-!<(B

;;   "$BL5JQ49(B" $B$N$h$&$JI=<($rH<$o$J$$%-!<$N>l9g$O(B

;;     (setq skk-sticky-key [muhenkan]) ; Windows $B4D6-$@$H(B [noconvert]

;;   $B$N$h$&$K$=$N%-!<$rI=$o$9(B vector $B$r@_Dj$7$F2<$5$$!#(B

;; 3. $BF1;~BG80(B

;;   2$B$D$N%-!<$rF1;~$KBG80$9$k$3$H$G$bJQ490LCV$r;XDj$G$-$^$9!#Nc$($P(B
;;   "f" $B$H(B "j" $B$NF1;~BG80$G;XDj$9$k>l9g$O(B

;;     (setq skk-sticky-key '(?f ?j))

;;   $B$N$h$&$K(B character $B$N%j%9%H$r@_Dj$7$F2<$5$$!#(B
;;   Dvorak $BG[Ns$N$h$&$J!"2!$7$d$9$$>l=j$KE,Ev$J%-!<$,$J$$4D6-$G$b$3$N(B
;;   $B5!G=$r;H$$$?$$>l9g$KJXMx$+$b$7$l$^$;$s!#(B

;; [$BCm0U;v9`(B]
;; o $B$3$l$O$"$/$^$G$b!VG$0U$N%-!<$GJQ490LCV$r;XDj$9$k!W$b$N$G$"$j!"(B
;;   sticky-shift $B$=$N$b$N$G$O$"$j$^$;$s!#$7$?$,$C$F!"(Blatin $B%b!<%I$d(B
;;   abbrev $B%b!<%I!"$^$?(B SKK $B0J30$G$b(B sticky-shift $B$r;H$$$?$$>l9g$OA0=R(B
;;   $B$N(B info $B$N$h$&$J@_Dj$rJ;MQ$9$kI,MW$,$"$j$^$9!#(B

;; o skk-hint.el $B$rJ;MQ$9$k>l9g!"(B`skk-hint-start-char' $B$N%G%U%)%k%H$,(B
;;   ";" $B$J$N$G@_Dj$K$h$C$F$O3dEv$F%-!<$,=EJ#$9$k2DG=@-$,$"$j$^$9!#(B

;;; TODO

;; * SPC $B$K3dEv$F$?$$$H$$$&?M$b5o$?$j$9$k(B?

;;; Code:

(require 'skk)

(defvar skk-sticky-key-orig-output nil)
(skk-deflocalvar skk-sticky-okuri-flag nil)

;;; $B=i4|@_Dj(B
(cond ((stringp skk-sticky-key)
       (let ((orig-rule (assoc skk-sticky-key
                               (append skk-rom-kana-rule-list
                                       skk-rom-kana-base-rule-list))))
         (setq skk-sticky-key-orig-output (car (cddr orig-rule)))
         (setq skk-rom-kana-rule-list
               (cons `(,skk-sticky-key nil skk-sticky-set-henkan-point)
                     (delete orig-rule skk-rom-kana-rule-list)))))
      ((vectorp skk-sticky-key)
       (define-key skk-j-mode-map skk-sticky-key 'skk-sticky-set-henkan-point)))

;;; Functions
(defun skk-sticky-looking-back-okuri-mark ()
  "`point' $B$ND>A0$NJ8;z$,(B `*' $B$J$i$P(B t $B$rJV$9!#(B"
  (and (not (bobp))
       (string= "*" (buffer-substring-no-properties
                     (1- (point)) (point)))))

(defun skk-sticky-set-okuri-mark ()
  "$BAw$j3+;OE@$r(B `*' $B$rA^F~$9$k$3$H$GI8<1$7!"Aw$j$"$jJQ49$NBT$A>uBV$KF~$k!#(B"
  (when (eq skk-henkan-mode 'on)
    (unless (and skk-sticky-okuri-flag
                 (skk-sticky-looking-back-okuri-mark))
      (when (and skk-dcomp-activate
                 (skk-dcomp-marked-p))
        (skk-dcomp-before-kakutei))
      (insert-and-inherit "*")
      (setq skk-sticky-okuri-flag t)
      nil)))

(defun skk-sticky-set-henkan-point (&optional arg)
  "$BJQ493+;O0LCV$b$7$/$OAw$j3+;O0LCV$r;XDj$9$k!#(B"
  (interactive "*P")
  (cond ((not (eq skk-henkan-mode 'on))
         (skk-set-henkan-point-subr))
        ((eq (point) (marker-position skk-henkan-start-point))
         (let ((data skk-sticky-key-orig-output))
           (when data
             (skk-kakutei)
             (when (functionp data)
               (setq data (funcall data (skk-make-raw-arg arg))))
             (when (stringp (if (consp data)
                                (car data)
                              data))
               (if (consp data)
                   (if skk-katakana
                       (car data)
                     (cdr data))
                 data)))))
        (t
         (skk-sticky-set-okuri-mark))))

(defadvice skk-kakutei (after skk-sticky-ad activate)
  "`skk-sticky-okuri-flag' $B$r%/%j%"$9$k!#(B"
  (setq skk-sticky-okuri-flag nil))

(defadvice keyboard-quit (after skk-sticky-ad activate)
  "`skk-sticky-okuri-flag' $B$r%/%j%"$9$k!#(B"
  (setq skk-sticky-okuri-flag nil))

(defadvice skk-insert (before skk-sticky-ad activate)
  "`*' $B$ND>8e$G$"$l$PF~NO$rBgJ8;z$KJQ49$9$k!#(B"
  (when (and skk-sticky-okuri-flag
             (skk-sticky-looking-back-okuri-mark)
             (string= "" skk-prefix))
    (let ((pair (rassq last-command-event skk-downcase-alist)))
      (set 'last-command-event (if pair
                                   (car pair)
                                 (upcase last-command-event))))))

(defadvice skk-set-henkan-point (before skk-sticky-ad activate)
  "`point' $BD>A0$N(B `*' $B$r>C$9!#(B"
  (when (and skk-sticky-okuri-flag
             (skk-sticky-looking-back-okuri-mark))
    (delete-backward-char 1)))

;; `skk-kana-input' $B$ODL>o(B `cancel-undo-boundary' $B$r8F$V$,!"(B
;; `skk-sticky-key' $B$N(B2$BEYBG$A$N:]$KK\Mh$"$k$Y$-(B boundary (C-k $B$N8e$J$I(B)
;; $B$b>C$7$F$7$^$&$?$a!"(Badvice $B$K$F8F$P$J$$$h$&$KBP=h$9$k!#(B
;; $B$3$l$K$h$j!"(B`skk-undo-kakutei-word-only' $B$,(B non-nil $B$G$b(B2$BEYBG$A$N;~(B
;; $B$K(B boundary $B$,F~$C$F$7$^$&I{:nMQ$,$"$k$,!"@h$NLdBj$h$j$O%^%7$@$H9M$((B
;; $B$k!#(B
;;; (defadvice skk-kana-input (around skk-sticky-ad activate)
;;;   "$B"&D>8e$N(B `skk-sticky-key' $B$NF~NO$N:](B `cancel-undo-boundary' $B$r8F$P$J$$$h$&$K!#(B"
;;;   (if (and (stringp skk-sticky-key)
;;;        (eq (skk-last-command-char) (string-to-char skk-sticky-key))
;;;        (eq skk-henkan-mode 'on)
;;;        (eq (point) (marker-position skk-henkan-start-point)))
;;;       (progn
;;;     (let ((skk-self-insert-non-undo-count 20))
;;;       ad-do-it)
;;;     (setq skk-self-insert-non-undo-count (1+ skk-self-insert-non-undo-count)))
;;;     ad-do-it))

;;; $BF1;~BG804XO"(B
(defun skk-sticky-double-p (first next)
  "FIRST $B$H(B NEXT $B$,F1;~BG80$G$"$l$P(B non-nil $B$rJV$9!#(B"
  (let ((char (if (characterp first)
                  first
                last-command-event)))
    (and (not (eq char next))
         (memq char skk-sticky-key)
         (memq next skk-sticky-key))))

(defadvice skk-insert (around skk-sticky-ad-double activate)
  "$BF1;~BG80$r8!=P$7$F=hM}$9$k!#(B"
  (cond ((not (consp skk-sticky-key))
         ad-do-it)
        ((not (memq last-command-event skk-sticky-key))
         ad-do-it)
        ((sit-for skk-sticky-double-interval t)
         ;; No input in the interval.
         ad-do-it)
        (t
         ;; Some key's pressed.
         (let ((next-event (read-event)))
           (if (skk-sticky-double-p this-command
                                    (aref (skk-event-key next-event) 0))
               (skk-sticky-set-henkan-point)
             ad-do-it
             (skk-unread-event next-event))))))

(provide 'skk-sticky)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-sticky.el ends here
