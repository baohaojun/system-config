;;; skk-jisx0201.el --- JIS X 0201 (I6E(B, Roman -*- coding: iso-2022-7bit -*-

;; Copyright (C) 1999-2007  SKK Development Team

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
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

;; <$B4JC1$J@bL@(B>

;; ~/.skk $B$K(B

;;   (setq skk-use-jisx0201-input-method t)

;; $B$H=q$/$3$H$G%$%s%9%H!<%k$5$l$^$9!#;H$$J}$O0J2<$N$h$&$K$J$j$^$9!#(B

;;   $B!}%+%?%+%J%b!<%I$K$*$$$F!"(B
;;     $B!&(B"C-q" $B$GA43Q%+%J%b!<%I$HH>3Q%+%J%b!<%I$r@Z$j$+$($^$9!#(B

;;   $B!}$R$i$,$J(B/$B%+%?%+%JN>%b!<%IFb$G$N"&%b!<%I$K$*$$$F!"(B
;;     $B!&(B"C-q" $B$r2!$9$H(I$$B8+=P$78l$H$7$FF~NO$5$l$?$R$i$,$J(B/$B%+%?%+%J$r(IJ]686@6E$B$KJQ(B
;;       $B49$7$^$9!#(B

;; `skk-jisx0201-roman-rule-list' $B$K(B JISX0201.1976 Japanese Roman
;; (latin-jisx0201) $B$NJ8;zNs$rDj5A$7$F$$$^$9!#$?$@$7(B GNU Emacs 23 $B0J9_!"%U%!(B
;; $B%$%kJ]B8;~$K(B JIS X 0201 Roman $B$H(B ASCII $B$N6hJL$,$5$l$J$/$J$C$?$h$&$G$9(B ($B1_(B
;; $B5-9f$*$h$S%*!<%P!<%i%$%s$r=|$/(B)$B!#$7$?$,$C$F$3$N%U%!%$%k$N8=9THG$G$O!"$3$l(B
;; $B$i$N(B 2 $BJ8;z0J30$O(B ASCII $B$NJ8;z$,Dj5A$5$l$F$$$^$9!#(B

;; (I6E(B $B$H(B roman $B$r@Z$jBX$($k5!G=(B `skk-toggle-jisx0201' $B$K$O%-!<Dj5A$7$F$$$^$;(B
;; $B$s!#(B

;; <$B6HL3O"Mm(B>

;; GNU Emacs 20.3 $B!A(B 22 $B$G$3$N%U%!%$%k$rJT=8$9$k>l9g$O!"%U%!%$%k$r3+$/A0$K(B

;;   (setq standard-translation-table-for-decode (make-translation-table nil))

;; $B$rI>2A$7$F$/$@$5$$!#(B

;;; Code:

(require 'skk)

(require 'japan-util)

;; $B=tHL$N;v>p$K$h$j(B skk-vars.el $B$KF~$l$k$Y$-$G$J$$JQ?t(B
(defvar skk-jisx0201-base-rule-list
  '(("a" nil "(I1(B")
    ("bb" "b" "(I/(B") ("ba" nil "(IJ^(B") ("be" nil "(IM^(B")
    ("bi" nil "(IK^(B") ("bo" nil "(IN^(B") ("bu" nil "(IL^(B") ("bya" nil "(IK^,(B")
    ("bye" nil "(IK^*(B") ("byi" nil "(IK^((B") ("byo" nil "(IK^.(B") ("byu" nil "(IK^-(B")
    ("cc" "c" "(I/(B") ("cha" nil "(IA,(B") ("che" nil "(IA*(B") ("chi" nil "(IA(B")
    ("cho" nil "(IA.(B") ("chu" nil "(IA-(B") ("cya" nil "(IA,(B") ("cye" nil "(IA*(B")
    ("cyi" nil "(IA((B") ("cyo" nil "(IA.(B") ("cyu" nil "(IA-(B")
    ("dd" "d" "(I/(B") ("da" nil "(I@^(B") ("de" nil "(IC^(B") ("dha" nil "(IC^,(B")
    ("dhe" nil "(IC^*(B") ("dhi" nil "(IC^((B") ("dho" nil "(IC^.(B") ("dhu" nil "(IC^-(B")
    ("di" nil "(IA^(B") ("do" nil "(ID^(B") ("du" nil "(IB^(B") ("dya" nil "(IA^,(B")
    ("dye" nil "(IA^*(B") ("dyi" nil "(IA^((B") ("dyo" nil "(IA^.(B") ("dyu" nil "(IA^-(B")
    ("e" nil "(I4(B")
    ("ff" "f" "(I/(B") ("fa" nil "(IL'(B") ("fe" nil "(IL*(B") ("fi" nil "(IL((B")
    ("fo" nil "(IL+(B") ("fu" nil "(IL(B") ("fya" nil "(IL,(B") ("fye" nil "(IL*(B")
    ("fyi" nil "(IL((B") ("fyo" nil "(IL.(B") ("fyu" nil "(IL-(B") ("gg" "g" "(I/(B")
    ("ga" nil "(I6^(B") ("ge" nil "(I9^(B") ("gi" nil "(I7^(B") ("go" nil "(I:^(B")
    ("gu" nil "(I8^(B") ("gya" nil "(I7^,(B") ("gye" nil "(I7^*(B") ("gyi" nil "(I7^((B")
    ("gyo" nil "(I7^.(B") ("gyu" nil "(I7^-(B")
    ("ha" nil "(IJ(B") ("he" nil "(IM(B") ("hi" nil "(IK(B") ("ho" nil "(IN(B")
    ("hu" nil "(IL(B") ("hya" nil "(IK,(B") ("hye" nil "(IK*(B") ("hyi" nil "(IK((B")
    ("hyo" nil "(IK.(B") ("hyu" nil "(IK-(B") ("i" nil "(I2(B")
    ("jj" "j" "(I/(B") ("ja" nil "(I<^,(B") ("je" nil "(I<^*(B") ("ji" nil "(I<^(B")
    ("jo" nil "(I<^.(B") ("ju" nil "(I<^-(B") ("jya" nil "(I<^,(B") ("jye" nil "(I<^*(B")
    ("jyi" nil "(I<^((B") ("jyo" nil "(I<^.(B") ("jyu" nil "(I<^-(B")
    ("kk" "k" "(I/(B") ("ka" nil "(I6(B") ("ke" nil "(I9(B") ("ki" nil "(I7(B")
    ("ko" nil "(I:(B") ("ku" nil "(I8(B") ("kya" nil "(I7,(B") ("kye" nil "(I7*(B")
    ("kyi" nil "(I7((B") ("kyo" nil "(I7.(B") ("kyu" nil "(I7-(B")
    ("mm" "c" "(I/(B") ("ma" nil "(IO(B") ("me" nil "(IR(B") ("mi" nil "(IP(B")
    ("mo" nil "(IS(B") ("mu" nil "(IQ(B") ("mya" nil "(IP,(B") ("mye" nil "(IP*(B")
    ("myi" nil "(IP((B") ("myo" nil "(IP.(B") ("myu" nil "(IP-(B")
    ("n" nil "(I](B") ("n'" nil "(I](B") ("na" nil "(IE(B") ("ne" nil "(IH(B")
    ("ni" nil "(IF(B") ("nn" nil "(I](B") ("no" nil "(II(B") ("nu" nil "(IG(B")
    ("nya" nil "(IF,(B") ("nye" nil "(IF*(B") ("nyi" nil "(IF((B") ("nyo" nil "(IF.(B")
    ("nyu" nil "(IF-(B")
    ("o" nil "(I5(B")
    ("pp" "p" "(I/(B") ("pa" nil "(IJ_(B") ("pe" nil "(IM_(B") ("pi" nil "(IK_(B")
    ("po" nil "(IN_(B") ("pu" nil "(IL_(B") ("pya" nil "(IK_,(B") ("pye" nil "(IK_*(B")
    ("pyi" nil "(IK_((B") ("pyo" nil "(IK_.(B") ("pyu" nil "(IK_-(B")
    ("rr" "r" "(I/(B") ("ra" nil "(IW(B") ("re" nil "(IZ(B") ("ri" nil "(IX(B")
    ("ro" nil "(I[(B") ("ru" nil "(IY(B") ("rya" nil "(IX,(B") ("rye" nil "(IX*(B")
    ("ryi" nil "(IX((B") ("ryo" nil "(IX.(B") ("ryu" nil "(IX-(B")
    ("ss" "s" "(I/(B") ("sa" nil "(I;(B") ("se" nil "(I>(B") ("sha" nil "(I<,(B")
    ("she" nil "(I<*(B") ("shi" nil "(I<(B") ("sho" nil "(I<.(B") ("shu" nil "(I<-(B")
    ("si" nil "(I<(B") ("so" nil "(I?(B") ("su" nil "(I=(B") ("sya" nil "(I<,(B")
    ("sye" nil "(I<*(B") ("syi" nil "(I<((B") ("syo" nil "(I<.(B") ("syu" nil "(I<-(B")
    ("tt" "t" "(I/(B") ("ta" nil "(I@(B") ("te" nil "(IC(B") ("tha" nil "(IC'(B")
    ("the" nil "(IC*(B") ("thi" nil "(IC((B") ("tho" nil "(IC.(B") ("thu" nil "(IC-(B")
    ("ti" nil "(IA(B") ("to" nil "(ID(B") ("tsu" nil "(IB(B") ("tu" nil "(IB(B")
    ("tya" nil "(IA,(B") ("tye" nil "(IA*(B") ("tyi" nil "(IA((B") ("tyo" nil "(IA.(B")
    ("tyu" nil "(IA-(B")
    ("u" nil "(I3(B")
    ("vv" "v" "(I/(B") ("va" nil "(I3^'(B") ("ve" nil "(I3^*(B") ("vi" nil "(I3^((B")
    ("vo" nil "(I3^+(B") ("vu" nil "(I3^(B")
    ("ww" "w" "(I/(B") ("wa" nil "(I\(B") ("we" nil "(I3*(B") ("wi" nil "(I3((B")
    ("wo" nil "(I&(B") ("wu" nil "(I3(B")
    ("xx" "x" "(I/(B") ("xa" nil "(I'(B") ("xe" nil "(I*(B") ("xi" nil "(I((B")
    ("xka" nil "(I6(B") ("xke" nil "(I9(B") ("xo" nil "(I+(B") ("xtsu" nil "(I/(B")
    ("xtu" nil "(I/(B") ("xu" nil "(I)(B") ("xwa" nil "(I\(B") ("xwe" nil "(I*(B")
    ("xwi" nil "(I((B") ("xya" nil "(I,(B") ("xyo" nil "(I.(B") ("xyu" nil "(I-(B")
    ("yy" "y" "(I/(B") ("ya" nil "(IT(B") ("ye" nil "(I2*(B") ("yo" nil "(IV(B")
    ("yu" nil "(IU(B")
    ("zz" "z" "(I/(B") ("z," nil "$B!E(B") ("z-" nil "$B!A(B") ("z." nil "$B!D(B")
    ("z/" nil "(I%(B") ("z[" nil "$B!X(B") ("z]" nil "$B!Y(B") ("za" nil "(I;^(B")
    ("ze" nil "(I>^(B") ("zh" nil "$B"+(B") ("zi" nil "(I<^(B") ("zj" nil "$B"-(B")
    ("zk" nil "$B",(B") ("zl" nil "$B"*(B") ("zo" nil "(I?^(B") ("zu" nil "(I=^(B")
    ("zya" nil "(I<^,(B") ("zye" nil "(I<^*(B") ("zyi" nil "(I<^((B") ("zyo" nil "(I<^.(B")
    ("zyu" nil "(I<^-(B")
    ("," nil "(I$(B") ("." nil "(I!(B") ("-" nil "(I0(B") (":" nil ":") (";" nil ";")
    ("?" nil "?") ("[" nil "(I"(B") ("]" nil "(I#(B")
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
  "*SKK JISX0201 $B%b!<%I$N%Y!<%9$N%k!<%k!#(B")

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
    ("[" nil "[") ("\\" nil "\(J\(B") ("]" nil "]") ("^" nil "^") ("_" nil "_")
    ("`" nil "`")
    ("a" nil "a") ("b" nil "b") ("c" nil "c") ("d" nil "d") ("e" nil "e")
    ("f" nil "f") ("g" nil "g") ("h" nil "h") ("i" nil "i") ("j" nil "j")
    ("k" nil "k") ("l" nil "l") ("m" nil "m") ("n" nil "n") ("o" nil "o")
    ("p" nil "p") ("q" nil "q") ("r" nil "r") ("s" nil "s") ("t" nil "t")
    ("u" nil "u") ("v" nil "v") ("w" nil "w") ("x" nil "x") ("y" nil "y")
    ("z" nil "z")
    ("{" nil "{") ("|" nil "|") ("}" nil "}") ("~" nil "(J~(B"))
  "*SKK JISX0201 $B%b!<%I$N(B Roman $B$N%k!<%k!#(B")

(defvar skk-jisx0201-rule-list
  '(("\(" nil "(")
    ("{" nil "{"))
  "*SKK JISX0201 $B%b!<%I$NDI2C$N%k!<%k!#(B")

(setq skk-jisx0201-base-rule-tree
      (skk-compile-rule-list skk-jisx0201-base-rule-list
                             skk-jisx0201-rule-list))
(setq skk-jisx0201-roman-rule-tree
      (skk-compile-rule-list skk-jisx0201-roman-rule-list))

;; Hooks.

;; inline functions.
(defsubst skk-jisx0201-mode-on (&optional arg)
  "SKK JIS X 0201 ($B%+%J(B) $B%b!<%I$r5/F0$9$k!#(B"
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
  "$BH>3Q%+%J$NAw$j2>L>$r@5$7$/<hF@$9$k!#(B"
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
       ((member okuri '("(I^(B" "(I_(B"))
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
        (unless (member sokuon '("(I/(B"))
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
  "SKK JIS X 0201 $B%b!<%I$NJ8;zF~NO$r9T$&!#(B"
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
  "SKK JIS X 0201 $B%b!<%I$G$OAw$j2>L>$rH>3Q%+%J$K$9$k!#(B"
  (when skk-jisx0201-mode
    (ad-set-arg 0 '("(I;(B" "(I<(B" "(I=(B" "(I>(B"))))

;; functions.
;;;###autoload
(defun skk-jisx0201-mode (arg)
  "SKK $B$N%b!<%I$r(B JIS X 0201 $B%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0201-mode-on))

(defun skk-toggle-jisx0201 (arg)
  "$BH>3Q%+%J%b!<%I$H%m!<%^;z%b!<%I$r@Z$jBX$($k!#(B"
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
  "STR $B$N(B JIS X 0201 $B%+%JJ8;z$rBP1~$9$k(B JIS X 0208 $B$NJ8;z$GCV$-49$($k!#(B"
  (skk-jisx0201-string-conversion str #'skk-jisx0201-zenkaku-region))

(defun skk-jisx0201-hankaku (str)
  "STR $B$N(B JIS X 0208 $BJ8;z$rBP1~$9$k(B JIS X 0201 $B%+%J$NJ8;z$GCV$-49$($k!#(B"
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
  "$B"&%b!<%I$G$"$l$P!"NN0h$N$R$i$,$J(B/$B%+%?%+%J$r(B (IJ]686@6E(B $B$KJQ49$9$k!#(B
$B"'%b!<%I$G$O2?$b$7$J$$!#(B
$B$=$NB>$N%b!<%I$G$O!"%*%j%8%J%k$N%-!<3d$jIU$1$G%P%$%s%I$5$l$F$$$k%3%^%s%I$r<B9T(B
$B$9$k!#(B"
  (interactive "*P")
  (skk-henkan-skk-region-by-func #'skk-jisx0201-region arg))

(defun skk-jisx0201-region (start end)
  "$BNN0h$N$R$i$,$J(B/$B%+%?%+%J$r(B (IJ]686@6E(B $B$KJQ49$9$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B"
  (interactive "*r\nP")
  (setq end (set-marker (make-marker) end))
  (skk-hiragana-to-jisx0201-region start end)
  (skk-katakana-to-jisx0201-region start end)
  (set-marker end nil))

;;;###autoload
(defun skk-hiragana-to-jisx0201-region (start end)
  (skk-search-and-replace
   start end
   "[$B$!(B-$B$s!#!"!&!<!+!,(B]+"
   (lambda (matched)
     (save-match-data
       (skk-jisx0201-hankaku matched)))))

;;;###autoload
(defun skk-katakana-to-jisx0201-region (start end)
  (skk-search-and-replace
   start end
   "[$B%!(B-$B%t!#!"!&!<!+!,(B]+"
   (lambda (matched)
     (save-match-data
       (skk-jisx0201-hankaku matched)))))

(provide 'skk-jisx0201)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-jisx0201.el ends here
