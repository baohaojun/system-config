;;; skk-tut.el --- SKK tutorial -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000, 2001, 2002
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999-2010  SKK Development Team

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

;;; Code:

(require 'skk)

(eval-and-compile
  (autoload 'skk-nicola-setup-tutorial "skk-nicola"))

(eval-when-compile
  (defvar skk-kanagaki-keyboard-type)
  (defvar skk-kanagaki-state))

;; internal variables and constants.
;; prefix should be `skktut-'.
(defvar skk-tut-section-face 'skk-tut-section-face)
(defvar skk-tut-do-it-face 'skk-tut-do-it-face)
(defvar skk-tut-question-face 'skk-tut-question-face)
(defvar skk-tut-key-bind-face 'skk-tut-key-bind-face)
(defvar skk-tut-hint-face 'skk-tut-hint-face)

(defconst skktut-adviced-alist
  '((skk-abbrev-mode . before)
    (skk-insert . before)
    (skk-kakutei . before)
    (skk-mode . before)
    (skk-create-file . around)
    (skk-save-jisyo-original . around)
    (skk-get-jisyo-buffer . around))
  "SKK $B%A%e!<%H%j%"%k$G(B advice $B$,IU$1$i$l$k4X?t$H(B advice class $B$N%(!<%j%9%H!#(B")

(defvar skktut-question-numbers nil "SKK $B%A%e!<%H%j%"%k$NLdBj?t!#(B")

(defconst skktut-tut-jisyo "~/skk-tut-jisyo"
  "SKK $B%A%e!<%H%j%"%kMQ$N<-=q!#(B")

(defconst skktut-init-variables-alist
  '((skk-cursor-abbrev-color . "royalblue")
    (skk-abbrev-mode-string . " a$B$"(B")
    (skk-allow-spaces-newlines-and-tabs . t)
    (skk-auto-fill-mode-hook . nil)
    (skk-auto-insert-paren . nil)
    (skk-auto-okuri-process . nil)
    (skk-auto-start-henkan . nil)
    (skk-byte-compile-init-file . nil)
    (skk-comp-load-hook . nil)
    (skk-compare-jisyo-size-when-saving . nil)
    (skk-count-jisyo-candidates-function
     . 'skk-count-jisyo-candidates-original)
    (skk-count-private-jisyo-candidates-exactly . nil)
    (skk-date-ad . 1)
    (skk-cursor-default-color . (cdr (assq 'cursor-color
                                           (frame-parameters (selected-frame)))))
    (skk-delete-implies-kakutei . t)
    (skk-delete-okuri-when-quit . nil)
    (skk-downcase-alist . nil)
    (skk-echo . t)
    (skk-egg-like-newline . nil)
    (skk-gadget-load-hook . nil)
    (skk-henkan-face . 'highlight)
    (skk-henkan-okuri-strictly . nil)
    (skk-henkan-overlay-priority . 600)
    (skk-henkan-show-candidates-keys . '(?a ?s ?d ?f ?j ?k ?l))
    (skk-henkan-number-to-display-candidates . 7)
    (skk-show-candidates-always-pop-to-buffer . nil)
    (skk-henkan-strict-okuri-precedence . nil)
    (skk-previous-candidate-keys
     . (if (skk-tut-nicola-p)
           '("\C-p")
         '("\C-p" "x")))
    (skk-hiragana-mode-string . " $B$+$J(B")
    (skk-init-file . "")
    (skk-input-by-code-menu-keys1 . '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y))
    (skk-input-by-code-menu-keys2
     . '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u))
    (skk-japanese-message-and-error . nil)
    (skk-cursor-jisx0208-latin-color . "gold")
    (skk-jisx0208-latin-mode-string . " $BA41Q(B")
    (skk-jisx0208-latin-vector . skk-default-jisx0208-latin-vector)
    (skk-jisyo . "~/skk-tut-jisyo")
    (skk-jisyo-save-count . nil)
    (skk-update-jisyo-count . 0)
    (skk-kakutei-early . t)
    (skk-kakutei-key . "\C-j")
    (skk-kana-input-search-function
     . (lambda ()
         (save-match-data
           (when (and (string-match
                       "^h\\([bcdfghjklmnpqrstvwxz]\\)$"
                       skk-prefix)
                      (member (char-to-string (preceding-char))
                              '("$B$*(B" "$B%*(B")))
             (cons '("$B%*(B" . "$B$*(B") (match-string 1 skk-prefix))))))
    (skk-katakana-mode-string . " $B%+%J(B")
    (skk-kcode-load-hook . nil)
    (skk-keep-record . nil)
    (skk-kuten-touten-alist . '((jp . ("$B!#(B" . "$B!"(B"))))
    (skk-kutouten-type . 'jp)
    (skk-cursor-latin-color . "ivory4")
    (skk-latin-mode-string . " SKK")
    (skk-load-hook . nil)
    (skk-mode-hook . nil)
    (skk-next-completion-char . ?.)
    (skk-number-style . 1)
    (skk-okuri-char-alist . nil)
    (skk-previous-completion-char . ?,)
    (skk-process-okuri-early . nil)
    (skk-public-jisyo-has-entry-p-function
     . 'skk-public-jisyo-has-entry-p-original)
    (skk-rom-kana-base-rule-list
     .
     '(("a" nil ("$B%"(B" . "$B$"(B")) ("bb" "b" ("$B%C(B" . "$B$C(B"))
       ("ba" nil ("$B%P(B" . "$B$P(B")) ("be" nil ("$B%Y(B" . "$B$Y(B"))
       ("bi" nil ("$B%S(B" . "$B$S(B")) ("bo" nil ("$B%\(B" . "$B$\(B"))
       ("bu" nil ("$B%V(B" . "$B$V(B")) ("bya" nil ("$B%S%c(B" . "$B$S$c(B"))
       ("bye" nil ("$B%S%'(B" . "$B$S$'(B")) ("byi" nil ("$B%S%#(B" . "$B$S$#(B"))
       ("byo" nil ("$B%S%g(B" . "$B$S$g(B")) ("byu" nil ("$B%S%e(B" . "$B$S$e(B"))
       ("cc" "c" ("$B%C(B" . "$B$C(B")) ("cha" nil ("$B%A%c(B" . "$B$A$c(B"))
       ("che" nil ("$B%A%'(B" . "$B$A$'(B")) ("chi" nil ("$B%A(B" . "$B$A(B"))
       ("cho" nil ("$B%A%g(B" . "$B$A$g(B")) ("chu" nil ("$B%A%e(B" . "$B$A$e(B"))
       ("cya" nil ("$B%A%c(B" . "$B$A$c(B")) ("cye" nil ("$B%A%'(B" . "$B$A$'(B"))
       ("cyi" nil ("$B%A%#(B" . "$B$A$#(B")) ("cyo" nil ("$B%A%g(B" . "$B$A$g(B"))
       ("cyu" nil ("$B%A%e(B" . "$B$A$e(B")) ("dd" "d" ("$B%C(B" . "$B$C(B"))
       ("da" nil ("$B%@(B" . "$B$@(B")) ("de" nil ("$B%G(B" . "$B$G(B"))
       ("dha" nil ("$B%G%c(B" . "$B$G$c(B")) ("dhe" nil ("$B%G%'(B" . "$B$G$'(B"))
       ("dhi" nil ("$B%G%#(B" . "$B$G$#(B")) ("dho" nil ("$B%G%g(B" . "$B$G$g(B"))
       ("dhu" nil ("$B%G%e(B" . "$B$G$e(B")) ("di" nil ("$B%B(B" . "$B$B(B"))
       ("do" nil ("$B%I(B" . "$B$I(B")) ("du" nil ("$B%E(B" . "$B$E(B"))
       ("dya" nil ("$B%B%c(B" . "$B$B$c(B")) ("dye" nil ("$B%B%'(B" . "$B$B$'(B"))
       ("dyi" nil ("$B%B%#(B" . "$B$B$#(B")) ("dyo" nil ("$B%B%g(B" . "$B$B$g(B"))
       ("dyu" nil ("$B%B%e(B" . "$B$B$e(B")) ("e" nil ("$B%((B" . "$B$((B"))
       ("ff" "f" ("$B%C(B" . "$B$C(B")) ("fa" nil ("$B%U%!(B" . "$B$U$!(B"))
       ("fe" nil ("$B%U%'(B" . "$B$U$'(B")) ("fi" nil ("$B%U%#(B" . "$B$U$#(B"))
       ("fo" nil ("$B%U%)(B" . "$B$U$)(B")) ("fu" nil ("$B%U(B" . "$B$U(B"))
       ("fya" nil ("$B%U%c(B" . "$B$U$c(B")) ("fye" nil ("$B%U%'(B" . "$B$U$'(B"))
       ("fyi" nil ("$B%U%#(B" . "$B$U$#(B")) ("fyo" nil ("$B%U%g(B" . "$B$U$g(B"))
       ("fyu" nil ("$B%U%e(B" . "$B$U$e(B")) ("gg" "g" ("$B%C(B" . "$B$C(B"))
       ("ga" nil ("$B%,(B" . "$B$,(B")) ("ge" nil ("$B%2(B" . "$B$2(B"))
       ("gi" nil ("$B%.(B" . "$B$.(B")) ("go" nil ("$B%4(B" . "$B$4(B"))
       ("gu" nil ("$B%0(B" . "$B$0(B")) ("gya" nil ("$B%.%c(B" . "$B$.$c(B"))
       ("gye" nil ("$B%.%'(B" . "$B$.$'(B")) ("gyi" nil ("$B%.%#(B" . "$B$.$#(B"))
       ("gyo" nil ("$B%.%g(B" . "$B$.$g(B")) ("gyu" nil ("$B%.%e(B" . "$B$.$e(B"))
       ;;("h" "" ("$B%*(B" . "$B$*(B"))
       ("ha" nil ("$B%O(B" . "$B$O(B")) ("he" nil ("$B%X(B" . "$B$X(B"))
       ("hi" nil ("$B%R(B" . "$B$R(B")) ("ho" nil ("$B%[(B" . "$B$[(B"))
       ("hu" nil ("$B%U(B" . "$B$U(B")) ("hya" nil ("$B%R%c(B" . "$B$R$c(B"))
       ("hye" nil ("$B%R%'(B" . "$B$R$'(B")) ("hyi" nil ("$B%R%#(B" . "$B$R$#(B"))
       ("hyo" nil ("$B%R%g(B" . "$B$R$g(B")) ("hyu" nil ("$B%R%e(B" . "$B$R$e(B"))
       ("i" nil ("$B%$(B" . "$B$$(B")) ("jj" "j" ("$B%C(B" . "$B$C(B"))
       ("ja" nil ("$B%8%c(B" . "$B$8$c(B")) ("je" nil ("$B%8%'(B" . "$B$8$'(B"))
       ("ji" nil ("$B%8(B" . "$B$8(B")) ("jo" nil ("$B%8%g(B" . "$B$8$g(B"))
       ("ju" nil ("$B%8%e(B" . "$B$8$e(B")) ("jya" nil ("$B%8%c(B" . "$B$8$c(B"))
       ("jye" nil ("$B%8%'(B" . "$B$8$'(B")) ("jyi" nil ("$B%8%#(B" . "$B$8$#(B"))
       ("jyo" nil ("$B%8%g(B" . "$B$8$g(B")) ("jyu" nil ("$B%8%e(B" . "$B$8$e(B"))
       ("kk" "k" ("$B%C(B" . "$B$C(B")) ("ka" nil ("$B%+(B" . "$B$+(B"))
       ("ke" nil ("$B%1(B" . "$B$1(B")) ("ki" nil ("$B%-(B" . "$B$-(B"))
       ("ko" nil ("$B%3(B" . "$B$3(B")) ("ku" nil ("$B%/(B" . "$B$/(B"))
       ("kya" nil ("$B%-%c(B" . "$B$-$c(B")) ("kye" nil ("$B%-%'(B" . "$B$-$'(B"))
       ("kyi" nil ("$B%-%#(B" . "$B$-$#(B")) ("kyo" nil ("$B%-%g(B" . "$B$-$g(B"))
       ("kyu" nil ("$B%-%e(B" . "$B$-$e(B")) ("mm" "c" ("$B%C(B" . "$B$C(B"))
       ("ma" nil ("$B%^(B" . "$B$^(B")) ("me" nil ("$B%a(B" . "$B$a(B"))
       ("mi" nil ("$B%_(B" . "$B$_(B")) ("mo" nil ("$B%b(B" . "$B$b(B"))
       ("mu" nil ("$B%`(B" . "$B$`(B")) ("mya" nil ("$B%_%c(B" . "$B$_$c(B"))
       ("mye" nil ("$B%_%'(B" . "$B$_$'(B")) ("myi" nil ("$B%_%#(B" . "$B$_$#(B"))
       ("myo" nil ("$B%_%g(B" . "$B$_$g(B")) ("myu" nil ("$B%_%e(B" . "$B$_$e(B"))
       ("n" nil ("$B%s(B" . "$B$s(B")) ("n'" nil ("$B%s(B" . "$B$s(B"))
       ("na" nil ("$B%J(B" . "$B$J(B")) ("ne" nil ("$B%M(B" . "$B$M(B"))
       ("ni" nil ("$B%K(B" . "$B$K(B")) ("nn" nil ("$B%s(B" . "$B$s(B"))
       ("no" nil ("$B%N(B" . "$B$N(B")) ("nu" nil ("$B%L(B" . "$B$L(B"))
       ("nya" nil ("$B%K%c(B" . "$B$K$c(B")) ("nye" nil ("$B%K%'(B" . "$B$K$'(B"))
       ("nyi" nil ("$B%K%#(B" . "$B$K$#(B")) ("nyo" nil ("$B%K%g(B" . "$B$K$g(B"))
       ("nyu" nil ("$B%K%e(B" . "$B$K$e(B")) ("o" nil ("$B%*(B" . "$B$*(B"))
       ("pp" "p" ("$B%C(B" . "$B$C(B")) ("pa" nil ("$B%Q(B" . "$B$Q(B"))
       ("pe" nil ("$B%Z(B" . "$B$Z(B")) ("pi" nil ("$B%T(B" . "$B$T(B"))
       ("po" nil ("$B%](B" . "$B$](B")) ("pu" nil ("$B%W(B" . "$B$W(B"))
       ("pya" nil ("$B%T%c(B" . "$B$T$c(B")) ("pye" nil ("$B%T%'(B" . "$B$T$'(B"))
       ("pyi" nil ("$B%T%#(B" . "$B$T$#(B")) ("pyo" nil ("$B%T%g(B" . "$B$T$g(B"))
       ("pyu" nil ("$B%T%e(B" . "$B$T$e(B")) ("rr" "r" ("$B%C(B" . "$B$C(B"))
       ("ra" nil ("$B%i(B" . "$B$i(B")) ("re" nil ("$B%l(B" . "$B$l(B"))
       ("ri" nil ("$B%j(B" . "$B$j(B")) ("ro" nil ("$B%m(B" . "$B$m(B"))
       ("ru" nil ("$B%k(B" . "$B$k(B")) ("rya" nil ("$B%j%c(B" . "$B$j$c(B"))
       ("rye" nil ("$B%j%'(B" . "$B$j$'(B")) ("ryi" nil ("$B%j%#(B" . "$B$j$#(B"))
       ("ryo" nil ("$B%j%g(B" . "$B$j$g(B")) ("ryu" nil ("$B%j%e(B" . "$B$j$e(B"))
       ("ss" "s" ("$B%C(B" . "$B$C(B")) ("sa" nil ("$B%5(B" . "$B$5(B"))
       ("se" nil ("$B%;(B" . "$B$;(B")) ("sha" nil ("$B%7%c(B" . "$B$7$c(B"))
       ("she" nil ("$B%7%'(B" . "$B$7$'(B")) ("shi" nil ("$B%7(B" . "$B$7(B"))
       ("sho" nil ("$B%7%g(B" . "$B$7$g(B")) ("shu" nil ("$B%7%e(B" . "$B$7$e(B"))
       ("si" nil ("$B%7(B" . "$B$7(B")) ("so" nil ("$B%=(B" . "$B$=(B"))
       ("su" nil ("$B%9(B" . "$B$9(B")) ("sya" nil ("$B%7%c(B" . "$B$7$c(B"))
       ("sye" nil ("$B%7%'(B" . "$B$7$'(B")) ("syi" nil ("$B%7%#(B" . "$B$7$#(B"))
       ("syo" nil ("$B%7%g(B" . "$B$7$g(B")) ("syu" nil ("$B%7%e(B" . "$B$7$e(B"))
       ("tt" "t" ("$B%C(B" . "$B$C(B")) ("ta" nil ("$B%?(B" . "$B$?(B"))
       ("te" nil ("$B%F(B" . "$B$F(B")) ("tha" nil ("$B%F%!(B" . "$B$F$!(B"))
       ("the" nil ("$B%F%'(B" . "$B$F$'(B")) ("thi" nil ("$B%F%#(B" . "$B$F$#(B"))
       ("tho" nil ("$B%F%g(B" . "$B$F$g(B")) ("thu" nil ("$B%F%e(B" . "$B$F$e(B"))
       ("ti" nil ("$B%A(B" . "$B$A(B")) ("to" nil ("$B%H(B" . "$B$H(B"))
       ("tsu" nil ("$B%D(B" . "$B$D(B")) ("tu" nil ("$B%D(B" . "$B$D(B"))
       ("tya" nil ("$B%A%c(B" . "$B$A$c(B")) ("tye" nil ("$B%A%'(B" . "$B$A$'(B"))
       ("tyi" nil ("$B%A%#(B" . "$B$A$#(B")) ("tyo" nil ("$B%A%g(B" . "$B$A$g(B"))
       ("tyu" nil ("$B%A%e(B" . "$B$A$e(B")) ("u" nil ("$B%&(B" . "$B$&(B"))
       ("vv" "v" ("$B%C(B" . "$B$C(B")) ("va" nil ("$B%t%!(B" . "$B$&!+$!(B"))
       ("ve" nil ("$B%t%'(B" . "$B$&!+$'(B")) ("vi" nil ("$B%t%#(B" . "$B$&!+$#(B"))
       ("vo" nil ("$B%t%)(B" . "$B$&!+$)(B")) ("vu" nil ("$B%t(B" . "$B$&!+(B"))
       ("ww" "w" ("$B%C(B" . "$B$C(B")) ("wa" nil ("$B%o(B" . "$B$o(B"))
       ("we" nil ("$B%&%'(B" . "$B$&$'(B")) ("wi" nil ("$B%&%#(B" . "$B$&$#(B"))
       ("wo" nil ("$B%r(B" . "$B$r(B")) ("wu" nil ("$B%&(B" . "$B$&(B"))
       ("xx" "x" ("$B%C(B" . "$B$C(B")) ("xa" nil ("$B%!(B" . "$B$!(B"))
       ("xe" nil ("$B%'(B" . "$B$'(B")) ("xi" nil ("$B%#(B" . "$B$#(B"))
       ("xka" nil ("$B%u(B" . "$B$+(B")) ("xke" nil ("$B%v(B" . "$B$1(B"))
       ("xo" nil ("$B%)(B" . "$B$)(B")) ("xtsu" nil ("$B%C(B" . "$B$C(B"))
       ("xtu" nil ("$B%C(B" . "$B$C(B")) ("xu" nil ("$B%%(B" . "$B$%(B"))
       ("xwa" nil ("$B%n(B" . "$B$n(B")) ("xwe" nil ("$B%q(B" . "$B$q(B"))
       ("xwi" nil ("$B%p(B" . "$B$p(B")) ("xya" nil ("$B%c(B" . "$B$c(B"))
       ("xyo" nil ("$B%g(B" . "$B$g(B")) ("xyu" nil ("$B%e(B" . "$B$e(B"))
       ("yy" "y" ("$B%C(B" . "$B$C(B")) ("ya" nil ("$B%d(B" . "$B$d(B"))
       ("ye" nil ("$B%$%'(B" . "$B$$$'(B")) ("yo" nil ("$B%h(B" . "$B$h(B"))
       ("yu" nil ("$B%f(B" . "$B$f(B")) ("zz" "z" ("$B%C(B" . "$B$C(B"))
       ("z," nil "$B!E(B") ("z-" nil "$B!A(B") ("z." nil "$B!D(B")
       ("z/" nil "$B!&(B") ("z[" nil "$B!X(B") ("z]" nil "$B!Y(B")
       ("za" nil ("$B%6(B" . "$B$6(B")) ("ze" nil ("$B%<(B" . "$B$<(B"))
       ("zh" nil "$B"+(B") ("zi" nil ("$B%8(B" . "$B$8(B"))
       ("zj" nil "$B"-(B") ("zk" nil "$B",(B") ("zl" nil "$B"*(B")
       ("zo" nil ("$B%>(B" . "$B$>(B")) ("zu" nil ("$B%:(B" . "$B$:(B"))
       ("zya" nil ("$B%8%c(B" . "$B$8$c(B")) ("zye" nil ("$B%8%'(B" . "$B$8$'(B"))
       ("zyi" nil ("$B%8%#(B" . "$B$8$#(B")) ("zyo" nil ("$B%8%g(B" . "$B$8$g(B"))
       ("zyu" nil ("$B%8%e(B" . "$B$8$e(B")) ("." nil skk-current-kuten)
       ("," nil skk-current-touten) ("-" nil "$B!<(B")
       (":" nil "$B!'(B") (";" nil "$B!((B") ("?" nil "$B!)(B")
       ("[" nil "$B!V(B") ("]" nil "$B!W(B") ("l" nil skk-latin-mode)
       ("q" nil skk-toggle-characters) ("L" nil skk-jisx0208-latin-mode)
       ("Q" nil skk-set-henkan-point-subr)
       ("X" nil skk-purge-from-jisyo) ("/" nil skk-abbrev-mode)
       ("$" nil skk-display-code-for-char-at-point)
       ("\C-j" nil skk-kakutei)
       ("@" nil skk-today) ("\\" nil skk-input-by-code-or-menu)))
    (skk-rom-kana-rule-list . '(("hh" "h" ("$B%C(B" . "$B$C(B"))))
    (skk-save-jisyo-function . 'skk-save-jisyo-original)
    (skk-search-excluding-word-pattern-function . nil)
    (skk-search-prog-list . '((skk-search-jisyo-buf
                               (get-buffer skktut-jisyo-buffer) 0)))
    (skk-set-henkan-point-key
     . '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z))
    (skk-share-private-jisyo . nil)
    (skk-special-midashi-char-list . '(?> ?< ??))
    (skk-start-henkan-char . ?\040)
    (skk-show-candidates-nth-henkan-char . 5)
    (skk-try-completion-char . ?\011)
    (skk-update-jisyo-function . 'skktut-update-jisyo)
    (skk-use-color-cursor . (and window-system
                                 (fboundp 'x-display-color-p)
                                 (x-display-color-p)))
    (skk-use-face . window-system)
    (skk-use-look . nil)
    (skk-use-numeric-conversion . t)
    (skk-use-rdbms . nil)
    (skk-use-relation . nil)
    (skk-use-viper . nil)
    (skk-show-tooltip . nil)
    (skk-show-inline . nil)
    (skk-show-annotation . nil)
    (skk-kcode-charset . (if (featurep 'jisx0213) ; Mule-UCS
                             (quote japanese-jisx0208)
                           skk-kcode-charset))
    (skk-kcode-method . 'code-or-char-list)
    (skk-dcomp-activate . nil)
    (skk-dcomp-multiple-activate . nil)
    (skk-read-from-minibuffer-function . nil)
    (skk-verbose . nil)
    (debug-on-error . nil)
    (skk-show-mode-show . nil)
    (skk-show-mode-enable . nil)

    ;; not user variables but to be localized.
    (skk-insert-new-word-function . nil)
    (skk-mode-invoked . t)
    (skk-rule-tree
     .
     (if (and skk-use-kana-keyboard
              (memq skk-kanagaki-keyboard-type '(oasys
                                                 nicola-jis nicola-us nicola-dvorak
                                                 omelet-jis omelet-us omelet-dvorak))
              (eq skk-kanagaki-state 'kana)
              skktut-nicola-tut-file)
         skk-kanagaki-rule-tree
       (skk-compile-rule-list skk-rom-kana-base-rule-list
                              skk-rom-kana-rule-list))))
  "skk.el $B$N%f!<%6!<JQ?t$N%j%9%H!#(B")

(defvar skktut-nicola-tut-file "NICOLA-SKK.tut"
  "NICOLA-DDSKK $B$N$?$a$N%A%e!<%H%j%"%k%U%!%$%kL>!#(B
$B%G%#%l%/%H%jL>$O4^$^$J$$!#(B")

(defvar skktut-japanese-tut nil
  "Non-nil $B$G$"$l$P!"%A%e!<%H%j%"%k$,F|K\8l$G$"$k$3$H$r<($9!#(B")
(defvar skktut-right-answer nil "$B@52r$NJ8;zNs!#(B")
(defvar skktut-question-count 1 "$B%A%e!<%H%j%"%k$N8=:_$NLdBjHV9f!#(B")
(defvar skktut-tutorial-end nil "$B%A%e!<%H%j%"%k$N=*N;$r<($9%U%i%0!#(B")
(defvar skktut-working-buffer " *skk-tutorial*")
(defvar skktut-question-buffer "*$BLd(B*")
(defvar skktut-answer-buffer "*$BEz(B*")
(defvar skktut-jisyo-buffer " *skk-tut-jisyo*")
(defvar skktut-original-window-configuration nil)
(defvar skktut-working-window-configuration nil)
(defvar skktut-original-local-map nil)
(defvar skktut-skk-mode-on nil
  "Non-nil $B$J$i!"(Btutorial $B$r5/F0;~$K(B SKK $B$,4{$K5/F0$5$l$F$$$?$3$H$r<($9!#(B")

(defvar skktut-latin-mode-map nil
  "SKK $B%A%e!<%H%j%"%k(B ASCII $B%b!<%I%-!<%^%C%W!#(B")

(unless skktut-latin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'skk-kakutei)
    (setq skktut-latin-mode-map map)))

(defvar skktut-j-mode-map nil
  "SKK $B%A%e!<%H%j%"%k$+$J(B/$B%+%J%b!<%I%-!<%^%C%W!#(B")

(unless skktut-j-mode-map
  (let ((map (make-sparse-keymap))
        (c 32))
    (while (< c 127)
      (define-key map (skk-char-to-unibyte-string c) 'skk-insert)
      (setq c (1+ c)))
    (setq skktut-j-mode-map map)))

(defvar skktut-jisx0208-latin-mode-map nil
  "SKK $B%A%e!<%H%j%"%kA43Q1Q?t;z%b!<%I%-!<%^%C%W!#(B")

(unless skktut-jisx0208-latin-mode-map
  (let ((map (make-sparse-keymap))
        (i 0))
    (while (< i 128)
      (if (aref skk-jisx0208-latin-vector i)
          (define-key map (skk-char-to-unibyte-string i) 'skk-jisx0208-latin-insert))
      (setq i (1+ i)))
    (define-key map "\C-j" 'skk-kakutei)
    (setq skktut-jisx0208-latin-mode-map map)))

(defvar skktut-abbrev-mode-map nil
  "SKK $B%A%e!<%H%j%"%k(B Abbrev $B%b!<%I%-!<%^%C%W!#(B")

(unless skktut-abbrev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "," 'skk-abbrev-comma)
    (define-key map "." 'skk-abbrev-period)
    (define-key map "\C-q" 'skk-toggle-characters)
    (define-key map "\C-j" 'skk-kakutei)
    (define-key map " " 'skk-start-henkan)
    (define-key map "\t" 'skk-try-completion)
    (setq skktut-abbrev-mode-map map)))

;; -- macros
(defmacro skktut-message (japanese english &rest arg)
  ;; skktut-japanese-tut $B$,(B non-nil $B$@$C$?$i(B JAPANESE $B$r(B nil $B$G$"$l$P(B ENGLISH
  ;; $B$r%(%3!<%(%j%"$KI=<($9$k!#(B
  ;; ARG $B$O(B message $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B
  (append `(message (if skktut-japanese-tut ,japanese ,english))
          arg))

(defmacro skktut-error (japanese english &rest arg)
  ;; skktut-japanese-tut $B$,(B non-nil $B$@$C$?$i(B JAPANESE $B$r(B nil $B$G$"$l$P(B ENGLISH
  ;; $B$r%(%3!<%(%j%"$KI=<($7!"%(%i!<$rH/@8$5$;$k!#(B
  ;; ARG $B$O(B error $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B
  (append `(error (if skktut-japanese-tut ,japanese ,english))
          arg))

(defmacro skktut-yes-or-no-p (japanese english)
  `(yes-or-no-p (if skktut-japanese-tut ,japanese ,english)))

;; advices.
(defadvice skk-create-file (around skktut-ad disable))

(defadvice skk-save-jisyo-original (around skktut-ad disable))

(defadvice skk-abbrev-mode (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (when (> 12 skktut-question-count)
    (skktut-error "$B$3$N%-!<$O$^$@;H$($^$;$s(B"
                  "Cannot use this key yet")))

(defadvice skk-insert (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (when (and (memq last-command-event skk-set-henkan-point-key)
             (> 12 skktut-question-count))
    (skktut-error "$B$+$J(B/$B%+%J%b!<%I$G$O!"1QBgJ8;z$O$^$@;H$($^$;$s(B"
                  "Cannot use upper case character in kana/katakana mode")))

(defadvice skk-kakutei (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (when (and (called-interactively-p 'interactive)
             (= skktut-question-count 1))
    (skktut-error "$B$3$N%-!<$O$^$@;H$($^$;$s(B"
                  "Cannot use this key yet")))

(defadvice skk-mode (before skktut-ad disable)
  "SKK $B%A%e!<%H%j%"%kMQ%"%I%P%$%9IU!#(B"
  (when (and (called-interactively-p 'interactive)
             (= skktut-question-count 1))
    (skktut-error "$B$3$N%-!<$O$^$@;H$($^$;$s(B"
                  "Cannot use this key yet")))

(defadvice skk-get-jisyo-buffer (around skktut-ad disable)
  (cond ((string= (skk-jisyo) skktut-tut-jisyo)
         (setq ad-return-value (get-buffer skktut-jisyo-buffer)))
        (t
         ad-do-it)))

;; hooks
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and (called-interactively-p 'interactive)
                       (member (buffer-name (current-buffer))
                               (list skktut-working-buffer
                                     skktut-question-buffer
                                     skktut-answer-buffer
                                     skktut-jisyo-buffer)))
              (skk-tutorial-quit 'now))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (buffer-live-p (get-buffer skktut-jisyo-buffer))
              (skk-tutorial-quit 'now))))

;; interactive commands. prefix should be `skk-tutorial'.
;;;###autoload
(defun skk-tutorial (&optional query-language)
  "Start SKK tutorial.
You can select English version by \\[universal-argument] \\[skk-tutorial]."
  (interactive "P")
  ;;
  (unless skk-mode-invoked
    ;; SKK $B$r5/F0$;$:$K$$$-$J$j(B
    ;; skk-tutorial $B$r<B9T$7$?$H$-$K(B skk-jisyo $B%P%C%U%!$,:n$i$l$J$$$N$G(B
    ;; skk-setup-jisyo-buffer $B$G%(%i!<$H$J$j!"(BEmacs $B$N=*N;$,$G$-$J$/(B
    ;; $B$J$k$N$G(B SKK $B%b!<%I$r0lEY5/$3$7$F$*$/!#(B
    ;; ($BDI5-(B: $B%b!<%I9T$N@_Dj$J$I$K$b(B SKK $B$r5/F0$7$F$*$/$3$H$OI,MW$J$N$G!"(B
    ;; $B$H$j$"$($:!"$J$k$Y$/Aa$$%?%$%_%s%0$KJQ99$7$^$9!#(B)
    (skk-mode 1)
    (skk-mode -1))
  ;;
  (when (and skk-use-kana-keyboard
             (memq skk-kanagaki-keyboard-type
                   '(oasys
                     nicola-jis nicola-us nicola-dvorak
                     omelet-jis omelet-us omelet-dvorak))
             (eq skk-kanagaki-state 'kana)
             skktut-nicola-tut-file)
    (skk-nicola-setup-tutorial)
    (setq skk-tut-file (expand-file-name skktut-nicola-tut-file
                                         (file-name-directory skk-tut-file))))
  ;;
  (let ((file skk-tut-file))
    (when (and (null skk-tut-current-lang)
               skk-tut-lang)
      (setq skk-tut-current-lang skk-tut-lang))
    (when query-language
      (setq skk-tut-current-lang (completing-read "Language: "
                                                  skk-tut-file-suffix-alist))
      (message "Language for the tutorial is %s until you exit Emacs"
               skk-tut-current-lang))
    (when skk-tut-current-lang
      (setq file (skk-tut-find-tut-file file)))
    (unless (file-exists-p (expand-file-name file))
      (error "No file found as %s" file))
    (let ((inhibit-quit t))
      (skktut-pre-setup-tutorial)
      (skktut-setup-jisyo-buffer)
      (skktut-setup-working-buffer file)
      (skktut-setup-question-buffer)
      (skktut-setup-answer-buffer)
      (skktut-enable-advice)
      (skktut-enable-tutmap)
      (add-hook 'minibuffer-setup-hook 'skktut-localize-and-init-variables)
      (skktut-make-windows))
    (skktut-setup-delete-backward-char)))

(defun skk-tut-nicola-p ()
  (equal skk-tut-file (expand-file-name skktut-nicola-tut-file
                                        (file-name-directory skk-tut-file))))

(defun skk-tut-find-tut-file (file)
  (cond
   ((skk-tut-nicola-p)
    skk-tut-file)
   (t
    (when (string-match "SKK\\.tut\\.\\w" file)
      (setq file (substring file 0 (- (match-end 0) 2))))
    (concat file (cdr (assoc skk-tut-current-lang
                             skk-tut-file-suffix-alist))))))

(defun skk-tutorial-again (&optional now)
  "$B%f!<%6$NF10U$,$"$l$P(B SKK $B%A%e!<%H%j%"%k$r:G=i$+$i$d$jD>$9!#(B
\\[universal-argument] \\[skk-tutorial-again] $B$9$k$HD>$A$K$d$jD>$9!#(B"
  (interactive "P")
  (when (or now
            (skktut-yes-or-no-p
             "$B:G=i$+$i(B Tutorial $B$r$d$jD>$7$^$9!#$h$m$7$$$G$9$M!)(B "
             "Quit tutorial and start from question 1 again? "))
    (skk-tutorial-quit 'now)
    (skk-tutorial)))

(defun skk-tutorial-quit (&optional now)
  "$B%f!<%6$NF10U$,$"$k>l9g(B SKK $B%A%e!<%H%j%"%k$r$d$a$k!#(B
\\[universal-argument] \\[skk-tutorial-quit] $B$9$k$HD>$A$K$d$a$k!#(B"
  (interactive "P")
  (when (or now
            (skktut-yes-or-no-p
             "$BK\Ev$K%A%e!<%H%j%"%k$r$d$a$^$9$+!)(B "
             "Really quit tutorial? "))
    (let ((inhibit-quit t))
      (delete-other-windows)
      ;; $B:FEY%A%e!<%H%j%"%k$r;H$($k$h$&$K!"FbItJQ?t$r=i4|2=$7$F$*$/!#(B
      (setq skktut-japanese-tut nil
            skktut-question-count 1
            skktut-right-answer nil
            skktut-tutorial-end nil)
      (remove-hook 'minibuffer-setup-hook
                   'skktut-localize-and-init-variables)
      (skktut-disable-tutmap)
      (skktut-disable-advice)
      (with-current-buffer skktut-jisyo-buffer
        (set-buffer-modified-p nil)
        (kill-buffer skktut-jisyo-buffer))
      (kill-buffer skktut-working-buffer)
      (kill-buffer skktut-answer-buffer)
      (kill-buffer skktut-question-buffer)
      (set-window-configuration skktut-original-window-configuration)
      ;; $B%A%e!<%H%j%"%k5/F0D>A0$K3+$$$F$$$?%P%C%U%!$G!"(Bskk-mode $B$r5/F0$7$F(B
      ;; $B$$$?$i!"$=$N>uBV$K$7$F!"%A%e!<%H%j%"%k$r=*N;$9$k!#(B
      (unless skktut-skk-mode-on
        (skk-mode -1)))))

;; the following commands are also interactive, but users may not call
;; them by name.  So prefix should be `skktut-'.
(defun skktut-next-question ()
  (interactive)
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (save-match-data
    (let (user-ans)
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (skip-chars-backward " \t")
        (setq user-ans (buffer-substring-no-properties
                        (point-min) (point))))
      (when (string-match "^>* *" user-ans)
        (setq user-ans (substring user-ans (match-end 0))))
      (if (not (string= skktut-right-answer user-ans))
          (progn
            (skktut-message "$BEz$,0c$$$^$9!#$b$&0lEY$d$C$F$_$F2<$5$$(B"
                            "Wrong.  Try again")
            (ding))
        (setq skktut-question-count (1+ skktut-question-count))
        ;; buffer independent.
        (skktut-get-question-page skktut-question-count)
        (if (>= skktut-question-count
                (1+ skktut-question-numbers))
            (skk-tutorial-quit 'now)
          (skktut-next-answer-buffer))))))

(defun skktut-skip-question (arg)
  (interactive "p")
  (set-window-configuration skktut-working-window-configuration)
  ;; called in skktut-answer-buffer.
  (skktut-erase-buffer)
  (setq skktut-question-count (+ skktut-question-count arg))
  (cond ((> 1 skktut-question-count)
         (setq skktut-question-count 1))
        ;; overrun
        ((> skktut-question-count skktut-question-numbers)
         (setq skktut-question-count skktut-question-numbers))
        ((and (>= skktut-question-count 3)
              (not skk-j-mode))
         (skk-mode 1)))
  ;; buffer independent.
  (skktut-get-question-page skktut-question-count)
  (if skktut-tutorial-end
      (skk-tutorial-quit 'now)
    (skktut-next-answer-buffer)))

;; internal functions.  prefix should be `skktut-'.
(defun skktut-make-windows ()
  ;; Make window fill its frame.
  (delete-other-windows)
  (split-window-vertically)
  (let ((height (window-height (selected-window))))
    (unless (< 22 height)
      ;; Keep at least 23 lines for the upper window.
      (enlarge-window (- 23 height))))
  ;;   (other-window 1)
  ;; make it selected window and current buffer.
  ;;   (switch-to-buffer skktut-answer-buffer)
  (pop-to-buffer skktut-answer-buffer)
  ;; not make it current buffer but visible.
  (display-buffer skktut-question-buffer)
  (setq skktut-working-window-configuration
        (current-window-configuration)))

(defun skktut-enable-advice ()
  (dolist (e skktut-adviced-alist)
    (ad-enable-advice (car e) (cdr e) 'skktut-ad)
    (ad-activate (car e))))

(defun skktut-disable-advice ()
  (dolist (e skktut-adviced-alist)
    (ad-disable-advice (car e) (cdr e) 'skktut-ad)
    (ad-activate (car e))))

(defun skktut-enable-tutmap ()
  (let ((inhibit-quit t))
    (skk-update-minor-mode-map-alist 'skk-latin-mode skktut-latin-mode-map)
    (skk-update-minor-mode-map-alist 'skk-abbrev-mode skktut-abbrev-mode-map)
    (skk-update-minor-mode-map-alist 'skk-j-mode skktut-j-mode-map)
    (skk-update-minor-mode-map-alist 'skk-jisx0208-mode skktut-jisx0208-latin-mode-map)
    (define-key skktut-j-mode-map "\C-p" 'skk-previous-candidate)
    (unless (skk-tut-nicola-p)
      (define-key skktut-j-mode-map "x" 'skk-previous-candidate))
    (define-key skktut-j-mode-map "\C-j" 'skk-insert)
    (define-key skktut-j-mode-map "\t" 'skk-insert)
    ;; for minor-mode-map-alist localized by Viper.
    (when (and (featurep 'viper)
               (local-variable-p 'minor-mode-map-alist))
      (setq-default minor-mode-map-alist minor-mode-map-alist))))

(defun skktut-disable-tutmap ()
  (let ((inhibit-quit t)
        (minor-mode-list '(skk-abbrev-mode
                           skk-latin-mode
                           skk-j-mode
                           skk-jisx0208-latin-mode))
        minor-mode e)
    (while minor-mode-list
      (setq minor-mode (car minor-mode-list)
            minor-mode-list (cdr minor-mode-list))
      ;; fail safe.
      (while (setq e (assq minor-mode minor-mode-map-alist))
        (setq minor-mode-map-alist (delq e minor-mode-map-alist))))
    (skk-update-minor-mode-map-alist 'skk-latin-mode skk-latin-mode-map)
    (skk-update-minor-mode-map-alist 'skk-abbrev-mode skk-abbrev-mode-map)
    (skk-update-minor-mode-map-alist 'skk-j-mode skk-j-mode-map)
    (skk-update-minor-mode-map-alist 'skk-jisx0208-mode skk-jisx0208-latin-mode-map))
  ;; for minor-mode-map-alist localized by Viper.
  (when (default-value skk-use-viper)
    (skk-viper-normalize-map)))

(defun skktut-pre-setup-tutorial ()
  (setq skktut-original-window-configuration (current-window-configuration)
        skktut-skk-mode-on skk-mode
        skktut-question-count 1))

(defun skktut-update-jisyo (word &optional purge)
  (let ((skk-share-private-jisyo nil)
        (skk-jisyo-update-vector nil)
        (skk-update-jisyo-count 0))
    (skk-update-jisyo-original word purge)))

(defun skktut-setup-jisyo-buffer ()
  ;; setup skktut-tut-jisyo buffer.
  (with-current-buffer (get-buffer-create skktut-jisyo-buffer)
    (buffer-disable-undo (current-buffer))
    (skktut-localize-and-init-variables)
    (setq case-fold-search nil)
    (insert "\
;; okuri-ari entries.
$B$[$C(Bs /$BM_(B/
$B$D$+(Bt /$B;H(B/
$B$?$C(Bs /$BC#(B/
$B$7(Bt /$BCN(B/
$B$&$4(Bk /$BF0(B/
;; okuri-nasi entries.
Cyrillic /$B'!(B/$B'"(B/$B'#(B/$B'$(B/$B'%(B/$B'&(B/$B''(B/$B'((B/$B')(B/$B'*(B/$B'+(B/$B',(B/$B'-(B/$B'.(B/$B'/(B/$B'0(B/$B'1(B/$B'2(B/$B'3(B/$B'4(B/$B'5(B/$B'6(B/\
$B'7(B/$B'8(B/$B'9(B/$B':(B/$B';(B/$B'<(B/$B'=(B/$B'>(B/$B'?(B/$B'@(B/$B'A(B/
Greek /$B&!(B/$B&"(B/$B&#(B/$B&$(B/$B&%(B/$B&&(B/$B&'(B/$B&((B/$B&)(B/$B&*(B/$B&+(B/$B&,(B/$B&-(B/$B&.(B/$B&/(B/$B&0(B/$B&1(B/$B&2(B/$B&3(B/$B&4(B/$B&5(B/$B&6(B/$B&7(B/\
$B&8(B/
cyrillic /$B'Q(B/$B'R(B/$B'S(B/$B'T(B/$B'U(B/$B'V(B/$B'W(B/$B'X(B/$B'Y(B/$B'Z(B/$B'[(B/$B'\(B/$B'](B/$B'^(B/$B'_(B/$B'`(B/$B'a(B/$B'b(B/$B'c(B/$B'd(B/$B'e(B/$B'f(B/\
$B'g(B/$B'h(B/$B'i(B/$B'j(B/$B'k(B/$B'l(B/$B'm(B/$B'n(B/$B'o(B/$B'p(B/$B'q(B/
greek /$B&A(B/$B&B(B/$B&C(B/$B&D(B/$B&E(B/$B&F(B/$B&G(B/$B&H(B/$B&I(B/$B&J(B/$B&K(B/$B&L(B/$B&M(B/$B&N(B/$B&O(B/$B&P(B/$B&Q(B/$B&R(B/$B&S(B/$B&T(B/$B&U(B/$B&V(B/$B&W(B/\
$B&X(B/
tutorial /$B%A%e!<%H%j%"%k(B/
$B$$$A(B /$B0l(B/
$B$$$A$*$/(B /$B0l2/(B/
$B$*$*$5$+(B /$BBg:e(B/
$B$*$/(B /$B2/(B/
$B$+$J(B /$B2>L>(B/
$B$+$s$8(B /$B4A;z(B/$B44;v(B/$B4F;v(B/
$B$,$/(B /$B3X(B/
$B$,$/$7$e$&(B /$B3X=,(B/
$B$-(B /$B4p(B/$B5-(B/$B5$(B/$BLZ(B/$B5"(B/
$B$-$4$&(B /$B5-9f(B/$B!&(B/$B!+(B/$B!,(B/$B!-(B/$B!.(B/$B!/(B/$B!1(B/$B!3(B/$B!4(B/$B!5(B/$B!6(B/$B!7(B/$B!8(B/$B!9(B/$B!:(B/$B!;(B/$B!=(B/$B!>(B/$B!B(B/$B!D(B/$B!E(B/\
$B!H(B/$B!I(B/$B!L(B/$B!M(B/$B!R(B/$B!S(B/$B!T(B/$B!U(B/$B!X(B/$B!Y(B/$B!Z(B/$B![(B/$B!^(B/$B!_(B/$B!`(B/$B!b(B/$B!e(B/$B!f(B/$B!g(B/$B!h(B/$B!i(B/$B!j(B/$B!k(B/$B!l(B/$B!m(B/$B!n(B/\
$B!o(B/$B!q(B/$B!r(B/$B!x(B/$B!y(B/$B!z(B/$B!{(B/$B!|(B/$B!}(B/$B!~(B/$B"!(B/$B""(B/$B"#(B/$B"$(B/$B"%(B/$B"&(B/$B"'(B/$B"((B/$B")(B/$B"*(B/$B"+(B/$B",(B/$B"-(B/$B".(B/
$B$-$g$&$H(B /$B5~ET(B/
$B$3$&(B /$B8}(B/
$B$3$&$Y(B /$B?@8M(B/
$B$4(B /$B8^(B/$B8_(B/$B8`(B/$B8a(B/$B8b(B/$B8c(B/$B8d(B/$B8e(B/$B8f(B/$B8g(B/$B8h(B/$B8i(B/$B8j(B/$B8k(B/$B8l(B/$B8m(B/$B8n(B/$B8o(B/
$B$5$$(B /$B:Y(B/$B:G(B/$B:F(B/
$B$5$$$7$g(B /$B:G=i(B/
$B$5$$$H$&(B /$B:XF#(B/
$B$5$H$&(B /$B:4F#(B/
$B$7$e$&(B /$B=,(B/$B=*(B/
$B$7$e$&$j$g$&(B /$B=*N;(B/
$B$7$g(B /$B=q(B/
$B$8(B /$B<-(B/$B;z(B/
$B$8$7$g(B /$B<-=q(B/$BCO=j(B/
$B$8$s(B /$B?M(B/
$B$8$s$3$&(B /$B?M8}(B/
$B$;$s(B /$BA*(B/$B@v(B/
$B$;$s$?$/(B /$BA*Br(B/$B@vBu(B/
$B$=$&(B /$BAv(B/
$B$@$$(B /$BBg(B/$BBh(B/$BBe(B/
$B$?$/(B /$BBr(B/$BBu(B/
$B$F$-(B /$BE*(B/$BE((B/$BE)(B/$BE,(B/$BE&(B/
$B$H$&(B /$BEl(B/$BEP(B/
$B$H$&$[$/(B /$BElKL(B/
$B$H$&$m$/(B /$BEPO?(B/
$B$I$&(B /$BF0(B/
$B$K$e$&$j$g$/(B /$BF~NO(B/
$B$R$3$&$-(B /$BHt9T5!(B/
$B$X$s$+$s(B /$BJQ49(B/
$B$[$/(B /$BKL(B/
$B$_$g$&(B /$BL>(B/
$B$_$g$&$8(B /$BL>;z(B/
$B$h$&$$(B /$BMF0W(B/$BMQ0U(B/
$B$j$g$&(B /$BN;(B/
$B$m$/(B /$BO?(B/
")
    (skk-setup-jisyo-buffer)))

(defun skktut-setup-working-buffer (file)
  (save-match-data
    (let (sexp)
      (set-buffer (get-buffer-create skktut-working-buffer))
      (buffer-disable-undo (current-buffer))
      (skktut-localize-and-init-variables)
      (skktut-erase-buffer) ; fail safe.
      (insert-file-contents file)
      ;; Count how many questions there are.
      (setq skktut-question-numbers -1)
      (goto-char (point-min))
      (while (re-search-forward "^----$" nil t)
        (setq skktut-question-numbers (1+ skktut-question-numbers)))
      ;;
      (goto-char (point-min))
      (setq skktut-japanese-tut (looking-at ";; SKK Japanese"))
      (while (re-search-forward "^>> \\((.+)\\)$" nil t nil)
        (setq sexp (buffer-substring-no-properties (match-beginning 1)
                                                   (match-end 1)))
        (delete-region (match-beginning 1) (match-end 1))
        ;; insert evaluated string instead of lisp program.
        (insert (eval (car (read-from-string sexp)))))
      (goto-char (point-min))
      (when skk-tut-use-face
        (skktut-colored)))))

(defun skktut-setup-question-buffer ()
  (with-current-buffer (get-buffer-create skktut-question-buffer)
    (buffer-disable-undo (current-buffer))
    (skktut-erase-buffer) ; fail safe.
    (setq buffer-read-only t)
    (skktut-get-question-page skktut-question-count)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question)))

(defun skktut-setup-answer-buffer ()
  (with-current-buffer (get-buffer-create skktut-answer-buffer)
    ;; users may use undo.
    ;; (buffer-disable-undo (current-buffer))
    ;; skktut-answer-buffer $B$N(B skk.el $B$NJQ?t$r%P%C%U%!%m!<%+%k2=$7!"=i4|2=$9(B
    ;; $B$k!#(B
    (skktut-localize-and-init-variables)
    (local-set-key "\C-xq" 'skk-tutorial-quit)
    (local-set-key "\C-xt" 'skk-tutorial-again)
    (local-set-key "\C-xn" 'skktut-next-question)
    (local-set-key "\C-xs" 'skktut-skip-question)
    (auto-fill-mode -1)
    (skktut-next-answer-buffer)))

(defun skktut-localize-and-init-variables ()
  ;; $B%f!<%6!<$,(B skk.el $B$NJQ?t$r%+%9%?%^%$%:$7$F$$$k2DG=@-$,$"$k$N$G!"%+%l%s%H(B
  ;; $B%P%C%U%!$N(B skk.el $B$NJQ?t$r%P%C%U%!%m!<%+%k2=$7!"=i4|2=$9$k!#(B
  (let ((alist skktut-init-variables-alist)
        v)
    (dolist (cell alist)
      (setq v (car cell))
      (make-local-variable v)
      (set v (eval (cdr cell))))))

(defun skktut-erase-buffer ()
  (let ((inhibit-read-only t)
        buffer-read-only)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)))

(defun skktut-colored ()
  ;; face $B$r(B Text Property $B$K$7$F$*$/$H%F%-%9%H$r%3%T!<$7$?$H$-$K0l=o$K%3%T!<$G(B
  ;; $B$-$k$N$G9%ET9g!#(B
  (while (re-search-forward "$B"'(B\\([^$B![(B $B$!(B-$B$s%!(B-$B%s(B]+\\)" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1) 'face
                       'highlight))
  (goto-char (point-min))
  (while (re-search-forward "^==.+==$" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-section-face))
  (goto-char (point-min))
  (while (re-search-forward "^!!.+" nil t nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'face skk-tut-do-it-face))
  (goto-char (point-min))
  (while (re-search-forward "^>> \\(.+\\)$" nil t nil)
    (put-text-property (match-beginning 1) (match-end 1)
                       'face skk-tut-question-face))
  (unless skktut-japanese-tut
    (goto-char (point-min))
    (while (re-search-forward "Hint: .*$" nil t nil)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face skk-tut-hint-face))))

(defun skktut-next-answer-buffer ()
  (save-match-data
    (with-current-buffer skktut-question-buffer
      ;; first get right answer in `skktut-question-buffer'.
      (goto-char (point-max))
      (search-backward "\n>>")
      (forward-char 1)
      (setq skktut-right-answer
            (buffer-substring-no-properties (+ 3 (point))
                                            (line-end-position)))
      (goto-char (point-min)))
    ;; not to save point.
    (let ((cbuf (current-buffer))
          p)
      (unwind-protect
          (let ((plist (cons 'rear-nonsticky
                             '(t intangible t read-only t))))
            ;; secondary make a new answer buffer.
            (set-buffer skktut-answer-buffer)
            (skktut-erase-buffer)
            (insert ">> \n\n")
            (add-text-properties (point-min) (- (point) 2) plist)
            (setq p (point))
            (insert
             (let ((next " `\\[skktut-next-question]'")
                   (quit " `\\[skk-tutorial-quit]'")
                   (skip " `\\[skktut-skip-question]'")
                   (sow  " `\\[scroll-other-window]'"))
               (substitute-command-keys
                (if skktut-japanese-tut
                    (concat
                     "* $BEz$,$G$-$?$i(B" next "\n"
                     "* $BESCf$G$d$a$k$K$O(B" quit "\n"
                     (if (/= skktut-question-count skktut-question-numbers)
                         (concat "* $B%9%-%C%W$9$k$K$O(B" skip "\n"))
                     "* $B!ZLd![$r%9%/%m!<%k$9$k$K$O(B" sow)
                  (concat
                   "* For next question" next "\n"
                   "* to quit " quit "\n"
                   (if (/= skktut-question-count skktut-question-numbers)
                       (concat "* to skip this question" skip "\n"))
                   "* to scroll question window" sow)))))
            (when skk-tut-use-face
              (put-text-property p (point) 'face skk-tut-key-bind-face))
            (add-text-properties p (point) plist)
            (goto-char (+ (point-min) 3)))
        (set-buffer cbuf)))))

(defun skktut-get-question-page (page)
  (save-excursion
    (save-match-data
      (if (>= skktut-question-count
              (1+ skktut-question-numbers))
          ;; No more need to search for S expressions in the working buffer.
          ;; It's time to finish.
          (skktut-end-tutorial)
        ;; Tutorial continues.
        (set-buffer skktut-working-buffer)
        (let (pos str)
          (goto-char (point-min))
          (search-forward "--\n" nil t page)
          (when (looking-at ";") ; lisp program exists.
            (forward-char 3)
            (setq pos (point))
            (end-of-line)
            (skk-save-point
             (eval-region pos (point) nil))
            (forward-char 1))
          (unless skktut-tutorial-end
            (setq pos (point))
            (search-forward "\n>>")
            (end-of-line)
            (setq str (buffer-substring pos (point)))
            (set-buffer skktut-question-buffer)
            (skktut-erase-buffer)
            (let (buffer-read-only)
              (insert str)
              (setq mode-line-buffer-identification
                    (concat "$B#S#K#K%A%e!<%H%j%"%k(B: $B!NLd(B "
                            (number-to-string page)
                            "$B!O(B $B!J;D$j(B "
                            (number-to-string
                             (- skktut-question-numbers page))
                            "$BLd!K(B"))
              (set-buffer-modified-p nil)
              (force-mode-line-update 'all))))))))

;; The following two functions are tricky, since they are executed by
;; `eval-region' in skktut-working-buffer.
(defun skktut-today ()
  (save-restriction
    (save-match-data
      (let (p)
        (widen)
        (search-forward "\n>> ")
        (when (re-search-forward "$B!V(B.*$B!W(B" (line-end-position) t)
          (delete-region (match-beginning 0) (match-end 0)))
        (setq p (point))
        (insert (concat "$B!V$-$g$&$O!"(B" (skk-current-date) "$B$G$9!#!W(B"))
        (narrow-to-region (point-min) (point))
        (when skk-tut-use-face
          (put-text-property p (point) 'face skk-tut-question-face))))))

(defun skktut-end-tutorial ()
  ;;  (switch-to-buffer skktut-question-buffer)
  (pop-to-buffer skktut-question-buffer)
  (delete-other-windows)
  (skktut-erase-buffer)
  (let (buffer-read-only)
    (goto-char (point-min))
    (insert
     (format
      (if skktut-japanese-tut
          "\
SKK $B%A%e!<%H%j%"%k$O$3$l$G=*$j$G$9!#(B

%s $B$K4X$9$k<ALd!"%3%a%s%H!"(Bbug report $BEy$O(B

https://github.com/skk-dev $BKx$*Aw$j$/$@$5$$!#(B

!! $B:G8e$K(B <return> $B%-!<$r2!$7$F$/$@$5$$!#(B"

        "\
Now we end the SKK tutorial.

Please post comments, questions and bug reports on %s to:

https://github.com/skk-dev

!! Hit <return> key when you are ready.")
      (skk-version t)))

    (when skk-tut-use-face
      (save-match-data
        (goto-char (point-min))
        (re-search-forward "^!!.+" nil t nil)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face skk-tut-do-it-face)))

    (while (not skktut-tutorial-end)
      (ignore-errors
        (let* ((event (read-event))
               (char (event-to-character event)))
          (skktut-message "<return> $B%-!<$r2!$7$F$/$@$5$$(B"
                          "Hit <return> key")
          (if (and char (eq ?\C-m char))
              (setq skktut-tutorial-end t)))))
    (message nil)))

(defun skktut-setup-delete-backward-char ()
  (let ((commands '(backward-delete-char-untabify
                    backward-delete-char
                    backward-or-forward-delete-char
                    delete-backward-char
                    picture-backward-clear-column
                    ;; following two are SKK adviced.
                    ;;viper-del-backward-char-in-insert
                    ;;vip-del-backward-char-in-insert
                    ))
        (map (if (and (boundp 'overriding-local-map)
                      (keymapp 'overriding-local-map))
                 overriding-local-map
               (current-global-map)))
        keys)
    (dolist (command commands)
      (setq keys (where-is-internal command map))
      (dolist (key keys)
        (define-key skktut-abbrev-mode-map
          key 'skk-delete-backward-char)
        (define-key skktut-j-mode-map
          key 'skk-delete-backward-char)))))

(defun skktut-user-name ()
  (let ((name (user-full-name)))
    (if (not (or (string= name "")
                 ;; non-ASCII characters
                 (save-match-data
                   (string-match "[^\000-\177]" name))))
        name
      (setq name (user-login-name))
      (if (not (or (string= name "")
                   ;; non-ASCII characters
                   (save-match-data
                     (string-match "[^\000-\177]" name))))
          name
        "nanasi"))))

(provide 'skk-tut)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-tut.el ends here
