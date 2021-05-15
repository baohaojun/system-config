;;; skk.el --- Daredevil SKK (Simple Kana to Kanji conversion program) -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988-1997 Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999-2020 SKK Development Team

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

;; SKK-MODE is a mode for inputting Japanese to a current buffer which is
;; composed of four minor modes described below.

;;      +---------------+-------- skk-mode -----+--------------------+
;;      |               |                       |                    |
;;      |               |                       |                    |
;;  skk-j-mode   skk-latin-mode   skk-jisx0208-latin-mode   skk-abbrev-mode
;;                  ASCII               JISX0208 LATIN         ABBREVIATION
;; (C-j wakes up skk-j-mode)      (ZEN'KAKU EIMOJI)
;;
;; skk-j-mode-map               skk-jisx0208-latin-mode-map
;;              skk-latin-mode-map                        skk-abbrev-mode-map

;; skk-katakana: nil
;;   HIRAKANA

;; skk-katakana: t
;;   KATAKANA

;;; Code:

(eval-when-compile ; shut up compiler warning.
  (defvar enable-character-translation)
  (defvar epoch::version)
  (defvar jka-compr-compression-info-list)
  (defvar message-log-max)
  (defvar migemo-isearch-enable-p)
  (defvar minibuffer-local-ns-map)
  (defvar self-insert-after-hook)
  (defvar skk-rdbms-private-jisyo-table)
  (defvar this-command-char))

;; Daredevil SKK 13.1 $B$O(B APEL 10.7 $B$rMW5a$9$k!#(B
;; APEL 10.7 $B$O<g$K(B install.el $B$N5!G=$N$?$a$KMW5a$5$l$F$$$k!#(B
;; APEL 10.6 $B$O%;%-%e%j%F%#>e$N=EBg$J7g4Y$KBP1~$7$F$$$k$?$a!"I,?\$G$"$k!#(B
;; make-temp-file() $B$N7g4Y$K$D$$$F$O4X?t(B skk-save-jisyo-original() $B$N%3%a(B
;; $B%s%H$r;2>H!#(B

(eval-when-compile
  (require 'cl-lib))

;; Emacs standard library.
(require 'advice)
(require 'easymenu)

(eval-and-compile
  ;; SKK common.


  ;; skk $B$N5/F0;~!"(B*-autoloads $B$O2<5-$N#2<oN`$N$&$A$I$A$i$+$K8B$C$FB8:_$9$k!#(B
  ;; MELPA$B7PM3$N>l9g!"(B autoload $B$O(B package.el $B$K$h$C$F4IM}$5$l$k$?$a!"(B
  ;; skk-autoloads$B$,%m!<%I$G$-$J$$>l9g!"C1$KL5;k$9$k!#(B
  ;;                | make $B$G%$%s%9%H!<%k(B     | MELPA $B7PM3$G%$%s%9%H!<%k(B
  ;;   $B%U%!%$%kL>(B     | skk-autoloads.el     | ddskk-autoloads.el
  ;;   provide $B@k8@(B  | $B$"$j(B (SKK-MK $B$,@8@.(B)   | $B$J$7(B
  (require 'skk-autoloads nil 'noerror)

  (provide 'skk)                        ; workaround for recursive require
  (require 'skk-vars)
  (require 'skk-macs)
  (require 'skk-emacs)

  ;; Shut up, compiler.
  (autoload 'skk-kanagaki-initialize "skk-kanagaki") ; nicola/
  (autoload 'skk-rdbms-count-jisyo-candidates "skk-rdbms"))

;;;###autoload
(defun skk-mode (&optional arg)
  "$BF|K\8lF~NO%b!<%I!#(B
$B%^%$%J!<%b!<%I$N0l<o$G!"%*%j%8%J%k$N%b!<%I$K$O1F6A$rM?$($J$$!#(B
$BIi$N0z?t$rM?$($k$H(B SKK $B%b!<%I$+$iH4$1$k!#(B

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is
\"$B$+$J(B\".  Lowercase romaji inputs are automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana \(mode line
indicator \"$B%+%J(B\"\) input submodes.

`l' is used to enter ASCII submode \(mode line indicator \"SKK\"\).
Uppercase `L' enters JISX0208 latin \(wide ASCII\) submode \(mode line
indicator \"$BA41Q(B\"\).  `\C-j' returns to hiragana submode from either
ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words \(eg, nouns\) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading \"$B"&(B\" indicates that
kanji conversion is in progress.  After entering the reading, press
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => \"$B"&$Y$s$j(B\", and pressing space produces \"$B"'JXMx(B\" \(the
solid triangle indicates that conversion is in progress\).  Backspace
steps through the candidate list in reverse.

A candidate can be accepted by pressing `\C-j', or by entering a
self-inserting character.  \(Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.\)

Inflected words \(verbs and adjectives\), like non-inflected words, begin
input with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> \"$B"'6/$$(B\".  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed \(pressing space is not necessary\).  Space and
backspace are used to step forward and backward through the list of
candidates.

For more information, see the `skk' topic in Info.  \(Japanese only.\)

A tutorial is available in Japanese or English via \\[skk-tutorial].
Use a prefix argument to choose the language.  The default is system-
dependent."
  (interactive "P")
  (setq skk-mode (cond ((null arg)
                        (not skk-mode))
                       ;; - $B$O(B -1 $B$KJQ49$5$l$k!#(B
                       ((> (prefix-numeric-value arg) 0)
                        t)
                       (t
                        nil)))
  (if (not skk-mode)
      ;; exit skk-mode
      (skk-mode-exit)
    ;; enter into skk-mode.
    (unless skk-mode-invoked
      ;; enter `skk-mode' for the first time in this session.
      (skk-mode-invoke))
    ;; $B0J2<$O(B skk-mode $B$KF~$k$?$S$KKh2s%3!<%k$5$l$k%3!<%I!#(B
    (skk-create-file (skk-jisyo)
                     "$B6u$N(B SKK $B8D?M<-=q$r:n$j$^$7$?(B"
                     "I have created an empty SKK Jisyo file for you")
    (skk-require-module)
    ;; To terminate kana input.
    (skk-add-skk-pre-command)
    (add-hook 'post-command-hook 'skk-after-point-move nil 'local)
    (skk-search-ja-dic-maybe t)
    (skk-j-mode-on)
    (run-hooks 'skk-mode-hook)))

;;;###autoload
(defun skk-auto-fill-mode (&optional arg)
  "$BF|K\8lF~NO%b!<%I!#<+F0@^$jJV$75!G=IU$-!#(B
$B%^%$%J!<%b!<%I$N0l<o$G!"%*%j%8%J%k$N%b!<%I$K$O1F6A$rM?$($J$$!#(B
$B@5$N0z?t$rM?$($k$H!"6/@)E*$K(B `auto-fill-mode' $B5Z$S(B SKK $B%b!<%I$KF~$k!#(B
$BIi$N0z?t$rM?$($k$H(B `auto-fill-mode' $B5Z$S(B SKK $B%b!<%I$+$iH4$1$k!#(B"
  (interactive "P")
  (let ((auto-fill
         (cond ((null arg)
                (not auto-fill-function))
               ((> (prefix-numeric-value arg) 0)
                t))))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (run-hooks 'skk-auto-fill-mode-hook)))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "$B8D?M<-=q$rJ]B8$;$:$K(B Emacs $B$r=*N;$9$k!#(B"
  (interactive "P")
  ;; format $B$r0z?t$K;}$?$;$?>l9g$O!"(Bskk-yes-or-no-p $B$r;H$&$H$+$($C$F>iD9$K$J(B
  ;; $B$k!#(B
  (when (yes-or-no-p
         (if skk-japanese-message-and-error
             "$B8D?M<-=q$rJ]B8$;$:$K(B Emacs $B$r=*N;$7$^$9!#NI$$$G$9$+!)(B "
           "Do you really wish to kill Emacs without saving Jisyo? "))
    (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
      (remove-hook 'kill-emacs-hook 'skk-save-jisyo)
      (when buff
        (set-buffer buff)
        (set-buffer-modified-p nil)
        (kill-buffer buff))
      (save-buffers-kill-emacs query))))

(defun skk-restart ()
  "`skk-init-file' $B$N:F%m!<%I5Z$S3F<o:F@_Dj$N8e(B SKK $B%b!<%I$r5/F0$9$k!#(B
$B3F<o%f!<%6JQ?t$r$$$C$?$s=i4|2=$9$k$N$G!"(B~/.emacs.d/init.el $B$G(B SKK $B$N@_Dj$r$7$F$$$k>l9g$O(B
$B;H$&$Y$-$G$J$$!#(B
`skk-kakutei-key' $B$NJQ99$K$OBP1~$G$-$F$$$J$$!#(B"
  (interactive)
  (skk-save-jisyo)
  (setq skk-jisyo-update-vector nil)
  (when (featurep 'skk-server)
    (skk-disconnect-server))
  (setq skk-server-disable-completion nil)
  (kill-local-variable 'skk-rule-tree)
  (setq skk-rule-tree nil)
  ;; $B$$$C$=(B unload-feature $B$H$+$7$?$[$&$,$$$$$N$+$b$7$l$J$$!#(B
  ;; skk-kakutei-key $B$K4X$7$F$O(B minibuffer-local-map $B$J$I$N=hM}$b!#(B
  (mapatoms (lambda (sym)
              ;; skk-user-directory, skk-init-file $B0J30$N(B defcustom $B$G@k8@(B
              ;; $B$5$l$?JQ?t$N$&$A!"(Bsaved-value $B$r;}$?$J$$$b$N$r:F=i4|2=!#(B
              ;; $BB>$K$b=|30$9$Y$-JQ?t$,$J$$$+MW8!F$!#(B
              (when (and (string-match "^skk-" (symbol-name sym))
                         (not (memq sym '(skk-user-directory skk-init-file)))
                         (not (plist-member (symbol-plist sym) 'saved-value))
                         (plist-member (symbol-plist sym) 'standard-value))
                (set-default sym
                             (eval (car (get sym 'standard-value)))))))
  (dolist (feature '(skk-act skk-azik))
    (when (featurep feature)
      (unload-feature feature)))
  (let (skk-mode-invoked)
    (skk-mode 1)))

(defun skk-require-module ()
  ;; skk-mode $B$KF~$k$?$S$K<B9T$5$l$k(B
  (when skk-use-viper
    (require 'skk-viper))
  (when (or skk-servers-list
            skk-server-host
            (getenv "SKKSERVER"))
    (require 'skk-server))
  (when (featurep 'skk-server)
    (skk-adjust-search-prog-list-for-server-search))
  (when skk-auto-okuri-process
    (require 'skk-auto)
    (skk-adjust-search-prog-list-for-auto-okuri))
  (when skk-use-look
    (require 'skk-look))
  (when (featurep 'skk-jisx0201)
    (setq skk-use-jisx0201-input-method t))
  (when (or skk-dcomp-activate
            skk-dcomp-multiple-activate)
    (require 'skk-dcomp))
  (when skk-sticky-key
    (require 'skk-sticky))
  (when skk-show-mode-show
    (require 'skk-dcomp)
    (require 'skk-show-mode))
  (when skk-use-search-web
    (require 'skk-search-web)))

(defun skk-mode-exit ()
  (let ((skk-mode t)
        skk-show-mode-show)
    (skk-kakutei))
  (skk-mode-off)
  (skk-remove-skk-pre-command)
  (remove-hook 'post-command-hook
               'skk-after-point-move
               'local)
  (skk-update-modeline)

  (dolist (b `("*$B8uJd(B*" "*SKK annotation*" "*$BC14A;z(B*"
               " *$B?tCMJQ49%?%$%W(B*" ,skk-list-chars-buffer-name))
    (when (get-buffer b)
      (kill-buffer b))))

(defun skk-mode-invoke ()
  ;; $B0lHV:G=i$N(B skk $B5/F0;~$N$_<B9T$5$l$k(B
  (when skk-user-directory
    (make-directory skk-user-directory t))
  (skk-compile-init-file-maybe)
  (skk-cus-setup)
  (load skk-init-file t)
  (skk-adjust-user-option)
  (skk-setup-modeline)
  (when (default-value 'skk-share-private-jisyo)
    (skk-setup-shared-private-jisyo))
  (when skk-keep-record
    (skk-create-file skk-record-file
                     "SKK $B$NE}7W>pJsMQ%U%!%$%k$r:n$j$^$7$?(B"
                     "I have created an SKK record file for you"))
  (skk-setup-auto-paren) ; necessary to call before compiling skk-rule-tree.
  ;; SKK $B3HD%F~NO5!G=$N@_Dj(B
  (cond (skk-use-act
         ;; $B3HD%%m!<%^;zF~NO(B ACT
         (require 'skk-act))
        (skk-use-azik
         ;; $B3HD%%m!<%^;zF~NO(B AZIK
         (require 'skk-azik))
        ((featurep 'skk-tutcdef)
         ;; TUT-code
         (require 'skk-tutcode))
        (skk-use-kana-keyboard
         ;; $B2>L>F~NO(B ($BF|K\8l5l(B JIS $B$^$?$O?F;X%7%U%H(B)
         (skk-kanagaki-initialize)))
  (skk-emacs-prepare-menu)
  (skk-setup-charset-list)
  (skk-setup-delete-selection-mode)
  (when skk-annotation-lookup-DictionaryServices
    (skk-annotation-start-python))
  (setq skk-mode-invoked t))

;;; setup
(defun skk-setup-shared-private-jisyo ()
  ;; vector $B$ND9$5$O!"(B`skk-save-jisyo' $B$,<:GT$7$J$$A0Ds$J$i$P(B
  ;; skk-jisyo-save-count $B$HF1$8$G$h$$!#%(%i!<$=$NB>$G<:GT$9$k2DG=@-$r(B
  ;; $B9MN8$9$k$H!"$=$l$h$j>/$7D9$$J}$,LdBj$,5/$3$j$K$/$$!#(B
  ;; ($B0J2<$N6qBNE*$J?t;z$K$O:,5r$O$J$$(B)
  (setq skk-jisyo-update-vector
        (make-vector (max (* 2 skk-jisyo-save-count)
                          (+ 20 skk-jisyo-save-count))
                     nil))
  (setq skk-emacs-id
        (concat (system-name) ":"
                (number-to-string (emacs-pid)) ":"
                (mapconcat 'int-to-string (current-time) "") ":"))
  (skk-create-file skk-emacs-id-file nil nil 384) ; 0600
  (with-temp-buffer
    (insert-file-contents skk-emacs-id-file)
    (insert skk-emacs-id "\n")
    (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)))

(defun skk-setup-charset-list ()
  (setq skk-charset-list
        (delq nil (mapcar (lambda (c)
                            (when (charsetp c)
                              c))
                          '(ascii
                            japanese-jisx0208
                            japanese-jisx0213-1
                            japanese-jisx0213-2
                            japanese-jisx0213.2004-1
                            katakana-jisx0201
                            latin-jisx0201
                            japanese-jisx0212
                            japanese-jisx0208-1978)))))

(defun skk-setup-keymap ()
  "SKK $B$N%-!<$r@_Dj$9$k!#(B"
  (cond (skk-j-mode
         (skk-define-j-mode-map)
         (skk-setup-j-mode-map-options))
        (skk-latin-mode
         (skk-define-latin-mode-map)
         (skk-setup-latin-mode-map-options))
        (skk-jisx0208-latin-mode
         (skk-define-jisx0208-latin-mode-map)
         (skk-setup-jisx0208-latin-mode-map-options))
        (skk-abbrev-mode
         (skk-define-abbrev-mode-map)
         (skk-setup-abbrev-mode-map-options)))
  (skk-define-minibuffer-maps)
  ;;
  (unless skk-rule-tree
    (setq skk-rule-tree (skk-compile-rule-list skk-rom-kana-base-rule-list
                                               skk-rom-kana-rule-list))))

(defun skk-define-menu (map)
  "SKK $B$N%W%k%@%&%s%a%K%e!<$rDj5A$9$k!#(B"
  (easy-menu-define skk-menu map "Menu used in SKK mode." skk-menu-items))

(defun skk-define-j-mode-map ()
  "$B%-!<%^%C%W(B `skk-j-mode-map' $B$rDj5A$9$k!#(B"
  (unless (keymapp skk-j-mode-map)
    (setq skk-j-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-j-mode skk-j-mode-map)
    (skk-update-minor-mode-map-alist 'skk-jisx0201-mode skk-j-mode-map))
  (unless (eq (lookup-key skk-j-mode-map "a") 'skk-insert)
    (dotimes (i 95)         ;from " " to "~".
      (define-key skk-j-mode-map (skk-char-to-unibyte-string (+ 32 i))
        'skk-insert))
    (skk-define-menu skk-j-mode-map)))

(defun skk-define-latin-mode-map ()
  "$B%-!<%^%C%W(B `skk-latin-mode-map' $B$rDj5A$9$k!#(B"
  (unless (keymapp skk-latin-mode-map)
    (setq skk-latin-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-latin-mode skk-latin-mode-map)
    (skk-define-menu skk-latin-mode-map)))

(defun skk-define-jisx0208-latin-mode-map ()
  "$B%-!<%^%C%W(B `skk-jisx0208-latin-mode-map' $B$rDj5A$9$k!#(B"
  (unless (keymapp skk-jisx0208-latin-mode-map)
    (setq skk-jisx0208-latin-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-jisx0208-latin-mode
                                     skk-jisx0208-latin-mode-map))
  (unless (eq (lookup-key skk-jisx0208-latin-mode-map "a")
              'skk-jisx0208-latin-insert)
    (dotimes (i 128)
      (when (aref skk-jisx0208-latin-vector i)
        (define-key skk-jisx0208-latin-mode-map
          (skk-char-to-unibyte-string i)
          'skk-jisx0208-latin-insert)))
    (define-key skk-jisx0208-latin-mode-map "\C-q" 'skk-toggle-characters)
    (skk-define-menu skk-jisx0208-latin-mode-map)))

(defun skk-define-abbrev-mode-map ()
  "$B%-!<%^%C%W(B `skk-abbrev-mode-map' $B$rDj5A$9$k!#(B"
  (unless (keymapp skk-abbrev-mode-map)
    (setq skk-abbrev-mode-map (make-sparse-keymap))
    (skk-update-minor-mode-map-alist 'skk-abbrev-mode skk-abbrev-mode-map)
    (dotimes (i 95)
      (define-key skk-abbrev-mode-map (skk-char-to-unibyte-string (+ 32 i))
        'skk-abbrev-insert))
    (define-key skk-abbrev-mode-map "," 'skk-abbrev-comma)
    (define-key skk-abbrev-mode-map "." 'skk-abbrev-period)
    (define-key skk-abbrev-mode-map "\C-q" 'skk-toggle-characters)
    (skk-define-menu skk-abbrev-mode-map)))

(defun skk-define-minibuffer-maps ()
  (unless (eq (lookup-key minibuffer-local-map skk-kakutei-key) 'skk-kakutei)
    (define-key minibuffer-local-map skk-kakutei-key #'skk-kakutei)
    (define-key minibuffer-local-completion-map skk-kakutei-key #'skk-kakutei)
    (define-key minibuffer-local-ns-map skk-kakutei-key #'skk-kakutei)))

(skk-define-abbrev-mode-map)
(skk-define-latin-mode-map)
(skk-define-jisx0208-latin-mode-map)
(skk-define-j-mode-map)

(defun skk-setup-j-mode-map-options ()
  (unless (eq (lookup-key skk-j-mode-map
                          (skk-char-to-unibyte-string skk-try-completion-char))
              'skk-insert)
    (when (vectorp skk-kakutei-key)
      (define-key skk-j-mode-map skk-kakutei-key #'skk-kakutei))
    (define-key skk-j-mode-map
      (skk-char-to-unibyte-string skk-try-completion-char)
      #'skk-insert)

    ;; Workaround for key translation.
    (when (eq skk-try-completion-char 9)
      ;; tab $B%-!<$O(B <tab> $B$NDj5A$,L5$1$l$P(B TAB $B$NDj5A$,3d$jEv$F$i$l$k!#(B
      ;; Org-mode $B$J$I$O(B <tab> $B$rDj5A$9$k$N$G!$(BSKK $B$NJ}$G$b(B <tab> $B$rDj5A(B
      ;; $B$9$kI,MW$,$"$k!#(B
      (define-key skk-j-mode-map [(tab)] #'skk-comp-wrapper))

    ;; comp $B$H(B dcomp $B$G$NA08uJd$XLa$kF0:n$r(B Shift TAB $B$G$b2DG=$H$9$k!#(B
    (when skk-previous-completion-use-backtab
      (define-key skk-j-mode-map skk-previous-completion-backtab-key
        #'skk-previous-comp-maybe))
    ;;
    (unless (featurep 'skk-kanagaki)
      (dolist (key skk-previous-candidate-keys)
        (define-key skk-j-mode-map key #'skk-previous-candidate)))
    ;;
    (when skk-use-jisx0201-input-method
      ;; This command is autoloaded.
      (define-key skk-j-mode-map "\C-q" #'skk-toggle-katakana))
    (unless skk-use-viper
      (define-key skk-j-mode-map
        (skk-char-to-unibyte-string skk-start-henkan-with-completion-char)
        #'skk-comp-start-henkan)
      (define-key skk-j-mode-map
        (skk-char-to-unibyte-string skk-backward-and-set-henkan-point-char)
        #'skk-backward-and-set-henkan-point))
    (skk-setup-delete-backward-char)
    (skk-setup-undo)
    ;;
    (when (eq skk-j-mode-function-key-usage 'conversion)
      (define-key skk-j-mode-map [f1] #'skk-start-henkan-prog-1)
      (define-key skk-j-mode-map [f2] #'skk-start-henkan-prog-2)
      (define-key skk-j-mode-map [f3] #'skk-start-henkan-prog-3)
      (define-key skk-j-mode-map [f4] #'skk-start-henkan-prog-4)
      (define-key skk-j-mode-map [f5] #'skk-start-henkan-prog-5)
      (define-key skk-j-mode-map [f6] #'skk-start-henkan-prog-6)
      (define-key skk-j-mode-map [f7] #'skk-start-henkan-prog-7)
      (define-key skk-j-mode-map [f8] #'skk-start-henkan-prog-8)
      (define-key skk-j-mode-map [f9] #'skk-start-henkan-prog-9)
      (define-key skk-j-mode-map [f10] #'skk-start-henkan-prog-0))))

(defun skk-setup-latin-mode-map-options ()
  (unless (eq (lookup-key skk-latin-mode-map skk-kakutei-key) 'skk-kakutei)
    (define-key skk-latin-mode-map skk-kakutei-key #'skk-kakutei)))

(defun skk-setup-jisx0208-latin-mode-map-options ()
  (unless (eq (lookup-key skk-jisx0208-latin-mode-map skk-kakutei-key)
              'skk-kakutei)
    (define-key skk-jisx0208-latin-mode-map skk-kakutei-key #'skk-kakutei)
    (unless skk-use-viper
      (define-key skk-jisx0208-latin-mode-map
        (skk-char-to-unibyte-string skk-backward-and-set-henkan-point-char)
        #'skk-backward-and-set-henkan-point))))

(defun skk-setup-abbrev-mode-map-options ()
  (unless (eq (lookup-key skk-abbrev-mode-map skk-kakutei-key) 'skk-kakutei)
    (define-key skk-abbrev-mode-map skk-kakutei-key #'skk-kakutei)
    (define-key skk-abbrev-mode-map
      (skk-char-to-unibyte-string skk-start-henkan-char)
      #'skk-start-henkan)
    (define-key skk-abbrev-mode-map
      (skk-char-to-unibyte-string skk-try-completion-char)
      #'skk-try-completion)
    (unless skk-use-viper
      (define-key skk-abbrev-mode-map
        (skk-char-to-unibyte-string skk-start-henkan-with-completion-char)
        #'skk-comp-start-henkan))))

(defun skk-make-indicator-alist ()
  "SKK $B%$%s%8%1!<%?7?%*%V%8%'%/%H$rMQ0U$7!"O"A[%j%9%H$K$^$H$a$k!#(B"
  (skk-emacs-prepare-modeline-properties)
  (let ((mode-string-list '(skk-latin-mode-string
                            skk-hiragana-mode-string
                            skk-katakana-mode-string
                            skk-jisx0208-latin-mode-string
                            skk-abbrev-mode-string
                            skk-jisx0201-mode-string))
        mode string base)
    (save-match-data
      (cons
       (cons 'default
             (cons "" (skk-mode-string-to-indicator 'default "")))
       (mapcar (lambda (symbol)
                 (setq mode (prin1-to-string symbol))
                 (string-match "skk-\\([-a-z0-9]+\\)-mode-string" mode)
                 (setq mode (intern (match-string-no-properties 1 mode)))
                 (setq string (symbol-value symbol))
                 ;; $BK\Mh$J$i$3$N$h$&$K%f!<%6JQ?t$r2C9)$9$k$N$O$*$+$7$$$,!"(B
                 ;; $B0\9T4|$N=hCV$H$7$F;CDjE*$K9T$J$&!#(B
                 (cond
                  ((string-match "^ +" string)
                   ;; minor-mode setting
                   (setq base (substring string (match-end 0))))
                  ((string-match "^--" string)
                   ;; mode-line left setting
                   (setq base (substring string (match-end 0)))
                   (when (string-match "::*$" base)
                     (setq base (substring base 0 (match-beginning 0)))))
                  (t
                   (setq base string)))
                 (cons mode
                       (cons (concat " " base)
                             (skk-make-indicator-alist-1 mode base))))
               mode-string-list)))))

(defun skk-make-indicator-alist-1 (mode base)
  (skk-mode-string-to-indicator mode
                                (format "%s%s%s" skk-indicator-prefix base
                                        (funcall skk-indicator-suffix-func mode))))

(defun skk-setup-modeline ()
  "$B%b!<%I%i%$%s$X$N%9%F!<%?%9I=<($r=`Hw$9$k!#(B"
  (setq skk-indicator-alist (skk-make-indicator-alist))
  (cl-case skk-status-indicator
    (left
     (unless (and mode-line-format
                  (memq 'skk-modeline-input-mode (default-value 'mode-line-format)))
       (setq-default mode-line-format
                     (append '("" skk-modeline-input-mode)
                             (default-value 'mode-line-format))))
     (skk-loop-for-buffers (buffer-list)
       (when (and (consp mode-line-format)
                  (local-variable-p 'mode-line-format)
                  (null (memq 'skk-modeline-input-mode mode-line-format)))
         (setq mode-line-format
               (append '("" skk-modeline-input-mode) mode-line-format))))
     (when skk-icon
       (unless (memq 'skk-icon (default-value 'mode-line-format))
         (setq-default mode-line-format
                       (append '("" skk-icon)
                               (default-value 'mode-line-format))))
       (skk-loop-for-buffers (buffer-list)
         (when (and (consp mode-line-format)
                    (local-variable-p 'mode-line-format)
                    (null (memq 'skk-icon mode-line-format)))
           (setq mode-line-format (append '("" skk-icon) mode-line-format)))))
     (force-mode-line-update t))
    ;;
    (t
     (when (and (consp mode-line-format)
                (equal (car mode-line-format) "")
                (eq 'skk-modeline-input-mode (nth 1 mode-line-format)))
       ;; for skk-restart.
       (setq-default mode-line-format
                     (nthcdr 2 mode-line-format)))

     (skk-loop-for-buffers (buffer-list)
       (when (and (consp mode-line-format)
                  (equal (car mode-line-format) "")
                  (eq 'skk-modeline-input-mode (nth 1 mode-line-format)))
         ;; for skk-restart.
         (setq mode-line-format (nthcdr 2 mode-line-format))))
     (setq-default skk-modeline-input-mode "")
     (add-minor-mode 'skk-mode 'skk-modeline-input-mode))))

(defun skk-setup-emulation-commands (commands emulation)
  (let ((map (if (and (boundp 'overriding-local-map)
                      (keymapp 'overriding-local-map))
                 overriding-local-map
               (current-global-map))))
    (dolist (command commands)
      (dolist (key (where-is-internal command map))
        (define-key skk-abbrev-mode-map key emulation)
        (define-key skk-j-mode-map key emulation)))))

(defun skk-setup-delete-backward-char ()
  "$B!V8eB`!W7O$N%-!<$K%3%^%s%I(B `skk-delete-backward-char' $B$r3dEv$F$k!#(B"
  (skk-setup-emulation-commands
   skk-delete-backward-char-commands 'skk-delete-backward-char))

(defun skk-setup-undo ()
  "$B!V$d$jD>$7!W7O$N%-!<$K%3%^%s%I(B `skk-undo' $B$r3dEv$F$k!#(B"
  (skk-setup-emulation-commands skk-undo-commands 'skk-undo))

(defun skk-setup-verbose-messages ()
  (unless skk-henkan-on-message
    (let ((list
           (split-string
            (apply 'format
                   ;; fotmat#STRING
                   (cl-case skk-j-mode-function-key-usage
                     (conversion
                      "\
\[F5]%s  [F6]%s  [F7]%s  [F8]%s  [F9]%s  [F10]%s")
                     (t
                      "\
\[C-5 SPC]%s  [C-6 SPC]%s  [C-7 SPC]%s  [C-8 SPC]%s  [C-9 SPC]%s  [C-0 SPC]%s"))
                   ;; format#OBJECTS
                   (mapcar
                    (lambda (x)
                      (cdr
                       (assoc (caar (symbol-value
                                     (intern-soft
                                      (format "skk-search-prog-list-%d"
                                              x))))
                              '((skk-search-tankanji . "$BC14A;z(B")
                                (skk-search-identity . "$BL5JQ49(B")
                                (skk-search-katakana . "$B%+%?%+%J(B")
                                (skk-search-hankaku-katakana . "$BH>3Q%+%J(B")
                                (skk-search-jisx0208-romaji . "$BA43Q%m!<%^(B")
                                (skk-search-romaji . "$B%m!<%^(B")
                                (nil . "$BL$Dj5A(B")))))
                    '(5 6 7 8 9 0))) "  "))
          new) ; END varlist
      ;; BODY
      (dolist (x list)
        (let* ((y (split-string x "]"))
               (s1 (car y))
               (s2 (nth 1 y)))
          (setq new (concat new
                            (propertize (concat s1 "]") 'face
                                        'skk-verbose-kbd-face)
                            s2 " "))))
      (setq skk-henkan-on-message new))))

(defun skk-compile-init-file-maybe ()
  "$BI,MW$J$i(B `skk-init-file' $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
`skk-byte-compile-init-file' $B$,(B non-nil $B$N>l9g$G!"(B`skk-init-file' $B$r%P%$%H%3(B
$B%s%Q%$%k$7$?%U%!%$%k$,B8:_$7$J$$$+!"$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$h$j(B
`skk-init-file' $B$NJ}$,?7$7$$$H$-$O!"(B`skk-init-file' $B$r%P%$%H%3%s%Q%$%k$9$k!#(B

`skk-byte-compile-init-file' $B$,(B nil $B$N>l9g$G!"(B`skk-init-file' $B$r%P%$%H%3%s%Q(B
$B%$%k$7$?%U%!%$%k$h$j(B `skk-init-file' $B$NJ}$,?7$7$$$H$-$O!"$=$N%P%$%H%3%s%Q%$(B
$B%k:Q%U%!%$%k$r>C$9!#(B"
  (save-match-data
    (let* ((file (expand-file-name skk-init-file))
           (init-file (if (file-exists-p (concat file ".el"))
                          (concat file ".el")
                        file))
           (elc (concat init-file
                        (if (string-match "\\.el$" init-file)
                            "c"
                          ".elc"))))
      (if skk-byte-compile-init-file
          (when (and (file-exists-p init-file)
                     (or (not (file-exists-p elc))
                         (file-newer-than-file-p init-file elc)))
            (save-window-excursion ; for keep window configuration.
              (skk-message "%s $B$r%P%$%H%3%s%Q%$%k$7$^$9(B"
                           "Byte-compile %s"
                           skk-init-file)
              (sit-for 2)
              (byte-compile-file init-file)))
        (when (and (file-exists-p init-file)
                   (file-exists-p elc)
                   (file-newer-than-file-p init-file elc))
          (delete-file elc))))))

(defun skk-delete-selection-wrapper ()
  (unless (and (member (char-to-string last-command-event)
                       (mapcar (lambda (x) (car x)) skk-auto-paren-string-alist))
               skk-use-auto-enclose-pair-of-region)
    (delete-region (point) (mark))
    (if (and overwrite-mode
             (eq this-command 'self-insert-command))
        (let ((overwrite-mode nil))
          (self-insert-command
           (prefix-numeric-value current-prefix-arg))
          (setq this-command 'ignore)))))

(defun skk-setup-delete-selection-mode ()
  "Delete Selection $B%b!<%I$N$?$a$N@_Dj$r$9$k!#(B
Delete Selection $B%b!<%I$,(B SKK $B$r;H$C$?F|K\8lF~NO$KBP$7$F$b5!G=$9$k$h$&$K(B
$B%;%C%H%"%C%W$9$k!#(B"
  (let ((funcs '(skk-current-kuten
                 skk-current-touten
                 skk-input-by-code-or-menu
                 skk-today))
        (supersede-funcs '(skk-delete-backward-char)))
    (unless (get 'skk-insert 'delete-selection)
      (dolist (func funcs)
        (put func 'delete-selection t))
      (dolist (func supersede-funcs)
        (put func 'delete-selection 'supersede))
      (put 'skk-insert 'delete-selection 'skk-delete-selection-wrapper))))

(defun skk-setup-auto-paren ()
  (when (and skk-auto-insert-paren
             skk-auto-paren-string-alist)
    ;; skk-auto-paren-string-alist $B$NCf$+$i!"(B
    ;; skk-special-midashi-char-list $B$NMWAG$K4XO"$9$k$b$N$r<h$j=|$/!#(B
    (dolist (s (mapcar #'char-to-string
                       skk-special-midashi-char-list))
      (skk-remove-alist 'skk-auto-paren-string-alist s))

    (when (memq t (mapcar (lambda (e)
                            (skk-ascii-char-p (string-to-char (car e))))
                          skk-auto-paren-string-alist))
      (let (rulealst str)
        (setq rulealst (nconc (mapcar (lambda (e)
                                        (nth 2 e))
                                      skk-rom-kana-rule-list)
                              (mapcar (lambda (e)
                                        (nth 2 e))
                                      skk-rom-kana-base-rule-list)))
        (dolist (cell skk-auto-paren-string-alist)
          (setq str (car cell))
          (when (and (skk-ascii-char-p (string-to-char str))
                     ;; $B=PNOJ8;z$,F~$C$F$$$k%;%k$rD4$Y$F!"$$$:$l$+$N(B
                     ;; $B%-!<$K%P%$%s%I$5$l$F$$$J$$$+$I$&$+$r3NG'$9$k!#(B
                     (null (assoc str rulealst))
                     (null (rassoc str rulealst))
                     ;; $B3d$jIU$1$h$&$H$7$F$$$k%-!<$,!"2?$+B>$N=PNOJ8;z$K(B
                     ;; $B%P%$%s%I$5$l$F$$$J$$$+$I$&$+$r3NG'$9$k!#(B
                     (null (assoc str skk-rom-kana-base-rule-list))
                     (null (assoc str skk-rom-kana-rule-list)))
            ;; skk-auto-paren-string-alist $B$N3FMWAG$N(B car $B$NJ8;z$,(B
            ;; ascii char $B$G$"$k>l9g$O!"(Bskk-rom-kana-rule-list,
            ;; skk-rom-kana-base-rule-list $B$K$=$NJ8;z$r=q$-9~$`!#(B
            ;; $BK\Mh$O(B ascii char $B$O(B skk-rom-kana-rule-list,
            ;; skk-rom-kana-base-rule-list $B$K=q$/I,MW$,$J$$(B
            ;; --- skk-emulate-original-map$B$K$h$kF~NO$,9T$J$o$l$k(B ---
            ;; $B$,!"(Bskk-auto-paren-string-alist $B$K;XDj$5$l$?BP$K$J$k(B
            ;; $BJ8;z$NA^F~$N$?$a$K$O!"%-!<$H$J$kJ8;z$r=q$$$F$*$/I,MW$,(B
            ;; $B$"$k!#(B
            (setq skk-rom-kana-rule-list
                  (cons (list str nil str)
                        skk-rom-kana-rule-list))))))))

(defun skk-setup-minibuffer ()
  "$B%+%l%s%H%P%C%U%!$NF~NO%b!<%I$K=>$C$F%_%K%P%C%U%!$NF~NO%b!<%I$r@_Dj$9$k!#(B"
  (cl-case skk-minibuffer-origin-mode
    (hiragana
     (skk-j-mode-on))
    (katakana
     (skk-j-mode-on t))
    (abbrev
     (skk-abbrev-mode-on))
    (latin
     (skk-latin-mode-on))
    (jisx0208-latin
     (skk-jisx0208-latin-mode-on))))

;;;###autoload
(defun skk-setup-jisyo-buffer ()
  "SKK $B$N<-=q%P%C%U%!$rMQ0U$9$k!#(B
`skk-jisyo' $B$N<-=q%P%C%U%!$G!"(B
 (1)$B6u%P%C%U%!$G$"$l$P!"?7$7$/%X%C%@!<$r:n$j!"(B
 (2)$B<-=q%(%s%H%j$,$"$k4{B8$N<-=q%P%C%U%!$J$i$P!"%X%C%@!<$,@5$7$$$+$I$&$+$r(B
    $B%A%'%C%/$9$k!#(B"

  ;; skk-okuri-ari-min $B$H(B skk-okuri-nasi-min $B$N0LCV$rJQ99$7$?!#(B
  ;;                       $B"-(B $B?7$7$$(B skk-okuri-ari-min
  ;;   ;; okuri-ari entries.
  ;;   $B"+(B $B0JA0$N(B skk-okuri-ari-min
  ;;
  ;;   $B"-(B skk-okuri-ari-max $B"-(B $B?7$7$$(B skk-okuri-nasi-min
  ;;   ;; okuri-nasi entries.
  ;;   $B"+(B $B0JA0$N(B skk-okuri-nasi-min
  ;;
  ;;
  ;; $BJQ99A0$N0LCV$G$"$l$P!"2<5-$N$h$&$J6u<-=q$N>l9g!"(B
  ;;
  ;;   ;; okuri-ari entries.
  ;;   ;; okuri-nasi entries.
  ;;
  ;; skk-okuri-ari-min $B$H(B skk-okuri-ari-max $B$N%^!<%+!<$,=E$J$C$F$7$^$$!"(B
  ;; skk-okuri-ari-min $B$N0LCV$KA^F~$7$?8uJd$,(B skk-okuri-ari-max $B$N%^!<%+!<(B
  ;; $B$r8eJ}$K2!$7$d$i$J$$!#(B
  ;;
  ;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
  (save-match-data
    (when (zerop (buffer-size))
      ;; $B6u%P%C%U%!$@$C$?$i!"%X%C%@!<$N$_A^F~!#(B
      (insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n"))
    (goto-char (point-min))
    (unless (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
      (skk-error "$BAw$j$"$j%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B"
                 "Header line for okuri-ari entries is missing!"))
    ;; $B8GDj%]%$%s%H$J$N$G!"(B(point) $B$G==J,!#(B
    (setq skk-okuri-ari-min (point))
    (unless (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
      (skk-error "$BAw$j$J$7%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B"
                 "Header line for okuri-nasi entries is missing!"))
    (beginning-of-line)
    ;; $B6&M-<-=q$J$i8GDj%]%$%s%H$G$bNI$$$N$@$,!"<-=q%P%C%U%!$GJT=8$r9T(B
    ;; $B$J$C$?$H$-$N$3$H$rG[N8$7$F%^!<%+!<$K$7$F$*$/!#(B
    (setq skk-okuri-ari-max (point-marker))
    (forward-line 1)
    (backward-char 1)
    (setq skk-okuri-nasi-min (point-marker))))

;;;###autoload
(defun skk-emulate-original-map (arg)
  "$B%-!<F~NO$KBP$7$F(B Emacs $B$N%*%j%8%J%k$N%-!<3d$jIU$1$G%3%^%s%I$r<B9T$9$k!#(B"
  (let ((prefix-arg arg)
        (keys (skk-command-key-sequence (this-command-keys) this-command)))
    (when keys ; If key is nil, the command may have been invoked by M-x.
      (let (skk-mode
            skk-latin-mode
            skk-j-mode
            skk-abbrev-mode
            skk-jisx0208-latin-mode
            skk-jisx0201-mode
            command)
        ;; have to search key binding after binding 4 minor mode flags to nil.
        (setq command (key-binding keys))
        (unless command
          (setq keys (lookup-key function-key-map keys))
          (when keys
            (setq command (key-binding keys))))
        ;; avoid recursive calling of skk-emulate-original-map.
        (unless (eq command this-command)
          (setq this-command command)
          (unless (or (memq command skk-undo-commands)
                      (memq command skk-delete-backward-char-commands))
            (skk-cancel-undo-boundary))
          ;; if no bindings are found, call `undefined'.  it's
          ;; original behaviour.
          (command-execute (or command
                               #'undefined)))))))

(defun skk-command-key-sequence (key command)
  "KEY $B$+$i(B universal arguments $B$r<h$j=|$-!"(BCOMMAND $B$r<B9T$9$k%-!<$rJV$9!#(B
`execute-extended-command' $B$K$h$C$F%3%^%s%I$,<B9T$5$l$?>l9g$O!"(Bnil $B$rJV$9!#(B"
  (while (not (or (zerop (length key))
                  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (unless (zerop (length key))
    key))

;;;###autoload
(defun skk-adjust-user-option ()
  "$B%f!<%6%*%W%7%g%s$NIT@09g$rD4@0$9$k!#(B"
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  ;; $BN>N)$G$-$J$$%*%W%7%g%s$rD4@0$9$k!#(B
  (when skk-process-okuri-early
    ;; skk-process-okuri-early $B$NCM$,(B non-nil $B$G$"$k$H$-$K2<5-$NCM$,(B non-nil
    ;; $B$G$"$l$P@5>o$KF0$+$J$$$N$G$3$NJQ?t$NM%@h=g0L$r9b$/$7$?!#(B
    (setq skk-kakutei-early nil
          skk-auto-okuri-process nil
          skk-henkan-okuri-strictly nil
          skk-henkan-strict-okuri-precedence nil))
  (unless skk-jisyo-save-count
    ;; $B8=:_$N<BAu$G$O!"8D?M<-=q$N%*!<%H%;!<%VL5$7$G$O8D?M<-=q$N6&M-$O$G$-$J$$(B
    ;; $B$3$H$K$J$C$F$$$k!#(B
    (setq skk-share-private-jisyo nil)))

;;;###autoload
(defun skk-latin-mode (arg)
  "SKK $B$N%b!<%I$r%"%9%-!<%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-latin-mode-on)
  nil)

;;;###autoload
(defun skk-jisx0208-latin-mode (arg)
  "SKK $B$N%b!<%I$rA41Q%b!<%I$KJQ99$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-jisx0208-latin-mode-on)
  nil)

(defun skk-abbrev-mode (arg)
  "ascii $BJ8;z$r%-!<$K$7$?JQ49$r9T$&$?$a$NF~NO%b!<%I!#(B"
  (interactive "*P")
  (cl-case skk-henkan-mode
    (active
     (skk-kakutei))
    (on
     (skk-error "$B4{$K"&%b!<%I$KF~$C$F$$$^$9(B" "Already in $B"&(B mode")))
  (let (skk-dcomp-activate)
    (skk-set-henkan-point-subr))
  (skk-abbrev-mode-on)
  nil)

;;;###autoload
(defun skk-toggle-characters (arg)
  "$B"#%b!<%I!""'%b!<%I$G!"$+$J%b!<%I$H%+%J%b!<%I$r%H%0%k$G@Z$jBX$($k!#(B
$B"&%b!<%I$G$O(B `skk-henkan-start-point' ($B"&$ND>8e(B) $B$H%+!<%=%k$N4V$NJ8;zNs$K$D$$(B
$B$F!"$R$i$,$J$H%+%?%+%J$rF~$lBX$($k!#(B"
  (interactive "P")
  (let ((show skk-show-mode-enable))    ;$B%A%e!<%H%j%"%k<B9TCf$N$_(B nil
    (cond
     ((eq skk-henkan-mode 'on)      ;$B"&%b!<%I(B
      (let (char-type)
        (skk-save-point
         (goto-char skk-henkan-start-point)
         (while (and (>= skk-save-point (point))
                     ;; (not (eobp))
                     (or
                      ;; "$B!<(B" $B$G$OJ8;z<oJL$,H=JL$G$-$J$$$N$G!"%]%$%s%H$r?J$a$k!#(B
                      (looking-at "$B!<(B")
                      (eq 'unknown (setq char-type (skk-what-char-type)))))
           (forward-char 1)))
        (skk-henkan-skk-region-by-func
         (cl-case char-type
           (hiragana #'skk-katakana-region)
           (katakana #'skk-hiragana-region)
           (jisx0208-latin #'skk-latin-region)
           (ascii #'skk-jisx0208-latin-region))
         ;; `skk-katakana-region' $B$N0z?t(B VCONTRACT $B$^$?$O(B
         ;; `skk-hiragana-region' $B$N0z?t(B VEXPAND $B$rM?$($k!#(B
         (memq char-type '(hiragana katakana))))
      (setq show nil))
     ((and (skk-in-minibuffer-p)
           (not skk-j-mode))
      ;; $B%_%K%P%C%U%!$X$N=iFMF~;~!#(B
      (skk-j-mode-on))
     (t
      (setq skk-katakana (not skk-katakana))))
    (skk-kakutei)
    (when skk-j-mode
      (let ((skk-show-mode-show show))
        (skk-j-mode-on skk-katakana))))
  nil)

(defun skk-misc-for-picture ()
  "`picture-mode' $B$XF~$C$?$H$-$K(B SKK $B5/F0A0$N>uBV$KLa$9!#(B
`edit-picture-hook' $B$K(B `add-hook' $B$7$F;HMQ$9$k!#(B"
  ;; $B4{B8$N%P%C%U%!$r(B picture mode $B$K$7$?$H$-!"(Bpicture-mode $B4X?t$O(B
  ;; kill-all-local-variables $B4X?t$r8F$P$J$$$N$G!"(BSKK $B4XO"$N%P%C%U%!%m!<%+%k(B
  ;; $BJQ?t$,85$N%P%C%U%!$NCM$N$^$^$K$J$C$F$7$^$&!#$=$3$G!"(Bpicture mode $B$KF~$C$?(B
  ;; $B$H$-$K%U%C%/$rMxMQ$7$F$3$l$i$N%P%C%U%!%m!<%+%kJQ?t$r(B kill $B$9$k!#(B
  ;; RMS $B$O(B picture-mode $B$G(B kill-all-local-variables $B4X?t$r8F$P$J$$$N$O!"%P%0(B
  ;; $B$G$O$J$$!"$H8@$C$F$$$?!#(B
  ;;
  ;; picture-mode $B$G(B SKK $B$r;HMQ$74A;zF~NO$r$7$?>l9g$K!"(BBS $B$GA43QJ8;z$,>C$;$J$$(B
  ;; $B$N$O!"(BSKK $B$NIT6q9g$G$O$J$/!"(Bpicture.el $B$NLdBj(B (move-to-column-force $B4X?t(B
  ;; $B$NCf$G;HMQ$7$F$$$k(B move-to-column $B$GA43QJ8;z$rL5;k$7$?%+%i%`?t$,M?$($i$l(B
  ;; $B$?$H$-$K%+!<%=%k0\F0$,$G$-$J$$$+$i(B) $B$G$"$k!#>C$7$?$$J8;z$K%]%$%s%H$r9g$o(B
  ;; $B$;!"(BC-c C-d $B$G0lJ8;z$:$D>C$9$7$+J}K!$O$J$$!#(B
  (when skk-mode
    (skk-kill-local-variables)))

(defun skk-kill-local-variables ()
  "SKK $B4XO"$N%P%C%U%!%m!<%+%kJQ?t$rL58z$K$9$k!#(B"
  (skk-mode -1)
  (let (v vstr)
    (dolist (lv (buffer-local-variables))
      (setq v (car lv)
            vstr (prin1-to-string v))
      (when (and (> (length vstr) 3)
                 (string= "skk-" (substring vstr 0 4)))
        (kill-local-variable v)))))

;;;; kana inputting functions
;;;###autoload
(defun skk-insert (&optional arg prog-list-number)
  "SKK $B$NJ8;zF~NO$r9T$J$&!#(B"
  (interactive "p")
  (unless prog-list-number
    (setq prog-list-number current-prefix-arg))
  (barf-if-buffer-read-only)
  (skk-with-point-move
   (let ((ch last-command-event))
     (cond ((and skk-henkan-mode
                 (memq ch skk-special-midashi-char-list))
            (if (= skk-henkan-start-point (point))
                ;; `$B"&(B' $B$KB3$/(B `>' $B$G$OJQ49=hM}$r3+;O$7$J$$(B
                (progn
                  (set 'last-command-event ?>)
                  (skk-kana-input arg))
              ;; $B@\F,<-!&@\Hx<-$N=hM}!#(B
              (skk-process-prefix-or-suffix arg)))

           ;; start writing a midasi key.
           ((and (memq ch skk-set-henkan-point-key)
                 (or skk-okurigana
                     (not (skk-get-prefix skk-current-rule-tree))
                     (not (skk-select-branch skk-current-rule-tree ch))))
            ;; normal pattern
            ;; skk-set-henkan-point -> skk-kana-input.
            (setq skk-insert-keysequence (string ch))
            (skk-set-henkan-point arg))

           ;; start conversion.
           ((and skk-henkan-mode        ; $B"&%b!<%I(B or $B"'%b!<%I(B
                 (eq ch skk-start-henkan-char)) ; SPC
            (skk-start-henkan arg prog-list-number))

           ;; just input kana.
           ((not (eq skk-henkan-mode 'on)) ; not $B"&%b!<%I(B
            (skk-kana-input arg))

           ;; for completion.
           ;; $B%3%s%W%j!<%7%g%s4XO"$N4X?t$O(B skk-rom-kana-base-rule-list $B$NCf$K2!(B
           ;; $B$79~$a!"(Bskk-kana-input $B$NCf$+$i@)8f$9$Y$-!#(B
           ;; $BC"$7!"(BTAB $B$O(B self-insert-command $B$G$O$J$$$N$G!"(Bskk-j-mode-map $B$N(B
           ;; $B%-!<%^%C%W$G(B substitute-key-definition $B$7$F$b(B skk-insert $B$K%P%$%s(B
           ;; $B%I$G$-$J$$!#(Bskk-j-mode-map $B$G(B $BD>@\(B "\t" $B$r(B skk-insert $B$K%P%$%s%I(B
           ;; $B$7$F!"(Bcompletion $B$H(B skk-current-kuten/skk-current-touten $B$r%3%s%H(B
           ;; $B%m!<%k$9$k%3%^%s%IL>$r(B skk-rom-kana-base-rule-list $B$K=q$1$PNI$$$+(B
           ;; $B$b!#(B
           ;; $B$G$b!"(Bskk-comp $B$H(B skk-current-kuten/skk-current-touten $B$N%3%s%H%m(B
           ;; $B!<%k$,%O!<%I%3!<%G%#%s%0$5$l$k$N$O$^$:$$$+$b(B (skk-comp $B$O;H$C$F$b(B
           ;; skk-current-kuten/skk-current-touten $B$O;H$o$J$$!"$H$$$&?M$,$$$k$+(B
           ;; $B$b(B)$B!#(B
           ((and (eq skk-henkan-mode 'on) ; $B"&%b!<%I(B
                 (eq ch skk-try-completion-char))
            (skk-comp (or prog-list-number ; C-u TAB $B$GJd40%-!<$r=i4|2=(B
                          (not (eq last-command 'skk-comp-do)))))

           ;;
           ((and (eq skk-henkan-mode 'on) ; $B"&%b!<%I(B
                 (memq ch (list skk-next-completion-char
                                skk-previous-completion-char))
                 (eq last-command 'skk-comp-do))
            (skk-comp-previous/next ch))

           ;; just input Kana.
           (t
            (setq skk-insert-keysequence (concat skk-insert-keysequence (string ch)))
            (skk-kana-input arg)))
     ;; verbose message
     (skk-henkan-on-message))))

(defun skk-process-prefix-or-suffix (&optional arg)
  "$B@\F,<-$^$?$O@\Hx<-$NF~NO$r3+;O$9$k!#(B
$B$3$l$O!"IaDL(B `skk-special-midashi-char-list' $B$K;XDj$5$l$?J8;z$NF~NO$,$"$C$?>l(B
$B9g$KHsBPOCE*$K8F$S=P$5$l$k$,!"BPOCE*$K8F=P$9$3$H$b2DG=$G$"$k!#(B"
  ;; SKK 10 $B$^$G$O!"(B> < ? $B$N(B 3 $B$D$K$D$$$F07$$$,J?Ey$G$J$+$C$?!#(BDaredevil SKK
  ;; 11 $B0J9_$G$O!"<-=q$K$*$1$kI=8=$r(B > $B$GE}0l$9$k$3$H$K$h$j!"(B3 $B<T$N07$$$rJ?Ey(B
  ;; $B$K$7!"$J$*$+$D!"$3$N%3%^%s%I$,J8;z%-!<$G$J$$F~NO$K$h$j8F$P$l$?$H$-$K$b@\(B
  ;; $BHx<-!&(B $B@\F,<-F~NO$,$G$-$k$h$&$K$9$k!#(B
  (interactive "*p")
  (cl-case skk-henkan-mode
    (active
     ;; $B@\Hx<-$N$?$a$N=hM}(B
     (skk-kakutei)
     (let (skk-kakutei-history)
       (skk-set-henkan-point-subr))
     (insert-and-inherit ?>))
    (on
     ;; $B@\F,<-$N=hM}(B
     (skk-kana-cleanup 'force)
     (insert-and-inherit ?>)
     (skk-set-marker skk-henkan-end-point (point))
     (setq skk-henkan-count 0)
     (setq skk-henkan-key (buffer-substring-no-properties
                           skk-henkan-start-point (point))
           skk-prefix "")
     (setq skk-after-prefix t)
     (skk-henkan))
    (t
     (if last-command-event
         ;; `skk-insert' $B$+$i8F$P$l$k>l9g$K$O!"$3$N%1!<%9$O$J$$!#(B
         (let ((i (prefix-numeric-value arg))
               (str (skk-char-to-unibyte-string last-command-event)))
           (while (> i 0)
             (skk-insert-str str)
             (setq i (1- i))))
       nil))))

(defun skk-kana-input (&optional arg)
  "$B$+$JJ8;z$NF~NO$r9T$&%k!<%A%s!#(B"
  ;; Message-Id: <19980610190611B.sakurada@kuis.kyoto-u.ac.jp>
  ;; From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
  ;; Date: Wed, 10 Jun 1998 19:06:11 +0900 (JST)
  ;;
  ;; $B?7$7$$(B skk-kana-input $B$O(B, $B4JC1$K@bL@$9$k$H(B,
  ;; $B$"$i$+$8$a%k!<%k$rLZ$N7A$KI=8=$7$F$*$$$F(B, $BF~NO$r8+(B
  ;; $B$FLZ$rC)$j(B, $B$=$l0J>eC)$l$J$/$J$C$?$i$=$N@a$KEPO?$5(B
  ;; $B$l$F$$$k2>L>$rF~NO$9$k(B. $B$H$$$&$7$/$_$G$9(B.
  ;;
  ;; $BNc$($P(B, n a t $B$N$_$+$i$J$k0J2<$N%k!<%k(B
  ;;
  ;;     a  $B"*(B $B$"(B
  ;;     n  $B"*(B $B$s(B
  ;;     nn $B"*(B $B$s(B
  ;;     na $B"*(B $B$J(B
  ;;     ta $B"*(B $B$?(B
  ;;     tt $B"*(B $B$C(B ($B<!>uBV(B t)
  ;;
  ;; $B$r%k!<%kLZ$KJQ49$9$k$H(B,
  ;;
  ;;                 $B!?(B/$B!@(B
  ;;               $B!?(B /   $B!@(B
  ;;           a $B!?(B  / t    $B!@(B n
  ;;           $B!?(B   /         $B!@(B
  ;;          $B$"(B   $B!&(B           $B$s(B
  ;;             $B!?(B|           / $B!@(B
  ;;         a $B!?(B  | t      a /    $B!@(B n
  ;;         $B!?(B    |         /       $B!@(B
  ;;       $B$?(B     $B$C(B        $B$J(B        $B$s(B
  ;;          ($B<!>uBV(B "t")
  ;;
  ;; $B$H$$$&7A$K$J$j$^$9(B.
  ;;
  ;; $B=i4|>uBV(B($BLZ$N:,(B)$B$G(B `a' $B$rF~NO$9$k$H(B, $BLZ$N:,$+$i(B
  ;; $B!V$"!W$K0\F0$7$^$9(B. $B<!$K$I$N$h$&$JF~NO$,Mh$F$b(B,
  ;; $B$=$l$h$j2<$KC)$l$J$$$N$G(B, $B!V$"!W$r=PNO$7$F:,$KLa$j$^$9(B.
  ;; $B%k!<%k$K<!>uBV$,@_Dj$5$l$F$$$k>l9g$O(B, $B@_Dj$5$l$F$$(B
  ;; $B$kJ8;zNs$r%-%e!<$KLa$7$F$+$i:,$KLa$j$^$9(B.
  ;;
  ;; $B=i4|>uBV$G(B `n' $B$rF~NO$9$k$H(B, $B!V$s!W$K0\F0$7$^$9(B.
  ;; $B<!$K(B `a' $B$^$?$O(B `n' $B$,F~NO$5$l$l$P$=$l$h$j2<$KC)$l$k(B
  ;; $B$N$G<!$NF~NO$r8+$k$^$G$^$@=PNO$7$^$;$s(B.
  ;; $B<!$K(B `t' $B$,F~NO$5$l$?>l9g$O(B, `t' $B$G$O2<$KC)$l$J$$$N$G(B,
  ;; $B!V$s!W$r=PNO$7$F(B `t' $B$r%-%e!<$KLa$7$^$9(B.
  ;;
  ;; $B$3$3$G(B, $B=i4|>uBV(B, $B8=>uBV$r$=$l$>$l(B skk-rule-tree,
  ;; skk-current-rule-tree $B$GI=$7(B.
  ;; $BLZ$r2<$KC)$k(B, $B$H$$$&A`:n$O(B, skk-select-branch $B$r(B
  ;; $BMQ$$$F(B,
  ;;
  ;;   (skk-select-branch rule-tree ?a)
  ;;
  ;; $B$N$h$&$K$7$^$9(B. $B8=>uBV$K@_Dj$5$l$F$$$k$+$J(B(("$B%"(B". "$B$"(B")$B$J$I(B),
  ;; $B<!>uBV(B("t" $B$J$I(B)$B$O(B, $B$=$l$>$l(B skk-get-kana,
  ;; skk-get-nextstate $B$G<h$j$^$9(B.
  ;; don't echo key strokes in the minibuffer.
  (let ((echo-keystrokes 0)
        (queue (list last-command-event)))
    (while queue
      (if (not (skk-get-prefix skk-current-rule-tree))
          (progn
            (skk-set-marker skk-kana-start-point (point))
            (setq skk-current-rule-tree skk-rule-tree))
        (skk-erase-prefix))
      (setq skk-prefix (concat (skk-get-prefix skk-current-rule-tree)
                               (skk-char-to-unibyte-string last-command-event)))
      (let ((next (skk-select-branch skk-current-rule-tree (car queue)))
            data)
        (cond
         (next
          ;; can go down SKK-CURRENT-RULE-TREE
          (cond
           ((skk-get-branch-list next)
            ;; NEXT have at least one branch
            (when (and (eq skk-henkan-mode 'active)
                       skk-kakutei-early
                       (not skk-process-okuri-early))
              (skk-kakutei)
              (skk-set-marker skk-kana-start-point (point)))
            (setq queue (cdr queue)
                  skk-current-rule-tree next))
           (t
            ;; NEXT does not have any branch (i.e. NEXT is a leaf)
            (setq data (skk-get-kana next)
                  queue (nconc (string-to-list (skk-get-nextstate next))
                               (cdr queue))
                  skk-current-rule-tree nil))))
         (t
          ;; can not go down SKK-CURRENT-RULE-TREE
          (let ((d (skk-get-kana skk-current-rule-tree)))
            (cond
             (d
              ;; SKK-CURRENT-RULE-TREE have a roma->kana rule
              (setq data d
                    queue (nconc (string-to-list
                                  (skk-get-nextstate skk-current-rule-tree))
                                 queue)
                    skk-current-rule-tree nil))
             (t
              ;; SKK-CURRENT-RULE-TREE does not have any roma->kana rule
              (let ((dd (when skk-kana-input-search-function
                          (funcall skk-kana-input-search-function))))
                (cond
                 (dd
                  (setq data (car dd)
                        queue (nconc (string-to-list (cdr dd))
                                     (cdr queue))
                        skk-current-rule-tree nil))
                 ((eq skk-current-rule-tree skk-rule-tree)
                  ;; typo on the root of tree
                  (setq queue nil
                        skk-current-rule-tree nil))
                 (t
                  ;; otherwise move to root of the tree, and redo
                  (setq skk-current-rule-tree nil)))))))))
        (cond
         ((not data)
          (if skk-current-rule-tree
              (progn
                ;;(digit-argument arg)
                ;; $B$&!A$s!"$h$&J,$+$i$s!#$H$j$"$($:!#(B
                (unless skk-isearch-message
                  (setq prefix-arg arg))
                (setq skk-prefix (skk-get-prefix skk-current-rule-tree))
                (skk-insert-prefix skk-prefix))
            ;;(skk-kana-cleanup 'force)
            (when (eq skk-henkan-mode 'active)
              (skk-kakutei))
            (setq skk-prefix "")
            (unless (or queue
                        (and (not (eq this-command 'skk-insert))
                             skk-henkan-mode))
              (skk-emulate-original-map (skk-make-raw-arg arg)))))
         (t
          ;;(skk-cancel-undo-boundary)
          (setq skk-prefix "")
          (when (functionp data)
            (setq data (funcall data (skk-make-raw-arg arg))))
          (when (stringp (if (consp data)
                             (car data)
                           data))
            (let* ((str (if (consp data)
                            (if skk-katakana
                                (car data)
                              (cdr data))
                          data))
                   (pair (when skk-auto-insert-paren
                           (cdr (assoc str skk-auto-paren-string-alist))))
                   (count0 arg)
                   (count1 arg)
                   (inserted 0)
                   region-str)
              (when (and (eq skk-henkan-mode 'active) ;$B"'%b!<%I(B
                         skk-kakutei-early
                         (not skk-process-okuri-early))
                (skk-kakutei))
              ;; arg $B$OJ]B8$7$F$*$+$J$$$H!"(B0 $B$K$J$C$F$7$^$$!"(Bqueue $B$,(B
              ;; $B$?$^$C$F$$$F:FEY$3$3$X$d$C$FMh$?$H$-$K!"J8;zF~NO$,(B
              ;; $B$G$-$J$/$J$k!#(B
              (skk-cancel-undo-boundary)
              (when (and (use-region-p)
                         skk-use-auto-enclose-pair-of-region
                         pair)
                (setq region-str (buffer-substring (region-beginning)
                                                   (region-end)))
                (delete-region (region-beginning) (region-end)))
              (while (> count0 0)
                (skk-insert-str str)
                (setq count0 (1- count0)))
              (when pair
                (when region-str
                  (insert region-str))
                (while (> count1 0)
                  (unless (string= pair
                                   (skk-char-to-unibyte-string (following-char)))
                    (setq inserted (1+ inserted))
                    (skk-insert-str pair))
                  (setq count1 (1- count1)))
                (unless (zerop inserted)
                  (backward-char inserted))
                (when region-str
                  (skip-chars-forward pair)))
              (when (and skk-okurigana
                         (null queue))
                (skk-set-okurigana)))))))
      ;; XXX I don't know how skk-isearch-message works....
      (when skk-isearch-message
        (skk-isearch-message)))))

;;; tree procedure ($B%D%j!<$K%"%/%;%9$9$k$?$a$N%$%s%?!<%U%'!<%9(B)
(defun skk-search-tree (tree char-list)
  "TREE $B$N:,$+$i@hC<$X(B CHAR-LIST $B$K=>$C$FC)$k!#(B
$B@.8y$7$?>l9g$O(B nil $B$H(B $B7k2L$NLZ$NAH$rJV$7!"<:GT$7$?>l9g$OC)$l$J$+$C$?(B
CHAR-LIST $B$N;D$j$HC)$l$J$/$J$C$?@aE@$NLZ$NAH$rJV$9!#(B"
  (catch 'return
    (let (next char rest)
      (while char-list
        (setq char (car char-list)
              rest (cdr char-list)
              next (skk-select-branch tree char))
        (if next
            (setq tree next
                  char-list rest)
          (throw 'return (cons char-list tree))))
      (cons nil tree))))

(defun skk-add-rule (tree rule)
  (let* ((prefix (nth 0 rule))
         (l (length prefix))
         (result (skk-search-tree tree (string-to-list prefix)))
         (rest (car result))
         (addpoint (cdr result)))
    (while rest
      (let ((addtree (skk-make-rule-tree
                      (car rest)
                      (if (vectorp prefix)
                          prefix
                        (substring prefix 0 (1+ (- l (length rest)))))
                      nil nil nil)))
        (skk-add-branch addpoint addtree)
        (setq addpoint addtree
              rest (cdr rest))))
    (skk-set-nextstate addpoint (nth 1 rule))
    (skk-set-kana addpoint (nth 2 rule))))

(defun skk-delete-rule (tree string)
  "$BF~NO(B STRING $B$KBP$9$k%k!<%k$r%k!<%kLZ(B TREE $B$+$i:o=|$9$k!#(B"
  (catch 'return
    (let ((char-list (string-to-list string))
          (cutpoint tree)
          ;; TREE $B$N:,$+$i=P$k;^$,(B1$BK\$7$+$J$$>l9g(B
          ;; $B$N$?$a$K0l1~=i4|2=$7$F$*$/(B
          (cuttree (car (skk-get-branch-list tree)))
          next)
      (while char-list
        (setq next (skk-select-branch tree (car char-list))
              char-list (cdr char-list))
        (if next
            (if (> (length (skk-get-branch-list tree)) 1)
                (setq cutpoint tree ; $B;^$,(B2$BK\0J>e$N;~(B cutpoint cuttree
                      cuttree next  ; $B$r(B update
                      tree next)
              (setq tree next))
          (throw 'return nil)))
      (skk-set-branch-list cutpoint
                           (delq cuttree (skk-get-branch-list cutpoint))))))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>  := nil | (<tree> . <branch-list>)
;; <tree>         := (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         := (<$B$R$i$,$JJ8;zNs(B> . <$B%+%?%+%JJ8;zNs(B>) | nil
;; <char>         := <$B1Q>.J8;z(B>
;; <nextstate>    := <$B1Q>.J8;zJ8;zNs(B> | nil
;;;###autoload
(defun skk-compile-rule-list (&rest rule-lists)
  "rule list $B$rLZ$N7A$K%3%s%Q%$%k$9$k!#(B"
  (let ((tree (skk-make-rule-tree nil "" nil nil nil))
        key)
    (dolist (rule-list rule-lists)
      (dolist (rule rule-list)
        (setq key (car rule))
        (ignore-errors
          (when (symbolp key)
            (setq key (eval key))
            (when (stringp key)
              (setq rule (cons key (cdr rule)))))
          (unless (or (not (stringp key))
                      (string-match "\\w" key)
                      (eq (key-binding key) 'self-insert-command))
            (define-key skk-j-mode-map key 'skk-insert)))
        (when (stringp key)
          (skk-add-rule tree rule))))
    tree))

(defun skk-insert-str (str)
  "$B%P%C%U%!$K(B STR $B$rA^F~$9$k!#(B
$BI,MW$G$"$l$P(B `skk-auto-start-henkan' $B$r%3!<%k$9$k!#(B
$BI,MW$G$"$l$P(B `self-insert-after-hook' $B$r%3!<%k$9$k!#(B
`overwrite-mode' $B$G$"$l$P!"E,@Z$K>e=q$-$9$k!#(B"
  (insert-and-inherit str)
  (if (eq skk-henkan-mode 'on)
      ;; $B"&%b!<%I(B
      (when (and skk-auto-start-henkan
                 (not skk-okurigana))
        (skk-auto-start-henkan str))
    ;; $B"&%b!<%I0J30(B
    (when (and (boundp 'self-insert-after-hook)
               self-insert-after-hook)
      (funcall self-insert-after-hook
               (- (point) (length str))
               (point)))
    (when overwrite-mode
      (skk-del-char-with-pad (skk-ovwrt-len str))))
  ;; SKK 9.6 $B$G$O$3$N%?%$%_%s%0$G(B fill $B$,9T$o$l$F$$$?$,!"(BSKK 10 $B$G$O9T$o$l$F$$(B
  ;; $B$J$+$C$?!#(B
  (when (and skk-j-mode
             (not skk-henkan-mode))
    (skk-do-auto-fill)))

(defun skk-ovwrt-len (str)
  "$B>e=q$-$7$FNI$$D9$5$rJV$9!#(B"
  (min (string-width (buffer-substring-no-properties (point)
                                                     (line-end-position)))
       (string-width str)))

(defun skk-del-char-with-pad (length)
  "$BD9$5(B LENGTH $B$NJ8;z$r>C5n$9$k!#(B
$BD4@0$N$?$a!"I,MW$G$"$l$P!"KvHx$K%9%Z!<%9$rA^F~$9$k!#(B"
  (let ((p (point))
        (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (buffer-substring-no-properties (point) p))))
    (delete-region p (point))
    (unless (= length len)
      (insert-and-inherit " ")
      (backward-char 1))))

(defun skk-cancel-undo-boundary ()
  ;; skk-insert, skk-jisx0208-latin-insert $B$GO"B3$7$FF~NO$5(B
  ;; $B$l$?(B 20 $BJ8;z$r(B 1 $B2s$N%"%s%I%%$NBP>]$H$9$k!#(B`20' $B$O(B
  ;; keyboard.c $B$KDj$a$i$l$?%^%8%C%/%J%s%P!<!#(BMule-2.3 $BE:IU(B
  ;; $B$N(B egg.el $B$r;29M$K$7$?!#(B
  (cond
   ((and (< skk-self-insert-non-undo-count 20)
         (memq last-command
               '(skk-insert
                 skk-jisx0208-latin-insert
                 skk-kakutei-henkan
                 ;; SKK abbrev $B%b!<%I$G$O!"%"%9%-!<J8;zF~NO$,(B Emacs $B%*%j%8%J(B
                 ;; $B%k$N(B self-insert-command $B$K$h$j9T$o$l$F$$$k$N$G!"(B
                 ;; skk-self-insert-non-undo-count $B$r%$%s%/%j%a%s%H$9$k$3$H(B
                 ;; $B$,$G$-$J$$$N$G!"%"%s%I%%$r%(%_%e%l!<%H$G$-$J$$!#(B
                 ;; $B$7$+$b!"%+%s%^$d%T%j%*%I$rA^F~$7$?;~E@$G!"(B
                 ;; skk-abbrev-comma $B$d(B skk-abbrev-period $B$r;H$&$3$H$K$J$k$N(B
                 ;; $B$G(B (self-insert-command $B0J30$N%3%^%s%I$r;H$C$F$7$^$&$N$G(B)$B!"(B
                 ;; $B%*%j%8%J%k$N%"%s%I%%$N5!G=$bB;$J$C$F$7$^$&!#(B
                 ;; $B$7$+$78=<BLdBj$H$7$F$O!"(BSKK abbrev $B%b!<%I$O>JN,7A$H$7$F$N(B
                 ;; $B8+=P$78l$rA^F~$9$k$?$a$N%b!<%I$G$"$k$N$G!"D9$$8+=P$78l$r(B
                 ;; $BA^F~$9$k$3$H$O$"$^$j$J$/!"LdBj$b>.$5$$$H9M$($i$l$k!#(B
                 ;;skk-abbrev-comma
                 ;;skk-abbrev-period
                 self-insert-command)))
    (cancel-undo-boundary)
    (when (null skk-current-rule-tree)
      ;; $B$^$@$+$JJ8;z$,40@.$7$F$$$J$$$H$-$O!"(Bundo count $B$r%$%s%/%j%a%s%H(B
      ;; $B$7$J$$!#(B
      (setq skk-self-insert-non-undo-count
            (1+ skk-self-insert-non-undo-count))))
   (t
    (setq skk-self-insert-non-undo-count 1))))

(defun skk-set-okurigana ()
  "$B8+=P$78l$+$i(B `skk-henkan-okurigana', `skk-henkan-key' $B$N3FCM$r%;%C%H$9$k!#(B"
  (cancel-undo-boundary)
  ;;(and skk-katakana (skk-hiragana-region skk-henkan-start-point (point)))
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
   (goto-char skk-okurigana-start-point)
   (unless (eq (following-char) ?*)
     (insert-and-inherit "*")))
  (setq skk-henkan-okurigana (buffer-substring-no-properties
                              (1+ skk-okurigana-start-point)
                              (point)))
  (setq skk-henkan-key (concat (buffer-substring-no-properties
                                skk-henkan-start-point
                                skk-henkan-end-point)
                               (or (skk-okurigana-prefix
                                    (if skk-katakana
                                        (skk-katakana-to-hiragana
                                         skk-henkan-okurigana)
                                      skk-henkan-okurigana))
                                   skk-okuri-char))
        skk-prefix "")
  (when skk-katakana
    (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)
          skk-henkan-okurigana
          (skk-katakana-to-hiragana skk-henkan-okurigana)))
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil))

(defun skk-set-char-before-as-okurigana (&optional no-sokuon)
  "$B%]%$%s%H$ND>A0$NJ8;z$rAw$j2>L>$H8+Jo$7$F!"JQ49$r3+;O$9$k!#(B
$B$?$@$7!"(B $B$b$&$R$H$DA0$NJ8;z$,B%2;$@$C$?>l9g$K$O!"(B $B$=$l0J9_$rAw$j2>L>$H8+Jo$9!#(B"
  (interactive)
  (let ((pt1 (point))
        pt2 okuri sokuon)
    (setq okuri (skk-save-point
                 (backward-char 1)
                 (buffer-substring-no-properties (setq pt2 (point))
                                                 pt1)))
    (when okuri
      (unless no-sokuon
        (setq sokuon (skk-save-point
                      (backward-char 2)
                      (buffer-substring-no-properties (point)
                                                      pt2)))
        (unless (member sokuon '("$B$C(B" "$B%C(B"))
          (setq sokuon nil)))
      ;;
      (skk-save-point
       (backward-char (if sokuon 2 1))
       (skk-set-marker skk-okurigana-start-point (point)))
      (setq skk-okuri-char (skk-okurigana-prefix okuri))
      (unless skk-current-search-prog-list
        (setq skk-current-search-prog-list skk-search-prog-list))
      (skk-set-okurigana))))

;;; other inputting functions
(defun skk-toggle-kutouten ()
  "$B6gFIE@$N<oN`$r%H%0%k$GJQ99$9$k!#(B"
  (interactive)
  (setq skk-kutouten-type (cl-case skk-kutouten-type
                            (jp 'en)
                            (en 'jp-en)
                            (jp-en 'en-jp)
                            (t 'jp)))
  (when (called-interactively-p 'interactive)
    (skk-message "$BFIE@(B: `%s'  $B6gE@(B: `%s'"
                 "Touten: `%s'  Kuten: `%s'"
                 (skk-current-touten nil)
                 (skk-current-kuten nil))))

(defun skk-current-kuten (arg)      ;$B6gE@(B($B!#(B)
  ;; just ignore arg.
  (if (symbolp skk-kutouten-type)
      (cadr (assq skk-kutouten-type skk-kuten-touten-alist))
    (car skk-kutouten-type)))

(defun skk-current-touten (arg)     ;$BFIE@(B($B!"(B)
  ;; just ignore arg.
  (if (symbolp skk-kutouten-type)
      (cdr (cdr (assq skk-kutouten-type skk-kuten-touten-alist)))
    (cdr skk-kutouten-type)))

(defun skk-auto-kutouten (arg)
  (let ((output (cdr (assq last-command-event
                           (list (list ?- "-" "$B!](B" "$B!<(B")
                                 (list ?, "," "$B!$(B" (skk-current-touten nil))
                                 (list ?. "." "$B!%(B" (skk-current-kuten nil))))))
        (chr (char-before (point))))
    (cond ((null chr)           ;point-min $BBP:v(B
           (nth 2 output))
          ((and (<= ?0 chr) (>= ?9 chr) skk-use-auto-kutouten)
           (nth 0 output))
          ((and (<= ?$B#0(B chr) (>= ?$B#9(B chr) skk-use-auto-kutouten)
           (nth 1 output))
          (t
           (nth 2 output)))))

(defun skk-abbrev-insert (arg)
  (interactive "*p")
  (self-insert-command arg))

(defun skk-abbrev-period (arg)
  "SKK abbrev $B%b!<%I$G8+=P$78l$NJd40Cf$G$"$l$P!"<!$N8uJd$rI=<($9$k!#(B
$BJd40$ND>8e$G$J$1$l$P!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-comp-do)
       (progn
         (setq this-command 'skk-comp-do)
         (skk-comp-do nil))
     (skk-emulate-original-map arg))))

(defun skk-abbrev-comma (arg)
  "SKK abbrev $B%b!<%I$G8+=P$78l$NJd40Cf$G$"$l$P!"D>A0$N8uJd$rI=<($9$k!#(B
$BJd40$ND>8e$G$J$1$l$P!"%*%j%8%J%k$N%-!<3d$jIU$1$N%3%^%s%I$r%(%_%e%l!<%H$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (if (eq last-command 'skk-comp-do)
       (progn
         (setq this-command 'skk-comp-do)
         (skk-comp-previous))
     (skk-emulate-original-map arg))))

(defun skk-jisx0208-latin-insert (arg)
  "$BA41QJ8;z$r%+%l%s%H%P%C%U%!$KA^F~$9$k!#(B
`skk-jisx0208-latin-vector' $B$r%F!<%V%k$H$7$F!":G8e$KF~NO$5$l$?%-!<$KBP1~$9$k(B
$BJ8;z$rA^F~$9$k!#(B
`skk-auto-insert-paren' $B$NCM$,(B non-nil $B$N>l9g$G!"(B`skk-auto-paren-string-alist'
$B$KBP1~$9$kJ8;zNs$,$"$k$H$-$O!"$=$NBP1~$9$kJ8;zNs(B ($B$+$C$3N`(B) $B$r<+F0E*$KA^F~$9(B
$B$k!#(B"
  (interactive "p")
  (barf-if-buffer-read-only)
  (skk-with-point-move
   (let* ((str (aref skk-jisx0208-latin-vector last-command-event))
          (arg2 arg)
          (pair-str (if skk-auto-insert-paren
                        (cdr (assoc str skk-auto-paren-string-alist))
                      nil))
          (pair-str-inserted 0)
          region-str)
     (if (not str)
         (skk-emulate-original-map arg)
       (skk-cancel-undo-boundary)
       (when (and (use-region-p)
                  skk-use-auto-enclose-pair-of-region
                  pair-str)
         (setq region-str (buffer-substring (region-beginning)
                                            (region-end)))
         (delete-region (region-beginning) (region-end)))
       (while (> arg 0)
         (skk-insert-str str)
         (setq arg (1- arg)))
       (when pair-str
         (when region-str
           (insert region-str))
         (while (> arg2 0)
           (unless (string= pair-str (char-to-string (following-char)))
             (setq pair-str-inserted (1+ pair-str-inserted))
             (skk-insert-str pair-str))
           (setq arg2 (1- arg2)))
         (unless (zerop pair-str-inserted)
           (backward-char pair-str-inserted))
         (when region-str
           (skip-chars-forward pair-str)))))))

(defun skk-delete-backward-char (arg)
  "$B"'%b!<%I$G(B `skk-delete-implies-kakutei' $B$J$iD>A0$NJ8;z$r>C$7$F3NDj$9$k!#(B
$B"'%b!<%I$G(B `skk-delete-implies-kakutei' $B$,(B nil $B$@$C$?$iA08uJd$rI=<($9$k!#(B
$B"&%b!<%I$G(B`$B"&(B'$B$h$j$bA0$N%]%$%s%H$G<B9T$9$k$H3NDj$9$k!#(B
$B3NDjF~NO%b!<%I$G!"$+$J%W%l%U%#%C%/%9$NF~NOCf$J$i$P!"$+$J%W%l%U%#%C%/%9$r>C$9!#(B"
  (interactive "*P")
  (skk-with-point-move
   (let ((count (prefix-numeric-value arg)))
     (cond
      ((eq skk-henkan-mode 'active)
       (if (and (not skk-delete-implies-kakutei)
                (= (+ skk-henkan-end-point (length skk-henkan-okurigana))
                   (point)))
           (skk-previous-candidate)
         ;; overwrite-mode $B$G!"%]%$%s%H$,A43QJ8;z$K0O$^$l$F$$$k$H(B
         ;; $B$-$K(B delete-backward-char $B$r;H$&$H!"A43QJ8;z$O>C$9$,H>(B
         ;; $B3QJ8;zJ,$7$+(B backward $BJ}8~$K%]%$%s%H$,La$i$J$$(B (Emacs
         ;; 19.31 $B$K$F3NG'(B)$B!#JQ49Cf$N8uJd$KBP$7$F$O(B
         ;; delete-backward-char $B$GI,$:A43QJ8;z(B 1 $BJ8;zJ,(B backward
         ;; $BJ}8~$KLa$C$?J}$,NI$$!#(B
         (if overwrite-mode
             (progn
               (backward-char count)
               (delete-char count arg))
           (skk-emulate-original-map arg))
         ;; XXX assume skk-prefix has no multibyte chars.
         (if (> (length skk-prefix) count)
             (setq skk-prefix (substring skk-prefix
                                         0 (- (length skk-prefix) count)))
           (setq skk-prefix ""))
         (when (>= skk-henkan-end-point (point))
           (if (eq skk-delete-implies-kakutei 'dont-update)
               (let ((skk-update-jisyo-function #'ignore))
                 (skk-kakutei))
             (skk-kakutei)))))
      ((and skk-henkan-mode
            (>= skk-henkan-start-point (point))
            (not (skk-get-prefix skk-current-rule-tree)))
       (setq skk-henkan-count 0)
       (skk-kakutei))
      ;; $BF~NOCf$N8+=P$78l$KBP$7$F$O(B delete-backward-char $B$G(B
      ;; $BI,$:A43QJ8;z(B 1$BJ8;zJ,(B backward $BJ}8~$KLa$C$?J}$,NI$$!#(B
      ((and skk-henkan-mode overwrite-mode)
       (backward-char count)
       (delete-char count arg))
      (t
       (skk-delete-okuri-mark)
       (if (skk-get-prefix skk-current-rule-tree)
           (skk-erase-prefix 'clean)
         (skk-set-marker skk-kana-start-point nil)
         (skk-emulate-original-map arg)))))))

;;; henkan routines
(defun skk-henkan (&optional prog-list-number)
  "$B%+%J$r4A;zJQ49$9$k%a%$%s%k!<%A%s!#(B"
  (let (mark prototype new-word pair kakutei-henkan)
    (if (string= skk-henkan-key "")
        (skk-kakutei)
      ;; we use mark to go back to the correct position after henkan
      (unless (eobp)
        (setq mark (skk-save-point (forward-char 1)
                                   (point-marker))))

      (unless (eq skk-henkan-mode 'active) ;$B"'%b!<%I0J30$J$i!#$D$^$j0lH/L\$r4^$`(B
        (skk-change-marker)
        (setq skk-current-search-prog-list
              (cond ((and (integerp prog-list-number)
                          (<= 0 prog-list-number)
                          (<= prog-list-number 9))
                     (or (symbol-value (intern (format "skk-search-prog-list-%d"
                                                       prog-list-number)))
                         skk-search-prog-list))
                    (t
                     skk-search-prog-list))))

      ;; skk-henkan-1 $B$NCf$+$i%3!<%k$5$l$k(B skk-henkan-show-candidates
      ;; $B$+$i(B throw $B$5$l$k!#$3$3$G%-%c%C%A$7$?>l9g$O!"(B?x $B$,%9%H%j!<%`$K(B
      ;; $BLa$5$l$F$$$k$N$G!"$3$N4X?t$r=P$F!"(Bskk-previous-candidate $B$X$f$/!#(B
      (catch 'unread
        (cond ((setq prototype (skk-henkan-1))
               (setq new-word prototype))
              ((setq prototype (skk-henkan-in-minibuff))
               (setq new-word (skk-quote-semicolon prototype))))
        (setq kakutei-henkan skk-kakutei-flag)
        (when new-word
          (setq pair (skk-insert-new-word new-word))))

      (skk-delete-overlay skk-inline-overlays)
      (if mark
          (progn
            (goto-char mark)
            ;; $B;2>H$5$l$F$$$J$$%^!<%+!<$O!"(BGarbage Collection $B$,%3!<%k$5$l$?$H(B
            ;; $B$-$K2s<}$5$l$k$,!"$=$l$^$G$N4V!"%F%-%9%H$N$I$3$+$r;X$7$F$$$k$H!"(B
            ;; $B%F%-%9%H$N%"%C%W%G!<%H$N:]$K$=$N%^!<%+!<CM$r99?7$9$kI,MW$,$"$k(B
            ;; $B$N$G!"$I$3$b;X$5$J$$$h$&$K$9$k!#(B
            (skk-set-marker mark nil)
            (backward-char 1))
        (goto-char (point-max)))

      (when (and skk-show-annotation
                 (not kakutei-henkan))
        (skk-annotation-find-and-show pair)))))

(defun skk-exit-henkan-in-minibuff ()
  (when (and (numberp skk-henkan-in-minibuff-nest-level)
             (= (1- (minibuffer-depth)) skk-henkan-in-minibuff-nest-level))
    (setq skk-henkan-in-minibuff-nest-level nil)))

(defun skk-henkan-1 ()
  "`skk-henkan' $B$N%5%V%k!<%A%s!#(B"
  (let (new-word)
    (cl-case skk-henkan-count
      ;; $B0lH/L\$N(B SPC $BBG80!J$D$^$j"&"*(BSPC$B"*"'$N$H$-!K(B
      (0
       (let ((prog-list-length (when (numberp skk-kakutei-search-prog-limit)
                                 (length skk-current-search-prog-list))))
         (while (and skk-current-search-prog-list
                     (not new-word))
           (setq skk-henkan-list (skk-nunion skk-henkan-list
                                             (skk-search)))
           (skk-henkan-list-filter)
           (setq new-word (skk-get-current-candidate)))
         ;; $BJQ498uJd$,0l$D$7$+L5$$;~$N3NDjJQ49MQ%A%'%C%/(B
         (when (and (or (eq skk-kakutei-when-unique-candidate t)
                        (cond (skk-abbrev-mode
                               (and (listp skk-kakutei-when-unique-candidate)
                                    (memq 'abbrev
                                          skk-kakutei-when-unique-candidate)))
                              (skk-henkan-okurigana
                               (and (listp skk-kakutei-when-unique-candidate)
                                    (memq 'okuri-ari
                                          skk-kakutei-when-unique-candidate)))
                              (t
                               ;; $BAw$jL5$7$N$_$rFCJL07$$$7$F$$$?8E$$;EMM$KBP1~(B
                               (or (eq 'okuri-nasi
                                       skk-kakutei-when-unique-candidate)
                                   (memq 'okuri-nasi
                                         skk-kakutei-when-unique-candidate)))))
                    (= (length skk-henkan-list) 1)
                    (not skk-undo-kakutei-flag))
           (while (and skk-current-search-prog-list
                       (or (not (numberp skk-kakutei-search-prog-limit))
                           (< (- prog-list-length
                                 (length skk-current-search-prog-list))
                              skk-kakutei-search-prog-limit))
                       (<= (length skk-henkan-list) 1))
             (setq skk-henkan-list (skk-nunion skk-henkan-list
                                               (skk-search)))
             (skk-henkan-list-filter))
           (when (and (= (length skk-henkan-list) 1)
                      (or (not (numberp skk-kakutei-search-prog-limit))
                          (<= (- prog-list-length
                                 (length skk-current-search-prog-list))
                              skk-kakutei-search-prog-limit)))
             (setq skk-kakutei-henkan-flag t)))
         ;; skk-henkan-list-filter $B$rDL$7$?8e$OG0$N0Y$K:F<hF@(B
         (setq new-word (skk-get-current-candidate))
         (when (and new-word
                    (not skk-undo-kakutei-flag)
                    skk-kakutei-henkan-flag)
           ;; found the unique candidate in kakutei jisyo
           (setq this-command 'skk-kakutei-henkan
                 skk-kakutei-flag t))))

      ;; $BFsH/L\0J9_$N(B SPC $BBG80!J$D$^$j"'$GO"B3(B SPC $BBG80!K(B
      (t
       (setq new-word (skk-get-current-candidate))
       (unless new-word
         ;; $B?7$7$$8uJd$r8+$D$1$k$+!"(Bskk-current-search-prog-list $B$,6u$K$J(B
         ;; $B$k$^$G(B skk-search $B$rO"B3$7$F%3!<%k$9$k!#(B
         (while (and skk-current-search-prog-list
                     (not new-word))
           (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search)))
           (skk-henkan-list-filter)
           (setq new-word (skk-get-current-candidate))))
       (when (and new-word
                  (> skk-henkan-count (- skk-show-candidates-nth-henkan-char 2) ))
         ;; show candidates in minibuffer
         (setq new-word (skk-henkan-show-candidates)))))
    new-word))

(defun skk-get-current-candidate (&optional noconv)
  (let ((candidate (skk-get-current-candidate-1)))
    (cond ((not (and (skk-numeric-p)
                     (consp candidate)))
           candidate)
          (noconv
           (car candidate))
          (t
           (cdr candidate)))))

(defun skk-henkan-list-filter ()
  (when (skk-numeric-p)
    (skk-num-uniq)
    (skk-num-multiple-convert))
  (when (and (featurep 'jisx0213)   ;Mule-UCS
             skk-jisx0213-prohibit)
    (skk-jisx0213-henkan-list-filter)))

(defun skk-henkan-show-candidates ()
  "$BJQ49$7$?8uJd72$r%(%3!<%(%j%"$KI=<($9$k!#(B"
  (skk-save-point
   (let* ((max-candidates skk-henkan-number-to-display-candidates)
          (candidate-keys ; $BI=<(MQ$N%-!<%j%9%H(B
           (mapcar (lambda (c)
                     (when (or (memq c '(?\C-g skk-start-henkan-char))
                               (skk-key-binding-member
                                (skk-char-to-unibyte-string c)
                                '(skk-previous-candidate)))
                       (skk-error "`%s' $B$KL58z$J%-!<$,;XDj$5$l$F$$$^$9(B"
                                  "Illegal key in `%s'"
                                  "skk-henkan-show-candidates-keys"))
                     (skk-char-to-unibyte-string (upcase c)))
                   skk-henkan-show-candidates-keys))
          key-num-alist ; $B8uJdA*BrMQ$NO"A[%j%9%H(B
          (key-num-alist1 ; key-num-alist $B$rAH$_N)$F$k$?$a$N:n6HMQO"A[%j%9%H!#(B
           ;; $B5U$5$^$K$7$F$*$$$F!"I=<($9$k8uJd$N?t$,>/$J$+$C$?$i@h(B
           ;; $BF,$+$i4v$D$+:o$k!#(B
           (let (alist)
             (dotimes (i max-candidates alist)
               (push (cons (nth i skk-henkan-show-candidates-keys)
                           i)
                     alist))))
          (nth-henkan (- skk-show-candidates-nth-henkan-char 1))
          (loop 0)
          inhibit-quit
          (echo-keystrokes 0)
          henkan-list
          new-one
          reverse
          n)
     ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
     ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
     (when skk-use-face
       (skk-henkan-face-off))
     (delete-region skk-henkan-start-point
                    skk-henkan-end-point)
     (while loop
       (cond
        (reverse
         (setq loop (1- loop)
               henkan-list (nthcdr (+ nth-henkan (* loop max-candidates))
                                   skk-henkan-list)
               reverse nil))

        (skk-exit-show-candidates
         ;; $B8uJd$,?T$-$F$7$^$C$F!"(Bskk-henkan-show-candidates ->
         ;; skk-henkan-in-minibuff -> skk-henkan
         ;; -> skk-henkan-show-candidates $B$N=g$G!":F$S$3$N4X?t$,8F$P$l(B
         ;; $B$?$H$-$O!"$3$3$G(B henkan-list $B$H(B loop $B$r7W;;$9$k!#(B
         (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
               loop (car skk-exit-show-candidates))
         (setq skk-exit-show-candidates nil))

        (t
         ;; skk-henkan-show-candidates-keys $B$N:G=*$N%-!<$KBP1~$9$k8uJd(B
         ;; $B$,=P$F$/$k$^$G%5!<%A$rB3$1$k!#(B
         (while (and skk-current-search-prog-list
                     (null (nthcdr (+ nth-henkan max-candidates (* loop max-candidates))
                                   skk-henkan-list)))
           ;; $B?75,$K8uJd$,F@$i$l$?;~$N$_(B skk-henkan-list-filter $B$r8F$V!#(B
           ;; skk-look $B$d(B skk-server-completion-search $B$rMxMQ$7$?M9JXHV9f$+$i$N(B
           ;; $BJQ49;~$J$I$N$h$&$K!"?tCM$r4^$_3n$DBgNL$N8uJd$,$"$k;~$NB.EY2~A1!#(B
           (let ((cands (skk-search)))
             (when cands
               (setq skk-henkan-list
                     (skk-nunion skk-henkan-list
                                 cands))
               (skk-henkan-list-filter))))
         (setq henkan-list (nthcdr (+ nth-henkan (* loop max-candidates))
                                   skk-henkan-list))))
       (save-window-excursion
         (setq n (skk-henkan-show-candidate-subr candidate-keys henkan-list))
         (when (> n 0)
           (condition-case nil
               (let* ((event (read-event))
                      (char (event-to-character event))
                      (key (skk-event-key event))
                      num)
                 ;; clear out candidates in echo area
                 (message "")
                 (if (and (null char)
                          (null key))
                     (skk-unread-event event)
                   (setq key-num-alist (nthcdr (- max-candidates n)
                                               key-num-alist1))
                   (when (and key-num-alist
                              char)
                     (setq num (cdr (or (assq char
                                              key-num-alist)
                                        (assq (if (skk-lower-case-p char)
                                                  (upcase char)
                                                (downcase char))
                                              key-num-alist)))))
                   (cond
                    (num
                     (setq skk-henkan-count (+ nth-henkan (* loop max-candidates) num))
                     (setq new-one (nth num henkan-list)
                           skk-kakutei-flag t
                           loop nil))

                    ((or (eq char skk-start-henkan-char) ; SPC
                         (skk-key-binding-member key
                                                 '(skk-nicola-self-insert-rshift)
                                                 skk-j-mode-map))
                     ;;
                     (if (or skk-current-search-prog-list
                             (nthcdr max-candidates henkan-list))
                         (setq loop (1+ loop))
                       ;; $B8uJd$,?T$-$?!#$3$N4X?t$+$iH4$1$k!#(B
                       (let ((last-showed-index (+ nth-henkan (* loop max-candidates))))
                         (setq skk-exit-show-candidates
                               ;; cdr $BIt$O!"<-=qEPO?$KF~$kA0$K:G8e$KI=<($7(B
                               ;; $B$?8uJd72$NCf$G:G=i$N8uJd$r;X$9%$%s%G%/%9(B
                               (cons loop last-showed-index))
                         ;; $B<-=qEPO?$KF~$k!#(Bskk-henkan-count $B$O(B
                         ;; skk-henkan-list $B$N:G8e$N8uJd$N<!(B ($BB8:_$7$J$$(B
                         ;; --- nil)$B$r;X$9!#(B
                         (setq skk-henkan-count (+ last-showed-index n))
                         (setq loop nil))))

                    ((eq char skk-force-registration-mode-char)
                     (let ((last-showed-index (+ nth-henkan (* loop max-candidates))))
                       (setq skk-exit-show-candidates
                             ;; cdr $BIt$O!"<-=qEPO?$KF~$kA0$K:G8e$KI=<($7(B
                             ;; $B$?8uJd72$NCf$G:G=i$N8uJd$r;X$9%$%s%G%/%9(B
                             (cons loop last-showed-index))
                       (setq skk-henkan-count last-showed-index)
                       (setq loop nil)))

                    ((eq char skk-show-candidates-toggle-display-place-char)
                     (setq skk-show-candidates-always-pop-to-buffer
                           (not skk-show-candidates-always-pop-to-buffer)))

                    ((skk-key-binding-member key
                                             '(skk-previous-candidate
                                               skk-delete-backward-char
                                               skk-undo)
                                             skk-j-mode-map)
                     (cl-case loop
                       (0
                        ;; skk-henkan-show-candidates $B$r8F$VA0$N(B
                        ;; $B>uBV$KLa$9!#(B
                        (skk-escape-from-show-candidates nth-henkan))
                       (t
                        ;; $B0l$DA0$N8uJd72$r%(%3!<%(%j%"$KI=<($9$k!#(B
                        (setq reverse t))))

                    ((eq char skk-annotation-toggle-display-char)
                     (skk-annotation-toggle-display-p))

                    ((skk-key-binding-member key skk-quit-commands
                                             skk-j-mode-map)
                     ;;
                     (signal 'quit nil))

                    (t
                     (skk-message "`%s' $B$OL58z$J%-!<$G$9!*(B"
                                  "`%s' is not valid here!"
                                  (or (key-description key)
                                      (key-description char)))
                     (sit-for 1)))))
             (quit
              ;; skk-previous-candidate $B$X(B
              (skk-escape-from-show-candidates 0)))))) ; end of while loop
     ;;
     (or (cdr-safe new-one)
         new-one))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  "$B8uJd72$rI=<($9$k4X?t!#(B
KEYS $B$H(B CANDIDATES $B$rAH$_9g$o$;$F#7$NG\?t8D$N8uJd72(B ($B8uJd?t$,(B
$BK~$?$J$+$C$?$i$=$3$GBG$A@Z$k(B) $B$NJ8;zNs$r:n$j!"%$%s%i%$%s!"%D!<%k%F%#%C%W!"(B
$B%(%3!<%(%j%"Kt$O8uJd%P%C%U%!$KI=<($9$k!#(B"
  (let* ((max-candidates skk-henkan-number-to-display-candidates)
         (workinglst (skk-henkan-candidate-list candidates max-candidates))
         str tooltip-str message-log-max)
    (when workinglst
      (dotimes (i (length workinglst))
        (let ((cand (if (consp (nth i workinglst))
                        (cdr (nth i workinglst))
                      (nth i workinglst)))
              (key (concat (propertize (nth i keys) 'face
                                       'skk-henkan-show-candidates-keys-face)
                           ":")))
          (when (and (zerop (% i skk-henkan-number-to-display-candidates)) ; $B3FNs$N:G=i$N8uJd(B
                     (not (zerop i)))
            (setq str (concat str "\n")))
          (setq str (concat str
                            (if (zerop (% i skk-henkan-number-to-display-candidates)) "" "  ")
                            key cand)
                tooltip-str (concat tooltip-str key cand "\n"))))
      (setq str (concat str (propertize
                             (format "  [$B;D$j(B %d%s]"
                                     (- (length candidates)
                                        (length workinglst))
                                     (make-string
                                      (length skk-current-search-prog-list)
                                      ?+))
                             'face 'skk-henkan-rest-indicator-face))
            tooltip-str (concat tooltip-str
                                (format "[$B;D$j(B %d%s]"
                                        (- (length candidates)
                                           (length workinglst))
                                        (make-string
                                         (length skk-current-search-prog-list)
                                         ?+))))
      (cond
       ;; (1) $B8=:_$N%P%C%U%!$NCf$KI=<($9$k(B ($B%$%s%i%$%sI=<((B)
       ((and skk-show-inline
             (not skk-isearch-switch)
             (not (skk-in-minibuffer-p)))
        (skk-inline-show str skk-inline-show-face tooltip-str max-candidates))

       ;; (2) tooptip $B$GI=<($9$k(B
       ((and window-system
             skk-show-tooltip
             (not (eq (symbol-function 'skk-tooltip-show-at-point) 'ignore)))
        (when (and skk-henkan-rest-indicator)
          (let* ((body (substring tooltip-str 0 (string-match "\\[$B;D$j(B" tooltip-str)))
                 (rest (substring tooltip-str (- (length body) (length tooltip-str)))))
            (setq tooltip-str (concat body
                                      (make-string (- (car (skk-tooltip-max-tooltip-size))
                                                      (string-width rest) 3)
                                                   ? )
                                      rest))))
        (funcall skk-tooltip-function tooltip-str))

       ;; (3) $B%(%3!<%(%j%"$r;H$&(B
       ((and (not skk-show-candidates-always-pop-to-buffer)
             (> (frame-width) (skk-max-string-width (split-string str "\n"))))
        (when skk-henkan-rest-indicator
          (let* ((body (substring str 0 (string-match "  \\[$B;D$j(B" str)))
                 (rest (substring str (- (length body) (length str)))))
            (setq str (concat body
                              (make-string (- (frame-width)
                                              (string-width str) 1) ? )
                              rest))))
        (message "%s" str))

       ;; (4) $B0l;~%P%C%U%!$r(B pop up $B$7$F;H$&(B
       (t
        (skk-henkan-show-candidates-buffer str keys))))
    ;; $BI=<($9$k8uJd?t$rJV$9!#(B
    (length workinglst)))

(defun skk-check-treat-candidate-appearance-function ()
  (when (or (eq skk-annotation-lookup-lookup 'always)
            (eq skk-annotation-lookup-DictionaryServices 'always))
    ;; Apple OS X $B$N<-=q%5!<%S%9$r8uJd0lMw$G$b;H$&>l9g$O!"(B
    ;; `skk-treat-candidate-appearance-function' $B$rD4@a$9$kI,MW$"$j(B
    (setq skk-treat-candidate-appearance-function
          'skk-treat-candidate-sample2))
  ;;
  (functionp skk-treat-candidate-appearance-function))

(defun skk-treat-candidate-appearance (candidate listing-p)
  (funcall skk-treat-candidate-appearance-function candidate listing-p))

(defun skk-henkan-candidate-list (candidates max)
  ;; CANDIDATES $B$N@hF,$N(B max $B8D$N$_$N%j%9%H$rJV$9!#(B
  (let ((count 0) e sep note v)
    (while (> max count)
      (setq e (nth count candidates))
      (setq sep  nil
            note nil)
      (when (and (skk-numeric-p) (consp e))
        (setq e (cdr e)))
      (cond
       (e
        ;; $B$^$@8uJd$,;D$C$F$$$k>l9g(B
        (when (skk-check-treat-candidate-appearance-function)
          ;; skk-treat-candidate-appearance-function $B$K$h$C$F%f!<%6$O(B
          ;; $BG$0U$K8uJdJ8;zNs$HCm<aJ8;zNs$r2C9)!&=$>~$9$k$3$H$,$G$-$k!#(B
          ;; $B%f!<%6$,JV$9CM$O(B cons cell $B$^$?$OJ8;zNs$H$J$k!#(B
          (let ((value (save-match-data
                         ;; $B8uJd0lMwI=<($N:]$O(B
                         ;; skk-treat-candidate-appearance-function $B$N(B
                         ;; $BBh(B 2 $B0z?t$r(B non-nil $B$H$9$k!#(B
                         (skk-treat-candidate-appearance e 'list))))
            (cond
             ((consp value)
              ;; $BJV$jCM$,(B cons cell $B$@$C$?>l9g(B
              (setq e (car value))
              (cond
               ((consp (cdr value))
                ;; ($B8uJd(B . ($B%;%Q%l!<%?(B . $BCm<a(B))
                ;; $BCm<a$O4{$K%;%Q%l!<%?H4$-(B
                (setq sep  (cadr value)
                      note (cddr value)))
               ((string-match "^;" (cdr value))
                ;; ($B8uJd(B . $BCm<a(B)
                ;; $BCm<a$O$^$@%;%Q%l!<%?$r4^$s$G$$$k(B
                (setq sep  (substring (cdr value) 0 1)
                      note (substring (cdr value) 1)))
               (t
                ;; ($B8uJd(B . $BCm<a(B)
                ;; $BCm<a$O4{$K%;%Q%l!<%?$r=|5n$7$F$$$k$b$N$H(B
                ;; $BH=CG$9$k(B
                (setq sep  ";"
                      note (cdr value)))))
             (t
              ;; $BJV$jCM$,J8;zNs$@$C$?>l9g(B
              (setq e    value
                    note nil)))))
        ;; $B%f!<%6$,Cm<aI=<($r2C9):Q$_$N>l9g$O$b$&Cm<a$N=hM}$O$7$J$$!#(B
        (when (and (not (stringp note))
                   (string-match ";" e))
          (setq sep  (substring e (match-beginning 0) (match-end 0))
                note (substring e (match-end 0))
                e    (substring e 0 (match-beginning 0))))
        ;; $B%f!<%6$,K>$`Cm<a$NI=<(7A<0$K1h$C$FCm<a$r2C9)$9$k!#(B
        (cond ((not skk-show-annotation)
               ;; $B!VI=<($7$J$$!W(B
               (setq sep  nil
                     note nil))
              ((skk-annotation-display-p 'list)
               ;; $B!VI=<($9$k!W(B
               ;; $B$=$N$^$^(B
               )
              (t
               ;; $B!V8uJd0lMw$G$OI=<($7$J$$!W(B
               ;; annotation $B$NB8:_$@$1$rCN$i$;$k!#(B
               (setq note nil)))
        ;; $B8uJd0lMwI=<($G$O8uJd$HCm<a$r0l3g$7$FI=<($9$k$N$G(B
        ;; $B$3$3$G7k9g$7$F$*$/!#(B
        (setq e (concat (skk-eval-string e)
                        sep
                        (when note
                          (skk-eval-string (skk-annotation-get note)))))
        ;; $BA4$F$N2C9)=hM}=*$o$j!#JQ?t$K%;%C%H$9$k!#(B
        (setq v     (cons e v)
              count (1+ count)))
       (t
        ;; $B8uJd$,?T$-$?>l9g(B
        (setq count max))))
    ;; $B8uJd$r=P8==g$K%=!<%H$7D>$7$FJV$9!#(B
    (nreverse v)))

(defun skk-henkan-show-candidates-buffer (str keys)
  ;; $B%(%3!<%(%j%"$NBe$o$j$K0l;~%P%C%U%!$r(B pop up $B$7$F8uJd0lMw$rI=<($9$k!#(B
  (let ((buff (get-buffer-create "*$B8uJd(B*"))
        (case-fold-search t))
    (with-current-buffer buff
      (erase-buffer)
      (insert (if skk-henkan-show-candidates-buffer-decor
                  (funcall skk-henkan-show-candidates-buffer-decor str)
                str))
      (goto-char (point-min))
      ;; 1 $B8uJd$K(B 1 $B9T$r3d$jEv$F$k!#(B
      (forward-char 2)
      (while (re-search-forward
              (concat "  "
                      (mapconcat 'identity keys ":\\|  ")
                      ":\\|"
                      "  \\[$B;D$j(B [0-9]+\\(\\++\\)?\\]") nil t)
        (goto-char (match-beginning 0))
        (delete-char 2)
        (insert "\n"))
      (goto-char (point-min))

      ;; $B1&C<$KM>Gr$r@_$1$k(B
      (while (and (move-to-column (- (frame-width) 2))
                  (not (eobp))
                  (>= (frame-width) (current-column)))
        (unless (eolp)
          ;; $B6XB'=hM}(B
          (unless (member (char-to-string (char-before))
                          skk-auto-start-henkan-keyword-list)
            (backward-char 1))
          (insert "\n  ")
          (forward-line -1))
        (forward-line 1))

      ;; [$B;D$j(B 99++] $B$r1&C<$X(B
      (when skk-henkan-rest-indicator
        (let ((col (progn (goto-char (point-max))
                          (skk-screen-column))))
          (beginning-of-line)
          (insert-char 32 (- (frame-width) col 1))))

      ;;$B8uJd%P%C%U%!$NGX7J?'(B
      (when skk-candidate-buffer-background-color
        (unless skk-candidate-buffer-background-color-odd
          (setq skk-candidate-buffer-background-color-odd
                skk-candidate-buffer-background-color))
        (goto-char (point-min))
        (let ((background-color skk-candidate-buffer-background-color))
          (while (null (eobp))
            (let* ((eol (save-excursion (end-of-line)
                                        (unless (eobp) (forward-char))
                                        (point)))
                   (bol (progn (beginning-of-line)
                               (point)))
                   (ovl (make-overlay bol eol)))
              (when (or (string-match "[ASDFJKL]:" (buffer-substring bol (+ 2 bol)))
                        (string-match "\\[$B;D$j(B [0-9]+\\(\\++\\)?\\]"
                                      (buffer-substring bol eol)))
                (setq background-color
                      (if (string= background-color skk-candidate-buffer-background-color)
                          skk-candidate-buffer-background-color-odd
                        skk-candidate-buffer-background-color)))
              (overlay-put ovl 'face `(:background ,background-color :extend t)))
            (forward-line))))

      (goto-char (point-min)))

    (let ((minibuf-p (skk-in-minibuffer-p))
          (window (get-buffer-window (skk-minibuffer-origin))))
      (when minibuf-p
        (if window
            (select-window window)
          (other-window 1)))
      (let ((w (selected-window)))
        (pop-to-buffer buff)
        (fit-window-to-buffer)
        (select-window w))
      (when minibuf-p
        (select-window (minibuffer-window))))))

;;;###autoload
(defun skk-henkan-in-minibuff ()
  "$B<-=qEPO?%b!<%I$KF~$j!"EPO?$7$?C18l$NJ8;zNs$rJV$9!#(B"
  (unless (numberp skk-henkan-in-minibuff-nest-level)
    (setq skk-henkan-in-minibuff-nest-level (minibuffer-depth)))
  (when (and window-system skk-show-tooltip)
    (tooltip-hide))
  (when skk-show-inline
    (skk-inline-show "$B"-<-=qEPO?Cf"-(B" 'skk-jisyo-registration-badge-face))
  (save-match-data
    (let ((enable-recursive-minibuffers t)
          (depth (- (1+ (minibuffer-depth)) skk-henkan-in-minibuff-nest-level))
          ;; XEmacs $B$G$O<!$NJQ?t$,:F5"E*%_%K%P%C%U%!$N2DH]$K1F6A$9$k!#(B
          minibuffer-max-depth
          ;; From skk-henkan()
          ;; we use mark to go back to the correct position after henkan
          (mark (unless (eobp)
                  (skk-save-point
                   (forward-char 1)
                   (point-marker))))
          skk-isearch-message   ; $BJQ49Cf$K(B isearch message $B$,=P$J$$$h$&$K$9$k(B
          orglen new-one pair)
      (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
      (add-hook 'minibuffer-setup-hook 'skk-add-skk-pre-command)
      (save-window-excursion
        (skk-show-num-type-info)
        (condition-case nil
            (setq new-one (read-from-minibuffer
                           (format "%s$B<-=qEPO?(B%s %s: "
                                   (make-string depth ?\[)
                                   (make-string depth ?\])
                                   (or (and (skk-numeric-p)
                                            (skk-num-henkan-key))
                                       (if skk-okuri-char
                                           (skk-compute-henkan-key2)
                                         skk-henkan-key)))
                           (when (and (not skk-okuri-char)
                                      skk-read-from-minibuffer-function)
                             (funcall skk-read-from-minibuffer-function))))
          (quit
           (skk-delete-overlay skk-inline-overlays)
           (setq new-one ""))))
      (when (and skk-check-okurigana-on-touroku
                 ;; $BAw$j$"$jJQ49$G$b(B skk-okuri-char $B$@$1$@$HH=CG$G$-$J$$!#(B
                 skk-henkan-okurigana new-one)
        (setq new-one (skk-remove-redundant-okurigana new-one)))
      (cond
       ((string= new-one "")
        (skk-delete-overlay skk-inline-overlays)
        (if skk-exit-show-candidates
            ;; $B%(%3!<%(%j%"$KI=<($7$?8uJd$,?T$-$F<-=qEPO?$KF~$C$?$,!"6uJ8;z(B
            ;; $BNs$,EPO?$5$l$?>l9g!#:G8e$K%(%3!<%(%j%"$KI=<($7$?8uJd72$r:FI=(B
            ;; $B<($9$k!#(B
            (progn
              (setq skk-henkan-count (cdr skk-exit-show-candidates))
              (skk-henkan))
          ;; skk-henkan-show-candidates $B$KF~$kA0$K8uJd$,?T$-$?>l9g(B
          (setq skk-henkan-count (1- skk-henkan-count))
          (if (= skk-henkan-count -1)
              ;; $BAw$j$"$j$NJQ49$G<-=qEPO?$KF~$j!"6uJ8;z$rEPO?$7$?8e!"$=$N(B
              ;; $B$^$^:FEYAw$j$J$7$H$7$FJQ49$7$?>l9g$O(B
              ;; skk-henkan-okurigana, skk-okuri-char $B$NCM$r(B nil $B$K$7$J$1(B
              ;; $B$l$P!"$=$l$>$l$NCM$K8E$$Aw$j2>L>$,F~$C$?$^$^$G8!:w$K<:GT(B
              ;; $B$9$k!#(B
              (progn
                (setq skk-henkan-okurigana nil
                      skk-okurigana nil
                      skk-okuri-char nil)
                (skk-change-marker-to-white))
            ;; $B<-=qEPO?$KF~$kD>A0$N8uJd$KCm<a$,$"$k2DG=@-$r9M$(!":FI=<($9$k!#(B
            ;;   skk-insert-new-word(), skk-henkan-candidate-list() $BFb$N(B
            ;;   $BCm<a2C9)=hM}$rFHN)$7$?4X?t$K$7$F!"(B
            ;;   $B$=$l$rMxMQ$9$k$h$&$K$7$?$[$&$,NI$5$=$&!#(B
            (setq pair (skk-insert-new-word (skk-get-current-candidate)))
            ;; From skk-henkan()
            ;; $BAw$j$"$jJQ49$N:]$N(B point $B$N0LCV$r!"<-=qEPO?%b!<%I$KF~$kA0$N(B
            ;; $B0LCV$KLa$9!#(B
            (if mark
                (progn
                  (goto-char mark)
                  (skk-set-marker mark nil)
                  (backward-char 1))
              (goto-char (point-max)))
            ;;
            (when skk-show-annotation
              (skk-annotation-find-and-show pair)))))
       (t
        (when (string-match "[ $B!!(B]+$" new-one)
          (setq new-one (substring new-one 0 (match-beginning 0))))
        (setq skk-henkan-list (nconc skk-henkan-list
                                     (list new-one)))
        (when (skk-numeric-p)
          (setq orglen (length skk-henkan-list))
          (skk-num-convert skk-henkan-count)
          (setq new-one (cdr (skk-get-current-candidate-1))))
        (when (or (not orglen)
                  (= orglen (length skk-henkan-list)))
          (setq skk-kakutei-flag t))
        (setq skk-henkan-in-minibuff-flag t
              skk-touroku-count (1+ skk-touroku-count))))
      ;; (nth skk-henkan-count skk-henkan-list) $B$,(B nil $B$@$+$i<-=qEPO?$K(B
      ;; $BF~$C$F$$$k!#(Bskk-henkan-count $B$r%$%s%/%j%a%s%H$9$kI,MW$O$J$$!#(B
      ;; new-one $B$,6uJ8;zNs$@$C$?$i(B nil $B$rJV$9!#(B
      (unless (string= new-one "")
        (setq skk-jisyo-updated t)  ; skk-update-jisyo $B$G;2>H(B
        new-one))))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana $B$,(B non-nil $B$J$i(B skk-henkan-key $B$+$i!"$+$D$F(B
  ;; skk-henkan-key2 $B$H8F$P$l$F$$$?$b$N$r:n$k!#(B
  ;; skk-henkan-key2 $B$H$O!"!V4A;zItJ,$NFI$_(B + "*" + $BAw$j2>L>!W$N7A<0$NJ8;zNs$r(B
  ;; $B8@$&!#(B
  (when skk-henkan-okurigana
    (save-match-data
      (string-match "[a-z]+$" skk-henkan-key)
      (concat (substring skk-henkan-key 0 (match-beginning 0))
              "*"
              skk-henkan-okurigana))))

(defun skk-remove-redundant-okurigana (word)
  "$B<-=q$KEPO?$5$l$k8uJd$N;}$DM>7W$JAw$j2>L>$r<h$j=|$/!#(B

$BAw$j$"$j$NEPO?$r$9$k$H$-!"Aw$j2>L>$r>C$7$F$+$i(B [RET] $B$r2!$5$J$1$l$P@5$7$/EPO?(B
$B$G$-$J$$!#(B $B$=$3$G!"%f!<%6$,4V0c$($FAw$j2>L>$r>C$7K:$l$F$$$J$$$+$I$&$+!"(B SKK
$B$NB&$G%A%'%C%/$G$-$kHO0O$K$D$$$F$O%f!<%6$N3NG'$r<h$k!#(B

`skk-check-okurigana-on-touroku' $B$r(B non-nil $B$K@_Dj$7$F$$$k>l9g$N$_M-8z!#(B
auto $B$K@_Dj$9$k$H%f!<%6$K3NG'$7$J$$!#(B
$BJQ49$,9T$o$l$?%P%C%U%!$G<B9T$5$l$k!#%_%K%P%C%U%!!"<-=q%P%C%U%!$G$O$J$$!#(B"
  (save-match-data
    (let* ((len (length word))
           (str1 (when (< 0 len)
                   (substring word (1- len) len)))
           (str2 (when (< 1 len)
                   (substring word (- len 2) (1- len))))
           (str (if (and str2
                         (string-match "^[$B$!(B-$B$s(B]$" str2))
                    (concat str2 str1)
                  str1)))
      (when (and str
                 (string-match "^[$B$!(B-$B$s(B]$" str1)
                 (or (eq skk-check-okurigana-on-touroku 'auto)
                     (skk-y-or-n-p
                      (format "%s: `%s' $B$r=|$$$FEPO?$7$^$9$+!)(B "
                              word str)
                      (format "%s: Remove `%s' when register? "
                              word str))))
        ;; $B%f!<%6$N;X<($K=>$$Aw$j2>L>$r<h$j=|$/!#(B
        (message "")
        (setq word (substring word
                              0
                              (if (string-match "^[$B$!(B-$B$s(B]$" str2)
                                  (- len 2)
                                (1- len)))))))
  ;;
  word)

(defun skk-show-num-type-info ()
  "$B?tCMJQ49%(%s%H%j$N<-=qEPO?;~$KJQ49%?%$%W$N0FFb$rI=<($9$k!#(B"
  (when (and skk-show-num-type-info
             skk-use-numeric-conversion
             (string-match "[0-9]" skk-henkan-key))
    (let ((buff (get-buffer-create " *$B?tCMJQ49%?%$%W(B*")))
      (with-current-buffer buff
        (erase-buffer)
        (insert "\
#0 $BL5JQ49(B                 e.g. 12
#1 $BA43Q?t;z(B               e.g. $B#1#2(B
#2 $B4A?t;z$G0L<h$j$"$j(B     e.g. $B8^8^!;!;(B
#3 $B4A?t;z$G0L<h$j$J$7(B     e.g. $B8^@i8^I4(B
#4 $B?tCM:FJQ49(B
#5 $B6b3[I=5-(B               e.g. $B0mot6eI46e=&8`(B
#9 $B>-4}4}IhMQ(B"))
      ;; skk-henkan-show-candidates-buffer $B$+$i$R$C$Q$C$F$-$?%3!<%I(B
      (let ((minibuf-p (skk-in-minibuffer-p))
            (window (get-buffer-window (skk-minibuffer-origin))))
        (when minibuf-p
          (if window
              (select-window window)
            (other-window 1)))
        (let ((w (selected-window)))
          (pop-to-buffer buff)
          (fit-window-to-buffer)
          (select-window w))
        (when minibuf-p
          (select-window (minibuffer-window)))))))

;;;###autoload
(defun skk-previous-candidate (&optional arg)
  "$B"'%b!<%I$G$"$l$P!"0l$DA0$N8uJd$rI=<($9$k!#(B
$B"'%b!<%I0J30$G$O%+%l%s%H%P%C%U%!$K%?%$%W$7$?J8;z$rA^F~$9$k!#(B
$B3NDj<-=q$K$h$k3NDj$ND>8e$K8F$V$H3NDj$r%"%s%I%%$7!"8+=P$7$KBP$9$k<!8uJd$rI=<($9$k!#(B
$B:G8e$K3NDj$7$?$H$-$N8uJd$O%9%-%C%W$5$l$k!#(B"
  (interactive "*p")
  (skk-with-point-move
   (cond
    ((not (eq skk-henkan-mode 'active))
     (if (not (eq last-command 'skk-kakutei-henkan))
         (when (and last-command-event
                    (characterp last-command-event))
           (skk-kana-input arg))
       (skk-undo-kakutei-subr)))
    ((string= skk-henkan-key "")
     nil)
    (t
     (let ((mark (unless (eobp)
                   (skk-save-point
                    (forward-char 1)
                    (point-marker))))
           pair)
       (skk-save-point
        (cl-case skk-henkan-count
          (0
           (when skk-okuri-char
             ;; roman prefix for okurigana should be removed.
             (setq skk-henkan-key (substring skk-henkan-key 0 -1)))
           (when skk-katakana
             (setq skk-henkan-key
                   (skk-hiragana-to-katakana skk-henkan-key)))
           (setq skk-henkan-count -1)
           (setq skk-henkan-in-minibuff-flag nil
                 skk-henkan-list nil
                 skk-henkan-okurigana nil
                 skk-okuri-char nil
                 skk-okuri-index-min -1
                 skk-okuri-index-max -1
                 skk-okurigana nil
                 skk-prefix "")
           (when (skk-numeric-p)
             (skk-num-initialize))
           ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l(B
           ;; $B$k(B skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
           (when skk-use-face
             (skk-henkan-face-off))
           (delete-region skk-henkan-start-point skk-henkan-end-point)
           (goto-char skk-henkan-end-point)
           (insert-and-inherit skk-henkan-key)
           (skk-change-marker-to-white))
          (t
           (setq skk-henkan-count (1- skk-henkan-count))
           (setq pair (skk-insert-new-word (skk-get-current-candidate))))))
       ;;
       (if mark
           (progn
             (goto-char mark)
             (skk-set-marker mark nil)
             (backward-char 1))
         (goto-char (point-max)))
       ;;
       (when skk-show-annotation
         (skk-annotation-find-and-show pair))
       ;;
       (when (and skk-abbrev-mode
                  (= skk-henkan-count -1))
         (skk-abbrev-mode-on)))))
   ;; verbose message
   (skk-henkan-on-message)))

(defun skk-undo (&optional arg)
  "`undo' $B$N5!G=$r!"(BSKK $B$H$N@09g@-$r9M$($FD4@a$9$k!#(B"
  (interactive "*P")
  (cond ((skk-get-prefix skk-current-rule-tree)
         (skk-kana-cleanup 'force))
        ((eq skk-henkan-mode 'active)
         (skk-previous-candidate))
        ((eq skk-henkan-mode 'on)
         (if (= (point)
                (marker-position skk-henkan-start-point))
             (skk-kakutei arg)
           (forward-char -1)
           (delete-char 1)))
        (t
         (skk-emulate-original-map arg))))

(defun skk-insert-new-word (word)
  "$B8+=P$78l$r>C$7!"$=$N>l=j$XJQ497k2L$NJ8;zNs$rA^F~$9$k!#(B"
  (save-match-data
    (let (retval note face)
      (while (and (setq retval (skk-treat-new-word (or retval word)))
                  (stringp retval))
        ;; nextword-or-retval $B$KJ8;zNs$,BeF~$5$l$?>l9g(B (`skk-ignore-dic-word'
        ;; $B$K$h$C$FA0$N8uJd$,%9%-%C%W$5$l$?>l9g(B) $B$O$=$NJ8;zNs$r?7$7$$8uJd$H$7$F(B
        ;; $B=hM}$r7+$jJV$9!#J8;zNs0J30$,BeF~$5$l$?$i%k!<%W$r=*N;$9$k!#(B
        ())
      (when (consp retval)
        (setq word (car retval)
              note (cdr retval)))
      (unless (eq retval 'none)
        ;; $BA^F~$9$Y$-8uJd$,$"$k!#(B
        (when skk-use-face
          (skk-henkan-face-off))
        (delete-region skk-henkan-start-point skk-henkan-end-point)
        (goto-char skk-henkan-start-point)
        ;; word $B$rA^F~$9$kA0$K$=$N(B face $BB0@-$r=|5n$9$k!#$?$@$7!$=|5n$9$kA0$K(B
        ;; face $BB0@-$rJQ?t$KB`Hr$9$k!#$3$N(B face $BB0@-$O<!$N8uJdI=<($K;HMQ$5$l(B
        ;; $B$k!#(B
        (setq face (get-text-property 0 'face word))
        (set-text-properties 0 (length word) nil word)
        (insert-and-inherit word)
        (skk-set-marker skk-henkan-end-point (point))
        (when skk-use-face
          (skk-henkan-face-on face))
        (when skk-insert-new-word-function
          (funcall skk-insert-new-word-function))
        (when skk-kakutei-flag
          ;; `skk-ignore-dic-word' $BFb$G<-=qEPO?%b!<%I$KF~$C$?>l9g!#(B
          (skk-kakutei))
        (cons word note)))))

(defun skk-treat-new-word (word)
  "`skk-insert-new-word' $B$N%5%V%k!<%A%s!#A^F~$9$Y$-C18l$rAu>~$9$k!#(B
$B%k!<%WFb$G$h$P$l$k!#=hM}Cf$K(B `skk-ignore-dic-word' $B$,8F$P$l$?>l9g$O$=$3$+$i(B
catch $B$9$k!#(Bcatch $B$7$?CM$,J8;zNs$J$i$P!"$=$l$rJV$9(B (word $B$r$=$l$KCV$-49$($F(B
$B:FEY8F$S=P$5$l$k(B)$B!#(B catch $B$7$J$+$C$?>l9g$OA^F~$9$Y$-J8;zNs$,F@$i$l$k!#$3$N>l9g(B
$B$OCm<a$HJ8;zNs$N(B cons cell $B$rJV$9!#(B"
  (let (note)
    (catch 'next-word
      ;; $B%k!<%W(B 1 $B2sL\$G$O(B next-word $B$O(B nil$B!#(B
      ;; `skk-ignore-dic-word' $B$K$h$j8=:_$N(B word $B$,(B skip $B$5$l!"(B
      ;; $B?7$7$$8l$,JV$C$F$-$?>l9g!"(Bnext-word $B$KJ8;zNs$,%;%C%H$5$l$F$$$k!#(B
      ;; $B$3$N>l9g(B word $B$r(B next-word $B$KCV$-49$($F=hM}$r$d$jD>$9!#(B

      ;; $B8uJd0lMw$GA*Br$5$l$?8uJd$rA^F~$9$k:]$O!"(B
      ;; skk-kakutei-flag $B$,(B t $B$K$J$C$F$$$k!#$3$N>l9g$OAu>~(B
      ;; $B$7$F$b$7$+$?$J$$!#(B
      (when (and (not skk-kakutei-flag)
                 (skk-check-treat-candidate-appearance-function))
        ;; skk-treat-candidate-appearance-function $B$K$h$C$F(B
        ;; $B%f!<%6$OG$0U$K8uJdJ8;zNs$HCm<aJ8;zNs$r2C9)!&=$>~(B
        ;; $B$9$k$3$H$,$G$-$k!#(B
        ;; $B%f!<%6$,JV$9CM$O(B cons cell $B$^$?$OJ8;zNs$H$J$k!#(B
        (save-match-data
          (let ((value (skk-treat-candidate-appearance word nil)))
            (if (consp value)
                ;; $BJV$jCM$,(B cons cell $B$@$C$?>l9g(B
                (setq word (car value)
                      note (cond
                            ((consp (cdr value))
                             ;; ($B8uJd(B . ($B%;%Q%l!<%?(B . $BCm<a(B))
                             ;; $BCm<a$O4{$K%;%Q%l!<%?H4$-(B
                             (cddr value))
                            ((string-match "^;" (cdr value))
                             ;; ($B8uJd(B . $BCm<a(B)
                             ;; $BCm<a$O$^$@%;%Q%l!<%?$r4^$s$G(B
                             ;; $B$$$k(B
                             (substring (cdr value) (match-end 0)))
                            (t
                             ;; ($B8uJd(B . $BCm<a(B)
                             ;; $BCm<a$O4{$K%;%Q%l!<%?$r=|5n$7$F(B
                             ;; $B$$$k$b$N$HH=CG$9$k(B
                             (cdr value))))
              ;; $BJV$jCM$,J8;zNs$@$C$?>l9g(B
              (setq word value)))))
      ;; $B%f!<%6$N0U?^$K$h$C$FCm<a$,4{$K@_Dj$5$l$F(B
      ;; $B$$$k>l9g$O(BSKK $B$NJ}$G$OBP=h$7$J$$!#(B
      (when (and (not (stringp note))
                 (string-match ";" word))
        (setq note (substring word (match-end 0))
              word (substring word 0 (match-beginning 0))))
      ;; word $B$NJ}$,(B S $B<0$NJ8;zNs$@$C$?$i!"$=$l$rI>2A$7$?(B
      ;; $BJ8;zNs$rJV$9!#(B
      ;; note $B$NJ}$b(B S $B<0$N>l9g$,$"$j$&$k$,!"$=$l$NI>2A$O(B
      ;; skk-annotation $B$,$d$C$F$/$l$k!#(B
      (setq word (skk-eval-string word))
      (cons word note))))

;;;###autoload
(defun skk-treat-strip-note-from-word (word)
  "$BJQ498uJd$NJ8;zNs(B WORD $B$r!"8uJd$=$N$b$N$HCm<a$H$KJ,3d$7$F(B cons cell $B$rJV$9!#(B
$B8uJd$=$N$b$N$HCm<a$H$N%;%Q%l!<%?$O(B \";\" $B$G$"$kI,MW$,$"$k!#(B
$BJ,3d$N%k!<%k$O0J2<$N$H$*$j!#(B

  \"word\" --> (\"word\" . nil)
  \"word;\" --> (\"word\" . \"\")
  \"word;note\" --> (\"word\" . \"note\")
"(when word
   (save-match-data
     (let (cand note)
       (if (string-match ";" word)
           (setq cand (substring word 0 (match-beginning 0))
                 note (substring word (match-end 0)))
         (setq cand word))
       (cons cand note)))))

;;;###autoload
(defun skk-kakutei (&optional arg word)
  "$B8=:_I=<($5$l$F$$$k8l$G3NDj$7!"<-=q$r99?7$9$k!#(B
$B%+%l%s%H%P%C%U%!$G(B SKK $B%b!<%I$K$J$C$F$$$J$+$C$?$i(B SKK $B%b!<%I$KF~$k!#(B
$B%*%W%7%g%J%k0z?t$N(B WORD $B$rEO$9$H!"8=:_I=<($5$l$F$$$k8uJd$H$OL54X78$K(B
WORD $B$G3NDj$9$k!#(B"
  ;; read only $B$G%(%i!<$K$J$k$h$&$K$9$k$H(B read only $B%P%C%U%!$G(B SKK $B$,5/F0$G$-(B
  ;; $B$J$/$J$k!#(B
  (interactive "P")
  (let ((inhibit-quit t)
        converted kakutei-word)
    (when skk-henkan-mode       ;'on or 'active
      (cl-case skk-henkan-mode
        (active             ;$B"'%b!<%I(B
         (setq kakutei-word
               ;; $B3NDj<-=q$N8l$G3NDj$7$?$H$-$O!"<-=q$K$=$N8l$r=q$-9~$`I,MW$b$J(B
               ;; $B$$$7!"99?7$9$kI,MW$b$J$$$H;W$C$F$$$?$,!"Jd40$r9T$&$H$-$O!"(B
               ;; $B8D?M<-=q$r;2>H$9$k(B ($B3NDj<-=q$O;2>H$7$J$$(B) $B$N$G!"B?>/;q8;$H;~(B
               ;; $B4V$rL5BL$K$7$F$b!"8D?M<-=q$K3NDj<-=q$N%(%s%H%j$r=q$-9~$s$G99(B
               ;; $B?7$b$7$F$*$/!#(B
               (or word (skk-get-current-candidate 'noconv)))
         (when (and kakutei-word
                    (skk-update-jisyo-p kakutei-word))
           (skk-update-jisyo kakutei-word)
           ;; $B@\Hx<-!&@\F,<-$K4X$9$k=hM}(B
           (cond
            ((not skk-learn-combined-word)
             ;; $B%f!<%6$,4uK>$7$J$$8B$j2?$N=hM}$b$7$J$$!#(B
             (setq skk-after-prefix nil))
            ((and skk-after-prefix
                  (not (string-match "^[^\000-\177]+>$" skk-henkan-key)))
             ;; $B$3$N%P%C%U%!$K$*$$$F!"@\F,<-$KB3$/F~NO$,?J9TCf!#(B
             (let* ((history (cdr skk-kakutei-history))
                    (list1 (car skk-kakutei-history)) ; ($B$j$h$&(B $BMxMQ(B)
                    (list2 (catch 'list ; ($B$5$$(B> $B:F(B)
                             (while history
                               (if (eq (nth 2 list1) (nth 2 (car history)))
                                   ;; $BF1$8%P%C%U%!$@$C$?$i(B
                                   (throw 'list (car history))
                                 (setq history (cdr history))))))
                    (list1-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list1))))
                    (list2-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list2))))
                    skk-henkan-key comb-word)
               (when (and (stringp list2-word)
                          (string-match "^[^\000-\177]+>$" (car list2))
                          (skk-save-point
                           (ignore-errors
                             (goto-char (- skk-henkan-start-point
                                           (length list1-word)))
                             (looking-at list2-word))))
                 (setq skk-henkan-key
                       (concat (substring (car list2)
                                          0
                                          (1- (length (car list2))))
                               (car list1)) ; $B$5$$$j$h$&(B
                       comb-word (concat list2-word list1-word)) ; $B:FMxMQ(B
                 (skk-update-jisyo comb-word))
               (setq skk-after-prefix nil)))
            ((and (stringp (caar skk-kakutei-history))
                  (string-match "^>[^\000-\177]+$" (caar skk-kakutei-history)))
             ;; $B:#2s$N3NDj$,@\Hx<-$@$C$?>l9g!"A02s$N3NDj$H:#2s$N@\Hx<-$r(B
             ;; $B9g$o$;$?8l$r<-=qEPO?$9$k!#(B
             (let* ((history (cdr skk-kakutei-history))
                    (list1 (car skk-kakutei-history)) ; (>$B$F$-(B $BE*(B)
                    (list2 (catch 'list ; ($B$+$s$I$&(B $B46F0(B)
                             (while history
                               (if (eq (nth 2 list1) (nth 2 (car history)))
                                   ;; $BF1$8%P%C%U%!$@$C$?$i(B
                                   (throw 'list (car history))
                                 (setq history (cdr history))))))
                    (list1-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list1))))
                    (list2-word (car (skk-treat-strip-note-from-word
                                      (nth 1 list2))))
                    skk-henkan-key comb-word)
               (when (stringp list2-word)
                 (setq skk-henkan-key
                       (concat (car list2)
                               (substring (car list1) 1)) ; $B$+$s$I$&$F$-(B
                       comb-word (concat list2-word list1-word)) ; $B46F0E*(B
                 (skk-update-jisyo comb-word)))))
           ;;
           (when (skk-numeric-p)
             (setq converted (skk-get-current-candidate))
             (skk-num-update-jisyo kakutei-word converted))))
        (t
         ;; $B"&%b!<%I$G3NDj$7$?>l9g!#JX59E*$K8=:_$N%]%$%s%H$^$G$r8+=P$78l$r07$$(B
         ;; $B$7$FMzNr$r99?7$9$k!#(B
         (when (and (> skk-kakutei-history-limit 0)
                    (< skk-henkan-start-point (point))
                    (skk-save-point
                     (goto-char skk-henkan-start-point)
                     (eq (skk-what-char-type) 'hiragana)))
           (skk-update-kakutei-history
            (buffer-substring-no-properties
             skk-henkan-start-point (point))))))
      (when (and window-system skk-show-tooltip)
        (tooltip-hide))
      (when skk-mode
        (skk-kakutei-cleanup-buffer)
        (when (and skk-undo-kakutei-word-only
                   (consp buffer-undo-list))
          ;; $B"&%b!<%I$K0\$C$F$+$i8=:_$^$G$N(B undo $B>pJs$+$i@hF,0J30$N(B
          ;; nil $B$r:o=|$9$k(B
          (setq buffer-undo-list
                (cons (car buffer-undo-list)
                      (nconc
                       (delq nil (cdr (butlast buffer-undo-list
                                               (length skk-last-buffer-undo-list))))
                       skk-last-buffer-undo-list))))
        ;; KAKUTEI-WORD $B$J$I$N>pJs$,I,MW$G$"$l$P!"(Bskk-last-henkan-data
        ;; $B$+$iF@$i$l$k!#I,MW$J%G!<%?$,$=$l$i$NJQ?t$K8BDj$5$l$J$$$N$G!"(B
        ;; $B0z?t$K$7$J$$!#(B
        (when skk-kakutei-end-function
          (funcall skk-kakutei-end-function))
        (skk-kakutei-initialize
         (if (skk-numeric-p)
             (cons kakutei-word converted)
           kakutei-word))))

    (skk-do-auto-fill)
    (when (and skk-undo-kakutei-return-previous-point
               (numberp skk-undo-kakutei-previous-point)
               (numberp skk-undo-kakutei-previous-length)
               (markerp skk-henkan-end-point)
               (markerp skk-henkan-start-point))
      (goto-char (+ skk-undo-kakutei-previous-point
                    (if (>= skk-undo-kakutei-previous-point
                            (1- (+ skk-henkan-start-point
                                   skk-undo-kakutei-previous-length)))
                        (- skk-henkan-end-point
                           skk-henkan-start-point
                           skk-undo-kakutei-previous-length)
                      0))))
    (setq skk-undo-kakutei-previous-point nil
          skk-undo-kakutei-previous-length nil)
    (cond
     ((not skk-mode)
      (skk-mode 1))
     (skk-undo-kakutei-prev-state
      (cond ((cdr (assq 'skk-latin-mode skk-undo-kakutei-prev-state))
             (skk-latin-mode-on))
            ((cdr (assq 'skk-jisx0208-latin-mode skk-undo-kakutei-prev-state))
             (skk-jisx0208-latin-mode-on))
            ;; skk-mode $B$,%*%U$N>uBV$KLa$=$&$H$9$k$H(B
            ;; `skk-mode-exit' $BFb$G:FEY(B `skk-kakutei' $B$r8F$V$?(B
            ;; $B$aL58B%k!<%W$K$J$C$F$7$^$&(B
            ;;      ((not (cdr (assq 'skk-mode skk-undo-kakutei-prev-state)))
            ;;       (skk-mode -1))
            ((cdr (assq 'skk-j-mode skk-undo-kakutei-prev-state))
             ;; M-x skk-undo-kakutei $B$G(B skk-abbrev-mode $B$KLa$C$?:]!"(B
             ;; $B3NDj8e$K(B skk-j-mode $B$KLa$k$?$a$K$O0J2<$,I,MW!#(B
             (skk-j-mode-on skk-katakana)))
      (setq skk-undo-kakutei-prev-state nil))
     ((not (or skk-j-mode skk-jisx0201-mode))
      (skk-j-mode-on skk-katakana))))
  nil)

(defun skk-update-jisyo-p (word)
  "WORD $B$,8D?M<-=q$KEPO?$5$l$k$Y$-$+H]$+$rH=Dj$9$k!#(B
$BJQ?t(B `skk-search-excluding-word-pattern-function' $B$,4X?t$G$"$l$P!"(BWORD $B$r(B
$B0z?t$K$7$F$=$N4X?t$r<B9T$7!"La$jCM$,(B non-nil $B$G$"$l$P(B `skk-update-jisyo-p' $B$O(B
 nil $B$rJV$9!#(B
$BJQ?t(B `skk-search-excluding-word-pattern-function' $B$,4X?t$N%j%9%H$G$"$l$P!"(B
 WORD $B$r0z?t$K$7$F$=$l$>$l$N4X?t$r<B9T$7!"$=$N$&$A$N$R$H$D$G$b(B non-nil $B$r(B
$BJV$;$P(B `skk-update-jisyo-p' $B$O(B nil $B$rJV$9!#(B

$BJQ?t(B `skk-search-excluding-word-pattern-function' $B$N(B docstring $B$b;2>H$N$3$H!#(B"
  (save-match-data
    (not (run-hook-with-args-until-success
          'skk-search-excluding-word-pattern-function word))))

(defun skk-kakutei-cleanup-buffer ()
  "$B3NDjD>8e$N%P%C%U%!$r@07A$9$k!#(B"
  (when skk-okurigana
    ;; $B3NDjJQ49$N$H$-$K$3$3$K$/$k!#(B
    ;; $B:FJQ49$N:]$KAw$j2>L>4XO"%U%i%0$,(B clear $B$5$l$F$$$k$H$h$m$7$/$J$$$N$G(B
    ;; `skk-delete-okuri-mark' $B$N(B NOCLEAR $B$r%;%C%H$7$F8F$V!#(B
    (skk-delete-okuri-mark t))
  (skk-delete-henkan-markers)
  (when (and (boundp 'self-insert-after-hook)
             self-insert-after-hook)
    (funcall self-insert-after-hook
             skk-henkan-start-point (point)))
  (when overwrite-mode
    (skk-del-char-with-pad (skk-ovwrt-len (buffer-substring-no-properties
                                           skk-henkan-start-point (point))))))

(defun skk-kakutei-initialize (&optional kakutei-word)
  "$B3NDj;~$KJQ?t$N=i4|2=$H%"%s%I%%$N$?$a$NJQ?t$NJ]B8$r9T$&!#(B"
  (when (and kakutei-word
             (or (consp kakutei-word)
                 (not (string= kakutei-word ""))))
    (setq skk-kakutei-count (1+ skk-kakutei-count))
    ;; skk-undo-kakutei $B$N$?$a$K:G8e$NJQ49$N%G!<%?$rJ]B8$9$k!#(B
    (skk-put-last-henkan-data
     (list (cons 'henkan-key skk-henkan-key)
           (cons 'okuri-char skk-okuri-char)
           (cons 'henkan-okurigana skk-henkan-okurigana)
           (cons 'henkan-list
                 ;; $B3NDj$7$?8l$r@hF,$K$9$k!#(B
                 (cons kakutei-word
                       (delete kakutei-word skk-henkan-list)))
           (cons 'henkan-buffer (current-buffer))
           (cons 'henkan-point
                 (let ((hpoint (skk-get-last-henkan-datum 'henkan-point)))
                   (if hpoint
                       (set-marker hpoint (point))
                     (point-marker))))
           (cons 'abbrev-mode skk-abbrev-mode)
           ;; (eq last-command 'skk-kakutei-henkan) $B$G%]!<%?%V%k$K3NG'$G$-(B
           ;; $B$k$N$G$"$($F$$$i$J$$$+!#(B
           ;; (cons 'kakutei-henkan (eq this-command 'skk-kakutei-henkan))
           ;; $B>e5-0J30$N(B henkan data $B$r(B skk-last-henkan-data $B$K;D$7$?$+$C$?$i!"(B
           ;; skk-kakutei-end-function $B$rMxMQ$9$k!#(B
           )))
  (setq skk-henkan-count -1)
  (setq skk-exit-show-candidates nil)
  (setq skk-abbrev-mode nil
        skk-henkan-in-minibuff-flag nil
        skk-henkan-key nil
        skk-henkan-list nil
        skk-henkan-okurigana nil
        skk-henkan-mode nil
        skk-kakutei-flag nil
        skk-kakutei-henkan-flag nil
        skk-undo-kakutei-flag nil
        skk-okuri-char nil
        skk-okuri-index-min -1
        skk-okuri-index-max -1
        ;; skk-prefix ""
        ))

(defun skk-undo-kakutei ()
  "$B0lHV:G8e$N3NDj$r%"%s%I%%$7!"8+=P$78l$KBP$9$k8uJd$rI=<($9$k!#(B
$B:G8e$K3NDj$7$?$H$-$N8uJd$O%9%-%C%W$5$l$k!#(B
$BB>$K8uJd$,$J$$$H$-$O!"%(%3!<%(%j%"$G$N<-=qEPO?$KF~$k!#(B"
  (interactive)
  (let (jmsg emsg)
    (cond ((eq last-command 'skk-undo-kakutei)
           (setq jmsg "$B3NDj%"%s%I%%$OO"B3;HMQ$G$-$^$;$s(B"
                 emsg "Cannot undo kakutei repeatedly"))
          ((eq skk-henkan-mode 'active)
           (setq jmsg "$B"'%b!<%I$G$O3NDj%"%s%I%%$G$-$^$;$s(B"
                 emsg "Cannot undo kakutei in $B"'(B mode"))
          ((or (not (skk-get-last-henkan-datum 'henkan-key))
               (string= (skk-get-last-henkan-datum 'henkan-key) "")
               (null skk-henkan-end-point))
           ;; skk-henkan-key may be nil or "".
           (setq jmsg "$B%"%s%I%%%G!<%?$,$"$j$^$;$s(B"
                 emsg "Lost undo data")))
    (if jmsg
        (skk-message jmsg emsg)
      (skk-with-point-move
       (condition-case nil
           (skk-undo-kakutei-subr)
         ;; skk-undo-kakutei $B$+$iESCf$GH4$1$?>l9g$O!"3F<o%U%i%0$r=i4|2=$7$F(B
         ;; $B$*$+$J$$$H<!$NF0:n$r$7$h$&$H$7$?$H$-$K%(%i!<$K$J$k!#(B
         ((error quit)
          (skk-kakutei)))))))

(defun skk-undo-kakutei-subr ()
  (let ((end (if (skk-get-last-henkan-datum 'henkan-okurigana)
                 (+ (length (skk-get-last-henkan-datum
                             'henkan-okurigana))
                    skk-henkan-end-point)
               skk-henkan-end-point)))
    (setq skk-henkan-mode 'active
          skk-current-search-prog-list
          (let ((prog-list-number current-prefix-arg))
            (cond
             ((and (integerp prog-list-number)
                   (<= 0 prog-list-number)
                   (<= prog-list-number 9))
              (let ((list (symbol-value
                           (intern
                            (format "skk-search-prog-list-%d" prog-list-number)))))
                (or list skk-search-prog-list)))
             (t
              skk-search-prog-list)))
          skk-undo-kakutei-flag t)
    ;; get henkan data back from skk-last-henkan-data.
    (setq skk-henkan-key (skk-get-last-henkan-datum 'henkan-key)
          skk-henkan-list (list (car (skk-get-last-henkan-datum 'henkan-list)))
          skk-henkan-okurigana (skk-get-last-henkan-datum
                                'henkan-okurigana)
          skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
    (setq skk-undo-kakutei-prev-state
          (list (cons 'skk-mode skk-mode)
                (cons 'skk-abbrev-mode skk-abbrev-mode)
                (cons 'skk-latin-mode skk-latin-mode)
                (cons 'skk-j-mode skk-j-mode)
                (cons 'skk-jisx0208-latin-mode skk-jisx0208-latin-mode)
                (cons 'skk-jisx0201-mode skk-jisx0201-mode)
                (cons 'skk-katakana skk-katakana)))
    (cond ((skk-get-last-henkan-datum 'abbrev-mode)
           (skk-abbrev-mode-on))
          ((or (not skk-mode) skk-latin-mode skk-jisx0208-latin-mode)
           (skk-j-mode-on)))
    (when (and skk-undo-kakutei-return-previous-point
               (markerp skk-henkan-end-point)
               (markerp skk-henkan-start-point))
      (setq skk-undo-kakutei-previous-point (point)
            skk-undo-kakutei-previous-length (- skk-henkan-end-point
                                                skk-henkan-start-point)))
    (when skk-use-numeric-conversion
      (setq skk-num-list (skk-get-last-henkan-datum 'skk-num-list)))
    (when (>= (point-max) end)
      ;; $B:G8e$NJQ49ItJ,$N%F%-%9%H$r>C$9!#Aw$j2>L>$rGD0.$7$F$$$k$N$J$i(B
      ;; (skk-process-okuri-early $B$,(B non-nil $B$J$iAw$j2>L>$rGD0.$G$-$J$$(B)$B!"(B
      ;; $BAw$j2>L>$r4^$a$?ItJ,$^$G$r>C$9!#(B
      (delete-region skk-henkan-start-point end))
    (when skk-undo-kakutei-word-only
      (setq skk-last-buffer-undo-list buffer-undo-list))
    (goto-char skk-henkan-start-point)
    (insert-and-inherit "$B"'(B")
    (skk-set-marker skk-henkan-start-point (point))
    (cond
     (skk-okuri-char
      ;; $BAw$j$"$j(B
      (insert-and-inherit (substring skk-henkan-key 0
                                     (1- (length skk-henkan-key))))
      (skk-set-marker skk-henkan-end-point (point))
      (when skk-henkan-okurigana
        (insert-and-inherit skk-henkan-okurigana)))
     (t
      (insert-and-inherit skk-henkan-key)
      (skk-set-marker skk-henkan-end-point (point))))
    (skk-message "$B3NDj%"%s%I%%!*(B"
                 "Undo kakutei!")
    (setq skk-henkan-count 1)
    (skk-henkan)))

(defun skk-set-henkan-point (&optional arg)
  "$BJQ49$r3+;O$9$k%]%$%s%H$r%^!<%/$7!"BP1~$9$k(B `skk-prefix' $B$+Jl2;$rF~NO$9$k!#(B"
  (let* ((last-char (skk-downcase last-command-event))
         (normal (not (eq last-char last-command-event)))
         (sokuon (if (string= skk-prefix (skk-char-to-unibyte-string last-char))
                     (/= last-char ?o)
                   nil))
         (henkan-active (eq skk-henkan-mode 'active)))
    (cond
     ((not (eq skk-henkan-mode 'on))
      (if normal
          (skk-set-henkan-point-subr)
        (when skk-henkan-mode
          (skk-set-henkan-point-subr))
        (if henkan-active
            (skk-emulate-original-map arg)
          ;; What's to be here?
          ;;(skk-insert arg)
          )))
     ((not normal)
      ;; special char
      (insert-and-inherit last-char)
      (skk-set-marker skk-henkan-end-point (point))
      (setq skk-henkan-count 0)
      (setq skk-henkan-key (buffer-substring-no-properties
                            skk-henkan-start-point (point))
            skk-prefix "")
      (skk-henkan))
     ;; prepare for the processing of okurigana if not skk-okurigana
     ;; and the preceding character is not a numeric character.
     ;; if the previous char is a special midashi char or a
     ;; numeric character, we assume that the user intended to type the
     ;; last-command-char in lower case.
     ((and (or
            ;; for KAnji, KanJIru
            (not (skk-get-prefix skk-current-rule-tree))
            (if (/= skk-kana-start-point skk-henkan-start-point)
                (prog1
                    t
                  (unless sokuon ; for TaSSi or TasSi
                    (skk-kana-cleanup))) ; for NEko
              nil))
           (not skk-okurigana)
           (or (= skk-henkan-start-point (point))
               (let ((p (char-before)))
                 (not (or
                       ;; previous char is a special midashi char
                       (memq p skk-special-midashi-char-list)
                       ;; previous char is an ascii numeric char
                       (and (<= ?0 p)
                            (<= p ?9))
                       ;; previous char is a JIS X 0208 numeric char
                       (and (skk-jisx0208-p p)
                            (= (skk-char-octet p 0) 35) ;?#
                            (<= 48 (skk-char-octet p 1)) ; ?0
                            (<= (skk-char-octet p 1) 57))  ; ?9
                       )))))
      (cond
       (skk-process-okuri-early
        (skk-set-marker skk-henkan-end-point (point))
        (let ((char (skk-char-to-unibyte-string last-char)))
          (setq skk-okuri-char
                (or (cdr (assoc char skk-okuri-char-alist))
                    char)))
        (cond
         (sokuon
          (setq skk-henkan-key
                (concat (buffer-substring-no-properties
                         skk-henkan-start-point
                         skk-kana-start-point)
                        (if skk-katakana "$B%C(B" "$B$C(B")
                        skk-henkan-okurigana))
          (skk-erase-prefix)
          (insert-and-inherit (if skk-katakana "$B%C(B " "$B$C(B "))
          (setq skk-prefix "")
          (setq skk-henkan-count 0)
          (skk-henkan)
          (delete-char -2))
         (t
          (setq skk-henkan-key (concat
                                (buffer-substring-no-properties
                                 skk-henkan-start-point
                                 (point))
                                skk-okuri-char))
          (insert-and-inherit " ")
          (setq skk-prefix "")
          (setq skk-henkan-count 0)
          (skk-henkan)
          (delete-char -1)))
        ;; we set skk-kana-start-point here, since the marker may no
        ;; longer point at the correct position after skk-henkan.
        (skk-set-marker skk-kana-start-point (point)))
       ((/= skk-henkan-start-point (point))
        (when sokuon
          (skk-erase-prefix 'clean)
          (insert-and-inherit (if skk-katakana "$B%C(B" "$B$C(B")))
        (cond
         ((and (not sokuon)
               (skk-get-prefix skk-current-rule-tree)
               normal
               (eq (char-before) (string-to-char skk-prefix)))
          ;; SKK $B$N;EMM$K$J$$Nc30$N=hM}!#(B
          ;; $BNc$($P!$%f!<%6$,(B $B!VJb$/!W$rA^F~$7$?$/$F(B "AruKu" $B$HBG$D$Y$-(B
          ;; $B$H$3$m$r(B "ArukU" $B$HBG$C$F$7$^$C$?>l9g!#$3$N>l9g(B SKK $BB&$G(B
          ;; $B$I$&=hM}$9$k$Y$-$+!"7h$^$C$F$$$J$$!#$3$3$G$O2>$N=hCV$H$7$F!"(B
          ;; "AruKu" $B$HF1MM$NJQ49$r$9$k$h$&$K$7$F$*$/!#(B
          (setq skk-okuri-char nil
                skk-okurigana nil
                normal nil)
          (set 'last-command-event last-char)
          (let ((skk-dcomp-activate nil))
            (skk-kana-input arg))
          (skk-set-char-before-as-okurigana))
         (t
          (when (and skk-dcomp-activate
                     (skk-dcomp-marked-p))
            ;; $B?7$7$$(B marker $B$r(B set $B$9$kA0$K(B  skk-dcomp $B$N(B marker $B$r%/%j%"(B
            ;; $B$7$F$*$/!#(B
            (skk-dcomp-before-kakutei))
          (skk-set-marker skk-okurigana-start-point (point))
          (insert-and-inherit "*")
          (skk-set-marker skk-kana-start-point (point))
          (setq skk-okuri-char (skk-char-to-unibyte-string last-char)
                skk-okurigana t)))))))
    (when normal
      (set 'last-command-event last-char)
      (skk-kana-input arg))))

;;;###autoload
(defun skk-henkan-on-message ()
  (condition-case nil
      (when (and skk-verbose
                 (not (or skk-isearch-switch
                          (skk-in-minibuffer-p)))
                 (eq skk-henkan-mode 'on)
                 (< (marker-position skk-henkan-start-point) (point))
                 (sit-for skk-verbose-wait))
        (skk-setup-verbose-messages)
        (message "%s" skk-henkan-on-message))
    (quit
     (keyboard-quit)))
  nil)

;;;###autoload
(defun skk-start-henkan (arg &optional prog-list-number)
  "$B"&%b!<%I$G$O4A;zJQ49$r3+;O$9$k!#"'%b!<%I$G$O<!$N8uJd$rI=<($9$k!#(B
$B"&%b!<%I$G%+%J%b!<%I$N$^$^4A;zJQ49$r3+;O$7$?>l9g$O!"8+=P$78l$rJ?2>L>$K(B
$BJQ49$7$F$+$i4A;zJQ49$r3+;O$9$k!#8+=P$78l$rJQ49$;$:$K$=$N$^$^4A;zJQ49$r(B
$B9T$$$?$1$l$P!"(B\\[universal-argument] SPC $B$H%?%$%W$9$k!#(B"
  (interactive "*p")
  (unless prog-list-number
    (setq prog-list-number current-prefix-arg))
  (skk-with-point-move
   (cancel-undo-boundary)
   (if (eq skk-henkan-mode 'active) ;$B"'%b!<%I(B
       (progn
         (setq skk-henkan-count (1+ skk-henkan-count))
         (skk-henkan))
     ;; $B"&%b!<%I(B
     (save-match-data
       (let (pos)
         (skk-kana-cleanup 'force)
         (when (skk-get-prefix skk-current-rule-tree)
           ;; Never.  `skk-erase-prefix' called by `skk-kana-cleanup'
           ;; initializes `skk-prefix'.
           (skk-error "$B%U%#%C%/%9$5$l$F$$$J$$(B skk-prefix $B$,$"$j$^$9(B"
                      "Have unfixed skk-prefix"))
         (setq pos (point))
         (when (< pos skk-henkan-start-point)
           (skk-error "$B%+!<%=%k$,JQ493+;OCOE@$h$jA0$K$"$j$^$9(B"
                      "Henkan end point must be after henkan start point"))
         (setq skk-henkan-key (buffer-substring-no-properties
                               skk-henkan-start-point pos))
         (when (and skk-katakana
                    ;; C-u $B$r;H$C$F$$$J$$>l9g(B
                    ;; prog-list-number $B$O(B skk-insert() $B$K$*$1$k(B current-prefix-arg
                    (not (and prog-list-number (listp prog-list-number))))
           (setq skk-henkan-key (skk-katakana-to-hiragana skk-henkan-key)))
         (when (and skk-okurigana
                    (string-match "\\* *$" skk-henkan-key))
           (skk-error "$B6u$NAw$j2>L>$G4A;z$rEPO?$7$h$&$H$7$F$$$^$9(B"
                      "No okurigana!"))
         (if skk-allow-spaces-newlines-and-tabs
             ;; skk-henkan-key $B$NCf$N(B "[ \n\t]+" $B$r40A4$K<h$j=|$/!#(B
             (while (string-match "[ \n\t]+" skk-henkan-key)
               (setq skk-henkan-key
                     (concat (substring skk-henkan-key 0 (match-beginning 0))
                             (substring skk-henkan-key (match-end 0)))))
           (skk-save-point
            (beginning-of-line)
            (when (> (point) skk-henkan-start-point)
              (skk-error "$BJQ49%-!<$K2~9T$,4^$^$l$F$$$^$9(B"
                         "Henkan key may not contain a new line character")))
           ;; $B:G=i$N%9%Z!<%9$G(B skk-henkan-key $B$r%+%C%H$9$k$@$1!#(B
           (setq skk-henkan-key (substring skk-henkan-key
                                           0
                                           (string-match " "
                                                         skk-henkan-key))))
         (skk-set-marker skk-henkan-end-point pos)
         (setq skk-henkan-count 0)
         (setq skk-annotation-first-candidate t)
         (skk-henkan prog-list-number)
         (when (and skk-abbrev-mode
                    (eq skk-henkan-mode 'active))
           ;; $B$3$&$7$F$*$+$J$$$HJQ498e!"<!$KF~NO$5$l$kJ8;z$b$^$?(B
           ;; SKK abbrev-mode $BF~NO$K$J$C$F$7$^$&!#(B
           (skk-j-mode-on skk-katakana)
           (setq skk-abbrev-mode t)))))))

(defun skk-auto-start-henkan (str)
  "STR $B$,(B `skk-auto-start-henkan-keyword-list' $B$NMWAG$H0lCW(B (member) $B$9$k>l9g$KJQ49$r3+;O$9$k!#(B
$B4X?t(B `skk-insert-str' $B$N<B9TCf!"JQ?t(B `skk-auto-start-henkan' $B$,(B non-nil $B$N$H$-$K$3$N4X?t$,%3!<%k$5$l$k!#(B"
  (when (member str skk-auto-start-henkan-keyword-list)
    (skk-save-point
     (backward-char 1)
     (when (> (point) skk-henkan-start-point)
       (let ((skk-prefix ""))
         (skk-start-henkan (prefix-numeric-value current-prefix-arg)))))))

(defun skk-backward-and-set-henkan-point (arg)
  "$B%]%$%s%H$ND>A0$K$"$kJ8;zNs$N@hF,$KJQ493+;O%]%$%s%H$r<($9(B \"$B"&(B\" $B$rIU$1$k!#(B
$B%+!<%=%k$ND>A0$K$"$kJ8;z(B ($B%9%Z!<%9J8;z!"%?%VJ8;z!"D92;$rI=$o$9!V!<!W(B $B$OL5>r7o(B
$B$K%9%-%C%W$5$l$k(B) $B$r(B `skk-what-char-type' $B$K$FH=JL$7!"F1<o$NJ8;zNs$r$R$H$+$?$^(B
$B$j$H$7$F8eJ}$X%9%-%C%W$9$k!#(B
$BC"$7!"$R$i$,$J$N>l9g$O!V$r!W$ND>A0$G!"%+%?%+%J$N>l9g$O!V%r!W$ND>A0$G;_$^$k!#(B
\\[universal-argument] ARG $B$G(B ARG $B$rM?$($k$H!"$=$NJ8;zJ,$@$1La$C$FF1$8F0:n$r9T$&!#(B"
  (interactive "*P")
  (if (not skk-mode)
      (skk-emulate-original-map arg)
    (catch 'exit1
      (skk-save-point
       ;; $B$H$j$"$($::G=i$N(B SPC, TAB, $BA43Q(B SPC $B$@$1%8%c%s%W$9$k!#(B
       (skip-chars-backward " \t$B!!(B")
       ;; $B0z?t$"$j!#(B
       (cond
        (arg
         (if (not skk-allow-spaces-newlines-and-tabs)
             (backward-char (prefix-numeric-value arg))
           (setq arg (prefix-numeric-value arg))
           (while (> arg 0)
             (skip-chars-backward " \t$B!!(B")
             (if (bolp)
                 ;; $B9TF,$@$C$?$i0l9TA0$N9TKv$^$GLa$k$,!"(Barg $B$O8:$i$5$J$$!#(B
                 (backward-char 1)
               (backward-char 1)
               (setq arg (1- arg))))))
        (t
         ;; $B0z?t$J$7!#(B
         (let ((limit
                (if (not skk-allow-spaces-newlines-and-tabs)
                    (skk-save-point (beginning-of-line) (point))
                  (point-min)))
               ;; $B!2!1!0!/!.!-!,!+!*!)!(!'!&!%!$!#(B
               (unknown-chars-regexp
                (if skk-allow-spaces-newlines-and-tabs
                    "[ $B!!(B\n\t$B!<!7!6!5!4!3(B]"
                  "[$B!!!<!7!6!5!4!3(B]"))
               type p)
           (save-match-data
             (skk-save-point
              (backward-char 1)
              (while (and (> (point) limit)
                          ;; unknown-chars-regexp $B$G$OJ8;z<oJL$,H=JL$G$-$J$$$N(B
                          ;; $B$G!"$=$NJ8;zNs$,B3$/8B$j%]%$%s%H$r%P%C%U%!$N@hF,(B
                          ;; $BJ}8~$XLa$9!#(B
                          (looking-at unknown-chars-regexp))
                (backward-char 1))
              (setq type (skk-what-char-type))
              (when (eq type 'unknown)
                (throw 'exit1 nil))
              (skk-backward-and-set-henkan-point-1 type)
              (setq p (point))
              (when skk-allow-spaces-newlines-and-tabs
                (while (and (> (point) limit) (bolp))
                  ;; 1 $B9T>e$N9TKv$X!#(B
                  (backward-char 1)
                  ;; $B%]%$%s%H$,H=JL$G$-$J$$J8;z<oJL$N>e$K$"$k4V$O(B
                  ;; backward $BJ}8~$X%]%$%s%H$rLa$9!#(B
                  ;;(while (and (> (point) limit)
                  ;;            (looking-at unknown-chars-regexp))
                  ;;  (backward-char 1))
                  (when ;;(or
                      (> 0 (skk-backward-and-set-henkan-point-1 type))
                    ;;(eq (skk-what-char-type) type))
                    (setq p (point)))))))
           (goto-char p)
           (skip-chars-forward unknown-chars-regexp))))
       (skk-set-henkan-point-subr)))))

(defun skk-backward-and-set-henkan-point-1 (type)
  "`skk-backward-and-set-henkan-point' $B$N%5%V%k!<%A%s!#(B
TYPE ($BJ8;z$N<oN`(B) $B$K1~$8$?J8;z$r%9%-%C%W$7$F%P%C%U%!$N@hF,J}8~$XLa$k!#(B"
  (skip-chars-backward
   (cl-case type
     (hiragana
      ;; "$B$r(B" $B$NA0$G;_$^$C$?J}$,JXMx!)(B
      "$B!3!4!5!6!7!<$s$!(B-$B$q(B")
     (katakana
      ;; "$B%r(B" $B$NA0$G;_$^$C$?J}$,JXMx!)(B
      "$B!3!4!5!6!7!<%s%!(B-$B%q(B")
     (jisx0208-latin
      "$B!!(B-$B#z(B")
     (ascii
      " -~"))))

(defun skk-what-char-type ()
  "$B8=:_$N%]%$%s%H$K$"$kJ8;z$N<oN`$rH=JL$9$k!#(B
$BJ8;z$N<oN`$K1~$8$F!"<!$N$$$:$l$+$N%7%s%\%k$rJV$9!#(B
'hiragana 'katakana 'jisx0208-latin 'ascii 'unknown"
  (save-match-data
    (cond ((looking-at "[$B$!(B-$B$s(B]")
           'hiragana)
          ((looking-at "[$B%!(B-$B%v!3!4(B]")
           'katakana)
          ;; "$B!<(B" $B$r=|30$7$F$$$k(B ("$B!<(B" $B$O(B "$B!;(B" $B$H(B "$B!=(B" $B$N4V$KF~$C$F$$$k(B)$B!#(B
          ((looking-at "[$B!!(B-$B!;!=(B-$B#z(B]")
           'jisx0208-latin)
          ((looking-at "[ -~]")
           'ascii)
          (t
           'unknown))))

;;;###autoload
(defun skk-set-henkan-point-subr (&optional arg)
  "$B$+$J$rF~NO$7$?8e$G!"%]%$%s%H$KJQ493+;O$N%^!<%/(B ($B"&(B) $B$rIU$1$k!#(B
$B$3$N4X?t$O(B `skk-set-henkan-point' $B$NFbIt4X?t$H$7$F$b;HMQ$5$l$F$$$k!#(B"
  (interactive "*P")
  (skk-with-point-move
;;;    (unless skk-undo-kakutei-word-only
;;;      (skk-cancel-undo-boundary))
   (if skk-henkan-mode
       (skk-kakutei)
     (skk-kana-cleanup));; XXX
   (when skk-undo-kakutei-word-only
     (setq skk-last-buffer-undo-list buffer-undo-list))
   (if (not (skk-get-prefix skk-current-rule-tree))
       (insert-and-inherit "$B"&(B")
     (skk-erase-prefix)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-kana-start-point (point))
     (skk-insert-prefix))
   (setq skk-henkan-mode 'on)
   (setq skk-henkan-end-point nil)
   (skk-set-marker skk-henkan-start-point (point)))
  nil)

(defun skk-change-marker ()
  "\"$B"&(B\"$B$r(B\"$B"'(B\"$B$KJQ$($k!#(B`skk-henkan-mode' $B$r(B active $B$K$9$k!#(B"
  (skk-save-point
   (goto-char (1- skk-henkan-start-point))
   (unless (looking-at "$B"&(B")
     (skk-kakutei)
     (skk-error "$B"&$,$"$j$^$;$s(B"
                "It seems that you have deleted $B"&(B"))
   (cancel-undo-boundary)
   (let ((buffer-undo-list t))
     (insert-and-inherit "$B"'(B")
     (delete-char 1))
   (setq skk-henkan-mode 'active)))

(defun skk-change-marker-to-white ()
  "\"$B"'(B\"$B$r(B\"$B"&(B\"$B$KJQ$($k!#(B`skk-henkan-mode' $B$r(B on $B$K$9$k!#(B"
  (skk-save-point
   (goto-char (1- skk-henkan-start-point))
   (cancel-undo-boundary)
   (if (looking-at "$B"'(B")
       (let ((buffer-undo-list t))
         (insert-and-inherit "$B"&(B")
         (delete-char 1))
     (goto-char skk-henkan-start-point)
     (insert-and-inherit "$B"&(B")
     (skk-set-marker skk-henkan-start-point (point))
     (skk-message "$B"'$,$"$j$^$;$s(B"
                  "It seems that you have deleted $B"'(B"))
   (setq skk-henkan-end-point nil)
   (setq skk-henkan-mode 'on)))

(defun skk-delete-henkan-markers (&optional nomesg)
  "$BJQ49;~$K%+%l%s%H%P%C%U%!$KI=$o$l$k(B \"$B"&(B\", \"$B"'(B\" $B%^!<%/$r>C$9!#(B"
  (when (marker-position skk-henkan-start-point)
    (save-match-data
      (skk-save-point
       (goto-char (1- skk-henkan-start-point))
       (cond
        ((eq skk-henkan-mode 'active)
         (when skk-use-face
           (skk-henkan-face-off))
         (if (looking-at "$B"'(B")
             (delete-char 1)
           (unless nomesg
             (skk-message "$B"'$,$"$j$^$;$s(B"
                          "It seems that you have deleted $B"'(B"))))
        ((looking-at "$B"&(B")
         (delete-char 1))
        ((not nomesg)
         (skk-message "$B"&$,$"$j$^$;$s(B"
                      "It seems that you have deleted $B"&(B")))))))

(defun skk-delete-okuri-mark (&optional noclear)
  "$BAw$j2>L>4XO"%U%i%0$r>C$9!#(B
$BAw$j2>L>F~NOCf$K%+%l%s%H%P%C%U%!$KI=$o$l$k(B `*' $B%^!<%/$r>C$7!"(B
NOCLEAR $B$,(B nil $B$G$"$l$PAw$j2>L>4XO"%U%i%0$r(B nil $B$K%;%C%H$9$k!#(B"
  (when (and skk-okurigana
             skk-okurigana-start-point
             (markerp skk-okurigana-start-point)
             (marker-position skk-okurigana-start-point))
    (skk-save-point
     (when (eq ?* (char-after skk-okurigana-start-point))
       (delete-region skk-okurigana-start-point
                      (1+ skk-okurigana-start-point))))
    (setq skk-okurigana nil)
    (unless noclear
      (setq skk-okuri-char nil
            skk-henkan-okurigana nil))))

;; "[F7] $B%+%?%+%J(B" $B$N$h$&$J4JC1$JJQ495!G=$r3d$jEv$F$k$?$a$N%3%^%s%IDj5A(B
(defun skk-start-henkan-prog-null-handler (arg)
  (condition-case nil
      (skk-emulate-original-map arg)
    (error
     (let ((key (this-command-keys)))
       (when (keymapp (let (skk-j-mode)
                        (key-binding key)))
         (define-key skk-j-mode-map key nil)
         (skk-unread-event (if (vectorp key)
                               (aref key 0)
                             (string-to-char key))))))))

(defun skk-start-henkan-prog-i (i arg)
  (cond ((and skk-henkan-mode
              (symbol-value (intern (format "skk-search-prog-list-%d" i))))
         (when (eq skk-henkan-mode 'active)
           (let ((skk-verbose-wait 0))
             (skk-henkan-inactivate)))
         (skk-start-henkan arg i))
        (t
         (skk-start-henkan-prog-null-handler arg))))

(eval-when-compile
  (defmacro skk-define-start-henkan-progs ()
    (let (list)
      (dotimes (i 10)
        (setq list
              (nconc
               list
               `((defun ,(intern (format "skk-start-henkan-prog-%d" i)) (arg)
                   (interactive "*p")
                   (skk-start-henkan-prog-i ,i arg))))))
      (cons 'progn list))))

(skk-define-start-henkan-progs)

;;; jisyo related functions
(defun skk-purge-from-jisyo (&optional arg)
  "$B"'%b!<%I$G8=:_$N8uJd$r<-=q%P%C%U%!$+$i>C5n$9$k!#(B"
  (interactive "*P")
  (skk-with-point-move
   (cond
    ((not (eq skk-henkan-mode 'active))
     (skk-emulate-original-map arg))
    ((and (eq skk-henkan-mode 'active)
          (not (string= skk-henkan-key ""))
          (yes-or-no-p
           (format
            (if skk-japanese-message-and-error
                "%s /%s/%s$B$r<-=q$+$i:o=|$7$^$9!#NI$$$G$9$+!)(B "
              "Really purge \"%s /%s/%s\"? ")
            skk-henkan-key
            (skk-get-current-candidate)
            (cond
             ((not (and skk-henkan-okurigana
                        (or skk-henkan-okuri-strictly
                            skk-henkan-strict-okuri-precedence)))
              " ")
             (skk-japanese-message-and-error
              (format " ($BAw$j2>L>(B: %s) " skk-henkan-okurigana))
             (t
              (format " (okurigana: %s) " skk-henkan-okurigana))))))
     ;; skk-henkan-start-point $B$+$i(B point $B$^$G:o=|$7$F$7$^$C$F$b!"JQ49D>8e(B
     ;; $B$K(B ($B%+!<%=%k$rF0$+$9$3$H$J$/(B) skk-purge-from-jisyo $B$r8F$Y$PLdBj$J$$(B
     ;; $B$,!"%+!<%=%k$,0c$&>l=j$X0\F0$7$F$$$?>l9g$O!":o=|$9$Y$-$G$J$$$b$N$^(B
     ;; $B$G:o=|$7$F$7$^$&2DG=@-$,$"$k!#$=$3$G!"Aw$j2>L>$,$"$l$P$=$ND9$5$r4^(B
     ;; $B$a$?(B end $B$r5a$a!":#2s$NJQ49$K4XO"$7$?8D=j$@$1$r@53N$K@Z$j<h$k$h$&$K(B
     ;; $B$9$k!#(B
     (let ((end (if skk-henkan-okurigana
                    (+ (length skk-henkan-okurigana)
                       skk-henkan-end-point)
                  skk-henkan-end-point))
           (word (skk-get-current-candidate)))
       (skk-update-jisyo word 'purge)
       ;; Emacs 19.28 $B$@$H(B Overlay $B$r>C$7$F$*$+$J$$$H!"<!$K(B insert $B$5$l$k(B
       ;; skk-henkan-key $B$K2?8N$+(B Overlay $B$,$+$+$C$F$7$^$&!#(B
       (when skk-use-face
         (skk-henkan-face-off))
       (delete-region skk-henkan-start-point end)
       (skk-change-marker-to-white)
       (skk-kakutei)))))
  nil)

;;;###autoload
(defun skk-save-jisyo (&optional quiet)
  "SKK $B$N<-=q%P%C%U%!$r%;!<%V$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B QUIET $B$,(B non-nil $B$G$"$l$P!"<-=q%;!<%V;~$N%a%C%;!<%8$r(B
$B=P$5$J$$!#(B"
  (interactive "P")
  ;; skk.el $B0J30$GDs6!$5$l$k<-=q%;!<%V5!G=$rMxMQ$G$-$k$h$&$K4X?t$r(B funcall $B$9$k(B
  ;; $B7A$K$7$F$*$/!#(B
  (unless noninteractive
    ;; emacs $B$N=*N;;~$K8D?M<-=q$rJ]B8$9$k$?$a!"(Bskk.el (5190 $B9T$"$?$j(B) $B$G(B
    ;; kill-emacs-hook $B$K(B #'skk-save-jisyo $B$r(B add-hook $B$7$F$$$k!#(B
    ;; skk-mode $B$KF~$k$?$S$K8D?M<-=q$NB8:_$r3NG'$7$F$$$k!J$J$1$l$P<+F0@8@.!K$,!"(B
    ;; $B2?$i$+$N860x$G8D?M<-=q$,B8:_$7$J$$>uBV$G(B emacs $B$r=*N;$7$h$&$H$9$k$H(B
    ;; skk-save-jisyo-original (skk-get-jisyo-buffer) $BFb$N%(%i!<$G(B emacs $B$r=*N;$G$-$J$$;vBV$H$J$k!#(B
    (when (file-exists-p (skk-jisyo))
      (funcall skk-save-jisyo-function quiet))))

;;;###autoload
(defun skk-save-jisyo-original (&optional quiet)
  "SKK $B$N<-=q%P%C%U%!$r%;!<%V$9$k!#(B
$B%*%W%7%g%J%k0z?t(B QUIET $B$,(B non-nil $B$G$"$l$P!"<-=q%;!<%V;~$N%a%C%;!<%8$r=P$5$J$$!#(B"
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (not (and jisyo-buffer
                  (buffer-modified-p jisyo-buffer)))
        (unless quiet
          (skk-message "SKK $B<-=q$rJ]B8$9$kI,MW$O$"$j$^$;$s(B"
                       "No need to save SKK jisyo")
          (sit-for 1))
      ;;
      (with-current-buffer jisyo-buffer
        (when (skk-share-private-jisyo-p)
          (lock-buffer (skk-jisyo))
          (when (skk-jisyo-is-shared-p)
            (skk-update-shared-jisyo)))
        (let ((inhibit-quit t)
              (tempo-file (make-temp-file "skk")))
          (unless quiet
            (skk-message "SKK $B<-=q$rJ]B8$7$F$$$^$9(B..."
                         "Saving SKK jisyo..."))
          (skk-save-jisyo-as tempo-file)
          (skk-check-size-and-do-save-jisyo tempo-file)
          ;; $B<-=q$N%;!<%V$K@.8y$7$F=i$a$F(B modified $B%U%i%0$r(B nil $B$K$9$k!#(B
          (cond
           ((skk-share-private-jisyo-p)
            (skk-init-shared-jisyo)
            ;; `set-buffer-modified-p' $B$OITMW$J(B lock $B$r2r=|$9$k!#$?$@$7!"(B
            ;; $B%P%C%U%!$H%U%!%$%kL>$,4XO"IU$1$i$l$F$$$kI,MW$,$"$k!#(B
            (let ((buffer-file-name (expand-file-name (skk-jisyo)))
                  (buffer-file-truename (file-truename (skk-jisyo))))
              (set-buffer-modified-p nil)))
           (t
            (set-buffer-modified-p nil)))
          (unless quiet
            (skk-message "SKK $B<-=q$rJ]B8$7$F$$$^$9(B...$B40N;!*(B"
                         "Saving SKK jisyo...done")
            (sit-for 1))))))
  (setq skk-update-jisyo-count 0))

(defun skk-init-shared-jisyo ()
  (fillarray skk-jisyo-update-vector nil)
  (with-temp-buffer
    (insert skk-emacs-id "\n")
    (write-region 1 (point-max) skk-emacs-id-file nil 'nomsg)))

(defun skk-jisyo-is-shared-p ()
  (and (file-exists-p skk-emacs-id-file)
       (with-temp-buffer
         (insert-file-contents skk-emacs-id-file)
         (goto-char (point-min))
         ;; $B8D?M<-=q$,B>$N(B emacs $B>e$N(B skk $B$K$h$j99?7$5$l$?$+$r%A%'%C%/(B
         (not (search-forward skk-emacs-id nil t)))))

(defun skk-update-shared-jisyo ()
  "$B8D?M<-=q$,6&M-$5$l$F$$$k>l9g$K!"?7$7$$>pJs$K99?7$9$k!#(B
$B8=:_$N<-=q%P%C%U%!$NFbMF$r>C5n$7$F!"B>$N(B Emacs $B>e$N(B SKK $B$,99?7$7$?(B
`skk-jisyo' $B$rFI$_9~$`!#(B"
  (erase-buffer)
  (insert-file-contents (skk-jisyo))
  (skk-setup-jisyo-buffer)
  ;; skk-jisyo-update-vector $B$K$7$?$,$C$F%P%C%U%!$r99?7$9$k!#(B
  (let ((index 0)
        (len (length skk-jisyo-update-vector))
        list skk-henkan-key)
    (while (and (< index len)
                (setq list (aref skk-jisyo-update-vector index)))
      ;; skk-update-jisyo-1, skk-search-jisyo
      ;; $B$G;2>H$5$l$k(B skk-henkan-key $B$r%;%C%H$9$k(B
      (when (setq skk-henkan-key (car list))
        (skk-update-jisyo-1 (nth 1 list)                          ;okurigana
                            (nth 2 list)                          ;word
                            (skk-search-jisyo (nth 1 list) 0 'delete) ;old-words-list
                            (nth 3 list)))                            ;purge
      (setq index (1+ index)))))

(defun skk-save-jisyo-as (file)
  (save-match-data
    (let (buffer-read-only)
      (goto-char (point-min))
      (unless (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
        (skk-error
         "\
$BAw$j$"$j%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s!*(B SKK $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
         "\
Header line for okuri-ari entries is missing!  Stop saving SKK jisyo"))
      (unless (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
        (skk-error
         "\
$BAw$j$J$7%(%s%H%j$N%X%C%@!<$,$"$j$^$;$s(B $B!*(B SKK $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
         "\
Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo")))
    (let ((coding-system-for-write (skk-find-coding-system (skk-jisyo t)))
          jka-compr-compression-info-list)
      (write-region 1 (point-max) file nil 'nomsg))))

(defun skk-check-size-and-do-save-jisyo (new-file)
  (skk-bind-last-command-char nil
    (let ((new-size (nth 7 (file-attributes new-file)))
          old-size
          ;; yes-or-no-p $B$K2sEz$7!"(Bnewline $B$9$k$H!"(Bthis-command $B$,JQ$C$F$7$^$&!#(B
          this-command this-command-char last-command)
      (when (zerop new-size)
        (delete-file new-file)
        (skk-error "SKK $B<-=q$,6u$K$J$C$F$$$^$9!*(B $B<-=q$N%;!<%V$rCf;_$7$^$9(B"
                   "Null SKK jisyo!  Stop saving jisyo"))
      (cond
       ((or (not skk-compare-jisyo-size-when-saving)
            ;; $B5l<-=q$H$N%5%$%:$rHf3S$7$J$$!#(B
            (progn
              ;; (1)skk-jisyo $B$,$J$$$+!"(B
              ;; (2)new-file $B$H(B skk-jisyo $B$,F10l$N%5%$%:$+(B
              ;;    (skk-(aux-)large-jisyo $B$+$i?75,$NC18l$rFI$_9~$^$J$+$C$?$j!"(B
              ;;    $B?75,C18l$NEPO?$r9T$o$J$+$C$?>l9g$O%5%$%:$,F1$8(B)$B!"(B
              ;; (3)new-file $B$NJ}$,Bg$-$$(B
              ;; $B>l9g(B ($B>e5-$N(B 3 $BDL$j$G$"$l$P$$$:$l$b@5>o(B)$B!#(B
              (setq old-size (nth 7 (file-attributes (skk-jisyo))))
              (or (not old-size)
                  (>= new-size old-size))))
        (skk-make-new-jisyo new-file))

       ((skk-yes-or-no-p
         (format "%s $B$,(B %dbytes $B>.$5$/$J$j$^$9$,!"%;!<%V$7$FNI$$$G$9$+!)(B"
                 (skk-jisyo) (- old-size new-size))
         (format "New %s will be %dbytes smaller.  Save anyway?"
                 (skk-jisyo) (- old-size new-size)))
        ;; $B$H$K$+$/%;!<%V!#(B
        (skk-make-new-jisyo new-file))

       (t
        ;; $B%;!<%V$H$j;_$a!#(B
        (delete-file new-file)
        (with-output-to-temp-buffer "*SKK warning*"
          (if skk-japanese-message-and-error
              (princ "\
$B%;!<%V$7$h$&$H$9$k<-=q$N%5%$%:$,85$N%5%$%:$h$j$b>.$5$/$J$C$F$7$^$&$?$a!"(B
$B%;!<%V$rCf;_$7$^$7$?!#<-=q$N%5%$%:$,>.$5$/$J$C$?860x$K$O!"Nc$($P!"(B

    (a) M-x skk-purge-from-jisyo $B$r<B9T$7$?!#(B

    (b) ~/.skk-jisyo $B$NJ8;z%3!<%I$H$O0[$J$kJ8;z%3!<%I$G(B \" *.skk-jisyo*\"
       $B%P%C%U%!$,J]B8$5$l$h$&$H$7$F$$$k!#(B

    (c) \" *.skk-jisyo*\" $B%P%C%U%!$r<+J,$GJT=8$7$?!#(B

$B$J$I$,$"$j$^$9!#(B(a) $B$H(B (b) $B$N>l9g$O0[>o$G$O$"$j$^$;$s!#(B
\(c) $B$N>l9g$OJT=8$NFbMF$K$h$j$^$9!#860x$r?5=E$K3NG'$7$F$+$i<-=q$rJ]B8$9$k$3(B
$B$H$r$*4+$a$7$^$9!#(B

$B85$N<-=q$r:F$SFI$_9~$`$K$O!"(B

    M-x skk-reread-private-jisyo

$B$r<B9T$7$F2<$5$$!#(B")
            (princ "\
Saving your private dictionary has been canceled, since the size of the
dictionary will be smaller.  The following cases should be considered:

   (a) You executed M-x skk-purge-from-jisyo,

   (b) The coding system SKK tried to save \" *.skk-jisyo*\" buffer in
       is different from that of ~/.skk-jisyo.

   (c) You have edited \" *.skk-jisyo*\" buffer manually.

Either the case (a) or (b) is not strange.  Probability of the case (c)
depends on how you edited the buffer.  Anyway, it is strongly recommended
that you check each of the cases above and save the dictionary carefully.

If you want to restore the dictionary from your drive, try

    M-x skk-reread-private-jisyo
")))
        (skk-error "SKK $B<-=q$N%;!<%V$rCf;_$7$^$7$?!*(B"
                   "Stop saving SKK jisyo!"))))))

(defun skk-make-new-jisyo (tempo-file)
  "TEMPO-FILE $B$r?75,$N(B `skk-jisyo' $B$K$9$k!#(B
`skk-backup-jisyo' $B$,(B non-nil $B$@$C$?$i%P%C%/%"%C%W<-=q$r:n$k!#(B"
  (if skk-backup-jisyo
      (progn
        (when (file-exists-p skk-backup-jisyo)
          (delete-file skk-backup-jisyo))
        (rename-file (skk-jisyo) skk-backup-jisyo))
    (delete-file (skk-jisyo)))
  (rename-file tempo-file (skk-jisyo) 'ok-if-already-exists))

;;;###autoload
(defun skk-reread-private-jisyo (&optional force)
  "$B%P%C%U%!$KFI$_9~$s$@8D?M<-=q$rGK4~$7!"%U%!%$%k$+$i%P%C%U%!$X:FFI$_9~$_$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B FORCE $B$,(B non-nil $B$G$"$l$P!"GK4~$N3NG'$r$7$J$$!#(B"
  (interactive "P")
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (when (and buf
               (or force
                   (skk-yes-or-no-p
                    "$BL$%;!<%V$N8D?M<-=q$rGK4~$7$^$9$+!)(B "
                    "Discard your unsaved private JISYO? ")))
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (kill-buffer buf))
      (unless (skk-get-jisyo-buffer skk-jisyo 'nomsg)
        (skk-error "$B8D?M<-=q$r:FFI$_9~$_$9$k$3$H$,$G$-$^$;$s!*(B"
                   "Cannot reread private JISYO!")))))

(defun skk-record-jisyo-data ()
  "$B8D?M<-=q$K4X$9$kE}7W>pJs$r(B `skk-record-file' $B$KJ]B8$9$k!#(B"
  (unless (or (not skk-keep-record)
              (> 1 skk-kakutei-count))
    (with-temp-file skk-record-file
      (insert-file-contents skk-record-file)
      (goto-char (point-min))
      (insert (format
               "%s  $BEPO?(B: %3d  $B3NDj(B: %4d  $B3NDjN((B: %3d%%  $B8l?t(B:%6d\n"
               (current-time-string)
               skk-touroku-count
               skk-kakutei-count
               (/ (* 100 (- skk-kakutei-count skk-touroku-count))
                  skk-kakutei-count)
               (cond
                ((featurep 'skk-rdbms)
                 ;; RDBMS $B$r;H$($P$b$C$H6=L#?<$$E}7W$,<h$l$k$+$b$7$l$J$$(B
                 ;; $B$,!"$H$j$"$($:8l?t$@$1?t$($FF~$l$F$*$/!#(B
                 (skk-rdbms-count-jisyo-candidates
                  skk-rdbms-private-jisyo-table))
                (skk-count-private-jisyo-candidates-exactly
                 (skk-count-jisyo-candidates
                  (expand-file-name (skk-jisyo))))

                ;; 1 $B9T(B 1 $B8uJd$H$_$J$9!#(B
                (t
                 (with-current-buffer (skk-get-jisyo-buffer
                                       skk-jisyo 'nomsg)
                   (- (count-lines (point-min) (point-max))
                      2))))))
      (when (integerp skk-keep-record)
        (setq selective-display nil)
        (widen)
        (goto-char (point-min))
        (forward-line skk-keep-record)
        (delete-region (point) (point-max))))
    (setq skk-touroku-count 0
          skk-kakutei-count 0)))

(defun skk-count-jisyo-candidates (file-or-table)
  "SKK $B<-=q$N8uJd?t$r?t$($k!#(B"
  (interactive
   (list (cond
          ((eq skk-count-jisyo-candidates-function
               'skk-count-jisyo-candidates-original)
           (read-file-name
            (format "Jisyo file: (default: %s) " (skk-jisyo))
            default-directory (skk-jisyo) 'confirm))
          ((eq skk-count-jisyo-candidates-function
               'skk-rdbms-count-jisyo-candidates)
           ;; $B%G!<%?%Y!<%9%U%!%$%k$rD>@\%U%!%$%kL>$G;XDj$G$-$k(B
           ;; permission $B$,$J$$>l9g$,B?$$$h$M(B...$B!#(B
           ;;(read-file-name
           ;; (format "Jisyo table: (default: %s) "
           ;;    skk-rdbms-private-jisyo-table))
           skk-rdbms-private-jisyo-table))))
  ;; mule@emacs19.31 $B$@$H2<5-$N$h$&$K$9$k$H(B (`$B%!(B' $B$,860x$N$h$&(B) $B2?8N$+(B
  ;; default-directory $B$NKvHx$K2~9T$,IU$/!#(B
  ;; $BDL>o$O5$$,IU$+$J$$$,!"(Brsz-mini.el $B$r;H$C$F(B resize-minibuffer-mode $B$r(B
  ;; non-nil $B$K$7$F$$$k$HITMW$J(B 2 $B9TL\$,=P8=$9$k!#(B
  ;; (interactive "f$B<-=q%U%!%$%k(B: ")
  (let ((count (funcall skk-count-jisyo-candidates-function
                        file-or-table)))
    (if (called-interactively-p 'interactive)
        (message (if (= count 1)
                     "%d candidate"
                   "%d candidates")
                 count)
      count)))

(defun skk-count-jisyo-candidates-original (file)
  "SKK $B<-=q$N8uJd?t$r?t$($k!#(B
`[' $B$H(B `]' $B$K0O$^$l$?Aw$j2>L>Kh$N%V%m%C%/Fb$O?t$($J$$!#(B"
  (let ((orig-buffer (find-buffer-visiting file)))
    (with-current-buffer (let ((find-file-visit-truename t))
                           (or orig-buffer (find-file-noselect file)))
      (save-match-data
        (let ((count 0)
              (min (point-min))
              (max (and (called-interactively-p 'interactive) (point-max)))
              (interactive-p (called-interactively-p 'interactive)))
          (goto-char min)
          (unless (and
                   ;; $B$3$A$i$O(B skk-save-point $B$r;H$o$:!"%]%$%s%H$r0\F0$5$;$k!#(B
                   (re-search-forward "^;; okuri-ari entries.$" nil t nil)
                   (skk-save-point
                    (re-search-forward "^;; okuri-nasi entries.$" nil t nil)))
            (skk-error "$B$3$N%U%!%$%k$O(B SKK $B<-=q$G$O$"$j$^$;$s(B"
                       "This file is not an SKK dictionary"))
          (beginning-of-line)
          (while (looking-at ";")
            (forward-line 1)
            (beginning-of-line))
          (search-forward " " nil t)
          (while (search-forward "/" nil t)
            (cond ((or (eolp)
                       (looking-at "\\["))
                   (forward-line 1)
                   (beginning-of-line)
                   (while (looking-at ";")
                     (forward-line 1)
                     (beginning-of-line))
                   (search-forward " " nil t))
                  (t
                   (setq count (1+ count))))
            (when interactive-p
              (message "Counting jisyo candidates...%3d%% done"
                       (/ (* 100 (- (point) min)) max))))
          (prog1
              count
            (unless orig-buffer
              (kill-buffer (current-buffer)))))))))

;;;###autoload
(defun skk-create-file (file &optional japanese english modes)
  "FILE $B$,$J$1$l$P!"(BFILE $B$H$$$&L>A0$N6u%U%!%$%k$r:n$k!#(B
$B%*%W%7%g%J%k0z?t$N(B JAPANESE/ENGLISH $B$r;XDj$9$k$H!"%U%!%$%k:n@.8e$=$N%a%C%;!<%8(B
$B$r%(%3!<%(%j%"$KI=<($9$k!#(B"
  (let ((file (expand-file-name file)))
    (if (file-exists-p file)
        (when modes
          (set-file-modes file modes))
      (write-region 1 1 file nil 0)
      (when modes
        (set-file-modes file modes))
      (when (or japanese english)
        (message "%s"
                 (if skk-japanese-message-and-error
                     japanese
                   english))
        (sit-for 3)))))

;;;###autoload
(defun skk-get-jisyo-buffer (file &optional nomsg)
  "FILE $B$r3+$$$F(B SKK $B<-=q%P%C%U%!$r:n$j!"%P%C%U%!$rJV$9!#(B
$B<-=q%P%C%U%!$K$O(B `skk-jisyo-code' $B$,E,MQ$5$l$k(B (nil $B$G$"$l$P(B euc) $B$,!"(BFILE $B$K(B (\"path/to/file\" . CODING-SYSTEM) $B$N%3%s%9%;%k$b;XDj$G$-$k!#(B
$B%*%W%7%g%J%k0z?t$N(B NOMSG $B$r;XDj$9$k$H%U%!%$%kFI$_9~$_$N:]$N%a%C%;!<%8$rI=<($7$J(B
$B$$!#(B"
  (when file
    (let* ((inhibit-quit t)
           (code (skk-find-coding-system (cond ((consp file)
                                                (cdr file))
                                               ((string= file (skk-jisyo))
                                                (skk-jisyo t))
                                               (t
                                                skk-jisyo-code))))
           (file (or (car-safe file)
                     file))
           (enable-character-translation
            (not (memq code '(euc-japan shift_jis junet))))
           (buf-name (concat " *"
                             (file-name-nondirectory file)
                             "*"))
           (buf (get-buffer buf-name)))
      ;; $B<-=q%P%C%U%!$H$7$F%*!<%W%s$5$l$F$$$k$J$i!"2?$b$7$J$$!#(B
      (unless (buffer-live-p buf)
        (setq buf (get-buffer-create buf-name))
        (setq file (expand-file-name file))
        (with-current-buffer buf
          (buffer-disable-undo)
          (auto-save-mode -1)
          ;; $B%o!<%-%s%0%P%C%U%!$N%b!<%I%i%$%s$O%"%C%W%G!<%H$5$l$J$$!)(B
          ;;(make-local-variable 'line-number-mode)
          ;;(make-local-variable 'column-number-mode)
          ;;(setq column-number-mode nil
          ;;      line-number-mode nil)
          (setq buffer-read-only nil
                case-fold-search nil
                ;; buffer-file-name $B$r(B nil $B$K$7$F$*$/$H(B M-x compile $B$J$I(B
                ;; $BFbIt$G(B save-some-buffers $B$r%3!<%k$7$F$$$k%3%^%s%I$r(B
                ;; $B;H$C$?$H$-$G$b%;!<%V$9$k$+$I$&$+$r?R$M$F$3$J$/$J$k!#(B
                ;; buffer-file-name file
                ;; cache-long-line-scans nil
                ;; dabbrev $B$N%5!<%A$H$J$k%P%C%U%!$K$J$i$J$$$h$&$KB8:_$7(B
                ;; $B$J$$%b!<%IL>$K$7$F$*$/!#<B32$N$"$kI{:nMQ$O$J$$$O$:!#(B
                major-mode 'skk-jisyo-mode
                mode-name "SKK dic")
          (unless nomsg
            (skk-message "SKK $B<-=q(B %s $B$r%P%C%U%!$KFI$_9~$s$G$$$^$9(B..."
                         "Inserting contents of %s ..."
                         (file-name-nondirectory file)))
          (condition-case nil
              (let ((coding-system-for-read code)
                    format-alist)
                (insert-file-contents file))
            (error
             (when (buffer-live-p buf)
               (kill-buffer buf))
             (skk-error "`%s'$B$rFI$_9~$a$^$;$s(B" "Cannot load `%s'." file)))
          (unless nomsg
            (skk-message "SKK $B<-=q(B %s $B$r%P%C%U%!$KFI$_9~$s$G$$$^$9(B...$B40N;!*(B"
                         "Inserting contents of %s ...done"
                         (file-name-nondirectory file)))
          (skk-setup-jisyo-buffer)
          (set-buffer-modified-p nil)))
      buf)))

;;;###autoload
(defun skk-search ()
  "`skk-current-search-prog-list' $B$N3FMWAG!J8!:w%W%m%0%i%`!K$r=g$KI>2A$9$k!#(B
$B$$$:$l$+$NMWAG$,8uJd$r8+$D$1$?;~E@$G=*N;$9$k!#(B"
  ;; $B8D?M<-=q$G8uJd$,8+$D$+$l$P$=$l$rJV$9!J(BL $B<-=q$^$G8+$K$$$/Lu$G$O$J$$!K(B
  (let (l prog)
    (while (and (null l)
                skk-current-search-prog-list)
      (setq prog (car skk-current-search-prog-list))
      (setq l (if (and skk-use-numeric-conversion
                       (string-match "[0-9]" skk-henkan-key)
                       (skk-numeric-program-p prog))
                  ;; -- 12.2.1 $B$+$i$NJQ99(B --
                  ;; $B?tCMJQ49;~$K!"Hs?tCMJQ49$bF1;~$K8!:w$7$F8uJd$K(B
                  ;; $B4^$a$k!#(B
                  (skk-nunion (let (skk-use-numeric-conversion)
                                (eval prog))
                              (eval prog))
                (let (skk-use-numeric-conversion)
                  (eval prog))))
      (setq skk-current-search-prog-list (cdr skk-current-search-prog-list)))
    (setq skk-search-state (list skk-henkan-key prog l))
    l))

(defun skk-search-state ()
  (interactive)
  (with-output-to-temp-buffer "*skk search state*"
    (with-current-buffer standard-output
      (insert (format "skk-henkan-key: %s\n" (nth 0 skk-search-state))
              (format "skk-search-prog: %s\n" (nth 1 skk-search-state))
              (format "skk-search() result: %s\n\n" (nth 2 skk-search-state)))
      (when (equal (nth 1 skk-search-state)
                   '(skk-search-extra-jisyo-files))
        (mapconcat #'(lambda (x)
                       (insert (format "%s\n" x)))
                   skk-search-ex-state "")))))

(defun skk-numeric-program-p (program)
  "$B<-=q8!:w%W%m%0%i%`(B PROGRAM $B$,?tCMJQ49M-8z$+$I$&$+H=Dj$9$k!#(B
$B$b$7%W%m%0%i%`$,(B `skk-non-numeric-prog-list' $B$K;XDj$5$l$F$$$?$i(B nil $B$rJV$9!#(B
$B$5$b$J$1$l$P(B non-nil $B$rJV$9!#(B"
  (not (or (memq (car program) skk-non-numeric-prog-list)
           (member program skk-non-numeric-prog-list))))

(defun skk-search-jisyo-file (file limit &optional nomsg)
  "SKK $B<-=q%U%)!<%^%C%H$N(B FILE $B$G(B `skk-henkan-key' $B$r%-!<$K$7$F8!:w$9$k!#(B
$B8!:wNN0h$,(B LIMIT $B0J2<$K$J$k$^$G%P%$%J%j%5!<%A$r9T$$!"$=$N8e%j%K%"%5!<%A$r9T$&!#(B
LIMIT $B$,(B 0 $B$G$"$l$P!"%j%K%"%5!<%A$N$_$r9T$&!#(B
$B<-=q$,%=!<%H$5$l$F$$$J$$>l9g$O(B LIMIT $B$r(B 0 $B$H$9$kI,MW$,$"$k!#(B
$B%*%W%7%g%J%k0z?t$N(B NOMSG $B$,(B non-nil $B$G$"$l$P(B `skk-get-jisyo-buffer' $B$N(B
$B%a%C%;!<%8$r=PNO$7$J$$!#(B

FILE $B$K$O<-=q%U%!%$%k$@$1$G$J$/!"(B
  ($B<-=q%U%!%$%k(B . $B%3!<%G%#%s%0%7%9%F%`(B)
$B$N%Z%"$b<u$1$D$1$k!#%3!<%G%#%s%0%7%9%F%`$O(B `skk-jisyo-code' $B$H(B
$BF1MM$K;XDj$9$k!#(B"
  (skk-search-jisyo-buf (skk-get-jisyo-buffer file nomsg)
                        limit))

(defun skk-search-extra-jisyo-files ()
  ;; skk-search-prog-list $B$NMWAG$N$R$H$D(B
  (setq skk-search-ex-state nil)
  (let (candidates words)
    (dolist (file skk-extra-jisyo-file-list)
      (setq words (skk-search-jisyo-file file 10000))
      (when words
        (add-to-list 'skk-search-ex-state (cons file words)))
      (setq candidates (nconc candidates words)))
    candidates))

(defun skk-search-itaiji ()
  ;; skk-search-prog-list $B$NMWAG$H$7$F;HMQ$9$k(B
  (and (= (length skk-henkan-key) 1)
       skk-itaiji-jisyo
       (skk-search-jisyo-file skk-itaiji-jisyo 0 t)))

(defun skk-search-server (file limit &optional nomsg)
  "$B<-=q%5!<%P$r;HMQ$7$F(B `skk-henkan-key' $B$r%-!<$K$7$F8!:w$9$k!#(B
$B<-=q%5!<%P$,;HMQ$G$-$J$$$H$-$O!"(BFILE $B$r%P%C%U%!$KFI$_9~$s$G8!:w$9$k!#(B
LIMIT $B$H(B NOMSG $B$O<-=q%5!<%P$,;HMQ$G$-$J$$$H$-$N$_M-8z!#(B
$B$3$l$i$N0z?t$K$D$$$F$O(B `skk-search-jisyo-file' $B$r;2>H$N$3$H!#(B"
  (if (or skk-server-host
          skk-servers-list)
      (skk-search-server-1 file limit)
    ;; $B<-=q%5!<%P$,MxMQ2DG=$G$J$1$l$P(B file $B$r8!:w$9$k!#(B
    ;; $B0z?t(B file $B$ODL>o(B `skk-aux-large-jisyo' $B$,;XDj$5$l$k!#(B
    (when (and (stringp file)
               (file-readable-p file))
      (skk-search-jisyo-file file limit nomsg))))

(defun skk-okuri-search ()
  "$B8+=P$78l$rAw$j2>L>$r4^$`$b$N$H$7$F8!:w$9$k!#(B
$BNc$($P!"(B`skk-auto-okuri-process' $B$,(B non-nil $B$J$i$P(B \"Uresii\" $B$N$h$&$KAw$j2>(B
$BL>$b4^$a$F%?%$%W$7$F$bAw$j$"$j$N(B \"$B4r$7$$(B\" $B$rC5$7=P$9!#(B"
  (when skk-auto-okuri-process
    (skk-okuri-search-1)))

(defun skk-search-jisyo-buf (buf limit)
  "$B%P%C%U%!$r(B BUF $B$K0\F0$7$F!"$=$3$r<-=q$H$7$F8!:w$9$k!#(B"
  (when (buffer-live-p buf)
    ;; skk-henkan-key $B$H(B skk-henkan-okurigana $B$O%+%l%s%H%P%C%U%!$N(B
    ;; $B%m!<%+%kCM$J$N$G!"$"$i$+$8$a<hF@!#(B
    (let ((okurigana (or skk-henkan-okurigana
                         skk-okuri-char))
          (midasi (if skk-use-numeric-conversion
                      (skk-num-compute-henkan-key skk-henkan-key)
                    skk-henkan-key))
          (henkan-buffer (current-buffer))
          words-list)
      (with-current-buffer buf
        (setq skk-henkan-key midasi
              words-list (skk-search-jisyo okurigana limit))
        (skk-select-words-from-list words-list
                                    henkan-buffer
                                    midasi
                                    okurigana)))))

(defun skk-search-jisyo (okurigana limit &optional delete)
  "$B%+%l%s%H%P%C%U%!$r<-=q$H$7$F8!:w$9$k!#(B
$B$3$N4X?t$NLa$jCM$O!"(B`skk-henkan-key' $B$r%-!<$H$7$F(B `skk-compute-henkan-lists' $B$r(B
$BI>2A$7$?CM$G$"$k!#(B
DELETE $B$,(B non-nil $B$G$"$l$P(B `skk-henkan-key' $B$K%^%C%A$9$k%(%s%H%j$r:o=|$9$k!#(B"

  ;; (let ((skk-henkan-key "$B$[$+$s(B"))
  ;;   (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
  ;;     (skk-search-jisyo nil 0)))
  ;; => (("$BJd4V(B" "$BJd40(B" "$BJ]4I(B") nil nil nil)

  ;; (let ((skk-henkan-key "$B$&$4(Bk"))
  ;;   (with-current-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)
  ;;     (skk-search-jisyo "$B$/(B" 0)))
  ;; => (("$BF0(B") ("[$B$/(B") ("$BF0(B") ("]"))

  (let ((key (concat "\n" skk-henkan-key " /"))
        min max size p)
    (save-match-data
      ;; skk-okuri-ari-min $B$H(B skk-okuri-ari-max $B$O<-=q%P%C%U%!$N%m!<%+%kCM!#(B
      (if okurigana
          (setq min skk-okuri-ari-min
                max skk-okuri-ari-max)
        (setq min skk-okuri-nasi-min
              max (point-max)))
      (when (> limit 0)
        ;; $BFsJ,C5:w(B
        (while (> (setq size (- max min)) limit)
          (goto-char (+ min (/ size 2)))
          (beginning-of-line)
          (setq p (point))
          (if (= p min)
              (setq max min)    ; return
            (let ((p-is-further
                   ;; $BAw$j$"$j$J$i5U=g$KHf3S$9$k!#(B
                   (if okurigana
                       (skk-string< (buffer-substring-no-properties
                                     p (1- (search-forward  " ")))
                                    skk-henkan-key)
                     (skk-string< skk-henkan-key
                                  (buffer-substring-no-properties
                                   p (1- (search-forward " ")))))))
              (if p-is-further
                  (setq max p)
                (setq min p))))))
      (goto-char min)
      ;; key $B$,8!:w3+;OCOE@$K$"$C$?>l9g$G$b8!:w2DG=$J$h$&$K0lJ8;zLa$k!#(B
      ;; key $B$N@hF,ItJ,$K(B "\n" $B$,4^$^$l$F$$$k$3$H$KCm0U!#(B
      (unless (bobp)
        (backward-char 1))
      ;; case-fold-search $B$O!"<-=q%P%C%U%!$G$O>o$K(B nil$B!#(B
      (when (search-forward key max 'noerror)
        (prog1
            (skk-compute-henkan-lists okurigana)
          (when delete
            (beginning-of-line)
            (delete-region (point)
                           (progn
                             (forward-line 1)
                             (point)))))))))

(defun skk-select-words-from-list (list buffer midasi okurigana)
  "`skk-search-jisyo' $B$,JV$7$?8uJd%j%9%H$+$i8=:_MW5a$5$l$F$$$k8uJd$rA*$S$@$9!#(B"
  (when list
    (let ((words
           (cond
            ((and okurigana
                  skk-henkan-okuri-strictly)
             ;; $BAw$j2>L>$,F10l$N8uJd$N$_$rJV$9!#(B
             (nth 2 list))
            ((and okurigana
                  skk-henkan-strict-okuri-precedence)
             ;; $BAw$j2>L>$,F10l$N8uJd$N$&$7$m$K!"(B
             ;; $B$=$NB>$N8uJd$r$D$1$F$+$($9!#(B
             (skk-nunion (nth 2 list)
                         (car list)))
            (t
             (car list)))))
      (dolist (function skk-search-end-function)
        (setq words (funcall function buffer midasi okurigana words)))
      words)))

;;;###autoload
(defun skk-compute-henkan-lists (okurigana)
  "$B<-=q8uJd72$r#4$D$N%j%9%H$KJ,2r$9$k!#(B
$B>\$7$/$O!"$3$N4X?t$N%3%a%s%H$r;2>H!#(B"
  ;; $BAw$j$J$7(B ($BNc$($P!"<-=q%(%s%H%j(B "$B$F$s$5$$(B /$BE>:\(B/$BE7:R(B/$BE7:M(B/" $B$N=hM}(B)
  ;;   words1 := ("$BE>:\(B" "$BE7:R(B" "$BE7:M(B") == $BA48uJd72(B
  ;;   words2 := nil
  ;;   words3 := nil
  ;;   words4 := nil
  ;;
  ;; (with-temp-buffer
  ;;   (insert "$B$F$s$5$$(B /$BE>:\(B/$BE7:R(B/$BE7:M(B/")
  ;;   (goto-char (point-min))
  ;;   (search-forward " /")
  ;;   (skk-compute-henkan-lists ""))
  ;; => (("$BE>:\(B" "$BE7:R(B" "$BE7:M(B") nil nil nil)

  ;; $BAw$j$"$j(B ($BNc$($P!"!V5c$/!W$NJQ49$r9T$C$?>l9g$N!"<-=q%(%s%H%j(B
  ;;           "$B$J(Bk /$BK4(B/$BL5(B/$BLD(B/$B5c(B/[$B$/(B/$BL5(B/$BLD(B/$B5c(B/]/[$B$-(B/$BK4(B/]/" $B$N=hM}(B)
  ;;   words1 := ("$BK4(B" "$BL5(B" "$BLD(B" "$B5c(B")  == $B4A;zItJ,$NA48uJd72(B
  ;;   words2 := ("[$B$/(B")               == $BB>$NAw$j2>L>$r;H$&4A;z8uJd72(B ($B$"$l(B
  ;;                                       $B$P(B) + $B:#2s$NJQ49$NAw$j2>L>ItJ,(B
  ;;   words3 := ("$BL5(B" "$BLD(B" "$B5c(B")       == $B:#2s$NJQ49$NAw$j2>L>$r;H$&2DG=@-$N(B
  ;;                                       $B$"$kA44A;z8uJd72(B
  ;;   words4 := ("]" "[$B$-(B" "$BK4(B" "]")   == $BB>$NAw$j2>L>$r;H$&4A;z8uJd72(B ($B;D(B
  ;;                                       $B$j!#$"$l$P(B)
  ;;
  ;; (with-temp-buffer
  ;;   (insert "$B$J(Bk /$BK4(B/$BL5(B/$BLD(B/$B5c(B/[$B$/(B/$BL5(B/$BLD(B/$B5c(B/]/[$B$-(B/$BK4(B/]/")
  ;;   (goto-char (point-min))
  ;;   (search-forward " /")
  ;;   (skk-compute-henkan-lists "$B$/(B"))
  ;; => (("$BK4(B" "$BL5(B" "$BLD(B" "$B5c(B") ("[$B$/(B") ("$BL5(B" "$BLD(B" "$B5c(B") ("]" "[$B$-(B" "$BK4(B" "]"))
  ;;
  ;;   * "[" $B$OD>8e$KB3$/$R$i$,$J$rAw$j2>L>$K;}$D4A;z$N8uJd72$N;O$^$j$rI=$7!"(B
  ;;     "]" $B$O!"3:Ev$NAw$j2>L>%0%k!<%W$N=*$j$r<($9!#(B

  ;; $B$3$N4X?t$O!"JQ49;~$H!"3NDjD>8e$N<-=q$N%"%C%W%G!<%H;~$N#2EY8F$P$l$k(B
  ;; ($BJQ49;~$K8!:w$r9T$C$?<-=q$,!"(Bskk-jisyo $B$H$O8B$i$J$$$N$G!"#2EY7W;;$;$6$k(B
  ;; $B$rF@$J$$(B)$B!#(B
  ;;
  ;; $BJQ49;~$O!"(B
  ;; o skk-henkan-okuri-strictly $B$,(B non-nil $B$G$"$l$P!"7W;;7k2L$N(B words3 $B$r!"(B
  ;; o skk-henkan-okuri-strictly $B$,(B nil $B$G$"$C$F(B
  ;;   - skk-henkan-strict-okuri-precedence $B$,(B non-nil $B$"$l$P(B (skk-nunion words3 words1) $B$r(B
  ;;   - skk-henkan-strict-okuri-precedence $B$,(B nil $B$N>l9g$O(B words1 $B$r(B
  ;; $B<h$j=P$9!#(B
  (cond
   ((not okurigana)
    (list (split-string (buffer-substring-no-properties (point) (1- (line-end-position)))
                        "/")
          nil nil nil))
   (t
    (save-match-data
      (let ((stage 1) q1 q2 q3 q4
            (okuri-key (concat "\[" okurigana))
            item headchar)

        (while (not (eolp))
          (setq item (buffer-substring-no-properties (point) (1- (search-forward "/")))
                headchar (if (string= item "")
                             (identity 0)
                           (aref item 0)))
          (cond ((and (eq headchar ?\[)
                      (<= stage 2))
                 (setq item (skk-compute-henkan-lists-sub-adjust-okuri item okuri-key))
                 (if (string= item okuri-key)
                     (progn
                       (setq q2 (cons item q2))
                       (setq stage 3))
                   (setq stage 2)
                   (setq q2 (cons item q2))))
                ((= stage 1)
                 (setq q1 (cons item q1)))
                ((= stage 2)
                 (setq q2 (cons item q2)))
                ((= stage 3)
                 (if (eq headchar ?\])  ; ?\]
                     (progn
                       (setq stage 4)
                       (setq q4 (cons item q4)))
                   (setq q3 (cons item q3))))
                ((= stage 4)
                 (setq q4 (cons item q4)))))

        (list (nreverse q1)       ; words1
              (nreverse q2)       ; words2
              (nreverse q3)       ; words3
              (nreverse q4))))))) ; words4

(defun skk-compute-henkan-lists-sub-adjust-okuri (item &optional okuri-key)
  ;; Yet to be elucidated.
  item)

;;;###autoload
(defun skk-remove-duplicates (list)
  "LIST $B$+$i!"=EJ#$9$kMWAG$r=|30$7$?%j%9%H$rJV$9!#(B"
  (let (new)
    (dolist (x list)
      (or (member x new)
          (setq new (cons x new))))
    (nreverse new)))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  "SKK $B<-=q%U%)!<%^%C%H$N(B FILE $B$+$i8!:w$9$k!#(B
$B8uJd$r8+$D$1$?>l9g$O!"Bg0hJQ?t(B `skk-kakutei-henkan-flag' $B$K(B non-nil $B$rBeF~$9$k!#(B
$B0z?t$K$D$$$F$O(B `skk-search-jisyo-file' $B$r;2>H!#(B

$BMxMQ$9$k>l9g$O(B `skk-search-prog-list' $B$N@hF,$KG[$9$k;v!#(B"
  (setq skk-kakutei-henkan-flag (skk-search-jisyo-file file limit nomsg)))

;;;###autoload
(defun skk-update-jisyo (word &optional purge)
  (funcall skk-update-jisyo-function word purge)
  (when (and skk-save-jisyo-instantly
             (or skk-jisyo-updated  ; skk-henkan-in-minibuff $B$G(B setq
                 purge))
    (skk-save-jisyo 'quiet)
    (setq skk-jisyo-updated nil)))

;;;###autoload
(defun skk-update-jisyo-original (word &optional purge)
  "$B<!$NJQ49;~$K(B WORD $B$,:G=i$N8uJd$K$J$k$h$&$K!"8D?M<-=q$r99?7$9$k!#(B
PURGE $B$,(B non-nil $B$G(B WORD $B$,6&M-<-=q$K$"$k8uJd$J$i(B `skk-ignore-dic-word'
$B4X?t$G%/%)!<%H$7$?8uJd$r8D?M<-=q$K:n$j!"<!$NJQ49$+$i=PNO$7$J(B
$B$$$h$&$K$9$k!#(B
WORD $B$,6&M-<-=q$K$J$1$l$P!"8D?M<-=q$N<-=q%(%s%H%j$+$i:o=|$9$k!#(B"
  ;;
  ;; $B8D?M<-=q$N%(%s%H%j(B
  ;;         ;; okuri-ari entries.
  ;;  $BJQ$G(B   $B$G(Bt /$B=P(B/[$B$F(B/$B=P(B/]/[$B$?(B/$B=P(B/]/
  ;;  $B49>:(B   $B$D(Bi /$BIU(B/[$B$$(B/$BIU(B/]/
  ;;  $B=g=g(B   $B$1(Bs /$B>C(B/[$B$9(B/$B>C(B/]/[$B$7(B/$B>C(B/]/[$B$;(B/$B>C(B/]/[$B$5(B/$B>C(B/]/
  ;;   $B"-(B    $B$+$((Bs /$BJV(B/[$B$7(B/$BJV(B/]/[$B$9(B/$BJV(B/]/[$B$5(B/$BJV(B/]/[$B$;(B/$BJV(B/]/
  ;;         ...
  ;;         ...
  ;;         $B$J$,(Bs /$BD9(B/$BN.(B/[$B$7(B/$BN.(B/]/[$B$5(B/$BD9(B/]/[$B$=(B/$BN.(B/]/
  ;;         ;; okuri-nasi entries.
  ;;  $BJQ$G(B   $B$8$g$&$?$$(B /$B>uBV(B/
  ;;  $B49>:(B   $B$=$&$K$e$&(B /$BA^F~(B/
  ;;  $B=g=g(B   $B$+$J(B /$B2>L>(B/
  ;;   $B"-(B    ...
  ;;         ...
  ;;
  ;; skk-auto-okuri-process $B$,(B non-nil $B$N$H$-$K!"(Bskk-okuri-search $B$O8+=P$78l$N(B
  ;; $BD9$$=g$K8uJd$rJV$9I,MW$,$"$k!#(B
  ;; skk-okuri-search $B$,!"8+IU$1$?8uJd$r8+=P$78l$r%-!<$H$7$F>:=g$K%=!<%H$7$FJV(B
  ;; $B$9$?$a!"8D?M<-=q$N%=!<%H$OI,MW$G$J$$!#(B
  ;; $B$h$C$F!":G8e$KJQ49$7$?$b$N$r(B skk-okuri-ari-min $B$N0LCV$KA^F~$9$k!#(B
  ;;
  (let* ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
         (cand (car (skk-treat-strip-note-from-word word)))
         (midasi (if (and (skk-numeric-p)
                          (or (string-match "#[0-9]" cand)
                              (skk-lisp-prog-p cand)))
                     (skk-num-compute-henkan-key skk-henkan-key)
                   skk-henkan-key))
         (henkan-buffer (and skk-update-end-function
                             (current-buffer))))
    ;; $BF~NOMzNr$r99?7$9$k!#(B
    ;; $BAw$j$"$jF~NO$O>JN,$7!"Aw$j$J$7F~NO$N$_MzNr$r$H$k!#(B
    (unless skk-henkan-okurigana
      (skk-update-kakutei-history midasi word))

    (when jisyo-buffer
      (let ((inhibit-quit t)
            buffer-read-only old-words-list okurigana)
        (when (> skk-okuri-index-min -1)
          (setq word (skk-remove-common word)
                ;; skk-henkan-key $B$O(B skk-remove-common $B$K$h$C$F(B
                ;; $BJQ99$5$l$F$$$k2DG=@-$,$"$k!#(B
                midasi skk-henkan-key))
        (setq okurigana (or skk-henkan-okurigana
                            skk-okuri-char))
        (with-current-buffer jisyo-buffer
          ;; $B4{B8%(%s%H%j$r8!:w8e>C5n$9$k!#A^F~$9$Y$-8uJd$,(B words1 $B$K(B 1 $B$D(B
          ;; $B$7$+$J$/!"(Bword $B$HF1$8J8;z$G$"$C$F$b!"$$$C$?$s>C$7$F$=$N%(%s%H(B
          ;; $B%j$r(B min $B%]%$%s%H$K0\F0$5$;$J$1$l$P$J$i$J$$!#$3$l$O!"FI$_$NJd(B
          ;; $B40$r9T$&$H$-$K!"(B min $B%]%$%s%H$+$i8+=P$7$rC5$9$?$a!"?7$7$$8+=P(B
          ;; $B$7$[$I!"(Bmin $B%]%$%s%H$K6a$$$H$3$m$K$J$1$l$P$J$i$J$$$+$i$G$"$k!#(B
          (setq skk-henkan-key midasi
                old-words-list (skk-search-jisyo okurigana 0 'delete))
          (skk-update-jisyo-1 okurigana word old-words-list purge)
          ;; $BJ#?t$N(B emacs $B$G(B SKK $B$,5/F0$5$l$F$$$k$H$-$K8D?M<-=q$r@09gE*$K(B
          ;; $B99?7$9$k$?$a$K3NDj$NF0:n$r5-O?$9$k!#(B
          (when (and (skk-share-private-jisyo-p)
                     (< skk-jisyo-save-count (length skk-jisyo-update-vector)))
            (aset skk-jisyo-update-vector skk-update-jisyo-count
                  (list midasi okurigana word purge)))
          (dolist (function skk-update-end-function)
            (funcall function henkan-buffer midasi okurigana word purge))
          (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
          ;; skk-share-private-jisyo $B$,(B non-nil $B$N$H$-$O(B skk-jisyo-save-count
          ;; $B$b(B non-nil $B$G$"$k$3$H$rA0Ds$H$9$k(B
          (when (and skk-jisyo-save-count
                     (<= skk-jisyo-save-count skk-update-jisyo-count))
            ;; auto save.
            (skk-save-jisyo 'quiet)))))))

(defun skk-update-jisyo-1 (okurigana word old-words-list purge)
  "$B8D?M<-=q$K?7$7$$%(%s%H%j$rA^F~$9$k!#(B
$B4{B8%(%s%H%j$+$i7W;;$7$?(B words[1-4] $B$NCM$H!":#2s$NJQ49$N7k2L(B word $B$H$r(B
$B7k9g$7$F!"?7$?$J%(%s%H%j$r7W;;$7!"A^F~$9$k!#(B"
  ;; $BF~NO$H$J$k(B OLD-WORDS-LIST $B$O!"4X?t(B `skk-search-jisyo' $B$,=PNO$7$?$b$N$G$"$k!#(B

  (let ((words1 (car   old-words-list))
        (words2 (nth 1 old-words-list))
        (words3 (nth 2 old-words-list))
        (words4 (nth 3 old-words-list)))
    (cond ((not purge)
           ;; words1 $B$r99?7(B
           (if skk-jisyo-fix-order
               (if (consp words1)
                   ;; $B?75,$N;~$O!"B>$NF12;8l$N8e$KDI2C(B
                   (unless (member word words1)
                     (setcdr (last words1) (cons word nil)))
                 ;; $B:#2s$NFI$_$N8l!"$=$N$b$N$,?75,(B
                 (setq words1 (cons word nil)))
             ;; words1 $B$N@hF,$N8uJd$r(B word $B$K$9$k!#(B
             (setq words1 (cons word (delete word words1)))))

          ;; $BAw$j$J$7!"$b$7$/$O(B skk-henkan-okuri-strictly $B$H(B
          ;; skk-henkan-strict-okuri-precedence $B$,(B nil $B$N>l9g!#(B
          (t
           ;; words1 $B$r(B purge$B!#6&MQ<-=q$K$"$k8uJd$@$C$?$i!"(B
           ;; skk-ignore-dic-word $B$G%/%)!<%H$7$F<!$NJQ49$+$i=PNO(B
           ;; $B$7$J$$$h$&$K$9$k!#6&MQ<-=q$K$J$$J8;zNs$O(B word $B$r>C$9!#(B
           (setq words1 (if (skk-public-jisyo-has-word-p okurigana word)
                            (skk-compose-ignore-word words1 word)
                          (delete word words1)))))

    (when words1 ;; words1 $B$,(B null $B$G$"$l$P!"$b$&2?$b$9$k$3$H$O$J$$!#(B
      (goto-char (if okurigana
                     skk-okuri-ari-min
                   skk-okuri-nasi-min))
      (insert "\n" skk-henkan-key " /")
      ;; words1 -- $BA48uJd72(B ($BAw$j$J$7$N>l9g(B) $B!"$^$?$O(B
      ;;           $BA48uJd72$N4A;zItJ,(B ($BAw$j$"$j$N>l9g(B)
      (insert (skk-update-jisyo-2 words1) "/")

      (when okurigana
        ;; words2 $B0J9_$N8uJd72$r=hM}$9$k$N$O!"Aw$j$"$j$N>l9g$N$_!#(B
        ;; $B@h$KA^F~$9$Y$-8uJd72$r7W;;!"D4@0$9$k!#(B
        (cond (words3
               (cond ((not purge)
                      (if skk-jisyo-fix-order
                          (unless (member word words3)
                            (setcdr (last words3) (cons word nil)))
                        (setq words3 (cons word (delete word words3)))))
                     (t
                      (setq words3 (delete word words3))
                      (when (null words3)
                        ;; words3 $B$H$7$FA^F~$9$k$b$N$,A4$/$J$1$l$P!"(B"/[$B$/(B/]/" $B$N$h(B
                        ;; $B$&$JAw$j2>L>$N$_$N8uJd$r:n$i$J$$$h$&$K$9$k(B ($BI,MW$G(B
                        ;; $B$"$l$P!"(Bwords2 $B$N:G8eJ}$H(B) words4 $B$N@hF,$N(B "]" $B$r:o=|!#(B
                        (let* ((len (length words2))
                               (last2 (cl-case len
                                        (0 nil)
                                        (1 (list nil (car words2)))
                                        (t (nthcdr (- (length words2) 2)
                                                   words2)))))
                          ;; words2 $B$N:G8eJ}$O>o$K(B "[$BAw$j2>L>(B" $B$H$O8B$i$J$$!#(B
                          (when (and last2
                                     (string= (nth 1 last2) (concat "[" okurigana)))
                            (cl-case len
                              (1 (setq words2 nil))
                              (t (setcdr last2 nil))))
                          ;; words4 $B$N@hF,$O>o$K(B "]"$B!#(B
                          (setq words4 (cdr words4)))))))
              (t
               ;; words3 $B$,(B null $B$G$"$l$P(B
               (unless (or skk-process-okuri-early
                           purge)
                 ;; skk-process-okuri-early $B$,(B non-nil $B$J$iAw$j2>L>$,J,$i$J$$$N$G(B
                 ;; $B2?$b$7$J$$!#(B-- $B:#2s;HMQ$7$?Aw$j2>L>$,$o$+$i$J$$$^$^JQ49$7$F$$(B
                 ;; $B$k$N$G!"A4$F$N8uJd$,(B words2 $B$KF~$C$F$$$k(B -- words3, words4 $B$O(B
                 ;; null$B!#(B
                 ;; words3 $B$H$7$FA^F~$9$k$b$N$,A4$/$J$1$l$P!"2?$b$7$J$$(B -- words3
                 ;; $B$,(B purge $BA0$+$i(B null $B$J$i!"(Bwords2 $B$NKvHx$O(B "[" $B$G$J$$$7!"(B
                 ;; words4 $B$O(B null $B$@$+$i(B words[234] $B$NA`:n$OITMW!#(B
                 (setq words2 (nconc words2
                                     (list (concat "[" okurigana)))
                       words3 (list word)
                       ;; purge $BA0$+$i(B words3 $B$,(B null $B$@$C$?$N$@$+$i(B
                       ;; words4 $B$b(B null$B!#(B
                       words4 (list "]"))))))
      (when words2
        ;; words2 -- $B:#2s;HMQ$7$J$+$C$?Aw$j2>L>$r;H$&4A;z$N8uJd72(B
        ;;         + "["
        ;;         + $B:#2s;HMQ$7$?Aw$j2>L>(B ($BAw$j2>L>$N$_!#$=$NAw$j(B
        ;;           $B2>L>$r;HMQ$9$k4A;z$N8uJd72$O!"(Bwords3 $B$K4^$^$l$k(B)
        (insert (skk-update-jisyo-2 words2) "/")

        ;; words2 $B$,(B null $B$J$i(B words3 $B$b(B null$B!#(B
        (when words3
          ;; words3 -- $B:#2s;HMQ$7$?Aw$j2>L>$r;H$&A44A;z8uJd(B
          (insert (skk-update-jisyo-2 words3) "/"))

        ;; purge $B$G(B words3 $B$,(B null $B$K$J$C$?>l9g$O(B words4 $B$,;D$C$F$$$k(B
        ;; $B$H$-$,$"$k!#(B
        (when words4
          ;; words4 -- "]" + $BB>$NAw$j2>L>$r;H$&A44A;z8uJd(B
          ;; (words2 $B$N;D$j(B)$B!#(B
          (insert (skk-update-jisyo-2 words4) "/"))))))

(defun skk-update-jisyo-2 (words)
  (mapconcat #'skk-quote-char
             words
             "/"))

;;;###autoload
(defun skk-quote-char (word)
  "WORD $B$r<-=q%(%s%H%j$H$7$F@5$7$$7A$K@07A$9$k!#(B
$B<-=q7A<0$N@)8B$+$i!"<-=q%(%s%H%jFb$K4^$a$F$O$J$i$J$$J8;z$,(B WORD $B$NCf$K$"$l$P!"(B
$BI>2A$7$?$H$-$K$=$NJ8;z$H$J$k$h$&$J(B Lisp $B%3!<%I$rJV$9!#(B"
  (save-match-data
    (cond ((and word
                (string-match "[/\n\r\"]" word)
                ;; we should not quote WORD if it is a symbolic expression
                (not (skk-lisp-prog-p word))
                ;; has an annotation
                (not (string-match ";" word)))
           (format "(concat \"%s\")"
                   (skk-quote-char-1 word (cdr skk-quote-char-alist))))
          (t
           word))))

;;;###autoload
(defun skk-quote-semicolon (word)
  "WORD $B$r<-=q%(%s%H%j$H$7$F@5$7$$7A$K@07A$9$k!#(B
`skk-quote-char' $B$H;w$F$$$k$,!"Cp<a$H4X78$J$$%;%_%3%m%s(B (;) $B$r=hM}$9$kE@$,(B
$B0[$J$k!#(B"
  ;; `save-match-data' $B$OMW$i$J$$!#(B
  (cond ((string-match ";" word)
         (format "(concat \"%s\")"
                 (skk-quote-char-1 word skk-quote-char-alist)))
        (t
         word)))

(defun skk-public-jisyo-has-word-p (okurigana word)
  "$B6&M-<-=q$,(B  WORD $B$r;}$C$F$$$k$+$I$&$+D4$Y$k!#(B
$B6&M-<-=q$,(B MIDASHI $B5Z$S$=$l$KBP1~$9$k(B $B8uJd(B WORD $B$r;}$C$F$$$l$P!"(B non-nil $B$rJV(B
$B$9!#8D?M<-=q$N%P%C%U%!$G<B9T$5$l$k!#(B"
  (let (fn
        skk-henkan-okuri-strictly
        skk-henkan-strict-okuri-precedence)
    (when okurigana
      (setq skk-henkan-okurigana okurigana))
    ;; skkserv $B$r;H$&@_Dj$K$J$C$F$$$?$i!"(Bskk-server.el $B$r%m!<%I$9$k!#(B
    (when (or skk-servers-list
              skk-server-host
              (getenv "SKKSERVER"))
      (require 'skk-server))
    (setq fn (funcall skk-public-jisyo-to-be-searched-function))
    ;;
    (and fn
         (member word (eval fn)))))

(defun skk-public-jisyo-to-be-searched-original ()
  "`skk-search-prog-list' $B$NCf$+$i!"0lHVBg$-$J6&M-<-=q$r;H$&%W%m%0%i%`$rJV$9!#(B"
  (let (fn)
    (when (and (featurep 'skk-server)
               (or skk-servers-list
                   skk-server-host))
      (setq fn (assq 'skk-search-server skk-search-prog-list)))
    ;; skk-search-server $B$+$i;O$^$k%j%9%H$,$J$1$l$P!"$H$K$+$/Bg$-$$<-=q$r0z?t(B
    ;; $B$K$7$F$$$k(B skk-search-jisyo-file $B%W%m%0%i%`$rC5$9!#(B
    (when (and (not fn)
               (or skk-aux-large-jisyo
                   skk-large-jisyo))
      (let ((spl skk-search-prog-list)
            cell)
        (while (setq cell (car spl))
          (if (and (eq (car cell) 'skk-search-jisyo-file)
                   (memq (nth 1 cell) '(skk-aux-large-jisyo skk-large-jisyo)))
              (setq fn cell
                    spl nil)
            (setq spl (cdr spl))))))
    fn))

(defun skk-compose-ignore-word (words &optional add)
  "$BL5;k$9$Y$-8uJd$r$^$H$a$k!#(B
WORDS $B$NCf$K(B `skk-ignore-dic-word' $B4X?t$G%/%)!<%H$7$?8uJd$,$"$l$P!"0l$D$N8uJd(B
$B$K$^$H$a$k!#(B
$B%*%W%7%g%J%k0z?t$N(B ADD $B$,;XDj$5$l$F$$$?$i!"(BADD $B$r4^$a$?(B `skk-ignore-dic-word'
$B8uJd72$r:n$k!#(B
$B?7$7$$(B `skk-ignore-dic-word' $B8uJd$r(B car $B$K!"$=$l0J30$N8uJd$r(B cdr $B$K$7$?%;%k(B
\($B%j%9%H(B)$B$rJV$9!#(B"
  (let (l arg e)
    (when add
      (setq words (delete add words)))
    (setq l words)
    (save-match-data
      (while l
        (setq e (car l)
              l (cdr l))
        (when (string-match "(skk-ignore-dic-word +\\([^\)]+\\))"
                            e)
          (setq arg (concat arg
                            (substring e
                                       (1+ (match-beginning 1))
                                       (1- (match-end 1)))
                            "\" \"")
                words (delq e words))))
      (setq arg (cond
                 ((not add)
                  ;; $BKvHx$N(B " \"" $B$r@Z$jMn$H$9!#(B
                  (substring arg 0 -2))
                 (arg
                  (concat arg
                          (skk-compose-ignore-word-sub-quote-char
                           add)))
                 (t
                  add)))
      (cons (format "(skk-ignore-dic-word \"%s\")"
                    (if (equal arg add)
                        (skk-compose-ignore-word-sub-quote-char arg)
                      arg))
            words))))

(defun skk-compose-ignore-word-sub-quote-char (str)
  "`skk-compose-ignore-word' $B$N2<0L%k!<%A%s!#(B
SKK $B<-=q$N8uJd$H$7$F@5$7$$7A$K@07A$9$k!#(B"
  (cond
   ((string-match "[/\n\r\";]" str)
    (let ((alist (if (string-match ";" str)
                     skk-quote-char-alist
                   (cdr skk-quote-char-alist))))
      (skk-quote-char-1 str alist)))
   (t
    str)))

(defun skk-search-katakana-maybe ()
  (when skk-search-katakana
    (skk-search-katakana (eq skk-search-katakana 'jisx0201-kana))))

(defun skk-search-katakana (&optional jisx0201-kana)
  "$B8+=P$78l$r%+%?%+%J$K$7$F!"%j%9%H$K$7$FJV$9!#(B
$B$3$l$O(B `skk-search-prog-list' $B$KDI2C$5$l$k$Y$-5!G=$G!"JQ49%-!<$rC1=c$K%+(B
$B%?%+%J$KJQ49$7$?$b$N$r8uJd$H$7$FJV$9!#(B
$B0lHLE*$J(B FEP $B$OC1=c$K%+%?%+%J$KJQ49$7$?$b$N$,8uJd$K8=$l$k$b$N$,B?$$$,!"(B
$B$=$N$h$&$J5sF0$,9%$_$N>l9g$K$O$3$N4X?t$rMQ$$$k$H$h$$!#(B"
  (unless (or skk-henkan-okurigana
              (string-match ">$" skk-henkan-key) ; $B@\F,<-(B
              (string-match "^>" skk-henkan-key)) ; $B@\Hx<-(B
    (let ((key skk-henkan-key)
          char words)
      (with-temp-buffer
        (insert key)
        ;; $B@\F,<-!&@\Hx<-$NF~NO$@$C$?$i(B ">" $B$r>C$7$F$*$/!#(B
        (goto-char (1- (point)))    ;
        (when (looking-at ">")      ;(looking-back ">")
          (delete-char 1))
        (goto-char (point-min))
        (when (looking-at ">")
          (delete-char 1))
        ;;
        (while (and (not (eobp))
                    ;; "$B!<(B" $B$G$OJ8;z<o$,H=JL$G$-$J$$$N$G(B point $B$r?J$a$k!#(B
                    (or (looking-at "$B!<(B")
                        (eq 'unknown (setq char (skk-what-char-type)))))
          (forward-char 1))
        (when (eq char 'hiragana)
          (skk-katakana-region (point-min) (point-max) t)
          (setq words (list (buffer-string))))
        (when (and jisx0201-kana
                   (or (eq char 'hiragana)
                       (string-match "$B!<(B" key)))
          (skk-katakana-to-jisx0201-region (point-min) (point-max))
          (setq words (nconc words (list (buffer-string))))))
      words)))

(defun skk-search-hankaku-katakana ()
  "$B8+=P$78l$rH>3Q%+%J$K$7$F!"%j%9%H$K$7$FJV$9!#(B"
  (cdr (skk-search-katakana t)))

(defun skk-search-romaji (&optional jisx0208)
  "$BJQ49%-!<$r%m!<%^;z$KJQ49$7$?8uJd$rJV$9!#(B"
  (cond ((executable-find "kakasi")
         (unless (or skk-henkan-okurigana
                     (string-match ">$" skk-henkan-key) ; $B@\F,<-(B
                     (string-match "^>" skk-henkan-key)) ; $B@\Hx<-(B
           (let ((key skk-henkan-key)
                 words chars)
             (with-temp-buffer
               (insert key)
               ;; $B@\F,<-!&@\Hx<-$NF~NO$@$C$?$i(B ">" $B$r>C$7$F$*$/!#(B
               (goto-char (1- (point)))
               (when (looking-at ">")
                 (delete-char 1))
               (goto-char (point-min))
               (when (looking-at ">")
                 (delete-char 1))
               ;;
               (while (not (eobp))
                 (add-to-list 'chars (skk-what-char-type))
                 (forward-char 1))
               (when (memq 'hiragana chars)
                 (skk-romaji-region (point-min) (point-max))
                 (setq words (list (buffer-string))))
               (when (and jisx0208 words)
                 (skk-jisx0208-latin-region (point-min) (point-max))
                 (setq words (nconc words (list (buffer-string))))))
             words)))

        (skk-insert-keysequence
         (list skk-insert-keysequence))))

(defun skk-search-jisx0208-romaji ()
  "$B8+=P$78l$rA43Q%m!<%^;z$KJQ49$7$F!"%j%9%H$K$7$FJV$9!#(B"
  (cdr (skk-search-romaji t)))

(defun skk-search-upcase (&optional initial)
  "$BJQ49%-!<$N(B ascii $B>.J8;z$rBgJ8;z$KJQ49$7$?8uJd$rJV$9!#(B
$B$3$N4X?t$O(B `skk-abbrev-mode' $B@lMQ!#(B"
  (if skk-abbrev-mode
      (list (if initial
                (upcase-initials skk-henkan-key)
              (upcase skk-henkan-key)))
    nil))

(defun skk-search-identity (&rest args)
  "$BJQ49%-!<$r$=$N$^$^8uJd$H$7$FJV$9!#(B
$B$3$N4X?t$OAw$j$J$7JQ49@lMQ!#(B"
  (if skk-henkan-okurigana
      nil
    (list (identity skk-henkan-key))))

(defun skk-search-function-usage ()
  "Emacs Lisp $B4X?t$N(B usage $B$rJV$9!#(B"
  (unless skk-henkan-okurigana
    (let* ((symbol (intern (format "%s" skk-henkan-key)))
           def doc usage arglist result)
      (when (fboundp symbol)
        (setq def (symbol-function symbol)
              doc (documentation symbol)
              usage (help-split-fundoc doc symbol)
              arglist (help-function-arglist symbol))
        (cond (usage
               (setq result (car usage)))

              ((listp arglist)
               (setq result (format "%S" (skk-help-make-usage symbol arglist))))

              ((stringp arglist)
               (setq result arglist))

              ((let ((fun symbol))
                 (while (and (symbolp fun)
                             (setq fun (symbol-function fun))
                             (not (setq usage (help-split-fundoc
                                               (documentation fun)
                                               symbol)))))
                 usage)
               (setq result (car usage)))

              ((or (stringp def)
                   (vectorp def))
               (setq result (format "\nMacro: %s" (format-kbd-macro def)))))

        (when result
          (list (format "(quote %s)" result)))))))

;;;###autoload
(defun skk-search-progs (key &optional prog-list remove-note)
  ;; prog-list $B$,>JN,$5$l$?;~$O(B skk-search-prog-list $B$NA4$F$,BP>](B
  ;; $B$b$70z?t$r$5$i$KDI2C$9$k$h$&$J;v$,$"$l$P(B
  ;;   okuri-nasi   $BAw$jM-$j$N%A%'%C%/$r%Q%9(B
  ;;   allow-duplicate  skk-nunion $B$G$J$/(B nconc $B$r;H$&(B
  ;; $B$"$?$j$+(B
  (save-match-data
    (let ((skk-henkan-key key)
          skk-henkan-okurigana
          skk-okuri-char
          skk-auto-okuri-process
          words)
      ;; $B:.$<=q$-$J?M$C$FAw$jM-$jJQ49$9$k$N$+$J(B  \cj $B$N$[$&$,$$$$(B?
      (when (string-match "[$B$!(B-$B$s(B][a-z]$" key)
        (setq skk-henkan-okurigana ""
              skk-okuri-char (substring key (1- (length key)))))
      (ignore-errors
        (dolist (form (or prog-list
                          skk-search-prog-list))
          (dolist (word (eval form))
            (when (and remove-note
                       (string-match ";" word))
              (setq word (substring word 0 (match-beginning 0))))
            (setq words (skk-nunion words (list word))))))
      words)))

(defun skk-search-sagyo-henkaku-maybe ()
  (when skk-search-sagyo-henkaku
    (skk-search-sagyo-henkaku nil (eq skk-search-sagyo-henkaku 'anything))))

(defun skk-search-sagyo-henkaku (&optional okuri-list anything)
  "$B8+=P$78l$r%59TJQ3J3hMQ$NF0;l$H$_$J$7$F!"Aw$j$"$j8uJd$r8!:w$9$k!#(B"
  (unless okuri-list
    (setq okuri-list '("$B$5(B" "$B$7(B" "$B$9(B" "$B$;(B")))
  (when (and skk-henkan-okurigana
             (or (member skk-henkan-okurigana okuri-list)
                 anything))
    (skk-search-progs (substring skk-henkan-key
                                 0
                                 (1- (length skk-henkan-key))))))

(defun skk-search-ja-dic-maybe (&optional check)
  ;; `skk-search-prog-list' $B$N0lMWAG$H$7$F:nMQ$9$k$[$+!"(B
  ;; skk-mode $B$KF~$k$?$S(B check $B$GI>2A$5$l$k!#(B
  (unless (or (and (stringp skk-large-jisyo)
                   (file-readable-p skk-large-jisyo))
              (and (stringp skk-aux-large-jisyo)
                   (file-readable-p skk-aux-large-jisyo))
              (and (stringp skk-cdb-large-jisyo)
                   (file-readable-p skk-cdb-large-jisyo))
              skk-server-host
              skk-inhibit-ja-dic-search)
    (if check
        (skk-message "$B<-=q$H$7$F(B leim/ja-dic $B$r;H$$$^$9(B"
                     "Use leim/ja-dic as dictionary")
      (skk-search-ja-dic))))

(defun skk-search-with-suffix ()
  (unless (or skk-henkan-okurigana
              (get 'skk-search-with-suffix 'active))
    (let ((i 1)
          (len (length skk-henkan-key))
          key suf-key words suffixes list)
      (put 'skk-search-with-suffix 'active t)
      (while (< i len)
        (setq key (substring skk-henkan-key 0 i)
              suf-key (substring skk-henkan-key i))
        (setq words (skk-search-progs key nil 'remove)
              suffixes (skk-search-progs (concat ">" suf-key) nil 'remove))
        (when (and words suffixes)
          (dolist (word words)
            (dolist (suffix suffixes)
              (setq list (nconc list (list (concat word suffix)))))))
        (setq i (1+ i)))
      (put 'skk-search-with-suffix 'active nil)
      list)))

(defun skk-katakana-region (start end &optional vcontract)
  "$BNN0h$N$R$i$,$J$r%+%?%+%J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VCONTRACT $B$,(B non-nil $B$G$"$l$P!"(B\"$B$&!+(B\" $B$r(B \"$B%t(B\" $B$KJQ49$9(B
$B$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B"
  (interactive "*r\nP")
  (when vcontract
    (skk-search-and-replace start end "$B$&!+(B"
                            (lambda (matched)
                              nil "$B%t(B")))
  (skk-search-and-replace start end "[$B$!(B-$B$s(B]+"
                          (lambda (matched)
                            (skk-hiragana-to-katakana matched))))

(defun skk-hiragana-region (start end &optional vexpand)
  "$BNN0h$N%+%?%+%J$r$R$i$,$J$KJQ49$9$k!#(B
$B%*%W%7%g%J%k0z?t$N(B VEXPAND $B$,(B non-nil $B$G$"$l$P!"(B\"$B%t(B\" $B$r(B \"$B$&!+(B\" $B$KJQ49$9$k!#(B
$B0z?t$N(B START $B$H(B END $B$O?t;z$G$b%^!<%+!<$G$bNI$$!#(B
\"$B%u(B\" $B$H(B \"$B%v(B\" $B$OJQ99$5$l$J$$!#$3$N(B 2 $B$D$NJ8;z$OBP1~$9$k$R$i$,$J$,$J$$$N$G!"(B
$B%+%?%+%J$H$7$F$O07$o$l$J$$!#(B"
  (interactive "*r\nP")
  (when vexpand
    (skk-search-and-replace start end "$B%t(B"
                            (lambda (matched)
                              nil "$B$&!+(B")))
  (skk-search-and-replace start end "[$B%!(B-$B%s(B]+"
                          (lambda (matched)
                            (skk-katakana-to-hiragana matched))))

(defun skk-jisx0208-latin-region (start end)
  "$BNN0h$N(B ascii $BJ8;z$rBP1~$9$kA43Q1QJ8;z$KJQ49$9$k!#(B"
  (interactive "*r")
  (skk-search-and-replace start end "[ -~]"
                          (lambda (matched)
                            (aref skk-default-jisx0208-latin-vector
                                  (string-to-char matched)))))

(defun skk-latin-region (start end)
  "$BNN0h$NA43Q1QJ8;z$rBP1~$9$k(B ascii $BJ8;z$KJQ49$9$k!#(B"
  (interactive "*r")
  (skk-search-and-replace start end "\\cj"
                          (lambda (matched)
                            (or (skk-jisx0208-to-ascii matched)
                                matched))))

;;;###autoload
(defun skk-search-and-replace (start end regexp func)
  (let (matched replace)
    (save-match-data
      (skk-save-point
       ;; END may be changed when length of MATCHED and one of REPLACE
       ;; are different.
       (setq end (set-marker (make-marker) end))
       (goto-char start)
       (while (re-search-forward regexp end 'noerror)
         (let ((beg0 (match-beginning 0))
               (end0 (match-end 0)))
           (setq matched (buffer-substring-no-properties beg0 end0)
                 replace (funcall func matched))
           (goto-char beg0)
           ;; firstly insert a new string, secondly delete an old string to save
           ;; the cursor position.
           (insert-and-inherit replace)
           (delete-region (+ beg0 (length replace))
                          (+ end0 (length replace)))))
       (set-marker end nil)))))

;;;###autoload
(defun skk-jisx0208-to-ascii (string)
  (require 'japan-util)
  (let ((char (get-char-code-property (string-to-char string)
                                      'ascii)))
    (if char
        (char-to-string char)
      nil)))

;;;###autoload
(defun skk-henkan-skk-region-by-func (func &optional arg)
  "`skk-henkan-start-point' $B$H(B `skk-henkan-end-point' $B$N4V$NJ8;zNs$rJQ49$9$k!#(B
$BJQ492DG=$+$I$&$+$N%A%'%C%/$r$7$?8e$K(B ARG $B$r0z?t$H$7$F(B FUNC $B$rE,MQ$7!"(B
`skk-henkan-start-point' $B$H(B `skk-henkan-end-point' $B$N4V$NJ8;zNs$rJQ49$9$k!#(B"
  (skk-with-point-move
   (cl-case skk-henkan-mode
     (active                ;$B"'%b!<%I(B
      nil)
     (on                ;$B"&%b!<%I(B
      (skk-set-marker skk-henkan-end-point (point))
      (when (and (> skk-kakutei-history-limit 0)
                 (< skk-henkan-start-point (point))
                 (skk-save-point
                  (goto-char skk-henkan-start-point)
                  (eq (skk-what-char-type) 'hiragana)))
        (skk-update-kakutei-history
         (buffer-substring-no-properties
          skk-henkan-start-point (point))))
      ;; $BJQ492DG=$+$I$&$+$N:G=*%A%'%C%/(B
      (when (skk-get-prefix skk-current-rule-tree)
        (skk-error "$BF~NOESCf$N2>L>%W%l%U%#%C%/%9$,$"$j$^$9(B"
                   "There remains a kana prefix"))

      (when (< (point) skk-henkan-start-point)
        (skk-error "$B%+!<%=%k$,JQ493+;OCOE@$h$jA0$K$"$j$^$9(B"
                   "Henkan end point must be after henkan start point"))

      (when (and (not skk-allow-spaces-newlines-and-tabs)
                 (skk-save-point
                  (beginning-of-line)
                  (> (point) skk-henkan-start-point)))
        (skk-error "$BJQ49%-!<$K2~9T$,4^$^$l$F$$$^$9(B"
                   "Henkan key may not contain a line feed"))
      ;;
      (apply func skk-henkan-start-point skk-henkan-end-point
             (if arg (list arg) nil))
      (skk-kakutei))
     (t
      (skk-emulate-original-map arg)))))

;;;###autoload
(defun skk-hiragana-to-katakana (hiragana)
  (let ((diff (- ?$B%"(B ?$B$"(B)))
    (mapconcat (lambda (e)
                 (if (and (<= ?$B$!(B e) (>= ?$B$s(B e))
                     (char-to-string (+ e diff))
                   (char-to-string e)))
               (string-to-int-list hiragana) "")))

;;;###autoload
(defun skk-katakana-to-hiragana (katakana)
  (let ((diff (- ?$B%"(B ?$B$"(B)))
    (mapconcat (lambda (e)
                 (if (and (<= ?$B%!(B e) (>= ?$B%s(B e))
                     (char-to-string (- e diff))
                   (char-to-string e)))
               (string-to-int-list katakana) "")))

;;;###autoload
(defun skk-henkan-face-on (&optional face)
  "SKK $B$N(B face $BB0@-$r(B ON $B$K$9$k!#(B
`skk-use-face' $B$,(B non-nil $B$N>l9g!"(B`skk-henkan-start-point' $B$H(B
`skk-henkan-end-point' $B$N4V$N(B face $BB0@-$r(B `skk-henkan-face' $B$NCM$KJQ99$9$k!#(B"
  ;; SKK 9.4 $B$h$j(B Text Properties $B$r;HMQ$9$k$N$r;_$a$F!"(BOverlays $B$r;HMQ$9$k$h(B
  ;; $B$&$K$7$?(B (egg.el, canna.el, wnn-egg.el $B$r;29M$K$7$?(B)$B!#(B
  ;; Overlays $B$O!"%F%-%9%H$N0lIt$G$O$J$$$N$G!"%P%C%U%!$+$iJ8;z$r@Z$j=P$7$F$b%3(B
  ;; $B%T!<$NBP>]$K$J$i$J$$$7!"%"%s%I%%;~$bL5;k$5$l$k$N$G!"JQ49$5$l$?8uJd$NI=<((B
  ;; $B$r0l;~E*$KJQ99$9$k$K$O(B Text Properties $B$h$j$b9%ET9g$G$"$k!#(B
  (unless face
    (setq face skk-henkan-face))
  (when (and face
             (marker-position skk-henkan-start-point)
             (marker-position skk-henkan-end-point))
    (setq skk-henkan-overlay nil)
    (skk-face-on skk-henkan-overlay
                 skk-henkan-start-point skk-henkan-end-point
                 face skk-henkan-overlay-priority)))

;;;###autoload
(defun skk-henkan-face-off ()
  "SKK $B$N(B face $BB0@-$r(B OFF $B$K$9$k!#(B
`skk-henkan-start-point' $B$H(B `skk-henkan-end-point' $B$N4V$NI=<($rJQ99$7$F$$$k(B
`skk-henkan-overlay' $B$r>C$9!#(B"
  (when skk-henkan-face
    (skk-detach-extent skk-henkan-overlay)))

(defun skk-make-face (face)
  "$B?7$7$$(B FACE $B$r:n@.$9$k!#(B
FACE $B$O!VA07J?'!WKt$O!VA07J?'(B + $B%9%i%C%7%e(B + $BGX7J?'!W$N7A<0$G;XDj$9$k!#(B"
  ;; hilit-lookup-face-create $B$N%5%V%;%C%H!#(Btutorial $B$G?'IU$1$r9T$&>l9g$G$b(B
  ;; hilit19 $B$K0MB8$;$:$H$j$"$($:(B face $B$r<+A0$G:n$k$3$H$,$G$-$k$h$&$K!"$H$$$&(B
  ;; $BL\E*$G:n$C$?$b$N$G!"4JC1$J?'IU$1$7$+$G$-$J$$!#$"$^$j8-$/$O$J$$!#J#;($J(B
  ;; face $B$r:n$j$?$$?M$O(B hilit-lookup-face-create $BEy$r;H$C$F2<$5$$!#(B
  (unless (car (memq face (face-list)))
    (save-match-data
      (let* ((list (split-string (symbol-name face) "/"))
             (bg (nth 1 list)))
        (setq face (make-face face))
        (set-face-foreground face (car list))
        (when bg
          (set-face-background face bg)))))
  face)

;; skk-auto.el, skk-rdbms.el $B$NN>J}$G;H$&$N$G!"(Bskk-auto.el $B$h$j0\F0$7$?!#(B
(defun skk-remove-common (word)
  "WORD $B$NAw$j2>L>0J30$NItJ,$rJV$9!#(B
`skk-henkan-key' $B$H(B WORD $B$N4V$K6&DL$NAw$j2>L>$r<h$j=|$-!"Aw$j2>L>0J30$NItJ,(B
$B$NJ8;zNs$rJV$9!#(B`skk-henkan-key' $B$H(B `skk-henkan-okurigana' $B$NCM$r%;%C%H$9$k!#(B
$BNc$($P!"(BWORD == $B;}$C$F$-$?(B $B$G$"$l$P!"(B`skk-henkan-key' := $B$b(Bt ,
`skk-henkan-okurigana' := $B$C$F(B , WORD := $B;}(B $B$N$h$&$KJ,2r$7!"(BWORD $B$rJV$9!#(B
`skk-auto-okuri-process' $B$NCM$,(B non-nil $B$G$"$k$H$-$K$3$N4X?t$r;HMQ$9$k!#(B
$BJQ49$,9T$o$l$?%P%C%U%!$G%3!<%k$5$l$k(B ($B<-=q%P%C%U%!$G$O$J$$(B)$B!#(B"
  (when (and (not (skk-numeric-p))
             (not skk-abbrev-mode)
             (or skk-henkan-in-minibuff-flag
                 (and (<= skk-okuri-index-min skk-henkan-count)
                      (<= skk-henkan-count skk-okuri-index-max))))
    (let ((midasi skk-henkan-key)
          (midasi-len (length skk-henkan-key))
          (word-len (length word))
          (cont t)
          char pos pos2
          midasi-tail word-tail new-word okuri-first
          new-skk-okuri-char new-skk-henkan-key)
      (when (and (>= midasi-len 2) (>= word-len 2))
        ;; check if both midasi and word end with the same ascii char.
        (when (and (skk-ascii-char-p (aref midasi (1- midasi-len)))
                   (eq (aref midasi (1- midasi-len))
                       (aref word (1- word-len))))
          ;; if so chop off the char from midasi and word.
          ;; assume size of an ASCII char is always 1.
          (setq midasi (substring midasi 0 -1)
                midasi-len (1- midasi-len)
                word (substring word 0 -1)
                word-len (1- word-len)))
        (setq midasi-tail (substring midasi (1- midasi-len)
                                     midasi-len)
              word-tail (substring word (1- word-len)
                                   word-len))
        (when (and (string= midasi-tail word-tail)
                   (or (and (skk-string<= "$B$!(B" midasi-tail)
                            (skk-string<= midasi-tail "$B$s(B"))
                       (member midasi-tail '("$B!"(B" "$B!#(B" "$B!$(B" "$B!%(B"))))
          ;; $B8+=P$78l$HC18l$H$NKvHx$,F10l$N$+$JJ8;z$N>l9g!#(B
          ;; $BAw$j$J$7$rAw$j$"$j$X(B
          (setq pos (1- word-len)
                new-word new-skk-henkan-key)
          (while (and cont (> pos 0))
            (setq char (substring word (1- pos) pos))
            (if (and (skk-string<= "$B0!(B" char)
                     (skk-string<= char "$Bt$(B"))
                ;; char is the right-most Kanji
                (setq cont nil)
              (setq pos (1- pos))))
          (setq pos2 (- midasi-len (- word-len pos)))
          ;; check if midasi and word has the same tail of length
          (when (string= (substring midasi pos2 midasi-len)
                         (substring word pos word-len))
            (setq okuri-first (substring word pos (1+ pos)))
            (setq skk-henkan-okurigana
                  (if (and (string= okuri-first "$B$C(B")
                           (<= (+ pos 2) word-len))
                      ;; in this case okuriga consits of two
                      ;; characters, e.g., $B!V;D$C$?!W(B
                      (substring word pos (+ pos 2))
                    okuri-first))
            (setq new-word (substring word 0 pos)
                  new-skk-okuri-char (skk-okurigana-prefix
                                      skk-henkan-okurigana)
                  new-skk-henkan-key (concat
                                      (substring midasi 0 pos2)
                                      new-skk-okuri-char))
            (let (inhibit-quit) ; allow keyboard quit
              (cond
               ((not skk-henkan-in-minibuff-flag)
                (setq word new-word
                      skk-henkan-key new-skk-henkan-key))
               ;; $B<-=qEPO?%b!<%I$GEPO?$5$l$?>l9g!#(B
               ;; ask if register as okuri-ari word.
               ((y-or-n-p
                 (format
                  (if skk-japanese-message-and-error
                      "%s /%s/ $B$rAw$j$"$j8uJd$H$7$FEPO?$7$^$9$+!)(B "
                    "Shall I register this as okuri-ari word: %s /%s/ ? ")
                  new-skk-henkan-key new-word))
                (setq word new-word
                      skk-okuri-char new-skk-okuri-char
                      skk-henkan-key new-skk-henkan-key))
               (t
                (setq skk-henkan-okurigana nil
                      skk-okuri-char nil)
                (message "")))))))))
  ;; $BJ,2r$7$?(B word ($BAw$j2>L>ItJ,$r=|$$$?$b$N(B) $B$rJV$9!#(B
  word)

(defun skk-okurigana-prefix (okurigana)
  (let ((headchar (substring okurigana 0 1)))
    (cond ((string= headchar "$B$s(B")
           "n")
          ((not (and (skk-string<= "$B$!(B" headchar)
                     (skk-string<= headchar "$B$s(B")))
           nil)
          ((and (string= headchar "$B$C(B")
                (not (string= okurigana "$B$C(B")))
           (aref skk-kana-rom-vector
                 ;; assume the character is hiragana of JIS X 0208.
                 (- (skk-char-octet
                     (string-to-char (substring okurigana 1 2))
                     1)
                    33)))
          (t
           (aref skk-kana-rom-vector
                 (- (skk-char-octet (string-to-char headchar)
                                    1)
                    33))))))

(defun skk-update-kakutei-history (midasi &optional word)
  "$BJQ?t(B `skk-kakutei-history' $B$r99?7$9$k!#(B
$B$3$NMzNr$O(B skk-comp.el $B$K$*$$$FMxMQ$5$l$k!#(B"
  (cond
   ((<= skk-kakutei-history-limit 0)
    (setq skk-kakutei-history nil))
   (t
    (setq skk-kakutei-history (cons (list midasi word (current-buffer))
                                    skk-kakutei-history))
    (when (> (length skk-kakutei-history)
             skk-kakutei-history-limit)
      (setcdr (nthcdr (1- skk-kakutei-history-limit)
                      skk-kakutei-history)
              nil)))))

;;; functions for hooks.
(defun skk-after-point-move ()
  (when (and (not (and skk-previous-point
                       (= skk-previous-point (point))))
             (skk-get-prefix skk-current-rule-tree))
    (skk-with-point-move
     (skk-erase-prefix 'clean))))

(defun skk-pre-command ()
  (when (and (memq last-command '(skk-insert skk-previous-candidate))
             (null (memq this-command skk-kana-cleanup-command-list)))
    (skk-kana-cleanup t)))

;;;###autoload
(defun skk-remove-minibuffer-setup-hook (&rest args)
  ;; Remove all args from minibuffer-setup-hook.
  (dolist (hook args)
    (remove-hook 'minibuffer-setup-hook hook)))

(defun skk-add-skk-pre-command ()
  (add-hook 'pre-command-hook 'skk-pre-command nil 'local))

(defun skk-remove-skk-pre-command ()
  (remove-hook 'pre-command-hook 'skk-pre-command 'local))

(add-hook 'edit-picture-hook #'skk-misc-for-picture 'append)
(add-hook 'kill-emacs-hook #'skk-record-jisyo-data)
;; add 'skk-save-jisyo only to remove easily.
(add-hook 'kill-emacs-hook #'skk-save-jisyo)
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (skk-remove-skk-pre-command)
            (skk-remove-minibuffer-setup-hook 'skk-j-mode-on
                                              'skk-setup-minibuffer
                                              'skk-add-skk-pre-command)
            (skk-exit-henkan-in-minibuff)))

;;;###autoload
(defun skk-preload ()
  "Emacs $B5/F0;~$K$"$i$+$8$a(B SKK $B$r8F$V$3$H$G(B SKK $B$N1~Ez$rB.$/$9$k!#(B
$B@hFI$_$NBP>]$K$J$k$N$O0J2<!#(B
1. skk.el $B$H4XO"$9$k$$$/$D$+$N%U%!%$%k(B ($B=i2s5/F0;~$NCY1d$r4KOB(B)
2. $B6&M-<-=q72(B ($B=iJQ49;~$NCY1d$r4KOB(B)
$BJQ?t(B `skk-preload' $B$,(B non-nil $B$N$H$-(B `after-init-hook' $B$+$i8F$P$l$k!#(B
$B$=$N$?$a(B Emacs $B$N5/F0;~4V$,D9$/$J$kE@$K$OCm0U$rMW$9$k!#(B"
  (with-temp-buffer
    (skk-mode 1))
  (dolist (item skk-search-prog-list)
    (when (eq (car item) 'skk-search-jisyo-file)
      (skk-preload-jisyo (cadr item))))
  (dolist (item skk-extra-jisyo-file-list)
    (skk-preload-jisyo item))
  (when skk-annotation-lookup-DictionaryServices
    (skk-annotation-start-python)))

(defun skk-preload-jisyo (jisyo)
  (cond
   ((eq jisyo 'skk-jisyo)
    (setq jisyo nil))
   ;;
   ((symbolp jisyo)
    (setq jisyo (symbol-value jisyo))
    (unless (and (stringp jisyo)
                 (file-readable-p jisyo))
      (setq jisyo nil)))
   ;;
   ((and (listp jisyo)
         (memq (car jisyo) '(cons quote)))
    (setq jisyo (ignore-errors (eval jisyo)))
    (unless (and (consp jisyo)
                 (stringp (car jisyo))
                 (file-readable-p (car jisyo)))
      (setq jisyo nil))))
  (when jisyo
    (skk-get-jisyo-buffer jisyo 'nomsg)))

(defun skk-toggle-isearch-mode (&optional arg)
  "skk-isearch $B$rMxMQ$9$k$+$I$&$+$r%H%0%k$GJQ99$9$k!#(B
$BJQ?t(B `skk-isearch-mode-enable' $B$NCM$r(B nil/t $B%H%0%k$GJQ99$9$k!#(B
$BJQ?t(B `migemo-isearch-enable-p' $B$,(B bound $B$5$l$F$$$l$P!"(B
`skk-isearch-mode-enable' $B$NCM$H5U$NCM$r%;%C%H$9$k!#(B"
  (interactive)
  (setq skk-isearch-mode-enable
        (cond ((null arg)
               (not skk-isearch-mode-enable))
              ((> (prefix-numeric-value arg) 0)
               t)))
  (when (boundp 'migemo-isearch-enable-p)
    (if skk-isearch-mode-enable
        (setq migemo-isearch-enable-p nil)
      (setq migemo-isearch-enable-p t)))
  (message "SKK isearch is %s" (if skk-isearch-mode-enable
                                   "enabled"
                                 "disabled")))

(defun skk-henkan-inactivate ()
  "$B"'%b!<%I$r%-%c%s%;%k$7$F"&%b!<%I$KLa$k!#J8;zNs$O0];}$9$k!#(B"
  (setq skk-henkan-count 0)
  (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
      (let ((count (length skk-henkan-okurigana)))
        (skk-previous-candidate nil)
        ;; $B$3$3$G$O(B delete-backward-char $B$KBhFs0z?t$rEO$5$J$$J}$,%Y%?!<!)(B
        (delete-char (- 0 count)))
    (skk-previous-candidate nil)))

(defun skk-henkan-off-by-quit ()
  "$B"&%b!<%I$r%-%c%s%;%k$7$F"#%b!<%I$KLa$k!#J8;zNs$OGK4~$9$k!#(B"
  (if (memq 'skk-comp-do (list last-command this-command))
      (skk-with-point-move
       (delete-region skk-henkan-start-point (point))
       (insert skk-comp-key)
       (when skk-verbose
         (setq last-command 'keyboard-quit))
       (setq this-command 'keyboard-quit))
    (skk-erase-prefix 'clean)
    (delete-region skk-henkan-start-point
                   (if (> (point) skk-henkan-start-point)
                       (point)
                     skk-previous-point))
    (skk-kakutei)))

;;; cover to original functions.
(skk-defadvice keyboard-quit (around skk-ad activate preactivate)
  "$B"'%b!<%I$G$"$l$P!"8uJd$NI=<($r$d$a$F"&%b!<%I$KLa$9(B ($B8+=P$78l$O;D$9(B)$B!#(B
$B"&%b!<%I$G$"$l$P!"8+=P$78l$r:o=|$9$k!#(B
$B>e5-$N$I$A$i$N%b!<%I$G$b$J$1$l$P(B `keyboard-quit' $B$HF1$8F0:n$r$9$k!#(B"
  (if (not skk-mode)
      ad-do-it
    (cond
     ((eq skk-henkan-mode 'active)
      (skk-henkan-inactivate))
     ((eq skk-henkan-mode 'on)
      (skk-henkan-off-by-quit))
     (t
      (if (skk-get-prefix skk-current-rule-tree)
          (skk-erase-prefix 'clean)
        ad-do-it)))))

(skk-defadvice abort-recursive-edit (around skk-ad activate preactivate)
  "$B"'%b!<%I$G$"$l$P!"8uJd$NI=<($r$d$a$F"&%b!<%I$KLa$9(B ($B8+=P$78l$O;D$9(B)$B!#(B
$B"&%b!<%I$G$"$l$P!"8+=P$78l$r:o=|$9$k!#(B
$B>e5-$N$I$A$i$N%b!<%I$G$b$J$1$l$P(B `abort-recursive-edit' $B$HF1$8F0:n$r$9$k!#(B"
  ;; subr command but no arg.
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer 'skk-add-skk-pre-command)
  (if (not skk-mode)
      ad-do-it
    (cond
     ((eq skk-henkan-mode 'active)
      (skk-henkan-inactivate))
     ((eq skk-henkan-mode 'on)
      (skk-henkan-off-by-quit))
     (t
      (if (skk-get-prefix skk-current-rule-tree)
          (skk-erase-prefix 'clean)
        ad-do-it)))))

(defadvice newline (around skk-ad activate)
  "`skk-egg-like-newline' $B$,(B non-nil $B$G$"$l$P!"3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-j-mode
               skk-jisx0201-mode
               skk-abbrev-mode))
      ad-do-it
    (let (;;(arg (ad-get-arg 0))
          ;; `skk-kakutei' $B$r<B9T$9$k$H(B `skk-henkan-mode' $B$NCM$,(B
          ;; $BL5>r7o$K(B nil $B$K$J$k$N$G!"J]B8$7$F$*$/I,MW$,$"$k!#(B
          (no-newline (and skk-egg-like-newline
                           skk-henkan-mode))
          (auto-fill-function (if (called-interactively-p 'interactive)
                                  auto-fill-function
                                nil)))
      ;; fill $B$5$l$F$b(B nil $B$,5"$C$F$/$k(B :-<
      ;;(if (skk-kakutei)
      ;;    (setq arg (1- arg)))
      ;;(if skk-mode
      ;;    (let ((opos (point)))
      ;;      ;; skk-kakutei (skk-do-auto-fill) $B$K$h$C$F9T$,@^$jJV$5$l$?$i(B
      ;;      ;; arg $B$r(B 1 $B$D8:$i$9!#(B
      ;;      (skk-kakutei)
      ;;      (if (and (not (= opos (point))) (integerp arg))
      ;;          (ad-set-arg 0 (1- arg)))))
      (when skk-mode
        (skk-kakutei))
      (undo-boundary)
      (unless no-newline
        ad-do-it))))

(defadvice newline-and-indent (around skk-ad activate)
  "`skk-egg-like-newline' $B$,(B non-nil $B$G$"$l$P!"3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (if (not (or skk-j-mode
               skk-jisx0201-mode
               skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline
                           skk-henkan-mode))
          (auto-fill-function (if (called-interactively-p 'interactive)
                                  auto-fill-function
                                nil)))
      (when skk-mode
        (skk-kakutei))
      (undo-boundary)
      (unless no-newline
        ad-do-it))))

(skk-defadvice exit-minibuffer (around skk-ad activate)
  ;; subr command but no arg.
  "`skk-egg-like-newline' $B$,(B non-nil $B$G$"$l$P!"3NDj$N$_9T$$!"2~9T$7$J$$!#(B"
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer 'skk-add-skk-pre-command)
  (if (not (or skk-j-mode
               skk-jisx0201-mode
               skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline
                           skk-henkan-mode)))
      (when skk-mode
        (skk-kakutei))
      (unless no-newline
        ad-do-it))))

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK $B$N%P%C%U%!%m!<%+%kJQ?t$rL58z$K$7!"(B`picture-mode-exit' $B$r%3!<%k$9$k!#(B
`picture-mode' $B$+$i=P$?$H$-$K$=$N%P%C%U%!$G(B SKK $B$r@5>o$KF0$+$9$?$a$N=hM}!#(B"
  (when skk-mode
    (skk-kill-local-variables)))

(defadvice undo (before skk-ad activate)
  "SKK $B%b!<%I$,(B on $B$J$i(B `skk-self-insert-non-undo-count' $B$r=i4|2=$9$k!#(B"
  (when skk-mode
    (setq skk-self-insert-non-undo-count 0)))

(defadvice next-line (before skk-ad activate)
  (when (eq skk-henkan-mode 'active)
    (skk-kakutei)))

(defadvice previous-line (before skk-ad activate)
  (when (eq skk-henkan-mode 'active)
    (skk-kakutei)))

(defadvice backward-kill-sentence (before skk-ad activate)
  ;; C-x DEL
  ;; $B$I$N$h$&$JF0:n$r$9$k$Y$-$+L$7hDj(B
  (when skk-mode
    (skk-kakutei)))

(defmacro skk-wrap-newline-command (cmd)
  "[return]$B%-!<$K3d$jEv$F$i$l$F$$$k$G$"$m$&%3%^%s%I(B (CMD) $B$r%i%C%W$7$F!"(B
skk $B$NF0:n$H@09g$5$;$k!#(B
 [return]$B%-!<$K%3%^%s%I$r3d$jEv$F$F$$$k%a%8%c!<%b!<%I$G(B skk $B$r;H$&$H!"(Bskk $B$,(B
`skk-kakutei' $B$r8F$S=P$95!2q$,$J$$$?$a$KJQ49$r3NDj$G$-$:(B `$B"'(B' $B$,%P%C%U%!$K(B
$B;D$C$F$7$^$&$H$$$&LdBj$,$"$k!#(B

$BK\%^%/%m$rMQ$$$k$H!"JQ49$r3NDj$7$F$+$i(B (`skk-kakutei' $B$r<B9T$7$F$+$i(B) CMD $BK\(B
$BBN$r<B9T$9$k$h$&$K(B CMD $B$r%i%C%W$9$k!#(B"
  `(defadvice ,cmd (around skk-ad activate compile)
     (cond (skk-henkan-mode
            (skk-kakutei)
            (unless skk-egg-like-newline
              ad-do-it))
           (t
            ad-do-it))))

(skk-wrap-newline-command comint-send-input)
(skk-wrap-newline-command ielm-return)
(skk-wrap-newline-command rcirc-send-input)
(skk-wrap-newline-command widget-field-activate)
(skk-wrap-newline-command org-insert-heading)
(skk-wrap-newline-command org-return)
(skk-wrap-newline-command org-meta-return)
(skk-wrap-newline-command markdown-enter-key)

;; hooks.

;;;###autoload
(add-hook 'after-init-hook
          (lambda ()
            (when (and (symbol-value 'init-file-user)
                       skk-preload)
              (skk-preload)
              (message "SKK preload...done"))
            (when window-system
              (ccc-setup)))
          t)

(add-hook 'kill-buffer-hook
          ;; SKK $B$N"'%b!<%I$@$C$?$i!"3NDj$7$F$+$i%P%C%U%!$r%-%k$9$k!#(B
          (lambda ()
            (when (and skk-mode
                       skk-henkan-mode)
              (skk-kakutei))))

(run-hooks 'skk-load-hook)

(provide 'skk)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk.el ends here
