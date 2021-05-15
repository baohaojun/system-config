;;; skk-macs.el --- Macros and inline functions commonly used in SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999-2010 SKK Development Team
;; Copyright (C) 1993-2000 Free Software Foundation, Inc.

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

;; Macros and inline functions commonly used in SKK

;;; Code:

(require 'advice)
(require 'skk)

;;;; macros

(defmacro skk-defadvice (function &rest everything-else)
  "Defines a piece of advice for FUNCTION (a symbol).
This is like `defadvice', but warns if FUNCTION is a subr command and advice
doesn't give arguments of `interactive'. See `interactive' for details."
  (let ((origfunc (and (fboundp function)
                       (if (ad-is-advised function)
                           (ad-get-orig-definition function)
                         (symbol-function function))))
        interactive)
    (unless
        (or (not origfunc)
            (not (subrp origfunc))
            (memq function ; XXX possibilly Emacs version dependent
                  ;; built-in commands which do not have interactive specs.
                  '(abort-recursive-edit
                    bury-buffer
                    delete-frame
                    delete-window
                    exit-minibuffer)))
      ;; check if advice definition has a interactive call or not.
      (setq interactive
            (cond
             ((and (stringp (nth 1 everything-else)) ; have document
                   (eq 'interactive (car-safe (nth 2 everything-else))))
              (nth 2 everything-else))
             ((eq 'interactive (car-safe (nth 1 everything-else)))
              (nth 1 everything-else))))
      (cond
       ((and (commandp origfunc)
             (not interactive))
        (message
         "\
*** WARNING: Adding advice to subr %s\
 without mirroring its interactive spec ***"
         function))
       ((and (not (commandp origfunc))
             interactive)
        (setq everything-else (delq interactive everything-else))
        (message
         "\
*** WARNING: Deleted interactive call from %s advice\
 as %s is not a subr command ***"
         function function))))
    `(defadvice ,function ,@everything-else)))

;;;###autoload
(put 'skk-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec skk-defadvice defadvice)

(defmacro skk-save-point (&rest body)
  `(let ((skk-save-point (point-marker)))
     (unwind-protect
         (progn
           ,@body)
       (goto-char skk-save-point)
       (skk-set-marker skk-save-point nil))))

(def-edebug-spec skk-save-point t)

(defmacro skk-message (japanese english &rest arg)
  "$B%a%C%;!<%8$rI=<($9$k!#(B
`skk-japanese-message-and-error' $B$,(B non-nil $B$G$"$l$P(B JAPANESE $B$r!"(B nil $B$G(B
$B$"$l$P(B ENGLISH $B$r%(%3!<%(%j%"$KI=<($9$k!#(B
 ARG $B$O(B `message' $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B"
  (append
   (if arg
       `(message (if skk-japanese-message-and-error
                     ,japanese
                   ,english))
     `(message "%s" (if skk-japanese-message-and-error
                        ,japanese
                      ,english)))
   arg))

(defmacro skk-error (japanese english &rest arg)
  "$B%a%C%;!<%8$rI=<($7$F!"%(%i!<$rH/@8$5$;$k!#(B
`skk-japanese-message-and-error' $B$,(B non-nil $B$G$"$l$P(B JAPANESE $B$r!"(B nil $B$G(B
$B$"$l$P(B ENGLISH $B$r%(%3!<%(%j%"$KI=<($7!"%(%i!<$rH/@8$5$;$k!#(B
 ARG $B$O(B `error' $B4X?t$NBh#20z?t0J9_$N0z?t$H$7$FEO$5$l$k!#(B"
  (append
   (if arg
       `(error (if skk-japanese-message-and-error
                   ,japanese
                 ,english))
     `(error "%s" (if skk-japanese-message-and-error
                      ,japanese
                    ,english)))
   arg))

(defmacro skk-yes-or-no-p (japanese english)
  "$B%f!<%6$K(B yes-or-no $B$r<ALd$7!"Ez$($,(B yes $B$@$C$?$i(B t $B$rJV$9!#(B
`skk-japanese-message-and-error' $B$,(B non-nil $B$G$"$l$P(B JAPANESE $B$r!"(B nil $B$G$"$l(B
$B$P(B ENGLISH $B$r(B PROMPT $B$H$7$F(B `yes-or-no-p' $B$r<B9T$9$k!#(B
`yes-or-no-p' $B$N0z?t(B PROMPT $B$,J#;($KF~$l9~$s$G$$$k>l9g$O(B `skk-yes-or-no-p' $B$r(B
$B;H$&$h$j$b%*%j%8%J%k$N(B `yes-or-no-p' $B$r;HMQ$7$?J}$,%3!<%I$,J#;($K$J$i$J$$>l9g(B
$B$,$"$k!#(B"
  `(yes-or-no-p (if skk-japanese-message-and-error
                    ,japanese ,english)))

(defmacro skk-y-or-n-p (japanese english)
  "$B%f!<%6$K(B \"y or n\" $B$r<ALd$7!"Ez$($,(B \"y\" $B$@$C$?$i(B t $B$rJV$9!#(B
`skk-japanese-message-and-error' $B$,(B non-nil $B$G$"$l$P(B JAPANESE $B$r!"(B nil $B$G$"$l(B
$B$P(B ENGLISH $B$r(B PROMPT $B$H$7$F(B `y-or-n-p' $B$r<B9T$9$k!#(B"
  `(y-or-n-p (if skk-japanese-message-and-error
                 ,japanese ,english)))

(defmacro skk-set-marker (marker position &optional buffer)
  "$B%^!<%+(B MARKER $B$r(B BUFFER $B$N(B POSITION $B$K0\F0$9$k!#(B
BUFFER $B$N%G%#%U%)%k%HCM$O%+%l%s%H%P%C%U%!$G$"$k!#(B
MARKER $B$,(B nil $B$@$C$?$i!"?75,%^!<%+!<$r:n$C$FBeF~$9$k!#(B"
  `(progn
     (if (not ,marker)
         (setq ,marker (make-marker)))
     (set-marker ,marker ,position ,buffer)))

(defmacro skk-with-point-move (&rest form)
  "$B%]%$%s%H$r0\F0$9$k$,%U%C%/$r<B9T$7$F$[$7$/$J$$>l9g$K;H$&!#(B"
  `(unwind-protect
       (progn
         ,@form)
     (setq skk-previous-point (point))))

(def-edebug-spec skk-with-point-move t)

(defmacro skk-face-on (object start end face &optional priority)
  `(let ((inhibit-quit t))
     (if (not (overlayp ,object))
         (progn
           (setq ,object (make-overlay ,start ,end))
           (when ,priority
             (overlay-put ,object 'priority ,priority))
           (overlay-put ,object 'face ,face)
           ;;(overlay-put (, object) 'evaporate t)
           )
       (move-overlay ,object ,start ,end))))

;;;###autoload
(put 'skk-loop-for-buffers 'lisp-indent-function 1)
(defmacro skk-loop-for-buffers (buffers &rest body)
  "BUFFERS $B$,;XDj$9$k3F%P%C%U%!$K0\F0$7$F(B BODY $B$r<B9T$9$k!#(B"
  `(save-current-buffer
     (dolist (buf ,buffers)
       (when (buffer-live-p buf)
         (set-buffer buf)
         ,@body))))

(defmacro skk-delete-overlay (list)
  ;; skk-dcomp-multiple-hide $B$H(B skk-inline-hide $B$rE}9g$7$?!#(B
  `(when ,list
     (dolist (o ,list)
       (delete-overlay o))
     (setq ,list nil)))

(defmacro skk-help-make-usage (symbol arglist)
  (cond ((fboundp 'help--make-usage)
         ;; GNU Emacs 25.1 $B$+$i(B
         `(help--make-usage ,symbol ,arglist))

        (t
         ;; GNU Emacs 24.1 $B$^$G(B
         `(help-make-usage ,symbol ,arglist))))

;;; functions.

(defmacro string-to-int-list (string)
  `(mapcar #'identity ,string))

(defun event-to-character (event)
  "Return the character approximation to the given event object.
If the event isn't a keypress, this returns nil."
  (cond
   ((symbolp event)
    ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
    (let ((mask (get event 'event-symbol-element-mask)))
      (if mask
          (let ((base (get (car mask) 'ascii-character)))
            (if base
                (logior base (cadr mask)))))))
   ((integerp event)
    event)))

(defun cancel-undo-boundary ()
  "Cancel undo boundary."
  (if (and (consp buffer-undo-list)
           (null (car buffer-undo-list)))
      (setq buffer-undo-list (cdr buffer-undo-list))))

;; For GNU Emacs.
(defun skk-color-cursor-display-p ()
  (and (skk-find-window-system)
       (fboundp 'x-display-color-p)
       (x-display-color-p)))

(defun skk-char-to-unibyte-string (char)
  (ignore-errors
    (cond
     ;; Warning: `string-make-unibyte' is an obsolete function (as of 26.1).
     ;;          use `encode-coding-string'.
     ((eval-when-compile (>= emacs-major-version 26))
      (encode-coding-string (char-to-string char) 'iso-8859-1))

     ;;  GNU Emacs 25 $B$^$G(B
     (t
      (string-make-unibyte (char-to-string char))))))

(defun skk-ascii-char-p (char)
  (eq (char-charset char skk-charset-list) 'ascii))

(defun skk-jisx0208-p (char)
  (eq (char-charset char skk-charset-list) 'japanese-jisx0208))

(defun skk-jisx0213-p (char)
  (memq (char-charset char skk-charset-list)
        '(japanese-jisx0213-1
          japanese-jisx0213.2004-1
          japanese-jisx0213-2)))

(defun skk-split-char (ch)
  ;; http://mail.ring.gr.jp/skk/200908/msg00006.html
  ;; C $B$N(B split-char() $B$HF1MM$N5!G=$@$,!"(Bchar-charset() $B$N8F=P$7$K$*$$$F(B
  ;; $BJ8;z=89g$NA*Br;h$r(B skk-charset-list $B$K4^$^$l$k$b$N$K@)8B$9$k!#(B
  ;; $B$3$l$ONc$($P!"(Bjapanese-jisx0208 $B$NJ8;z$,(B unicode-bmp $B$KB0$9$k!"(B
  ;; $B$HH=Dj$5$l$k$h$&$J>u67$r2sHr$9$k!#(B
  (let* ((charset (char-charset ch skk-charset-list))
         (code (encode-char ch charset))
         (dimension (charset-dimension charset))
         val)
    (while (> dimension 0)
      (setq val (cons (logand code 255) ;; 0xFF
                      val))
      (setq code (lsh code -8))
      (setq dimension (1- dimension)))
    (cons charset val)))

(defun skk-indicator-to-string (indicator &optional no-properties)
  "SKK $B%$%s%8%1!<%?7?%*%V%8%'%/%H(B INDICATOR $B$rJ8;zNs$KJQ49$9$k!#(B"
  (if no-properties
      (with-temp-buffer
        (insert indicator)
        (buffer-substring-no-properties (point-min) (point-max)))
    indicator))

(defun skk-mode-string-to-indicator (mode string)
  "$BJ8;zNs(B STRING $B$r(B SKK $B%$%s%8%1!<%?7?%*%V%8%'%/%H$KJQ49$9$k!#(B"
  (if (and window-system
           (not (eq mode 'default)))
      (apply 'propertize string
             (cdr (assq mode skk-emacs-property-alist)))
    string))

(defun skk-face-proportional-p (face)
  (or (face-equal face 'variable-pitch)
      (eq (face-attribute face :inherit) 'variable-pitch)))

(defun skk-event-key (event)
  "$B%$%Y%s%H(B EVENT $B$rH/@8$7$?F~NO$N>pJs$r<hF@$9$k!#(B"
  (let ((char (event-to-character event))
        keys)
    (if char
        (vector char)
      (setq keys (recent-keys))
      (vector (aref keys (1- (length keys)))))))

(put 'skk-bind-last-command-char 'lisp-indent-function 1)
(defmacro skk-bind-last-command-char (char &rest body)
  (let ((variable 'last-command-event))
    `(let ((,variable ,char))
       (progn
         ,@body))))

;;; version independent

(defsubst skk-char-octet (ch &optional n)
  (or (nth (if n
               (1+ n)
             1)
           (skk-split-char ch))
      0))

(defun skk-cursor-set (&optional color force)
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  (when (or skk-use-color-cursor
            force)
    (skk-cursor-set-1 color)))

(defun skk-cursor-off ()
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  (when skk-use-color-cursor
    (skk-cursor-off-1)))

(defun skk-modify-indicator-alist (mode string)
  (setcdr (assq mode skk-indicator-alist)
          (cons string (skk-mode-string-to-indicator mode string))))

(defun skk-update-modeline (&optional mode string)
  (unless mode
    (setq mode 'default))
  ;;
  (when string
    (skk-modify-indicator-alist mode string))
  ;;
  (let ((indicator (cdr (assq mode skk-indicator-alist))))
    (setq skk-modeline-input-mode
          (if (eq skk-status-indicator 'left)
              (cdr indicator)
            (car indicator)))
    (force-mode-line-update)))

;; $B%D%j!<$K%"%/%;%9$9$k$?$a$N%$%s%?!<%U%'!<%9(B
(defun skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
        prefix
        (if (string= nextstate "")
            nil
          nextstate)
        kana
        branch-list))

(defsubst skk-get-prefix (tree)
  (nth 1 tree))

(defsubst skk-get-nextstate (tree)
  (nth 2 tree))

(defsubst skk-set-nextstate (tree nextstate)
  (when (string= nextstate "")
    (setq nextstate nil))
  (setcar (nthcdr 2 tree) nextstate))

(defsubst skk-get-kana (tree)
  (nth 3 tree))

(defsubst skk-set-kana (tree kana)
  (setcar (nthcdr 3 tree) kana))

(defsubst skk-get-branch-list (tree)
  (nth 4 tree))

(defsubst skk-set-branch-list (tree branch-list)
  (setcar (nthcdr 4 tree) branch-list))

;; tree procedure for skk-kana-input.
(defsubst skk-add-branch (tree branch)
  (skk-set-branch-list tree (cons branch (skk-get-branch-list tree))))

(defsubst skk-select-branch (tree char)
  (assq char (skk-get-branch-list tree)))

(defun skk-erase-prefix (&optional clean)
  "`skk-echo' $B$,(B non-nil $B$G$"$l$P8=:_I=<($5$l$F$$$k(B `skk-prefix' $B$r>C$9!#(B
$B%*%W%7%g%J%k0z?t$N(B CLEAN $B$,;XDj$5$l$k$H!"JQ?t$H$7$F$N(B `skk-prefix' $B$r6uJ8;z$K!"(B
`skk-current-rule-tree' $B$r(B nil $B$K=i4|2=$9$k!#(B"
  ;; $B$+$JJ8;z$NF~NO$,$^$@40@.$7$F$$$J$$>l9g$K$3$N4X?t$,8F$P$l$?$H$-$J$I$O(B
  ;; $BI=<($5$l$F$$$k(B skk-prefix $B$O:o=|$7$?$$$,!"JQ?t$H$7$F$N(B skk-prefix $B$O(B
  ;; null $BJ8;z$K$7$?$/$J$$!#(B
  (when (overlayp skk-prefix-overlay)
    (condition-case nil
        (delete-overlay skk-prefix-overlay)
      (error
       (skk-set-marker skk-kana-start-point nil)
       (setq skk-prefix ""
             skk-current-rule-tree nil))))
  (when clean
    (setq skk-prefix ""
          skk-current-rule-tree nil))) ; fail safe

(defun skk-kana-cleanup (&optional force)
  (let ((data (cond
               ((and skk-current-rule-tree
                     (null (skk-get-nextstate skk-current-rule-tree)))
                (skk-get-kana skk-current-rule-tree))
               (skk-kana-input-search-function
                (car (funcall skk-kana-input-search-function)))))
        kana)
    (when (or force data)
      (skk-erase-prefix 'clean)
      (setq kana (if (functionp data)
                     (funcall data nil)
                   data))
      (when (consp kana)
        (setq kana (if skk-katakana
                       (car kana)
                     (cdr kana))))
      (when (stringp kana)
        (skk-insert-str kana))
      (skk-set-marker skk-kana-start-point nil)
      (or data t)))) ; skk-prefix $B$KBP1~$9$k%G!<%?$,$"$C$?$J$i$=$l$rJV$9(B

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion
       (require 'skk-num)
       skk-num-list))

(defsubst skk-file-exists-and-writable-p (file)
  (and (setq file (expand-file-name file))
       (file-exists-p file)
       (file-writable-p file)))

(defsubst skk-share-private-jisyo-p ()
  (vectorp skk-jisyo-update-vector))

(defsubst skk-lower-case-p (char)
  "CHAR $B$,>.J8;z$N%"%k%U%!%Y%C%H$G$"$l$P!"(Bt $B$rJV$9!#(B"
  (and (<= ?a char)
       (>= ?z char)))

(defsubst skk-downcase (char)
  "$BO"A[%j%9%H(B `skk-downcase-alist' $B$K3:Ev$"$l$P(B (assq)$B!"MWAG(B($B;R%j%9%H(B)$B$N(B cdr $B$rJV$9!#(B
$B3:Ev$J$1$l$P(B $B4X?t(B `downcase'$B$N7k2L$rJV$9!#(B
$B4X?t(B `skk-set-henkan-point' $B$+$i8F$P$l$F$$$k!#(B"
  (or (cdr (assq char skk-downcase-alist))
      (downcase char)))

(defun skk-mode-off ()
  (setq skk-mode nil
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  ;; initialize
  (skk-update-modeline)
  (skk-cursor-off)
  (skk-remove-skk-pre-command))

(defun skk-j-mode-on (&optional katakana)
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode t
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana katakana)
  (skk-setup-keymap)
  (skk-update-modeline (if skk-katakana
                           'katakana
                         'hiragana))
  (skk-cursor-set))

(defun skk-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode t
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'latin)
  (skk-cursor-set))

(defun skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode t
        skk-jisx0201-mode nil
        ;; sub mode of skk-j-mode.
        skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'jisx0208-latin)
  (skk-cursor-set))

(defun skk-abbrev-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode t
        skk-latin-mode nil
        skk-j-mode nil
        skk-jisx0208-latin-mode nil
        skk-jisx0201-mode nil
        ;; skk-abbrev-mode $B$O0l;~E*$J(B ascii $BJ8;z$K$h$kJQ49$J$N$G!"JQ498e$O85$N(B
        ;; $BF~NO%b!<%I(B ($B$+$J%b!<%I$+%+%J%b!<%I(B) $B$KLa$k$3$H$,4|BT$5$l$k!#(B
        ;; skk-katakana $B$O(B minor-mode $B%U%i%0$G$O$J$/!"(Bskk-j-mode $B%^%$%J!<%b!<%I(B
        ;; $B$NCf$G$3$N%U%i%0$K$h$jF~NOJ8;z$r7hDj$9$k%]%$%s%?$rJQ99$9$k$@$1$J$N$G(B
        ;; skk-abbrev-mode $B%^%$%J!<%b!<%I2=$9$k$N$K(B skk-katakana $B%U%i%0$r=i4|2=(B
        ;; $B$7$J$1$l$P$J$i$J$$I,A3@-$O$J$$!#(B
        ;; sub mode of skk-j-mode.
        ;;skk-katakana nil
        )
  (skk-setup-keymap)
  (skk-update-modeline 'abbrev)
  (skk-cursor-set))

(defsubst skk-in-minibuffer-p ()
  "$B%+%l%s%H%P%C%U%!$,%_%K%P%C%U%!$G$"$l$P(B t $B$rJV$9!#(B"
  (eq (current-buffer) (window-buffer (minibuffer-window))))

(defun skk-screen-column ()
  "$B%9%/%j!<%s9T$+$iF@$?%+!<%=%k0LCV$N7e?t$rJV$9!#(B
$B%F%-%9%H9T!J2~9TJ8;z$G6h@Z$i$l$?%F%-%9%H!K$,%&%#%s%I%&I}$r1[$($F@^$jJV$7$FI=<((B
$B$5$l$F$$$k>l9g$K$bBP1~$9$k!#(B"
  (- (current-column)
     (save-excursion
       (vertical-motion 0)      ;$B%9%/%j!<%s9T$N9TF,$K0\F0$9$k(B
       (current-column))))      ;$B",$3$N7k2L!"%9%/%j!<%s9T$N9TF,$J$N$+(B
                                        ;$B%F%-%9%H9T$N9TF,$J$N$+(B

(defun skk-move-to-screen-column (col)
  "$B%9%/%j!<%s9T$+$i8+$?(B COL $B7e0LCV$K%]%$%s%H$r0\F0$9$k!#(B
$B%F%-%9%H9T!J2~9TJ8;z$G6h@Z$i$l$?%F%-%9%H!K$,%&%#%s%I%&I}$r1[$($F@^$jJV$7$FI=<((B
$B$5$l$F$$$k>l9g$K$bBP1~$9$k$,!"2~9TJ8;z$r1[$($k0\F0$O9T$o$J$$!#(B"
  (move-to-column (+ (current-column)   ;$B%F%-%9%H9T$+$i8+$?7e?t(B
                     (- col (skk-screen-column))))
  (skk-screen-column))

(defun skk-max-string-width (list)
  "LIST $B$N3FMWAG$NCf$+$i!":GD9$N%3%i%`I}(B (string-width) $B$rJV$9!#(B"
  (apply 'max (mapcar 'string-width list)))

(defun skk-insert-prefix (&optional char)
  "`skk-echo' $B$,(B non-nil $B$G$"$l$P%+%l%s%H%P%C%U%!$K(B `skk-prefix' $B$rA^F~$9$k!#(B"
  (when skk-echo
    ;; skk-prefix $B$O%"%s%I%%$NBP>]$H$J$i$J$$$N$G(B
    ;; $B%P%C%U%!$KA^F~$5$l$kI,MW$,$J$$!#(B
    (setq skk-prefix-overlay (make-overlay (point) (point)))
    (let ((prefix (or char skk-prefix)))
      (when (and skk-use-face (not skk-henkan-mode))
        (setq prefix
              (propertize prefix 'face
                          (cond ((and skk-j-mode (not skk-katakana))
                                 'skk-prefix-hiragana-face)
                                (skk-katakana
                                 'skk-prefix-katakana-face)
                                (skk-jisx0201-mode
                                 'skk-prefix-jisx0201-face)))))
      (overlay-put skk-prefix-overlay 'after-string prefix))))

(defsubst skk-string-lessp-in-coding-system (str1 str2 coding-system)
  (string< (encode-coding-string str1 coding-system)
           (encode-coding-string str2 coding-system)))

(defun skk-string< (str1 str2)
  "STR1 $B$H(B STR2 $B$H$rHf3S$9$k!#(B
$BFbIt%3!<%I$,(B emacs-mule $B$G$J$$$J$I(B `stringp' $B$NJV$jCM$,0[$J$k(B Emacs $B$K(B
$BBP$7$F(B emacs-mule $B$N(B encoded string $B$KJQ49$7$FHf3S$9$k!#(B
$BHf3S$N7k2L(B str1 < str2 $B$J$i$P(B t $B$rJV$9!#(B"
  ;; mule-version $B$,(B 6.0 $B0J>e$N(B GNU Emacs $B$O(B Emacs with coding system utf-8-emacs
  ;;   (emacs-version) => "GNU Emacs 28.0.50"
  ;;   (stringp "$B4A(B") => t
  ;;   (encode-coding-string "$B4A(B" 'emacs-mule) => "\222\264\301"
  (skk-string-lessp-in-coding-system str1 str2 'emacs-mule))

(defsubst skk-string<= (str1 str2)
  "STR1 $B$H(B STR2 $B$H$rHf3S$7$F!"(B`string<' $B$+(B `string=' $B$G$"$l$P!"(Bt $B$rJV$9!#(B"
  (or (skk-string< str1 str2)
      (string= str1 str2)))

(defsubst skk-do-auto-fill ()
  "`auto-fill-function' $B$KCM$,BeF~$5$l$F$$$l$P!"$=$l$r%3!<%k$9$k!#(B"
  (when auto-fill-function
    (funcall auto-fill-function)))

(defsubst skk-current-input-mode ()
  (cond (skk-abbrev-mode 'abbrev)
        (skk-latin-mode 'latin)
        (skk-jisx0208-latin-mode 'jisx0208-latin)
        (skk-katakana 'katakana)
        (skk-j-mode 'hiragana)))

(defsubst skk-get-current-candidate-1 (&optional count)
  (setq count (or count skk-henkan-count))
  (when (> 0 count)
    (skk-error "$B8uJd$r<h$j=P$9$3$H$,$G$-$^$;$s(B"
               "Cannot get current candidate"))
  ;; (nth -1 '(A B C)) $B$O!"(BA $B$rJV$9$N$G!"Ii$G$J$$$+$I$&$+%A%'%C%/$9$k!#(B
  (nth count skk-henkan-list))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>    ::= nil | (<tree> . <branch-list>)
;; <tree>         ::= (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         ::= (<$B$R$i$,$JJ8;zNs(B> . <$B%+%?%+%JJ8;zNs(B>) | nil
;; <char>         ::= <$B1Q>.J8;z(B>
;; <nextstate>    ::= <$B1Q>.J8;zJ8;zNs(B> | nil

(defsubst skk-make-raw-arg (arg)
  (cond ((eql arg '1) nil)
        ((eql arg '-1) '-)
        (t (if (numberp arg)
               (list arg)
             nil))))

(defsubst skk-unread-event (event)
  "Unread single EVENT."
  (setq unread-command-events
        (nconc unread-command-events (list event))))

(defsubst skk-get-last-henkan-datum (key)
  (cdr (assq key skk-last-henkan-data)))

(defun skk-put-last-henkan-data (alist)
  (let (e)
    (dolist (kv alist)
      (if (setq e (assq (car kv) skk-last-henkan-data))
          (setcdr e (cdr kv))
        (push (cons (car kv) (cdr kv))
              skk-last-henkan-data)))))

(defun skk-find-coding-system (code)
  "CODE $B$,!"(BEmacs $B$,2r<a$9$k(B coding-system $B%7%s%\%kI=8=$G$"$l$P$=$N$^$^JV$7!"(B
$BJ8;zNs$G$"$l$PO"A[%j%9%H(B `skk-coding-system-alist' $B$rMQ$$$F%7%s%\%kI=8=$XJQ49$9$k!#(B
$B$3$l$i0J30!J(Bnil $B$r4^$`!K$G$"$l$P%7%s%\%k(B euc-jis-2004 $B$rJV$9!#(B"
  (cond ((and code (coding-system-p code))
         code)

        ((and code (stringp code))
         (cdr (assoc code skk-coding-system-alist)))

        (t
         (cdr (assoc "euc" skk-coding-system-alist)))))

(defsubst skk-lisp-prog-p (string)
  "STRING $B$,(B Lisp $B%W%m%0%i%`$G$"$l$P!"(Bt $B$rJV$9!#(B"
  (let ((l (length string)))
    (and (> l 2)
         (eq (aref string 0) ?\()
         ;; second character is ascii or not.
         (skk-ascii-char-p (aref string 1))
         (eq (aref string (1- l)) ?\))      ; $B$3$N9T!"$b$&ITMW$+$b(B
         (ignore-errors
           (= l (cdr (read-from-string string)))))))

(defun skk-eval-string (string)
  "Eval STRING as a lisp program and return the result."
  (cond ((string-match ";" string)
         (concat (skk-eval-string (substring string 0 (match-beginning 0)))
                 (substring string (match-beginning 0) (match-end 0))
                 (skk-eval-string (substring string (match-end 0)))))
        ((skk-lisp-prog-p string)
         (let (func face)
           ;; (^_^;) $B$N$h$&$JJ8;zNs$KBP$7!"(Bread-from-string $B$r8F$V$H(B
           ;; $B%(%i!<$K$J$k$N$G!"(Bignore-errors $B$G0O$`!#(B
           (ignore-errors
             (setq func (car (read-from-string string)))
             (when (and (listp func)
                        (functionp (car func)))
               (setq face (get-text-property 0 'face string))
               (setq string (if face
                                (propertize (format "%s" (eval func))
                                            'face face)
                              (format "%s" (eval func))))))
           string))
        (t
         string)))

;;;; from dabbrev.el.  Welcome!
;; $BH=Dj4V0c$$$rHH$9>l9g$"$j!#MW2~NI!#(B
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)))

(defun skk-quote-char-1 (word alist)
  (mapconcat (lambda (char)
               (or (cdr (assq char alist))
                   (char-to-string char)))
             ;; $BJ8;zNs$rBP1~$9$k(B char $B$N%j%9%H$KJ,2r$9$k!#(B
             (append word nil) ""))

(defun skk-key-binding-member (key commands &optional map)
  "$BF~NO(B KEY $B$,H/F0$9$k%3%^%s%I$,!"(BCOMMANDS $B$K4^$^$l$l$P(B non-nil $B$rJV$9!#(B
MAP $B$OF~NO$,=q$+$l$F$$$k%-!<%^%C%W$r;XDj$9$k$,!";XDj$5$l$J$1$l$P(B
`skk-j-mode-map' $B$r;2>H$9$k!#(B
$B$3$N4X?t$O!"F~NO(B KEY $B$,(B `lookup-key' $B$GC5$;$J$$7A<0$G$"$j$&$k>l9g$KMQ$$$k!#(B"
  (unless map
    (setq map skk-j-mode-map))
  (let (keys)
    (dolist (command commands)
      (setq keys (nconc keys
                        (where-is-internal command map))))
    (member (key-description key)
            (mapcar #'key-description
                    keys))))

(defun skk-update-minor-mode-map-alist (mode map)
  (let ((element (assq mode minor-mode-map-alist)))
    (if element
        (setcdr element map)
      (add-to-list 'minor-mode-map-alist (cons mode map)))))

;; Functions from alist.el (APEL)
(defun skk-put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defun skk-del-alist (key alist)
  "Delete an element whose car equals KEY from ALIST.
Return the modified ALIST."
  (let ((pair (assoc key alist)))
    (if pair
        (delq pair alist)
      alist)))

(defun skk-remove-alist (symbol key)
  "Delete an element whose car equals KEY from the alist bound to SYMBOL."
  (and (boundp symbol)
       (set symbol (skk-del-alist key (symbol-value symbol)))))

(defun skk-reset-henkan-count (count)
  ;; $B"&%b!<%I$KLa$k$H$-$O(B 0
  ;; $B"'%b!<%I$N$^$^8uJd0lMw$N<jA0$KLa$k$H$-$O(B 4
  (setq skk-henkan-count count)
  (skk-unread-event (aref (car (where-is-internal
                                'skk-previous-candidate
                                skk-j-mode-map))
                          0)))

(defun skk-escape-from-show-candidates (count)
  ;; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
  (skk-reset-henkan-count count)
  (throw 'unread nil))

(defun skk-nunion (x y)
  "X $B$H(B Y $B$NOB=89g$r:n$k!#(B
$BEy$7$$$+$I$&$+$NHf3S$O!"(B`equal' $B$G9T$o$l$k!#(BX $B$K(B Y $B$rGK2uE*$KO"@\$9$k!#(B"
  (cond
   ((null x)
    y)
   ((null y)
    x)
   (t
    (save-match-data
      (let ((list2 y) list1 origlist1 e1 e2)
        (while list2
          (setq list1 (cons nil x)
                e2 (car list2)
                origlist1 list1)
          (catch 'found
            (while (setq e1 (cadr list1))
              (cond
               ((equal e1 e2)
                (throw 'found nil))
               ((and (stringp e1)
                     (stringp e2)
                     (string-match ";" e1))
                (setq e1 (substring e1 0 (match-beginning 0)))
                (when (or (equal e1 e2)
                          (and
                           (string-match ";" e2)
                           (equal (substring e2 0 (match-beginning 0))
                                  e1)))
                  (throw 'found nil))))
              (setq list1 (cdr list1)))
            (setcdr list1 (list e2))
            (setq x (cdr origlist1)))
          (setq list2 (cdr list2)))
        x)))))

(defun skk-splice-in (org offset spliced)
  ;; ORG := '(A B C), SPLICED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (unless (> offset 0)
      (error "%s" "Cannot splice in!"))
    (setq tmp (nthcdr (1- offset) org)
          tail (cdr tmp))
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail
                    (nconc spliced tail)
                  spliced))
    org))

(defun skk-detach-extent (object)
  (when (overlayp object)
    (delete-overlay object)))

(defun skk-time-difference (a b)
  ;; from type-break.el.  Welcome!
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a))))


(provide 'skk-macs)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-macs.el ends here
