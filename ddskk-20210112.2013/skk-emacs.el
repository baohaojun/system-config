;;; skk-emacs.el --- GNU Emacs support for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 2004 Masatake YAMATO <jet@gyve.org>
;; Copyright (C) 2004-2010 SKK Development Team

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

(eval-when-compile
  (require 'cl-lib)
  (require 'ja-dic-utl)
  (require 'tooltip)

  (defvar tool-bar-border)
  (declare-function x-show-tip "xfns.c"))

(eval-and-compile
  (autoload 'mouse-avoidance-banish-destination "avoid")
  (autoload 'mouse-avoidance-point-position "avoid")
  (autoload 'mouse-avoidance-set-mouse-position "avoid")
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-emacs-modeline-menu-items
  (when window-system
    '("Daredevil SKK Menu"
      ["Hiragana"
       (skk-j-mode-on)
       :selected (and skk-j-mode (not skk-katakana))
       :style radio
       :keys nil
       :key-sequence nil]
      ["Katakana"
       (skk-j-mode-on t)
       :selected (and skk-j-mode skk-katakana)
       :style radio
       :keys nil
       :key-sequence nil]
      ["Hankaku alphabet"
       skk-latin-mode
       :selected skk-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      ["Zenkaku alphabet"
       skk-jisx0208-latin-mode
       :selected skk-jisx0208-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      "--"
      ["Find kanji by radicals" skk-tankan t]
      ["Show list of characters" (skk-list-chars nil) t]
      ["Lookup word in region or at point"
       skk-annotation-lookup-region-or-at-point t]
      ["SKK Clock" (skk-clock nil t) t]
      "--"
      ["Read Manual" skk-emacs-info t]
      ["Start Tutorial" skk-tutorial t]
      ["Customize SKK" skk-customize-group-skk t]
      ["Customize SKK (simple)" skk-customize t]
      "--"
      ["About Daredevil SKK.." skk-version t]
      ["Visit Web Site" skk-emacs-visit-website t])))

(defvar skk-emacs-menu-resource-ja
  '(("Daredevil SKK Menu" . "Daredevil SKK $B%a%K%e!<(B")
    ("Convert Region and Echo" . "$BNN0h$rJQ49$7$F%_%K%P%C%U%!$KI=<((B")
    ("Gyakubiki" . "$B5U0z$-(B")
    ("to Hiragana" . "$B$R$i$,$J$KJQ49(B")
    ("to Hiragana, All Candidates" . "$B$R$i$,$J$KJQ49!"A4$F$N8uJd$rI=<((B")
    ("to Katakana" . "$B%+%?%+%J$KJQ49(B")
    ("to Katakana, All Candidates" . "$B%+%?%+%J$KJQ49!"A4$F$N8uJd$rI=<((B")
    ("Hurigana" . "$B$U$j$,$J(B")
    ("Convert Region and Replace" . "$BNN0h$rJQ49$7$FCV$-49$($k(B")
    ("Hiragana" . "$B$R$i$,$J(B")
    ("Katakana" . "$B%+%?%+%J(B")
    ("Hiragana to Katakana" . "$B$R$i$,$J$r%+%?%+%J$KJQ49(B")
    ("Katakana to Hiragana" . "$B%+%?%+%J$r$R$i$,$J$KJQ49(B")
    ("Kana and Zenkaku to Romaji" . "$B$+$J!&%+%J!&A43Q$r%m!<%^;z$KJQ49(B")
    ("Ascii to Zenkaku" . "ASCII $B$rA43Q1Q?t$KJQ49(B")
    ("Zenkaku to Ascii" . "$BA43Q1Q?t$r(B ASCII $B$KJQ49(B")
    ("Count Jisyo Candidates" . "$B<-=qCf$N8uJd?t$r?t$($k(B")
    ("Save Jisyo" . "$B<-=q$rJ]B8$9$k(B")
    ("Undo Kakutei" . "$B3NDj$r<h$j>C$9(B ($B%"%s%I%%(B)")
    ("Restart SKK" . "SKK $B$N:F5/F0(B")
    ("Version" . "SKK $B$N%P!<%8%g%s(B")
    ("Daredevil SKK Menu" . "Daredevil SKK $B%a%K%e!<(B")
    ("Hankaku alphabet" . "$BH>3Q1Q?t(B")
    ("Zenkaku alphabet" . "$BA43Q1Q?t(B")
    ("Read Manual" . "$B%^%K%e%"%k$rFI$`(B")
    ("Start Tutorial" . "$B%A%e!<%H%j%"%k(B")
    ("Customize SKK" . "SKK $B$r%+%9%?%^%$%:(B")
    ("Customize SKK (simple)" . "SKK $B$r%+%9%?%^%$%:(B ($B4J0WHG(B)")
    ("Find kanji by radicals" . "$B4A;z$rIt<s$+$iD4$Y$k(B")
    ("Show list of characters" . "$BJ8;z%3!<%II=(B")
    ("SKK Clock" . "SKK $B;~7W(B")
    ("Lookup word in region or at point" . "$BNN0h$^$?$O%]%$%s%H$N8l6g$rD4$Y$k(B")
    ("About Daredevil SKK.." . "Daredevil SKK $B$K$D$$$F(B..")
    ("Visit Web Site" . "SKK $B$N%5%$%H$X(B")))

(defvar skk-emacs-max-tooltip-size '(80 . 40)
  "Not used if `x-max-tooltip-size' is bound.")

(defvar skk-emacs-modeline-menu nil)

(defvar skk-emacs-tool-bar-height
  (+ (if (and (boundp 'tool-bar-images-pixel-height)
              (integerp tool-bar-images-pixel-height))
         tool-bar-images-pixel-height
       0)
     (if (and (boundp 'tool-bar-button-margin)
              (integerp tool-bar-button-margin))
         (* 2 tool-bar-button-margin)
       0)
     (if (and (boundp 'tool-bar-button-relief)
              (integerp tool-bar-button-relief))
         (* 2 tool-bar-button-relief)
       0)
     (if (boundp 'tool-bar-border)
         (cond ((integerp tool-bar-border)
                tool-bar-border)
               ((symbolp tool-bar-border)
                (or (frame-parameter (selected-frame)
                                     tool-bar-border)
                    0))
               (t
                0))
       0)
     (if (featurep 'gtk)
         ;; inaccurate (seems each few pixels (top and bottom) are used)
         6
       0)))

(defvar skk-emacs-menu-bar-height
  (+ (frame-char-height) ; menu font is not the frame font, though
     (if (featurep 'gtk)
         ;; inaccurate (seems each few pixels (top and bottom) are used)
         4
       0)))

;; Functions.

;;@@ Menu related functions.

;;;###autoload
(defun skk-emacs-prepare-menu ()
  (unless skk-emacs-modeline-menu
    (setq skk-emacs-modeline-menu
          (easy-menu-create-menu (car skk-emacs-modeline-menu-items)
                                 (cdr skk-emacs-modeline-menu-items))))
  ;;
  (unless (or (null window-system)
              (eq window-system 'w32)
              (boundp 'mac-carbon-version-string) ; Carbon Emacs
              (featurep 'ns) ; Cocoa Emacs
              (and (eq window-system 'x)
                   (boundp 'gtk-version-string)
                   (stringp (symbol-value 'gtk-version-string))
                   (string< "2.0" (symbol-value 'gtk-version-string))))
    (setq skk-show-japanese-menu nil))
  ;;
  (when skk-show-japanese-menu
    (skk-emacs-menu-replace skk-emacs-modeline-menu)
    (dolist (map (list skk-j-mode-map
                       skk-latin-mode-map
                       skk-jisx0208-latin-mode-map
                       skk-abbrev-mode-map))
      (skk-emacs-menu-replace (or (assq 'skk (assq 'menu-bar map))
                                  (assq 'SKK (assq 'menu-bar map)))))))

(defun skk-emacs-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-emacs-modeline-menu-items)
        7
        (cond (skk-katakana
               (skk-emacs-find-func-keys 'skk-toggle-characters))
              ((not skk-mode)
               (skk-emacs-find-func-keys 'skk-mode))
              ((not skk-j-mode)
               (skk-emacs-find-func-keys 'skk-kakutei))
              (t
               nil)))
  (aset (nth 2 skk-emacs-modeline-menu-items)
        7
        (if (and skk-j-mode
                 (not skk-katakana))
            (skk-emacs-find-func-keys 'skk-toggle-characters)
          nil))
  (aset (nth 3 skk-emacs-modeline-menu-items)
        7
        (if skk-j-mode
            (skk-emacs-find-func-keys 'skk-latin-mode)
          nil))
  (aset (nth 4 skk-emacs-modeline-menu-items)
        7
        (if skk-j-mode
            (skk-emacs-find-func-keys 'skk-jisx0208-latin-mode)
          nil))
  ;;
  (let ((easy-menu-converted-items-table
         (make-hash-table :test 'equal)))
    (popup-menu skk-emacs-modeline-menu)))

(defun skk-emacs-circulate-modes (&optional arg)
  (interactive "P")
  (cond (skk-henkan-mode
         nil)
        ((not skk-mode)
         (skk-mode arg))
        (skk-j-mode
         (if skk-katakana
             (skk-jisx0208-latin-mode arg)
           (skk-toggle-characters arg)))
        (skk-jisx0208-latin-mode
         (skk-latin-mode arg))
        (skk-latin-mode
         (skk-j-mode-on))))

(defun skk-emacs-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-emacs-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-emacs-visit-website ()
  (interactive)
  (browse-url "https://github.com/skk-dev"))

;;;###autoload
(defun skk-emacs-prepare-modeline-properties ()
  (setq skk-icon
        (let* ((dir (ignore-errors
                      (file-name-directory
                       (or (locate-file "skk.xpm" load-path)
                           (locate-file "skk/skk.xpm"
                                        (list (expand-file-name
                                               "../../.."
                                               data-directory)))
                           (locate-file "skk/skk.xpm"
                                        (list data-directory))))))
               (image (when dir
                        (find-image
                         `((:type xpm
                                  :file ,(expand-file-name "skk.xpm" dir)
                                  :ascent center)))))
               (string "skk"))
          (if (and skk-show-icon window-system image)
              (apply 'propertize string
                     (cons 'display (cons image skk-emacs-modeline-property)))
            nil)))
  ;;
  (unless skk-use-color-cursor
    (setq skk-indicator-use-cursor-color nil))
  ;;
  (when window-system
    (let (face)
      (dolist (mode '(hiragana
                      katakana
                      jisx0208-latin
                      jisx0201
                      abbrev))
        (setq face (intern (format "skk-emacs-%s-face" mode)))
        (unless (facep face)
          (make-face face)
          (when skk-indicator-use-cursor-color
            (set-face-foreground face
                                 (symbol-value
                                  (intern
                                   (format "skk-cursor-%s-color" mode))))))
        (push (cons mode (append skk-emacs-modeline-property
                                 (list 'face face)))
              skk-emacs-property-alist)))))

(defun skk-emacs-find-func-keys (func)
  (let ((keys
         (or (cl-do ((spec (nth 4 skk-rule-tree) (cdr spec))
                     (list nil (car spec))
                     (str nil (when (eq (nth 3 list) func)
                                (nth 1 list))))
                 ((or str (null spec))
                  (if (stringp str)
                      str
                    nil)))
             (car (where-is-internal func skk-j-mode-map)))))
    (if keys
        (format "%s" (key-description keys))
      nil)))

(defun skk-emacs-menu-replace (list)
  (let ((running-ntemacs (and (eq window-system 'w32)
                              (not (fboundp 'Meadow-version))))
        (workaround '("Hiragana"
                      "Katakana"
                      "Hankaku alphabet"
                      "Zenkaku alphabet"))
        cons)
    (while (and list (listp list))
      (cond
       ((and (car-safe list)
             (listp (car list)))
        (skk-emacs-menu-replace (car list)))
       ((and (stringp (car-safe list))
             (setq cons (assoc (car list) skk-emacs-menu-resource-ja)))
        (setcar list (if (and running-ntemacs
                              (member (car list) workaround))
                         ;; NTEmacs $B$G(B Widget $BIU$-%a%K%e!<%"%$%F%`$N(B
                         ;; $BF|K\8l$,$&$^$/I=<($G$-$J$$LdBj$X$NBP:v(B
                         ;; (NTEmacs 22.1, 23.1)
                         (encode-coding-string (cdr cons) 'shift_jis)
                       (cdr cons))))
       ((and (vectorp (car-safe list))
             (setq cons (assoc (aref (car list) 0) skk-emacs-menu-resource-ja)))
        (aset (car list) 0 (if (and running-ntemacs
                                    (member (aref (car list) 0) workaround))
                               ;; NTEmacs $B$G(B Widget $BIU$-%a%K%e!<%"%$%F%`$N(B
                               ;; $BF|K\8l$,$&$^$/I=<($G$-$J$$LdBj$X$NBP:v(B
                               ;; (NTEmacs 22.1, 23.1)
                               (encode-coding-string (cdr cons) 'shift_jis)
                             (cdr cons)))))
      (setq list (cdr list)))))

;;@@ Tooltip related functions.

(defun skk-emacs-mouse-position ()
  "$B%]%$%s%H$N0LCV$r(B (FRAME X . Y) $B$N7A$GJV$9!#(B
$B$3$l$O(B `mouse-avoidance-point-position' $B$H$[$\F1$8$@$,!"(BSKK $B"'%b!<%I$N$H$-$O(B
$B"'$N%]%$%s%H$rJV$9!#(B"
  (let* ((w (if skk-isearch-switch
                (minibuffer-window)
              (selected-window)))
         (edges (window-edges w))
         (list
          (compute-motion (max (window-start w) (point-min))   ; start pos
                          ;; window-start can be < point-min if the
                          ;; latter has changed since the last redisplay
                          '(0 . 0)       ; start XY
                          (if (eq skk-henkan-mode 'active)
                              (ignore-errors
                                (marker-position skk-henkan-start-point))
                            (point))       ; stop pos
                          (cons (window-width w)
                                (window-height w)); stop XY: none
                          (1- (window-width w))       ; width
                          (cons (window-hscroll w) 0)     ; 0 may not be right?
                          w)))
    ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
    ;; we want:               (frame hpos . vpos)
    (cons (selected-frame)
          (cons (+ (car edges) (cadr list))
                (+ (cadr edges) (car (cddr list)))))))

(defun skk-tooltip-max-tooltip-size ()
  (if (boundp 'x-max-tooltip-size)
      (symbol-value 'x-max-tooltip-size)
    ;; Workaround.
    ;; Cocoa Emacs 23.2 $B$G(B x-max-tooltip-size $B$,Dj5A$5$l$F$$$J$$$N$r3NG'(B
    skk-emacs-max-tooltip-size))

(defun skk-tooltip-resize-text (text)
  (let ((lines 0)
        (max-lines (- (/ (/ (display-pixel-height) 2) ;$B%G%#%9%W%l%$$NH>J,(B (ex.512)
                         (frame-char-height)) ;$B$N9T?t(B(ex.16) => 32
                      2))       ;$B4p=`$H$9$k:GBg9b(B => 16
        (max-columns (- (car (skk-tooltip-max-tooltip-size)) 2)) ;ex.78
        (columns 0)
        current-column indent)
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert text)
      (setq indent (if (and (memq (downcase (char-after (point-min)))
                                  skk-henkan-show-candidates-keys)
                            (eq ?: (char-after (1+ (point-min)))))
                       "  "
                     ""))
      (goto-char (point-min))
      (while (not (eobp))
        (setq lines (1+ lines))
        (cond ((= lines max-lines)  ;$BD9$9$.$k(B
               (beginning-of-line)
               (insert "($BD9$9$.$k$N$G>JN,$5$l$^$7$?(B)")
               (delete-region (point) (point-max))
               (goto-char (point-max)))
              ;;
              (t
               (when (> (progn (end-of-line)
                               (current-column))
                        max-columns)
                 (move-to-column max-columns)
                 (backward-char)
                 (if (member (char-to-string (following-char))
                             skk-auto-start-henkan-keyword-list)
                     (forward-char))
                 (insert "\n" indent)
                 (forward-line -1))
               (end-of-line)
               (setq current-column (current-column))
               (when (> current-column columns)
                 (setq columns current-column))
               (forward-line 1))))
      (goto-char (point-min))
      (while (re-search-forward "\n *\n" nil t)
        (replace-match "\n" nil t))
      (setq text (buffer-string)))
    ;; (text . (x . y))
    (cons text (cons columns lines))))

(defun skk-tooltip-show-at-point (text &optional situation)
  "TEXT $B$r(B tooltip $B$GI=<($9$k!#(B"
  (require 'tooltip)

  (and (null (eq situation 'annotation))
       skk-tooltip-show-at-point-decor
       (setq text (funcall skk-tooltip-show-at-point-decor text)))

  (let* ((P (cdr (skk-emacs-mouse-position)))
         (oP (cdr (mouse-position)))
         event
         parameters
         (avoid-destination (if (memq skk-tooltip-mouse-behavior
                                      '(avoid avoid-maybe banish))
                                (mouse-avoidance-banish-destination)))
         win
         tip-destination
         fontsize
         left top
         tooltip-info tooltip-size
         spacing border-width internal-border-width
         text-width text-height
         screen-width screen-height
         (inhibit-quit t))
    ;;
    (when (null (car P))
      (unless (memq skk-tooltip-mouse-behavior '(avoid-maybe banish nil))
        (setq oP (cdr (mouse-avoidance-point-position)))))
    ;;
    (when (eq skk-tooltip-mouse-behavior 'follow)
      (mouse-avoidance-set-mouse-position P))
    ;;
    (when (or (and (memq skk-tooltip-mouse-behavior '(avoid banish))
                   (not (equal (mouse-position) avoid-destination)))
              (and (eq skk-tooltip-mouse-behavior 'avoid-maybe)
                   (cadr (mouse-position))
                   (not (equal (mouse-position) avoid-destination))))
      (set-mouse-position (selected-frame)
                          (car avoid-destination)
                          ;; XXX pending
                          ;; $B%^%&%9%]%$%s%?$O$I$3$X$$$/$Y$-$+(B
                          ;; (cdr avoid-destination)
                          0))
    ;;
    (cond
     ((eq skk-tooltip-mouse-behavior 'follow)
      (setq tooltip-info (skk-tooltip-resize-text text)
            text (car tooltip-info)))
     (t
      ;; $B%^%&%9%]%$%s%?$K0MB8$;$:(B tooptip $B$N0LCV$r7hDj$9$k!#(B
      (setq win (if skk-isearch-switch
                    (minibuffer-window)
                  (selected-window))
            tip-destination (posn-x-y
                             (if skk-isearch-switch
                                 (posn-at-point
                                  (with-current-buffer
                                      (window-buffer (minibuffer-window))
                                    (point-min))
                                  (minibuffer-window))
                               (posn-at-point (point))))
            fontsize (frame-char-height)
            spacing (let ((val (or (cdr-safe (assq 'line-spacing
                                                   skk-tooltip-parameters))
                                   (cdr-safe (assq 'line-spacing
                                                   tooltip-frame-parameters))
                                   (frame-parameter (selected-frame)
                                                    'line-spacing)
                                   (default-value 'line-spacing)
                                   0)))
                      (if (integerp val)
                          val
                        (truncate (* fontsize spacing))))
            border-width (or (cdr-safe (assq 'border-width
                                             skk-tooltip-parameters))
                             (cdr-safe (assq 'border-width
                                             tooltip-frame-parameters))
                             (frame-parameter (selected-frame) 'border-width)
                             0)
            internal-border-width (or (cdr-safe (assq 'internal-border-width
                                                      skk-tooltip-parameters))
                                      (cdr-safe (assq 'internal-border-width
                                                      tooltip-frame-parameters))
                                      (frame-parameter (selected-frame)
                                                       'internal-border-width)
                                      0)

            ;; $B0J2<(B left $B$H(B top $B$O!"(BX Window System $B2<$G$O2hLLA4BN$NCf$G$N:BI8$r(B
            ;; $B;XDj$9$k!#(B Apple OS X $B$K$*$$$F$b!"(BCarbon Emacs 22.3 $B$G$OF1MM$@$,(B
            ;; Cocoa Emacs 23.2 $B$G$O(B Emacs $B%U%l!<%`Fb$G$N:BI8$r;XDj$9$kI,MW$,$"$k!#(B

            ;; x $B:BI8(B ($B:8$+$i$N(B)
            left (+ (car tip-destination)
                    (nth 0 (window-inside-pixel-edges win))
                    (eval (frame-parameter (selected-frame) 'left))
                    skk-tooltip-x-offset)
            ;; y $B:BI8(B ($B>e$+$i$N(B)
            top  (+ (cdr tip-destination)
                    (nth 1 (window-inside-pixel-edges win))
                    (+ (if tool-bar-mode
                           skk-emacs-tool-bar-height
                         0)
                       (if (and menu-bar-mode
                                (not (or (boundp 'mac-carbon-version-string)
                                         (featurep 'ns))))
                           skk-emacs-menu-bar-height
                         0)
                       (eval (frame-parameter (selected-frame) 'top))
                       (+ fontsize spacing))
                    skk-tooltip-y-offset)
            tooltip-info (skk-tooltip-resize-text text)
            text (car tooltip-info)
            tooltip-size (cdr tooltip-info)
            text-width (+ (* (/ (1+ fontsize) 2) (car tooltip-size))
                          (* 2 (+ border-width internal-border-width)))
            text-height (+ (* (+ fontsize spacing) (cdr tooltip-size))
                           (* 2 (+ border-width internal-border-width)))
            screen-width (display-pixel-width)
            screen-height (display-pixel-height))
      ;;
      (when (> (+ left text-width) screen-width)
        ;; $B1&$K4s$j$9$.$F7g$1$F$7$^$o$J$$$h$&$K(B
        (setq left (- left (- (+ left text-width
                                 ;; $B>/$7M>7W$K:8$K4s$;$J$$$H(B avoid
                                 ;; $B$7$?%^%&%9%]%$%s%?$H43>D$9$k(B
                                 (* 2 fontsize))
                              screen-width))))
      (when (> (+ top text-height) screen-height)
        ;; $B2<$K4s$j$9$.$F7g$1$F$7$^$o$J$$$h$&$K(B
        (setq top (- top
                     ;; $B==J,>e$2$J$$$H%F%-%9%H$H=E$J$k$N$G!"(B
                     ;; $B$$$C$=%F%-%9%H$N>e$K$7$F$_$k(B
                     text-height (* 2 (+ fontsize spacing))))
        ;; $B$5$i$K(B X $B:BI8$r(B...
        (let ((right (+ left
                        text-width
                        skk-tooltip-x-offset))
              (mouse-x (+ (frame-parameter (selected-frame) 'left)
                          (* (frame-pixel-width)))))
          (when (and (<= left mouse-x) (<= mouse-x right))
            ;; $B%^%&%9%]%$%s%?$HHo$j$=$&$J$H$-(B
            (setq left (- left (- right mouse-x) fontsize)))))))
    ;; END **$B%^%&%9%]%$%s%?$K0MB8$;$:(B tooptip $B$N0LCV$r7hDj$9$k(B**
    ;;
    (setq parameters (if (eq skk-tooltip-mouse-behavior 'follow)
                         skk-tooltip-parameters
                       (append skk-tooltip-parameters
                               (list (cons 'top top)
                                     (cons 'left left)))))
    ;;
    (skk-tooltip-show-1 text parameters)
    ;;
    (when (eq situation 'annotation)
      (skk-annotation-message situation))
    ;;
    (setq event (read-event))
    (cond ((skk-key-binding-member (skk-event-key event)
                                   '(keyboard-quit
                                     skk-kanagaki-bs
                                     skk-kanagaki-esc)
                                   skk-j-mode-map)
           (tooltip-hide)
           (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
                      (car oP))
             (mouse-avoidance-set-mouse-position oP))
           (setq skk-henkan-count 0)
           (cond ((eq skk-henkan-mode 'active) ; $B"'%b!<%I(B
                  (skk-unread-event
                   (aref (car (where-is-internal 'skk-previous-candidate
                                                 skk-j-mode-map))
                         0))
                  (when (eq situation 'listing)
                    (throw 'unread nil))) ; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
                 (t
                  (skk-unread-event event))))

          (t
           (when (and (not (memq skk-tooltip-mouse-behavior '(banish nil)))
                      (car oP))
             (mouse-avoidance-set-mouse-position oP))
           (tooltip-hide)
           (skk-unread-event event)))))

(defun skk-tooltip-show-1 (text skk-params)
  "TEXT $B$r(B tooltip $B$GI=<($9$k!#(B
SKK-PARAMS $B$O(B `skk-tooltip-parameters' $BKt$O(B `tooltip-frame-parameters' $B$N$$$:$l$+!#(B
TEXT $B$K$O(B `skk-tooltip-face' $B$,E,MQ$5$l$k!#(B"
  (condition-case error
      (let ((params (or skk-params tooltip-frame-parameters))
            fg bg)
        (if skk-params
            ;; $B%f!<%6$,FH<+$K(B tooltip $BI=<(@_Dj$9$k(B
            (dolist (cell tooltip-frame-parameters)
              (unless (assq (car cell) skk-params)
                (setq params (cons cell params))))
          ;; tooltip $B$N%G%U%)%k%H$N@_Dj$r$9$k(B
          (setq fg (face-attribute 'tooltip :foreground))
          (setq bg (face-attribute 'tooltip :background))
          (when (stringp fg)
            (setq params (skk-put-alist 'foreground-color fg params))
            (setq params (skk-put-alist 'border-color fg params)))
          (when (stringp bg)
            (setq params (skk-put-alist 'background-color bg params))))
        ;;
        (when (facep skk-tooltip-face)
          (setq text (propertize text 'face skk-tooltip-face)))
        ;; $B%_%K%P%C%U%!$K$$$k$H$-M>7W$J%a%C%;!<%8$r%/%j%"$9$k(B
        (when (or skk-isearch-switch
                  (skk-in-minibuffer-p))
          (message nil))
        ;;
        (let ((x-gtk-use-system-tooltips nil))
          ;; GTK $BIU(B Emacs $B$G!"(BGTK $B$N%D!<%k%F%#%C%W$rMxMQ$9$k$H(B
          ;; $B8=>u%U%'%$%9B0@-$,E,MQ$5$l$J$$$N$G!"(BEmacs $B$N%D!<%k(B
          ;; $B%F%#%C%W$rMQ$$$k!#(B
          (x-show-tip text (selected-frame) params skk-tooltip-hide-delay
                      ;;
                      (if (eq skk-tooltip-mouse-behavior 'follow)
                          skk-tooltip-x-offset
                        tooltip-x-offset)
                      ;;
                      (if (eq skk-tooltip-mouse-behavior 'follow)
                          skk-tooltip-y-offset
                        tooltip-y-offset))))
    (error
     (message "Error while displaying tooltip: %s" error)
     (sit-for 1)
     (message "%s" text))))


;;@@ Other functions.

;;;###autoload
(defun skk-search-ja-dic ()
  "GNU Emacs $B$KIUB0$9$k$+$J4A;zJQ49<-=q$rMQ$$$F8!:w$9$k!#(B
$B8=:_$N(B Emacs $B$K$O(B SKK-JISYO.L $B$r4p$KJQ49$5$l$?(B ja-dic.el $B$,IUB0$7$F$$$k!#(B
$B$3$N<-=q%G!<%?$rMQ$$$FAw$j$"$j!"Aw$j$J$7!"@\F,<-!"@\Hx<-$NJQ49$r9T$&!#(B
$B$?$@$7!"(BSKK-JISYO.L $B$N$h$&$J1Q?tJQ49!"?tCMJQ49$J$I$O$G$-$J$$!#(B"
  (require 'ja-dic-utl)
  ;; Mostly from ja-dic-utl.el.
  (when (and (not skkdic-okuri-nasi)
             (locate-library "ja-dic/ja-dic"))
    (ignore-errors
      (load-library "ja-dic/ja-dic")))
  (when skkdic-okuri-nasi
    (let* ((len (length skk-henkan-key))
           (vec (make-vector len 0))
           (i 0)
           entry result)
      ;; At first, generate vector VEC from SEQ for looking up SKK
      ;; alists.  Nth element in VEC corresponds to Nth element in SEQ.
      ;; The values are decided as follows.
      ;;   If SEQ[N] is `$B!<(B', VEC[N] is 0,
      ;;   else if SEQ[N] is a Hiragana character, VEC[N] is:
      ;;     ((The 2nd position code of SEQ[N]) - 32),
      ;;   else VEC[N] is 128.
      (while (< i len)
        (let ((ch (aref skk-henkan-key i))
              code)
          (cond ((= ch ?$B!<(B)
                 (aset vec i 0))
                ((and (>= ch (car skkdic-jisx0208-hiragana-block))
                      (<= ch (cdr skkdic-jisx0208-hiragana-block))
                      (setq code (encode-char ch 'japanese-jisx0208)))
                 (aset vec i (- (logand code #xFF) 32)))
                (t
                 (aset vec i 128))))
        (setq i (1+ i)))
      (cond
       (skk-henkan-okurigana ; $BAw$j$"$jJQ49(B
        (let ((okurigana (assq (aref skk-henkan-okurigana 0)
                               skkdic-okurigana-table))
              orig-element)
          (when okurigana
            (setq orig-element (aref vec (1- len)))
            (aset vec (1- len) (- (cdr okurigana)))
            (when (and (setq entry (lookup-nested-alist vec
                                                        skkdic-okuri-ari
                                                        len 0 t))
                       (consp (car entry)))
              (setq entry (nreverse (copy-sequence (car entry))))))))
       ((string-match ">$" skk-henkan-key) ; $B@\F,<-(B
        (setq entry (lookup-nested-alist vec skkdic-prefix (1- len) 0 t)))
       ((string-match "^>" skk-henkan-key) ; $B@\Hx<-(B
        (setq entry (lookup-nested-alist vec skkdic-postfix len 1 t)))
       (t ; $BDL>o$NAw$j$J$7JQ49(B
        (setq entry (lookup-nested-alist vec skkdic-okuri-nasi len 0 t))))
      ;;
      (when (consp (car entry))
        (setq entry (car entry)))
      (while entry
        (when (stringp (car entry))
          (setq result (nconc result (list (car entry)))))
        (setq entry (cdr entry)))
      result)))

;; advices.

(defadvice tooltip-hide (after ccc-ad activate)
  (ccc-update-buffer-local-frame-params))


(provide 'skk-emacs)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-emacs.el ends here
