;;; skk-show-mode.el --- $B%b!<%I@ZBX;~$K(B tooltip/inline $BI=<($9$k(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 2011 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

;; Author: 2011 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
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

;;  ~/.skk $B$K$F(B
;;      (setq skk-show-mode-show t)
;;  $B$H@_Dj$7$F$/$@$5$$!#(B
;;  Emacs $B5/F0Cf$O(B M-x skk-show-mode $B$G%H%0%k$9$k$3$H$,$G$-$^$9!#(B

;;  $BJQ?t(B skk-show-mode-style $B$K(B 'tooltip $BKt$O(B 'inline $B$r@_Dj$7$F$/$@$5$$!#(B

;;; Code:

(require 'skk-emacs)

(defadvice skk-isearch-set-initial-mode (before skk-show-mode activate)
  (setq skk-show-mode-show nil))

(defadvice skk-isearch-initialize-working-buffer (before skk-show-mode activate)
  (setq skk-show-mode-show nil))

(defadvice skk-cursor-set (after skk-show-mode activate)
  "$B$+$J%b!<%I$d%"%9%-!<%b!<%I$X@Z$jBX$o$C$?$H$-$K(B skk-*-mode-string $B$r(B
tooltip / inline $BI=<($9$k(B."
  (when (and skk-show-mode-invoked
             skk-show-mode-show
             (not skk-isearch-switch))
    (let ((func (cdr (assq skk-show-mode-style skk-show-mode-functions))))
      (when func
        (funcall func))))
  (setq skk-show-mode-invoked t))

(defun skk-show-mode-inline ()
  (let ((skk-henkan-start-point (point))
        string)
    (unless (skk-in-minibuffer-p)
      (cond
       (skk-abbrev-mode
        (setq string skk-abbrev-mode-string)
        (set-face-foreground 'skk-show-mode-inline-face skk-cursor-abbrev-color))
       (skk-jisx0208-latin-mode
        (setq string skk-jisx0208-latin-mode-string)
        (set-face-foreground 'skk-show-mode-inline-face skk-cursor-jisx0208-latin-color))
       (skk-katakana
        (setq string skk-katakana-mode-string)
        (set-face-foreground 'skk-show-mode-inline-face skk-cursor-katakana-color))
       (skk-j-mode
        (setq string skk-hiragana-mode-string)
        (set-face-foreground 'skk-show-mode-inline-face skk-cursor-hiragana-color))
       (skk-jisx0201-mode
        (setq string skk-jisx0201-mode-string)
        (set-face-foreground 'skk-show-mode-inline-face skk-cursor-jisx0201-color))
       (t
        (setq string skk-latin-mode-string)
        (set-face-foreground 'skk-show-mode-inline-face skk-cursor-latin-color)))
      ;;
      (skk-show-mode-inline-1 string)))

  (sit-for 0.5)
  (skk-delete-overlay skk-show-mode-inline-overlays))

(defun skk-show-mode-inline-1 (str)
  ;; skk-dcomp-multiple-show() $B$+$iGR<Z(B
  (let* ((margin 1)
         (beg-col (max 0 (- (skk-screen-column) margin)))
         (max-width (string-width str))
         bottom col ol)
    (when (zerop beg-col)
      (setq margin 0))
    (setq str (propertize (concat (make-string margin 32)
                                  str
                                  (make-string margin 32))
                          'face 'skk-show-mode-inline-face))
    (save-excursion
      (scroll-left (max 0
                        (- (+ beg-col margin max-width margin 1)
                           (window-width) (window-hscroll))))
      (setq bottom (zerop (vertical-motion 1)))
      (cond (bottom
             ;; $B%P%C%U%!:G=*9T$G$O!"IaDL$K(B overlay $B$rDI2C$7$F$$$/J}K!$@$H(B
             ;; overlay $B$NI=<($5$l$k=gHV$,68$&$3$H$,$"$C$F$&$^$/$J$$!#(B
             ;; $B$7$?$,$C$FA02s$N(B overlay $B$N(B after-string $B$KDI2C$9$k!#(B
             ;; $B$?$@$7!"(BEOB $B$N>l9g$O(B prefix $B$N(B overlay $B$H>WFM$9$k$?$a(B
             ;; `skk-prefix-overlay' $B$KDI2C$9$k(B
             (setq ol (cond ((or (not skk-echo)
                                 (string= "" skk-prefix)
                                 (< (overlay-end skk-prefix-overlay)
                                    (point)))
                             (make-overlay (point) (point)))
                            (t skk-prefix-overlay)))

             (setq str (concat (overlay-get ol 'after-string)
                               "\n" (make-string beg-col ? ) str)))
            ;; bottom $B0J30(B
            (t
             (setq col (skk-move-to-screen-column beg-col))
             (cond ((> beg-col col)
                    ;; $B7e9g$o$;$N6uGr$rDI2C(B
                    (setq str (concat (make-string (- beg-col col) ? )
                                      str)))
                   ;; overlay $B$N:8C<$,%^%k%AI}J8;z$H=E$J$C$?$H$-$NHyD4@0(B
                   ((< beg-col col)
                    (backward-char)
                    (setq col (skk-screen-column))
                    (setq str (concat (make-string (- beg-col col) ? )
                                      str))))))

      ;; $B$3$N;~E@$G(B overlay $B$N3+;O0LCV$K(B point $B$,$"$k(B
      (unless bottom
        (let ((ol-beg (point))
              (ol-end-col (+ col (string-width str)))
              base-ol)
          (setq col (skk-move-to-screen-column ol-end-col))
          ;; overlay $B$N1&C<$,%^%k%AI}J8;z$H=E$J$C$?$H$-$NHyD4@0(B
          (when (< ol-end-col col)
            (setq str (concat str
                              (make-string (- col ol-end-col) ? ))))
          (setq ol (make-overlay ol-beg (point)))

          ;; $B85%F%-%9%H$N(B face $B$r7Q>5$7$J$$$h$&$K(B1$B$D8e$m$K(B overlay $B$r:n$C$F!"(B
          ;; $B$=$N(B face $B$r(B 'default $B$K;XDj$7$F$*$/(B
          (setq base-ol (make-overlay (point) (1+ (point))))
          (overlay-put base-ol 'face 'default)
          (push base-ol skk-show-mode-inline-overlays))))

    (overlay-put ol 'invisible t)
    (overlay-put ol 'after-string str)
    (push ol skk-show-mode-inline-overlays)))


(defun skk-show-mode-tooltip ()
  (when window-system
    (let ((indicator
           (apply 'skk-mode-string-to-indicator
                  (cond
                   (skk-abbrev-mode
                    `(abbrev ,skk-abbrev-mode-string))
                   (skk-jisx0208-latin-mode
                    `(jisx0208-latin ,skk-jisx0208-latin-mode-string))
                   (skk-katakana
                    `(katakana ,skk-katakana-mode-string))
                   (skk-j-mode
                    `(hiragana ,skk-hiragana-mode-string))
                   (skk-jisx0201-mode
                    `(jisx0201 ,skk-jisx0201-mode-string))
                   (t
                    `(latin ,skk-latin-mode-string))))))
      (skk-tooltip-show-at-point indicator))))

;;;###autoload
(defun skk-show-mode ()
  (interactive)
  (message "skk-show-mode-show: %s"
           (setq skk-show-mode-show (null skk-show-mode-show))))
(provide 'skk-show-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-show-mode.el ends here
