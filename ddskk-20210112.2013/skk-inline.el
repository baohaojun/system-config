;;; skk-inline.el --- Inline candidate display support for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 2005 Masatake YAMATO <jet@gyve.org>
;; Copyright (C) 2007 IRIE Tetsuya <irie@t.email.ne.jp>

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
  (require 'cl-lib))

;; Functions.
(defun skk-add-background-color (string color)
  "STRING $B$N$&$AGX7J?'$,;XDj$5$l$F$$$J$$J8;z$K8B$C$F(B COLOR $B$NGX7J?'$r(B
$BE,MQ$9$k!#(B"
  (when (and string
             color
             (color-defined-p color))
    (let ((start 0)
          (end 1)
          (len (length string))
          orig-face)
      (while (< start len)
        (setq orig-face (get-text-property start 'face string))
        (while (and (< end len)
                    (eq orig-face (get-text-property end 'face string)))
          (cl-incf end))
        (cond ((not orig-face)
               (put-text-property start end 'face
                                  `(:background ,color)
                                  string))
              ;;
              ((and (facep orig-face)
                    (not (face-background orig-face)))
               (put-text-property start end 'face
                                  `(:inherit ,orig-face :background ,color)
                                  string))
              ;;
              ((and (listp orig-face)
                    (not (plist-get (get-text-property start 'face string)
                                    :background))
                    (not (and (plist-get (get-text-property start 'face start)
                                         :inherit)
                              (face-background
                               (plist-get (get-text-property start 'face start)
                                          :inherit)))))
               (put-text-property start end 'face
                                  (cons `(:background ,color)
                                        orig-face)
                                  string)))

        (setq start (max (1+ start) end)
              end (1+ start)))))
  string)

;;;###autoload
(defun skk-inline-show (str face &optional vertical-str text-max-height)
  (skk-delete-overlay skk-inline-overlays)
  (if (and (eq 'vertical skk-show-inline)
           ;; window $B$,8uJd72$rI=<($G$-$k9b$5$,$"$k$+%A%'%C%/(B
           (stringp vertical-str)
           (integerp text-max-height)
           (< (1+ text-max-height) (window-body-height)))
      (skk-inline-show-vertically vertical-str face)
    (skk-inline-show-horizontally str face)))

(defun skk-inline-show-horizontally (string face)
  (unless (skk-in-minibuffer-p)
    (let ((ol (make-overlay (point) (point)))
          (base-ol (make-overlay (point) (1+ (point)))))
      (overlay-put base-ol 'face 'default)
      (push base-ol skk-inline-overlays)
      (push ol skk-inline-overlays)
      (when face
        (setq string (propertize string 'face face)))
      (when skk-inline-show-background-color
        (setq string (skk-add-background-color
                      string skk-inline-show-background-color)))
      (overlay-put ol 'after-string string))))

(defun skk-inline-show-vertically (string face)
  (when skk-inline-show-vertically-decor
    (setq string (funcall skk-inline-show-vertically-decor string)))
  (unless (skk-in-minibuffer-p)
    (let* ((margin 2)
           ;; XXX beg-col $B$,(B -1 $B$K$J$C$F(B `make-string' $B$G%(%i!<$K$J$k(B
           ;; $B>l9g$"$j(B ?
           (beg-col (max 0 (- (skk-screen-column) margin)))
           (candidates (split-string string "\n"))
           (max-width (skk-max-string-width candidates))
           (i 0)
           bottom col ol invisible)
      (dolist (str candidates)
        (setq str (concat (when (/= 0 i) (make-string margin ? ))
                          str
                          (make-string (+ (- max-width (string-width str))
                                          margin)
                                       ? )))
        (when face
          (setq str (propertize str 'face face)))
        (when skk-inline-show-background-color
          (setq str (skk-add-background-color str
                                              (if (zerop (mod i 2))
                                                  skk-inline-show-background-color
                                                skk-inline-show-background-color-odd))))
        (save-excursion
          (scroll-left (max 0
                            (- (+ beg-col margin max-width margin 1)
                               (window-width) (window-hscroll))))

          (unless (zerop (window-hscroll))
            (setq beg-col
                  (save-excursion (goto-char skk-henkan-start-point)
                                  (- (current-column) margin))))
          (cl-case i
            (0
             (setq col (skk-screen-column)))
            (t
             (setq bottom (> i (vertical-motion i)))
             (cond
              (bottom
               ;; $B%P%C%U%!:G=*9T$G$OIaDL$K(B overlay $B$rDI2C$7$F$$$/J}K!$@(B
               ;; $B$H(B overlay $B$NI=<($5$l$k=gHV$,68$&$3$H$,$"$C$F$&$^$/$J(B
               ;; $B$$!#$7$?$,$C$FA02s$N(B overlay $B$N(B after-string $B$KDI2C$9(B
               ;; $B$k!#(B
               (setq ol (cond ((< (overlay-end
                                   (car skk-inline-overlays))
                                  (point))
                               (make-overlay (point) (point)))
                              (t (pop skk-inline-overlays))))
               (setq str (concat (overlay-get ol 'after-string)
                                 "\n" (make-string beg-col ? ) str)))
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
                                        str))))))))
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
              ;; $B85%F%-%9%H$N(B face $B$r7Q>5$7$J$$$h$&$K(B1$B$D8e$m$K(B overlay
              ;; $B$r:n$C$F!"$=$N(B face $B$r(B 'default $B$K;XDj$7$F$*$/(B
              (setq base-ol (make-overlay (point) (1+ (point))))
              (overlay-put base-ol 'face 'default)
              (push base-ol skk-inline-overlays)
              ;; $B8uJd$,2D;k$+$I$&$+%A%'%C%/(B
              (unless (pos-visible-in-window-p (point))
                (setq invisible t)))))
        (overlay-put ol 'invisible t)
        (overlay-put ol 'after-string str)
        (push ol skk-inline-overlays)
        (cl-incf i))
      (when (or invisible
                (and bottom
                     (> (1+ skk-henkan-number-to-display-candidates)
                        (- (window-body-height)
                           (count-screen-lines (window-start) (point))))))
        (recenter (- (1+ skk-henkan-number-to-display-candidates)))))))

(provide 'skk-inline)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-inline.el ends here
