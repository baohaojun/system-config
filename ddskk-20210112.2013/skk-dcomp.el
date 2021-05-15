;;; skk-dcomp.el --- SKK dynamic completion -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Copyright (C) 2008, 2009 IRIE Tetsuya <irie@t.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Author: IRIE Tetsuya <irie@t.email.ne.jp>
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

;; $B$3$l$O"&%b!<%I$K$*$1$k8+=P$78l$NF~NO$r!"<+F0E*$K%@%$%J%_%C%/$K%3%s%W(B
;; $B%j!<%7%g%s$9$k%W%m%0%i%`$G$9!#(B

;; MS Excel $B$N%;%kF~NO$N<+F0Jd40(B ($BF1$8Ns$K4{$KF~NO$7$F$$$kJ8;zNs$,$"$C(B
;; $B$?$H$-$K$=$l$r;2>H$7$FJd40$7$h$&$H$9$k5!G=(B) $B$r8+$F$$$F!"$3$lJXMx$@$J$!(B
;; $B$H;W$C$?$N$,!"3+H/$N$-$C$+$1$G$9!#(B

;; $B$=$N8e!"A}0f=SG7(B $B$5$s$,3+H/$7$F$$$k(B POBox $B$r8+$F!"(BMS Excel $B$r8+$?:]$K(B
;; $B;W$C$?$3$H$r;W$$=P$7!"(BSKK $B$N(B skk-comp.el $B$GDs6!$5$l$F$$$k%3%s%W%j!<%7(B
;; $B%g%s$N5!G=$r<+F0E*$KDs6!$9$kJ}8~$G<BAu$7$F$_$?$N$,(B skk-dcomp.el $B$N%3!<(B
;; $B%G%#%s%0;O$^$j$G$9!#(B

;; POBox $B$OBt;38uJd$r=P$7$^$9$,!">/$7F0:n$,CY$$$N$,FqE@$G$9!#(Bskk-dcomp.el
;; $B$O0l$D$7$+8uJd$r=P$7$^$;$s$,!"%f!<%6$N8+=P$78l$NF~NO$KDI=>$7%@%$%J%_%C(B
;; $B%/$K%3%s%W%j!<%7%g%s$9$k5!G=$O(B POBox $BF1MM;}$C$F$$$^$9$7!"$^$?F0:n$O$+$J(B
;; $B$j9bB.$G!"(Bskk-dcomp.el $B$r;H$&$3$H$K$h$k%*!<%P!<%X%C%I$rBN46$9$k$3$H$O$J(B
;; $B$$$H;W$$$^$9!#(B


;; <INSTALL>

;; SKK $B$rIaDL$K(B make $B$7$F2<$5$$!#FC$K:n6H$OITMW$G$9!#(B

;; <HOW TO USE>

;; ~/.emacs.d/init.el $B$b$7$/$O(B ~/.skk $B$K(B (setq skk-dcomp-activate t) $B$H=q$-(B
;; $B$^$7$g$&!#(B
;; SKK $B5/F08e$K%@%$%J%_%C%/%3%s%W%j!<%7%g%s$N5!G=$r;_$a$?$+$C$?$i!"(B
;; (setq skk-dcomp-activate nil) $B$rI>2A$7$^$7$g$&!#(B


;; <HOW TO WORK>

;; $B"&%b!<%I$KF~$j8+=P$78l$rF~NO$9$k$H!"8D?M<-=q$r<+F0E*$K8!:w$7!"8+=P(B
;; $B$78l$r(B $B%3%s%W%j!<%7%g%s$7$^$9!#2<5-$N$h$&$KF0:n$7$^$9(B ($B%+%C%3Fb$O%-!<(B
;; $BF~NO$r!"(B-!- $B$O%]%$%s%H0LCV$rI=$7$^$9(B)$B!#(B

;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B

;;   * SKK $B$N%3%s%W%j!<%7%g%s$O!"85Mh8D?M<-=q$N$_$r;2>H$9$k;EMM$K$J$C$F$$(B
;;     $B$k$N$G!"8D?M<-=q$K$J$$8+=P$78l$O%3%s%W%j!<%7%g%s$5$l$^$;$s!#(B
;;   * $B%3%s%W%j!<%7%g%s$O!"Aw$j$J$7JQ49$N>l9g$7$+9T$o$l$^$;$s!#(B
;;   * Ho $B$NF~NO$KBP$7!"!V$[$s$H$&!W$,%3%s%W%j!<%7%g%s$5$l$k$+$I$&$+$O8D(B
;;     $B?M<-=q$N%(%s%H%j$N=gHV<!Bh(B ($BJQ49=g$K9_=g$KJB$s$G$$$k(B) $B$G$9$N$G!"?M(B
;;     $B$=$l$>$l0c$&$O$:$G$9!#(B

;; $B<+F0E*$K%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$,!"<+J,$N0U?^$7$?$b$N$G$"$l$P(B TAB
;; $B$r2!$9$3$H$G%]%$%s%H0LCV$rF0$+$7!"%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$rA*Br$9(B
;; $B$k$3$H$,$G$-$^$9!#$=$N$^$^(B SPC $B$r2!$7$FJQ49$9$k$J$j!"(Bq $B$r2!$7$F%+%?%+%J(B
;; $B$K$9$k$J$j(B SKK $BK\Mh$NF0:n$r2?$G$b9T$&$3$H$,$G$-$^$9!#(B

;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (TAB) -> $B"&$[$s$H$&(B-!- (TAB)

;; $B%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$,<+J,$N0U?^$7$?$b$N$G$J$$>l9g$O!"$+$^(B
;; $B$o$:<!$NF~NO$r$7$F2<$5$$!#%3%s%W%j!<%7%g%s$5$l$?ItJ,$rL5;k$7$?$+$N$h$&$K(B
;; $BF0:n$7$^$9!#(B

;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (ka) -> $B"&$[$+(B-!-$B$s(B

;; $B%3%s%W%j!<%7%g%s$5$l$J$$>uBV$,<+J,$N0U?^$7$?$b$N$G$"$k>l9g$b!"%3%s%W%j!<(B
;; $B%7%g%s$5$l$?ItJ,$rC1$KL5;k$9$k$@$1$G(B OK $B$G$9!#(B

;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (C-j) -> $B$[(B
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (SPC) -> $B"'J](B ($B!V$[!W$r8+=P$78l$H$7$?JQ49$,9T$o$l$k(B)
;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (q) -> $B%[(B

;; $B%3%s%W%j!<%7%g%s$5$l$?>uBV$+$i(B BS $B$r2!$9$H!">C$5$l$?%3%s%W%j!<%7%g%sA0$N(B
;; $B8+=P$78l$+$i:FEY%3%s%W%j!<%7%g%s$r9T$$$^$9!#(B

;;   (Ho) $B"&$[(B -> $B"&$[(B-!-$B$s$H$&(B (ka) -> $B"&$[$+(B-!-$B$s(B (BS) -> $B"&$[(B-!-$B$s$H$&(B

;;; Code:

(require 'skk)

(require 'skk-comp)
(eval-when-compile (require 'cl-lib))

(defsubst skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
               skk-dcomp-face-priority))

(defsubst skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

(defsubst skk-dcomp-delete-completion ()
  (ignore-errors
    (delete-region skk-dcomp-start-point skk-dcomp-end-point)))

;;;###autoload
(defun skk-dcomp-marked-p ()
  (and (eq skk-henkan-mode 'on)
       (markerp skk-dcomp-start-point)
       (markerp skk-dcomp-end-point)
       (marker-position skk-dcomp-start-point)
       (marker-position skk-dcomp-end-point)
       (< skk-dcomp-start-point skk-dcomp-end-point)))

(defun skk-dcomp-cleanup-buffer ()
  (skk-delete-overlay skk-dcomp-multiple-overlays)
  (when (and (or skk-dcomp-activate
                 skk-dcomp-multiple-activate)
             (skk-dcomp-marked-p))
    (skk-dcomp-face-off)
    (delete-region skk-dcomp-end-point (point))
    (skk-set-marker skk-dcomp-end-point (point))
    (setq skk-dcomp-multiple-select-index -1)))

(defun skk-dcomp-activate-p ()
  (and skk-dcomp-activate
       (cond ((functionp skk-dcomp-activate)
              (save-match-data
                (funcall skk-dcomp-activate)))
             ((listp skk-dcomp-activate)
              (save-match-data
                (eval skk-dcomp-activate)))
             (skk-hint-inhibit-dcomp
              nil)
             (t
              t))))

(defun skk-dcomp-multiple-activate-p ()
  (and skk-dcomp-multiple-activate
       (cond ((functionp skk-dcomp-multiple-activate)
              (save-match-data
                (funcall skk-dcomp-multiple-activate)))
             ((listp skk-dcomp-multiple-activate)
              (save-match-data
                (eval skk-dcomp-multiple-activate)))
             (skk-hint-inhibit-dcomp
              nil)
             (t
              t))))

(defun skk-dcomp-do-completion (pos)
  (when (and (eq skk-henkan-mode 'on)
             (not skk-okurigana))
    (when (and (not (eq (marker-position skk-henkan-start-point) (point)))
               (skk-dcomp-activate-p))
      (condition-case nil
          (progn
            (skk-comp-do 'first 'silent)
            (skk-set-marker skk-dcomp-start-point pos)
            (skk-set-marker skk-dcomp-end-point (point))
            (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
            (goto-char skk-dcomp-start-point))
        (error
         (setq skk-comp-stack nil)
         (message nil))))
    (when (and (skk-dcomp-multiple-activate-p)
               (skk-dcomp-multiple-available-p))
      (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates
                                (and (string= skk-dcomp-multiple-key
                                              (buffer-substring-no-properties
                                               skk-henkan-start-point (point)))
                                     (string= skk-dcomp-multiple-prefix skk-prefix)))))))

;; $BJ#?tI=<($N$?$a$K8!:w$7$F<-=q%P%C%U%!$N(B point $B$rF0$+$9$H!"(Bskk-comp $B$N(B
;; $BJd408uJd$,68$C$F$7$^$&$N$G0lC6J]B8$7$F$*$-:G8e$K85$KLa$9(B
(defmacro skk-dcomp-save-point-in-jisyo-buffer (form)
  `(let (alist)
     (dolist (buf skk-dcomp-multiple-keep-point-buffer-list)
       (when (get-buffer buf)
         (with-current-buffer buf
           (setq alist (cons (cons buf (point)) alist)))))
     ,form
     (dolist (pair alist)
       (with-current-buffer (car pair)
         (goto-char (cdr pair))))))

(defun skk-dcomp-multiple-available-p ()
  (< (1+ skk-dcomp-multiple-rows)
     (window-body-height)))

(defun skk-dcomp-multiple-increase-index (index &optional ignore-search-done)
  (cond ((and skk-comp-circulate
              (or ignore-search-done
                  skk-dcomp-multiple-search-done
                  skk-comp-search-done)
              (<= (1- (length skk-dcomp-multiple-candidates))
                  index))
         -1)
        ((and skk-dcomp-multiple-search-done
              (<= (1- (length skk-dcomp-multiple-candidates))
                  index))
         index)
        (t (1+ index))))

(defun skk-dcomp-multiple-extract-candidates (candidates index)
  (let ((i 0)
        head extract)
    (setq head (cond ((< index skk-dcomp-multiple-rows)
                      candidates)
                     (t
                      (nthcdr (* (/ index skk-dcomp-multiple-rows)
                                 skk-dcomp-multiple-rows)
                              candidates))))
    (while (and (< i skk-dcomp-multiple-rows) head)
      (push (pop head) extract)
      (cl-incf i))
    (nreverse extract)))

(defun skk-dcomp-multiple-get-candidates (&optional same-key)
  (let (candidates)
    (cond
     ;; (1) $B?75,8!:w(B
     ((not same-key)
      (setq skk-dcomp-multiple-select-index
            ;; skk-comp $B$N(B C-u TAB $B$r9MN8$9$k(B
            (if (and current-prefix-arg (listp current-prefix-arg)) 0 -1))
      (setq skk-dcomp-multiple-key
            ;; skk-comp $B$N(B C-u TAB $B$r9MN8$9$k(B
            (if (and current-prefix-arg (listp current-prefix-arg))
                skk-comp-key
              (let ((key (buffer-substring-no-properties
                          skk-henkan-start-point (point))))
                (if skk-katakana
                    (skk-katakana-to-hiragana key)
                  key))))
      (setq skk-dcomp-multiple-prefix skk-prefix)
      (setq skk-dcomp-multiple-search-done nil)
      (let ( ;; `skk-comp-get-candidate' $B$KI,MW$J%G!<%?$rB+G{(B
            (skk-comp-key skk-dcomp-multiple-key)
            (skk-comp-prefix skk-dcomp-multiple-prefix)
            ;; `skk-comp-get-candidate' $B$GCM$,JQ$o$C$F$7$^$&$?$aB+G{(B
            (skk-current-completion-prog-list
             skk-current-completion-prog-list)
            (skk-server-completion-words skk-server-completion-words)
            (skk-look-completion-words skk-look-completion-words)
            (i 0)
            cand)
        (when (or skk-comp-use-prefix
                  ;; skk-comp-use-prefix $B$,(B nil $B$N>l9g!""&(Bn $B$J$I$O(B
                  ;; $BJd408uJd$r8!:w$7$J$$(B
                  (not (skk-get-kana skk-current-rule-tree)))
          (skk-dcomp-save-point-in-jisyo-buffer
           (while (and (< i skk-dcomp-multiple-rows)
                       (setq cand (skk-comp-get-candidate (zerop i))))
             (unless (member cand candidates)
               (push cand candidates)
               (cl-incf i)))))
        (setq candidates (nreverse candidates))
        (when (< i skk-dcomp-multiple-rows)
          (setq skk-dcomp-multiple-search-done t))
        (setq skk-dcomp-multiple-candidates candidates)))

     ;; (2) $BA4$F8!:w:Q(B
     (skk-dcomp-multiple-search-done
      (setq candidates (skk-dcomp-multiple-extract-candidates
                        skk-dcomp-multiple-candidates
                        skk-dcomp-multiple-select-index)))

     ;; (3) $BA48!:w$9$k(B (TAB $BO"BG$G7+$j1[$7$?$H$-(B)
     ((and same-key
           (< (1- (length skk-dcomp-multiple-candidates))
              skk-dcomp-multiple-select-index))
      (skk-dcomp-save-point-in-jisyo-buffer
       (let ( ;; `skk-comp-get-all-candidates' $B$G6u$K$J$C$F$7$^$&$?$aB+G{(B
             (skk-comp-kakutei-midasi-list skk-comp-kakutei-midasi-list)
             (skk-server-completion-words skk-server-completion-words)
             (skk-look-completion-words skk-look-completion-words))
         (setq skk-dcomp-multiple-candidates
               (skk-comp-get-all-candidates skk-dcomp-multiple-key
                                            skk-dcomp-multiple-prefix
                                            skk-completion-prog-list))))
      (setq skk-dcomp-multiple-search-done t)
      (setq skk-dcomp-multiple-select-index
            (min skk-dcomp-multiple-select-index
                 (1- (length skk-dcomp-multiple-candidates))))
      (setq candidates (skk-dcomp-multiple-extract-candidates
                        skk-dcomp-multiple-candidates
                        skk-dcomp-multiple-select-index)))

     ;; (4) $BC1$J$k(B TAB $BBG80(B
     (t
      (setq candidates skk-dcomp-multiple-candidates)))
    (when candidates
      (append candidates
              (list (format " [ %s / %s ]"
                            (if (< skk-dcomp-multiple-select-index 0)
                                "-"
                              (1+ skk-dcomp-multiple-select-index))
                            (if skk-dcomp-multiple-search-done
                                (length skk-dcomp-multiple-candidates)
                              "??")))))))

(defun skk-dcomp-multiple-show (candidates)
  (skk-delete-overlay skk-dcomp-multiple-overlays)
  (unless (skk-in-minibuffer-p)
    (let* ((margin 1)
           (beg-col (save-excursion
                      (goto-char skk-henkan-start-point)
                      (max 0 (- (skk-screen-column) margin))))
           (i 0)
           max-width bottom col ol invisible)
      (when candidates
        (when (zerop beg-col)
          (setq margin 0))
        (setq max-width (skk-max-string-width candidates))
        (dolist (str candidates)
          (setq str (concat (make-string margin ? )
                            str
                            (make-string (+ (- max-width (string-width str))
                                            margin)
                                         ? )))
          (cond
           ((and (< -1 skk-dcomp-multiple-select-index)
                 (= i (mod skk-dcomp-multiple-select-index
                           skk-dcomp-multiple-rows)))
            (setq str (propertize
                       str 'face 'skk-dcomp-multiple-selected-face)))
           (t
            (setq str (propertize str 'face 'skk-dcomp-multiple-face))
            (unless (string= "" skk-dcomp-multiple-key)
              (let ((beg (string-match skk-dcomp-multiple-key str))
                    (end (progn (string-match "[^ ]+" str) (match-end 0))))
                (when beg
                  (put-text-property (+ beg (length skk-dcomp-multiple-key))
                                     end
                                     'face 'skk-dcomp-multiple-trailing-face
                                     str))))))
          (save-excursion
            (scroll-left (max 0
                              (- (+ beg-col margin max-width margin 1)
                                 (window-width) (window-hscroll))))
            (unless (zerop (window-hscroll))
              (setq beg-col
                    (save-excursion (goto-char skk-henkan-start-point)
                                    (- (current-column) margin))))
            (setq bottom (> (1+ i) (vertical-motion (1+ i))))
            (cond (bottom
                   ;; $B%P%C%U%!:G=*9T$G$OIaDL$K(B overlay $B$rDI2C$7$F$$$/J}(B
                   ;; $BK!$@$H(B overlay $B$NI=<($5$l$k=gHV$,68$&$3$H$,$"$C$F(B
                   ;; $B$&$^$/$J$$!#$7$?$,$C$FA02s$N(B overlay $B$N(B
                   ;; after-string $B$KDI2C$9$k!#$?$@$7!"(BEOB $B$N>l9g$O(B
                   ;; prefix $B$N(B overlay $B$H>WFM$9$k$?$a(B
                   ;; `skk-prefix-overlay' $B$KDI2C$9$k(B
                   (setq ol (if (zerop i)
                                (cond ((or (not skk-echo)
                                           (string= "" skk-prefix)
                                           (< (overlay-end skk-prefix-overlay)
                                              (point)))
                                       (make-overlay (point) (point)))
                                      (t skk-prefix-overlay))
                              (pop skk-dcomp-multiple-overlays)))
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
                ;; $B85%F%-%9%H$N(B face $B$r7Q>5$7$J$$$h$&$K(B1$B$D8e$m$K(B
                ;; overlay $B$r:n$C$F!"$=$N(B face $B$r(B 'default $B$K;XDj$7$F$*$/(B
                (setq base-ol (make-overlay (point) (1+ (point))))
                (overlay-put base-ol 'face 'default)
                (push base-ol skk-dcomp-multiple-overlays)
                ;; $B8uJd$,2D;k$+$I$&$+%A%'%C%/(B
                (unless (pos-visible-in-window-p (point))
                  (setq invisible t)))))
          (overlay-put ol 'invisible t)
          (overlay-put ol 'after-string str)
          (push ol skk-dcomp-multiple-overlays)
          (cl-incf i))
        (when (or invisible
                  (and bottom
                       (> (+ 2 skk-dcomp-multiple-rows)
                          (- (window-body-height)
                             (count-screen-lines (window-start) (point))))))
          (recenter (- (+ 2 skk-dcomp-multiple-rows))))))))

;;;###autoload
(defun skk-dcomp-before-kakutei ()
  (skk-delete-overlay skk-dcomp-multiple-overlays)
  (when (and (or skk-dcomp-activate
                 skk-dcomp-multiple-activate)
             (eq skk-henkan-mode 'on)
             (skk-dcomp-marked-p))
    (skk-dcomp-face-off)
    (skk-dcomp-delete-completion)))

(defun skk-dcomp-after-kakutei ()
  (when (or skk-dcomp-activate
            skk-dcomp-multiple-activate)
    (skk-set-marker skk-dcomp-start-point nil)
    (skk-set-marker skk-dcomp-end-point nil)
    (setq skk-comp-stack nil)
    (setq skk-dcomp-multiple-select-index -1)))

;;;###autoload
(defun skk-dcomp-after-delete-backward-char ()
  (when (and (or skk-dcomp-activate
                 skk-dcomp-multiple-activate)
             skk-mode
             (eq skk-henkan-mode 'on)
             (not skk-hint-inhibit-dcomp))
    (skk-delete-overlay skk-dcomp-multiple-overlays)
    (setq skk-dcomp-multiple-select-index -1)
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    (when (and skk-abbrev-mode
               skk-use-look)
      (setq skk-look-completion-words nil))
    (skk-dcomp-do-completion (point)))
  ;; dcomp $B$H$N=gHV@)8f$N$?$a!"$3$3$G8F$V(B
  (skk-henkan-on-message))

;;; advices.
;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (cond
   ((or skk-hint-inhibit-dcomp
        (not (and (or skk-dcomp-activate
                      skk-dcomp-multiple-activate)
                  skk-henkan-mode)))
    ad-do-it)
   (t
    (cond
     ((or (eq skk-henkan-mode 'active) ; $B"'%b!<%I(B
          (skk-get-prefix skk-current-rule-tree)
          (not skk-comp-stack))
      (skk-set-marker skk-dcomp-start-point nil)
      (skk-set-marker skk-dcomp-end-point nil))

     ;; experimental/skk-pre-henkan.el
     ((and (featurep 'skk-pre-henkan)
           (eq last-command 'skk-comp-do))
      (skk-kakutei))

     ((skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (unless (member (this-command-keys)
                      skk-dcomp-keep-completion-keys)
        (skk-dcomp-delete-completion))))
    ad-do-it
    (when (and skk-j-mode
               (or skk-use-kana-keyboard
                   ;; $BAw$j$"$jJQ49$,;O$^$C$?$iJd40$7$J$$(B
                   (not (memq last-command-event skk-set-henkan-point-key))))
      (if (skk-get-prefix skk-current-rule-tree)
          (when (and (skk-dcomp-multiple-activate-p)
                     (skk-dcomp-multiple-available-p))
            (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates)))
        (skk-dcomp-do-completion (point)))))))

(defadvice skk-set-henkan-point-subr (around skk-dcomp-ad activate)
  (cond
   ((or skk-dcomp-activate
        skk-dcomp-multiple-activate)
    (let ((henkan-mode skk-henkan-mode))
      ad-do-it
      (unless (or henkan-mode
                  (char-after (point)))
        (skk-dcomp-do-completion (point)))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-insert (around skk-dcomp-ad activate)
  (cond
   ((or skk-dcomp-activate
        skk-dcomp-multiple-activate)
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-event '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-comma (around skk-dcomp-ad activate)
  (cond
   ((and (or skk-dcomp-activate
             skk-dcomp-multiple-activate)
         (not (eq last-command 'skk-comp-do)))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-event '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-period (around skk-dcomp-ad activate)
  (cond
   ((and (or skk-dcomp-activate
             skk-dcomp-multiple-activate)
         (not (eq last-command 'skk-comp-do)))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-event '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-comp-previous (after skk-dcomp-ad activate)
  (when (and (skk-dcomp-multiple-activate-p)
             (skk-dcomp-multiple-available-p)
             (or skk-comp-circulate
                 (< 0 skk-dcomp-multiple-select-index)))
    (skk-kana-cleanup 'force)
    (setq skk-dcomp-multiple-select-index
          (cond ((and skk-comp-circulate
                      (< skk-dcomp-multiple-select-index 0))
                 (1- (length skk-comp-stack)))
                (t (1- skk-dcomp-multiple-select-index))))
    (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates t))))

(defadvice skk-kakutei (around skk-dcomp-ad activate)
  (skk-dcomp-before-kakutei)
  ad-do-it
  (skk-dcomp-after-kakutei))

(defadvice keyboard-quit (around skk-dcomp-ad activate)
  (skk-dcomp-before-kakutei)
  ad-do-it
  (skk-dcomp-after-delete-backward-char))

;; (defadvice skk-henkan (before skk-dcomp-ad activate)
(defadvice skk-start-henkan (before skk-dcomp-ad activate)
  (skk-dcomp-cleanup-buffer))

(defadvice skk-process-prefix-or-suffix (before skk-dcomp-ad activate)
  (when skk-henkan-mode
    (skk-dcomp-cleanup-buffer)))

(defadvice skk-comp (around skk-dcomp-ad activate)
  (cond ((and (or skk-dcomp-activate
                  skk-dcomp-multiple-activate)
              (skk-dcomp-marked-p))
         (cond ((integerp (ad-get-arg 0))
                (skk-dcomp-cleanup-buffer)
                ad-do-it)
               (t
                (goto-char skk-dcomp-end-point)
                (setq this-command 'skk-comp-do)
                (skk-dcomp-face-off)
                (skk-set-marker skk-dcomp-start-point nil)
                (skk-set-marker skk-dcomp-end-point nil)
                (when (and (skk-dcomp-multiple-activate-p)
                           (skk-dcomp-multiple-available-p)
                           (or skk-comp-circulate
                               (< skk-dcomp-multiple-select-index
                                  (1- (length skk-dcomp-multiple-candidates)))))
                  (setq skk-dcomp-multiple-select-index
                        (skk-dcomp-multiple-increase-index
                         skk-dcomp-multiple-select-index t))
                  (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates t))))))
        (t
         ad-do-it
         (when (and (skk-dcomp-multiple-activate-p)
                    (skk-dcomp-multiple-available-p))
           (setq skk-dcomp-multiple-select-index
                 (skk-dcomp-multiple-increase-index
                  skk-dcomp-multiple-select-index))
           (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates
                                     ;; skk-comp $B$N(B C-u TAB $B$r9MN8$9$k(B
                                     (not (and current-prefix-arg
                                               (listp current-prefix-arg)))))))))

(defadvice skk-comp-do (before skk-dcomp-ad activate)
  (when (and skk-comp-use-prefix
             (not (string= "" skk-prefix))
             (eq last-command-event skk-next-completion-char))
    (ad-set-arg 0 t)))

(defadvice skk-comp-do (after skk-dcomp-ad activate)
  (when (and (skk-dcomp-multiple-activate-p)
             (skk-dcomp-multiple-available-p)
             ;;(not (ad-get-arg 0))
             (eq last-command-event skk-next-completion-char))
    (skk-kana-cleanup 'force)
    (setq skk-dcomp-multiple-select-index
          (skk-dcomp-multiple-increase-index skk-dcomp-multiple-select-index))
    (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates t))))

(defadvice skk-comp-start-henkan (around skk-dcomp-ad activate)
  (cond ((and (eq skk-henkan-mode 'on)
              (or skk-dcomp-activate
                  skk-dcomp-multiple-activate)
              (skk-dcomp-marked-p))
         (goto-char skk-dcomp-end-point)
         (setq this-command 'skk-comp-do)
         (skk-dcomp-face-off)
         (skk-set-marker skk-dcomp-start-point nil)
         (skk-set-marker skk-dcomp-end-point nil)
         (skk-start-henkan (ad-get-arg 0)))
        (t
         ad-do-it)))

(defadvice skk-delete-backward-char (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice skk-undo (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice viper-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice vip-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice skk-previous-candidate (around skk-dcomp-ad activate)
  (let ((active (eq skk-henkan-mode 'active)))
    ad-do-it
    (when active
      (skk-dcomp-after-delete-backward-char))))

(provide 'skk-dcomp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-dcomp.el ends here
