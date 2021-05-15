;;; skk-decor.el --- decorate the candidate for SKK -*- mode: emacs-lisp; coding: iso-2022-jp -*-

;; Copyright (C) 2020 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

;; Author: 2020 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
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

;; skk-insert()
;; |- skk-start-henkan()
;;    |- skk-henkan()
;;       |- skk-henkan-1()
;;          |- skk-insert-new-word()
;;          |  |- skk-henkan-face-on()
;;          |     |- skk-face-on() @ skk-macs.el
;;          |- skk-henkan-show-candidates()
;;             |- skk-henkan-show-candidate-subr()
;;                |- skk-henkan-candidate-list()
;;                   |- skk-treat-candidate-appearance()

;; $BJ8=q%P%C%U%!$N"'%b!<%I$K$*$1$kJQ498uJd$NI=<($rAu>~$9$k$K$O(B
;; skk-henkan-face $B$r@_Dj$7$^$9!#EvA3$G$9$,!"J8=q%P%C%U%!$NJQ498uJd$K$7(B
;; $B$+:nMQ$7$^$;$s!#(B

;; $B%$%s%i%$%s!"(Btooltip $BKt$O(B $B8uJd%P%C%U%!$NJQ498uJd$NI=<($rAu>~$9$k$K$O(B
;; skk-treat-candidate-appearance-function $B$r@_Dj$7$^$9!#(B
;; $B$7$+$7!"(Bskk-treat-candidate-appearance-function $B$N@_Dj$O!"(B
;;   o $BJ8=q%P%C%U%!$N"'%b!<%I$K$*$1$kJQ498uJd$NI=<($K$b1F6A(B
;;     $B!J(Bskk-henkan-face $B$h$j$bM%@h$7$FE,MQ$5$l$k!K(B
;;   o $B%$%s%i%$%s!"(Btooltip $B5Z$S(B $B8uJd%P%C%U%!$N#3$D$9$Y$F$K1F6A(B
;; $B$H$$$&8z2L$,$"$j$^$9!#(B

;; $BNc$($P!"8uJd%P%C%U%!$NJQ498uJd$NI=<($rBg$-$/$7$h$&$H;W$C$?$H$-!"(B
;; skk-henkan-face $B$G$O8uJd%P%C%U%!$NAu>~$rJQ$($k$3$H$,$G$-$J$$$?$a!"(B
;; skk-treat-candidate-appearance-function $B$r;HMQ$7$F%5%$%:$NBg$-$J(B
;; face $B$rE,MQ$5$;$^$9!#$7$+$7!"(Bskk-treat-candidate-appearance-function $B$O(B
;; $BJ8=q%P%C%U%!$NAu>~$K$b1F6A$,5Z$V$3$H$+$i!"I=<($,%,%?%,%?$K$J$C$F$7$^(B
;; $B$$$^$9!#(B

;; $B$=$3$G!"!VJ8=q%P%C%U%!$K1F6A$;$:!"8uJd%P%C%U%!$K8B$C$?I=<($NAu>~!W$r(B
;; $B$7$?$$J}$N$?$a$K:n$C$F$_$^$7$?!#(B

;;; Howto:

;; (setq skk-inline-show-vertically-decor
;;       #'skk-inline-show-vertically-decor-func)

;; (setq skk-tooltip-show-at-point-decor
;;       #'skk-tooltip-show-at-point-decor-func)

;; (setq skk-henkan-show-candidates-buffer-decor
;;       #'skk-henkan-show-candidates-buffer-decor-func)


;;; Code:

(require 'skk)

;; skk-show-inline 'vertical $B$K8B$C$F%U%'%$%9$r:nMQ$5$;$k(B.
;;;###autoload
(defun skk-inline-show-vertically-decor-func (string)
  (let* ((sp (reverse (split-string string "\n")))
         (rest (car sp))
         (cands (reverse (cdr sp))))
    (concat (mapconcat
             (lambda (x)
               (let* ((d (skk-treat-strip-note-from-word (substring x 2)))
                      (c (car d))
                      (n (cdr d)))
                 (concat (substring x 0 2)
                         (propertize c 'face 'skk-inline-show-vertically-cand-face)
                         (when n
                           (concat " " (propertize n 'face 'skk-inline-show-vertically-anno-face))))))
             cands "\n")
            "\n" rest)))

;; tooltip $B$K8B$C$F%U%'%$%9$r:nMQ$5$;$k(B
;;;###autoload
(defun skk-tooltip-show-at-point-decor-func (text)
  (let* ((sp (reverse (split-string text "\n")))
         (rest (car sp))
         (cands (reverse (cdr sp))))
    (concat (mapconcat
             (lambda (x)
               (let* ((d (skk-treat-strip-note-from-word (substring x 2)))
                      (c (car d))
                      (n (cdr d)))
                 (concat (substring x 0 2)
                         (propertize c 'face 'skk-tooltip-show-at-point-cand-face)
                         (when n
                           (concat " " (propertize n 'face 'skk-tooltip-show-at-point-anno-face))))))
             cands "\n")
            "\n" rest)))

;; $B8uJd%P%C%U%!$K8B$C$F%U%'%$%9$r:nMQ$5$;$k(B
;;;###autoload
(defun skk-henkan-show-candidates-buffer-decor-func (str)
  (let* ((cand (reverse (split-string str "  " t)))
         (nokori (car cand))
         (cand (reverse (cdr cand))))
    (concat (mapconcat (lambda (x)
                         (let* ((d (skk-treat-strip-note-from-word (substring x 2)))
                                (c (car d))
                                (n (cdr d)))
                           (concat (substring x 0 2) ; `A:'
                                   (propertize c 'face 'skk-henkan-show-candidates-buffer-cand-face)
                                   (when n
                                     (concat " " (propertize n 'face 'skk-henkan-show-candidates-buffer-anno-face))))))
                       cand "  ")
            "  " nokori)))

(provide 'skk-decor)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-decor.el ends here
