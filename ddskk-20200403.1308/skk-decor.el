;;; skk-decor.el --- decorate the candidate for SKK -*- mode: emacs-lisp; coding: iso-2022-jp -*-

;; Copyright (C) 2020 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

;; Author: 2020 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
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

;; 文書バッファの▼モードにおける変換候補の表示を装飾するには
;; skk-henkan-face を設定します。当然ですが、文書バッファの変換候補にし
;; か作用しません。

;; インライン、tooltip 又は 候補バッファの変換候補の表示を装飾するには
;; skk-treat-candidate-appearance-function を設定します。
;; しかし、skk-treat-candidate-appearance-function の設定は、
;;   o 文書バッファの▼モードにおける変換候補の表示にも影響
;;     （skk-henkan-face よりも優先して適用される）
;;   o インライン、tooltip 及び 候補バッファの３つすべてに影響
;; という効果があります。

;; 例えば、候補バッファの変換候補の表示を大きくしようと思ったとき、
;; skk-henkan-face では候補バッファの装飾を変えることができないため、
;; skk-treat-candidate-appearance-function を使用してサイズの大きな
;; face を適用させます。しかし、skk-treat-candidate-appearance-function は
;; 文書バッファの装飾にも影響が及ぶことから、表示がガタガタになってしま
;; います。

;; そこで、「文書バッファに影響せず、候補バッファに限った表示の装飾」を
;; したい方のために作ってみました。

;;; Howto:

;; (setq skk-inline-show-vertically-decor
;;       #'skk-inline-show-vertically-decor-func)

;; (setq skk-tooltip-show-at-point-decor
;;       #'skk-tooltip-show-at-point-decor-func)

;; (setq skk-henkan-show-candidates-buffer-decor
;;       #'skk-henkan-show-candidates-buffer-decor-func)


;;; Code:

(require 'skk)

;; skk-show-inline 'vertical に限ってフェイスを作用させる.
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

;; tooltip に限ってフェイスを作用させる
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

;; 候補バッファに限ってフェイスを作用させる
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
