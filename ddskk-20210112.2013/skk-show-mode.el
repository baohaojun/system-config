;;; skk-show-mode.el --- モード切替時に tooltip/inline 表示する -*- coding: iso-2022-jp -*-

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

;;  ~/.skk にて
;;      (setq skk-show-mode-show t)
;;  と設定してください。
;;  Emacs 起動中は M-x skk-show-mode でトグルすることができます。

;;  変数 skk-show-mode-style に 'tooltip 又は 'inline を設定してください。

;;; Code:

(require 'skk-emacs)

(defadvice skk-isearch-set-initial-mode (before skk-show-mode activate)
  (setq skk-show-mode-show nil))

(defadvice skk-isearch-initialize-working-buffer (before skk-show-mode activate)
  (setq skk-show-mode-show nil))

(defadvice skk-cursor-set (after skk-show-mode activate)
  "かなモードやアスキーモードへ切り替わったときに skk-*-mode-string を
tooltip / inline 表示する."
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
  ;; skk-dcomp-multiple-show() から拝借
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
             ;; バッファ最終行では、普通に overlay を追加していく方法だと
             ;; overlay の表示される順番が狂うことがあってうまくない。
             ;; したがって前回の overlay の after-string に追加する。
             ;; ただし、EOB の場合は prefix の overlay と衝突するため
             ;; `skk-prefix-overlay' に追加する
             (setq ol (cond ((or (not skk-echo)
                                 (string= "" skk-prefix)
                                 (< (overlay-end skk-prefix-overlay)
                                    (point)))
                             (make-overlay (point) (point)))
                            (t skk-prefix-overlay)))

             (setq str (concat (overlay-get ol 'after-string)
                               "\n" (make-string beg-col ? ) str)))
            ;; bottom 以外
            (t
             (setq col (skk-move-to-screen-column beg-col))
             (cond ((> beg-col col)
                    ;; 桁合わせの空白を追加
                    (setq str (concat (make-string (- beg-col col) ? )
                                      str)))
                   ;; overlay の左端がマルチ幅文字と重なったときの微調整
                   ((< beg-col col)
                    (backward-char)
                    (setq col (skk-screen-column))
                    (setq str (concat (make-string (- beg-col col) ? )
                                      str))))))

      ;; この時点で overlay の開始位置に point がある
      (unless bottom
        (let ((ol-beg (point))
              (ol-end-col (+ col (string-width str)))
              base-ol)
          (setq col (skk-move-to-screen-column ol-end-col))
          ;; overlay の右端がマルチ幅文字と重なったときの微調整
          (when (< ol-end-col col)
            (setq str (concat str
                              (make-string (- col ol-end-col) ? ))))
          (setq ol (make-overlay ol-beg (point)))

          ;; 元テキストの face を継承しないように1つ後ろに overlay を作って、
          ;; その face を 'default に指定しておく
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
