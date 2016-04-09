;;; skk-cursor.el --- SKK cursor control -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(unless (skk-color-cursor-display-p)
  (error "%s" "SKK-CURSOR requires color display"))

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(eval-and-compile
  (when (featurep 'emacs)
    (require 'ccc)))

;; Functions.

(defun skk-cursor-default-color ()
  (cond ((eval-when-compile (featurep 'xemacs))
	 (frame-property (selected-frame) 'cursor-color))
	(t
	 (ccc-frame-cursor-color))))

;;;###autoload
(defun skk-cursor-current-color ()
  ;; カレントバッファの SKK のモードから、カーソルの色を取得する。
  (cond
   ((not (and skk-use-color-cursor
	      skk-mode))
    (skk-cursor-default-color))
   ;; `skk-start-henkan' の中では、skk-j-mode フラグを立てながら、
   ;; skk-abbrev-mode フラグも立てている (変換後、直後に入力する文
   ;; 字が元の入力モードにて行われるように)。従い、skk-abbrev-mode
   ;; フラグのチェックの優先度を上げる。
   (skk-abbrev-mode
    skk-cursor-abbrev-color)
   (skk-jisx0208-latin-mode
    skk-cursor-jisx0208-latin-color)
   (skk-katakana
    skk-cursor-katakana-color)
   (skk-j-mode
    skk-cursor-hiragana-color)
   (skk-jisx0201-mode
    skk-cursor-jisx0201-color)
   (t
    skk-cursor-latin-color)))

;;;###autoload
(defun skk-cursor-set-1 (color)
  (cond
   ((eval-when-compile (featurep 'xemacs))
    ;;At 10 Jul 2000 16:37:49 +0900,
    ;;Yoshiki Hayashi <t90553@mail.ecc.u-tokyo.ac.jp> wrote:
    ;;> foreground を background に変える必要があること以外は、今の
    ;;> ところそのままで動いているようです。しばらく test してみます。
    ;;> どうも、text-cursor も普通の face のようで、foreground が文
    ;;> 字の色を、background が文字の背景の色を表しているようです。
    (set-face-property 'text-cursor
		       'background
		       (or color
			   (skk-cursor-current-color))
		       (current-buffer)))
   (t
    (when ccc-default-cursor-color
      (ccc-set-buffer-local-cursor-color
       (or color
	   (skk-cursor-current-color)))))))

;;;###autoload
(defun skk-cursor-off-1 ()
  (cond ((eval-when-compile (featurep 'xemacs))
	 (skk-cursor-set))
	(t
	 (when ccc-default-cursor-color
	   (ccc-set-cursor-color-buffer-local nil)))))

;; advices.
(when (eval-when-compile (featurep 'xemacs))
  (defadvice minibuffer-keyboard-quit (before skk-cursor-ad activate)
    (unless skk-henkan-mode
      (skk-cursor-set (skk-cursor-default-color)))))

;; Hooks
(when (eval-when-compile (featurep 'xemacs))
  (add-hook 'isearch-mode-end-hook #'skk-cursor-set 'append)

  (add-hook 'minibuffer-setup-hook #'skk-cursor-set 'append)

  (add-hook 'minibuffer-exit-hook
	    (lambda ()
	      (with-current-buffer (skk-minibuffer-origin)
		(skk-cursor-set))
	      (skk-cursor-set skk-cursor-default-color 'force))
	    'append))

(provide 'skk-cursor)

;;; skk-cursor.el ends here
