;;; skk-cursor.el --- SKK cursor control -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Masatake YAMATO <masata-y@is.aist-nara.ac.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
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

(unless (skk-color-cursor-display-p)
  (error "%s" "SKK-CURSOR requires color display"))

(require 'skk-macs)
(require 'skk-vars)

(eval-and-compile
  (require 'ccc))

;; Functions.

(defun skk-cursor-default-color ()
  (ccc-frame-cursor-color))

;;;###autoload
(defun skk-cursor-current-color ()
  ;; カレントバッファの SKK のモードから、カーソルの色を取得する。
  (cond ((not (and skk-use-color-cursor
                   skk-mode))
         (skk-cursor-default-color))

        ;; `skk-start-henkan' の中では、skk-j-mode フラグを立てながら、
        ;; skk-abbrev-mode フラグも立てている (変換後、直後に入力する文
        ;; 字が元の入力モードにて行われるように)。従い、skk-abbrev-mode
        ;; フラグのチェックの優先度を上げる。
        (skk-abbrev-mode skk-cursor-abbrev-color)

        (skk-jisx0208-latin-mode skk-cursor-jisx0208-latin-color)
        (skk-katakana skk-cursor-katakana-color)
        (skk-j-mode skk-cursor-hiragana-color)
        (skk-jisx0201-mode skk-cursor-jisx0201-color)
        (t skk-cursor-latin-color)))

;;;###autoload
(defun skk-cursor-set-1 (color)
  (when ccc-default-cursor-color
    (ccc-set-buffer-local-cursor-color (or color
                                           (skk-cursor-current-color)))))

;;;###autoload
(defun skk-cursor-off-1 ()
  (when ccc-default-cursor-color
    (ccc-set-cursor-color-buffer-local nil)))

(provide 'skk-cursor)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-cursor.el ends here
