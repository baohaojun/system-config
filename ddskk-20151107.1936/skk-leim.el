;;; skk-leim.el --- SKK related code for LEIM

;; Copyright (C) 1997, 1999, 2000
;;   Murata Shuuichirou <mrt@astec.co.jp>

;; Author: Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-leim.el,v 1.20 2012/08/11 10:48:38 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2012/08/11 10:48:38 $

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

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

;;;###autoload
(defun skk-activate (&optional name)
  (skk-set-deactivate-im-func 'skk-inactivate)
  (skk-mode 1)
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'skk-leim-exit-from-minibuffer)))

;;;###autoload
(defun skk-auto-fill-activate (&optional name)
  (skk-set-deactivate-im-func 'skk-auto-fill-inactivate)
  (skk-auto-fill-mode 1)
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'skk-leim-exit-from-minibuffer)))

;;;###autoload
(defun skk-inactivate ()
  (skk-mode -1))

;;;###autoload
(defun skk-auto-fill-inactivate ()
  (skk-auto-fill-mode -1))

(defun skk-leim-exit-from-minibuffer ()
  (skk-deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'skk-leim-exit-from-minibuffer)))

;;;###autoload
(register-input-method
 "japanese-skk" "Japanese"
 'skk-activate ""
 "Simple Kana to Kanji conversion program")

;;;###autoload
(register-input-method
 "japanese-skk-auto-fill" "Japanese"
 'skk-auto-fill-activate ""
 "Simple Kana to Kanji conversion program with auto-fill")

(provide 'skk-leim)

;;; skk-leim.el ends here
