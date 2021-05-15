;;; skk-leim.el --- SKK related code for LEIM

;; Copyright (C) 1997, 1999, 2000
;;   Murata Shuuichirou <mrt@astec.co.jp>

;; Author: Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
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

;; SKK related code for LEIM

;;; Code:

(require 'skk)

;;;###autoload
(defun skk-activate (&optional name)
  (setq deactivate-current-input-method-function 'skk-inactivate)
  (skk-mode 1)
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'skk-leim-exit-from-minibuffer)))

;;;###autoload
(defun skk-auto-fill-activate (&optional name)
  (setq deactivate-current-input-method-function 'skk-auto-fill-inactivate)
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
  (deactivate-input-method)
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

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-leim.el ends here
