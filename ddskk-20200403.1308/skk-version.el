;;; skk-version.el --- version information for SKK -*- coding:iso-2022-jp -*-

;; Copyright (C) 2000, 2001, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

;;; Code:

(require 'skk-vars)

(put 'skk-version 'product-name "Daredevil SKK")
(put 'skk-version 'version-string
     (let ((ver "17.0.50")
           (mepla "mepla")
           (path (catch 'exit
                   (dolist (p load-path)
                     ;; ;; ~/.emacs.d/elpa/ddskk-20150107.409/
                     (when (string-match "ddskk-[0-9]+\.[0-9]+" p)
                       (throw 'exit p))))))
       (if path
           (format "%s/%s-%s" ver mepla
                   (car (cdr (split-string path "ddskk-"))))
         ver)))
(put 'skk-version 'codename "Neppu") ; See also `READMEs/CODENAME.ja'
(put 'skk-version 'codename-ja "熱郛")

;;;###autoload
(defun skk-version (&optional without-codename)
  "Return SKK version with its codename.
If WITHOUT-CODENAME is non-nil, simply return SKK version without
the codename."
  (interactive "P")
  (if (called-interactively-p 'interactive)
      (message "%s" (skk-version without-codename))
    (if without-codename
        (format "%s/%s"
                (get 'skk-version 'product-name)
                (get 'skk-version 'version-string))
      (format "%s/%s (%s)"
              (get 'skk-version 'product-name)
              (get 'skk-version 'version-string)
              (if skk-version-codename-ja
                  (get 'skk-version 'codename-ja)
                (get 'skk-version 'codename))
              ))))

(provide 'skk-version)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-version.el ends here
