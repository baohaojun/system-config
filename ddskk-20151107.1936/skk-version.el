;;; skk-version.el --- version information for SKK -*- coding:iso-2022-jp -*-

;; Copyright (C) 2000, 2001, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-version.el,v 1.80 2014/12/06 00:29:51 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2014/12/06 00:29:51 $

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
(eval-and-compile
  (require 'skk-macs))

(put 'skk-version 'product-name "Daredevil SKK")
(put 'skk-version 'version-string
     (let ((ver "16.0.51")
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
(put 'skk-version 'codename "Futamata") ; See also `READMEs/CODENAME.ja'
(put 'skk-version 'codename-ja "二股")

;;;###autoload
(defun skk-version (&optional without-codename)
  "Return SKK version with its codename.
If WITHOUT-CODENAME is non-nil, simply return SKK version without
the codename."
  (interactive "P")
  (if (skk-called-interactively-p 'interactive)
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

;;; skk-version.el ends here
