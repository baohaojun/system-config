;;; skk-jisx0213.el --- SKK 用 JISX0213 文字コード関連プログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method
;; Created: Sep. 30, 2000.

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

(require 'skk)

;;;###autoload
(defun skk-jisx0213-henkan-list-filter ()
  ;; remove candidate that contains jisx0213 characters from
  ;; SKK-HENKAN-LIST.
  (let ((henkan-list (nthcdr skk-henkan-count skk-henkan-list))
        e charset)
    (while (setq e (car henkan-list))
      (setq charset (find-charset-string (if (consp e) (cdr e) e)))
      (if (or (memq 'japanese-jisx0213-1 charset)
              (memq 'japanese-jisx0213-2 charset))
          (setq skk-henkan-list (delq e skk-henkan-list)
                henkan-list (delq e henkan-list))
        (setq henkan-list (cdr henkan-list))))))

(provide 'skk-jisx0213)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-jisx0213.el ends here
