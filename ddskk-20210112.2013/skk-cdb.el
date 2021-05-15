;;;  skk-cdb.el --- Directly search words in CDB dictionary

;; Copyright (C) 2010  Yusuke Shinyama <yusuke at cs . nyu . edu>

;; Author: Yusuke Shinyama <yusuke at cs . nyu . edu>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method, cdb

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

;; Usage: Set skk-cdb-large-jisyo in your ~/.skk like the following.

;;   (setq skk-cdb-large-jisyo "/usr/share/skk/SKK-JISYO.L.cdb")


;;; Code:

(require 'cdb)
(require 'skk)

(defconst skk-cdb-version "20100719+")

(defvar skk-cdb-working-buffer " *skk-cdb*")

;;;###autoload
(defun skk-search-cdb-jisyo (cdb-path)
  (unless cdb-path
    (setq cdb-path skk-cdb-large-jisyo))
  (when (and (stringp cdb-path)
             (file-readable-p cdb-path))
    (cdb-init cdb-path)
    (let* ((key (if skk-use-numeric-conversion
                    (skk-num-compute-henkan-key skk-henkan-key)
                  skk-henkan-key))
           (bkey (encode-coding-string key skk-cdb-coding-system))
           (bval (cdb-get cdb-path bkey))
           l)
      (when bval
        (with-current-buffer (get-buffer-create skk-cdb-working-buffer)
          (erase-buffer)
          (insert (decode-coding-string bval skk-cdb-coding-system))
          (goto-char (point-min))
          (when (eq (following-char) ?/)
            (let ((okurigana (or skk-henkan-okurigana skk-okuri-char)))
              (forward-char 1)
              (setq l (skk-compute-henkan-lists okurigana))
              (when l
                (cond ((and okurigana skk-henkan-okuri-strictly)
                       (nth 2 l))
                      ((and okurigana skk-henkan-strict-okuri-precedence)
                       (skk-nunion (nth 2 l) (car l)))
                      (t
                       (car l)))))))))))

(provide 'skk-cdb)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-cdb.el ends here
