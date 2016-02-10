;;  skk-cdb.el --- Directly search words in CDB dictionary

;;  Copyright (C) 2010  Yusuke Shinyama <yusuke at cs . nyu . edu>

;; Author: Yusuke Shinyama <yusuke at cs . nyu . edu>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method, cdb

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

;;  Usage: Set skk-cdb-large-jisyo in your ~/.skk like the following.
;;
;;    (setq skk-cdb-large-jisyo "/usr/share/skk/SKK-JISYO.L.cdb")
;;

(require 'cdb)
(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(defconst skk-cdb-version "20100719+")

;;  skk-cdb-working-buffer
;;
(defvar skk-cdb-working-buffer " *skk-cdb*")

;;  skk-search-cdb-jisyo
;;
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

;;; skk-cdb.el ends here
