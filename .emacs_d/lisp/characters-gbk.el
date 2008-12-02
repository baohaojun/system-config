;;; characters-gbk.el --- syntax for Chinese GBK characters

;; Copyright (C) 2002, 2003, 2004  Su Yong <yoyosu@ustc.edu.cn>

;; Author: Su Yong <yoyosu@ustc.edu.cn>
;; Keywords: multilingual, Chinese, GBK

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:


;; Chinese character set (GBK)

;;; Setting syntax and category.
(let ((st (standard-syntax-table))
      (ct (standard-category-table)))
  (let ((gbk-list '(chinese-cns11643-5
                    chinese-cns11643-6
                    chinese-cns11643-7))
        generic-charset)
    (while gbk-list
      (setq generic-charset (car gbk-list))
      (modify-syntax-entry (make-char generic-charset) "w" st)
      (modify-category-entry (make-char generic-charset) ?c ct)
      (modify-category-entry (make-char generic-charset) ?C ct)
      (modify-category-entry (make-char generic-charset) ?\| ct)
      (setq gbk-list (cdr gbk-list))))

  (modify-syntax-entry ?¡° "_" st)
  (modify-syntax-entry ?¡± "_" st)

  (modify-syntax-entry ?\¡² "(¡³" st)
  (modify-syntax-entry ?\¡´ "(¡µ" st)
  (modify-syntax-entry ?\¡¶ "(¡·" st)
  (modify-syntax-entry ?\¡¸ "(¡¹" st)
  (modify-syntax-entry ?\¡º "(¡»" st)
  (modify-syntax-entry ?\¡¼ "(¡½" st)
  (modify-syntax-entry ?\¡¾ "(¡¿" st)
  (modify-syntax-entry ?\¡³ ")¡²" st)
  (modify-syntax-entry ?\¡µ ")¡´" st)
  (modify-syntax-entry ?\¡· ")¡¶" st)
  (modify-syntax-entry ?\¡¹ ")¡¸" st)
  (modify-syntax-entry ?\¡» ")¡º" st)
  (modify-syntax-entry ?\¡½ ")¡¼" st)
  (modify-syntax-entry ?\¡¿ ")¡¾" st))


;;; Setting word boundary.
;;; Needed by `forward-word'.
(add-to-list 'word-combining-categories
             '(?C . ?C))

;; For each character set, put the information of the most proper
;; coding system to encode it by `preferred-coding-system' property.
(let ((l '((chinese-cns11643-5 . chinese-gbk)
	   (chinese-cns11643-6 . chinese-gbk)
	   (chinese-cns11643-7 . chinese-gbk))))
  (while l
    (put-charset-property (car (car l)) 'preferred-coding-system (cdr (car l)))
    (setq l (cdr l))))

;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLIE are already set.  Also put `nospace-between-words'
;; property to the charsets.
;; See also `lisp/textmodes/fill.el' line 474, in GNU Emacs21.3.
(let ((l '(chinese-cns11643-5 chinese-cns11643-6 chinese-cns11643-7)))
  (while l
    (aset auto-fill-chars (make-char (car l)) t)
    (put-charset-property (car l) 'nospace-between-words t)
    (setq l (cdr l))))


(provide 'characters-gbk)

;;; Local Variables:
;;; coding: chinese-gbk
;;; End:

;;; characters-gbk.el ends here
