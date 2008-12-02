;;; mule-gbk.el --- Chinese GBK support for Mule/Emacs

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



;; Variables

(defconst mule-gbk-version "0.1.2004080701")
(defconst mule-gbk-maintainer-address "yoyosu@ustc.edu.cn")

(require 'chinese-gbk)

(eval-when-compile
  (load-file "characters-gbk.el"))

(require 'characters-gbk)
(if window-system
    (require 'fontset-gbk))

;;; For X selection
;;(setq x-select-enable-clipboard t)
(defun mule-gbk-selection-setup ()
  "Setup X Selection for mule-gbk."
  (interactive)
  (when window-system
    (set-selection-coding-system 'compound-text-with-extensions)
    ;; Check to see if `non-standard-icccm-encodings-alist' has
    ;; been renamed to `ctext-non-standard-encodings-alist'.
    ;; Thanks Zhan Jian <zhanjian@ustc.edu> for the CVS version
    ;; Emacs X Selection support. 
    ;;                  --  Wed,  9 Jun 2004 15:25:50 +0800
    (if (boundp 'ctext-non-standard-encodings-alist)
        ;; For Emacs CVS (21.3.50)
        (progn
          (add-to-list 'ctext-non-standard-encodings-alist
                       '("gbk-0" chinese-gbk 2
                         (chinese-cns11643-5
                          chinese-cns11643-6
                          chinese-cns11643-7)))
          (add-to-list 'ctext-non-standard-encodings "gbk-0"))
      ;; For Emacs21
      (add-to-list 'non-standard-icccm-encodings-alist
                   '("gbk-0" . chinese-gbk)))
    ;; our `ctext-pre-write-conversion' needs `string-reverse'.
    (unless (fboundp 'string-reverse)
      (defun string-reverse (str)
        "Reverse STR."
        (apply 'string (nreverse (string-to-list str)))))
    ;; Redefine `ctext-pre-write-conversion' to fit our need.
    (defun ctext-pre-write-conversion (from to)
      "Encode characters between FROM and TO as Compound Text w/Extended Segments.

If FROM is a string, or if the current buffer is not the one set up for us
by run_pre_post_conversion_on_str, generate a new temp buffer, insert the
text, and convert it in the temporary buffer.  Otherwise, convert in-place."
      (let (str)
        (cond ((and (string= (buffer-name) " *code-converting-work*")
                    (not (stringp from)))
               ;; Minimize consing due to subsequent insertions and deletions.
               (buffer-disable-undo)
               (setq str (buffer-substring-no-properties from to))
               (delete-region (point-min) (point-max)))
              (t
               (if (stringp from)
                   (setq str from)
                 (setq str (buffer-substring-no-properties from to))
                 (delete-region from to))))
        (set-buffer-multibyte nil)
        ;; nice hack!
        (insert
         (string-reverse
          ;; encode `str' into compound text in reversed order
          (encode-coding-string (string-reverse str)
                                'chinese-gbk-compound-text-reverse)))
        (set-buffer-multibyte t))
      nil))
  )

;; `mm-util' is Utility functions for Mule and low level things,
;; which is part of Gnus and GNU Emacs.
;; We need to add mime charset stuffs of GBK to
;; `mm-charset-synonym-alist' after `mm-util' loaded.
(eval-after-load "mm-util"
  '(setq mm-charset-synonym-alist
         (append
          '((x-gbk       . gbk)
            (cp936       . gbk)
            (ms936       . gbk)
            (windows-936 . gbk))
          mm-charset-synonym-alist)))

(provide 'mule-gbk)
;;; mule-gbk.el ends here

