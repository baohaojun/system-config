;;; fontset-gbk.el --- Fontset setup for Chinese GBK

;; Copyright (C) 2002, 2003, 2004  Su Yong <yoyosu@ustc.edu.cn>

;; Author: Su Yong <yoyosu@ustc.edu.cn>
;; Keywords: multilingual, Chinese, GBK, fontset

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

(require 'fontset)

(let ((l `((chinese-cns11643-5 . ("*" . "gbk"))
           (chinese-cns11643-6 . ("*" . "gbk"))
           (chinese-cns11643-7 . ("*" . "gbk"))
           ))
      charset font-spec arg)
  (while l
    (setq charset (car (car l)) font-spec (cdr (car l)) l (cdr l))
    (if (symbolp charset)
        (setq arg (make-char charset))
      (setq arg charset))
    (set-fontset-font "fontset-default" arg font-spec)
    (set-fontset-font "fontset-standard" arg font-spec)))


;; Create standard fontset from 16 dots fonts which are the most widely
;; installed fonts.  Fonts for Chinese-GB, Korean, and Chinese-CNS are
;; specified here because FAMILY of those fonts are not "fixed" in
;; many cases.
(defvar standard-fontset-spec
  (purecopy "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard,
	chinese-gb2312:-*-medium-r-normal-*-16-*-gb2312*-*,
	korean-ksc5601:-*-medium-r-normal-*-16-*-ksc5601*-*,
	chinese-cns11643-5:-*-medium-r-normal-*-16-*-gbk*-0,
	chinese-cns11643-6:-*-medium-r-normal-*-16-*-gbk*-0,
	chinese-cns11643-7:-*-medium-r-normal-*-16-*-gbk*-0,
         sjis:-*-medium-r-normal--20-*-jisx0208*-*")
  "String of fontset spec of the standard fontset.
You have the biggest chance to display international characters
with correct glyphs by using the standard fontset.
See the documentation of `create-fontset-from-fontset-spec' for the format.")



(define-ccl-program ccl-encode-gbk-font
  `(0
    ;; In:  R0:chinese-gbk-1 or chinese-gbk-2
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
    (if (r0 == ,(charset-id 'ascii))
	((r2 = r1)
	 (r1 = 0))
      (if (r0 == ,(charset-id 'chinese-cns11643-5))
          ((r1 *= 94)
           (r1 += r2)
           (r1 -= ,(+ (* 33 94) 33))
           (r1 //= ?\xBE) ; 0xBE == 0xFF - 0x80 + 0x7F - 0x40
           (r2 = r7)
           (if (r2 < ?\x3F)
               (r2 += ?\x40)
             (r2 += ?\x41))
           (r1 += ?\x81)) ; part 1
        (if (r0 == ,(charset-id 'chinese-cns11643-6))
            ((r1 *= 94)
             (r1 += r2)
             (r1 -= ,(+ (* 33 94) 33))
             (r1 //= ?\xBE) ; 0xBE == 0xFF - 0x80 + 0x7F - 0x40
             (r2 = r7)
             (if (r2 < ?\x3F)
                 (r2 += ?\x40)
               (r2 += ?\x41))
             (r1 += ?\xAB)) ; part 2
          (if (r0 == ,(charset-id 'chinese-cns11643-7))
              ((r1 *= 94)
               (r1 += r2)
               (r1 -= ,(+ (* 33 94) 33))
               (r1 //= ?\xBE) ; 0xBE == 0xFF - 0x80 + 0x7F - 0x40
               (r2 = r7)
               (if (r2 < ?\x3F)
                   (r2 += ?\x40)
                 (r2 += ?\x41))
               (r1 += ?\xD5)) ; part 3
            (end)))))))
        
        
            

(setq font-ccl-encoder-alist
      (cons '("gbk" . ccl-encode-gbk-font)
	    font-ccl-encoder-alist))


(setq x-pixel-size-width-font-regexp
      (concat x-pixel-size-width-font-regexp "\\|gbk"))

(setq vertical-centering-font-regexp
      (concat vertical-centering-font-regexp "\\|gbk"))



(provide 'fontset-gbk)
;;; fontset-gbk.el ends here
