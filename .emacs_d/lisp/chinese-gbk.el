;;; chinese-gbk.el --- Chinese GBK support

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


;; (define-charset nil 'chinese-gbk-1
;;   [2 94 2 0 ?K 0 "GBK-1" "GBK-1 (Chinese)"
;;      "GBK Chinese Part 1"])
;; (define-charset nil 'chinese-gbk-2
;;   [2 94 2 0 ?L 0 "GBK-2" "GBK-2 (Chinese)"
;;      "GBK Chinese Part 2"])
;; (define-charset nil 'chinese-gbk-3
;;   [2 94 2 0 ?M 0 "GBK-3" "GBK-3 (Chinese)"
;;      "GBK Chinese Part 3"])


;; Part 1:
;; 0x81 - 0xAA : 0x2A chars * 0xBE == 0x1F2C == 7980
;; Part 2:
;; 0xAB - 0xD4 : 0x2A chars * 0xBE == 0x1F2C == 7980
;; Part 3:
;; 0xD5 - 0xFE : 0x2A chars * 0xBE == 0x1F2C == 7980


;; Variables

(require 'mule)
(define-ccl-program ccl-decode-chinese-gbk
  `(2
    ((loop
      (read r0)
     
      ;; 1 byte encoding, i.e., ascii
      (if (r0 < ?\x80)
          (write r0)

        ;; 2 byte encoding
        (if (r0 > ?\x80)
            (if (r0 < ?\xFF)
                ((read r1)
                

                 (if (r1 > ?\x3F)
                     (if (r1 < ?\x7F)
                         (r1 -= ?\x40)
                       (if (r1 > ?\x7F)
                           (if (r1 < ?\xFF)
                               (r1 -= ?\x41)
                             ((write r0 r1)
                              (repeat)))
                         ((write r0 r1)
                          (repeat))))
                   ((write r0 r1)
                    (repeat)))
                       
                                
                 (if (r0 < ?\xAB)
                     ((r2 = (r0 - ?\x81)) ;; part 1
                      (r0 = ,(charset-id 'chinese-cns11643-5)))
                   (if (r0 < ?\xD5)
                       ((r2 = (r0 - ?\xAB)) ;; part 2
                        (r0 = ,(charset-id 'chinese-cns11643-6)))
                     ((r2 = (r0 - ?\xD5)) ;; part 3
                      (r0 = ,(charset-id 'chinese-cns11643-7)))))

                 (r2 *= ?\xBE) ;; 0xBE == 0xFF - 0x80 + 0x7F - 0x40
                 (r1 += r2)
                 (r2 = (r1 / 94))
                 (r2 <<= 7)
                 (r1 %= 94)
                 (r1 |= r2)
                 (r1 += ,(+ (* 33 128) 33))
                 (write-multibyte-character r0 r1))
              ((write r0 r1)
               (repeat)))
          ((write r0 r1)
           (repeat))))
      (repeat))))

  "CCL program to decode GBK.")


(define-ccl-program ccl-encode-chinese-gbk
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (write r1)

        (if (r0 == ,(charset-id 'chinese-cns11643-5))
            ((r0 = ((((r1 & 16256) >> 7) - 33) * 94))
             ;; 16256 == (0011 1111 1000 0000)b
             (r1 &= ?\x7F)
             (r1 += (r0 - 33))
             (r0 = (r1 / ?\xBE))     ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
             (r1 %= ?\xBE)           ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
             (r0 += ?\x81) ;; part 1
             (if (r1 < ?\x3F)
                 (r1 += ?\x40)
               (r1 += ?\x41))
             (write r0 r1))
         
          (if (r0 == ,(charset-id 'chinese-cns11643-6))
              ((r0 = ((((r1 & 16256) >> 7) - 33) * 94))
               ;; 16256 == (0011 1111 1000 0000)b
               (r1 &= ?\x7F)
               (r1 += (r0 - 33))
               (r0 = (r1 / ?\xBE))   ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
               (r1 %= ?\xBE)         ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
               (r0 += ?\xAB) ;; part 2
               (if (r1 < ?\x3F)
                   (r1 += ?\x40)
                 (r1 += ?\x41))
               (write r0 r1))
           
            (if (r0 == ,(charset-id 'chinese-cns11643-7))
                ((r0 = ((((r1 & 16256) >> 7) - 33) * 94))
                 ;; 16256 == (0011 1111 1000 0000)b
                 (r1 &= ?\x7F)
                 (r1 += (r0 - 33))
                 (r0 = (r1 / ?\xBE)) ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
                 (r1 %= ?\xBE)       ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
                 (r0 += ?\xD5) ;; part 3
                 (if (r1 < ?\x3F)
                     (r1 += ?\x40)
                   (r1 += ?\x41))
                 (write r0 r1))
              (if (r0 == ,(charset-id 'chinese-gb2312))
                  ;; 16256 == (0011 1111 1000 0000)b
                  ((r0 = ((r1 & 16256) >> 7))
                   (r0 += 128)
                   ;; 127 == (0111 1111)b
                   (r1 = (r1 & 127))
                   (r1 += 128)
                   (write r0 r1)))
              ))))
      (repeat))))

  "CCL program to encode into GBK.")


(make-coding-system
 'chinese-gbk 4 ?Z
 "Chinese GBK encoding for GBK characters mainly used
on Chinese PCs (MIME:GBK)"

 '(ccl-decode-chinese-gbk . ccl-encode-chinese-gbk)
 '((safe-charsets
    ascii
    chinese-cns11643-5
    chinese-cns11643-6
    chinese-cns11643-7
    chinese-gb2312
    )
   (mime-charset . gbk)
   (valid-codes (0 . 254))))



;; (define-ccl-program ccl-decode-chinese-gbk-selection
;;   `(2
;;     ((r3 = 0) ;; clear read cache flag
;;      (loop
;;       (if (r3 == 0)
;;           (read r0)
;;         ((r0 = r4) ;; read from cache
;;          (r3 = 0)))

;;       ;; remove escape sequence
;;       ;; A(ascii): [0x1B 0x28 0x42]
;;       ;; G(GBK):   [0x1B 0x25 0x2F 0x32 128 136 0x47 0x42 0x4B 0x2D 0x30
;;       ;;            2 128 (136+2*N,if there are N GBK chars)
;;       ;;            0x67 0x62 0x6B 0x2D 0x30 2]
;;       (if (r0 == ?\x1B) ;; G0 or A0
;;           ((read r0)
;;            (if (r0 == ?\x25) ;; G1
;;                ((read r0)
;;                 (if (r0 == ?\x2F) ;; G2
;;                     ((read r0)
;;                      (if (r0 == ?\x32) ;; G3
;;                          ((read r0)
;;                           (if (r0 == 128) ;; G4
;;                               ((read r0)
;;                                (if (r0 == 136) ;; G5
;;                                    ((read r0)
;;                                     (if (r0 == ?\x47) ;; G6  'G'
;;                                         ((read r0)
;;                                          (if (r0 == ?\x42) ;; G7  'B'
;;                                              ((read r0)
;;                                               (if (r0 == ?\x4B) ;; G8  'K'
;;                                                   ((read r0)
;;                                                    (if (r0 == ?\x2D) ;; G9  '-'
;;                                                        ((read r0)
;;                                                         (if (r0 == ?\x30) ;; G10  '0'
;;                                                             ((read r0)
;;                                                              (if (r0 == 2) ;; G11
;;                                                                  ((read r0)
;;                                                                   (if (r0 == 128) ;; G12
;;                                                                       ((read r0)
;;                                                                        (if (r0 == r0) ;; G13
;;                                                                            ((read r0)
;;                                                                             (if (r0 == ?\x67) ;; G14  'g'
;;                                                                                 ((read r0)
;;                                                                                  (if (r0 == ?\x62) ;; G15  'b'
;;                                                                                      ((read r0)
;;                                                                                       (if (r0 == ?\x6B) ;; G16  'k'
;;                                                                                           ((read r0)
;;                                                                                            (if (r0 == ?\x2D) ;; G17  '-'
;;                                                                                                ((read r0)
;;                                                                                                 (if (r0 == ?\x30) ;; G18  '0'
;;                                                                                                     ((read r0)
;;                                                                                                      (if (r0 == 2) ;; G19
;; ;;; -------------------------------------------------------------------
;;                                                                                                          (
;;                                                                                                           ;; (read r0)
;;                                                                                                           (repeat)
;;                                                                                                           )
;; ;;; -------------------------------------------------------------------
;;                                                                                                        ((write r0) ;; not G19
;;                                                                                                         (r4 = r0)
;;                                                                                                         (r3 = 1)
;;                                                                                                         (repeat))))
;;                                                                                                   ((write r0) ;; not G18
;;                                                                                                    (r4 = r0)
;;                                                                                                    (r3 = 1)
;;                                                                                                    (repeat))))
;;                                                                                              ((write r0) ;; not G17
;;                                                                                               (r4 = r0)
;;                                                                                               (r3 = 1)
;;                                                                                               (repeat))))
;;                                                                                         ((write r0) ;; not G16
;;                                                                                          (r4 = r0)
;;                                                                                          (r3 = 1)
;;                                                                                          (repeat))))
;;                                                                                    ((write r0) ;; not G15
;;                                                                                     (r4 = r0)
;;                                                                                     (r3 = 1)
;;                                                                                     (repeat))))
;;                                                                               ((write r0) ;; not G14
;;                                                                                (r4 = r0)
;;                                                                                (r3 = 1)
;;                                                                                (repeat))))
;;                                                                          ((write r0) ;; not G13
;;                                                                           (r4 = r0)
;;                                                                           (r3 = 1)
;;                                                                           (repeat))))
;;                                                                     ((write "G0-11") ;; not G12
;;                                                                      (r4 = r0)
;;                                                                      (r3 = 1)
;;                                                                      (repeat))))
;;                                                                ((write "G0-10") ;; not G11
;;                                                                 (r4 = r0)
;;                                                                 (r3 = 1)
;;                                                                 (repeat))))
;;                                                           ((write "G0-9") ;; not G10
;;                                                            (r4 = r0)
;;                                                            (r3 = 1)
;;                                                            (repeat))))
;;                                                      ((write "G0-8") ;; not G9
;;                                                       (r4 = r0)
;;                                                       (r3 = 1)
;;                                                       (repeat))))
;;                                                 ((write "G0-7") ;; not G8
;;                                                  (r4 = r0)
;;                                                  (r3 = 1)
;;                                                  (repeat))))
;;                                            ((write "G0-6") ;; not G7
;;                                             (r4 = r0)
;;                                             (r3 = 1)
;;                                             (repeat))))
;;                                       ((write "G0-5") ;; not G6
;;                                        (r4 = r0)
;;                                        (r3 = 1)
;;                                        (repeat))))
;;                                  ((write "G0-4") ;; not G5
;;                                   (r4 = r0)
;;                                   (r3 = 1)
;;                                   (repeat))))
;;                             ((write "G0-3") ;; not G4
;;                              (r4 = r0)
;;                              (r3 = 1)
;;                              (repeat))))
;;                        ((write "G0-2") ;; not G3
;;                         (r4 = r0)
;;                         (r3 = 1)
;;                         (repeat))))
;;                   ((write "G0-1") ;; not G2
;;                    (r4 = r0)
;;                    (r3 = 1)
;;                    (repeat))))
;;              ;; not G1
;;              (if (r0 == ?\x28) ;; A1
;;                  ((read r0)
;;                   (if (r0 == ?\x42) ;; A2
;;                       (repeat)
;;                     ((write "A0-1") ;; not A2
;;                      (r4 = r0)
;;                      (r3 = 1)
;;                      (repeat))))
;;                ((write "A0-0") ;; not A1
;;                 (r4 = r0)
;;                 (r3 = 1)
;;                 (repeat)))))
;;         ((write "") ;; not G0 and not A0
;;          ))

     
;;       ;; 1 byte encoding, i.e., ascii
;;       (if (r0 < ?\x80)
;;           (write r0)

;;         ;; 2 byte encoding
;;         (if (r0 > ?\x80)
;;             (if (r0 < ?\xFF)
;;                 ((read r1)
                

;;                  (if (r1 > ?\x3F)
;;                      (if (r1 < ?\x7F)
;;                          (r1 -= ?\x40)
;;                        (if (r1 > ?\x7F)
;;                            (if (r1 < ?\xFF)
;;                                (r1 -= ?\x41)
;;                              ((write r0 r1)
;;                               (repeat)))
;;                          ((write r0 r1)
;;                           (repeat))))
;;                    ((write r0 r1)
;;                     (repeat)))
                       
                                
;;                  (if (r0 < ?\xAB)
;;                      ((r2 = (r0 - ?\x81)) ;; part 1
;;                       (r0 = ,(charset-id 'chinese-cns11643-5)))
;;                    (if (r0 < ?\xD5)
;;                        ((r2 = (r0 - ?\xAB)) ;; part 2
;;                         (r0 = ,(charset-id 'chinese-cns11643-6)))
;;                      ((r2 = (r0 - ?\xD5)) ;; part 3
;;                       (r0 = ,(charset-id 'chinese-cns11643-7)))))

;;                  (r2 *= ?\xBE) ;; 0xBE == 0xFF - 0x80 + 0x7F - 0x40
;;                  (r1 += r2)
;;                  (r2 = (r1 / 94))
;;                  (r2 <<= 7)
;;                  (r1 %= 94)
;;                  (r1 |= r2)
;;                  (r1 += ,(+ (* 33 128) 33))
;;                  (write-multibyte-character r0 r1))
;;               ((write r0 r1)
;;                (repeat)))
;;           ((write r0 r1)
;;            (repeat))))
;;       (repeat))))

;;   "CCL program to decode GBK.")


;; r2: byte counter for Chinese characters
;; r3: stat flag:  0->ascii  1->Chinese characters  2->unstarted
(define-ccl-program ccl-encode-chinese-gbk-compound-text-reverse
  `(1
    ((r2 = 0) ;; clear byte counter
     (r3 = 2) ;; set stat flag to "unstarted"
     (loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (if (r3 == 0) ;; if ascii
              (write r1)
            (if (r3 == 2) ;; if unstarted
                ((write r1)
                 (r3 = 0))  ;; set stat flag to ascii
              ((write "\x02\x30\x2D\x6B\x62\x67") ;; reversed "gbk-0"
               (r4 = ((r2 & 127) + 128)) ;; 127 == (0111 1111)b
               (write r4)
               (r4 = ((r2 / 128) + 128))
               (write r4)
               (write "\x32\x2F\x25\x1B") ;; reversed "%/2"               
               (write r1)
               (r3 = 0))))

        (if (r0 == ,(charset-id 'chinese-cns11643-5))
            ((r0 = ((((r1 & 16256) >> 7) - 33) * 94))
             ;; 16256 == (0011 1111 1000 0000)b
             (r1 &= ?\x7F)
             (r1 += (r0 - 33))
             (r0 = (r1 / ?\xBE))        ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
             (r1 %= ?\xBE)              ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
             (r0 += ?\x81) ;; part 1
             (if (r1 < ?\x3F)
                 (r1 += ?\x40)
               (r1 += ?\x41))
             (if (r3 == 1) ;; check stat flag
                 ((r2 += 2) ;; increment byte counter
                  (write r1 r0))
               ((if (r3 == 0) ;; ascii
                    (write "B(\e")) ;; reversed "\e(B"
                (write r1 r0)
                ;; 6 == length("gbk-0")
                (r2 = 8) ;; byte counter start with 8
                (r3 = 1))))
         
          (if (r0 == ,(charset-id 'chinese-cns11643-6))
              ((r0 = ((((r1 & 16256) >> 7) - 33) * 94))
               ;; 16256 == (0011 1111 1000 0000)b
               (r1 &= ?\x7F)
               (r1 += (r0 - 33))
               (r0 = (r1 / ?\xBE))      ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
               (r1 %= ?\xBE)            ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
               (r0 += ?\xAB) ;; part 2
               (if (r1 < ?\x3F)
                   (r1 += ?\x40)
                 (r1 += ?\x41))
               (if (r3 == 1) ;; check stat flag
                   ((r2 += 2) ;; increment byte counter
                    (write r1 r0))
                 ((if (r3 == 0) ;; ascii
                      (write "B(\e")) ;; reversed "\e(B"
                  (write r1 r0)
                  ;; 6 == length("gbk-0")
                  (r2 = 8) ;; byte counter start with 8
                  (r3 = 1))))
           
            (if (r0 == ,(charset-id 'chinese-cns11643-7))
                ((r0 = ((((r1 & 16256) >> 7) - 33) * 94))
                 ;; 16256 == (0011 1111 1000 0000)b
                 (r1 &= ?\x7F)
                 (r1 += (r0 - 33))
                 (r0 = (r1 / ?\xBE))    ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
                 (r1 %= ?\xBE)          ;0xBE == 0xFF - 0x80 + 0x7F - 0x40
                 (r0 += ?\xD5) ;; part 3
                 (if (r1 < ?\x3F)
                     (r1 += ?\x40)
                   (r1 += ?\x41))
                 (if (r3 == 1) ;; check stat flag
                     ((r2 += 2) ;; increment byte counter
                      (write r1 r0))
                   ((if (r3 == 0) ;; ascii
                        (write "B(\e")) ;; reversed "\e(B"
                    (write r1 r0)
                    ;; 6 == length("gbk-0")
                    (r2 = 8) ;; byte counter start with 8
                    (r3 = 1))))
              ;; GB2312 will be convert to GBK
              (if (r0 == ,(charset-id 'chinese-gb2312))
                  ;; 16256 == (0011 1111 1000 0000)b
                  ((r0 = ((r1 & 16256) >> 7))
                   (r0 += 128)
                   ;; 127 == (0111 1111)b
                   (r1 = (r1 & 127))
                   (r1 += 128)
                   (if (r3 == 1) ;; check stat flag
                       ((r2 += 2) ;; increment byte counter
                        (write r1 r0))
                     ((if (r3 == 0) ;; ascii
                          (write "B(\e")) ;; reversed "\e(B"
                      (write r1 r0)
                      ;; 6 == length("gbk-0")
                      (r2 = 8) ;; byte counter start with 8
                      (r3 = 1)))))
              ))))
      (repeat)))
    (if (r3 == 1) ;; if Chinese character
        ((write "\x02\x30\x2D\x6B\x62\x67") ;; reversed "gbk-0"
         (r4 = ((r2 & 127) + 128)) ;; 127 == (0111 1111)b
         (write r4)
         (r4 = ((r2 / 128) + 128))
         (write r4)
         (write "\x32\x2F\x25\x1B") ;; reversed "%/2"               
         )
      ((r3 = r3)))
    )

  "CCL program to encode into GBK(Compound Text).")

;; Do not use this coding system to decode.
;; It should only be used to decoding Chinese GBK text into
;; compound text in reversed order!
(make-coding-system
 'chinese-gbk-compound-text-reverse 4 ?Z
 "Chinese GBK encoding for GBK characters mainly used
on Chinese PCs."

 '(ccl-decode-chinese-gbk . ccl-encode-chinese-gbk-compound-text-reverse)
 '((safe-charsets
    ascii
    chinese-cns11643-5
    chinese-cns11643-6
    chinese-cns11643-7
    chinese-gb2312)
   (valid-codes (0 . 254))))


;; (define-ccl-program ccl-decode-chinese-gbk-show-byte
;;   `(30
;;     ((loop
;;       (read r0)
;;       (r1 = r0)
;;       (r1 /= 16)
;;       (r3 = r1)
;;       (r3 *= 16)
;;       (r2 = r0)
;;       (r2 -= r3)
;;       (r3 = r1)
;;       (r4 = r2)
;;       (if (r1 < 10)
;;            (r3 += ?\x30) ;; '0' ...
;;         (r3 += ?\x37)) ;; 'A' ...
;;       (if (r2 < 10)
;;            (r4 += ?\x30) ;; '0' ...
;;         (r4 += ?\x37)) ;; 'A' ...
;;       (write "(write ?\\x")
;;       (write r3 r4)
;;       (write ")\n")

;;       (repeat))))

;;   "CCL program to decode GBK.")


;; (make-coding-system
;;  'chinese-gbk-show-byte 4 ?Z
;;  "Chinese GBK encoding for GBK characters mainly used
;;  on Chinese PCs."

;;  '(ccl-decode-chinese-gbk-show-byte . ccl-encode-chinese-gbk)
;;  '((safe-charsets
;;     ascii
;;     chinese-cns11643-5
;;     chinese-cns11643-6
;;     chinese-cns11643-7)
;;    (valid-codes (0 . 254))))



(define-coding-system-alias 'gbk 'chinese-gbk)
(define-coding-system-alias 'cn-gbk 'chinese-gbk)
(define-coding-system-alias 'chinese-gbk-unix 'chinese-gbk)

(set-language-info-alist
 "Chinese-GBK" '((coding-system chinese-gbk chinese-iso-7bit)
                 (coding-priority chinese-gbk iso-2022-cn chinese-iso-8bit)
                 (input-method . "chinese-py")
                 (sample-text . "Chinese GBK ÄãºÃ Š…ºÃ")
                 (documentation . "Support for Chinese GBK character set."))
 '("Chinese"))


(update-coding-systems-internal)



(provide 'chinese-gbk)
;;; chinese-gbk.el ends here
