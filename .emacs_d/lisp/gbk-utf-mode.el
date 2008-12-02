;;; gbk-utf-mode.el --- Modified utf-translate-mode for Chinese-GBK

;; Copyright (C) 2004 Zhan Jian <zhanjian@ustc.edu>

;; Author: Zhan Jian <zhanjian@ustc.edu>
;; Keywords: multilingual, Chinese, GBK, UTF, Unicode

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

(if (fboundp 'utf-translate-cjk-load-tables)
    (defun utf-translate-cjk-load-tables ()
      "Load tables for `utf-translate-cjk-mode'."
      ;; Fixme: Allow the use of the CJK charsets to be
      ;; customized by reordering and possible omission.
      (let ((redefined (< (hash-table-size ucs-mule-cjk-to-unicode) 43000)))
        (if redefined
            ;; Redefine them with realistic initial sizes and a
            ;; smallish rehash size to avoid wasting significant
            ;; space after they're built.
            (setq ucs-mule-cjk-to-unicode
                  (make-hash-table :test 'eq :size 43000 :rehash-size 1000)
                  ucs-unicode-to-mule-cjk
                  (make-hash-table :test 'eq :size 21500 :rehash-size 1000)))

        ;; Load the files explicitly, to avoid having to keep
        ;; around the large tables they contain (as well as the
        ;; ones which get built).
        (cond ((string= "Korean" current-language-environment)
               (load "subst-jis")
               (load "subst-big5")
               (load "subst-gb2312")
               (load "subst-ksc"))
              ((string= "Japanese" current-language-environment)
               (load "subst-ksc")
               (load "subst-big5")
               (load "subst-gb2312")
               (load "subst-jis"))
              ((string= "Chinese-BIG5" current-language-environment)
               (load "subst-jis")
               (load "subst-ksc")
               (load "subst-gb2312")
               (load "subst-big5"))
              ((string= "Chinese-GB" current-language-environment)
               (load "subst-jis")
               (load "subst-ksc")
               (load "subst-big5")
               (load "subst-gb2312"))
              ((string= "Chinese-GBK" current-language-environment)
               (load "subst2-ksc")
               (load "subst-gbk"))
              (t
               (load "subst2-ksc")
               (load "subst-gbk")))

        (when redefined
          (define-translation-hash-table 'utf-subst-table-for-decode
            ucs-unicode-to-mule-cjk)
          (define-translation-hash-table 'utf-subst-table-for-encode
            ucs-mule-cjk-to-unicode)
          (set-char-table-extra-slot (get 'utf-translation-table-for-encode
                                          'translation-table)
                                     1 ucs-mule-cjk-to-unicode))

        (setq utf-translate-cjk-lang-env current-language-environment)))

  (define-minor-mode utf-translate-cjk-mode
    "Whether the UTF based coding systems should decode/encode CJK characters.
Enabling this loads tables which allow the coding systems mule-utf-8,
mule-utf-16le and mule-utf-16be to encode characters in the charsets
`chinese-gbk', `korean-ksc5601', `chinese-gb2312', `chinese-big5-1',
`chinese-big5-2', `japanese-jisx0208' and `japanese-jisx0212', and to
decode the corresponding unicodes into such characters.

Where the charsets overlap, the one preferred for decoding is chosen
according to the language environment in effect when this option is
turned on: ksc5601 for Korean, gb2312 for Chinese-GB, big5 for
Chinese-Big5 and gbk for other environments.

The tables are large (over 40000 entries), so this option is not the
default.  Also, installing them may be rather slow."
    :init-value nil
    :version "21.4"
    :type 'boolean
    :set-after '(current-language-environment)
    :group 'mule
    :global t
    (if utf-translate-cjk-mode
        ;; Fixme: Allow the use of the CJK charsets to be
        ;; customized by reordering and possible omission.
        (progn
          ;; Redefine them with realistic initial sizes and a
          ;; smallish rehash size to avoid wasting significant
          ;; space after they're built.
          (setq ucs-mule-cjk-to-unicode
                (make-hash-table :test 'eq :size 43000 :rehash-size 1000)
                ucs-unicode-to-mule-cjk
                (make-hash-table :test 'eq :size 21500 :rehash-size 1000))
          ;; Load the files explicitly, to avoid having to keep
          ;; around the large tables they contain (as well as the
          ;; ones which get built).
          (cond
           ((string= "Korean" current-language-environment)
            (load "subst-jis")
            (load "subst-big5")
            (load "subst-gb2312")
            (load "subst-ksc"))
           ((string= "Japanese" current-language-environment)
            (load "subst-ksc")
            (load "subst-big5")
            (load "subst-gb2312")
            (load "subst-jis"))
           ((string= "Chinese-BIG5" current-language-environment)
            (load "subst-jis")
            (load "subst-ksc")
            (load "subst-gb2312")
            (load "subst-big5"))
           ((string= "Chinese-GB" current-language-environment)
            (load "subst-jis")
            (load "subst-ksc")
            (load "subst-big5")
            (load "subst-gb2312"))
           ((string= "Chinese-GBK" current-language-environment)
            (load "subst2-ksc")
            (load "subst-gbk"))
           (t
            (load "subst2-ksc")
            (load "subst-gbk")
            ))
          (define-translation-hash-table 'utf-subst-table-for-decode
            ucs-unicode-to-mule-cjk)
          (define-translation-hash-table 'utf-subst-table-for-encode
            ucs-mule-cjk-to-unicode)
          (set-char-table-extra-slot (get 'utf-translation-table-for-encode
                                          'translation-table)
                                     1 ucs-mule-cjk-to-unicode))
      (define-translation-hash-table 'utf-subst-table-for-decode
        (make-hash-table :test 'eq))
      (define-translation-hash-table 'utf-subst-table-for-encode
        (make-hash-table :test 'eq))
      (set-char-table-extra-slot (get 'utf-translation-table-for-encode
                                      'translation-table)
                                 1 nil))))

(provide 'gbk-utf-mode)
