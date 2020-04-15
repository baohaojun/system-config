;;; skk-abbrev.el --- SKK/Emacs abbrev mode interface -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method
;; Created: Oct. 23, 1999

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

;; <how to install>
;;   下記のフォームを ~/.emacs.d/init.el か ~/.skk に書いて下さい。
;;   skk-search-prog-list については、全部を写す必要はありません。
;;   (skk-abbrev-search) を含むように指定して下さい。
;;
;; (setq skk-search-prog-list
;;       '((skk-search-jisyo-file skk-jisyo 0 t)
;;         ;; ADD NEXT LINE.
;;         (skk-abbrev-search)
;;         (skk-search-server skk-aux-large-jisyo 10000)))
;;
;;
;; <how to work>
;;   skk-abbrev-mode で変換したとき、補完したときに Emacs original の
;;   abbrev mode を使って変換します。
;;
;; skk-look との優先順位は、
;;  a.変換については、skk-search-prog-list の中で (skk-abbrev-search) を
;;    先順位に書き指定して下さい。skk-abbrev-search の方が skk-look より軽い
;;    です。
;;  b.補完については、skk-use-look を指定していても、常に skk-abbrev-search
;;    の方が先に検索されます。

;;; Code:

(eval-when-compile
  (require 'skk)
  (require 'skk-comp))

;; ;;;###autoload
;; (defgroup skk-abbrev nil "SKK abbrev related customization."
;;   :prefix "skk-abbrev-"
;;   :group 'skk)

;;;###autoload
(defun skk-abbrev-search ()
  (let ((var (when skk-abbrev-mode
               (abbrev-expansion skk-henkan-key))))
    (when var
      (list var))))

(defadvice skk-completion-original (around skk-abbrev-ad activate)
  (let ((first (ad-get-arg 0))
        c-word)
    (condition-case nil
        ;; not to search by look in ad-do-it.
        (let (skk-use-look)
          ad-do-it)
      ;; no word to be completed.
      (error
       (when skk-abbrev-mode
         (setq c-word (and (abbrev-expansion skk-completion-word)))
         (when (and skk-use-look
                    (or (not c-word)
                        (member c-word skk-completion-stack)))
           ;; more searching by look when abbreviating is not enough.
           (while (or (not c-word)
                      (member c-word skk-completion-stack))
             (setq c-word (skk-look-completion)))))
       (unless c-word
         (if skk-japanese-message-and-error
             (error "\"%s\" で補完すべき見出し語は%sありません"
                    skk-completion-word
                    (if first "" "他に"))
           (error "No %scompletions for \"%s\""
                  (if first "" "more ")
                  skk-completion-word)))
       (setq skk-completion-stack (cons c-word skk-completion-stack))
       (delete-region skk-henkan-start-point (point))
       (insert c-word)))))

(provide 'skk-abbrev)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-abbrev.el ends here
