;;; skk-abbrev.el --- SKK/Emacs abbrev mode interface -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
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
;;   $B2<5-$N%U%)!<%`$r(B ~/.emacs.d/init.el $B$+(B ~/.skk $B$K=q$$$F2<$5$$!#(B
;;   skk-search-prog-list $B$K$D$$$F$O!"A4It$r<L$9I,MW$O$"$j$^$;$s!#(B
;;   (skk-abbrev-search) $B$r4^$`$h$&$K;XDj$7$F2<$5$$!#(B
;;
;; (setq skk-search-prog-list
;;       '((skk-search-jisyo-file skk-jisyo 0 t)
;;         ;; ADD NEXT LINE.
;;         (skk-abbrev-search)
;;         (skk-search-server skk-aux-large-jisyo 10000)))
;;
;;
;; <how to work>
;;   skk-abbrev-mode $B$GJQ49$7$?$H$-!"Jd40$7$?$H$-$K(B Emacs original $B$N(B
;;   abbrev mode $B$r;H$C$FJQ49$7$^$9!#(B
;;
;; skk-look $B$H$NM%@h=g0L$O!"(B
;;  a.$BJQ49$K$D$$$F$O!"(Bskk-search-prog-list $B$NCf$G(B (skk-abbrev-search) $B$r(B
;;    $B@h=g0L$K=q$-;XDj$7$F2<$5$$!#(Bskk-abbrev-search $B$NJ}$,(B skk-look $B$h$j7Z$$(B
;;    $B$G$9!#(B
;;  b.$BJd40$K$D$$$F$O!"(Bskk-use-look $B$r;XDj$7$F$$$F$b!">o$K(B skk-abbrev-search
;;    $B$NJ}$,@h$K8!:w$5$l$^$9!#(B

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
             (error "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
                    skk-completion-word
                    (if first "" "$BB>$K(B"))
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
