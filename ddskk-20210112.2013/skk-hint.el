;;; skk-hint.el --- SKK conversion with hints -*- coding: iso-2022-jp -*-
;; Copyright (C) 2001, 2003 Yoshiki Hayashi <yoshiki@xemacs.org>

;; Author: Yoshiki Hayashi <yoshiki@xemacs.org>
;; Keywords: japanese

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

;; $B$3$l$O"&%b!<%I$H"'%b!<%I$GFI$_$N@Q=89g(B ($B$_$?$$$J$b$N(B) $B$r<h$k$3$H$K(B
;; $B$h$C$F8uJd$r9J$j9~$`%W%m%0%i%`$G$9!#(B

;; $B%$%s%9%H!<%k$O(B ~/.skk $B$K0J2<$r5-F~$7$^$9!#(B

;; (require 'skk-hint)

;; $BNc$($P!""&$+$s$I$&(B $B$H$J$C$F$$$k$H$-$K!"(B; michi SPC $B$HF~NO$9$k$H!"(B
;; $B"'4VF;$H$$$&>uBV$K$J$j$^$9!#(B

;; $B87L)$K@Q=89g$r<h$C$F$$$k$o$1$G$O$J$/!""&$+$s$I$&(B $B$G(B ; doubutsu SPC
;; $B$HF~NO$9$k$H"'46F0$H$$$&>uBV$K$J$j$^$9!#(B

;; $B$D$^$j!"DL>o$NJQ498uJd$N$J$+$G!"%R%s%H$H$7$FM?$($i$l$?FI$_$r4^$s$@(B
;; $B4A;z$r;}$D$b$N$K8uJd$r9J$j$^$9!#(B

;; -- Tips --

;; skk-hint.el $B$OC14A;z$N8uJd$,$?$/$5$s$"$k>l9g$K!"$=$3$+$i8uJd$r9J$j(B
;; $B$3$`<jCJ$H$7$FHs>o$KM-8z$G$9!#Nc$($P(B

;;   $B"&$+(B

;; $B$rJQ49$9$k$H!"2c!"2=!"2D!"2<!"F|!"(B...$B$H2L$F$7$J$/8uJd$,=P$F$-$^$9!#(B
;; $B$3$NCf$+$i!V2_!W$r$H$/$K=P$7$?$$$H$7$^$9!#IaDL$KJQ49$7$F$b$=$N$&$A(B
;; $B=P$F$-$^$9$,$3$l$r(B

;;   $B"&$+(B;kahei

;; $B$N$h$&$KF~NO$7$F$+$i(B SPC $B$r2!$7$FJQ49$r3+;O$9$k$H!"!V$+$X$$!W$N8uJd(B
;; $B$G$"$k!V2_J>!W$K4^$^$l$k(B

;;   $B"'2_(B

;; $B$,>e0L$K8=$l$^$9!#(B

;;; Code:

(require 'skk)

(defadvice skk-search (around skk-hint-ad activate)
  ;; skk-current-search-prog-list $B$NMWAG$K$J$C$F$$$k%W%m%0%i%`$rI>2A$7$F!"(B
  ;; skk-henkan-key $B$r%-!<$K$7$F8!:w$r9T$&!#(B
  (if (null skk-hint-henkan-hint)
      ad-do-it
    (let (l kouho hint)
      (while (and (null l) skk-current-search-prog-list)
        (setq l (eval (car skk-current-search-prog-list)))
        (let ((skk-henkan-key (nth 0 skk-hint-henkan-hint))
              (skk-henkan-okurigana (nth 1 skk-hint-henkan-hint))
              (skk-okuri-char (nth 2 skk-hint-henkan-hint)))
          (setq hint (skk-nunion hint (eval (car skk-current-search-prog-list)))))
        (setq kouho (skk-nunion kouho l))
        (setq l (skk-hint-limit kouho hint))
        (setq skk-current-search-prog-list (cdr skk-current-search-prog-list)))
      (setq ad-return-value l))))

(defun skk-hint-setup-hint ()
  (cond ((eq skk-hint-state 'kana)
         (skk-kana-cleanup t)
         (let ((hint (buffer-substring-no-properties
                      skk-hint-start-point (point))))
           (unless (string= hint "")
             (setq skk-hint-henkan-hint
                   (list (if skk-katakana
                             (skk-katakana-to-hiragana hint)
                           hint))))))
        ((eq skk-hint-state 'okuri)
         (let ((henkan-key (buffer-substring-no-properties
                            skk-hint-start-point skk-hint-end-point))
               (okurigana (buffer-substring-no-properties
                           skk-hint-end-point (point))))
           (unless (or (string= henkan-key "")
                       (string= okurigana ""))
             (when skk-katakana
               (setq henkan-key (skk-katakana-to-hiragana henkan-key)
                     okurigana (skk-katakana-to-hiragana okurigana)))
             (setq skk-hint-henkan-hint
                   (list (concat henkan-key skk-hint-okuri-char)
                         okurigana skk-hint-okuri-char)))))
        (t
         (skk-error "$BM=4|$7$J$$>uBV$G(B %s $B$,8F$P$l$^$7$?(B"
                    "%s is called from unexpected place"
                    "skk-hint-setup-hint")))
  (setq skk-hint-inhibit-kakutei nil))

(defadvice skk-insert (around skk-hint-ad activate)
  (cond ((and skk-henkan-mode
              (eq last-command-event skk-hint-start-char)
              (not skk-hint-state))
         (skk-with-point-move
          (when (featurep 'skk-dcomp)
            (skk-dcomp-before-kakutei))
          (setq skk-hint-inhibit-dcomp t)
          (skk-set-marker skk-hint-start-point (point))
          (setq skk-hint-state 'kana
                skk-hint-inhibit-kakutei t)))
        ((and (eq skk-hint-state 'kana)
              (eq last-command-event skk-start-henkan-char))
         (skk-with-point-move
          (skk-hint-setup-hint)
          (delete-region skk-hint-start-point (point))
          (setq skk-hint-state 'henkan)
          (setq skk-henkan-count -1)
          (setq skk-henkan-list nil)
          (skk-start-henkan arg)))
        ((and (eq skk-hint-state 'kana)
              (memq last-command-event skk-set-henkan-point-key))
         (skk-with-point-move
          (setq skk-hint-end-point (point))
          (setq skk-hint-state 'okuri)
          (set 'last-command-event (skk-downcase last-command-event))
          (setq skk-hint-okuri-char (skk-char-to-unibyte-string
                                     last-command-event))
          (skk-kana-input arg)
          (when (skk-jisx0208-p (char-before))
            (skk-hint-setup-hint)
            (delete-region skk-hint-start-point (point))
            (setq skk-hint-state 'henkan)
            (setq skk-henkan-count -1)
            (setq skk-henkan-list nil)
            (skk-start-henkan arg))))
        ((eq skk-hint-state 'okuri)
         (skk-with-point-move
          (skk-kana-input arg)
          (skk-hint-setup-hint)
          (delete-region skk-hint-start-point (point))
          (setq skk-hint-state 'henkan)
          (setq skk-henkan-count -1)
          (setq skk-henkan-list nil)
          (skk-start-henkan arg)))
        (t ad-do-it)))

(defadvice keyboard-quit (before skk-hint-ad activate)
  (setq skk-hint-inhibit-kakutei nil))

(defadvice abort-recursive-edit (before skk-hint-ad activate)
  (setq skk-hint-inhibit-kakutei nil))

(defadvice skk-previous-candidate (before skk-hint-ad activate)
  (when (and (eq skk-henkan-mode 'active)
             (not (string= skk-henkan-key ""))
             (zerop skk-henkan-count))
    (setq skk-hint-henkan-hint nil
          skk-hint-state nil))
  (setq skk-hint-inhibit-kakutei nil))

(defadvice skk-kakutei (around skk-hint-ad activate)
  (unless skk-hint-inhibit-kakutei
    ad-do-it))

(defadvice skk-kakutei-initialize (after skk-hint-ad activate)
  (setq skk-hint-henkan-hint nil
        skk-hint-start-point nil
        skk-hint-state nil
        skk-hint-inhibit-dcomp nil
        skk-hint-inhibit-kakutei nil))

(defadvice skk-delete-backward-char (before skk-hint-ad activate)
  (when (and (markerp skk-hint-start-point)
             (or (eq (1+ skk-hint-start-point) (point))
                 (eq skk-hint-start-point (point))))
    (setq skk-hint-state nil
          skk-hint-inhibit-kakutei nil)))

(defun skk-hint-member (char kouho)
  ;; $BJ8;zNs$N%j%9%H(B KOUHO $B$NCf$KJ8;z(B CHAR $B$r4^$`$b$N$,$"$l$P!"$=$NJ8;zNs$rJV$9(B
  (catch 'found
    (dolist (word kouho)
      (let ((length (length word)))
        (dotimes (i length)
          (if (eq char (aref word i))
              (throw 'found word)))))))

(defun skk-hint-limit (kouho hint)
  ;; $BJQ498uJd(B KOUHO $B$r!"J8;zNs$N%j%9%H(B HINT $B$NCf$N$I$l$+$NJ8;z$,(B
  ;; $B4^$^$l$F$$$k$b$N$N$_$K@)8B$9$k!#(B
  (let ((kouho (copy-sequence kouho))
        result)
    (dolist (string hint)
      (let ((length (length string)))
        (dotimes (i length)
          (let (ret)
            (when (setq ret (skk-hint-member (aref string i) kouho))
              (unless (eq (aref string i) ?\;)
                (setq result (cons ret result))
                (delete ret kouho)))))))
    (nreverse result)))

(provide 'skk-hint)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-hint.el ends here
