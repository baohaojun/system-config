;;; skk-viper.el --- SKK related code for Viper -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;;   Murata Shuuichirou <mrt@notwork.org>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@notwork.org>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method

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

;;; Code:

(require 'skk)

(eval-when-compile
  (if (boundp 'viper-mode)
      (when (eq viper-mode 'ask)
        (setq viper-mode nil))
    (defvar viper-mode nil)))
(require 'viper)

(eval-when-compile
  (defvar viper-insert-state-cursor-color))

;;; macros and inline functions.
(defmacro skk-viper-advice-select (viper vip arg body)
  `(if skk-viper-use-vip-prefix
       (defadvice ,vip ,arg ,@body)
     (defadvice ,viper ,arg ,@body)))

(setq skk-kana-cleanup-command-list
      (cons
       (if skk-viper-use-vip-prefix
           'vip-delete-backward-char
         'viper-del-backward-char-in-insert)
       skk-kana-cleanup-command-list))

(setq skk-use-viper t)
(save-match-data
  (unless (string-match (sentence-end) "$B!#!)!*!%(B")
    (setq sentence-end (concat "[$B!#!)!*!%(B]\\|" sentence-end))))

;;; cursor color support.
;; what should we do if older Viper that doesn't have
;; `viper-insert-state-cursor-color'?
(when (boundp 'viper-insert-state-cursor-color)
  (defadvice skk-cursor-current-color (around skk-viper-cursor-ad activate)
    "vi-state $B0J30$G3n$D(B SKK $B%b!<%I$N$H$-$N$_(B SKK $BM3Mh$N%+!<%=%k?'$rJV$9!#(B"
    (cond
     ((not skk-use-color-cursor)
      ad-do-it)
     ((or (and (boundp 'viper-current-state)
               (eq viper-current-state 'vi-state))
          (and (boundp 'vip-current-mode)
               (eq vip-current-mode 'vi-mode)))
      (setq ad-return-value skk-cursor-default-color))
     ((not skk-mode)
      (setq viper-insert-state-cursor-color
            skk-viper-saved-cursor-color)
      (setq ad-return-value
            (cond
             ((eq viper-current-state 'insert-state)
              viper-insert-state-cursor-color)
             ((eq viper-current-state 'replace-state)
              viper-replace-overlay-cursor-color)
             ((eq viper-current-state 'emacs-state)
              viper-emacs-state-cursor-color))))
     (t
      ad-do-it
      (setq viper-insert-state-cursor-color ad-return-value))))

  (let ((funcs
         ;; cover to VIP/Viper functions.
         (if skk-viper-use-vip-prefix
             '(vip-Append
               vip-Insert
               vip-insert
               vip-escape-to-emacs
               vip-open-line)
           '(viper-change-state-to-insert
             viper-change-state-to-replace
             viper-change-state-to-vi
             viper-change-state-to-emacs
             viper-insert-state-post-command-sentinel))))
    (dolist (func funcs)
      (eval
       `(defadvice ,(intern (symbol-name func))
            (after skk-viper-cursor-ad activate)
          "Set cursor color which represents skk mode."
          (when skk-use-color-cursor
            (skk-cursor-set))))))

  (let ((funcs '(skk-abbrev-mode
                 skk-jisx0208-latin-mode
                 skk-latin-mode
                 skk-toggle-characters)))
    (dolist (func funcs)
      (eval
       `(defadvice ,(intern (symbol-name func))
            (after skk-viper-cursor-ad activate)
          "\
`viper-insert-state-cursor-color' $B$r(B SKK $B$NF~NO%b!<%I$N%+!<%=%k?'$H9g$o$;$k!#(B"
          (when skk-use-color-cursor
            (setq viper-insert-state-cursor-color
                  (skk-cursor-current-color)))))))

  (defadvice skk-mode (after skk-viper-cursor-ad activate)
    "Set cursor color which represents skk mode."
    (when skk-use-color-cursor
      (skk-cursor-set)))

  (defadvice skk-kakutei (after skk-viper-cursor-ad activate)
    (setq viper-insert-state-cursor-color skk-cursor-hiragana-color)))

(when (boundp 'viper-insert-state-cursor-color)
  (skk-defadvice read-from-minibuffer (before skk-viper-ad activate)
    "`minibuffer-setup-hook' $B$K(B `update-buffer-local-frame-params' $B$r%U%C%/$9$k!#(B
`viper-read-string-with-history' $B$O(B `minibuffer-setup-hook' $B$r4X?t%m!<%+%k(B
$B$K$7$F$7$^$&$N$G!"M=$a(B `minibuffer-setup-hook' $B$K$+$1$F$*$$$?%U%C%/$,L58z(B
$B$H$J$k!#(B"
    (when skk-use-color-cursor
      ;; non-command subr.
      (add-hook 'minibuffer-setup-hook 'ccc-update-buffer-local-frame-params
                'append))))

;;; advices.
;; vip-4 $B$NF1<o$N4X?tL>$O(B vip-read-string-with-history$B!)(B
(defadvice viper-read-string-with-history (after skk-viper-ad activate)
  "$B<!2s%_%K%P%C%U%!$KF~$C$?$H$-$K(B SKK $B%b!<%I$K$J$i$J$$$h$&$K$9$k!#(B"
  (skk-remove-skk-pre-command)
  (skk-remove-minibuffer-setup-hook 'skk-j-mode-on
                                    'skk-setup-minibuffer
                                    'skk-add-skk-pre-command))

(skk-viper-advice-select
 viper-forward-word-kernel vip-forward-word
 (around skk-ad activate)
 ("SKK $B%b!<%I$,%*%s$G!"%]%$%s%H$ND>8e$NJ8;z$,(B JISX0208/JISX0213 $B$@$C$?$i(B\
 `forward-word' $B$9$k!#(B"
  (if (and skk-mode
           (or (skk-jisx0208-p (following-char))
               (skk-jisx0213-p (following-char))))
      (forward-word (ad-get-arg 0))
    ad-do-it)))

(skk-viper-advice-select
 viper-backward-word-kernel vip-backward-word
 (around skk-ad activate)
 ("SKK $B%b!<%I$,%*%s$G!"%]%$%s%H$ND>A0$NJ8;z$,(B JISX0208/JISX0213 $B$@$C$?$i(B\
 `backward-word' $B$9$k!#(B"
  (if (and skk-mode (or (skk-jisx0208-p (preceding-char))
                        (skk-jisx0213-p (preceding-char))))
      (backward-word (ad-get-arg 0))
    ad-do-it)))

;; please sync with `skk-delete-backward-char'
(skk-viper-advice-select
 viper-del-backward-char-in-insert vip-delete-backward-char
 (around skk-ad activate)
 ("$B"'%b!<%I$G(B `skk-delete-implies-kakutei' $B$J$iD>A0$NJ8;z$r>C$7$F3NDj$9$k!#(B
$B"'%b!<%I$G(B `skk-delete-implies-kakutei' $B$,(B nil $B$@$C$?$iA08uJd$rI=<($9$k!#(B
$B"&%b!<%I$G(B`$B"&(B'$B$h$j$bA0$N%]%$%s%H$G<B9T$9$k$H3NDj$9$k!#(B
$B3NDjF~NO%b!<%I$G!"$+$J%W%l%U%#%C%/%9$NF~NOCf$J$i$P!"$+$J%W%l%U%#%C%/%9$r>C$9!#(B"
  (skk-with-point-move
   (let ((count (or (prefix-numeric-value (ad-get-arg 0)) 1)))
     (cond
      ((eq skk-henkan-mode 'active)
       (if (and (not skk-delete-implies-kakutei)
                (= (+ skk-henkan-end-point (length skk-henkan-okurigana))
                   (point)))
           (skk-previous-candidate)
         ;; overwrite-mode $B$G!"%]%$%s%H$,A43QJ8;z$K0O$^$l$F$$$k$H(B
         ;; $B$-$K(B delete-backward-char $B$r;H$&$H!"A43QJ8;z$O>C$9$,H>(B
         ;; $B3QJ8;zJ,$7$+(B backward $BJ}8~$K%]%$%s%H$,La$i$J$$(B (Emacs
         ;; 19.31 $B$K$F3NG'(B)$B!#JQ49Cf$N8uJd$KBP$7$F$O(B
         ;; delete-backward-char $B$GI,$:A43QJ8;z(B 1 $BJ8;zJ,(B backward
         ;; $BJ}8~$KLa$C$?J}$,NI$$!#(B
         (if overwrite-mode
             (progn
               (backward-char count)
               (delete-char count))
           ad-do-it)
         ;; XXX assume skk-prefix has no multibyte chars.
         (if (> (length skk-prefix) count)
             (setq skk-prefix (substring skk-prefix
                                         0 (- (length skk-prefix) count)))
           (setq skk-prefix ""))
         (when (>= skk-henkan-end-point (point))
           (if (eq skk-delete-implies-kakutei 'dont-update)
               (let ((skk-update-jisyo-function #'ignore))
                 (skk-kakutei))
             (skk-kakutei)))))
      ((and skk-henkan-mode
            (>= skk-henkan-start-point (point))
            (not (skk-get-prefix skk-current-rule-tree)))
       (setq skk-henkan-count 0)
       (skk-kakutei))
      ;; $BF~NOCf$N8+=P$78l$KBP$7$F$O(B delete-backward-char $B$G(B
      ;; $BI,$:A43QJ8;z(B 1$BJ8;zJ,(B backward $BJ}8~$KLa$C$?J}$,NI$$!#(B
      ((and skk-henkan-mode
            overwrite-mode)
       (backward-char count)
       (delete-char count))
      (t
       (skk-delete-okuri-mark)
       (if (skk-get-prefix skk-current-rule-tree)
           (skk-erase-prefix 'clean)
         (skk-set-marker skk-kana-start-point nil)
         ad-do-it)))))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-escape-to-emacs
 (before skk-add activate)
 ("$B"&%b!<%I!""'%b!<%I$@$C$?$i3NDj$9$k!#(B"
  (when (and skk-mode
             skk-henkan-mode)
    (skk-kakutei))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-escape-to-emacs
 (after skk-kana-cleanup-ad activate)
 ("vi-state $B0\9T$N:]$K3NDjF~NO%b!<%I$GF~NO$5$l$?%m!<%^;z%W%l%U%#%C%/%9$r>C$9!#(B"
  (skk-kana-cleanup t)))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("$B%9%Z!<%9$NN>B&$NJ8;z%;%C%H$,(B JISX0208/JISX0213 $B$@$C$?$i%9%Z!<%9$r<h$j=|$/!#(B"
  (save-match-data
    (let ((char-after (char-after (progn
                                    (skip-chars-forward " ")
                                    (point))))
          (char-before (char-before (progn
                                      (skip-chars-backward " ")
                                      (point)))))
      (when (and (or (skk-jisx0208-p char-after)
                     (skk-jisx0213-p char-after))
                 (or (skk-jisx0208-p char-before)
                     (skk-jisx0213-p char-before)))
        (while (looking-at " ")
          (delete-char 1)))))))

;;; functions.
;;;###autoload
(defun skk-viper-normalize-map ()
  (let ((other-buffer (local-variable-if-set-p 'minor-mode-map-alist)))
    ;; for current buffer and buffers to be created in the future.
    ;; substantially the same job as viper-harness-minor-mode does.
    (funcall skk-viper-normalize-map-function)
    (setq-default minor-mode-map-alist minor-mode-map-alist)
    (when other-buffer
      ;; for buffers which are already created and have
      ;; the minor-mode-map-alist localized by Viper.
      (skk-loop-for-buffers (buffer-list)
        (unless (assq 'skk-j-mode minor-mode-map-alist)
          (skk-update-minor-mode-map-alist 'skk-latin-mode skk-latin-mode-map)
          (skk-update-minor-mode-map-alist 'skk-abbrev-mode skk-abbrev-mode-map)
          (skk-update-minor-mode-map-alist 'skk-j-mode skk-j-mode-map)
          (skk-update-minor-mode-map-alist 'skk-jisx0208-mode skk-jisx0208-latin-mode-map))
        (funcall skk-viper-normalize-map-function)))))

(eval-after-load "viper-cmd"
  '(defun viper-toggle-case (arg)
     "Toggle character case.
Convert hirakana to katakana and vice versa."
     (interactive "P")
     (let ((val (viper-p-val arg)) (c))
       (viper-set-destructive-command
        (list 'viper-toggle-case val nil nil nil nil))
       (while (> val 0)
         (setq c (following-char))
         (delete-char 1 nil)
         (cond ((skk-ascii-char-p c)
                (if (eq c (upcase c))
                    (insert-char (downcase c) 1)
                  (insert-char (upcase c) 1)))
               ((and (<= ?$B$!(B c) (>= ?$B$s(B c))
                (insert (skk-hiragana-to-katakana (char-to-string c))))
               ((and (<= ?$B%!(B c) (>= ?$B%s(B c))
                (insert (skk-katakana-to-hiragana (char-to-string c))))
               (t
                (insert-char c 1)))
         (when (eolp)
           (backward-char 1))
         (setq val (1- val))))))

(defun skk-viper-init-function ()
  (when (and (boundp 'viper-insert-state-cursor-color)
             (featurep 'skk-cursor))
    (setq viper-insert-state-cursor-color (skk-cursor-current-color)))
  ;; viper-toggle-key-action $B$HO"F0$5$;$k!)(B
  (skk-viper-normalize-map)
  (remove-hook 'skk-mode-hook 'skk-viper-init-function))

(add-hook 'skk-mode-hook 'skk-viper-init-function)

(provide 'skk-viper)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-viper.el ends here
