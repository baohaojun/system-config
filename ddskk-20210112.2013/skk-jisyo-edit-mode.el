;;; skk-jisyo-edit-mode.el --- major mode for editing SKK dictionaries -*- coding: iso-2022-jp -*-

;; Copyright (C) 2001-2010 SKK Development Team

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

;; This is a major mode for editing SKK dictionaries.

;;; Code:

(require 'skk)
(require 'skk-cus)

(eval-when-compile
  (defvar font-lock-defaults))

(defvar skk-jisyo-edit-map nil
  "Keymap for SKK JISYO Edit mode.")

(defvar skk-jisyo-edit-mode-hook nil
  "Hook run on entry in `skk-jisyo-edit-mode'.")

(defvar skk-jisyo-edit-syntax-table nil)

(unless skk-jisyo-edit-map
  (setq skk-jisyo-edit-map (make-sparse-keymap 'skk-jisyo-edit-map)))

(defvar skk-jisyo-edit-font-lock-keywords
  '(("\\(\\[[^]]*/\\]\\)" 1 font-lock-constant-face)
    ("^\\([^; ]+ \\)/" 1 font-lock-function-name-face)
    ("/[^;\n]+\\(;[^/\n]*\\)" 1 font-lock-type-face t)
    ("^\\(;.+\\)$" 1 font-lock-comment-face t)
    ("^\\(;; okuri-ari entries\\.\\)$" 1 font-lock-keyword-face t)
    ("^\\(;; okuri-nasi entries\\.\\)$" 1 font-lock-keyword-face t)
    ("/\\([^/\n]+\\)$" 1 highlight)
    ("\\(/\\)" 1 font-lock-warning-face))
  "Additional expressions to highlight in SKK JISYO edit mode.")

(put 'skk-jisyo-edit-mode
     'font-lock-defaults
     '(skk-jisyo-edit-font-lock-keywords))

(defvar skk-jisyo-edit-original-window-configuration nil)

;;;###autoload
(defun skk-jisyo-edit-mode ()
  "Major mode for editing SKK JISYO."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "SKK JISYO Edit")
  (setq major-mode #'skk-jisyo-edit-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(skk-jisyo-edit-font-lock-keywords))
  (make-local-variable 'skk-jisyo-edit-syntax-table)
  (setq skk-jisyo-edit-syntax-table (make-syntax-table))
  (set-syntax-table skk-jisyo-edit-syntax-table)
  (let ((map (make-sparse-keymap)))
    (use-local-map (nconc map skk-jisyo-edit-map)))
  (modify-syntax-entry ?\" "w" skk-jisyo-edit-syntax-table)
  (modify-syntax-entry ?/ "w" skk-jisyo-edit-syntax-table)
  (run-hooks 'skk-jisyo-edit-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("SKK-JISYO" . skk-jisyo-edit-mode) t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.skk-jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$"
                                . skk-jisyo-edit-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\..*skk/jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$"
                                . skk-jisyo-edit-mode))

;;;###autoload
(defun skk-edit-private-jisyo (&optional coding-system)
  "$B8D?M<-=q%U%!%$%k(B `skk-jisyo' $B$rJT=8$9$k!#(B
$BG$0U$G$N8D?M<-=qJ]B8$N$"$H!"(B`skk-jisyo' $B$r3+$-!"(B`skk-jisyo-edit-mode' $B$KF~$k!#(B
$B%m!<%+%k$K(B $B0J2<$N%-!<Dj5A$,DI2C$5$l$k!#(B

key       binding
---       -------
C-c C-c   Save & Exit
C-c C-k   Abort

SKK $B;HMQCf$N>l9g$O(B SKK $B$K$h$k8D?M<-=q%P%C%U%!$N99?7$,6X;_$5$l$k!#(B

$B%*%W%7%g%J%k0z?t(B CODING-SYSTEM $B$K$F8D?M<-=q$N%3!<%I7O$r;XDj2DG=!#(B

$B$3$N5!G=$O=>Mh$N<jF0$G$N8D?M<-=qJT=8$h$jG[N8$5$l$F$$$k$,!"(BSKK $B<-=q$N9=J8$r(B
$B%A%'%C%/$9$k$3$H$O$G$-$:!"<+8J@UG$$G$NJT=8$G$"$k$3$H$OJQ$o$j$J$$!#(B"
  (interactive "P")
  (let (answer)
    (unless skk-jisyo-edit-user-accepts-editing
      (setq answer (skk-yes-or-no-p "\
$B8D?M<-=q$NJT=8$O<-=q$r2u$92DG=@-$,$"$j$^$9!#<+8J@UG$$G$N<B9T$KF10U$7$^$9$+!)(B "
                                    "\
You must edit your private dictionary at your own risk.  Do you accept it? "))
      (when answer
        (skk-cus-set '((skk-jisyo-edit-user-accepts-editing . t))))))
  (when skk-jisyo-edit-user-accepts-editing
    (when coding-system
      (setq coding-system (read-coding-system
                           "$B8D?M<-=q$N%3!<%G%#%s%0%7%9%F%`$r;XDj(B: "
                           (skk-find-coding-system (skk-jisyo t)))))
    (unless coding-system
      (setq coding-system (skk-find-coding-system (skk-jisyo t))))
    ;;
    (when (skk-y-or-n-p "$B8D?M<-=q$rJ]B8$7$^$9$+!)(B "
                        "Save private jisyo? ")
      (skk-save-jisyo))
    (skk-edit-private-jisyo-1 coding-system)))

(defun skk-edit-private-jisyo-1 (coding-system)
  (setq skk-jisyo-edit-original-window-configuration
        (current-window-configuration))
  ;; SKK $B<-=q$NJ8;z%3!<%I$O8mH=Dj$,$"$j$&$k$?$a!"Cm0U$9$k(B
  (let ((coding-system-for-read coding-system))
    (find-file (skk-jisyo)))
  (unless (eq major-mode 'skk-jisyo-edit-mode)
    (skk-jisyo-edit-mode))
  ;; $BJT=8Cf$K:FEY<B9T$7$F$b!"(B
  ;; $B"-(B $B$N$h$&$K$J$k$+$i(B skk-update-jisyo-function $B$OI|85$5$l$k!#(B
  ;; '((lambda nil
  ;;     (setq skk-update-jisyo-function #'ignore))
  ;;   (lambda nil
  ;;     (setq skk-update-jisyo-function #'skk-update-jisyo-original))
  ;;   t)
  (add-hook 'kill-buffer-hook
            `(lambda ()
               (setq skk-update-jisyo-function
                     #',skk-update-jisyo-function)
               (ad-disable-advice 'skk-henkan-in-minibuff 'before 'notify-no-effect)
               (ad-disable-advice 'skk-purge-from-jisyo 'around 'notify-no-effect)
               (ad-activate 'skk-henkan-in-minibuff)
               (ad-activate 'skk-purge-from-jisyo))
            nil t)
  (setq skk-update-jisyo-function #'ignore)
  (ad-enable-advice 'skk-henkan-in-minibuff 'before 'notify-no-effect)
  (ad-enable-advice 'skk-purge-from-jisyo 'around 'notify-no-effect)
  (ad-activate 'skk-henkan-in-minibuff)
  (ad-activate 'skk-purge-from-jisyo)
  (local-set-key "\C-c\C-c"
                 (lambda ()
                   (interactive)
                   (when (skk-y-or-n-p "$BJT=8$r=*N;$7$^$9$+!)(B "
                                       "Finish editing jisyo? ")
                     (save-buffer)
                     (kill-buffer (current-buffer))
                     (skk-reread-private-jisyo t)
                     (set-window-configuration
                      skk-jisyo-edit-original-window-configuration))
                   (message nil)))
  (local-set-key "\C-c\C-k"
                 (lambda ()
                   (interactive)
                   (when (skk-y-or-n-p "$BJT=8$rCf;_$7$^$9$+!)(B "
                                       "Abort editing jisyo? ")
                     (set-buffer-modified-p nil)
                     (kill-buffer (current-buffer))
                     (set-window-configuration
                      skk-jisyo-edit-original-window-configuration))
                   (message nil)))
  (skk-message "$BJ]B8=*N;(B: C-c C-c, $BJT=8Cf;_(B: C-c C-k"
               "Save & Exit: C-c C-c, Abort: C-c C-k"))

(defadvice skk-henkan-in-minibuff (before notify-no-effect disable)
  (ding)
  (skk-message "$B8D?M<-=q$NJT=8Cf$G$9!#EPO?$OH?1G$5$l$^$;$s!#(B"
               "You are editing private jisyo.  This registration has no effect.")
  (sit-for 1.5))

(defadvice skk-purge-from-jisyo (around notify-no-effect disable)
  (if (eq skk-henkan-mode 'active)
      (progn
        (ding)
        (skk-message "$B8D?M<-=q$NJT=8Cf$G$9!#:o=|$G$-$^$;$s!#(B"
                     "You are editing private jisyo.  Can't purge."))
    ad-do-it))

(provide 'skk-jisyo-edit)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-jisyo-edit-mode.el ends here
