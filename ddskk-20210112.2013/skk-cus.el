;;; skk-cus.el --- SKK $B$N4JC1$+$9$?$^$$$:;n:nIJ(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 2001 SKK Development Team

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

;; SKK $B5/F08e(B M-x skk-customize $B$G@_Dj$9$k!#(B

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'skk)
(require 'cus-edit)
(require 'custom)

(defvar skk-custom-params nil)
(defvar skk-custom-alist nil)
(defvar skk-custom-buffer-original nil)
(defvar skk-custom-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map [mouse-1] 'widget-move-and-invoke)
    map))

(defconst skk-cus-params-visual
  '((skk-use-face
     (const :tag "$BJQ49Cf$K?'$r$D$1$k(B" t) "")
    (skk-use-color-cursor
     (const :tag "$B%+!<%=%k$K?'$r$D$1$k(B" t) "")
    (skk-japanese-message-and-error
     (const :tag "$B%a%C%;!<%8$OF|K\8l$GDLCN$9$k(B" t) "")
    (skk-verbose
     (const :tag "$B>iD9$J%a%C%;!<%8$rI=<($9$k(B" t) "")
    (skk-show-japanese-menu
     (const :tag "$B%a%K%e!<%P!<$rF|K\8l$GI=<($9$k(B" t) "")
    (skk-show-annotation
     (radio :tag "$BJQ49;~$KCp<a$rI=<($9$k!)(B"
            (const :tag "$B>o$KI=<((B" t)
            (const :tag "$B8uJd0lMw$G$OHsI=<((B" (not list))
            (const :tag "$B%_%K%P%C%U%!$G$OHsI=<((B" (not minibuf))
            (const :tag "$B8uJd0lMw$H%_%K%P%C%U%!$G$OHsI=<((B"
                   (not list minibuf))
            (const :tag "$BHsI=<((B" nil))
     "")
    (skk-show-inline
     (radio :tag "$B8uJd0lMw$r%$%s%i%$%sI=<($9$k!)(B"
            (const :tag "$B=D$KJB$Y$FI=<((B" vertical)
            (const :tag "$B0lNs$KI=<((B" t)
            (const :tag "$B%$%s%i%$%sI=<($7$J$$(B" nil))
     "")
    (skk-show-tooltip
     (const :tag "$B8uJd0lMw!&Cm<a$r%D!<%k%F%#%C%W$GI=<($9$k(B" t) "")
    (skk-show-candidates-always-pop-to-buffer
     (const :tag "$B8uJd0lMw$rJL%&%$%s%I%&$rMQ0U$7$FI=<($9$k(B" t) "")))

(defconst skk-cus-params-ui
  '((skk-egg-like-newline
     (const :tag "$B"'%b!<%I$G$N(B Return [Enter] $B%-!<$O3NDj$N$_$G2~9T$O$7$J$$(B" t)
     "")
    (skk-kakutei-early
     (const :tag "$BL@<(E*$J3NDj$r>JN,2DG=$K$9$k(B" t) "")
    (skk-delete-implies-kakutei
     (const :tag "$B"'%b!<%I$G(B BS $B$r2!$7$?$i3NDj$9$k(B" t) "")
    (skk-auto-insert-paren
     (const :tag "$BJD3g8L$r<+F0E*$KA^F~$9$k(B" t) "")))

(defconst skk-cus-params-henkan
  '((skk-auto-start-henkan
     (const :tag "$BFCDj$NJ8;z$NF~NO;~$K<+F0E*$KJQ49$r3+;O$9$k(B" t) "")
    (skk-henkan-okuri-strictly
     (const :tag "$BAw$j2>L>$,87L)$K@5$7$$8uJd$N$_I=<($9$k(B" t) "")
    (skk-henkan-strict-okuri-precedence
     (const :tag "$BAw$j2>L>$,87L)$K@5$7$$8uJd$rM%@h$7$FI=<($9$k(B" t) "")
    (skk-check-okurigana-on-touroku
     (radio :tag "$B<-=qEPO?;~$NM>7W$JAw$j2>L>$N<+F0=hM}$O!)(B"
            (const :tag "$B<+F0=hM}$9$k(B" auto)
            (const :tag "$B%f!<%6$N;X<($K$h$k(B" ask)
            (const :tag "$B<+F0=hM}$7$J$$(B" nil))
     "")
    (skk-j-mode-function-key-usage
     (radio :tag "$B$+$J%b!<%I$G%U%!%s%/%7%g%s%-!<$r;H$&!)(B"
            (const :tag "$BFC<lJQ49$K3d$jEv$F$k(B" conversion)
            (const :tag "$B;H$o$J$$(B" nil))
     "")))

(defconst skk-cus-params-search
  '((skk-use-look
     (const :tag "$BJd40$N;~$K(B look $B%3%^%s%I$r;H$&(B" t) "")
    (skk-auto-okuri-process
     (const :tag "$BAw$j$J$7JQ49$GAw$j$"$j8uJd$b8!:w$9$k(B" t) "")
    (skk-use-numeric-conversion
     (const :tag "$B?tCMJQ495!G=$r;H$&(B" t) "")))

(defconst skk-cus-params-input
  '((skk-use-jisx0201-input-method
     (const :tag "$BH>3Q%+%J$rF~NO2DG=$K$9$k(B" t) "")))

(defconst skk-cus-params-misc
  '((skk-share-private-jisyo
     (const :tag "$BJ#?t$N(B SKK $B$,8D?M<-=q$r6&M-$9$k(B" t) "")
    (skk-show-icon
     (const :tag "SKK $B$N%"%$%3%s$rI=<($9$k(B" t) "")
    (skk-preload
     (const :tag "SKK $B$r$"$i$+$8$a%m!<%I$7$F=i2s5/F0$r9bB.$K$9$k(B" t) "")))

(defun skk-custom-mode ()
  (kill-all-local-variables)
  (setq major-mode 'skk-custom-mode
        mode-name "SKK $B$N@_Dj(B")
  (use-local-map skk-custom-map)
  (when (and (facep 'custom-button-face)
             (facep 'custom-button-pressed-face))
    (set (make-local-variable 'widget-button-face)
         'custom-button-face)
    (set (make-local-variable 'widget-button-pressed-face)
         'custom-button-pressed-face)
    (set (make-local-variable 'widget-mouse-face)
         'custom-button-pressed-face))
  (when (and (boundp 'custom-raised-buttons)
             (symbol-value 'custom-raised-buttons))
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))
  (run-hooks 'skk-custom-mode-hook))

(defun skk-cus-info (params)
  (delq nil
        (mapcar (lambda (el)
                  (let ((val (symbol-value (car el))))
                    (if val
                        (cons (car el) val)
                      nil)))
                params)))

;;;###autoload
(defun skk-customize-group-skk ()
  (interactive)
  (customize-group 'skk))

;;;###autoload
(defun skk-customize ()
  (interactive)
  (dolist (param (append skk-cus-params-visual
                         skk-cus-params-ui
                         skk-cus-params-henkan
                         skk-cus-params-search
                         skk-cus-params-input
                         skk-cus-params-misc))
    (let ((var (car param)))
      (when (and (eq 'const (cl-caadr param))
                 (symbol-value var))
        (set var t))))
  (setq skk-custom-buffer-original (current-buffer))
  (let (
        (visual (mapcar (lambda (entry)
                          `(cons :format "%v%h\n"
                                 :doc ,(nth 2 entry)
                                 (const :format "" ,(nth 0 entry))
                                 ,(nth 1 entry)))
                        skk-cus-params-visual))
        (ui (mapcar (lambda (entry)
                      `(cons :format "%v%h\n"
                             :doc ,(nth 2 entry)
                             (const :format "" ,(nth 0 entry))
                             ,(nth 1 entry)))
                    skk-cus-params-ui))
        (henkan (mapcar (lambda (entry)
                          `(cons :format "%v%h\n"
                                 :doc ,(nth 2 entry)
                                 (const :format "" ,(nth 0 entry))
                                 ,(nth 1 entry)))
                        skk-cus-params-henkan))
        (search (mapcar (lambda (entry)
                          `(cons :format "%v%h\n"
                                 :doc ,(nth 2 entry)
                                 (const :format "" ,(nth 0 entry))
                                 ,(nth 1 entry)))
                        skk-cus-params-search))
        (input (mapcar (lambda (entry)
                         `(cons :format "%v%h\n"
                                :doc ,(nth 2 entry)
                                (const :format "" ,(nth 0 entry))
                                ,(nth 1 entry)))
                       skk-cus-params-input))
        (misc (mapcar (lambda (entry)
                        `(cons :format "%v%h\n"
                               :doc ,(nth 2 entry)
                               (const :format "" ,(nth 0 entry))
                               ,(nth 1 entry)))
                      skk-cus-params-misc))
        (info (append
               (skk-cus-info skk-cus-params-visual)
               (skk-cus-info skk-cus-params-ui)
               (skk-cus-info skk-cus-params-henkan)
               (skk-cus-info skk-cus-params-search)
               (skk-cus-info skk-cus-params-input)
               (skk-cus-info skk-cus-params-misc))))
    (kill-buffer (get-buffer-create "*SKK $B$N4pK\@_Dj(B*"))
    ;;     (switch-to-buffer (get-buffer-create "*SKK $B$N4pK\@_Dj(B*"))
    (set-window-buffer (selected-window)
                       (get-buffer-create "*SKK $B$N4pK\@_Dj(B*"))
    (set-buffer "*SKK $B$N4pK\@_Dj(B*")

    (skk-custom-mode)
    (widget-insert "SKK $B$N4pK\@_Dj!#=*$o$C$?$i(B ")
    (widget-create 'push-button
                   :tag "done"
                   :help-echo "$B=*$o$C$?$i%\%/$r2!$7$F!#(B"
                   :action 'skk-customize-done)
    (widget-insert " $B$r2!$7$F$/$@$5$$!#(B\n\n")
    (widget-insert "$BCm0U(B: $B$$$/$D$+$N@_Dj$O:F5/F0$,I,MW$G$9!#(B\n\n")
    (setq skk-custom-params
          (list
           (widget-create 'group
                          :value info
                          `(set :inline t
                                :greedy t
                                :tag "$BI=<($K4X$9$k@_Dj(B"
                                :format "%t:\n%h%v"
                                :doc ""
                                ,@visual))
           (widget-create 'group
                          :value info
                          `(set :inline t
                                :greedy t
                                :tag "$B4pK\E*$J%f!<%6!&%$%s%?!<%U%'!<%9(B"
                                :format "%t:\n%h%v"
                                :doc ""
                                ,@ui))
           (widget-create 'group
                          :value info
                          `(set :inline t
                                :greedy t
                                :tag "$BJQ49$K4X$9$k@_Dj(B"
                                :format "%t:\n%h%v"
                                :doc ""
                                ,@henkan))
           (widget-create 'group
                          :value info
                          `(set :inline t
                                :greedy t
                                :tag "$B<-=q8!:w$K4X$9$k@_Dj(B"
                                :format "%t:\n%h%v"
                                :doc ""
                                ,@search))
           (widget-create 'group
                          :value info
                          `(set :inline t
                                :greedy t
                                :tag "$BF~NOJ}<0$K4X$9$k@_Dj(B"
                                :format "%t:\n%h%v"
                                :doc ""
                                ,@input))
           (widget-create 'group
                          :value info
                          `(set :inline t
                                :greedy t
                                :tag "$B$=$NB>$N@_Dj(B"
                                :format "%t:\n%h%v"
                                :doc ""
                                ,@misc))))
    (use-local-map skk-custom-map)
    (local-set-key "q" 'bury-buffer)
    (widget-setup)
    (goto-char (point-min))))

(defun skk-customize-done (&rest args)
  (interactive)
  (setq skk-custom-alist nil)
  (dolist (group skk-custom-params)
    (setq skk-custom-alist (append skk-custom-alist
                                   (widget-value group))))
  (dolist (param (append skk-cus-params-visual
                         skk-cus-params-ui
                         skk-cus-params-henkan
                         skk-cus-params-search
                         skk-cus-params-input
                         skk-cus-params-misc))
    (unless (assq (car param) skk-custom-alist)
      (push (cons (car param) nil) skk-custom-alist)))
  (skk-cus-set)
  (bury-buffer)
  (unless (eq skk-custom-buffer-original (current-buffer))
    ;;      (switch-to-buffer skk-custom-buffer-original))
    (set-window-buffer (selected-window)
                       (get-buffer skk-custom-buffer-original)))
  (skk-adjust-user-option))

;;;###autoload
(defun skk-cus-setup ()
  ;; obsolete
  (when (and (boundp 'skk-custom-file)
             (ignore-errors
               (file-readable-p (symbol-value 'skk-custom-file))))
    (let* ((old-name (expand-file-name (symbol-value 'skk-custom-file)))
           (new-name (concat old-name ".not_used")))
      (load-file old-name)
      (copy-file old-name new-name 'ok-if-already-exists)
      (delete-file old-name))
    (skk-cus-set)))

;;;###autoload
(defun skk-cus-set (&optional alist)
  (unless alist
    (setq alist skk-custom-alist))
  (dolist (param alist)
    (let ((variable (car param))
          (value (cdr param)))
      (funcall (or (get variable 'custom-set) 'set-default) variable value)
      (put variable 'saved-value (list (custom-quote value)))
      (custom-push-theme 'theme-value variable 'user 'set
                         (custom-quote value))
      (put variable 'customized-value nil)
      (put variable 'customized-variable-comment nil)))
  (custom-save-all)
  ;;
  (when (eq alist skk-custom-alist)
    (setq skk-custom-alist nil)))

(provide 'skk-cus)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-cus.el ends here
