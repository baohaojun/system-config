;;; ac-helm.el --- Helm interface for auto-complete

;; Copyright (C) 2009  rubikitch
;; Copyright (C) 2013  Yasuyuki Oka <yasuyk@gmail.com>

;; Author: rubikitch <rubikitch@ruby-lang.org>
;;         Yasuyuki Oka <yasuyk@gmail.com>
;; Maintainer: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 20140919.917
;; X-Original-Version: 2.2
;; Package-Requires: ((helm "1.6.3")(auto-complete "1.4.0")(popup "0.5.0") (cl-lib "0.5"))
;; Keywords: completion, convenience, helm

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Auto Complete with Helm.  It enables us to narrow candidates
;; with helm interface.  If you have helm-match-plugin.el,
;; candidates can be narrowed many times.

;; Commands:
;;
;; Below are complete command list:
;;
;;  `ac-complete-with-helm'
;;    Select auto-complete candidates by `helm'.
;;
;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Installation:

;; Add the following to your emacs init file:
;;
;; (require 'ac-helm) ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-:") 'ac-complete-with-helm)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

;; That's all.

;;; Code:

(require 'helm)
(require 'helm-match-plugin nil t)
(require 'helm-elisp)
(require 'auto-complete)
(require 'cl-lib)
(require 'popup)

;;;###autoload
(defun ac-complete-with-helm ()
  "Select `auto-complete' candidates by `helm'.
It is useful to narrow candidates."
  (interactive)
  (unless ac-completing
    (call-interactively 'auto-complete))
    (with-helm-show-completion ac-point (point)
      (helm :sources 'helm-source-auto-complete-candidates
            :buffer  "*helm auto-complete*")))

(defun helm-auto-complete-init ()
  (helm-attrset 'ac-candidates ac-candidates)
  (helm-attrset 'menu-width
                (popup-preferred-width ac-candidates))
  (helm-attrset 'ac-prefix ac-prefix)
  (when (<= (length ac-candidates) 1)
    (helm-exit-minibuffer))
  (ac-abort))

(defun helm-auto-complete-action (string)
  (delete-char (- (length (helm-attr 'ac-prefix))))
  (insert string)
  (prog1 (let ((action (get-text-property 0 'action string)))
           (if action (funcall action)))
    ;; for GC
    (helm-attrset 'ac-candidates nil)))

(defun helm-auto-complete-candidates ()
  (cl-loop for x in (helm-attr 'ac-candidates) collect
           (cons
            (helm-aif (get-text-property 0 'action x)
                (format "%s%s <%s>"
                        x
                        ;; padding
                        (make-string (- (helm-attr 'menu-width) (length x)) ? )
                        ;; action function name
                        it)
              x)
            x)))

(defvar helm-source-auto-complete-candidates
  '((name . "Auto Complete")
    (init . helm-auto-complete-init)
    (candidates . helm-auto-complete-candidates)
    (action . helm-auto-complete-action)
    (persistent-action . popup-item-show-help)
    (ac-candidates)
    (menu-width)))

(provide 'ac-helm)

;;; ac-helm.el ends here
