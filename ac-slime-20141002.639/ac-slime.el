;;; ac-slime.el --- An auto-complete source using slime completions

;; Copyright (C) 2010-2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/ac-slime
;; Version: 20141002.639
;; X-Original-Version: DEV
;; Package-Requires: ((auto-complete "1.4") (slime "2.9") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:

;;     (require 'ac-slime)
;;     (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'slime-repl-mode))
;;

;;; Code:

(require 'cl-lib)
(require 'slime)
(require 'auto-complete)

(defgroup ac-slime nil
  "Slime auto-complete customizations"
  :prefix "ac-slime-"
  :group 'slime)

(defcustom ac-slime-show-flags t
  "When non-nil, show completion result flags during fuzzy completion."
  :group 'ac-slime)

(defun ac-source-slime-fuzzy-candidates ()
  "Return a possibly-empty list of fuzzy completions for the symbol at point."
  (when (slime-connected-p)
    (let ((slime-fuzzy-completion-limit 50))
      (mapcar (lambda (result)
                (let ((sym (car result))
                      (flags (car (last result))))
                  (if ac-slime-show-flags
                      (propertize sym 'summary flags)
                    sym)))
              (car (slime-fuzzy-completions (substring-no-properties ac-prefix)))))))

(defun ac-source-slime-simple-candidates ()
  "Return a possibly-empty list of completions for the symbol at point."
  (when (slime-connected-p)
    (car (slime-simple-completions (substring-no-properties ac-prefix)))))

(defun ac-source-slime-case-correcting-completions (name collection)
  (mapcar #'(lambda (completion)
              ;; FIXME
              (cl-replace completion name))
          (all-completions (downcase name) collection)))

(defvar ac-slime-current-doc nil "Holds slime docstring for current symbol.")
(defun ac-slime-documentation (symbol-name)
  "Return a documentation string for SYMBOL-NAME."
  (let ((symbol-name (substring-no-properties symbol-name)))
    (slime-eval `(swank:documentation-symbol ,symbol-name))))

(defun ac-slime-init ()
  "Called when completion source is initialized."
  (setq ac-slime-current-doc nil))

;;;###autoload
(defface ac-slime-menu-face
  '((t (:inherit ac-candidate-face)))
  "Face for slime candidate menu."
  :group 'auto-complete)

;;;###autoload
(defface ac-slime-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the slime selected candidate."
  :group 'auto-complete)

;;;###autoload
(defvar ac-source-slime-fuzzy
  '((init . ac-slime-init)
    (candidates . ac-source-slime-fuzzy-candidates)
    (candidate-face . ac-slime-menu-face)
    (selection-face . ac-slime-selection-face)
    (prefix . slime-symbol-start-pos)
    (symbol . "l")
    (match . (lambda (prefix candidates) candidates))
    (document . ac-slime-documentation))
  "Source for fuzzy slime completion.")

;;;###autoload
(defvar ac-source-slime-simple
  '((init . ac-slime-init)
    (candidates . ac-source-slime-simple-candidates)
    (candidate-face . ac-slime-menu-face)
    (selection-face . ac-slime-selection-face)
    (prefix . slime-symbol-start-pos)
    (symbol . "l")
    (document . ac-slime-documentation)
    (match . ac-source-slime-case-correcting-completions))
  "Source for slime completion.")


;;;###autoload
(defun set-up-slime-ac (&optional fuzzy)
  "Add an optionally FUZZY slime completion source to `ac-sources'."
  (interactive)
  (add-to-list 'ac-sources
               (if fuzzy
                   'ac-source-slime-fuzzy
                 'ac-source-slime-simple)))


(provide 'ac-slime)

;;; ac-slime.el ends here
