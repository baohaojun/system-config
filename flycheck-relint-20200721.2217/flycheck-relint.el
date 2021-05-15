;;; flycheck-relint.el --- A Flycheck checker for elisp regular expressions  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: lisp
;; Package-Commit: 4d112871b82d5ea6d0d3e4aaacc55a45536707b7
;; Package-Version: 20200721.2217
;; Package-X-Original-Version: 0-pre
;; URL: https://github.com/purcell/flycheck-relint
;; Package-Requires: ((emacs "26.1") (flycheck "0.22") (relint "1.15"))

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

;; Provides feedback via flycheck about issues with `rx' and literal
;; regular expressions in Emacs Lisp, using `relint'.

;; To enable, use something like this:

;;    (eval-after-load 'flycheck
;;      '(flycheck-relint-setup))

;;; Code:

(require 'flycheck)
(require 'relint)
(require 'pcase)


(defun flycheck-relint--start (checker callback)
  "Flycheck start function for relint.
CHECKER is this checker, and CALLBACK is the flycheck dispatch function."
  (funcall callback 'finished
           (mapcar (pcase-lambda (`(,message ,expr-pos ,error-pos ,str ,str-idx ,severity))
                     (if error-pos
                         (flycheck-relint--error-at error-pos severity message)
                       (flycheck-relint--error-at expr-pos
                                                  severity
                                                  (mapconcat 'identity
                                                             (cons message
                                                                   (when str
                                                                     (cons (relint--quote-string str)
                                                                           (when str-idx
                                                                             (list (concat " " (relint--caret-string str str-idx)))))))
                                                             "\n"))))
                   (relint-buffer (current-buffer)))))


(defun flycheck-relint--error-at (pos severity message)
  "Create a flycheck error with MESSAGE and SEVERITY for POS."
  (save-excursion
    (goto-char pos)
    (move-beginning-of-line 1)
    (let* ((line (line-number-at-pos))
           (col (- pos (point))))
      (flycheck-error-new-at line (1+ col) severity message :checker 'flycheck-relint))))



;;; Checker definition

(flycheck-define-generic-checker 'emacs-lisp-relint
  "Report errors detected by `relint'."
  :start #'flycheck-relint--start
  :modes '(emacs-lisp-mode lisp-interaction-mode))



;;;###autoload
(defun flycheck-relint-setup ()
  "Setup flycheck-package.
Add `emacs-lisp-relint' to `flycheck-checkers' and set up the checker chain."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-relint t)
  (flycheck-add-next-checker 'emacs-lisp-checkdoc 'emacs-lisp-relint t)
  (when (flycheck-valid-checker-p 'emacs-lisp-package)
    (flycheck-add-next-checker 'emacs-lisp-package 'emacs-lisp-relint t)))


(provide 'flycheck-relint)
;;; flycheck-relint.el ends here
