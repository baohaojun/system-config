;;; purescript-navigate-imports.el --- A function for cycling through PureScript import lists

;; Copyright (C) 2010  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The cycling step will stop once at the last import list so
;; that it is easy to add a new import list.

;; This module works completely independently of any libraries
;; (including purescript-mode).

;; Exports three interactive functions:
;; 1. purescript-navigate-imports
;; 2. purescript-navigate-imports-go
;; 3. purescript-navigate-imports-return

;; Example usage:

;; (require 'purescript-navigate-imports)
;; (define-key purescript-mode-map [f8] 'purescript-navigate-imports)

;;; Code:

(defvar purescript-navigate-imports-start-point nil)

;;;###autoload
(defun purescript-navigate-imports (&optional return)
  "Cycle the PureScript import lines or return to point (with prefix arg)."
  (interactive "P")
  (if return
      (purescript-navigate-imports-return)
    (purescript-navigate-imports-go)))

;;;###autoload
(defun purescript-navigate-imports-go ()
  "Go to the first line of a list of consequtive import lines. Cycles."
  (interactive)
  (unless (or (purescript-navigate-imports-line)
              (equal (line-beginning-position) (point-min))
              (save-excursion (forward-line -1)
                              (purescript-navigate-imports-line)))
    (setq purescript-navigate-imports-start-point (point)))
  (purescript-navigate-imports-go-internal))

;;;###autoload
(defun purescript-navigate-imports-return ()
  "Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily."
  (interactive)
  (when purescript-navigate-imports-start-point
    (goto-char purescript-navigate-imports-start-point)))

(defun purescript-navigate-imports-go-internal ()
  "Go to the first line of a list of consequtive import lines. Cycle."
  (if (purescript-navigate-imports-line)
      (progn (purescript-navigate-imports-goto-end)
             (when (purescript-navigate-imports-find-forward-line)
               (purescript-navigate-imports-go-internal)))
    (let ((point (purescript-navigate-imports-find-forward-line)))
      (if point
          (goto-char point)
        (progn (goto-char (point-min))
               (if (purescript-navigate-imports-find-forward-line)
                   (purescript-navigate-imports-go-internal)
                 (when (search-forward-regexp "^module" nil t 1)
                   (search-forward "\n\n" nil t 1))))))))

(defun purescript-navigate-imports-goto-end ()
  "Skip a bunch of consequtive import lines."
  (while (not (or (equal (point)
                         (point-max))
                  (not (purescript-navigate-imports-line))))
    (forward-line)))

(defun purescript-navigate-imports-find-forward-line ()
  "Return a point with at an import line, or nothing."
  (save-excursion
    (while (not (or (equal (point) (point-max))
                    (purescript-navigate-imports-after-imports-p) ;; This one just speeds it up.
                    (purescript-navigate-imports-line)))
      (forward-line))
    (let ((point (point)))
      (if (purescript-navigate-imports-line)
          (point)
        nil))))

(defun purescript-navigate-imports-line ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match "^import " line)
        line
      nil)))

(defun purescript-navigate-imports-after-imports-p ()
  "Are we after the imports list? Just for a speed boost."
  (save-excursion
    (goto-char (line-beginning-position))
    (not (not (search-forward-regexp "\\( = \\|\\<instance\\>\\| :: \\)"
                                     (line-end-position) t 1)))))

(provide 'purescript-navigate-imports)

;;; purescript-navigate-imports.el ends here
