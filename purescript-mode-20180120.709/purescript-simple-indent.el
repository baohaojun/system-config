;;; purescript-simple-indent.el --- Simple indentation module for PureScript Mode

;; Copyright (C) 1998  Heribert Schuetz, Graeme E Moss

;; Author: Heribert Schuetz <Heribert.Schuetz@informatik.uni-muenchen.de>
;;         Graeme E Moss <gem@cs.york.ac.uk>
;; Keywords: indentation files PureScript

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;; To support simple indentation of PureScript scripts.
;;
;;
;; Installation:
;;
;; To bind TAB to the indentation command for all PureScript buffers, add
;; this to .emacs:
;;
;;    (add-hook 'purescript-mode-hook 'turn-on-purescript-simple-indent)
;;
;; Otherwise, call `turn-on-purescript-simple-indent'.
;;
;;
;; Customisation:
;;
;; None supported.
;;
;;
;; History:
;;
;; If you have any problems or suggestions, after consulting the list
;; below, email gem@cs.york.ac.uk quoting the version of you are
;; using, the version of Emacs you are using, and a small example of
;; the problem or suggestion.
;;
;; Version 1.0:
;;   Brought over from PureScript mode v1.1.
;;
;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; (None so far.)

;;; Code:

;; All functions/variables start with
;; `(turn-(on/off)-)purescript-simple-indent'.

(require 'purescript-mode)

(defgroup purescript-simple-indent nil
  "Simple PureScript indentation."
  :link '(custom-manual "(purescript-mode)Indentation")
  :group 'purescript
  :prefix "purescript-simple-indent-")

;; Version.
(defconst purescript-simple-indent-version "1.2"
  "`purescript-simple-indent' version number.")
(defun purescript-simple-indent-version ()
  "Echo the current version of `purescript-simple-indent' in the minibuffer."
  (interactive)
  (message "Using purescript-simple-indent version %s"
           purescript-simple-indent-version))

;; Partly stolen from `indent-relative' in indent.el:
(defun purescript-simple-indent ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following whitespace in
lines preceeding point.  A position is visible if it is to the left of
the first non-whitespace of every nonblank line between the position and
the current line.  If there is no visible indent point beyond the current
column, `tab-to-tab-stop' is done instead."
  (interactive)
  (let* ((start-column (current-column))
         (invisible-from nil)           ; `nil' means infinity here
         (indent
          (catch 'purescript-simple-indent-break
            (save-excursion
              (while (progn (beginning-of-line)
                            (not (bobp)))
                (forward-line -1)
                (if (not (looking-at "[ \t]*\n"))
                    (let ((this-indentation (current-indentation)))
                      (if (or (not invisible-from)
                              (< this-indentation invisible-from))
                          (if (> this-indentation start-column)
                              (setq invisible-from this-indentation)
                            (let ((end (line-beginning-position 2)))
                              (move-to-column start-column)
                              ;; Is start-column inside a tab on this line?
                              (if (> (current-column) start-column)
                                  (backward-char 1))
                              (or (looking-at "[ \t]")
                                  (skip-chars-forward "^ \t" end))
                              (skip-chars-forward " \t" end)
                              (let ((col (current-column)))
                                (throw 'purescript-simple-indent-break
                                       (if (or (= (point) end)
                                               (and invisible-from
                                                    (> col invisible-from)))
                                           invisible-from
                                         col)))))))))))))
    (if indent
        (let ((opoint (point-marker)))
          (indent-line-to indent)
          (if (> opoint (point))
              (goto-char opoint))
          (set-marker opoint nil))
      (tab-to-tab-stop))))

(defun purescript-simple-indent-backtab ()
  "Indent backwards.  Dual to `purescript-simple-indent'."
  (interactive)
  (let ((current-point (point))
        (i 0)
        (x 0))
    (goto-char (line-beginning-position))
    (save-excursion
      (while (< (point) current-point)
        (purescript-simple-indent)
        (setq i (+ i 1))))
    (while (< x (- i 1))
      (purescript-simple-indent)
      (setq x (+ x 1)))))

(defun purescript-simple-indent-newline-same-col ()
  "Make a newline and go to the same column as the current line."
  (interactive)
  (let ((point (point)))
    (let ((start-end
           (save-excursion
             (let* ((start (line-beginning-position))
                    (end (progn (goto-char start)
                                (search-forward-regexp
                                 "[^ ]" (line-end-position) t 1))))
               (when end (cons start (1- end)))))))
      (if start-end
          (progn (newline)
                 (insert (buffer-substring-no-properties
                          (car start-end) (cdr start-end))))
        (newline)))))

(defun purescript-simple-indent-newline-indent ()
  "Make a newline on the current column and indent on step."
  (interactive)
  (purescript-simple-indent-newline-same-col)
  (insert (make-string purescript-indent-spaces ? )))

;;;###autoload
(define-minor-mode purescript-simple-indent-mode
  "Simple PureScript indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `purescript-simple-indent-backtab'
\ (bound to <backtab> by default) will move the cursor the
previous indent point.  An indent point is a non-whitespace
character following whitespace.

Runs `purescript-simple-indent-hook' on activation."
  :lighter " Ind"
  :group 'purescript-simple-indent
  :keymap '(([backtab] . purescript-simple-indent-backtab))
  (kill-local-variable 'indent-line-function)
  (when purescript-simple-indent-mode
    (set (make-local-variable 'indent-line-function) 'purescript-simple-indent)
    (run-hooks 'purescript-simple-indent-hook)))

;; The main functions.
;;;###autoload
(defun turn-on-purescript-simple-indent ()
  "Turn on function `purescript-simple-indent-mode'."
  (interactive)
  (purescript-simple-indent-mode))

(defun turn-off-purescript-simple-indent ()
  "Turn off function `purescript-simple-indent-mode'."
  (interactive)
  (purescript-simple-indent-mode 0))

;; Provide ourselves:

(provide 'purescript-simple-indent)

;;; purescript-simple-indent.el ends here
