;;; move-dup.el --- Eclipse-like moving and duplicating lines or rectangles

;; Copyright (C) 2014-2021 Jimmy Yuen Ho Wong

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 2.0.0
;; Package-Version: 20210127.1938
;; Package-Commit: 5906503e0b9b832b1d5062c9cd27cf72a2ce4817
;; Created: 11 June 2014
;; Url: https://github.com/wyuenho/move-dup
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience text edit

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package offers convenient editing commands much like Eclipse's ability
;; to move and duplicate lines or rectangular selections by way of
;; `move-dup-mode'.

;; If you aren't using `package.el' or plan to customize the default
;; key-bindings, you need to put `move-dup.el' into your Emacs' load-path and
;; `require' it in your Emacs init file; otherwise you can skip this part.

;; (require 'move-dup)

;; If you don't want to toggle the minor mode, you can bind these functions like
;; so.  All of these functions work on a single line or a rectangle.

;; (global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
;; (global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
;; (global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
;; (global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

;; If you used `package.el' to install `move-dup.el', this is equivalent to all
;; of the above.
;; (global-move-dup-mode)

;; You can also turn on `move-dup-mode' individually for each buffer.
;; (move-dup-mode)

;;; Code:

(defgroup move-dup nil
  "Move or duplicate text."
  :group 'editing
  :prefix "move-dup")

(defcustom move-dup-undo-auto-amalgamate t
  "Whether a series of move-dup command should only require one undo to undo all."
  :type 'boolean
  :group 'move-dup)

(defun move-dup-merge-undo-boundaries-maybe ()
  (when move-dup-undo-auto-amalgamate
    (undo-auto-amalgamate)))
(make-obsolete 'md-merge-undo-boundaries-maybe 'move-dup-merge-undo-boundaries-maybe "2.0.0")

(defun move-dup-ensure-rectangle ()
  "Normalizes the selection by making sure it's always a rectangle.

After normalization, the point always comes after mark.  The
region will always be expanded such that it will always begin
from the beginning of the line the mark is on, and ends at the
beginning of the next line of the end of the region."
  (let (deactivate-mark)
    (if (< (point) (mark))
        (exchange-point-and-mark))
    (when (not (char-equal (char-before (region-end)) 10))
      (if (and (eobp) (not (char-equal (following-char) 10)))
          (newline)
        (progn
          (end-of-line)
          (forward-char))))
    (exchange-point-and-mark)
    (beginning-of-line)
    (exchange-point-and-mark)))
(make-obsolete 'md-ensure-rectangle 'move-dup-ensure-rectangle "2.0.0")

;;;###autoload
(defun move-dup-move-region (&optional n)
  "Interactive function to move the current selection N lines.

If the selection is not a rectangle, this function will expand
the selection to a rectangle via the function
`move-dup-ensure-rectangle' and move it accordingly.  If the prefix N
is positive, this function moves the rectangle forward N lines;
otherwise backward."
  (interactive "*p")
  (move-dup-ensure-rectangle)
  (let* ((start (region-beginning))
         (end (region-end)))
    (if (< n 0)
        (exchange-point-and-mark))
    (pop-mark)
    (push-mark)
    (forward-line n)
    (let* ((swap-start (region-beginning))
           (swap-end (region-end)))
      (let (deactivate-mark)
        (let ((text (delete-and-extract-region start end)))
          (insert text)
          (pop-mark)))
      (push-mark)
      (backward-char (- end start))
      (exchange-point-and-mark))))
(make-obsolete 'md-move-region 'move-dup-move-region "2.0.0")

;;;###autoload
(defun move-dup-move-line (&optional n)
  "Interactive function to move the current line by N lines.

If the prefix N is positive, this function moves the current line
forward N lines; otherwise backward."
  (interactive "*p")
  (let ((col (current-column)))
    (goto-char (save-mark-and-excursion
                 (push-mark)
                 (forward-line)
                 (move-dup-move-region n)
                 (region-beginning)))
    (move-to-column col)))
(make-obsolete 'md-move-line 'move-dup-move-line "2.0.0")

(defun move-dup-move-line-or-region (n)
  "Decides whether a line or selection should be moved N lines."
  (move-dup-merge-undo-boundaries-maybe)
  (if (use-region-p)
      (move-dup-move-region n)
    (move-dup-move-line n)))
(make-obsolete 'md-move-line-or-region 'move-dup-move-line-or-region "2.0.0")

;;;###autoload
(defun move-dup-move-lines-up (&optional n)
  "Interactive function to move the current line or selection up.

If the prefix N is positive, this function moves the current line
or selection up N lines; otherwise down."
  (interactive "*p")
  (move-dup-move-line-or-region (if (or (null n) (= n 0)) -1 (- n))))
(make-obsolete 'md-move-lines-up 'move-dup-move-lines-up "2.0.0")

;;;###autoload
(defun move-dup-move-lines-down (&optional n)
  "Interactive function to move the current line or selection down.

If the prefix N is positive, this function moves the current line
or selection down N lines; otherwise up."
  (interactive "*p")
  (move-dup-move-line-or-region (if (or (null n) (= n 0)) 1 n)))
(make-obsolete 'md-move-lines-down 'move-dup-move-lines-down "2.0.0")

;;;###autoload
(defun move-dup-duplicate-up (&optional n)
  "Interactive function to duplicate the current line or selection upward.

If the prefix N is positive, this function makes N duplicates of
the current line or selection and place them above the current
line or selection."
  (interactive "*p")
  (dotimes (i n) (move-dup-duplicate-line-or-region "up")))
(make-obsolete 'md-duplicate-up 'move-dup-duplicate-up "2.0.0")

;;;###autoload
(defun move-dup-duplicate-down (&optional n)
  "Interactive function to duplicate the current line or selection downward.

If the prefix N is positive, this function makes N duplicates of
the current line or selection and place them below the current
line or selection."
  (interactive "*p")
  (dotimes (i n) (move-dup-duplicate-line-or-region "down")))
(make-obsolete 'md-duplicate-down 'move-dup-duplicate-down "2.0.0")

(defun move-dup-duplicate-line-or-region (direction)
  "Decides whether a line or selection should be duplicated.

DIRECTION must be one of \"up\" or \"down\"."
  (move-dup-merge-undo-boundaries-maybe)
  (if (use-region-p)
      (move-dup-duplicate-region direction)
    (move-dup-duplicate-line direction)))
(make-obsolete 'md-duplicate-line-or-region 'move-dup-duplicate-line-or-region "2.0.0")

(defun move-dup-duplicate-line (direction)
  "Function to duplicate the current line.

DIRECTION must be one of \"up\" or \"down\"."
  (let ((text (buffer-substring (line-beginning-position) (line-end-position)))
        (col (current-column)))
    (unless (and (zerop (forward-line))
                 (= (preceding-char) ?\n))
      (newline))
    (insert text)
    (open-line 1)
    (if (string= direction "up")
        (forward-line -1))
    (move-to-column col)))
(make-obsolete 'md-duplicate-line 'move-dup-duplicate-line "2.0.0")

(defun move-dup-duplicate-region (direction)
  "Function to duplicate the current selection.

DIRECTION must be one of \"up\" or \"down\".

If the selection is not a rectangle, this function will expand
the selection to a rectangle via the function
`move-dup-ensure-rectangle' and duplicate it accordingly.  If the
DIRECTION is \"up\", this function duplicates the selected
rectangle and places it __below__ the selection; __above__ if
DIRECTION is \"down\"."
  (move-dup-ensure-rectangle)
  (let* ((start (region-beginning))
         (end (region-end))
         (text (buffer-substring start end))
         (text-length (length text)))
    (let (deactivate-mark)
      (insert text))
    (cond ((string= direction "down")
           (pop-mark)
           (push-mark)
           (backward-char text-length)
           (exchange-point-and-mark))
          ((string= direction "up")
           (backward-char text-length)))))
(make-obsolete 'md-duplicate-region 'move-dup-duplicate-region "2.0.0")

;;;###autoload
(define-minor-mode move-dup-mode
  "Minor mode for Eclipse-like moving and duplicating lines or
rectangles with default key bindings.

The default key bindings are:

\([M-up]     . move-dup-move-lines-up)
\([M-down]   . move-dup-move-lines-down)
\([C-M-up]   . move-dup-duplicate-up)
\([C-M-down] . move-dup-duplicate-down)"
  :lighter " md"
  :group 'move-dup
  :keymap '(([M-up]     . move-dup-move-lines-up)
            ([M-down]   . move-dup-move-lines-down)
            ([C-M-up]   . move-dup-duplicate-up)
            ([C-M-down] . move-dup-duplicate-down)))

(defun move-dup-on ()
  "Decides whether the function `move-dup-mode' should be called with t."
  (unless (minibufferp)
    (move-dup-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-move-dup-mode move-dup-mode move-dup-on
  :group 'move-dup)

(provide 'move-dup)
;;; move-dup.el ends here
