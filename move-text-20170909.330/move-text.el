;;; move-text.el --- Move current line or region with M-up or M-down.

;; filename: move-text.el
;; Description: Move current line or region with M-up or M-down.
;; Author: Jason Milkins <jasonm23@gmail.com>
;; Keywords: edit
;; Package-Version: 20170909.330
;; Url: https://github.com/emacsfodder/move-text
;; Compatibility: GNU Emacs 25.1
;; Version: 2.0.8
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; MoveText 2.0.0 is a re-write of the old move-text and compatible with >= Emacs 25.1
;;
;; It allows you to move the current line using M-up / M-down if a
;; region is marked, it will move the region instead.
;;
;; Using the prefix (C-u *number* or META *number*) you can predefine how
;; many lines move-text will travel.
;;

;;; Installation:
;;
;; Put move-text.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'move-text)
;; (move-text-default-bindings)

;;; Acknowledgements:
;;
;;  Original v1.x was a Feature extracted from basic-edit-toolkit.el - by Andy Stewart (LazyCat)
;;

;;; Code:

(defun move-text-get-region-and-prefix ()
    "Get the region and prefix for the `interactive' macro, without aborting.

Note: `region-beginning' and `region-end' are the reason why an
`interactive' macro with \"r\" will blow up with the error:

\"The mark is not set now, so there is no region\"

So the predicate `region-active-p' is needed to avoid calling
them when there's no region."
    `(,@(if (region-active-p)
            (list (region-beginning) (region-end))
          (list nil nil))
      ,current-prefix-arg))

;;;###autoload
(defun move-text--total-lines ()
  "Convenience function to get the total lines in the buffer / or narrowed buffer."
  (line-number-at-pos (point-max)))

;;;###autoload
(defun move-text--at-first-line-p ()
  "Predicate, is the point at the first line?"
  (= (line-number-at-pos) 1))

;;;###autoload
(defun move-text--at-penultimate-line-p ()
  "Predicate, is the point at the penultimate line?"
  (= (line-number-at-pos) (1- (move-text--total-lines))))

;; save-mark-and-excursion in Emacs 25 works like save-excursion did before
(eval-when-compile
  (when (< emacs-major-version 25)
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))

;;;###autoload
(defun move-text--last-line-is-just-newline ()
  "Predicate, is last line just a newline?"
  (save-mark-and-excursion
   (goto-char (point-max))
   (beginning-of-line)
   (= (point-max) (point))))

;;;###autoload
(defun move-text--at-last-line-p ()
  "Predicate, is the point at the last line?"
  (= (line-number-at-pos) (move-text--total-lines)))

;;;###autoload
(defun move-text-line-up ()
  "Move the current line up."
  (interactive)
    (if (move-text--at-last-line-p)
        (let ((target-point))
          (kill-whole-line)
          (forward-line -1)
          (beginning-of-line)
          (setq target-point (point))
          (yank)
          (unless (looking-at "\n")
            (newline))
          (goto-char target-point))
      (let ((col (current-column)))
        (progn (transpose-lines 1)
               (forward-line -2)
               (move-to-column col)))))

;;;###autoload
(defun move-text-line-down ()
  "Move the current line down."
  (interactive)
  (unless (or
           (move-text--at-last-line-p)
           (and
            (move-text--last-line-is-just-newline)
            (move-text--at-penultimate-line-p)))
    (let ((col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col))))

;;;###autoload
(defun move-text-region (start end n)
  "Move the current region (START END) up or down by N lines."
  (interactive (move-text-get-region-and-prefix))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

;;;###autoload
(defun move-text-region-up (start end n)
  "Move the current region (START END) up by N lines."
  (interactive (move-text-get-region-and-prefix))
  (move-text-region start end (if (null n) -1 (- n))))

;;;###autoload
(defun move-text-region-down (start end n)
  "Move the current region (START END) down by N lines."
  (interactive (move-text-get-region-and-prefix))
  (move-text-region start end (if (null n) 1 n)))

;;;###autoload
(defun move-text-up (&optional start end n)
  "Move the line or region (START END) up by N lines."
  (interactive (move-text-get-region-and-prefix))
  (if (not (move-text--at-first-line-p))
    (if (region-active-p)
        (move-text-region-up start end n)
      (if n (cl-loop repeat n do (move-text-line-up))
        (move-text-line-up)))))

;;;###autoload
(defun move-text-down (&optional start end n)
  "Move the line or region (START END) down by N lines."
  (interactive (move-text-get-region-and-prefix))
  (if (region-active-p)
      (move-text-region-down start end n)
    (if n (cl-loop repeat n do (move-text-line-down))
      (move-text-line-down))))

;;;###autoload
(defun move-text-default-bindings ()
  "Use default bindings for move-text-up and move-text-down (M-up / M-down)."
  (interactive)
  "Bind `move-text-up' and `move-text-down' to M-up & M-down."
  (global-set-key [M-down] 'move-text-down)
  (global-set-key [M-up]   'move-text-up))

(provide 'move-text)

;;; move-text.el ends here
