;;; windswap.el --- Like windmove, but swaps buffers while moving point  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Homepage: https://github.com/purcell/windswap
;; Keywords: frames, convenience
;; Package-Commit: 1a334f6543e0a30c55ea1e6071e9732d948f9e4b
;; Package-Version: 20200722.411
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides commands analagous to `windmove' commands such as
;; `windmove-left' which also swap the buffers in the previous and new
;; windows.  This allows the user to "drag" the current buffer to
;; neighbouring windows.  The idea is to bind keys similarly to
;; `windmove', so that the two packages can be used interchangeably to
;; navigate and rearrange windows.

;;   (windmove-default-keybindings 'control)
;;   (windswap-default-keybindings 'control 'shift)

;;; Code:

(require 'windmove)
(require 'cl-lib)

(defgroup windswap nil
  "Like windmove, but swaps buffers while moving point."
  :group 'frames)

;;;###autoload
(defun windswap-default-keybindings (&rest modifiers)
  "Set up keybindings for `windswap'.
Keybindings are of the form MODIFIERS-{left,right,up,down}.
Default MODIFIERS are `meta' and `shift'.
See also `windmove-default-keybindings'."
  (interactive)
  (unless modifiers (setq modifiers (list 'meta 'shift)))
  (global-set-key (vector (append modifiers '(left)))  'windswap-left)
  (global-set-key (vector (append modifiers '(right))) 'windswap-right)
  (global-set-key (vector (append modifiers '(up)))    'windswap-up)
  (global-set-key (vector (append modifiers '(down)))  'windswap-down))

(defun windswap--find-other-window (direction sign)
  "Find the swappable window in DIRECTION.
Unlike the `windmove' equivalent, this skips over the minibuffer
and any dedicated windows, because the contents of those windows
cannot be swapped.  SIGN is as per that named argument in
`window-in-direction'.  This function honours the variable
`windmove-wrap-around'."
  (let* ((dir (cond
               ((eq direction 'up) 'above)
               ((eq direction 'down) 'below)
               (t direction)))
         (initial (selected-window))
         (window initial))
    (cl-block nil
      (while t
        (setq window (window-in-direction dir window nil sign windmove-wrap-around t))
        (when (or (null window)
                  (eq initial window)
                  (not (window-minibuffer-p window)))
          (cl-return window))))))

(defun windswap--check-window (window)
  "Report a user error if WINDOW cannot be swapped."
  (when (window-minibuffer-p window)
    (user-error "Can't swap the minibuffer window"))
  (when (window-dedicated-p window)
    (user-error "Dedicated windows can't be swapped")))

(defalias 'windswap--swap-states
  (if (fboundp 'window-swap-states)
      'window-swap-states
    (lambda (a b)
      (let ((buf-b (window-buffer b))
            (buf-a (window-buffer a)))
        (set-window-buffer a buf-b)
        (set-window-buffer b buf-a)))))

(defun windswap--do-swap (direction arg)
  "Try to swap in DIRECTION.
ARG is as for the `windmove' commands."
  (let ((initial (selected-window)))
    (windswap--check-window initial)
    (let ((new (windswap--find-other-window direction arg)))
      (unless new
        (user-error "No swappable window %s from selected window" direction))
      (windswap--check-window new)
      (windswap--swap-states initial new)
      (select-window new))))

;;;###autoload
(defun windswap-right (&optional arg)
  "Like `windmove-right', but transpose buffers after switching windows.
ARG is as for that function."
  (interactive "P")
  (windswap--do-swap 'right arg))

;;;###autoload
(defun windswap-left (&optional arg)
  "Like `windmove-left', but transpose buffers after switching windows.
ARG is as for that function."
  (interactive "P")
  (windswap--do-swap 'left arg))

;;;###autoload
(defun windswap-up (&optional arg)
  "Like `windmove-up', but transpose buffers after switching windows.
ARG is as for that function."
  (interactive "P")
  (windswap--do-swap 'up arg))

;;;###autoload
(defun windswap-down (&optional arg)
  "Like `windmove-down', but transpose buffers after switching windows.
ARG is as for that function."
  (interactive "P")
  (windswap--do-swap 'down arg))


(provide 'windswap)
;;; windswap.el ends here
