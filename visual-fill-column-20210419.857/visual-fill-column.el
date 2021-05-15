;;; visual-fill-column.el --- fill-column for visual-line-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 Joost Kremers
;; Copyright (C) 2016 Martin Rudalics
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; URL: https://github.com/joostkremers/visual-fill-column
;; Package-Version: 20210419.857
;; Package-Commit: 6fa9e7912af412533aec0da8b8f62c227f9f3f54
;; Created: 2015
;; Version: 2.4
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; visual-fill-column is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visual-fill-column is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `visual-fill-column-mode' is a small Emacs minor mode that mimics the effect
;; of `fill-column' in `visual-line-mode'.  Instead of wrapping lines at the
;; window edge, which is the standard behaviour of `visual-line-mode', it wraps
;; lines at `fill-column'.  If `fill-column' is too large for the window, the
;; text is wrapped at the window edge.

;;; Code:

(defgroup visual-fill-column nil "Wrap lines according to `fill-column' in `visual-line-mode'."
  :group 'text
  :prefix "visual-fill-column-")

(defcustom visual-fill-column-width nil
  "Width of the text area.
By default, the global value of `fill-column' is used, but if
this option is set to a value, it is used instead."
  :group 'visual-fill-column
  :type '(choice (const :tag "Use `fill-column'" :value nil)
                 (integer :tag "Specify width" :value 70)))
(make-variable-buffer-local 'visual-fill-column-width)
(put 'visual-fill-column-width 'safe-local-variable 'numberp)

(defcustom visual-fill-column-extra-text-width nil
  "Additional columns added to the text area.
This is a cons cell of two numbers: the first is subtracted from
the left margin, the second from the right margin.  This option
can be used to accommodate, e.g,, line numbers, which would
otherwise reduce the actual size of the text area."
  :group 'visual-fill-column
  :type '(choice (const :tag "No extra text width" :value nil)
                 (cons :tag "Extra text width"
                       (integer :tag "Extra width left ")
                       (integer :tag "Extra width right"))))
(make-variable-buffer-local 'visual-fill-column-extra-text-width)

(defcustom visual-fill-column-fringes-outside-margins t
  "Put the fringes outside the margins."
  :group 'visual-fill-column
  :type '(choice (const :tag "Put fringes outside the margins" t)
                 (const :tag "Keep the fringes inside the margins" nil)))
(make-variable-buffer-local 'visual-fill-column-fringes-outside-margins)
(put 'visual-fill-column-fringes-outside-margins 'safe-local-variable 'symbolp)

(defcustom visual-fill-column-center-text nil
  "If set, center the text area in the window."
  :group 'visual-fill-column
  :type '(choice (const :tag "Display text area at window margin" nil)
                 (const :tag "Center text area" t)))
(make-variable-buffer-local 'visual-fill-column-center-text)
(put 'visual-fill-column-center-text 'safe-local-variable 'symbolp)

(defcustom visual-fill-column-enable-sensible-window-split nil
  "Set `split-window-preferred-function' so as to allow vertical window splits.
If this option is set, `visual-fill-column' sets the variable
`split-window-preferred-function' to
`visual-fill-column-split-window-sensibly', in order to allow
`display-buffer' to split windows in two side-by-side windows."
  :group 'visual-fill-column
  :type '(choice (const :tag "Allow vertical window split" nil)
                 (const :tag "Use standard window split" t)))

(defvar visual-fill-column--use-split-window-parameter nil "If set, the window parameter `split-window' is used.")

(defvar visual-fill-column--use-min-margins nil "If set, the window parameter `min-margins' is used.")

(defvar visual-fill-column-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right-margin mouse-1] (global-key-binding [mouse-1])) ; #'mouse-set-point
    (define-key map [right-margin mouse-2] (global-key-binding [mouse-2])) ; #'mouse-yank-primary
    (define-key map [right-margin mouse-3] (global-key-binding [mouse-3])) ; #'mouse-save-then-kill
    (define-key map [right-margin drag-mouse-1] #'ignore)
    (define-key map [right-margin drag-mouse-2] #'ignore)
    (define-key map [right-margin drag-mouse-3] #'ignore)
    (define-key map [right-margin double-mouse-1] #'ignore)
    (define-key map [right-margin double-mouse-2] #'ignore)
    (define-key map [right-margin double-mouse-3] #'ignore)
    (define-key map [right-margin triple-mouse-1] #'ignore)
    (define-key map [right-margin triple-mouse-2] #'ignore)
    (define-key map [right-margin triple-mouse-3] #'ignore)
    (define-key map [left-margin mouse-1] (global-key-binding [mouse-1])) ; #'mouse-set-point
    (define-key map [left-margin mouse-2] (global-key-binding [mouse-2])) ; #'mouse-yank-primary
    (define-key map [left-margin mouse-3] (global-key-binding [mouse-3])) ; #'mouse-save-then-kill
    (define-key map [left-margin drag-mouse-1] #'ignore)
    (define-key map [left-margin drag-mouse-2] #'ignore)
    (define-key map [left-margin drag-mouse-3] #'ignore)
    (define-key map [left-margin double-mouse-1] #'ignore)
    (define-key map [left-margin double-mouse-2] #'ignore)
    (define-key map [left-margin double-mouse-3] #'ignore)
    (define-key map [left-margin triple-mouse-1] #'ignore)
    (define-key map [left-margin triple-mouse-2] #'ignore)
    (define-key map [left-margin triple-mouse-3] #'ignore)
    (when (bound-and-true-p mouse-wheel-mode)
      (define-key map [right-margin mouse-wheel-down-event] #'mwheel-scroll)
      (define-key map [right-margin mouse-wheel-up-event] #'mwheel-scroll)
      (define-key map [left-margin mouse-wheel-down-event] #'mwheel-scroll)
      (define-key map [left-margin mouse-wheel-up-event] #'mwheel-scroll))
    map))

;;;###autoload
(define-minor-mode visual-fill-column-mode
  "Wrap lines according to `fill-column' in `visual-line-mode'."
  :init-value nil :lighter nil :global nil
  (if visual-fill-column-mode
      (visual-fill-column-mode--enable)
    (visual-fill-column-mode--disable)))

;;;###autoload
(define-globalized-minor-mode global-visual-fill-column-mode visual-fill-column-mode turn-on-visual-fill-column-mode
  :require 'visual-fill-column-mode
  :group 'visual-fill-column)

(defun turn-on-visual-fill-column-mode ()
  "Turn on `visual-fill-column-mode'.
Note that `visual-fill-column-mode' is only turned on in buffers
in which Visual Line mode is active as well, and only in buffers
that actually visit a file."
  (when buffer-file-name
    (visual-fill-column-mode 1)))

(defun visual-fill-column-mode--enable ()
  "Set up `visual-fill-column-mode' for the current buffer."
  (add-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window 'append 'local)

  (when visual-fill-column-enable-sensible-window-split
    ;; Note that `split-window-preferred-function' is not reset to its original
    ;; value when `visual-fill-column-mode' is disabled, because it may still be
    ;; enabled in other buffers.  When `visual-fill-column-mode' is disabled,
    ;; `visual-fill-column-split-window-sensibly' simply invokes
    ;; `split-window-sensibly', so keeping it is harmless.
    (setq-default split-window-preferred-function #'visual-fill-column-split-window-sensibly))

  (cond
   ((version<= emacs-version "27.1")
    (add-hook 'window-size-change-functions #'visual-fill-column--adjust-window 'append 'local)
    (setq visual-fill-column--use-split-window-parameter t))

   ((version< "27.1" emacs-version)
    (add-hook 'window-state-change-functions #'visual-fill-column--adjust-window 'append 'local)
    (setq visual-fill-column--use-min-margins t)))

  (visual-fill-column--adjust-window (selected-window)))

(defun visual-fill-column-mode--disable ()
  "Disable `visual-fill-column-mode' for the current buffer."
  (remove-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window 'local)

  (let ((window (get-buffer-window (current-buffer))))
    (cond
     ((version<= emacs-version "27.1")
      (remove-hook 'window-size-change-functions #'visual-fill-column--adjust-window 'local))

     ((version< "27.1" emacs-version)
      (remove-hook 'window-state-change-functions #'visual-fill-column--adjust-window 'local)
      (set-window-margins window 0 0)
      (set-window-parameter window 'min-margins nil)))
    (set-window-fringes window nil)))

(defun visual-fill-column-split-window (&optional window size side)
  "Split WINDOW, unsetting its margins first.
SIZE, and SIDE are passed on to `split-window'.  This function is
for use in the window parameter `split-window'."
  ;; Note: `split-window' has another optional argument, `pixelwise', but this
  ;; is not passed to the function in the `split-window' window parameter.
  (let ((horizontal (memq side '(t left right)))
	margins new)
    (when horizontal
      ;; Reset margins.
      (setq margins (window-margins window))
      (set-window-margins window nil))
    ;; Now try to split the window.
    (set-window-parameter window 'split-window nil)
    (unwind-protect
	(setq new (split-window window size side))
      (set-window-parameter window 'split-window #'visual-fill-column-split-window)
      ;; Restore old margins if we failed.
      (when (and horizontal (not new))
	(set-window-margins window (car margins) (cdr margins))))))

;;;###autoload
(defun visual-fill-column-split-window-sensibly (&optional window)
  "Split WINDOW sensibly, unsetting its margins first.
This function unsets the window margins and calls
`split-window-sensibly'.

By default, `split-window-sensibly' does not split a window in
two side-by-side windows if it has wide margins, even if there is
enough space for a vertical split.  This function is used as the
value of `split-window-preferred-function' to allow
`display-buffer' to split such windows."
  (let ((margins (window-margins window))
        new)
    ;; unset the margins and try to split the window
    (when (buffer-local-value 'visual-fill-column-mode (window-buffer window))
      (set-window-margins window nil))
    (unwind-protect
        (setq new (split-window-sensibly window))
      (when (not new)
        (set-window-margins window (car margins) (cdr margins))))))

(defun visual-fill-column--reset-window (window)
  "Reset the parameters and margins of WINDOW."
  (set-window-parameter window 'split-window nil)
  (set-window-parameter window 'min-margins nil)
  (set-window-margins window nil))

(defun visual-fill-column--adjust-window (&optional window)
  "Adjust the margins and fringes of WINDOW.
WINDOW defaults to the selected window.  This function only
adjusts the margins and fringes if the buffer displayed in the
selected window has `visual-fill-column-mode' enabled."
  (or window (setq window (selected-window)))
  (with-selected-window window
    (visual-fill-column--reset-window window)
    (when visual-fill-column-mode
      (set-window-fringes window nil nil visual-fill-column-fringes-outside-margins)
      (if visual-fill-column--use-split-window-parameter
          (set-window-parameter window 'split-window #'visual-fill-column-split-window))
      (if visual-fill-column--use-min-margins  ; This is non-nil if the window parameter `min-margins' is used (Emacs 27.2).
          (set-window-parameter window 'min-margins '(0 . 0)))
      (visual-fill-column--set-margins window))))

(defun visual-fill-column-adjust (&optional _inc)
  "Adjust the window margins and fringes.
This function is for use as advice to `text-scale-adjust'.  It
calls `visual-fill-column--adjust-window', but only if
`visual-fill-column' is active."
  (if visual-fill-column-mode
      (visual-fill-column--adjust-window (selected-window))))

(defun visual-fill-column--window-max-text-width (&optional window)
  "Return the maximum possible text width of WINDOW.
The maximum possible text width is the width of the current text
area plus the margins, but excluding the fringes, scroll bar, and
right divider.  WINDOW defaults to the selected window.  The
return value is scaled to account for `text-scale-mode-amount'
and `text-scale-mode-step'."
  (or window (setq window (selected-window)))
  (let* ((margins (window-margins window))
         (buffer (window-buffer window))
         (scale (if (and (boundp 'text-scale-mode-step)
                         (boundp 'text-scale-mode-amount))
                    (with-current-buffer buffer
                      (expt text-scale-mode-step
                            text-scale-mode-amount))
                  1.0)))
    (truncate (/ (+ (window-width window)
                    (or (car margins) 0)
                    (or (cdr margins) 0))
                 (float scale)))))

(defun visual-fill-column--add-extra-width (left right add-width)
  "Calculate new margins given additional text width.
LEFT and RIGHT are the current margins, ADD-WIDTH a cons cell of
  additional columns to be added to the text area.  Return a cons
  cell of the new margins, which will never be less than zero."
  (cons (max 0 (- left (car add-width)))
        (max 0 (- right (cdr add-width)))))

(defun visual-fill-column--set-margins (window)
  "Set window margins for WINDOW."
  ;; Calculate left & right margins.
  (let* ((total-width (visual-fill-column--window-max-text-width window))
         (width (or visual-fill-column-width
                    fill-column))
         (margins (if (< (- total-width width) 0) ; margins must be >= 0
                      0
                    (- total-width width)))
         (left (if visual-fill-column-center-text
                   (/ margins 2)
                 0))
         (right (- margins left)))

    (if visual-fill-column-extra-text-width
        (let ((add-width (visual-fill-column--add-extra-width left right visual-fill-column-extra-text-width)))
          (setq left (car add-width)
                right (cdr add-width))))

    ;; put an explicitly R2L buffer on the right side of the window
    (when (and (eq bidi-paragraph-direction 'right-to-left)
               (= left 0))
      (setq left right)
      (setq right 0))

    (set-window-margins window left right)))

(provide 'visual-fill-column)

;;; visual-fill-column.el ends here
