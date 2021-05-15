;;; cfrs.el --- Child-frame based read-string -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "26.1") (dash "2.11.0") (s "1.10.0") (posframe "0.6.0"))
;; Package-Commit: 7c42f2c82c7ae689f3ef291b066688c58ab96298
;; Package-Version: 20210217.1848
;; Package-X-Original-Version: 1.5.4
;; Homepage: https://github.com/Alexander-Miller/cfrs

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
;;; Simple implementation of reading a string with child-frames.
;;; Synchronous control is maintained by using `recursive-edit'.  When finished
;;; the entered text is read from the input buffer and the child-frame is
;;; hidden.

;;; Code:

(require 's)
(require 'dash)
(require 'posframe)

(defgroup cfrs nil
  "Cfrs configuration options."
  :group 'cfrs
  :prefix "cfrs-")

(defcustom cfrs-frame-parameters nil
  "Alist of parameters for cfrs' child frames.
Can be used to override useful parameters like `internal-border-width' or
`background-color' for better frame visibility."
  :type '(alist :key-type symbol)
  :group 'cfrs)

(defface cfrs-border-color
  `((t :inherit internal-border))
  "The face for the border of the cfrs popup frame.
Only the `:background' part is used."
  :group 'cfrs)

;;;###autoload
(defun cfrs-read (prompt &optional initial-input)
  "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT."
  (if (not (or (display-graphic-p)
               (not (fboundp #'display-buffer-in-side-window))))
      (read-string prompt nil nil initial-input)
    (let* ((buffer (get-buffer-create " *Pos-Frame-Read*"))
           (border-color (face-attribute 'cfrs-border-color :background nil t))
           (cursor cursor-type)
           (frame (posframe-show
                   buffer
                   :min-height 1
                   :min-width (+ 40 (length prompt))
                   :internal-border-width 2
                   :internal-border-color border-color
                   :string ""
                   :override-parameters `(,@cfrs-frame-parameters
                                          (cursor-type . ,cursor)
                                          (no-accept-focus . nil)))))
      (with-selected-frame frame
        (x-focus-frame frame)
        (add-hook 'delete-frame-functions #'cfrs--on-frame-kill nil :local)
        (with-current-buffer buffer
          (setq-local cursor-type cursor)
          (cfrs-input-mode)
          (-each (overlays-in (point-min) (point-max)) #'delete-overlay)
          (erase-buffer)
          (-doto (make-overlay 1 2)
            (overlay-put 'before-string (propertize (concat " " prompt) 'face 'minibuffer-prompt))
            (overlay-put 'rear-nonsticky t)
            (overlay-put 'read-only t))
          (when initial-input
            (insert initial-input))
          (when (fboundp 'evil-insert-state)
            (evil-insert-state nil))
          (end-of-line)
          (recursive-edit)
          (cfrs--hide)
          (s-trim (buffer-string)))))))

(defun cfrs--hide ()
  "Hide the current cfrs frame."
  (when (eq major-mode 'cfrs-input-mode)
    (posframe-hide (current-buffer))
    (x-focus-frame (frame-parent (selected-frame)))))

(defun cfrs--adjust-height ()
  "Adjust input frame's height to the number of lines in the buffer."
  (set-frame-height (selected-frame) (count-lines (point-min) (point-max))))

(defun cfrs--on-frame-kill (frame)
  "Redirect focus after FRAME is killed."
  (-let [parent (or (frame-parent frame) (selected-frame))]
    (x-focus-frame parent)))

(defun cfrs-finish ()
  "Finish the cfrs read, returning the entered string."
  (interactive)
  ;; XXX: workaround for persp believing we are in a different frame
  ;; and need a new perspective when the recursive edit ends
  (set-frame-parameter (selected-frame) 'persp--recursive nil)
  (exit-recursive-edit))

(defun cfrs-cancel ()
  "Cancel the `cfrs-read' call and the function that called it."
  (interactive)
  (cfrs--hide)
  (abort-recursive-edit))

(defvar cfrs-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'cfrs-finish)
    (define-key map [return] #'cfrs-finish)
    (define-key map [remap keyboard-quit] #'cfrs-cancel)
    map))

(define-derived-mode cfrs-input-mode fundamental-mode "Child Frame Read String"
  "Simple mode for buffers displayed in cfrs's input frames."
  (add-hook 'post-command-hook #'cfrs--adjust-height nil :local)
  (display-line-numbers-mode -1))

;; https://github.com/Alexander-Miller/treemacs/issues/775
(with-eval-after-load 'beacon
  (add-to-list 'beacon-dont-blink-major-modes 'cfrs-input-mode))

(provide 'cfrs)

;;; cfrs.el ends here
