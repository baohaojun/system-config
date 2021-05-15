;;; purescript-presentation-mode.el --- Presenting PureScript things

;; Copyright (C) 2013  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'purescript-mode)

(define-derived-mode purescript-presentation-mode
  purescript-mode "Presentation"
  "Major mode for viewing PureScript snippets.
          \\{hypertext-mode-map}"
  (setq case-fold-search nil))

(define-key purescript-presentation-mode-map (kbd "q") 'quit-window)

(defun purescript-present (name session code)
  "Present CODE in a popup buffer suffixed with NAME and set
SESSION as the current purescript-session."
  (let* ((name (format "*PureScript Presentation%s*" name))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (purescript-presentation-mode)
      (if (boundp 'shm-display-quarantine)
          (set (make-local-variable 'shm-display-quarantine) nil))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (propertize "-- Hit `q' to close this window.\n\n"
                            'face
                            'font-lock-comment-face))
        (let ((point (point)))
          (insert code "\n\n")
          (font-lock-fontify-region point (point))
          (goto-char point))))
    (if (and (boundp 'purescript-presentation-mode)
             purescript-presentation-mode)
        (switch-to-buffer buffer)
      (pop-to-buffer buffer))))

(provide 'purescript-presentation-mode)

;;; purescript-presentation-mode.el ends here
