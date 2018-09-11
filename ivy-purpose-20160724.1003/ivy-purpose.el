;;; ivy-purpose.el --- Ivy Interface for Purpose -*- lexical-binding: t -*-

;; Copyright (C) 2016 Bar Magal

;; Author: Bar Magal (2016)
;; Package: ivy-purpose
;; Version: 0.1
;; Package-Version: 20160724.1003
;; Homepage: https://github.com/bmag/ivy-purpose
;; Package-Requires: ((emacs "24") (ivy "0.8") (window-purpose "1.5"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; Provide Ivy commands for Purpose.
;; Features:
;; - make Purpose use Ivy interface when `ivy-mode' is on
;; - special ivy commands:
;;   + `ivy-purpose-switch-buffer-with-purpose': same as `ivy-switch-buffer',
;;     but only list buffers with a specific purpose (default: same purpose as
;;     current buffer).
;;   + `ivy-purpose-switch-buffer-with-some-purpose': choose a purpose, then
;;     call `ivy-purpose-switch-buffer-with-purpose'.
;;   + `ivy-purpose-switch-buffer-without-purpose': same as `ivy-switch-buffer',
;;     but ignore Purpose when displaying the buffer.
;;
;; Setup:
;; Call `ivy-purpose-setup' in your init file.  It will "Ivy-fy" Purpose
;; commands, but won't change any key bindings.
;; Alternatively, you can set `purpose-preferred-prompt' to `vanilla' instead.
;;
;; Key Bindings:
;; `ivy-purpose' doesn't bind any keys, you are free to define your own
;; bindings as you please.

;;; Code:

(require 'ivy)
(require 'window-purpose)

;;;###autoload
(defun ivy-purpose-switch-buffer-with-purpose (&optional purpose)
  "Switch to buffer, choose from buffers with purpose PURPOSE.
PURPOSE defaults to the purpose of the current buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (current-purpose (or purpose
                              (purpose-buffer-purpose current-buffer))))
    (ivy-read "Buffer: "
              (mapcar #'buffer-name
                      (delq current-buffer
                            (purpose-buffers-with-purpose current-purpose)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller #'ivy-switch-buffer)))

;;;###autoload
(defun ivy-purpose-switch-buffer-with-some-purpose ()
  "Choose a purpose, then switch to a buffer with that purpose."
  (interactive)
  (ivy-purpose-switch-buffer-with-purpose
   (purpose-read-purpose "Purpose: "
                         ;; don't show purposes that have no buffers
                         (cl-delete-if-not #'purpose-buffers-with-purpose
                                           (purpose-get-all-purposes))
                         t)))

;;;###autoload
(defun ivy-purpose-switch-buffer-without-purpose ()
  "Same as `ivy-switch-buffer', but disable window-purpose while the
command executes."
  (interactive)
  (without-purpose (ivy-switch-buffer)))

;;;###autoload
(defun ivy-purpose-setup ()
  "Setup Ivy interface for Purpose.
Currently just sets `purpose-preferred-prompt' to 'vanilla.
Doesn't bind any keys."
  (setq purpose-preferred-prompt 'vanilla))

(provide 'ivy-purpose)

;;; ivy-purpose.el ends here
