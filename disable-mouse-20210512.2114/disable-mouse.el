;;; disable-mouse.el --- Disable mouse commands globally  -*- lexical-binding: t -*-

;; Copyright (C) 2016  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/disable-mouse
;; Package-Commit: cae3be9dd012727b40ad3b511731191f79cebe42
;; Package-Version: 20210512.2114
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: mouse

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

;; Provides `disable-mouse-mode' and `global-disable-mouse-mode', pair
;; of minor modes which suppress all mouse events by intercepting them
;; and running a customisable handler command (`ignore' by default).

;;; Code:

(defgroup disable-mouse nil
  "Disable mouse commands globally."
  :prefix "disable-mouse-"
  :group 'mouse)

(defcustom disable-mouse-command 'ignore
  "The command to run when a mouse action is attempted."
  :group 'disable-mouse
  :type 'function)

(defcustom disable-mouse-mode-lighter " NoMouse"
  "Mode-line lighter for `disable-mouse-mode'."
  :group 'disable-mouse
  :type 'string)

(defcustom disable-mouse-mode-global-lighter " NoMouse!"
  "Mode-line lighter for `global-disable-mouse-mode'."
  :group 'disable-mouse
  :type 'string)

(defconst disable-mouse--bindings-modifier-combos
  '("C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "M-C-S-"))

(defconst disable-mouse--bindings-targets '("mode-line" "bottom-divider" "vertical-line"))

(defconst disable-mouse--multipliers '("double" "triple"))

(defconst disable-mouse--button-numbers '(1 2 3 4 5))

(defconst disable-mouse--button-events '("mouse" "up-mouse" "down-mouse" "drag-mouse"))

(defvar disable-mouse-wheel-events '("wheel-up" "wheel-down" "wheel-left" "wheel-right")
  "Mouse wheel event base names.
Before `disable-mouse' is loaded, you can set this to nil if you
do not want to disable mouse wheel events.")

(defconst disable-mouse-button-bindings
  (apply 'append
         (mapcar (lambda (n)
                   (mapcar (lambda (e) (format "%s-%d" e n))
                           disable-mouse--button-events))
                 disable-mouse--button-numbers)))

(defvar disable-mouse-bindings
  (append disable-mouse-button-bindings disable-mouse-wheel-events)
  "Root names for mouse events to be disabled.")

(defun disable-mouse--all-bindings (include-targets)
  "Return an extensive list of mouse-related keybindings.
When INCLUDE-TARGETS is non-nil, also return bindings that target
the elements in `disable-mouse--bindings-targets'."
  (let ((bindings))
    (dolist (target (append '(nil)
                            (when include-targets
                              disable-mouse--bindings-targets)))
      (dolist (mod (append '(nil) disable-mouse--bindings-modifier-combos))
        (dolist (mult (append '(nil) disable-mouse--multipliers))
          (dolist (binding disable-mouse-bindings)
            (push (read-kbd-macro
                   (concat (when target (concat "<" target "> "))
                           mod
                           "<"
                           (when mult (concat mult "-"))
                           binding
                           ">"))
                  bindings)))))
    bindings))

(defun disable-mouse--handle ()
  "Handle when a disabled mouse event is fired."
  (interactive)
  (call-interactively disable-mouse-command))

;;;###autoload
(defun disable-mouse-in-keymap (map &optional include-targets)
  "Rebind all mouse commands in MAP so that they are disabled.
When INCLUDE-TARGETS is non-nil, also disable mouse actions that
target GUI elements such as the modeline."
  (dolist (binding (disable-mouse--all-bindings include-targets))
    (define-key map binding 'disable-mouse--handle)))

(defvar disable-mouse-mode-map (make-sparse-keymap)
  "Map containing no-op bindings for all mouse events.")

(defvar disable-mouse-global-mode-map (make-sparse-keymap)
  "Map containing no-op bindings for all mouse events.")

(disable-mouse-in-keymap disable-mouse-mode-map)
(disable-mouse-in-keymap disable-mouse-global-mode-map t)

;;;###autoload
(define-minor-mode disable-mouse-mode
  "Disable the mouse in the current buffer.
You can still use the mouse to click into other buffers or
interact with GUI elements such as divider lines."
  :lighter disable-mouse-mode-lighter
  (if disable-mouse-mode
      (set (make-local-variable 'mouse-highlight) nil)
    (kill-local-variable 'mouse-highlight)))

;;;###autoload
(define-minor-mode disable-mouse-global-mode
  "Disable the mouse globally.
Interact with GUI elements such as divider lines will also be prevented."
  :require 'disable-mouse
  :lighter disable-mouse-mode-global-lighter
  :global t)

;;;###autoload
(defalias 'global-disable-mouse-mode 'disable-mouse-global-mode)

(provide 'disable-mouse)
;;; disable-mouse.el ends here
