;;; ac-haskell-process.el --- Haskell auto-complete source which uses the current haskell process

;; Copyright (C) 2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages
;; Package-Version: 20150423.702
;; Version: DEV
;; Package-Requires: ((auto-complete "1.4") (haskell-mode "13"))

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

;; Provides an auto-complete source for Haskell which obtains its
;; completions from the current inferior haskell process (see the
;; `haskell-process' library included in `haskell-mode').

;;; Installation:

;; Available as a package in MELPA at http://melpa.org/
;; M-x package-install ac-haskell-process

;;; Usage:

;;     (require 'ac-haskell-process) ; if not installed via package.el
;;     (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
;;     (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'haskell-interactive-mode))

;; If you want to trigger auto-complete using TAB in REPL buffers, you may
;; want to use auto-complete in your `completion-at-point-functions':

;;     (defun set-auto-complete-as-completion-at-point-function ()
;;       (add-to-list 'completion-at-point-functions 'auto-complete))
;;     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;     (add-to-list 'ac-modes 'haskell-interactive-mode)
;;     (add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;     (add-hook 'haskell-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;; You can use `ac-haskell-process-popup-doc' to pop up documentation
;; for the symbol at point:
;;
;;     (eval-after-load 'haskell-mode
;;       '(define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))
;;
;; This will only work if Emacs can execute the "hoogle" command.

;;; Code:

(require 'auto-complete)
(require 'haskell)
(require 'haskell-process)


(defun ac-haskell-process-available-p ()
  "Return non-nil if completions are (or might later be) available from this source."
  (or (haskell-session-maybe)
      (memq major-mode '(haskell-mode haskell-interactive-mode))))

(defun ac-haskell-process-candidates ()
  "Return a list of completion candidates for the current `ac-prefix'."
  (when (haskell-session-maybe)
    (let* ((p1 (point))
           (pref (or (save-excursion
                       (beginning-of-line)
                       (when (looking-at "import[ \t]")
                         (buffer-substring (point) p1)))
                     ac-prefix)))
      (haskell-process-get-repl-completions (haskell-process) pref))))

(defun ac-haskell-process-doc (sym)
  "Return the docstring for SYM."
  (when (executable-find "hoogle")
    (shell-command-to-string
     (concat "hoogle --info " (shell-quote-argument sym)))))

;;;###autoload
(defconst ac-source-haskell-process
  '((available . ac-haskell-process-available-p)
    (candidates . ac-haskell-process-candidates)
    (document . ac-haskell-process-doc)
    (symbol . "h"))
  "Haskell auto-complete source which uses the current haskell process.")

;;;###autoload
(defun ac-haskell-process-setup ()
  "Add the haskell process completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-haskell-process))

(defun ac-haskell-process-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (car (bounds-of-thing-at-point 'symbol)))))

;;;###autoload
(defun ac-haskell-process-popup-doc ()
  "Show documentation for the symbol at point in a popup."
  (interactive)
  (let ((doc (ac-haskell-process-doc (symbol-name (symbol-at-point)))))
    (when doc
     (popup-tip doc
                :point (ac-haskell-process-symbol-start-pos)
                :around t
                :scroll-bar t
                :margin t))))



(provide 'ac-haskell-process)
;;; ac-haskell-process.el ends here
