;;; consult-icomplete.el --- Icomplete integration for Consult -*- lexical-binding: t -*-

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

;; Integration code for the Icomplete completion system. This package
;; is automatically loaded by Consult.

;;; Code:

(require 'consult)
(require 'icomplete)

(defun consult-icomplete--refresh ()
  "Refresh icomplete view, keep current candidate selected if possible."
  (when icomplete-mode
    (let ((top (car completion-all-sorted-completions)))
      (completion--flush-all-sorted-completions)
      ;; force flushing, otherwise narrowing is broken!
      (setq completion-all-sorted-completions nil)
      (when top
        (let* ((completions (completion-all-sorted-completions))
               (last (last completions))
               (before)) ;; completions before top
          ;; warning: completions is an improper list
          (while (consp completions)
            (if (equal (car completions) top)
                (progn
                  (setcdr last (append (nreverse before) (cdr last)))
                  (setq completion-all-sorted-completions completions
                        completions nil))
              (push (car completions) before)
              (setq completions (cdr completions)))))))
    (icomplete-exhibit)))

(add-hook 'consult--completion-refresh-hook #'consult-icomplete--refresh)

(provide 'consult-icomplete)
;;; consult-icomplete.el ends here
