;;; ac-inf-ruby.el --- Enable auto-complete in inf-ruby sessions

;; Copyright (C) 2013  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages, tools
;; Version: 20131115.350
;; X-Original-Version: DEV
;; Package-Requires: ((inf-ruby "2.3.2") (auto-complete "1.4"))

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

;; Provides an `auto-complete' source for use in `inf-ruby-mode' buffers,
;; which ties directly into the accurate inf-ruby completions mechanism.

;; Enable using:

;;     (require 'ac-inf-ruby) ;; when not installed via package.el
;;     (eval-after-load 'auto-complete
;;       '(add-to-list 'ac-modes 'inf-ruby-mode))
;;     (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; Optionally bind auto-complete to TAB in inf-ruby buffers:
;;     (eval-after-load 'inf-ruby '
;;       '(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))

;;; Code:

(require 'inf-ruby)
(require 'auto-complete)

(defun ac-inf-ruby-candidates ()
  "Return completion candidates for `ac-prefix'."
  (inf-ruby-completions ac-prefix))

(defun ac-inf-ruby-prefix ()
  "Return starting position of completion prefix."
  (and inf-ruby-at-top-level-prompt-p
       (car (inf-ruby-completion-bounds-of-expr-at-point))))

(defun ac-inf-ruby-available ()
  "Return t if inf-ruby completions are available, otherwise nil."
  (eq 'inf-ruby-mode major-mode))

(defvar ac-source-inf-ruby
  '((available . ac-inf-ruby-available)
    (candidates . ac-inf-ruby-candidates)
    (symbol . "r")
    (prefix . ac-inf-ruby-prefix))
  "Auto-complete source for `inf-ruby-mode'.")

;;;###autoload
(defun ac-inf-ruby-enable ()
  "Add `ac-source-inf-ruby' to `ac-sources' for this buffer."
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-inf-ruby))


(provide 'ac-inf-ruby)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ac-inf-ruby.el ends here
