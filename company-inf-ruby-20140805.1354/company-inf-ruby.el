;;; company-inf-ruby.el --- company-mode completion back-end for inf-ruby

;; Copyright (C) 2013-2014  Dmitry Gutov

;; Author: Dmitry Gutov <dgutov@yandex.ru>
;; Version: 0.3
;; Package-Version: 20140805.1354
;; URL: https://github.com/company-mode/company-inf-ruby
;; Package-Requires: ((company "0.6.10") (inf-ruby "2.2.7") (emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Uses inf-ruby's built-in completion mechanism.
;;

;;; Code:
(require 'company)
(eval-when-compile (require 'inf-ruby))

(defun company-inf-ruby (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `inf-ruby-mode'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-inf-ruby))
    (`prefix (and (eq major-mode 'inf-ruby-mode)
                  (inf-ruby-completion-expr-at-point)))
    (`candidates (and inf-ruby-at-top-level-prompt-p
                      (inf-ruby-completions arg)))))

(provide 'company-inf-ruby)
;;; company-inf-ruby.el ends here
