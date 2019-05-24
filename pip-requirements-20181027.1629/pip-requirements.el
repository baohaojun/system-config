;;; pip-requirements.el --- A major mode for editing pip requirements files.

;; Copyright (C) 2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 September 2014
;; Version: 0.6
;; Package-Version: 20181027.1629
;; Package-Requires: ((dash "2.8.0"))

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a major mode for editing pip requirements files, with the following features:

;; * Syntax highlighting
;; * Togglable comments
;; * Auto completion of package names from PyPI

;; TODO: Steal shamelessly all the fantasic ideas in
;; https://github.com/wuub/requirementstxt

;;; Code:

(require 'dash)
(require 'cl-lib)

(defgroup pip-requirements nil
  "Requirements files for pip"
  :prefix "pip-requirements-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/Wilfred/pip-requirements.el"))

(defcustom pip-requirements-mode-hook nil
  "Hook to run after `pip-requirements-mode'."
  :group 'pip-requirements
  :type 'hook
  :risky t)

(defcustom pip-requirements-index-url
  "https://pypi.org/simple/"
  "The URL used to fetch the list of packages used for completion."
  :group 'pip-requirements
  :type 'string)

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(rx ".pip" string-end) . pip-requirements-mode))
;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(rx "requirements" (zero-or-more anything) ".txt" string-end) . pip-requirements-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(rx "requirements.in") . pip-requirements-mode))

(defconst pip-requirements-name-regex
  (rx
   line-start
   (group (1+ (or alphanumeric "-" "_" ".")))))

(defconst pip-requirements-version-regex
  ;; https://www.python.org/dev/peps/pep-0440/#version-specifiers
  (rx
   (group (or "==" ">" ">=" "<" "<=" "!=" "~="))
   (group (1+ (or digit "b" "." "post" "*")))))

(defconst pip-requirements-arbitrary-version-regex
  ;; https://www.python.org/dev/peps/pep-0440/#arbitrary-equality
  (rx (group "===") (group (1+ not-newline))))

(defconst pip-requirements-operators
  (list
   (list pip-requirements-name-regex 1 'font-lock-variable-name-face)
   (list pip-requirements-version-regex 1 'font-lock-builtin-face)
   (list pip-requirements-arbitrary-version-regex 1 'font-lock-builtin-face)
   (list pip-requirements-version-regex 2 'font-lock-constant-face)
   (list pip-requirements-arbitrary-version-regex 2 'font-lock-constant-face)))

(defconst pip-requirements-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?~ "." table)
    table))

(defvar pip-http-buffer nil)
(defvar pip-packages nil
  "List of PyPI packages for completion.")

(defun pip-requirements-callback (&rest _)
  (with-current-buffer pip-http-buffer
    ;; Move over the HTTP header.
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)

    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (body-tag (-last-item dom))
           (body-children (cdddr body-tag))
           (a-tags (--filter (eq (car-safe it) 'a) body-children)))
      (setq pip-packages
            ;; Inner text of anchor tags.
            (-map #'cl-third a-tags))))
  (kill-buffer pip-http-buffer))

(defun pip-requirements-fetch-packages ()
  "Get a list of all packages available on PyPI and store them in `pip-packages'.
Assumes Emacs is compiled with libxml."
  (setq pip-http-buffer
        (url-retrieve pip-requirements-index-url
                      #'pip-requirements-callback nil t)))

(defun pip-requirements-complete-at-point ()
  "Complete at point in Pip Requirements Mode."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point))))
    (list start end pip-packages)))

;; Declare variables from AC, to avoid a hard dependency on Auto Complete.
(defvar ac-modes)
(defvar ac-sources)

;;;###autoload
(defun pip-requirements-auto-complete-setup ()
  "Setup Auto-Complete for Pip Requirements.

See URL `https://github.com/auto-complete/auto-complete' for
information about Auto Complete."
  (add-to-list 'ac-modes 'pip-requirements-mode)
  (add-to-list 'ac-sources '((candidates . pip-packages)))
  (when (and (fboundp 'auto-complete-mode)
             (not (bound-and-true-p auto-complete-mode)))
    ;; Enable Auto Complete
    (auto-complete-mode)))

(custom-add-frequent-value 'pip-requirements-mode-hook
                           'pip-requirements-auto-complete-setup)

;;;###autoload
(define-derived-mode pip-requirements-mode prog-mode "pip-require"
  "Major mode for editing pip requirements files."
  :syntax-table pip-requirements-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(pip-requirements-operators))
  (set (make-local-variable 'comment-start) "#")
  (add-hook 'completion-at-point-functions
            #'pip-requirements-complete-at-point nil 'local)
  (unless pip-packages
    ;; Fetch the list of packages for completion
    (pip-requirements-fetch-packages)))

(provide 'pip-requirements)
;;; pip-requirements.el ends here
