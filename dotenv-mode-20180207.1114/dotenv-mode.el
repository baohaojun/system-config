;;; dotenv-mode.el --- Major mode for .env files -*- lexical-binding: t; -*-

;; Author: Preetpal S. Sohal
;; URL: https://github.com/preetpalS/emacs-dotenv-mode
;; Package-Version: 20180207.1114
;; Version: 0.2.4
;; Package-Requires: ((emacs "24.3"))
;; License: GNU General Public License Version 3

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
;;
;; Major mode for editing .env files (which are used for storing
;; environment variables).
;;
;; Usage:
;;
;; (require 'dotenv-mode) ; unless installed from a package
;; (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode

;;; Code:

(defgroup dotenv ()
  "Major mode for editing .env files."
  :group 'languages
  :prefix "dotenv-")

(defvar dotenv-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"'" table) ; ?' is a string delimiter
    (modify-syntax-entry ?\" "\"" table) ; ?\" is a string delimiter
    (modify-syntax-entry ?# "<" table)   ; ?# starts comments
    (modify-syntax-entry ?\n ">" table)  ; ?\n ends comments
    (modify-syntax-entry ?_ "_" table)   ; ?_ can be used in variable and command names
    (modify-syntax-entry ?\\ "\\" table) ; ?\\ is an escape sequence character
    (modify-syntax-entry ?$ "'" table)   ; ?$ is an expression prefix; Used in highlighting $VARIABLES, ${SUBSTITUTED_VARIABLES}, and $(substituted commands) embedded in double-quoted strings
    table))

;; Adapted from code generously donated by Fuco1 (https://github.com/Fuco1; see: https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html)
(defun dotenv-mode--match-variables-in-double-quotes (limit)
  "Match variables in double-quotes in `dotenv-mode'."
  (with-syntax-table dotenv-mode-syntax-table
    (catch 'done
      (while (re-search-forward
              ;; `rx' is cool, mkay.
              (rx (or line-start
                      (not (any "\\")))
                  (group "$")
                  (group
                   (or (and "{" (+? nonl) "}")
                       (and "(" (+? nonl) ")") ;; Added to support for interpolated command substitution syntax (like: "$(shell command)")
                       (and (+ (any alnum "_")))
                       (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
              limit t)
        (let ((string-syntax (nth 3 (syntax-ppss))))
          (when (and string-syntax (= string-syntax 34))
            (throw 'done (point))))))))

(defvar dotenv-mode-keywords
  '(("\\(export\\)[[:space:]]+" . 1)
    ;; Adapted from code generously donated by Fuco1 (https://github.com/Fuco1; see: https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html)
    (dotenv-mode--match-variables-in-double-quotes (1 'default t)
                                                   (2 font-lock-variable-name-face t))
    ("\\([[:alpha:]_]+[[:alnum:]_]*\\)[=]" 1 font-lock-variable-name-face)
    ("^\\([[:alpha:]_]+[[:alnum:]_]*\\)[:=]" 1 font-lock-variable-name-face)
    ("\$[[:alpha:]]+[[:alnum:]_]*" . font-lock-variable-name-face)
    ("\${[[:alpha:]]+[[:alnum:]_]*}" . font-lock-variable-name-face)
    ("\$([[:alpha:]]+[[:alnum:]_]*)" . font-lock-variable-name-face)))

;;;###autoload
(define-derived-mode dotenv-mode prog-mode ".env"
  "Major mode for `.env' files."
  :abbrev-table nil
  :syntax-table dotenv-mode-syntax-table
  (setq-local font-lock-defaults '((dotenv-mode-keywords))))

;;;###autoload
(mapc (lambda (s) (add-to-list 'auto-mode-alist `(,s . dotenv-mode)))
      '(
        "\\.env\\'"
        "\\.env\\.example\\'"
        ))

(provide 'dotenv-mode)

;;; dotenv-mode.el ends here
