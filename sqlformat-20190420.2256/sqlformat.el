;;; sqlformat.el --- Reformat SQL using sqlformat or pgformatter  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages
;; Package-Version: 20190420.2256
;; URL: https://github.com/purcell/sqlformat
;; Package-Requires: ((emacs "24") (reformatter "0.3"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides commands and a minor mode for easily reformatting SQL
;; using external programs such as "sqlformat" and "pg_format".

;; Install the "sqlparse" (Python) package to get "sqlformat", or
;; "pgformatter" to get "pg_format".

;; Customise the `sqlformat-command' variable as desired, then call
;; `sqlformat', `sqlformat-buffer' or `sqlformat-region' as convenient.

;; Enable `sqlformat-on-save-mode' in SQL buffers like this:

;;     (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)

;; or locally to your project with a form in your .dir-locals.el like
;; this:

;;     ((sql-mode
;;       (mode . sqlformat-on-save)))

;; You might like to bind `sqlformat' or `sqlformat-buffer' to a key,
;; e.g. with:

;;     (define-key 'sql-mode-map (kbd "C-c C-f") 'sqlformat)

;;; Code:


;; Minor mode and customisation

(require 'reformatter)

(defgroup sqlformat nil
  "Reformat SQL using sqlformat or pgformatter."
  :group 'sql)

(defcustom sqlformat-command 'sqlformat
  "Command used for reformatting.
This command should receive SQL input via STDIN and output the
reformatted SQL to STDOUT, returning an appropriate exit code."
  :type '(choice (const :tag "Use \"sqlformat\"" sqlformat)
                 (const :tag "Use \"pgformatter\"" pgformatter)))

(defcustom sqlformat-args '()
  "List of args for reformatting command.
For example these options may be useful for sqlformat command: '(\"-k\" \"upper\")"
  :type '(repeat string))


;; Commands for reformatting

;;;###autoload (autoload 'sqlformat-buffer "sqlformat" nil t)
;;;###autoload (autoload 'sqlformat-region "sqlformat" nil t)
;;;###autoload (autoload 'sqlformat-on-save-mode "sqlformat" nil t)
(reformatter-define sqlformat
  :program (pcase sqlformat-command
             (`sqlformat "sqlformat")
             (`pgformatter "pg_format"))
  :args (pcase sqlformat-command
          (`sqlformat  (append sqlformat-args '("-r" "-")))
          (`pgformatter (append sqlformat-args '("-"))))
  :lighter " SQLFmt"
  :group 'sqlformat)

;;;###autoload
(defun sqlformat (beg end)
  "Reformat SQL in region from BEG to END using `sqlformat-region'.
If no region is active, the current statement (paragraph) is reformatted.
Install the \"sqlparse\" (Python) package to get \"sqlformat\", or
\"pgformatter\" to get \"pg_format\"."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (save-excursion
                (backward-paragraph)
                (skip-syntax-forward " >")
                (point))
          end (save-excursion
                (forward-paragraph)
                (skip-syntax-backward " >")
                (point))))
  (sqlformat-region beg end (called-interactively-p 'any)))


(provide 'sqlformat)
;;; sqlformat.el ends here
