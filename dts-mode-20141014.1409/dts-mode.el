;;; dts-mode.el --- Major mode for Devicetree source code

;; Copyright (C) 2014  Ben Gamari

;; Version: 0.1.0
;; Package-Version: 20141014.1409
;; Author: Ben Gamari <ben@smart-cactus.org>
;; Keywords: languages

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

;;; Code:

(defconst dts-re-ident "\\([[:word:]_][[:word:][:multibyte:]_,[:digit:]-]*\\)")

(defvar dts-mode-font-lock-keywords
  `(
    ;; names like `name: hi {`
    (,(concat dts-re-ident ":") 1 font-lock-variable-name-face)
    ;; nodes
    (,(concat dts-re-ident "\\(@[[:xdigit:]]+\\)?[[:space:]]*{") 1 font-lock-type-face)
    ;; assignments
    (,(concat dts-re-ident "[[:space:]]*=") 1 font-lock-variable-name-face)
    (,(concat dts-re-ident "[[:space:]]*;") 1 font-lock-variable-name-face)
    ;; references
    (,(concat "\\&" dts-re-ident) 1 font-lock-variable-name-face)
    )
  )

(defvar dts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?<  "(" table)
    (modify-syntax-entry ?>  ")" table)

    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?~  "." table)

    ;; _ and , are both word characters
    (modify-syntax-entry ?,  "_" table)
    (modify-syntax-entry ?_  "w" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defun dts--calculate-indentation ()
  (interactive)
  (save-excursion
    (let ((end (point-at-eol))
          (cnt 0)
          (initial-point (point)))
      (goto-char 0)
      (while (re-search-forward "\\([{}]\\)" end t)
        (if (string= (match-string-no-properties 0) "{")
            (setq cnt (1+ cnt))
          (setq cnt (1- cnt))))
      ;; subtract one if the current line has an opening brace since we
      ;; shouldn't add the indentation level until the following line
      (goto-char initial-point)
      (beginning-of-line)
      (when (re-search-forward "{" (point-at-eol) t)
        (setq cnt (1- cnt)))
      cnt)))

(defun dts-indent-line ()
  (interactive)
  (let ((indent (dts--calculate-indentation)))
    (indent-line-to (* indent tab-width))))

(defalias 'dts-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode dts-mode dts-parent-mode "Devicetree"
  "Major mode for editing Devicetrees"
  :group 'dts-mode
  :syntax-table dts-mode-syntax-table

  ;; Fonts
  (set (make-local-variable 'font-lock-defaults) '(dts-mode-font-lock-keywords nil nil nil nil))

  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end)   " */")
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'indent-line-function) 'dts-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dts\\'" . dts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dtsi\\'" . dts-mode))

(provide 'dts-mode)
;;; dts-mode.el ends here
