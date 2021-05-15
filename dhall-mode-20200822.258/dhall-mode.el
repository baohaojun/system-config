;;; dhall-mode.el --- Major mode for the dhall configuration language -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sibi Prabakaran

;; Author: Sibi Prabakaran <sibi@psibi.in>
;; Maintainer: Sibi Prabakaran <sibi@psibi.in>
;; Keywords: languages
;; Package-Version: 20200822.258
;; Package-Commit: ad259c8a2292fb398dff1ce7d25c686edb02945d
;; Version: 0.1.3
;; Package-Requires: ((emacs "24.4") (reformatter "0.3"))
;; URL: https://github.com/psibi/dhall-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Dhall configuration file (See
;; https://github.com/dhall-lang/dhall-lang to learn more) in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;
;;  - Basic indentation, multi line string support
;;
;;  - Automatic formatting on save (configurable)
;;
;;  - Error highlighting
;;
;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'reformatter)

(defvar dhall-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'dhall-repl-show)
    (define-key map (kbd "C-c C-f") 'dhall-format-buffer)
    (define-key map (kbd "C-c C-t") 'dhall-buffer-type-show)
    map)
  "Keymap for using `dhall-mode'.")

(defgroup dhall nil
  "Major mode for editing dhall files"
  :group 'languages
  :prefix "dhall-"
  :link '(url-link :tag "Site" "https://github.com/psibi/dhall-mode")
  :link '(url-link :tag "Repository" "https://github.com/psibi/dhall-mode"))

;; Create the syntax table for this mode.
(defvar dhall-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Taken from haskell-mode: https://stackoverflow.com/a/20845468/1651941
    (modify-syntax-entry ?\{  "(}1nb" st)
    (modify-syntax-entry ?\}  "){4nb" st)
    (modify-syntax-entry ?-  ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    ;; Let us handle escapes and string
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\" "." st)
    ;; End
    st)
  "Syntax table used while in `dhall-mode'.")

;; define several category of keywords
(defvar dhall-mode-keywords (regexp-opt '("if" "then" "else" "let" "in" "using" "as") 'symbols))

(defvar dhall-mode-types
  (regexp-opt '("Optional" "Bool" "Natural" "Integer" "Double" "Text" "List" "Type") 'symbols))

(defconst dhall-mode-constants (regexp-opt '("True" "False") 'symbols))
(defconst dhall-mode-numerals "\\_<[+\\-][1-9]+\\_>")
(defconst dhall-mode-doubles "\\_<[+\\-]?[0-9]+\.[0-9]+\\_>")
(defconst dhall-mode-operators (regexp-opt '("->" "\\[" "]" "," "++" "#" ":" "=" "==" "!=" "\\\\\(" "λ" "⫽" ")" "&&" "||" "{" "}" "(")))
(defconst dhall-mode-variables "\\([a-zA-Z_][a-zA-Z_0-9\\-]*\\)[[:space:]]*=")
(defconst dhall-mode-urls "\\_<\\(?:https?\\|file\\):[^[:space:]]+")
(defconst dhall-mode-shas "\\_<sha256:[a-f0-9]+\\_>")

(defconst dhall-mode-font-lock-keywords
  `( ;; Variables
    (,dhall-mode-urls . font-lock-function-name-face)
    (,dhall-mode-shas . font-lock-constant-face)
    (,dhall-mode-types . font-lock-type-face)
    (,dhall-mode-constants . font-lock-constant-face)
    (,dhall-mode-operators . font-lock-builtin-face)
    (,dhall-mode-variables . (1 font-lock-variable-name-face))
    (,dhall-mode-keywords . font-lock-keyword-face)
    (,dhall-mode-doubles . font-lock-constant-face)
    (,dhall-mode-numerals . font-lock-constant-face)))

(defcustom dhall-command "dhall"
  "Command used to normalize Dhall files.
Should be dhall or the complete path to your dhall executable,
  e.g.: /home/sibi/.local/bin/dhall"
  :type 'file
  :group 'dhall
  :safe 'stringp)

(defcustom dhall-use-header-line t
  "If non-nil, display the type of the file in the window's header line."
  :type 'boolean
  :group 'dhall
  :safe 'booleanp)

(defcustom dhall-format-command nil
  "Command used to format Dhall files.
If your dhall command is old and does not support the \"format\" sub-command,
then set this to \"dhall-format\".

If specified, this should be the complete path to your dhall-format executable,
  e.g.: /home/sibi/.local/bin/dhall-format"
  :type 'file
  :group 'dhall
  :safe 'stringp)

(defcustom dhall-format-at-save t
  "If non-nil, the Dhall buffers will be formatted after each save."
  :type 'boolean
  :group 'dhall
  :safe 'booleanp)

(defcustom dhall-format-arguments nil
  "Provide a list of arguments for the formatter e.g. '(\"--ascii\")."
  :type 'list
  :group 'dhall
  :safe 'listp)

(defcustom dhall-type-check-inactivity-timeout 1
  "Wait for this period of inactivity before refreshing the buffer type.
You can try increasing this if type checking is slowing things
down.  You can also disable type-checking entirely by setting
`dhall-use-header-line' to nil."
  :type 'number
  :group 'dhall
  :safe 'numberp)

(defun dhall-buffer-type ()
  "Return the type of the expression in the current buffer."
  ;; We resolve dhall-command in the current buffer, in case
  ;; dhall-command, exec-path or process-environment is local
  ;; there, so that we can propagate it to the temp buffer.
  (let ((cmd (executable-find dhall-command)))
    (when cmd
      (let ((errbuf (get-buffer-create "*dhall-buffer-type-errors*"))
            (source (buffer-string)))
        (with-temp-buffer
          (with-current-buffer errbuf
            (read-only-mode -1)
            (erase-buffer))
          (insert source)
          (if (zerop (shell-command-on-region (point-min)
                                              (point-max)
                                              (concat cmd " resolve|" cmd " type")
                                              nil t errbuf))
              (replace-regexp-in-string "\\(?:\\` \\| \\'\\)" ""
                                        (replace-regexp-in-string "[[:space:]]+" " " (buffer-string)))
            (prog1
                nil
              (with-current-buffer errbuf
                (ansi-color-apply-on-region (point-min) (point-max))
                (view-mode)))))))))

(reformatter-define dhall-format
  :program (or dhall-format-command dhall-command)
  :args (append (unless dhall-format-command '("format")) dhall-format-arguments)
  :group 'dhall
  :lighter " DhFmt")

(reformatter-define dhall-freeze
  :program dhall-command
  :args '("freeze")
  :group 'dhall
  :lighter " DhFreeze")

(reformatter-define dhall-lint
  :program dhall-command
  :args '("lint")
  :group 'dhall
  :lighter " DhLint")

(defun dhall--get-string-type (parse-state)
  "Get the type of string based on PARSE-STATE."
  (let ((string-start (nth 8 parse-state)))
    (and string-start (get-text-property string-start 'dhall-string-type))))

(defun dhall--mark-string (pos string-type)
  "Mark string as a Dhall string.

POS position of start of string
STRING-TYPE type of string based off of Emacs syntax table types"
  (put-text-property pos (1+ pos)
                     'syntax-table (string-to-syntax "|"))
  (put-text-property pos (1+ pos)
                     'dhall-string-type string-type))

(defun dhall--double-quotes ()
  "Handle Dhall double quotes."
  (let* ((pos (match-beginning 0)))
    (dhall--mark-string pos ?\")))

(defun dhall--single-quotes ()
  "Handle Dhall single quotes."
  (let* ((pos (match-beginning 0)))
    (dhall--mark-string pos ?\")))

(defun dhall-syntax-propertize (start end)
  "Special syntax properties for Dhall from START to END."
  (goto-char start)
  (remove-text-properties start end '(syntax-table nil dhall-string-type nil))
  (funcall
   (syntax-propertize-rules
    ("'\\{2,\\}"
     (0 (ignore (dhall--single-quotes))))
    ("\""
     (0 (ignore (dhall--double-quotes)))))
   start end))

(defvar-local dhall-buffer-type nil)
(defvar-local dhall-buffer-type-compute-timer nil)

(defun dhall-buffer-type-compute (buffer)
  "Recompute variable `dhall-buffer-type' in BUFFER."
  (with-current-buffer buffer
    (let ((type (dhall-buffer-type)))
      (setq dhall-buffer-type
            (if type
                (if (<= (length type) (window-width))
                    type
                  (concat
                   (substring type 0
                              (- (window-width) 10))
                   "..."))
              (propertize "Error determining type. See *dhall-buffer-type-errors*" 'face 'error))))))

(defun dhall-after-change (&optional _beg _end _length)
  "Called after any change in the buffer."
  (when dhall-use-header-line
    (when dhall-buffer-type-compute-timer
      (cancel-timer dhall-buffer-type-compute-timer))
    (setq dhall-buffer-type-compute-timer
          (run-at-time dhall-type-check-inactivity-timeout
                       nil
                       (apply-partially 'dhall-buffer-type-compute (current-buffer))))))

;; The main mode functions
;;;###autoload
(define-derived-mode dhall-mode prog-mode
  "Dhall"
  "Major mode for editing Dhall files."
  :group 'dhall
  :keymap dhall-mode-map
  :syntax-table dhall-mode-syntax-table
  (when dhall-use-header-line
    (setq header-line-format
          '((:eval dhall-buffer-type)))
    (dhall-after-change))
  (setq font-lock-defaults '(dhall-mode-font-lock-keywords))
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  ;; Use only spaces for indentation, for consistency with "dhall format"
  (setq-local indent-tabs-mode nil)
  ;; Special syntax properties for Dhall
  (setq-local syntax-propertize-function 'dhall-syntax-propertize)
  (add-hook 'after-change-functions 'dhall-after-change nil t)
  (when dhall-format-at-save
    (dhall-format-on-save-mode)))

;; Automatically use dhall-mode for .dhall files.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))


;; REPL
(defconst dhall-prompt-regexp "⊢ ")

(define-derived-mode dhall-repl-mode comint-mode "Dhall-REPL"
  "Interactive prompt for Dhall."
  (setq-local comint-prompt-regexp dhall-prompt-regexp)
  (setq-local comint-prompt-read-only t))

(defun dhall-repl-show ()
  "Load the Dhall-REPL."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create "*Dhall-REPL*"))
  (unless (comint-check-proc (current-buffer))
    (dhall--make-repl-in-buffer (current-buffer))
    (dhall-repl-mode)))

(defun dhall--make-repl-in-buffer (buffer)
  "Make Dhall Repl in BUFFER."
  (make-comint-in-buffer "Dhall-REPL" buffer dhall-command nil "repl"))

(defun dhall-buffer-type-show ()
  "Show the type of the current buffer's dhall expression in the minibuffer."
  (interactive)
  (let ((type (dhall-buffer-type)))
    (if type
        (message type)
      (user-error "Error determining type.  See *dhall-buffer-type-errors*"))))


;; Provide ourselves:
(provide 'dhall-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; dhall-mode.el ends here
