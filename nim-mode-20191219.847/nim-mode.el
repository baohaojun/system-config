;;; nim-mode.el --- A major mode for the Nim programming language -*- lexical-binding: t -*-
;;
;; Filename: nim-mode.el
;; Description: A major mode for the Nim programming language
;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Version: 0.4.2
;; Keywords: nim languages
;; Compatibility: GNU Emacs 24.4
;; Package-Requires: ((emacs "24.4") (epc "0.1.1") (let-alist "1.0.1") (commenter "0.5.1") (flycheck-nimsuggest "0.8.1"))

;; Taken over from James H. Fisher <jameshfisher@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;; Abstraction:
;; Large parts of this code is shamelessly stolen from python.el and
;; adapted to Nim.
;;
;; This package provide a major-mode future (syntax highlight and
;; indentation) and some necessity futures (jump-to-definition,
;; linting, el-doc) if you install nimsuggest.  See below for the detail.
;;
;;; Commentary:
;; ## TL;DR

;; For regular Emacs users, all you need is below configuration in your
;; dot Emacs after you installed nim-mode from MELPA and nimsuggest
;; which you can make by `./koch tools` or `./koch nimsuggest`command in
;; the Nim repository (or check the official document on Nim website if
;; this information was outdated):
;;
;; Activate nimsuggest dedicated mode on `nim-mode':
;;
;;   (add-hook 'nim-mode-hook 'nimsuggest-mode)
;;
;; Below configs are can be optional
;;
;; The `nimsuggest-path' will be set the value of (executable-find "nimsuggest")
;; automatically.
;;
;;   (setq nimsuggest-path "path/to/nimsuggest")
;;
;; You may need to install below packages if you haven't installed yet.
;;
;; -- Auto completion --
;; You can omit if you configured company-mode on 'prog-mode-hook
;;   (add-hook 'nimsuggest-mode-hook 'company-mode)  ; auto complete package
;;
;; -- Auto lint --
;; You can omit if you configured flycheck-mode on 'prog-mode-hook
;;
;;   (add-hook 'nimsuggest-mode-hook 'flycheck-mode) ; auto linter package
;;
;; See more information at https://github.com/nim-lang/nim-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; To update some autoloads on local for `el-get'
;; (el-get-update-autoloads 'nim-mode)

(require 'cl-lib)

;; Order of loading
(require 'nim-vars)
(require 'nim-rx)
(require 'nim-syntax)
(require 'nim-util)
(require 'nim-helper)
(require 'nim-smie)
(require 'paren) ; for ‘show-paren-data-function’
(require 'nim-fill)
(require 'commenter)

(put 'nim-mode 'font-lock-defaults '(nim-font-lock-keywords nil t))

(defun nim-font-lock-syntactic-face-function (syntax-ppss)
  "Return syntactic face given SYNTAX-PPSS."
  (if (nth 4 syntax-ppss) ; if nth 4 is exist, it means inside comment.
      (if (nim-docstring-p syntax-ppss)
          font-lock-doc-face
        font-lock-comment-face)
    font-lock-string-face))

(defun nim--common-init ()
  "Common configuration for ‘nim-mode’ and ‘nimscript-mode’."
  (run-hooks 'nim-common-init-hook)

  (setq-local nim--inside-compiler-dir-p
              (when (and buffer-file-name
                         (string-match
                          nimsuggest-ignore-dir-regex buffer-file-name))
                t))

  ;; Comment
  (setq-local comment-style 'indent)
  (setq-local comment-use-syntax t)
  ;; Those start and end comment variables are for initial value.
  (setq-local comment-start "#")
  (setq-local comment-end "")
  ;; actual comment values are defined here
  (setq-local commenter-config nim-comment)
  (commenter-setup)

  ;; ElDoc
  ;; See `eldoc-documentation-function'. Don't move this function out
  ;; side of major-mode function; it may cause weird eldoc behavior
  ;; (`eldoc-documentation-function' was set, but eldoc doesn't start
  ;; only first time you open a nim file)
  (nim-eldoc-on)

  ;; SMIE
  (smie-setup nim-mode-smie-grammar 'nim-mode-smie-rules
              :forward-token 'nim-mode-forward-token
              :backward-token 'nim-mode-backward-token)
  (setq-local indent-line-function #'nim-indent-line)
  ;; FIXME: due to uncompleted Nim’s smie grammar,
  ;; ‘smie--matching-block-data’ function gets stop when
  ;; the cursor is at proc/template/macro to find terminator
  ;; (I guess). To prevent this, temporary use default
  ;; show-paren-mode’s function instead.
  (setq-local show-paren-data-function #'show-paren--default)
  ;; Work around for #111
  (remove-hook 'post-self-insert-hook 'smie-blink-matching-open t)

  ;; Always indent with SPACES!
  (setq-local indent-tabs-mode nil)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)

  ;; Syntax highlight for strings
  (setq-local syntax-propertize-function nim-syntax-propertize-function)

  ;; Paragraph
  (setq-local paragraph-start "\\s-*$")
  ;; Navigation
  (setq-local beginning-of-defun-function #'nim-nav-beginning-of-defun) ; C-M-a
  (setq-local end-of-defun-function       #'nim-nav-end-of-defun)       ; C-M-e
  ;; Fill
  (setq-local fill-paragraph-function     #'nim-fill-paragraph)
  ;; add-log
  (setq-local add-log-current-defun-function #'nim-info-current-defun)

  ;; Hooks
  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'nim-electric-pair-string-delimiter 'append t)
  (add-hook 'post-self-insert-hook
            #'nim-indent-post-self-insert-function 'append 'local)
  (add-hook 'which-func-functions #'nim-info-current-defun nil t)

  ;; Workaround with org
  (when (and (fboundp 'org-in-src-block-p) (org-in-src-block-p))
    (modify-syntax-entry ?# "<" nim-mode-syntax-table))

  ;; Because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)

  ;; Use user configuration of `electric-indent-chars' mainly to use C-m/Return
  ;; key as `newline-and-indent'. If you prefer Emacs' old behavior regarding
  ;; C-m and C-j, you can configure like this:
  ;;
  ;;   (setq electric-indent-chars '())
  ;;   (global-set-key (kbd "C-j") #'newline-and-indent)
  (setq-local electric-indent-chars (append electric-indent-chars '(?: ?\s)))
  (when electric-indent-mode
    (define-key nim-mode-map [remap delete-backward-char] 'nim-electric-backspace)))

;; add ‘nim-indent-function’ to electric-indent’s
;; blocklist. ‘electric-indent-inhibit’ isn’t enough for old emacs.
(add-to-list 'electric-indent-functions-without-reindent 'nim-indent-line)

;;;###autoload
(define-derived-mode nim-mode prog-mode "Nim"
  "A major mode for the Nim programming language."
  :group 'nim

  ;; init hook
  (run-hooks 'nim-mode-init-hook)

  (nim--common-init)

  ;; Font lock
  (nim--set-font-lock-keywords 'nim-mode))

;;; NimScript
;;  -- https://nim-lang.org/docs/nims.html
;;  -- https://github.com/nim-lang/Nim/wiki/Using-nimscript-for-configuration

;;;###autoload
(define-derived-mode nimscript-mode prog-mode "NimScript"
  "A major-mode for NimScript files.
This major-mode is activated when you enter *.nims and *.nimble
suffixed files, but if it’s .nimble file, also another logic is
applied. See also ‘nimscript-mode-maybe’."
  :group 'nim

  (nim--common-init)

  (nim--set-font-lock-keywords 'nimscript-mode))

;;;###autoload
(defun nimscript-mode-maybe ()
  "Most likely turn on ‘nimscript-mode’.
In *.nimble files, if the first line sentence matches
‘nim-nimble-ini-format-regex’, this function activates ‘conf-mode’
instead.  The default regex’s matching word is [Package]."
  (interactive)
  (if (not (buffer-file-name))
      (nimscript-mode)
    (let ((extension (file-name-extension (buffer-file-name))))
      (cond ((equal "nims" extension)
             (nimscript-mode))
            ((equal "nimble" extension)
             (save-excursion
               (goto-char (point-min))
               (if (looking-at-p nim-nimble-ini-format-regex)
                   (conf-mode)
                 (nimscript-mode))))))))

;;; `auto-mode-alist'
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe))


;;; Font locks
(defun nim--set-font-lock-keywords (mode &optional arg)
  (let ((keywords
         (cl-case mode
           (nim-mode
            (cl-typecase (or arg font-lock-maximum-decoration)
              (null (nim--get-font-lock-keywords 0))
              (list
               (nim--get-font-lock-keywords
                (or (assoc-default 'nim-mode font-lock-maximum-decoration)
                    (assoc-default t font-lock-maximum-decoration)
                    t)))
              (number (nim--get-font-lock-keywords font-lock-maximum-decoration))
              (t (nim--get-font-lock-keywords t))))
           (nimscript-mode
            (append nim-font-lock-keywords
                    nim-font-lock-keywords-extra
                    nim-font-lock-keywords-2
                    ;; Add extra keywords for NimScript
                    nimscript-keywords)))))
    (setq-local font-lock-defaults
              `(,keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . nim-font-lock-syntactic-face-function)))))

(defun nim--get-font-lock-keywords (level)
  "Return font lock keywords, according to ‘font-lock-maximum-decoration’ LEVEL.

You can set below values as LEVEL:

0 or nil - only comment and string will be highlighted
1 - only basic keywords like if, or when
2 - don’t highlight some extra highlights
t - default

Note that without above values will be treated as t."
  (cl-case level
    (0 nil)
    (1 nim-font-lock-keywords)
    (2 (append nim-font-lock-keywords
               nim-font-lock-keywords-2
               nim-font-lock-keywords-3))
    (t (append nim-font-lock-keywords
               nim-font-lock-keywords-extra
               nim-font-lock-keywords-2
               nim-font-lock-keywords-3))))


;; Electric
;; (https://www.emacswiki.org/emacs/Electricity)

(defun nim-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (nim-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (nim-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     (t
      (let ((char last-command-event))
        (when (and  (memq char electric-indent-chars)
                    (not (nim-syntax-comment-or-string-p)))
          (cl-case char
            (?:  (nim-electric-colon))
            (?\s (nim-electric-space)))))))))

(defun nim-electric-colon ()
  (when (and (not current-prefix-arg)
             ;; Trigger electric colon only at end of line
             (eolp)
             ;; Avoid re-indenting on extra colon
             (not (equal ?: (char-before (1- (point))))))
    ;; Just re-indent dedenters
    (let ((dedenter-pos (nim-info-dedenter-statement-p))
          (current-pos (point)))
      (when dedenter-pos
        (save-excursion
          (goto-char dedenter-pos)
          (nim--indent-line-core)
          (unless (= (line-number-at-pos dedenter-pos)
                     (line-number-at-pos current-pos))
            ;; Reindent region if this is a multiline statement
            (indent-region dedenter-pos current-pos)))))))

(defun nim-electric-space ()
  (let (next)
    (when (and
           (eq (current-indentation) (current-column))
           (looking-back "^ +" (point-at-bol))
           (cl-oddp (current-indentation))
           (let* ((levels (nim-indent-calculate-levels))
                  (next-indent (cadr (member (1- (current-indentation)) levels))))
             (prog1 (and next-indent (< (current-indentation) next-indent))
               (setq next next-indent))))
      (indent-line-to next))))

(defun nim-electric-backspace (&rest args)
  "Delete preceding char or levels of indentation.
The ARGS are passed to original ‘delete-backward-char’ function."
  (interactive "p\nP")
  (let (back)
    (if (and electric-indent-mode
             (eq (current-indentation) (current-column))
             (called-interactively-p 'interactive)
             (not (nim-syntax-comment-or-string-p))
             (not (bolp))
             (not current-prefix-arg)
             (let ((levels (reverse (nim-indent-calculate-levels))))
               (setq back (cadr (member (current-indentation) levels)))))
        (indent-line-to back)
      (apply 'delete-backward-char args))))

;;; ELDOC

;; Eldoc supports for Nim's static information like pragma.
;; When you activate nimsuggest-mode, this also support's
;; nimsuggest's eldoc information.

(defvar nim-eldoc--skip-regex
  (rx (or (group symbol-start
                 (or "if" "when" "elif" "while"
                     ;; for tuple assignment
                     "var" "let" "const")
                 symbol-end (0+ " "))
          (group line-start (0+ " ")))))

(defun nim-eldoc-on-p()
  "Return non-nil if eldoc mode is activated."
  (or (bound-and-true-p eldoc-mode)
      ;; This mode was added at Emacs 25
      (bound-and-true-p global-eldoc-mode)))

(defun nim-eldoc--try-p ()
  "Return non-nil if current position can check eldoc."
  (and (nim-eldoc-on-p)
       (not (nim-line-comment-p))
       (not (member (char-after (point))
                    ;; not sure this works on windows
                    '(?\s ?\n)))))

;;;###autoload
(defun nim-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil."
  (interactive)
  (when (nim-eldoc--try-p)
    (if (nim-inside-pragma-p)
        (nim-eldoc--pragma-at-point)
      (funcall nimsuggest-eldoc-function))))

;;;###autoload
(defun nim-eldoc-on ()
  "This may or may not work.  Maybe this configuration has to set on.
Major-mode configuration according to the document."
  (interactive)
  (add-function :before-until (local 'eldoc-documentation-function)
                'nim-eldoc-function))

(defun nim-eldoc-off ()
  "Turn off nim eldoc mode."
  (interactive)
  (remove-function (local 'eldoc-documentation-function) 'nim-eldoc-function))

;;;###autoload
(defun nim-eldoc-setup (&rest _args)
  "This function may not work.
I tried to configure this stuff to be user definable, but currently failing.
The eldoc support should be turned on automatically, so please
use `nim-eldoc-off' manually if you don't like it."
  ;; TODO: describe not to support for manual configuration?
  ;; see also `eldoc-documentation-function'. It implies that the
  ;; configure should be done by major-mode side, so might be
  ;; impossible to configure by hook?
  (if (nim-eldoc-on-p) (nim-eldoc-on) (nim-eldoc-off)))

(defun nim-eldoc--get-pragma (pragma)
  "Get the PRAGMA's doc string."
  (let ((data (assoc-default pragma nim-pragmas)))
    (cl-typecase data
      (string data)
      ;; FIXME: more better operation
      (list (car data)))))

(defun nim-eldoc--pragma-at-point ()
  "Return string of pragma's description at point."
  (let* ((thing (thing-at-point 'symbol))
         (desc (nim-eldoc--get-pragma thing)))
    (when (and desc (string< "" desc))
      (format "%s: %s" thing (nim-eldoc--get-pragma thing)))))

(defun nim-eldoc-inside-paren-p ()
  "Return non-nil if it's inside pragma."
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (and (< 0 (nth 0 ppss))
           (eq ?\( (char-after (nth 1 ppss)))))))

;; backward compatibility
(defalias 'nim-eldoc-setup 'ignore)


;; hideshow.el (hs-minor-mode)
(defun nim-hideshow-forward-sexp-function (_arg)
  "Nim specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
  (nim-nav-end-of-defun)
  (unless (nim-line-empty-p)
    (backward-char)))

(add-to-list
 'hs-special-modes-alist
 `(nim-mode
   ,nim-nav-beginning-of-defun-regexp
   ;; Use the empty string as end regexp so it doesn't default to
   ;; "\\s)".  This way parens at end of defun are properly hidden.
   ""
   "#"
   nim-hideshow-forward-sexp-function
   nil))

(provide 'nim-mode)

;;; nim-mode.el ends here
