;;; purescript-mode.el --- A PureScript editing mode    -*- coding: utf-8 -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc
;; Copyright (C) 1992, 1997-1998  Simon Marlow, Graeme E Moss, and Tommy Thorn

;; Author:  1992      Simon Marlow
;;          1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>,
;;          2001-2002 Reuben Thomas (>=v1.4)
;;          2003      Dave Love <fx@gnu.org>
;;          2014      Tim Dysinger <tim@dysinger.net>
;; Keywords: faces files PureScript
;; URL: https://github.com/purescript/purescript-mode

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

;;; Code:

(require 'dabbrev)
(require 'compile)
(require 'outline)
(require 'purescript-align-imports)
(require 'purescript-sort-imports)
(require 'purescript-string)
(with-no-warnings (require 'cl))

;; All functions/variables start with `(literate-)purescript-'.

;; Version of mode.
(defconst purescript-version "@VERSION@"
  "The release version of `purescript-mode'.")

(defconst purescript-git-version "@GIT_VERSION@"
  "The Git version of `purescript-mode'.")

(defvar purescript-mode-pkg-base-dir (file-name-directory load-file-name)
  "Package base directory of installed `purescript-mode'.
Used for locating additional package data files.")

;;;###autoload
(defun purescript-version (&optional here)
  "Show the `purescript-mode` version in the echo area.
With prefix argument HERE, insert it at point.
When FULL is non-nil, use a verbose version string.
When MESSAGE is non-nil, display a message with the version."
  (interactive "P")
  (let* ((purescript-mode-dir (ignore-errors
                             (file-name-directory (or (locate-library "purescript-mode") ""))))
         (_version (format "purescript-mode version %s (%s @ %s)"
                           purescript-version
                           purescript-git-version
                           purescript-mode-dir)))
    (if here
        (insert _version)
      (message "%s" _version))))

;;;###autoload
(defun purescript-mode-view-news ()
  "Display information on recent changes to purescript-mode."
  (interactive)
  (with-current-buffer (find-file-read-only (expand-file-name "NEWS" purescript-mode-pkg-base-dir))
    (goto-char (point-min))
    (hide-sublevels 1)
    (outline-next-visible-heading 1)
    (show-subtree)))

(defgroup purescript nil
  "Major mode for editing PureScript programs."
  :link '(custom-manual "(purescript-mode)")
  :group 'languages
  :prefix "purescript-")

;;;###autoload
(defun purescript-customize ()
  "Browse the purescript customize sub-tree.
This calls 'customize-browse' with purescript as argument and makes
sure all purescript customize definitions have been loaded."
  (interactive)
  ;; make sure all modules with (defcustom ...)s are loaded
  (mapc 'require
        '(purescript-font-lock
          purescript-indentation
          purescript-indent
          purescript-interactive-mode
          purescript-yas))
  (customize-browse 'purescript))

;; Are we looking at a literate script?
(defvar purescript-literate nil
  "*If not nil, the current buffer contains a literate PureScript script.
Possible values are: `bird' and `tex', for Bird-style and LaTeX-style
literate scripts respectively.  Set by `purescript-mode' and
`literate-purescript-mode'.  For an ambiguous literate buffer -- i.e. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `purescript-literate-default' is used.")
(make-variable-buffer-local 'purescript-literate)
(put 'purescript-literate 'safe-local-variable 'symbolp)
;; Default literate style for ambiguous literate buffers.
(defcustom purescript-literate-default 'bird
  "Default value for `purescript-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style."
  :group 'purescript
  :type '(choice (const bird) (const tex) (const nil)))
;;;###autoload
(defvar purescript-mode-map
  (let ((map (make-sparse-keymap)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Editing-specific commands
    (define-key map (kbd "C-c C-.") 'purescript-mode-format-imports)
    (define-key map [remap delete-indentation] 'purescript-delete-indentation)

    map)
  "Keymap used in PureScript mode.")

;; Syntax table.
(defvar purescript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\'" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (cond ((featurep 'xemacs)
           ;; I don't know whether this is equivalent to the below
           ;; (modulo nesting).  -- fx
           (modify-syntax-entry ?{  "(}5" table)
           (modify-syntax-entry ?}  "){8" table)
           (modify-syntax-entry ?-  "_ 1267" table))
          (t
           ;; In Emacs 21, the `n' indicates that they nest.
           ;; The `b' annotation is actually ignored because it's only
           ;; meaningful on the second char of a comment-starter, so
           ;; on Emacs 20 and before we get wrong results.  --Stef
           (modify-syntax-entry ?\{  "(}1nb" table)
           (modify-syntax-entry ?\}  "){4nb" table)
           (modify-syntax-entry ?-  "_ 123" table)))
    (modify-syntax-entry ?\n ">" table)

    (let (i lim)
      (map-char-table
       (lambda (k v)
         (when (equal v '(1))
           ;; The current Emacs 22 codebase can pass either a char
           ;; or a char range.
           (if (consp k)
               (setq i (car k)
                     lim (cdr k))
             (setq i k
                   lim k))
           (while (<= i lim)
             (when (> i 127)
               (modify-syntax-entry i "_" table))
             (setq i (1+ i)))))
       (standard-syntax-table)))

    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?\\ "\\" table)
    (mapc (lambda (x)
            (modify-syntax-entry x "_" table))
          ;; Some of these are actually OK by default.
          "!#$%&*+./:<=>?@^|~")
    (unless (featurep 'mule)
      ;; Non-ASCII syntax should be OK, at least in Emacs.
      (mapc (lambda (x)
              (modify-syntax-entry x "_" table))
            (concat "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿"
                    "×÷"))
      (mapc (lambda (x)
              (modify-syntax-entry x "w" table))
            (concat "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
                    "ØÙÚÛÜÝÞß"
                    "àáâãäåæçèéêëìíîïðñòóôõö"
                    "øùúûüýþÿ")))
    table)
  "Syntax table used in PureScript mode.")

(defun purescript-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (purescript-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cdr reg)))))

(defun purescript-ident-pos-at-point ()
  "Return the span of the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    ;; Skip whitespace if we're on it.  That way, if we're at "map ", we'll
    ;; see the word "map".
    (if (and (not (eobp))
             (eq ?  (char-syntax (char-after))))
        (skip-chars-backward " \t"))

    (let ((case-fold-search nil))
      (multiple-value-bind (start end)
          (if (looking-at "\\s_")
              (list (progn (skip-syntax-backward "_") (point))
                    (progn (skip-syntax-forward "_") (point)))
            (list
             (progn (skip-syntax-backward "w'")
                    (skip-syntax-forward "'") (point))
             (progn (skip-syntax-forward "w'") (point))))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (cons start end)))))

(defun purescript-delete-indentation (&optional arg)
  "Like `delete-indentation' but ignoring Bird-style \">\"."
  (interactive "*P")
  (let ((fill-prefix (or fill-prefix (if (eq purescript-literate 'bird) ">"))))
    (delete-indentation arg)))

;; Various mode variables.

(defcustom purescript-mode-hook nil
  "Hook run after entering `purescript-mode'.

Some of the supported modules that can be activated via this hook:

   `purescript-indentation', Kristof Bastiaensen
     Intelligent semi-automatic indentation Mk2

   `purescript-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `purescript-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`purescript-indent' is activated using `turn-on-purescript-indent'.
For more information on a specific module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.

See Info node `(purescript-mode)purescript-mode-hook' for more details.

Warning: do not enable more than one of the three indentation
modes. See Info node `(purescript-mode)indentation' for more
details."
  :type 'hook
  :group 'purescript
  :link '(info-link "(purescript-mode)purescript-mode-hook")
  :link '(function-link purescript-mode)
  :options `(capitalized-words-mode
             turn-on-eldoc-mode
             turn-on-purescript-indent
             turn-on-purescript-indentation
             turn-on-purescript-simple-indent
             turn-on-purescript-unicode-input-method))

(defvar eldoc-print-current-symbol-info-function)

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'purescript-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;; The main mode functions
;;;###autoload
(define-derived-mode purescript-mode purescript-parent-mode "PureScript"
  "Major mode for editing PureScript programs.

See also Info node `(purescript-mode)Getting Started' for more
information about this mode.

\\<purescript-mode-map>
Literate scripts are supported via `literate-purescript-mode'.
The variable `purescript-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `purescript-version' to find out what version of PureScript mode you are
currently using.

Additional PureScript mode modules can be hooked in via `purescript-mode-hook';
see documentation for that variable for more details."
  :group 'purescript
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'fill-paragraph-function) 'purescript-fill-paragraph)
  ;; (set (make-local-variable 'adaptive-fill-function) 'purescript-adaptive-fill)
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-start-skip) "--\s*|?\s*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'indent-line-function) 'purescript-mode-suggest-indent-choice)
  ;; Set things up for font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(purescript-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . purescript-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . purescript-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t)))
  ;; PureScript's layout rules mean that TABs have to be handled with extra care.
  ;; The safer option is to avoid TABs.  The second best is to make sure
  ;; TABs stops are 8 chars apart, as mandated by the PureScript Report.  --Stef
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 8)
  ;; dynamic abbrev support: recognize PureScript identifiers
  ;; PureScript is case-sensitive language
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-distinction) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  (set (make-local-variable 'dabbrev-abbrev-char-regexp) "\\sw\\|[.]")
  (add-hook 'before-save-hook 'purescript-mode-before-save-handler nil t)
  (add-hook 'after-save-hook 'purescript-mode-after-save-handler nil t)
  )

(defun purescript-fill-paragraph (justify)
  (save-excursion
    ;; Fill paragraph should only work in comments.
    ;; The -- comments are handled properly by default
    ;; The {- -} comments need some extra love.
    (let* ((syntax-values (syntax-ppss))
           (comment-num (nth 4 syntax-values)))
      (cond
       ((eq t comment-num)
        ;; standard fill works wonders inside a non-nested comment
        (fill-comment-paragraph justify))

       ((integerp comment-num)
        ;; we are in a nested comment. lets narrow to comment content
        ;; and use plain paragraph fill for that
        (let* ((comment-start-point (nth 8 syntax-values))
               (comment-end-point
                (save-excursion
                  (re-search-forward "-}" (point-max) t comment-num)
                  (point)))
               (fill-paragraph-handle-comment nil))
          (save-restriction
            (narrow-to-region (+ 2 comment-start-point) (- comment-end-point 2))
            (fill-paragraph justify))))
       ((eolp)
        ;; do nothing outside of a comment
        t)
       (t
        ;; go to end of line and try again
        (end-of-line)
        (purescript-fill-paragraph justify))))))


;; (defun purescript-adaptive-fill ()
;;   ;; We want to use "--  " as the prefix of "-- |", etc.
;;   (let* ((line-end (save-excursion (end-of-line) (point)))
;;          (line-start (point)))
;;     (save-excursion
;;       (unless (in-comment)
;;         ;; Try to find the start of a comment. We only fill comments.
;;         (search-forward-regexp comment-start-skip line-end t))
;;       (when (in-comment)
;;         (let ();(prefix-start (point)))
;;           (skip-syntax-forward "^w")
;;           (make-string (- (point) line-start) ?\s))))))



;;;###autoload
(define-derived-mode literate-purescript-mode purescript-mode "LitPureScript"
  "As `purescript-mode' but for literate scripts."
  (setq purescript-literate
        (save-excursion
          (goto-char (point-min))
          (cond
           ((re-search-forward "^\\\\\\(begin\\|end\\){code}$" nil t) 'tex)
           ((re-search-forward "^>" nil t) 'bird)
           (t purescript-literate-default))))
  (if (eq purescript-literate 'bird)
      ;; fill-comment-paragraph isn't much use there, and even gets confused
      ;; by the syntax-table text-properties we add to mark the first char
      ;; of each line as a comment-starter.
      (set (make-local-variable 'fill-paragraph-handle-comment) nil))
  (set (make-local-variable 'mode-line-process)
       '("/" (:eval (symbol-name purescript-literate)))))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))

(defun purescript-pursuit (query &optional info)
  "Do a Pursuit search for QUERY.
When `purescript-pursuit-command' is non-nil, this command runs
that.  Otherwise, it opens a Pursuit search result in the browser.

If prefix argument INFO is given, then `purescript-pursuit-command'
is asked to show extra info for the items matching QUERY.."
  (interactive
   (let ((def (purescript-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Pursuit query (default %s): " def)
                          "Pursuit query: ")
                        nil nil def)
           current-prefix-arg)))
  (browse-url (format "https://pursuit.purescript.org/search?q=%s" query)))

(defcustom purescript-completing-read-function 'ido-completing-read
  "Default function to use for completion."
  :group 'purescript
  :type '(choice
          (function-item :tag "ido" :value ido-completing-read)
          (function-item :tag "helm" :value helm--completing-read-default)
          (function-item :tag "completing-read" :value completing-read)
          (function :tag "Custom function")))

(defcustom purescript-indent-spaces 2
  "Number of spaces to indent inwards.")

(defun purescript-mode-suggest-indent-choice ()
  "Ran when the user tries to indent in the buffer but no indentation mode has been selected.
Brings up the documentation for purescript-mode-hook."
  (describe-variable 'purescript-mode-hook))

(defun purescript-mode-format-imports ()
  "Format the imports by aligning and sorting them."
  (interactive)
  (let ((col (current-column)))
    (purescript-sort-imports)
    (purescript-align-imports)
    (goto-char (+ (line-beginning-position)
                  col))))

(defun purescript-mode-message-line (str)
  "Message only one line, multiple lines just disturbs the programmer."
  (let ((lines (split-string str "\n" t)))
    (when (and (car lines) (stringp (car lines)))
      (message "%s"
               (concat (car lines)
                       (if (and (cdr lines) (stringp (cadr lines)))
                           (format " [ %s .. ]" (purescript-string-take (purescript-trim (cadr lines)) 10))
                         ""))))))

(defun purescript-mode-before-save-handler ()
  "Function that will be called before buffer's saving."
  )

(defun purescript-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  )

;; Provide ourselves:

(provide 'purescript-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; purescript-mode.el ends here
