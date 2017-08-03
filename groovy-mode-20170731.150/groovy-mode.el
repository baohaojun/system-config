;;; groovy-mode.el --- Major mode for Groovy source files

;;  Copyright © 2006, 2009–2010, 2012–2016  Russel Winder

;;  Author: Russel Winder <russel@winder.org.uk>, 2006–
;;	Jim Morris <morris@wolfman.com>, 2009–
;;	Wilfred Hughes <me@wilfred.me.uk>, 2017–
;;  Maintainer:  Russel Winder <russel@winder.org.uk>
;;  Created: 2006-08-01
;;  Keywords: languages
;; Package-Requires: ((s "1.11.0"))

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; If you install using the packaging system no further set up should be needed. If you install this mode
;; manually then you will likely need to put these lines in your init file:
;;
;;   (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;;   (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

;;; Commentary:
;;  This mode was initially developed using the Java and Awk modes that are part of CC Mode (the 5.31 source
;;  was used) and C# Mode from Dylan R. E. Moonfire <contact@mfgames.com> (the 0.5.0 source was used).  This
;;  code may contain some code fragments from those sources that was cut-and-pasted then edited.  All other
;;  code was newly entered by the author.  Obviously changes have been made since then.

;;; Bugs:
;;  Bug tracking is currently handled using the GitHub issue tracker at
;;  https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues

;;; Versions:
;;  This mode is available on MELPA which tracks the mainline Git repository on GitHub, so there is a rolling release
;;  system based on commits to the mainline.

;;; Notes:
;;  Should we support GString / template markup ( e.g. `<%' and `%>') specially?

;;;  TODO:
;;   Issues with this code are managed via the project issue management
;;   on GitHub: https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues?state=open

;; History:
;;   History is tracked in the Git repository rather than in this file.
;;   See https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/commits/master

;;----------------------------------------------------------------------------
;;; Code:

(require 's)

(defvar groovy-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    ;; http://docs.groovy-lang.org/latest/html/documentation/#groovy-operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@ ?=))
      (modify-syntax-entry i "." table))
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table used in Groovy mode buffers.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(defconst groovy-type-regexp
  (rx symbol-start
      (group
       (or
        ;; Treat Foo, FooBar and FFoo as type names, but not FOO.
        (seq (+ upper) lower (0+ (or (syntax symbol) (syntax word))))
        "byte"
        "short"
        "int"
        "long"
        "float"
        "double"
        "boolean"
        "char"
        "void"))
      symbol-end
      (? "[]"))
  "Matches types, where the name is first group.")

(defvar groovy-symbol-regexp
  (rx
   symbol-start
   (group (+ (or (syntax word) (syntax symbol))))
   symbol-end)
  "A variable name or a type name.")

(defvar groovy-function-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     ;; A function may start with 'public static final'
     (1+
      (or
       (seq
        (or "public" "private" "protected"
            "abstract" "final" "static"
            "synchronized" "native" "def")
        (+ space))
       ;; or it may start with a type name.
       (seq (regexp ,groovy-type-regexp) (+ space))))

     ;; The actual function name.
     (group (regexp ,groovy-symbol-regexp))

     ;; Require an open paren to avoid confusing with "def foo ="
     (0+ space)
     "("))
  "Matches functions and methods in groovy code.
The function name is the second group in the regexp.")

(defvar groovy-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in groovy code, select match 2")

(defvar groovy-interface-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     (0+ (or "abstract" "public") (+ space))
     "interface" (+ space)
     (group (regexp ,groovy-symbol-regexp))))
  "Matches interface names in groovy code.")

(defvar groovy-imenu-regexp
  (list (list "Functions" groovy-function-regexp 2)
        (list "Classes" groovy-class-regexp 2)
        (list "Interfaces" groovy-interface-regexp 1)
        (list "Closures" "def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*=[ \t]*{" 1))
  "Imenu expression for Groovy")


;; For compatibility with Emacs < 24
(defalias 'groovy-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(defvar groovy-declaration-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     (+
      (or "def" "public" "private" "protected" "final"
          (regexp ,groovy-type-regexp))
      (+ space))
     (group (regexp ,groovy-symbol-regexp))))
  "Match 'def foo' or 'private Type foo'. The name is the second group.")

(defsubst groovy--in-string-p ()
  "Return t if (point) is in a string."
  (nth 3 (syntax-ppss)))
(defsubst groovy--in-string-at-p (pos)
  "Return t if POS is in a string."
  (save-excursion
    (nth 3 (syntax-ppss pos))))

(defvar groovy-font-lock-keywords
  `((,(regexp-opt
       ;; http://docs.groovy-lang.org/latest/html/documentation/#_keywords
       '("as"
         "assert"
         "break"
         "case"
         "catch"
         "class"
         "const"
         "continue"
         "def"
         "default"
         "do"
         "else"
         "enum"
         "extends"
         "finally"
         "for"
         "goto"
         "if"
         "implements"
         "import"
         "in"
         "instanceof"
         "interface"
         "new"
         "package"
         "return"
         "super"
         "switch"
         "this"
         "throw"
         "throws"
         "trait"
         "try"
         "while"
         ;; Other strings that we want to highlight as keywords.
         "abstract"
         "final"
         "native"
         "private"
         "protected"
         "public"
         "static"
         "synchronized"
         )
       'symbols)
     . font-lock-keyword-face)
    ;; Highlight println as a keyword, but don't highlight foo.println.
    (,(rx (or line-start space)
          symbol-start (or "print" "printf" "println") symbol-end)
     . font-lock-keyword-face)
    ;; Constants
    (,(regexp-opt '("false" "null" "true") 'symbols)
     . font-lock-constant-face)
    (,(rx symbol-start "it" symbol-end)
     . font-lock-variable-name-face)
    ;; Annotations
    (,(rx "@" symbol-start (+ (or (syntax word) (syntax symbol))) symbol-end)
     . c-annotation-face)
    (,groovy-type-regexp
     1 font-lock-type-face)
    ;; Highlight function names.
    (,groovy-function-regexp 2 font-lock-function-name-face)
    ;; Highlight declarations of the form 'def foo'.
    (,groovy-declaration-regexp
     2 font-lock-variable-name-face)
    ;; Highlight variables of the form 'foo = '
    (,(rx
       line-start (0+ space)
       (group (+ (or (syntax word) (syntax symbol))))
       (0+ space) "=")
     1 font-lock-variable-name-face)
    ;; Highlight $foo and $foo.bar string interpolation, but not \$foo.
    (,(lambda (limit)
        (let ((pattern
               (rx "$" (+ (or (syntax word) (syntax symbol))) symbol-end
                   (? "." (+ (or (syntax word) (syntax symbol))) symbol-end)))
              res match-data)
          (save-match-data
            ;; Search forward for $foo and terminate on the first
            ;; instance we find that's inside a sring.
            (while (and
                    (not res)
                    (re-search-forward pattern limit t))

              (let* ((string-delimiter-pos (nth 8 (syntax-ppss)))
                     (string-delimiter (char-after string-delimiter-pos))
                     (escaped-p (eq (char-before (match-beginning 0))
                                    ?\\)))
                (when (and (groovy--in-string-p)
                           ;; Interpolation does not apply in single-quoted strings.
                           (not (eq string-delimiter ?'))
                           (not escaped-p)
                           (not (equal (match-string 0) "$$")))
                  (setq res (point))
                  (setq match-data (match-data))))))
          ;; Set match data and return point so we highlight this
          ;; instance.
          (when res
            (set-match-data match-data)
            res)))
     (0 font-lock-variable-name-face t))
    ;; Highlight ${foo} string interpolation.
    (,(lambda (limit)
        (let (res start)
          (while (and
                  (not res)
                  (search-forward "${" limit t))
            (when (and (groovy--in-string-p)
                       (not (eq (char-before (- (point) 2))
                                ?\\)))
              (setq start (match-beginning 0))
              (let ((restart-pos (match-end 0)))
                (let (finish)
                  ;; Search forward for the } that matches the opening {.
                  (while (and (not res) (search-forward "}" limit t))
                    (let ((end-pos (point)))
                      (save-excursion
                        (when (and (ignore-errors (backward-list 1))
                                   (= start (1- (point))))
                          (setq res end-pos)))))
                  (unless res
                    (goto-char restart-pos))))))
          ;; Set match data and return point so we highlight this
          ;; instance.
          (when res
            (set-match-data (list start res))
            res)))
     (0 font-lock-variable-name-face t))))

(eval-when-compile
  ;; http://groovy-lang.org/syntax.html#_shebang_line
  (defconst groovy-shebang-regex
    (rx buffer-start "#"))
  (defconst groovy-triple-double-quoted-string-regex
    (rx "\"\"\""))
  (defconst groovy-triple-single-quoted-string-regex
    (rx "'''"))
  (defconst groovy-slashy-open-regex
    ;; /foo/ is a slashy-string, but // or /* are not.
    (rx "/" (not (any "/" "*"))))

  (defconst groovy-dollar-slashy-open-regex
    (rx "$/"))
  (defconst groovy-dollar-slashy-close-regex
    (rx "/$")))

(defun groovy-stringify-triple-quote ()
  "Put `syntax-table' property on triple-quoted strings."
  ;; This applies to both ''' and """
  (let* ((string-end-pos (point))
         (string-start-pos (- string-end-pos 3))
         (ppss (prog2
                   (backward-char 3)
                   (syntax-ppss)
                 (forward-char 3))))
    (unless (nth 4 ppss) ;; not inside comment
      (if (nth 8 ppss)
          ;; We're in a string, so this must be the closing triple-quote.
          ;; Put | on the last ' or " character.
          (put-text-property (1- string-end-pos) string-end-pos
                             'syntax-table (string-to-syntax "|"))
        ;; We're not in a string, so this is the opening triple-quote.
        ;; Put | on the first ' or " character.
        (put-text-property string-start-pos (1+ string-start-pos)
                           'syntax-table (string-to-syntax "|"))))))

(defun groovy--comment-p (pos)
  "Return t if POS is in a comment."
  (save-excursion
    (nth 4 (syntax-ppss pos))))

(defun groovy-stringify-slashy-string ()
  "Put `syntax-table' property on slashy-quoted strings (strings
of the form /foo/)."
  ;; We match to characters ?/ ?something, so move backwards so point
  ;; is on the /.
  (save-excursion
    (backward-char 1)
    (let* ((slash-pos (point))
           ;; Look at the previous char: // is a comment, not an empty
           ;; slashy-string.
           (singleline-comment (eq (char-before (1- (point))) ?/))
           ;; Look at this syntax on the previous char: if we're on a /*
           ;; or a */ this isn't a slashy-string.
           (multiline-comment (prog2
                                  (backward-char 1)
                                  (groovy--comment-p (point))
                                (forward-char 1)))
           (string-open-pos (nth 8 (syntax-ppss))))

      (unless (or singleline-comment multiline-comment)
        (if string-open-pos
            ;; If we're in a string, that was opened with /, then this
            ;; is the closing /. This prevents confusion with """ /* """
            (when (eq (char-after string-open-pos) ?/)
              (put-text-property (1- slash-pos) slash-pos
                                 'syntax-table (string-to-syntax "|")))
          ;; We're not in a string, so this is the opening / or division
          (let ((str (buffer-substring-no-properties (line-beginning-position) slash-pos)))
            ;; test if operator precedes slash. if so, slashy-string, otherwise division and ignore
            (when (string-match
                   (rx
                    (or bol
                        (or "+" "-" "=" "+=" "-=" "==" "!="
                            "<" "<=" ">" ">=" "&&" "!!" "?" "?:" ":"
                            "=~" "==~" "<=>" "("))
                    (0+ whitespace)
                    "/"
                    eol)
                   str)
              (put-text-property (1- slash-pos) slash-pos
                                 'syntax-table (string-to-syntax "|")))))))))

(defun groovy-stringify-dollar-slashy-open ()
  "Put `syntax-table' property on the opening $/ of
dollar-slashy-quoted strings."
  (let ((delimiter-end-pos (point)))
    (if (or (groovy--comment-p delimiter-end-pos) (groovy--in-string-p))
        ;; if not $/ open comment then go back a char so we can test
        ;;   if it's a closing slashy-string /
        (backward-char 1)
      ;; Mark the $ in $/ as a generic string delimiter.
      (put-text-property (- delimiter-end-pos 2) (- delimiter-end-pos 1)
                         'syntax-table (string-to-syntax "|")))))

(defun groovy-stringify-dollar-slashy-close ()
  "Put `syntax-table' property on the closing /$ of
dollar-slashy-quoted strings."
  (let* ((delimiter-end-pos (point))
         ;; We can't use `syntax-ppss' here as the state may not be
         ;; set yet. Using `parse-partial-sexp' ensures that the
         ;; highlighting is correct even when the mode is started
         ;; initially.
         (in-string (nth 3 (parse-partial-sexp (point-min) delimiter-end-pos))))
    (unless (or (groovy--comment-p delimiter-end-pos) (not in-string)
                ;; Ignore $/$ as it's escaped and not a /$ close delimiter.
                (looking-back (rx "$/$") 3))
      ;; Mark the $ in /$ as a generic string delimiter.
      (put-text-property (- delimiter-end-pos 1) delimiter-end-pos
                         'syntax-table (string-to-syntax "|")))))


(defconst groovy-syntax-propertize-function
  (syntax-propertize-rules
   ;; Treat the shebang as a comment. We reuse comment sequence b
   ;; (which is the // comment) so Emacs treats \n as the end of the
   ;; comment.
   (groovy-shebang-regex
    (0 "< b"))
   (groovy-triple-double-quoted-string-regex
    (0 (ignore (groovy-stringify-triple-quote))))
   (groovy-triple-single-quoted-string-regex
    (0 (ignore (groovy-stringify-triple-quote))))
   ;; http://groovy-lang.org/syntax.html#_dollar_slashy_string
   (groovy-dollar-slashy-open-regex
    (0 (ignore (groovy-stringify-dollar-slashy-open))))
   (groovy-dollar-slashy-close-regex
    (0 (ignore (groovy-stringify-dollar-slashy-close))))
   ;; http://groovy-lang.org/syntax.html#_slashy_string
   (groovy-slashy-open-regex
    (0 (ignore (groovy-stringify-slashy-string))))))

(defgroup groovy nil
  "A Groovy major mode."
  :group 'languages)

(defcustom groovy-indent-offset 4
  "Indentation amount for Groovy."
  :safe #'integerp
  :group 'groovy)

(defun groovy--ends-with-infix-p (str)
  "Does STR end with an infix operator?"
  (string-match-p
   (rx
    (or symbol-end space)
    ;; http://docs.groovy-lang.org/next/html/documentation/core-operators.html
    (or "+" "-" "*" "/" "%" "**"
        "=" "+=" "-=" "*=" "/=" "%=" "**="
        "==" "!=" "<" "<=" ">" ">=" "<<=" ">>=" ">>>=" "&=" "^=" "|="
        "&&" "!!"
        "&" "|" "^" "<<" "<<<" ">>" ">>>"
        "?" "?:" ":"
        "=~" "==~"
        "<=>" "<>"
        "in" "as")
    (0+ space)
    line-end)
   str))

(defun groovy--current-line ()
  "The current line enclosing point."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun groovy--enclosing-blocks ()
  "Return a list of the block keywords that enclose point.

Given the code, where | is point:

def foo() {
  if (bar) {
    switch (baz) {
      |
    }
  }
}

Then this function returns (\"def\" \"if\" \"switch\")."
  (let (enclosing-paren-pos blocks)
    (save-excursion
      (setq enclosing-paren-pos (nth 1 (syntax-ppss)))
      (while enclosing-paren-pos
        (goto-char enclosing-paren-pos)
        (let* ((line (s-trim (groovy--current-line)))
               (symbols (s-split (rx symbol-end) line t))
               (symbol (car-safe symbols)))
          (push symbol blocks))
        (setq enclosing-paren-pos (nth 1 (syntax-ppss)))))
    blocks))

(defconst groovy--case-regexp
  (rx (or
       (seq "case" symbol-end (+ any))
       (seq "default" symbol-end))
      ":"))

(defun groovy-indent-line ()
  "Indent the current line according to the number of parentheses."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation)))
         (syntax-bol (syntax-ppss (line-beginning-position)))
         (multiline-string-p (nth 3 syntax-bol))
         (multiline-comment-p (nth 4 syntax-bol))
         (current-paren-depth (nth 0 syntax-bol))
         (current-paren-pos (nth 1 syntax-bol))
         (text-after-paren
          (when current-paren-pos
            (save-excursion
              (goto-char current-paren-pos)
              (s-trim
               (buffer-substring
                (1+ current-paren-pos)
                (line-end-position))))))
         (current-line (s-trim (groovy--current-line)))
         has-closing-paren)
    ;; If this line starts with a closing paren, unindent by one level.
    ;;   if {
    ;;   } <- this should not be indented.
    (when (or (s-starts-with-p "}" current-line)
              (s-starts-with-p ")" current-line)
              (s-starts-with-p "]" current-line))
      (setq has-closing-paren t)
      (setq current-paren-depth (1- current-paren-depth)))

    ;; `current-paren-depth' should never be negative, unless the code
    ;; contains unbalanced parens. Ensure we handle that robustly.
    (when (< current-paren-depth 0)
      (setq current-paren-depth 0))

    (cond
     ;; Don't try to indent the line if we're in a multiline string.
     (multiline-string-p 'noindent)
     ;; Ensure we indent
     ;; /*
     ;;  * foo
     ;;  */
     ;;  correctly.
     (multiline-comment-p
      (indent-line-to (1+ (* groovy-indent-offset current-paren-depth))))

     ;; Ensure we indent
     ;; def x = [1,
     ;;          2,
     ;; ]
     ;; correctly.
     ((and (not (s-blank-str? text-after-paren))
           (not has-closing-paren)
           (not (equal text-after-paren "->")))
      (let (open-paren-column)
        (save-excursion
          (goto-char current-paren-pos)
          (setq open-paren-column (current-column)))
        (indent-line-to (1+ open-paren-column))))

     ;; Indent according to the number of parens.
     (t
      (let ((indent-level current-paren-depth)
            prev-line
            end-slashy-string)
        ;; If the previous line ended `foo +` then this line should be
        ;; indented one more level.
        (save-excursion
          ;; Try to go back one line.
          (when (zerop (forward-line -1))
            ;; Ignore the previous line if it's a comment or end slashy-string
            (let ((line-end (line-end-position)))
              (unless (groovy--comment-p line-end)
                (setq prev-line (buffer-substring (point) (line-end-position))))
              ;; check if the last thing is a slashy-string end
              (setq end-slashy-string (and
                                       (eq (char-before line-end) ?/)
                                       (groovy--in-string-at-p (- line-end 1)))))))
        (when (and prev-line
                   (not end-slashy-string)
                   (groovy--ends-with-infix-p prev-line)
                   (not (s-matches-p groovy--case-regexp prev-line)))
          (setq indent-level (1+ indent-level)))

        ;; If this line is .methodCall() then we should indent one
        ;; more level.
        (when (s-starts-with-p "." current-line)
          (setq indent-level (1+ indent-level)))

        ;; If we're inside a switch statement, we should indent
        ;; another level after case labels, e.g.
        ;; case foo:
        ;;     bar // <- extra indent
        (let ((blocks (groovy--enclosing-blocks))
              (switch-count 0))
          (dolist (block-symbol blocks)
            (when (equal block-symbol "switch")
              (setq switch-count (1+ switch-count))))
          (when (> switch-count 0)
            (setq indent-level (+ indent-level switch-count))
            ;; The `case foo:' line should be indented less than the body.
            (when (s-matches-p groovy--case-regexp current-line)
              (setq indent-level (1- indent-level)))
            ;; The extra indent does not apply to the } closing the
            ;; switch block.
            (when (and
                   (equal (car (last blocks)) "switch")
                   (equal current-line "}"))
              (setq indent-level (1- indent-level)))))

        (indent-line-to (* groovy-indent-offset indent-level)))))
    ;; Point is now at the beginning of indentation, restore it
    ;; to its original position (relative to indentation).
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

;;;###autoload
(define-derived-mode groovy-mode groovy-parent-mode "Groovy"
  "Major mode for editing Groovy code.

The hook `groovy-mode-hook' is run with no args at mode
initialization.

Key bindings:
\\{groovy-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(groovy-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       groovy-syntax-propertize-function)
  (setq imenu-generic-expression groovy-imenu-regexp)
  (set (make-local-variable 'indent-line-function) #'groovy-indent-line)
  (set (make-local-variable 'comment-start) "//"))

(provide 'groovy-mode)

;;; groovy-mode.el ends here
