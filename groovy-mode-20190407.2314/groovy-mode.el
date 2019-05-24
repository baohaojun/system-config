;;; groovy-mode.el --- Major mode for Groovy source files

;;  Copyright © 2006, 2009–2010, 2012–2016  Russel Winder

;; Author: Russel Winder <russel@winder.org.uk>, 2006–
;;    Jim Morris <morris@wolfman.com>, 2009–
;;    Wilfred Hughes <me@wilfred.me.uk>, 2017–
;; Maintainer:  Russel Winder <russel@winder.org.uk>
;; Created: 2006-08-01
;; Keywords: languages
;; Version: 2.1
;; Package-Requires: ((s "1.12.0") (emacs "24.3") (dash "2.13.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; If you install using the packaging system no further set up should be needed. If you install this mode
;; manually then you will likely need to put these lines in your init file:
;;
;;   (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;;   (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

;;; Bugs:
;;  Bug tracking is currently handled using the GitHub issue tracker at
;;  https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues

;;; Notes:
;;  Should we support GString / template markup ( e.g. `<%' and `%>') specially?

;;; Code:

(require 's)
(require 'dash)

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
;;;###autoload
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; Regexp Constants
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

(defconst groovy-declaration-keyword-regex
  (rx
   (* space)
   symbol-start
   (group (*
           (seq
            (+ (or "def" "public" "private" "protected" "final" "static"
                   "abstract" "synchronized" "native")
               (+ space))))))
  "Matches declaration keywords.")

(defconst groovy-declaration-regexp
  (rx-to-string
   `(seq
     (or bol "(" ";" "," "{")
     (regexp ,groovy-declaration-keyword-regex)
     (seq
      (* space)
      symbol-start
      (group
       (or
        ;; Treat Foo, FooBar and FFoo as type names, but not FOO.
        ;; also Foo<Bar>
        (seq (+ upper) lower (* (syntax word)) (or " " "<"))
        (seq (or
              "def"
              "byte"
              "short"
              "int"
              "long"
              "float"
              "double"
              "boolean"
              "char"
              "void")
             symbol-end
             (? "[]")))))))
  "Matches declarations of the type 'def FooBar<?>'.")

(defconst groovy-symbol-regexp
  (rx
   symbol-start
   (group (+ (or (syntax word) (syntax symbol))))
   symbol-end)
  "A variable name or a type name.")

(defconst groovy-variable-assignment-regexp
  (rx-to-string
   `(seq
     (regexp ,groovy-symbol-regexp)
     (* space)
     "="
     (not (any "~" "="))))
  "Matches variable assignments of the type 'a = 1'.")

(defconst groovy-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in groovy code, select match 2.")

(defconst groovy-interface-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     (0+ (or "abstract" "public") (+ space))
     "interface" (+ space)
     (group (regexp ,groovy-symbol-regexp))))
  "Matches interface names in groovy code.")

(defconst groovy-annotation-regexp
  (rx "@" symbol-start (+ (or (syntax word) (syntax symbol))) symbol-end)
  "Match annotation names.")

;; vars
(defvar groovy-imenu-regexp
  (list ;; FIXME: removed `groovy-function-regexp', now is using a function.
        ;;(list "Functions" groovy-function-regexp 2)
        (list "Classes" groovy-class-regexp 2)
        (list "Interfaces" groovy-interface-regexp 1)
        (list "Closures" "def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*=[ \t]*{" 1))
  "Imenu expression for Groovy.")


;; For compatibility with Emacs < 24
(defalias 'groovy-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(defsubst groovy--in-string-p ()
  "Return t if (point) is in a string."
  (nth 3 (syntax-ppss)))
(defsubst groovy--in-string-at-p (pos)
  "Return t if POS is in a string."
  (save-excursion
    (nth 3 (syntax-ppss pos))))

(defvar groovy-font-lock-keywords
  ;; Annotations are defined with the @interface, which is a keyword:
  ;; http://groovy-lang.org/objectorientation.html#_annotation
  ;; but it's nicer to use annotation highlighting.
  `(("@interface" . groovy-annotation-face)
    (,(regexp-opt
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
    (,groovy-annotation-regexp . groovy-annotation-face)
    ;; highlight types 'Foo', 'char'
    (,groovy-type-regexp 1 font-lock-type-face)
    ;; Highlight declarations of the form 'def foo' and 'public void fooBar()'.
    (groovy-declaration-search 1 font-lock-variable-name-face)
    ;; Highlight $foo and $foo.bar string interpolation, but not \$foo.
    (,(lambda (limit)
        (let ((pattern
               (rx (not (any "\\"))
                   (group
                    "$" (+ (or (syntax word) (syntax symbol))) symbol-end
                    (? "." (+ (or (syntax word) (syntax symbol))) symbol-end))))
              res match-data)
          (save-match-data
            ;; Search forward for $foo and terminate on the first
            ;; instance we find that's inside a sring.
            (while (and
                    (not res)
                    (re-search-forward pattern limit t))
              (let* ((string-delimiter-pos (nth 8 (syntax-ppss)))
                     (string-delimiter (char-after string-delimiter-pos)))
                (when (and (groovy--in-string-p)
                           ;; Interpolation does not apply in single-quoted strings.
                           (not (eq string-delimiter ?')))
                  (setq res (point))
                  ;; Set match data to the group we matched.
                  (setq match-data (list (match-beginning 1) (match-end 1)))))))
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
            (let* ((string-delimiter-pos (nth 8 (syntax-ppss)))
                   (string-delimiter (char-after string-delimiter-pos))
                   (escaped-p (eq (char-before (- (point) 2))
                                  ?\\)))
              (when (and (groovy--in-string-p)
                         ;; Interpolation does not apply in single-quoted strings.
                         (not (eq string-delimiter ?'))
                         (not escaped-p))
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
                      (goto-char restart-pos)))))))
          ;; Set match data and return point so we highlight this
          ;; instance.
          (when res
            (set-match-data (list start res))
            res)))
     (0 font-lock-variable-name-face t))
    (groovy-special-variable-search 1 font-lock-variable-name-face)
    (groovy-function-name-search 1 font-lock-function-name-face)))

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

(defconst groovy-postfix-operator-regex
  (rx (or "++" "--")))

(defun groovy-special-variable-search (limit)
  "Search for text marked with `groovy-special-variable' up to LIMIT."
  (groovy-special-prop-search limit 'groovy-special-variable))

(defun groovy-function-name-search (limit)
  "Search for text marked with `groovy-special-variable' up to LIMIT."
  (groovy-special-prop-search limit 'groovy-function-name))

(defun groovy-special-prop-search (limit prop-name)
  "Search up to LIMIT for text property PROP-NAME."
  (let* ((pos (point))
         (chg (next-single-property-change pos prop-name nil limit)))
    (when (and chg (> chg pos))
      (goto-char chg)
      (let ((v (get-text-property chg prop-name)))
        (set-match-data v)
        (or v (groovy-special-prop-search limit prop-name))))))

(defun groovy--travel-parameterized-types ()
  "Pass over <Foo<Bar>> when searching declarations."
  (let ((count 1)
        (found t))
    (while (and (> count 0) found)
      (setq found (re-search-forward (rx (or ">" "<")) (line-end-position) t))
      (when found
        (setq count (if (equal (match-string 0) ">")
                        (1- count)
                      (1+ count)))))))

(defun groovy-variable-assignment-search (limit)
  "Highlight variable assignments up to LIMIT."
  (let ((case-fold-search nil)
        (pos (point))
        (match (re-search-forward groovy-variable-assignment-regexp limit t)))
    (when (and match (> match pos))
      (or (save-excursion
            (save-match-data
              (not (string-match
                    (rx-to-string
                     `(seq
                       (regexp ,groovy-annotation-regexp)
                       (* space)
                       (zero-or-one (seq "(" (* (not (any ")")))))
                       eol))
                    (buffer-substring-no-properties (line-beginning-position) (match-beginning 0))))))
          (groovy-variable-assignment-search limit)))))

(defun groovy-declaration-search (limit)
  "Find variable declarations up to LIMIT."
  (remove-text-properties (point)
                          (or limit (point-max))
                          '(groovy-special-variable nil
                                                    groovy-function-name nil))
  (let ((pos (point))
        (case-fold-search nil)
        (match (re-search-forward groovy-declaration-regexp limit t)))
    (when (and match (> match pos))
      (or (and
           (not (groovy--in-string-p))
           (not (groovy--comment-p (point)))
           (let ((match-s (s-trim (match-string 0))))
             (when (s-ends-with-p "<" match-s)
               (groovy--travel-parameterized-types))
             (let ((var-match
                    (re-search-forward
                     (rx-to-string `(seq point (* space)
                                         (regexp ,groovy-symbol-regexp)))
                     (line-end-position) t)))
               ;; matches initial regexp, now look at special cases
               (if var-match
                   ;; if the var ends in a '(' it's a method name
                   (let ((md (match-data)))
                     (if (re-search-forward (rx point (* space) "(") nil t)
                         (progn
                           (backward-char 1)
                           (set-match-data md)
                           (with-silent-modifications
                             (put-text-property (match-beginning 1) (match-end 1)
                                                'groovy-function-name (match-data)))
                           nil)
                       ;; else if declaration followed by ',' then it's of form `String a, b,c'
                       (unless (or (s-starts-with-p "(" match-s)
                                   (s-starts-with-p "{" match-s)
                                   (s-starts-with-p "," match-s))

                         (with-silent-modifications
                           (while (re-search-forward (rx-to-string
                                                      `(seq point (* space) "," (* space)
                                                            (regexp ,groovy-symbol-regexp)))
                                                     limit t)
                             (put-text-property (match-beginning 1) (match-end 1)
                                                'groovy-special-variable (match-data))))
                         (set-match-data md))
                       t))
                 ;; didn't match regexp, check if it's of form `def (a, b, c) = [1, 2, 3]'
                 (when (re-search-forward
                        (rx-to-string
                         `(seq point (* space) "(" (* space)
                               (group (+ (seq
                                          (regexp ,groovy-symbol-regexp)
                                          (* space)
                                          (opt ",")
                                          (* space))))
                               (* space) ")" (* space) "="))
                        limit t)
                   (let ((s (match-string 1))
                         (beg (match-beginning 0))
                         (end (match-end 0)))
                     (when (save-excursion
                             (string-match
                              (rx-to-string
                               `(seq
                                 bol
                                 (+ (seq
                                     (* space)
                                     (regexp ,groovy-symbol-regexp)
                                     (* space)
                                     (or "," eol)))
                                 eol))
                              s))
                       (with-silent-modifications
                         (save-excursion
                           (goto-char beg)
                           (while (re-search-forward groovy-symbol-regexp end t)
                             ;; mark list for later search
                             (put-text-property (match-beginning 1) (match-end 1)
                                                'groovy-special-variable (match-data)))))
                       nil)))))))
          (groovy-declaration-search limit)))))

(defun groovy-stringify-triple-quote ()
  "Put `syntax-table' property on triple-quoted strings."
  ;; This applies to both ''' and """
  (let* ((delim-end-pos (point))
         (delim-start-pos (- delim-end-pos 3))
         (ppss (prog2
                   (backward-char 3)
                   (syntax-ppss)
                 (forward-char 3)))
         (in-comment (nth 4 ppss))
         (string-start-pos (nth 8 ppss)))
    (unless in-comment
      (if string-start-pos
          (let ((open-delimiter (char-after string-start-pos))
                (current-delimiter (char-after delim-start-pos)))
            ;; Ensure that we're closing with the same triple-quote
            ;; type as we opened with, because '''""""''' is a legal
            ;; string literal.
            (when (equal open-delimiter current-delimiter)
              ;; We're in a string, so this must be the closing triple-quote.
              ;; Put | on the last ' or " character.
              (put-text-property (1- delim-end-pos) delim-end-pos
                                 'syntax-table (string-to-syntax "|"))))
        ;; We're not in a string, so this is the opening triple-quote.
        ;; Put | on the first ' or " character.
        (put-text-property delim-start-pos (1+ delim-start-pos)
                           'syntax-table (string-to-syntax "|"))))))

(defun groovy--comment-p (pos)
  "Return t if POS is in a comment."
  (save-excursion
    (nth 4 (syntax-ppss pos))))

(defun groovy-stringify-slashy-string ()
  "Put `syntax-table' property on slashy-quoted strings (strings
of the form /foo/)."
  (let* ((final-pos (point))
         ;; We matched two characters starting with "/", e.g. "/x". Point is
         ;; currently after the "x". Calculate the position just after the "/".
         (slash-pos (1- (point)))
         (prev-char (char-before (- slash-pos 1)))
         (prev-prev-char (char-before (- slash-pos 2)))
         ;; Look at the previous char: // is a comment, not an empty
         ;; slashy-string. /foo\// does not contain a comment though.
         (singleline-comment (and (eq prev-char ?/)
                                  (not (eq prev-prev-char ?\\))))
         ;; Look at this syntax on the previous char: if we're on a /*
         ;; or a */ this isn't a slashy-string.
         (multiline-comment (save-excursion
                              (goto-char (1- slash-pos))
                              (groovy--comment-p (point))))
         (string-open-pos (nth 8 (syntax-ppss slash-pos))))

    (unless (or singleline-comment multiline-comment)
      (if string-open-pos
          ;; If we're in a string, that was opened with /, then this
          ;; is the closing /. This prevents confusion with """ /* """
          (if (eq (char-after string-open-pos) ?/)
              (put-text-property (1- slash-pos) slash-pos
                                 'syntax-table (string-to-syntax "|"))
            ;; We're in a string, but not a slashy string. Ensure we
            ;; don't put point beyond the /, to avoid confusion with
            ;; """foo/""".
            (setq final-pos (1- final-pos)))
        ;; We're not in a string, so this is the opening / or division.
        (let ((str (buffer-substring-no-properties (line-beginning-position) slash-pos)))
          ;; Test if an operator precedes this slash. if so, slashy-string,
          ;; otherwise it's division so ignore.
          (when (string-match
                 (rx
                  (or bol
                      (or "+" "-" "=" "+=" "-=" "==" "!="
                          "<" "<=" ">" ">=" "&&" "||" "?" "?:" ":"
                          "=~" "==~" "<=>" "(" "~"))
                  (0+ whitespace)
                  "/"
                  eol)
                 str)
            (put-text-property (1- slash-pos) slash-pos
                               'syntax-table (string-to-syntax "|"))))))
    (goto-char final-pos)))

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
         (ppss (parse-partial-sexp (point-min) delimiter-end-pos))
         (in-string (nth 3 ppss))
         (string-start-pos (nth 8 ppss)))
    (cond
     ((groovy--comment-p delimiter-end-pos)
      ;; Do nothing inside comments.
      nil)
     ((not in-string)
      ;; If we're not in a string, then /$ is the start of a normal
      ;; slashy-string, e.g. /$ foo/.
      ;;
      ;; Note that both `groovy-stringify-dollar-slashy-close' and
      ;; `groovy-stringify-slashy-string' expect to be two characters
      ;; after the /, so we don't need to move point before calling.
      (groovy-stringify-slashy-string))
     ((looking-back (rx "$/$") 3)
      ;; Ignore $/$ as it's escaped and not a /$ close delimiter.
      nil)
     (t
      ;; Otherwise, we're in a string.
      (when (eq (char-after string-start-pos) ?$)
        ;; If this string opened with $, this is a string of the form
        ;; $/foo/$. Mark the final $ as a generic string delimiter.
        (put-text-property (- delimiter-end-pos 1) delimiter-end-pos
                           'syntax-table (string-to-syntax "|")))))))


(defconst groovy-syntax-propertize-function
  (syntax-propertize-rules
   ;; Treat the shebang as a comment. We reuse comment sequence b
   ;; (which is the // comment) so Emacs treats \n as the end of the
   ;; comment.
   (groovy-shebang-regex
    (0 "< b"))

   ;; WARNING: These are a pain to refactor. Emacs tries each one of
   ;; these regexps in order. It resumes parsing from wherever point
   ;; is left at the end of the function call.
   ;;
   ;; As a result, it's important that these functions move point
   ;; backwards if they may have moved over another delimiter (e.g. /$
   ;; and /). However, they must all move point by a non-zero amount,
   ;; or you get an infinite loop during fontification.
   ;;
   ;; The unit tests are pretty thorough, so they should catch any
   ;; issues.
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

(defcustom groovy-highlight-assignments nil
  "Highlight variable assignments after declaration."
  :type 'boolean
  :group 'groovy)

(defvar groovy-annotation-face 'groovy-annotation-face)
(defface groovy-annotation-face
  '((default :inherit font-lock-constant-face))
  "Face for highlighting annotations in Groovy mode."
  :group 'groovy)

(defconst groovy-expression-end-regexp
  (rx-to-string
   `(or symbol-end
        space
        (syntax string-quote)
        (syntax close-parenthesis)
        (regexp ,groovy-postfix-operator-regex))))

(defun groovy--ends-with-token-p (token-list str)
  "Return t if STR ends with one of the tokens in TOKEN-LIST."
  (string-match-p
   (rx-to-string
    `(seq
      (regexp ,groovy-expression-end-regexp)
      (or ,@token-list)
      (0+ space)
      line-end))
    str))

(defun groovy--ends-with-infix-p (str)
  "Does STR end with an infix operator?"
  (groovy--ends-with-token-p
   ;; http://docs.groovy-lang.org/next/html/documentation/core-operators.html
   '("+" "-" "*" "/" "%" "**"
     "=" "+=" "-=" "*=" "/=" "%=" "**="
     "==" "!=" "<" "<=" ">" ">=" "<<=" ">>=" ">>>=" "&=" "^=" "|="
     "&&" "||"
     "&" "|" "^" "<<" "<<<" ">>" ">>>"
     "?" "?:"
     ;; Kludge: require a space before :. This enables us to
     ;; distinguish labels from ternary calls. Strictly speaking,
     ;; Groovy does not require a space before :, instead it seems to
     ;; look for a preceding ?.
     " :"
     "=~" "==~"
     "<=>" "<>"
     "in" "as")
   str))

(defun groovy--ends-with-comma-p (str)
  "Does STR end with a comma?"
  (groovy--ends-with-token-p
   '(",")
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

(defun groovy--remove-comments (src)
  "Remove all comments from a string of groovy source code."
  (->> src
       (replace-regexp-in-string (rx "/*" (*? anything) "*/") "")
       (replace-regexp-in-string (rx "//" (* not-newline)) "")))

(defun groovy--effective-paren-depth (pos)
  "Return the paren depth of position POS, but ignore repeated parens on the same line."
  (let ((paren-depth 0)
        (syntax (syntax-ppss pos))
        (current-line (line-number-at-pos pos)))
    (save-excursion
      ;; Keep going whilst we're inside parens.
      (while (> (nth 0 syntax) 0)
        ;; Go to the most recent enclosing open paren.
        (goto-char (nth 1 syntax))

        ;; Count this paren, but only if it was on another line.
        (let ((new-line (line-number-at-pos (point))))
          (unless (= new-line current-line)
            (setq paren-depth (1+ paren-depth))
            (setq current-line new-line)))

        (setq syntax (syntax-ppss (point)))))
    paren-depth))

(defun groovy--extract-line-without-comments ()
  "Extracts the part of the current line that is not a comment."
  (let (code-text
        (start-pos (line-beginning-position))
        (end-pos (line-end-position)))
    ;; Unless this line is already inside a multiline comment,
    ;; use parse-partial-sexp to get the part of the line not
    ;; a part of a comment.
    (unless (groovy--comment-p start-pos)
      (save-excursion
        (parse-partial-sexp start-pos end-pos nil nil nil t)
        (setq code-text (buffer-substring start-pos (point)))

        ;; Unless we went all the way to the end of the line, we
        ;; encountered a comment delimiter //, /* or #. Remove this delimiter.
        (unless (= (point) end-pos)
          (let ((delims '("//" "/*" "#")))
            (setq code-text (replace-regexp-in-string
                             (rx-to-string `(seq (or ,@delims) line-end)) "" code-text))))))

    ;; Return the part of the line that isn't a comment (may be nil).
    code-text))

(defun groovy--backwards-to-prev-code-line ()
  "Move point to the previous non-comment line, and return its contents."
  (catch 'done
    (let (code-text)
      (while t
        ;; Move backwards one line, or throw 'done if we're at the
        ;; beginning of the buffer.
        (unless (zerop (forward-line -1))
          (throw 'done nil))

        ;; Get the part of the line that isn't in a comment.
        ;; If this isn't just white space, return it as a code line.
        (setq code-text (groovy--extract-line-without-comments))
        (unless (s-blank-str-p code-text)
          (throw 'done code-text))))))

(defun groovy--line-ends-with-incomplete-block-statement-p ()
  "Return t if the current line ends with an incomplete block
statement, without an open curly brace."
  (unless (groovy--in-string-p)
    (save-excursion
      (end-of-line)
      ;; Search backwards until we are no longer in a comment.
      (while (and (not (bolp)) (groovy--comment-p (point)))
        (re-search-backward "/[/*]" (line-beginning-position) t))
      (unless (groovy--comment-p (point))
        (skip-chars-backward (rx blank))
        ;; We should be at the end of the actual code line. We are looking
        ;; for a line that ends with either if|while|for followed by
        ;; (condition), or a lone else.
        (if (eq (char-before) ?\))
            (when (condition-case nil (progn (backward-sexp) t) (scan-error nil))
              (skip-chars-backward (rx blank))
              (string-match (rx symbol-start (group (or "if" "for" "while")) line-end)
                            (buffer-substring-no-properties (line-beginning-position) (point))))
          (string-match (rx symbol-start "else" line-end)
                        (buffer-substring-no-properties (line-beginning-position) (point))))))))

(defun groovy--trim-current-line ()
  "Return the current code line trimmed and without comments."
  (s-trim (groovy--remove-comments (groovy--current-line))))

(defun groovy-indent-line ()
  "Indent the current line according to the number of parentheses."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation)))
         (syntax-bol (syntax-ppss (line-beginning-position)))
         (multiline-string-p (nth 3 syntax-bol))
         (multiline-comment-p (nth 4 syntax-bol))
         (current-paren-depth (groovy--effective-paren-depth (line-beginning-position)))
         (current-paren-pos (nth 1 syntax-bol))
         (current-paren-character
          (when (nth 1 syntax-bol) (char-after (nth 1 syntax-bol))))
         (text-after-paren
          (when current-paren-pos
            (save-excursion
              (goto-char current-paren-pos)
              (s-trim
               (groovy--remove-comments
                (buffer-substring
                 (1+ current-paren-pos)
                 (line-end-position)))))))
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
           ;; ensure we don't indent closures
           (not (string-match (rx "->" eol) text-after-paren)))
      (let (open-paren-column)
        (save-excursion
          (goto-char current-paren-pos)
          (setq open-paren-column (current-column)))
        (indent-line-to (1+ open-paren-column))))

     ;; Indent according to the number of parens.
     (t
      (let ((indent-level current-paren-depth))
        ;; If the previous line ended with an arithmetic operator like
        ;; `foo +`, then this line should be indented one more level.
        (save-excursion
          (let* ((prev-line (groovy--backwards-to-prev-code-line))
                 (line-end (line-end-position))
                 ;; Check if the last thing is a slashy-string end, so we
                 ;; distinguish a string `/foo bar/` from arithmetic `x /`.
                 (end-slashy-string (and
                                     prev-line
                                     (eq (char-before line-end) ?/)
                                     (groovy--in-string-at-p (- line-end 1)))))
            (when (and
                   prev-line
                   (not end-slashy-string)
                   (not (s-matches-p groovy--case-regexp prev-line))
                   (or (groovy--ends-with-infix-p prev-line)
                       (and (groovy--ends-with-comma-p prev-line)
                            (not (memq current-paren-character (list ?\[ ?\()))
                            (not has-closing-paren))))
              (setq indent-level (1+ indent-level)))))

        ;; If the previous lines are block statements (if, for, while, else)
        ;; without the optional curly brace and without a body, then indent
        ;; for each block.
        (save-excursion
          (let (next-line)
            (while
                (and
                 (setq line-after (groovy--trim-current-line))
                 (groovy--backwards-to-prev-code-line)
                 
                 ;; Handle the special case of a chain of if-else statements, e.g.
                 ;;   if (x) foo()
                 ;;   else if (y) bar()
                 ;;   else baz()
                 (progn
                   (while
                       (let ((cur-line (groovy--trim-current-line)))
                         (and (s-starts-with-p "else" line-after)
                              (string-match (rx line-start (optional "else" (+ blank)) "if" symbol-end) cur-line)
                              (groovy--backwards-to-prev-code-line)
                              (setq line-after cur-line))))
                   t)
                 
                 (groovy--line-ends-with-incomplete-block-statement-p))
                (setq indent-level (1+ indent-level)))))
        
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
  ;; if `groovy-highlight-assignments' add to keyword search.
  (when groovy-highlight-assignments
    (add-to-list 'groovy-font-lock-keywords
          '(groovy-variable-assignment-search 1 font-lock-variable-name-face) t))
  (set (make-local-variable 'font-lock-defaults)
       '(groovy-font-lock-keywords))

  ;; set electric characters
  (setq-local electric-indent-chars
              (append "{}():;,[]" electric-indent-chars))

  (set (make-local-variable 'syntax-propertize-function)
       groovy-syntax-propertize-function)
  (setq imenu-generic-expression groovy-imenu-regexp)
  (set (make-local-variable 'indent-line-function) #'groovy-indent-line)
  (set (make-local-variable 'comment-start) "//"))

(provide 'groovy-mode)

;;; groovy-mode.el ends here
