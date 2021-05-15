;;; nim-syntax.el --- -*- lexical-binding: t -*-

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

;;; Commentary:

;; Sorry, this implementation is pretty much black magic ish...
;; Maybe understanding Emacs' Syntax Class Table would help.

;; my memo: seems like posix_other_consts.nim is good example to test
;; fortify limitation; adding new face would cause unfinished
;; fortification.  From user's side, they could solve by
;; `revert-buffer', but I'm not sure this is good approach...

;;; Code:
(require 'nim-vars)
(require 'nim-rx)

(defvar nim-font-lock-keywords
  `((,(nim-rx (or line-start ";") (* " ")
              defun  (+ " ")
              (group (or identifier quoted-chars) (* " ") (? (group "*"))))
     (1 (if (match-string 2)
            'nim-font-lock-export-face
          font-lock-function-name-face)
        keep t)
     ;; (8 font-lock-type-face keep t) TODO nim-rx needs a proper colon-type expression
     )

    ;; Highlight everything that starts with a capital letter as type.
    (,(rx symbol-start (char upper) (* (char alnum "_")) symbol-end) . (0 font-lock-type-face keep))

    ;; Warning face for tab characters.
    ("	+" . (0 font-lock-warning-face))

    ;; This only works if it’s one line
    (,(nim-rx (or line-start ";") (* " ")
              (or "var" "let" "const" "type") (+ " ")
              (group (or identifier quoted-chars) (* " ") (? (group "*"))))
     . (1 (if (match-string 2)
              'nim-font-lock-export-face
            font-lock-variable-name-face))))
  "Font lock expressions for Nim mode.")

(defvar nim-font-lock-keywords-extra
  `(;; export properties
    (,(nim-rx
       line-start (1+ " ")
       (? "case" (+ " "))
       (group
        (or identifier quoted-chars) "*"
        (? (and "[" word "]"))
        (0+ (and "," (? (0+ " "))
                 (or identifier quoted-chars) "*")))
       (0+ " ") (or ":" "{." "=") (0+ nonl)
       line-end)
     (1 'nim-font-lock-export-face))
    ;; Number literal
    (,(nim-rx nim-numbers)
     (0 'nim-font-lock-number-face))
    ;; Highlight identifier enclosed by "`"
    (nim-backtick-matcher
     (10 font-lock-constant-face prepend))
    ;; Highlight $# and $[0-9]+ inside string
    (nim-format-$-matcher 0 font-lock-preprocessor-face prepend)
    ;; Highlight word after ‘is’ and ‘distinct’
    (,(nim-rx " " (or "is" "distinct") (+ " ")
              (group identifier))
     (1 font-lock-type-face))
    ;; pragma
    (nim-pragma-matcher . (0 'nim-font-lock-pragma-face)))
  "Extra font-lock keywords.
If you feel uncomfortable because of this font-lock keywords,
set nil to this value by ‘nim-mode-init-hook’.")

(defun nim--convert-to-nim-style-insensitive (str)
  (let ((first-str (substring str 0 1))
        (rest-str  (substring str 1 (length str))))
    (format "%s_?%s" first-str
            (mapconcat
             (lambda (s)
               (if (string-match "[a-zA-Z]" s)
                   (format "[%s%s]" (downcase s) (upcase s))
                 s))
             (split-string rest-str (rx not-word-boundary))
             "_?"))))

(defun nim--format-keywords (keywords)
  (format "\\_<\\(%s\\)\\_>"
          (mapconcat
           'nim--convert-to-nim-style-insensitive
           (cl-typecase keywords
             (symbol (symbol-value keywords))
             (list keywords))
           "\\|")))

(defvar nim-font-lock-keywords-2
  (append
   (cl-loop
    with pairs = `((nim-types . font-lock-type-face)
                   (nim-variables . font-lock-variable-name-face)
                   (nim-exceptions . 'error)
                   (nim-constants . font-lock-constant-face)
                   (nim-builtin-functions . font-lock-builtin-face)
                   (nim-nonoverloadable-builtins . 'nim-non-overloadable-face)
                   (nim-keywords . font-lock-keyword-face))
    for (keywords . face) in pairs
    collect (cons (nim--format-keywords keywords) face))
   `((,(rx symbol-start "result" symbol-end) . font-lock-variable-name-face))))

(defvar nim-font-lock-keywords-3
  (list (cons (nim--format-keywords 'nim-builtins-without-nimscript)
              font-lock-builtin-face)))

(defvar nimscript-keywords
  (append
   `(,(cons (nim--format-keywords 'nimscript-builtins)
            font-lock-builtin-face)
     ,(cons (nim--format-keywords 'nimscript-variables)
            font-lock-variable-name-face))
   `((,(rx symbol-start "task" symbol-end (1+ " ")
           (group  symbol-start (or "build" "tests" "bench") symbol-end))
      (1 font-lock-builtin-face))
     ("\\_<ScriptMode\\_>" (0 font-lock-type-face)))))

(defsubst nim-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defconst nim-syntax-propertize-function
  (syntax-propertize-rules
   ;; single/multi line comment
   ((rx (or (group (or line-start (not (any "]#\"")))
                   (group "#" (? "#") "["))
            (group "]" "#" (? "#"))
            (group "#")))
    (0 (ignore (nim-syntax-commentify))))
   ;; Char
   ;; Put syntax entry("\"") for character type to highlight
   ;; when only the character-delimiter regex matched.
   ((nim-rx character-delimiter)
    (1 "\"")  ; opening quote
    (2 "\"")) ; closing quote
   ;; String
   ((nim-rx string-delimiter)
    (0 (ignore (nim-syntax-stringify))))))

(defun nim-pretty-triple-double-quotes (pbeg pend &optional close-quote)
  (when (and nim-pretty-triple-double-quotes
             (bound-and-true-p prettify-symbols-mode))
    (compose-region pbeg pend
                    (if close-quote
                        (or (cdr nim-pretty-triple-double-quotes)
                            (car nim-pretty-triple-double-quotes))
                      (car nim-pretty-triple-double-quotes)))))

(defun nim-syntax--raw-string-p (pos)
  "Return non-nil if char of before POS is not word syntax class."
  ;; See also #212
  (eq ?w (char-syntax (char-before pos))))

(defun nim-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple double quotes."
  (unless (nth 4 (syntax-ppss))
    (let* ((num-quotes (length (match-string-no-properties 1)))
           (ppss (prog2
                     (backward-char num-quotes)
                     (syntax-ppss)
                   (forward-char num-quotes)))
           (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
           (quote-starting-pos (- (point) num-quotes))
           (quote-ending-pos (point))
           (num-closing-quotes
            (and string-start
                 (nim-syntax-count-quotes
                  (char-before) string-start quote-starting-pos))))
      (cond ((and string-start (= num-closing-quotes 0))
             ;; This set of quotes doesn't match the string starting
             ;; kind. Do nothing.
             nil)
            ((not string-start)
             ;; This set of quotes delimit the start of a string.
             (put-text-property quote-starting-pos (1+ quote-starting-pos)
                                'syntax-table (string-to-syntax "|"))
             (when (eq num-quotes 3)
               (nim-pretty-triple-double-quotes
                quote-starting-pos (+ quote-starting-pos 3))))
            ((and string-start (< string-start (- (point) 2)) ;; avoid r""
                  (not (eq 3 num-closing-quotes))
                  ;; Skip "" in the raw string literal
                  (nim-syntax--raw-string-p string-start)
                  (or
                   ;;  v point is here
                   ;; ""
                   (and
                    (eq ?\" (char-before (1- (point))))
                    (eq ?\" (char-before (point))))
                   ;; v point is here
                   ;; ""
                   (and
                    (eq ?\" (char-before (point)))
                    (eq ?\" (char-after  (point))))))
             nil)
            ((= num-quotes num-closing-quotes)
             ;; This set of quotes delimit the end of a string.
             ;; If there are some double quotes after quote-ending-pos,
             ;; shift the point to right number of `extra-quotes' times.
             (let* ((extra-quotes 0))
               ;; Only count extra quotes when the double quotes is 3 to prevent
               ;; wrong highlight for r"foo""bar" forms.
               (when (eq num-quotes 3)
                 (while (eq ?\" (char-after (+ quote-ending-pos extra-quotes)))
                   (setq extra-quotes (1+ extra-quotes))))
               ;; #212 Change syntax class of "\" before end of double quote because
               ;; Nim support end of "\" in raw string literal.
               ;; """str""" will be handled by regex of string delimiter on nim-rx.el
               (when (and
                      (eq num-closing-quotes 1)
                      (nim-syntax--raw-string-p string-start)
                      (eq ?\\ (char-after (- (point) 2))))
                 (put-text-property (- (point) 2) (1- (point))
                                    'syntax-table (string-to-syntax ".")))

               (let ((pbeg (+ (1- quote-ending-pos) extra-quotes))
                     (pend (+ quote-ending-pos      extra-quotes)))
                 (put-text-property
                  pbeg pend 'syntax-table (string-to-syntax "|"))
                 (when (eq num-quotes 3)
                   (nim-pretty-triple-double-quotes (- pend 3) pend t)))))
            ((> num-quotes num-closing-quotes)
             ;; This may only happen whenever a triple quote is closing
             ;; a single quoted string. Add string delimiter syntax to
             ;; all three quotes.
             (put-text-property quote-starting-pos quote-ending-pos
                                'syntax-table (string-to-syntax "|")))))))

(defun nim-syntax-commentify ()
  "Put comment syntax property for Nim's single and multi line comment."
  (let* ((hash (or (match-string-no-properties 2)
                   (match-string-no-properties 3)
                   (match-string-no-properties 4)))
         (start-pos (- (point) (length hash)))
         (ppss (syntax-ppss))
         (start-len (save-excursion
                      (when (nth 8 ppss)
                        (goto-char (nth 8 ppss))
                        (looking-at "##?\\[")
                        (length (match-string 0))))))
    (cond
     ;; single line comment
     ((and (eq nil (nth 4 ppss)) (eq 1 (length hash)))
      ;; comment start
      (put-text-property start-pos (1+ start-pos)
                         'syntax-table (string-to-syntax "<"))
      ;; comment end
      ;; #112, make sure ‘comment-indent-new-line’ (C-M-j key)
      (put-text-property (point-at-eol) (point-at-eol)
                         'syntax-table (string-to-syntax ">")))
     ;; ignore
     ((or (eq t (nth 4 ppss)) ; t means single line comment
          (<= (length hash) 1)
          ;; don't put syntax comment start or end
          ;; if it’s "#[" or "]#" inside ##[]##
          (and start-len (= 3 start-len) (= 2 (length hash))))
      nil)
     ;; multi comment line start
     ((eq ?# (string-to-char hash))
      (put-text-property start-pos (1+ start-pos)
                         'syntax-table (string-to-syntax "< bn")))
     ;; multi comment line end
     ((eq ?\] (string-to-char hash))
      (put-text-property (1- (point)) (point)
                         'syntax-table (string-to-syntax "> bn"))))))

(defun nim-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defun nim-syntax--context-compiler-macro (form type &optional syntax-ppss)
  (pcase type
    (`'comment
     `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
        (and (nth 4 ppss) (nth 8 ppss))))
    (`'string
     `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
        (and (nth 3 ppss) (nth 8 ppss))))
    (`'paren
     `(nth 1 (or ,syntax-ppss (syntax-ppss))))
    (_ form)))

(defun nim-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro nim-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      (`comment (and (nth 4 ppss) (nth 8 ppss)))
      (`string (and (nth 3 ppss) (nth 8 ppss)))
      (`paren (nth 1 ppss))
      (_ nil))))

(defsubst nim-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside 'comment or 'string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst nim-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (= (syntax-class (syntax-after (point)))
     (syntax-class (string-to-syntax ")"))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight matcher

(defun nim-backtick-matcher (&optional limit)
  "Highlight matcher for ``symbol`` in comment."
  (let (res)
    (while
        (and
         (setq res (re-search-forward
                    (nim-rx backticks) limit t))
         (not (nth 4 (syntax-ppss)))))
    res))

(defconst nim--string-interpolation-regex
  ;; I think two digit is enough...
  (rx "$" (or "#" (and (in "1-9") (? num)))))

(defun nim-format-$-matcher (&optional limit)
  "Highlight matcher for $# and $[1-9][0-9]? in string within LIMIT."
  (let (res)
    (while
        (and
         (setq res (re-search-forward
                    nim--string-interpolation-regex limit t))
         (not (nth 3 (syntax-ppss)))))
    res))

(defun nim-inside-pragma-p ()
  (let* ((ppss (syntax-ppss))
         (pos  (nth 1 ppss)))
    (and
     ;; not in comment or string
     (not (or (nth 3 ppss) (nth 4 ppss)))
     ;; there is an open brace
     pos
     ;; open brace is curly
     (eq ?\{ (char-after pos))
     ;; followed by a dot
     (eq ?.  (char-after (1+  pos))))))

(defconst nim-pragma-regex (nim--format-keywords (mapcar 'car nim-pragmas)))

(defun nim-pragma-matcher (&optional limit)
  "Highlight pragma."
  (let (res)
    (while
        (and
         (setq res (re-search-forward
                    nim-pragma-regex limit t))
         (not (nim-inside-pragma-p))))
    res))

(provide 'nim-syntax)
;;; nim-syntax.el ends here
