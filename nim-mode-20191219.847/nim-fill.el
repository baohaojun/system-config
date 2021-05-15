;;; nim-fill.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:
(require 'nim-smie)

;;; Fill Paragraph ;;;
(defcustom nim-fill-comment-function 'nim-fill-comment
  "Function to fill comments.
This is the function used by `nim-fill-paragraph' to
fill comments."
  :type 'symbol
  :group 'nim)

(defcustom nim-fill-string-function 'nim-fill-string
  "Function to fill strings.
This is the function used by `nim-fill-paragraph' to
fill strings."
  :type 'symbol
  :group 'nim)

(defcustom nim-fill-decorator-function 'nim-fill-decorator
  "Function to fill decorators.
This is the function used by `nim-fill-paragraph' to
fill decorators."
  :type 'symbol
  :group 'nim)

(defcustom nim-fill-paren-function 'nim-fill-paren
  "Function to fill parens.
This is the function used by `nim-fill-paragraph' to
fill parens."
  :type 'symbol
  :group 'nim)

(defcustom nim-fill-docstring-style 'pep-257
  "Style used to fill docstrings.
This affects `nim-fill-string' behavior with regards to
triple quotes positioning.

Possible values are `django', `onetwo', `pep-257', `pep-257-nn',
`symmetric', and nil.  A value of nil won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

`django':

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`onetwo':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257-nn':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`symmetric':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice
          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257 with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :group 'nim
  :safe (lambda (val)
          (memq val '(django onetwo pep-257 pep-257-nn symmetric nil))))

(defun nim-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified."
  (interactive "P")
  (save-excursion
    (cond
     ;; Comments
     ((nim-syntax-context 'comment)
      (funcall nim-fill-comment-function justify))
     ;; Strings/Docstrings
     ((save-excursion (or (nim-syntax-context 'string)
                          (equal (string-to-syntax "|")
                                 (syntax-after (point)))))
      (funcall nim-fill-string-function justify))
     ;; Decorators
     ((equal (char-after (save-excursion
                           (nim-nav-beginning-of-statement))) ?@)
      (funcall nim-fill-decorator-function justify))
     ;; Parens
     ((or (nim-syntax-context 'paren)
          (looking-at (nim-rx open-paren))
          (save-excursion
            (skip-syntax-forward "^(" (line-end-position))
            (looking-at (nim-rx open-paren))))
      (funcall nim-fill-paren-function justify))
     (t t))))

(defun nim-fill-comment (&optional justify)
  "Comment fill function for `nim-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (fill-comment-paragraph justify))

(defun nim-fill-string (&optional justify)
  "String fill function for `nim-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (let* ((str-start-pos
          (set-marker
           (make-marker)
           (or (nim-syntax-context 'string)
               (and (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (point)))))
         (num-quotes (nim-syntax-count-quotes
                      (char-after str-start-pos) str-start-pos))
         (str-end-pos
          (save-excursion
            (goto-char (+ str-start-pos num-quotes))
            (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                (goto-char (point-max)))
            (point-marker)))
         (multi-line-p
          ;; Docstring styles may vary for oneliners and multi-liners.
          (> (count-matches "\n" str-start-pos str-end-pos) 0))
         (delimiters-style
          (pcase nim-fill-docstring-style
            ;; delimiters-style is a cons cell with the form
            ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
            ;; is NIL means to not add any newlines for start or end
            ;; of docstring.  See `nim-fill-docstring-style' for a
            ;; graphic idea of each style.
            (`django (cons 1 1))
            (`onetwo (and multi-line-p (cons 1 2)))
            (`pep-257 (and multi-line-p (cons nil 2)))
            (`pep-257-nn (and multi-line-p (cons nil 1)))
            (`symmetric (and multi-line-p (cons 1 1)))))
         (fill-paragraph-function))
    (save-restriction
      (narrow-to-region str-start-pos str-end-pos)
      (fill-paragraph justify))
    (save-excursion
      (when (and (nim-docstring-p) nim-fill-docstring-style)
        ;; Add the number of newlines indicated by the selected style
        ;; at the start of the docstring.
        (goto-char (+ str-start-pos num-quotes))
        (delete-region (point) (progn
                                 (skip-syntax-forward "> ")
                                 (point)))
        (and (car delimiters-style)
             (or (newline (car delimiters-style)) t)
             ;; Indent only if a newline is added.
             (indent-according-to-mode))
        ;; Add the number of newlines indicated by the selected style
        ;; at the end of the docstring.
        (goto-char (if (not (= str-end-pos (point-max)))
                       (- str-end-pos num-quotes)
                     str-end-pos))
        (delete-region (point) (progn
                                 (skip-syntax-backward "> ")
                                 (point)))
        (and (cdr delimiters-style)
             ;; Add newlines only if string ends.
             (not (= str-end-pos (point-max)))
             (or (newline (cdr delimiters-style)) t)
             ;; Again indent only if a newline is added.
             (indent-according-to-mode))))) t)

(defun nim-fill-decorator (&optional _justify)
  "Decorator fill function for `nim-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  t)

(defun nim-fill-paren (&optional justify)
  "Paren fill function for `nim-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (nim-syntax-context 'paren)
                          (goto-char (1- (point))))
                        (line-beginning-position))
                      (progn
                        (when (not (nim-syntax-context 'paren))
                          (end-of-line)
                          (when (not (nim-syntax-context 'paren))
                            (skip-syntax-backward "^)")))
                        (while (and (nim-syntax-context 'paren)
                                    (not (eobp)))
                          (goto-char (1+ (point))))
                        (point)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (nim--indent-line-core)
      (goto-char (line-end-position))))
  t)

(provide 'nim-fill)
;;; nim-fill.el ends here
