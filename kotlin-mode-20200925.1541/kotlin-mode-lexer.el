;;; kotlin-mode-lexer.el --- Major mode for kotlin, lexer -*- lexical-binding: t; -*-

;; Copyright © 2019 taku0

;; Author: taku0 (http://github.com/taku0)
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

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

;;  Lexical level routines.

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'eieio)

;; Terminology:
;;   https://kotlinlang.org/docs/reference/basic-types.html#string-templates
;;   See also doc/string.png.
;;
;;   String template:
;;     A string containing expressions to be evaluated and inserted into the
;;     string at run time.
;;     Example: "1 + 1 = ${1 + 1}" is evaluated to "1 + 1 = 2" at run time.
;;
;;   Template expression:
;;     An expression between ${ and } inside a string.
;;     Suppose a string "aaa${ foo() }bbb${ bar() }ccc",
;;     `foo()' and `bar()' are template expressions.
;;
;;   String chunk:
;;     A part of single-line/multiline string delimited with quotation marks
;;     or template expressions.
;;     Suppose a string "aaa${ foo() }bbb${ bar() }ccc",
;;     "aaa${, }bbb${, and }ccc" are string chunks.
;;
;;     This is not a official term; used only in kotlin-mode.

;;; Text properties

;; See also doc/string_properties.png.
;;
;; Some properties are put by `syntax-propertize-function', that is
;; `kotlin-mode--syntax-propertize'.
;;
;; The beginning of and end of strings, character literals, backquoted
;; identifiers are marked with text property '(syntax-table (15)),
;; which indicates generic string delimiters.  Both single-line
;; strings and multiline strings are marked with it. The brackets
;; surrounding template expressions are also marked with
;; '(syntax-table (15)). The property helps font-lock and the
;; tokenizer to recognize strings.
;;
;; The entire string including template expressions, character
;; literal, backquoted identifiers are marked with text property
;; '(syntax-multiline t).  The property is used by
;; `kotlin-mode--syntax-propertize-extend-region' to avoid scanning
;; from the middle of strings.
;;
;; The brackets surrounding template expressions have text property
;; '(kotlin-property--matching-bracket POS), where POS is the position
;; of the matching bracket.  Strictly speaking, the POS on the closing
;; bracket refers to the dollar sign before the opening
;; parenthesis. The property speeds up the indentation logic.

(defun kotlin-mode--syntax-propertize-extend-region (start end)
  "Return region to be propertized.

The returned region contains the region (START . END).
If the region is not modified, return nil.
Intended for `syntax-propertize-extend-region-functions'."
  (syntax-propertize-multiline start end))

(defun kotlin-mode--syntax-propertize (start end)
  "Update text properties for strings.

Mark the beginning of and the end of single-line/multiline
strings, character literals, backquoted identifiers between the
position START and END as general string delimiters.

Intended for `syntax-propertize-function'."
  (remove-text-properties start end
                          '(syntax-table
                            nil
                            syntax-multiline
                            nil
                            kotlin-property--matching-bracket
                            nil
                            kotlin-property--interpolation
                            nil))
  (let* ((chunk (kotlin-mode--chunk-after (syntax-ppss start))))
    (cond
     ((kotlin-mode--chunk-multiline-string-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "\"\"\""))

     ((kotlin-mode--chunk-single-line-string-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "\""))

     ((kotlin-mode--chunk-character-literal-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "'"))

     ((kotlin-mode--chunk-backquoted-identifier-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "`"))

     ((kotlin-mode--chunk-comment-p chunk)
      (goto-char (kotlin-mode--chunk-start chunk))
      (forward-comment (point-max)))))

  (kotlin-mode--syntax-propertize-scan end 0))

(defun kotlin-mode--syntax-propertize-scan (end nesting-level)
  "Update text properties for strings.

Mark the beginning of and the end of single-line/multiline
strings and character literals between the current position and
END as general string delimiters.

Assuming the cursor is not on strings, character-literal,
backquoted identifier, nor comments.

If NESTING-LEVEL is non-zero, nesting of brackets are tracked and
the scan stops where the level becomes zero."
  (let ((found-matching-bracket nil)
        (pattern (rx (or "\"\"\"" "\"" "//" "/*" "{" "}" "'" "`"))))
    (while (and (not found-matching-bracket)
                (< (point) end)
                (search-forward-regexp pattern end t))
      (cond
       ((member (match-string-no-properties 0) '("\"\"\"" "\"" "'" "`"))
        (let ((start (match-beginning 0))
              (quotation (match-string-no-properties 0)))
          (put-text-property start (1+ start)
                             'syntax-table
                             (string-to-syntax "|"))
          (kotlin-mode--syntax-propertize-end-of-string end quotation)
          (put-text-property start (point) 'syntax-multiline t)

          (when (equal quotation "`")
            ;; Backquotes cannot be escaped. So declares the backslashes in
            ;; the identifier are not a escape-syntax characters.
            (put-text-property (1+ start) (1- (point))
                               'syntax-table
                               (string-to-syntax "w")))))

       ((equal "//" (match-string-no-properties 0))
        (goto-char (match-beginning 0))
        (forward-comment (point-max)))

       ((equal "/*" (match-string-no-properties 0))
        (goto-char (match-beginning 0))
        (forward-comment (point-max)))

       ((and (equal "{" (match-string-no-properties 0))
             (/= nesting-level 0))
        (setq nesting-level (1+ nesting-level)))

       ((and (equal "}" (match-string-no-properties 0))
             (/= nesting-level 0))
        (setq nesting-level (1- nesting-level))
        (when (= nesting-level 0)
          (setq found-matching-bracket t)))))
    (unless found-matching-bracket
      (goto-char end))
    found-matching-bracket))

(defun kotlin-mode--syntax-propertize-end-of-string (end quotation)
  "Move point to the end of single-line/multiline string.

Assuming the cursor is on a string, a character literal, or a backquoted
identifier.
If the string go beyond END, stop there.
The string should be terminated with QUOTATION."
  (if (and (< (point) end)
           (search-forward-regexp
            (rx-to-string
             `(or ,quotation
                  "${"
                  (and "$" (or
                            (and (char alpha "_") (* (char alnum "_")))
                            (and "`" (+ (not (any "`\n"))) "`")))))
            end t))
      (cond
       ((and (equal quotation (match-string-no-properties 0))
             (or (equal quotation "`")  ; backquotes cannot be escaped
                 (not (kotlin-mode--escaped-p (match-beginning 0)))))
        (put-text-property (1- (point)) (point)
                           'syntax-table
                           (string-to-syntax "|")))
       ((and (equal "${" (match-string-no-properties 0))
             (member quotation '("\"\"\"" "\""))
             ;; Dollar signs cannot be escaped, so we don't need to check it.
             )
        ;; Found an template expression. Skips the expression.
        ;; We cannot use `scan-sexps' because multiline strings are not yet
        ;; propertized.
        (let ((pos-after-open-bracket (point))
              (start
               (save-excursion
                 (backward-char) ;; {
                 (backward-char) ;; $
                 (point))))
          ;; Declares the open bracket is a generic string delimiter.
          (put-text-property
           (1- pos-after-open-bracket) pos-after-open-bracket
           'syntax-table
           (string-to-syntax "|"))
          (when (kotlin-mode--syntax-propertize-scan end 1)
            ;; Found the matching bracket. Going further.
            ;; Declares the close bracket is a generic string delimiter.
            (put-text-property (1- (point)) (point)
                               'syntax-table
                               (string-to-syntax "|"))
            ;; Records the positions.
            (put-text-property (1- (point)) (point)
                               'kotlin-property--matching-bracket
                               start)
            (put-text-property start pos-after-open-bracket
                               'kotlin-property--matching-bracket
                               (1- (point)))
            (kotlin-mode--syntax-propertize-end-of-string end quotation))))
       ((match-string-no-properties 0)
        (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                           'kotlin-property--interpolation
                           (match-data))
        (kotlin-mode--syntax-propertize-end-of-string end quotation))
       (t
        (kotlin-mode--syntax-propertize-end-of-string end quotation)))
    (goto-char end)))

(defun kotlin-mode--escaped-p (position)
  "Return t if the POSITION in a string is escaped.

A position is escaped if it is proceeded by odd number of backslashes.
Return nil otherwise."
  (let ((p position)
        (backslash-count 0))
    (while (eq (char-before p) ?\\)
      (setq backslash-count (1+ backslash-count))
      (setq p (1- p)))
    (= (mod backslash-count 2) 1)))


;;; Comment or string chunks

(defclass kotlin-mode--chunk ()
  ((type :initarg :type
         :type symbol
         :accessor kotlin-mode--chunk-type
         :documentation "The type of the chunk.

Valid values:
- single-line-string
- multiline-string
- single-line-comment
- multiline-comment
- character-literal
- backquoted-identifier")
   (start :initarg :start
          :type number
          :accessor kotlin-mode--chunk-start
          :documentation "The start position of the chunk."))
  "String-chunks, comments, character literals, or backquoted identifiers.

It have the type and the start position.")

(defun kotlin-mode--chunk-comment-p (chunk)
  "Return non-nil if the CHUNK is a comment."
  (and chunk
       (memq (kotlin-mode--chunk-type chunk)
             '(single-line-comment multiline-comment))))

(defun kotlin-mode--chunk-string-p (chunk)
  "Return non-nil if the CHUNK is a string."
  (and chunk
       (memq (kotlin-mode--chunk-type chunk)
             '(single-line-string multiline-string))))

(defun kotlin-mode--chunk-single-line-comment-p (chunk)
  "Return non-nil if the CHUNK is a single-line comment."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'single-line-comment)))

(defun kotlin-mode--chunk-multiline-comment-p (chunk)
  "Return non-nil if the CHUNK is a multiline comment."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'multiline-comment)))

(defun kotlin-mode--chunk-single-line-string-p (chunk)
  "Return non-nil if the CHUNK is a single-line string."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'single-line-string)))

(defun kotlin-mode--chunk-multiline-string-p (chunk)
  "Return non-nil if the CHUNK is a multiline string."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'multiline-string)))

(defun kotlin-mode--chunk-character-literal-p (chunk)
  "Return non-nil if the CHUNK is a character literal."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'character-literal)))

(defun kotlin-mode--chunk-backquoted-identifier-p (chunk)
  "Return non-nil if the CHUNK is a backquoted identifier."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'backquoted-identifier)))

(defun kotlin-mode--chunk-after (&optional parser-state)
  "Return the chunk at the cursor.

If the cursor is outside of strings and comments, return nil.

If PARSER-STATE is given, it is used instead of (syntax-ppss)."
  (save-excursion
    (when (number-or-marker-p parser-state)
      (goto-char parser-state))
    (when (or (null parser-state) (number-or-marker-p parser-state))
      (setq parser-state (save-excursion (syntax-ppss parser-state))))
    (cond
     ((nth 3 parser-state)
      ;; Syntax category "|" is attached to both single-line and multiline
      ;; string delimiters.  So (nth 3 parser-state) may be t even for
      ;; single-line string delimiters.
      (if (save-excursion (goto-char (nth 8 parser-state))
                          (looking-at "\"\"\""))
          (make-instance 'kotlin-mode--chunk
                         :type 'multiline-string
                         :start (nth 8 parser-state))
        (make-instance 'kotlin-mode--chunk
                       :type 'single-line-string
                       :start (nth 8 parser-state))))
     ((eq (nth 4 parser-state) t)
      (make-instance 'kotlin-mode--chunk
                     :type 'single-line-comment
                     :start (nth 8 parser-state)))
     ((nth 4 parser-state)
      (make-instance 'kotlin-mode--chunk
                     :type 'multiline-comment
                     :start (nth 8 parser-state)))
     ((and (eq (char-before) ?/) (eq (char-after) ?/))
      (make-instance 'kotlin-mode--chunk
                     :type 'single-line-comment
                     :start (1- (point))))
     ((and (eq (char-before) ?/) (eq (char-after) ?*))
      (make-instance 'kotlin-mode--chunk
                     :type 'multiline-comment
                     :start (1- (point))))
     (t
      nil))))

(provide 'kotlin-mode-lexer)

;;; kotlin-mode-lexer.el ends here
