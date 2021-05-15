;;; merlin-eldoc.el --- eldoc for OCaml and Reason  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Louis Roché

;; Author: Louis Roché <louis@louisroche.net>
;; Created: 27 April 2018
;; Version: 1.3
;; Package-Version: 20190830.517
;; Package-Commit: db7fab1eddfe34781b7e79694f8923b285698032
;; Keywords: merlin ocaml languages eldoc
;; Homepage: https://github.com/khady/merlin-eldoc
;; Package-Requires: ((emacs "24.4") (merlin "3.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; merlin-eldoc is a wrapper of the Emacs merlin mode.  It enables
;; automatic display of information available in merlin for OCaml
;; code.

;; Installation:
;; You need merlin-mode and merlin installed on your
;; system (ocamlmerlin binary) for merlin-eldoc to work.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'eldoc)
(require 'newcomment)
(require 'merlin)

(defgroup merlin-eldoc nil
  "Eldoc for OCaml/Reasonml based on merlin."
  :group 'merlin
  :prefix "merlin-eldoc-")

(defface merlin-eldoc-occurrences-face
  '((t :inherit merlin-type-face))
  "Face for highlighting occurrences.")

(defcustom merlin-eldoc-type t
  "Enable display of type for the thing at point."
  :type 'boolean)

(defcustom merlin-eldoc-doc t
  "Enable display of documentation for the thing at point."
  :type 'boolean)

(defcustom merlin-eldoc-function-arguments t
  "Enable display of expected arguments when calling a function."
  :type 'boolean)

(defcustom merlin-eldoc-occurrences t
  "Enable highlight of other occurrences of the thing at point."
  :type 'boolean)

(defcustom merlin-eldoc-delimiter "     "
  "Delimiter between type and documentation if both are to be displayed."
  :type 'string)

(defcustom merlin-eldoc-truncate-marker "…"
  "Marker used to show when the documentation has been truncated."
  :type 'string)

(defcustom merlin-eldoc-skip-on-merlin-error t
  "Don't show anything if merlin marked the area where the point is as an error.
If nil it is possible that eldoc and merlin will fight to show
information and error at the same time.  Only one tool can win."
  :type 'boolean)

(defcustom merlin-eldoc-max-lines
  (cond ((equal eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) 8)
        ((equal eldoc-echo-area-use-multiline-p t) 8)
        ((equal eldoc-echo-area-use-multiline-p nil) 1))
  "Maximum number of lines that will be used to display information.

The number of lines used by result also depends on
`eldoc-echo-area-use-multiline-p'.

If the value of `eldoc-echo-area-use-multiline-p' is nil, this
setting will be ignored and result will be a single line.  If
value is truncate-sym-name-if-fit, result will be as compact as
possible.  For example if type and documentation are both one
line long and can be combined to fit on a single line, they will
be merged into one line.  Otherwise `merlin-eldoc-max-lines' will
be respected."
  :type 'integer)

(defcustom merlin-eldoc-max-lines-function-arguments merlin-eldoc-max-lines
  "Maximum number of lines the arguments of the function can use.

If value is more than `merlin-eldoc-max-lines', it is replaced by
`merlin-eldoc-max-lines'.  If less than 1, it is replaced by 1."
  :type 'integer)

(defcustom merlin-eldoc-max-lines-type merlin-eldoc-max-lines
  "Maximum number of lines the type can use.

If value is more than `merlin-eldoc-max-lines', it is replaced by
`merlin-eldoc-max-lines'.  If less than 1, it is replaced by 1.

See documentation of `merlin-eldoc-max-lines-doc' for more
details on interaction between `merlin-eldoc-max-lines-type' and
`merlin-eldoc-max-lines-doc'"
  :type 'integer)

(defcustom merlin-eldoc-max-lines-doc merlin-eldoc-max-lines
  "Maximum number of lines the documentation can use.

If value is fit, the documentation is wrapped into one line and
then truncated.  It is displayed a line with the type.

If value is single, the same behavior is applied except that
documentation will use a dedicated line.

If documentation contains more lines and can't be wrapped into
one line, it will be truncated to the maximum number of lines
allowed.

When both type and documentation are to be displayed, type takes
priority over documentation.

For example with the given configuration:

  - `merlin-eldoc-max-lines': 5
  - `merlin-eldoc-max-lines-doc': 4
  - `merlin-eldoc-max-lines-type': 4

If the type is 3 lines long, only 2 lines will be available for
the documentation."
  :type '(radio (integer :tag "number of lines'")
                (const :tag "dedicate a single line to the doc" single)
                (const :tag "fit the type and doc on one line" fit)))

(defcustom merlin-eldoc-type-verbosity 'max
  "How verbose the description of the type will be.
Based on the merlin feature allowing to call
`merlin-type-enclosing' multiple times on the same value and get
a type with more and more details."
  :type '(radio (const :tag "minimal" min)
                (const :tag "maximal" max)))

;;; Utils

;; imported from evil-matchit
(defun merlin-eldoc--current-font-among-fonts-p (pos fonts)
  "If current font at POS is among FONTS."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

(defun merlin-eldoc--in-comment-p (pos)
  "Return non-nil if character at POS is comment or documentation.
This is done by comparing font face.  So a mode such as
`tuareg-mode' or `reason-mode' must be activated in the buffer
before to call this function."
  (merlin-eldoc--current-font-among-fonts-p pos '(font-lock-comment-face
                                                  font-lock-comment-delimiter-face
                                                  font-lock-doc-face)))

(defun merlin-eldoc--in-string-p (pos)
  "Return non-nil if character at POS is string.
This is done by comparing font face.  So a mode such as
`tuareg-mode' or `reason-mode' must be activated in the buffer
before to call this function."
  (merlin-eldoc--current-font-among-fonts-p pos '(font-lock-string-face)))

(defun merlin-eldoc--in-keyword-p (pos)
  "Return non-nil if character at POS is keyword.
This is done by comparing font face.  So a mode such as
`tuareg-mode' or `reason-mode' must be activated in the buffer
before to call this function."
  (merlin-eldoc--current-font-among-fonts-p pos '(tuareg-font-lock-governing-face
                                                  font-lock-keyword-face)))

(defun merlin-eldoc--in-operator-p (pos)
  "Return non-nil if character at POS is operator.
This is done by comparing font face.  So a mode such as
`tuareg-mode' or `reason-mode' must be activated in the buffer
before to call this function."
  (merlin-eldoc--current-font-among-fonts-p pos '(tuareg-font-lock-operator-face)))

(defun merlin-eldoc--valid-type-position-p (pos)
  "Return non-nil if POS is in a place valid to get a type."
  (let ((symbol (thing-at-point 'symbol))
        (operator (merlin-eldoc--in-operator-p pos))
        (string (merlin-eldoc--in-string-p pos))
        (comment (merlin-eldoc--in-comment-p pos))
        (keyword (merlin-eldoc--in-keyword-p pos)))
    (and (or symbol operator string)
         (not comment)
         (or (not keyword) string))))

(defun merlin-eldoc--valid-fun-args-position-p (pos)
  "Return non-nil if POS is in a place valid to get a type."
  (let ((symbol (thing-at-point 'symbol))
        (string (merlin-eldoc--in-string-p pos))
        (comment (merlin-eldoc--in-comment-p pos)))
    (and (not symbol)
         (not comment)
         (not string))))

(defun merlin-eldoc--minibuffer-width ()
  "Get writable width of the minibuffer."
  ;; Subtract 1 from window width since emacs will not write
  ;; any chars to the last column, or in later versions, will
  ;; cause a wraparound and resize of the echo area.
  (1- (window-width (minibuffer-window))))

(defun merlin-eldoc--fontify (s)
  "Fontify the string S."
  (merlin/display-in-type-buffer s)
  (with-current-buffer merlin-type-buffer-name
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))

(defvar merlin-eldoc--doc-error-messages
  '("No documentation available"
    "Not a valid identifier"
    "Not in environment '.*'"
    "is a builtin, no documentation is available")
  "List of regexp used to match invalid values for the documentation.")

(defun merlin-eldoc--skip-doc-p (doc)
  "Look for invalid values of DOC based on value `merlin-eldoc--doc-error-messages'."
  (cl-find-if (lambda (s) (string-match-p s doc))
              merlin-eldoc--doc-error-messages))

(defun merlin-eldoc--merlin-error-at-point-p ()
  "Return non-nil if place under point is marked as error by merlin."
  (when merlin-mode
    (let* ((pos (point)) (beg pos) (end (1+ pos))
           (errors (overlays-in beg end)))
      (cl-find-if #'merlin--overlay-pending-error errors))))

(defun merlin-eldoc--wrap (text)
  "Trim all lines of TEXT and merge them in one line."
  (string-join (delete "" (split-string text)) " "))

(defun merlin-eldoc--wrap-line (text &optional doc)
  "Wrap TEXT to fit on a single line.
If DOC is non-nil, take comment delimiters into account."
  (let* ((com-len (+ (length comment-start) (length comment-end)))
         (delimiters (if doc com-len 0))
         (text (merlin-eldoc--wrap text))
         (max-width (- (merlin-eldoc--minibuffer-width) delimiters))
         (max-trunc (- max-width (length merlin-eldoc-truncate-marker))))
    (if (> (length text) max-width)
        (concat (substring text 0 max-trunc) merlin-eldoc-truncate-marker)
      text)))

(defun merlin-eldoc--split-line (line max-len)
  "Split LINE into a list of lines not larger than MAX-LEN.
This function does not detect the presence of existing line
breaks and so doesn't preserve existing lines.  In general it
should be used with text which is only on one line."
  (let ((len (length line)))
    (if (> len max-len)
        (let* ((trunc-position-rev (string-match split-string-default-separators
                                                 (reverse line) (- len max-len)))
               (trunc-position (if trunc-position-rev (- len trunc-position-rev) max-len))
               (beginning (substring line 0 (1- trunc-position)))
               (end (substring line trunc-position len)))
          (cons beginning (merlin-eldoc--split-line end max-len)))
      (list line))))

(defun merlin-eldoc--text-lines (text &optional max-len)
  "Return the list of all lines from TEXT.
If a line is longer than MAX-LEN, it is turned into multiple
lines.  If MAX-LEN is not provided, it is the width of the
minibuffer."
  (let* ((max-len (if max-len max-len (merlin-eldoc--minibuffer-width)))
         (lines (delete "" (split-string text "[\r\n]+")))
         (lines (mapcar (lambda (l) (merlin-eldoc--split-line l max-len)) lines)))
    (apply #'append lines)))

(defun merlin-eldoc--count-lines (text)
  "Count number of line breaks in string TEXT."
  (cl-count ?\n text))

;;; Main logic

(defvar-local merlin-eldoc--max-lines merlin-eldoc-max-lines
  "Local copy of `merlin-eldoc-max-lines' adjusted to follow eldoc config.")

(defvar-local merlin-eldoc--max-lines-type merlin-eldoc-max-lines-type
  "Local copy of `merlin-eldoc-max-lines-type' adjusted to follow max lines.")

(defvar-local merlin-eldoc--max-lines-doc merlin-eldoc-max-lines-doc
  "Local copy of `merlin-eldoc-max-lines-doc' adjusted to follow max lines.")

(defvar-local merlin-eldoc--max-lines-fun-args
  merlin-eldoc-max-lines-function-arguments
  "Local copy of `merlin-eldoc-max-lines-function-arguments' adjusted to follow max lines.")

(defun merlin-eldoc--adjust-max-len ()
  "Adjust max len variables based on eldoc settings."
  ;; merlin-eldoc--max-lines
  (setq merlin-eldoc--max-lines merlin-eldoc-max-lines)
  (when (or (not eldoc-echo-area-use-multiline-p)
            (< merlin-eldoc--max-lines 1))
    (setq-local merlin-eldoc--max-lines 1))
  ;; merlin-eldoc--max-lines-type
  (when (> merlin-eldoc-max-lines-type merlin-eldoc--max-lines)
    (setq-local merlin-eldoc--max-lines-type merlin-eldoc--max-lines))
  (when (< merlin-eldoc-max-lines-type 1)
    (setq-local merlin-eldoc--max-lines-type 1))
  ;; merlin-eldoc--max-lines-doc
  (when (and (integerp merlin-eldoc-max-lines-doc)
             (> merlin-eldoc-max-lines-doc merlin-eldoc--max-lines))
    (setq-local merlin-eldoc--max-lines-doc merlin-eldoc--max-lines))
  ;; merlin-eldoc--max-lines-fun-args
  (when (> merlin-eldoc-max-lines-function-arguments merlin-eldoc--max-lines)
    (setq-local merlin-eldoc--max-lines-fun-args merlin-eldoc--max-lines))
  (when (< merlin-eldoc-max-lines-function-arguments 1)
    (setq-local merlin-eldoc--max-lines-fun-args 1)))

(defun merlin-eldoc--raw-type (&optional no-reset-verbosity)
  "Return a string containing type of the symbol at point.
Verbosity is reset unless NO-RESET-VERBOSITY is passed and
non-nil."
  (when merlin-eldoc-type
    ;; reset verbosity to not display deeper types
    (unless no-reset-verbosity
      (setq merlin--verbosity-cache nil))
    (if (region-active-p)
        (merlin--type-region)
      (merlin--type-enclosing-query)
      (when merlin-enclosing-types
        (let* ((data (elt merlin-enclosing-types merlin-enclosing-offset))
               (type (merlin--type-enclosing-text data)))
          (when (cddr data) type))))))

(defun merlin-eldoc--format-type (type)
  "Return formated TYPE."
  (let ((type-lines (merlin-eldoc--text-lines type)))
    (if (<= (length type-lines) merlin-eldoc--max-lines)
        (string-join type-lines "\n")
      (merlin-eldoc--wrap-line type))))

(defun merlin-eldoc--type ()
  "Return a string containing formated type."
  (let* ((type (merlin-eldoc--raw-type)))
    (when type (merlin-eldoc--format-type type))))

(defun merlin-eldoc--verbose-type ()
  "Return a string containg formated verbose type."
  (let ((previous-type nil)
        (type (merlin-eldoc--raw-type t)))
    (while (not (equal previous-type type))
      (setq previous-type type)
      (setq type (merlin-eldoc--raw-type t)))
    (when type (merlin-eldoc--format-type type))))

(defun merlin-eldoc--raw-doc ()
  "Return a string containing raw documentation of the thing at point."
  (when (and merlin-eldoc-doc (not (merlin-eldoc--in-string-p (point))))
    (let ((doc (merlin--document-pos nil)))
      (when (not (merlin-eldoc--skip-doc-p doc))
        (string-trim doc)))))

(defun merlin-eldoc--shape (type-lines)
  "Return display shape based on TYPE-LINES and eldoc config.
DOC must be a list of lines and TYPE-LINES a count of lines in type.
The value returned is one of:
  - fit (doc and type should fit on a line),
  - single (doc should fit on one dedicated line)
  - multi (doc should fit in multiple lines)."
  (let* ((type-lines (if type-lines type-lines 0))
         (doc-max-lines (- merlin-eldoc--max-lines type-lines))
         (doc-type (cond ((integerp merlin-eldoc--max-lines-doc) 'multi)
                         (t merlin-eldoc--max-lines-doc))))
    (cond ((or (not eldoc-echo-area-use-multiline-p)
               (equal merlin-eldoc--max-lines-doc 'fit))
           'fit)
          ((> type-lines 1)
           (cond ((equal merlin-eldoc--max-lines-doc 'fit) 'fit)
                 ((= doc-max-lines 0) 'fit)
                 ((= doc-max-lines 1) 'single)
                 ((> doc-max-lines 1) doc-type)))
          ((or (equal eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
               (equal doc-type 'fit))
           'fit)
          (eldoc-echo-area-use-multiline-p doc-type))))

(defun merlin-eldoc--format-doc-multi (doc type-lines)
  "Format DOC to fit on multiple lines leaving space for TYPE-LINES."
  (let* ((doc-lines (length doc))
         (doc-max-lines (min (- merlin-eldoc--max-lines type-lines)
                             merlin-eldoc--max-lines-doc)))
    (if (> doc-lines doc-max-lines)
        (concat
         (string-join (butlast doc (- doc-lines doc-max-lines)) "\n")
         merlin-eldoc-truncate-marker)
      (string-join doc "\n"))))

(defun merlin-eldoc--format-doc-single (doc)
  "Format DOC to fit on a single line."
  (merlin-eldoc--wrap-line doc t))

(defun merlin-eldoc--format-doc-fit-all (doc type-len)
  "Format DOC to fit with a type of length TYPE-LEN."
  (let* ((com-len (+ (length comment-start) (length comment-end)))
         (doc (merlin-eldoc--wrap doc))
         (delimiter-len (if type-len (length merlin-eldoc-delimiter) 0))
         (max-width (- (merlin-eldoc--minibuffer-width) com-len type-len delimiter-len))
         (max-trunc (- max-width (length merlin-eldoc-truncate-marker))))
    (cond ((and (> (length doc) max-width) (>= max-width 10))
           (concat (substring doc 0 max-trunc) merlin-eldoc-truncate-marker))
          ((< max-width 10) nil)
          (t doc))))

(defun merlin-eldoc--format-doc-fit (doc type)
  "Format DOC to fit with TYPE on a single line."
  (merlin-eldoc--format-doc-fit-all doc (if type (length type) 0)))

(defun merlin-eldoc--format-doc-fit-multiline (doc type)
  "Format DOC to fit on one line with multiline TYPE."
  (let* ((type-nl (if type (string-match "\n" (reverse type))))
         (type-len (if type-nl type-nl 0)))
    (merlin-eldoc--format-doc-fit-all doc type-len)))

(defun merlin-eldoc--format-doc (doc &optional type)
  "Format DOC for display in echo area.  Adapt to TYPE if provided."
  (let* ((doc-split (delete "" (merlin-eldoc--text-lines doc)))
         (type-lines (if type (merlin-eldoc--count-lines type) 0))
         (shape (merlin-eldoc--shape type-lines))
         (doc (cond
               ((equal shape 'multi) (merlin-eldoc--format-doc-multi doc-split type-lines))
               ((equal shape 'single) (merlin-eldoc--format-doc-single doc))
               ((and (equal shape 'fit) (> type-lines 1))
                (merlin-eldoc--format-doc-fit-multiline doc type))
               ((equal shape 'fit) (merlin-eldoc--format-doc-fit doc type))))
         (doc (if doc (concat comment-start doc comment-end))))
    (cond ((not doc) type)
          ((not type) doc)
          ((equal shape 'fit) (concat type merlin-eldoc-delimiter doc))
          (t (concat doc "\n" type)))))

(defun merlin-eldoc--gather-type-and-doc-info ()
  "Return a string with type and/or document of the thing at point."
  (let* ((type (cond ((equal 'min merlin-eldoc-type-verbosity)
                      (merlin-eldoc--type))
                     ((equal 'max merlin-eldoc-type-verbosity)
                      (merlin-eldoc--verbose-type))))
         (doc (merlin-eldoc--raw-doc))
         (output (cond ((and type doc) (merlin-eldoc--format-doc doc type))
                       (type type)
                       (doc (merlin-eldoc--format-doc doc))
                       (t nil))))
    (if output (merlin-eldoc--fontify output))))

(defvar-local merlin-eldoc--last-occurrences nil
  "List of bounds of the form (START . END) for the last symbol that had occurrences.")

(defvar-local merlin-eldoc--last-occurrence-index nil
  "Index of the last occurrence visited in `merlin-eldoc--last-occurrences'.")

(defun merlin-eldoc--store-occurrences (occurrences)
  "Store OCCURRENCES and current position.

OCCURRENCES must be a list of BOUNDS (of the form (START . END)).

This function updates the content of
`merlin-eldoc--last-occurences' and
`merlin-eldoc--last-occurrence-index'.

Occurrences and position are meant to be used by
`merlin-eldoc-jump-to-prev-occurrence' and
`merlin-eldoc-jump-to-next-occurrence'."
  (setq-local merlin-eldoc--last-occurrences (vconcat occurrences))
  (setq-local merlin-eldoc--last-occurrence-index nil)
  (let ((point (point))
        (found-p nil)
        (i 0)
        (len (length merlin-eldoc--last-occurrences)))
    (while (and (not found-p) (< i len))
      (let* ((bounds (aref merlin-eldoc--last-occurrences i))
             (begin (car bounds))
             (end (cdr bounds)))
        (when (and (>= point begin)
                   (<= point end))
          (setq-local merlin-eldoc--last-occurrence-index i)
          (setq found-p t))
        (setq i (1+ i))))))

(defun merlin-eldoc--occurrences ()
  "Produce list of BOUNDS (of the form (START . END)) of occurrences of the symbol at point."
  (merlin/call "occurrences"
               "-identifier-at" (merlin/unmake-point (point))))

(defun merlin-eldoc--highlight-occurrence (bounds face)
  "Create an overlay on BOUNDS (of the form (START . END)) and give FACE."
  (let ((overlay (make-overlay (car bounds) (cdr bounds))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'merlin-eldoc 'occurrences)))

(defun merlin-eldoc--unhighlight-occurrences ()
  "Remove highlights from previously highlighted identifiers."
  (remove-overlays nil nil 'merlin-eldoc 'occurrences))

(defun merlin-eldoc--highlight-occurrences ()
  "Create an overlay on all the occurences of symbol at point."
  (when merlin-eldoc-occurrences
    (merlin-eldoc--unhighlight-occurrences)
    (let* ((occurrences (merlin-eldoc--occurrences))
           (bounds (mapcar #'merlin--make-bounds occurrences)))
      (dolist (occ bounds)
        (merlin-eldoc--highlight-occurrence occ
                                            'merlin-eldoc-occurrences-face))
      (merlin-eldoc--store-occurrences bounds))))

(defun merlin-eldoc--on-overlay-p (id)
  "Return whether point is on a merlin-eldoc overlay of type ID."
  (cl-find-if (lambda (el)
                (eq (overlay-get el 'merlin-eldoc) id))
              (overlays-at (point))))

(defun merlin-eldoc--hl-identifiers-post-command-hook ()
  "Unhighlight if point moves off identifier."
  (unless (merlin-eldoc--on-overlay-p 'occurrences)
    (merlin-eldoc--unhighlight-occurrences)
    (setq-local merlin-eldoc--last-occurrences nil)
    (setq-local merlin-eldoc--last-occurrence-index nil)))

(defun merlin-eldoc--hl-identifiers-before-change-function (_beg _end)
  "Unhighlight any time the buffer changes."
  (merlin-eldoc--unhighlight-occurrences)
  (setq-local merlin-eldoc--last-occurrences nil)
  (setq-local merlin-eldoc--last-occurrence-index nil))

(defun merlin-eldoc--format-args-single (text)
  "Format TEXT content into a string fitting on a single line."
  (let* ((hint (merlin-eldoc--wrap text))
         (max-width (merlin-eldoc--minibuffer-width))
         (max-trunc (- max-width (length merlin-eldoc-truncate-marker))))
    (if (> (length hint) max-width)
        (concat (substring hint 0 max-trunc) merlin-eldoc-truncate-marker)
      hint)))

(defun merlin-eldoc--format-label (label)
  "Format type and name of LABEL."
  (let* ((name (cdr (assoc 'name label)))
         (type (cdr (assoc 'type label)))
         (type (if (string-match-p "->" type) (format "(%s)" type) type)))
    (format "%s:%s" (string-remove-prefix "~" name) type)))

(defun merlin-eldoc--gather-fun-args ()
  "Return a string with expected types for function application."
  (interactive)
  (when merlin-eldoc-function-arguments
    (let* ((data (merlin/call "complete-prefix"
                              "-position" (merlin/unmake-point (point))
                              "-prefix" ""
                              "-doc" "n"))
           (context (cdr (assoc 'context data)))
           (application (and (listp context)
                             (equal (car context) "application")
                             (cadr context)))
           ;; argument type
           (expected-ty (and application
                             (not (string-equal "'_a" (cdr (assoc 'argument_type application))))
                             (cdr (assoc 'argument_type application))))
           ;; labels
           (labels (and application (cdr (assoc 'labels application))))
           (labels (mapcar #'merlin-eldoc--format-label labels))
           (labels (if labels (string-join labels " -> ")))
           (output (cond ((and labels expected-ty)
                          (format "(* expected type *) %s\n(* labels *) %s"
                                  expected-ty labels))
                         (labels (format "(* expected labels *) %s" labels))
                         (expected-ty (format "(* expected type *) %s" expected-ty))
                         (t nil))))
      (when output
        (let* ((lines (merlin-eldoc--text-lines output))
               (lines (length lines))
               (output (if (> lines merlin-eldoc--max-lines-fun-args)
                           (merlin-eldoc--format-args-single output)
                         output)))
          (merlin-eldoc--fontify output))))))

(defun merlin-eldoc--gather-info ()
  "Return a string containing information about the thing at point."
  (interactive)
  (when (and (not (string-equal merlin-type-buffer-name (buffer-name)))
             (not (minibufferp))
             (not (and merlin-eldoc-skip-on-merlin-error
                       (merlin-eldoc--merlin-error-at-point-p))))
    (merlin-eldoc--adjust-max-len)
    (cond ((merlin-eldoc--valid-type-position-p (point))
           (ignore-errors (merlin-eldoc--highlight-occurrences))
           (merlin-eldoc--gather-type-and-doc-info))
          ((merlin-eldoc--valid-fun-args-position-p (point))
           (merlin-eldoc--gather-fun-args))
          (t nil))))

(defun merlin-eldoc-jump-to-prev-occurrence ()
  "Jump to prev occurrence of value at point."
  (interactive)
  (when merlin-eldoc--last-occurrence-index
    (let* ((len (length merlin-eldoc--last-occurrences))
           (i merlin-eldoc--last-occurrence-index)
           (i (1- i))
           (i (if (>= i 0) i (1- len)))
           (bounds (aref merlin-eldoc--last-occurrences i)))
      (setq-local merlin-eldoc--last-occurrence-index i)
      (message "jumping to occurrence %s/%s" (1+ i) len)
      (goto-char (car bounds)))))

(defun merlin-eldoc-jump-to-next-occurrence ()
  "Jump to next occurrence of value at point."
  (interactive)
  (when merlin-eldoc--last-occurrence-index
    (let* ((len (length merlin-eldoc--last-occurrences))
           (i merlin-eldoc--last-occurrence-index)
           (i (1+ i))
           (i (if (< i len) i 0))
           (bounds (aref merlin-eldoc--last-occurrences i)))
      (setq-local merlin-eldoc--last-occurrence-index i)
      (message "jumping to occurrence %s/%s" (1+ i) len)
      (goto-char (car bounds)))))

;;;###autoload
(defun merlin-eldoc-setup ()
  "Setup eldoc for OCaml/Reasonml based on merlin."
  (interactive)
  (setq-local eldoc-documentation-function #'merlin-eldoc--gather-info)
  (add-hook 'post-command-hook
            #'merlin-eldoc--hl-identifiers-post-command-hook nil t)
  (add-hook 'before-change-functions
            #'merlin-eldoc--hl-identifiers-before-change-function nil t)
  (eldoc-mode t))

;;;###autoload
(defun merlin-eldoc-disable ()
  "Cleanup hooks created by merlin-eldoc."
  (interactive)
  (merlin-eldoc--unhighlight-occurrences)
  (setq-local eldoc-documentation-function #'ignore)
  (remove-hook 'post-command-hook
               #'merlin-eldoc--hl-identifiers-post-command-hook t)
  (remove-hook 'before-change-functions
               #'merlin-eldoc--hl-identifiers-before-change-function t))

;;;###autoload
(defun merlin-eldoc-customize ()
  "Open the customize buffer for the group merlin-eldoc."
  (interactive)
  (customize-group 'merlin-eldoc))

(provide 'merlin-eldoc)

;;; merlin-eldoc.el ends here
