;;; relint.el --- Elisp regexp mistake finder   -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 1.19
;; Package-Requires: ((xr "1.20") (emacs "26.1"))
;; URL: https://github.com/mattiase/relint
;; Keywords: lisp, regexps

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

;; Relint scans elisp files for regexps and reports potential errors,
;; including deprecated syntax and bad practice. See the README file
;; for more information.

;;; News:

;; Version 1.19
;; - Progress indicator in `relint-directory'
;; - Some performance improvements
;; - Fix some false positives in the regexp provenance detector
;; - Scan assignments to `font-lock-defaults' correctly
;; - Recognise regexp arguments to functions in the s.el package
;; Version 1.18:
;; - New check for ineffective backslashes in all strings (not just regexps)
;; - Warnings emitted in order of their position in file or buffer
;; - Performance improvements
;; Version 1.17:
;; - Fixed message display on Emacs 26
;; Version 1.16:
;; - Suppression comments now use regexp matching of messages
;; - New filename-specific checks in calls to `directory-files' etc
;; - Check some keyword arguments (:regexp and :regex)
;; - Improved rx checks
;; - `relint-directory' now displays number of files found
;; Version 1.15:
;; - Improved position accuracy in various lists of regexps
;; - Check for mistake in rx `any' forms
;; - `relint-buffer' now also returns severity (warning, error)
;; - Relint can now also check the *scratch* buffer
;; Version 1.14:
;; - Added `relint-buffer'
;; - Report error position inside string literals when possible
;; - Scan arguments to `search-forward-regexp' and `search-backward-regexp'
;; - Use text quoting for messages
;; Version 1.13:
;; - Look in function/macro doc strings to find regexp arguments and
;;   return values
;; - Detect binding and mutation of some known regexp variables
;; - Detect regexps as arguments to `syntax-propertize-rules'
;; - More font-lock-keywords variables are scanned for regexps
;; - `relint-batch' no longer outputs a summary if there were no errors
;; Version 1.12:
;; - Improved detection of regexps in defcustom declarations
;; - Better suppression of false positives
;; - Nonzero exit status upon error in `relint-batch'
;; Version 1.11:
;; - Improved evaluator, now handling limited local variable mutation
;; - Bug fixes
;; - Test suite
;; Version 1.10:
;; - Check arguments to `skip-syntax-forward' and `skip-syntax-backward'
;; - Add error suppression mechanism
;; Version 1.9:
;; - Limited tracking of local variables in regexp finding
;; - Recognise new variable `literal' and `regexp' rx clauses
;; - Detect more regexps in defcustom declarations
;; - Requires xr 1.13
;; Version 1.8:
;; - Updated diagnostics list
;; - Requires xr 1.12
;; Version 1.7:
;; - Expanded regexp-generating heuristics
;; - Some `defalias' are now followed
;; - All diagnostics are now documented (see README.org)
;; Version 1.6:
;; - Add `relint-current-buffer'
;; - Show relative file names in *relint*
;; - Extended regexp-generating heuristics, warning about suspiciously-named
;;   variables used as skip-sets
;; - "-patterns" and "-pattern-list" are no longer interesting variable
;;   suffixes
;; Version 1.5:
;; - Substantially improved evaluator, able to evaluate some functions and
;;   macros defined in the same file, even when passed as parameters
;; - Detect regexps spliced into [...]
;; - Check bad skip-set provenance
;; - The *relint* buffer now uses a new relint-mode for better usability,
;;   with "g" bound to `relint-again'
;; Version 1.4:
;; - First version after name change to `relint'

;;; Code:

(require 'xr)
(require 'compile)
(require 'cl-lib)

(defun relint--get-error-buffer ()
  "Buffer to which errors are printed, or nil if noninteractive."
  (and (not noninteractive)
       (let ((buf (get-buffer-create "*relint*")))
         (with-current-buffer buf
           (unless (eq major-mode 'relint-mode)
             (relint-mode))
           (let ((inhibit-read-only t))
             (compilation-forget-errors)
             (erase-buffer)))
         buf)))

(defun relint--add-to-error-buffer (error-buffer string)
  (with-current-buffer error-buffer
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert string))))

(defun relint--skip-whitespace ()
  (when (looking-at (rx (1+ (or blank "\n" "\f"
                                (seq ";" (0+ nonl))))))
    (goto-char (match-end 0))))

(defun relint--follow-path (path)
  "Move point forward along PATH (reversed list of list indices
to follow to target).
For example, if point is before the form (A B (C ((D E F G))))
and PATH is (3 0 1 2), then the returned position is right before G."
  (let ((p (reverse path)))
    (while p
      (relint--skip-whitespace)
      (let ((skip (car p)))
        ;; Enter next sexp and skip past the `skip' first sexps inside.
        (cond
         ((looking-at (rx (or "'" "#'" "`" ",@" ",")))
          (goto-char (match-end 0))
          (setq skip (1- skip)))
         ((looking-at (rx "("))
          (forward-char 1)))
        (while (> skip 0)
          (relint--skip-whitespace)
          (if (looking-at (rx "."))
              (progn
                (goto-char (match-end 0))
                (relint--skip-whitespace)
                (cond
                 ((looking-at (rx (or "'" "#'" "`" ",@" ",")))
                  ;; Sugar after dot represents one sexp.
                  (goto-char (match-end 0))
                  (setq skip (1- skip)))
                 ((looking-at (rx "("))
                  ;; `. (' represents zero sexps.
                  (goto-char (match-end 0)))))
            (forward-sexp)
            (setq skip (1- skip)))))
      (setq p (cdr p))))
  (relint--skip-whitespace)
  (when (looking-at (rx "."))
    (forward-char)
    (relint--skip-whitespace)))

(defun relint--pos-from-start-pos-path (start-pos path)
  "Compute position from START-POS and PATH (reversed list of
list indices to follow to target)."
  (save-excursion
    (goto-char start-pos)
    (relint--follow-path path)
    (point)))

(defun relint--literal-string-pos (string-pos n)
  "Position of character N in a literal string at STRING-POS."
  (save-excursion
    (goto-char (1+ string-pos))         ; Skip first double quote.
    (dotimes (_ n)
      ;; Match a single character in a string. Since we already read it,
      ;; we know that it's well-formed.
      (looking-at
       (rx (* ?\\ (any " \n"))   ; Skip escaped space and newline.
           (or (not (any ?\\))   ; Unescaped char.
               (seq ?\\
                    (or (** 1 3 (any "0-7"))              ; Octal.
                        (seq ?x (+ (any "0-9A-Fa-f")))    ; Hex.
                        (seq ?u (= 4 (any "0-9A-Fa-f")))  ; Unicode.
                        (seq ?U (= 8 (any "0-9A-Fa-f")))  ; Unicode.
                        (seq "N{" (+ (not (any "}"))) "}")  ; Named.
                        (seq (any "CMS") "-" anything)    ; Keystroke.
                        anything)))))
      (goto-char (match-end 0)))
    (point)))

(defun relint--string-pos (pos n)
  "Position of character N in a string expression at POS,
or nil if no position could be determined."
  (save-excursion
    (goto-char pos)
    (pcase (read (current-buffer))
      ((pred stringp) (relint--literal-string-pos pos n))
      (`(concat . ,args)
       ;; Find out in which argument the sought position is.
       (let ((index 1))
         (while (and args (stringp (car args)) (>= n (length (car args))))
           (setq n (- n (length (car args))))
           (setq index (1+ index))
           (setq args (cdr args)))
         (and args (stringp (car args))
              (let ((string-pos
                     (relint--pos-from-start-pos-path pos (list index))))
                (relint--literal-string-pos string-pos n))))))))

(defun relint--suppression (pos message)
  "Whether there is a suppression for MESSAGE at POS."
  (save-excursion
    ;; On a preceding line, look for a comment on the form
    ;;
    ;; relint suppression: REGEXP
    ;;
    ;; where REGEXP matches MESSAGE. There can be
    ;; multiple suppression lines preceding a line of code with
    ;; several errors.
    (goto-char pos)
    (forward-line -1)
    (let ((matched nil))
      (while (and
              (not (setq matched
                         (and
                          (looking-at (rx (0+ blank) (1+ ";") (0+ blank)
                                          "relint suppression:" (1+ blank)
                                          (group (0+ nonl)
                                                 (not (any "\n" blank)))))
                          (let ((regexp (match-string 1)))
                            (string-match-p regexp message)))))
              (looking-at (rx bol
                              (0+ blank) (opt ";" (0+ nonl))
                              eol))
              (not (bobp)))
        (forward-line -1))
      matched)))

(defun relint--output-message (error-buffer string)
  (if error-buffer
      (relint--add-to-error-buffer error-buffer (concat string "\n"))
    (message "%s" string)))

(cl-defun relint--output-report (error-buffer file
                                 (message expr-pos error-pos
                                  str str-idx severity))
  (let* ((pos (or error-pos expr-pos))
         (line (line-number-at-pos pos t))
         (col (save-excursion
                (goto-char pos)
                (1+ (current-column)))))
    (relint--output-message
     error-buffer
     (concat
      (format "%s:%d:%d: " file line col)
      (and (eq severity 'error) "error: ")
      message
      (and str-idx (format " (pos %d)" str-idx))
      (and str     (format "\n  %s" (relint--quote-string str)))
      (and str-idx (format "\n   %s" (relint--caret-string str str-idx)))))))
  
(defun relint--output-complaints (buffer file complaints error-buffer)
  (with-current-buffer buffer
    (dolist (complaint complaints)
      (relint--output-report error-buffer file complaint))))

(defvar relint--suppression-count)
(defvar relint--complaints)

(defun relint--report (start-pos path message str str-idx severity)
  (let* ((expr-pos (relint--pos-from-start-pos-path start-pos path))
         (error-pos (and str-idx (relint--string-pos expr-pos str-idx))))
    (if (relint--suppression expr-pos message)
        (setq relint--suppression-count (1+ relint--suppression-count))
      (push (list message expr-pos error-pos str str-idx severity)
            relint--complaints))))

(defun relint--warn (start-pos path message &optional str str-idx)
  (relint--report start-pos path message str str-idx 'warning))

(defun relint--err (start-pos path message &optional str str-idx)
  (relint--report start-pos path message str str-idx 'error))

(defun relint--escape-string (str escape-printable)
  (replace-regexp-in-string
   ;; Use pair notation for raw chars; "\200-\377" is buggy in Emacs 26.
   (rx (any cntrl ?\177 (#x3fff80 . #x3fffff) ?\\ ?\"))
   (lambda (s)
     (let ((c (logand (string-to-char s) #xff)))
       (or (cdr (assq c '((?\b . "\\b")
                          (?\t . "\\t")
                          (?\n . "\\n")
                          (?\v . "\\v")
                          (?\f . "\\f")
                          (?\r . "\\r")
                          (?\e . "\\e"))))
           (if (memq c '(?\\ ?\"))
               (if escape-printable (string ?\\ c) (string c))
             (format "\\%03o" c)))))
   str t t))

(defun relint--quote-string (str)
  (concat "\"" (relint--escape-string str t) "\""))

(defun relint--caret-string (string pos)
  (let ((quoted-pos
         (length (relint--escape-string (substring string 0 pos) t))))
    (concat (make-string quoted-pos ?.) "^")))

(defun relint--check-string (string checker name pos path)
  (let ((complaints
         (condition-case err
             (funcall checker string)
           (error
            (relint--err pos path
                         (format "In %s: %s" name (cadr err))
                         string nil)
            nil))))
    (dolist (c complaints)
      (relint--warn pos path (format "In %s: %s" name (cdr c))
                    string (car c)))))

(defun relint--check-skip-set (skip-set-string name pos path)
  (relint--check-string skip-set-string #'xr-skip-set-lint name pos path))

(defun relint--check-re-string (re name pos path)
  (relint--check-string re #'xr-lint name pos path))
  
(defun relint--check-file-re-string (re name pos path)
  (relint--check-string re (lambda (x) (xr-lint x 'file)) name pos path))
  
(defun relint--check-syntax-string (syntax name pos path)
  (relint--check-string syntax #'relint--syntax-string-lint name pos path))

(defconst relint--syntax-codes
  '((?-  . whitespace)
    (?\s . whitespace)
    (?.  . punctuation)
    (?w  . word)
    (?W  . word)       ; undocumented
    (?_  . symbol)
    (?\( . open-parenthesis)
    (?\) . close-parenthesis)
    (?'  . expression-prefix)
    (?\" . string-quote)
    (?$  . paired-delimiter)
    (?\\ . escape)
    (?/  . character-quote)
    (?<  . comment-start)
    (?>  . comment-end)
    (?|  . string-delimiter)
    (?!  . comment-delimiter)))

(defun relint--syntax-string-lint (syntax)
  "Check the syntax-skip string SYNTAX.  Return list of complaints."
  (let ((errs nil)
        (start (if (string-prefix-p "^" syntax) 1 0)))
    (when (member syntax '("" "^"))
      (push (cons start "Empty syntax string") errs))
    (let ((seen nil))
      (dolist (i (number-sequence start (1- (length syntax))))
        (let* ((c (aref syntax i))
               (sym (cdr (assq c relint--syntax-codes))))
          (if sym
              (if (memq sym seen)
                  (push (cons i (relint--escape-string
                                 (format-message
                                  "Duplicated syntax code `%c'" c)
                                 nil))
                        errs)
                (push sym seen))
            (push (cons i (relint--escape-string
                           (format-message
                            "Invalid char `%c' in syntax string" c)
                           nil))
                  errs)))))
    (nreverse errs)))

(defvar relint--variables nil
  "Alist of global variable definitions.
Each element is either (NAME expr EXPR), for unevaluated expressions,
or (NAME val VAL), for values.")

;; List of variables that have been checked, so that we can avoid
;; checking direct uses of it.
(defvar relint--checked-variables)

;; Alist of functions taking regexp argument(s).
;; The names map to a list of the regexp argument indices.
(defvar relint--regexp-functions)

;; List of functions defined in the current file, each element on the
;; form (FUNCTION ARGS BODY), where ARGS is the lambda list and BODY
;; its body expression list.
(defvar relint--function-defs)

;; List of macros defined in the current file, each element on the
;; form (MACRO ARGS BODY), where ARGS is the lambda list and BODY its
;; body expression list.
(defvar relint--macro-defs)

;; Alist of alias definitions in the current file.
(defvar relint--alias-defs)

;; Alist of local variables. Each element is either (NAME VALUE),
;; where VALUE is the (evaluated) value, or just (NAME) if the binding
;; exists but the value is unknown.
(defvar relint--locals)

(defvar relint--eval-mutables nil
  "List of local variables mutable in the current evaluation context.")

(defconst relint--safe-functions
  '(cons list append
    concat
    car cdr caar cadr cdar cddr car-safe cdr-safe nth nthcdr
    caaar cdaar cadar cddar caadr cdadr caddr cdddr
    format format-message
    regexp-quote regexp-opt regexp-opt-charset
    reverse
    member memq memql remove remq member-ignore-case
    assoc assq rassoc rassq assoc-string
    identity
    string make-string make-list
    substring
    length safe-length
    symbol-name
    intern intern-soft make-symbol
    null not xor
    eq eql equal
    string-equal string= string< string-lessp string> string-greaterp
    compare-strings
    char-equal string-match-p
    string-match split-string
    wildcard-to-regexp
    combine-and-quote-strings split-string-and-unquote
    string-to-multibyte string-as-multibyte string-to-unibyte string-as-unibyte
    string-join string-trim-left string-trim-right string-trim
    string-prefix-p string-suffix-p
    string-blank-p string-remove-prefix string-remove-suffix
    string-search string-replace
    vector aref elt vconcat
    char-to-string string-to-char
    number-to-string string-to-number int-to-string
    string-to-list string-to-vector string-or-null-p
    upcase downcase capitalize
    purecopy copy-sequence copy-alist copy-tree
    flatten-tree
    member-ignore-case
    last butlast number-sequence
    plist-get plist-member
    1value
    consp atom stringp symbolp listp nlistp booleanp
    integerp numberp natnump fixnump bignump characterp zerop
    sequencep vectorp arrayp
    + - * / % mod 1+ 1- max min < <= = > >= /= abs
    ash lsh logand logior logxor)
  "Functions that are safe to call during evaluation.
Except for altering the match state, these are side-effect-free
and reasonably pure (some depend on variables in fairly uninteresting ways,
like `case-fold-search').
More functions could be added if there is evidence that it would
help in evaluating more regexp strings.")

(defconst relint--safe-alternatives
  '((nconc    . append)
    (delete   . remove)
    (delq     . remq)
    (nreverse . reverse)
    (nbutlast . butlast))
"Alist mapping non-safe functions to semantically equivalent safe
alternatives.")

(defconst relint--safe-cl-alternatives
  '((cl-delete-duplicates . cl-remove-duplicates)
    (cl-delete            . cl-remove)
    (cl-delete-if         . cl-remove-if)
    (cl-delete-if-not     . cl-remove-if-not)
    (cl-nsubstitute       . cl-substitute)
    (cl-nunion            . cl-union)
    (cl-nintersection     . cl-intersection)
    (cl-nset-difference   . cl-set-difference)
    (cl-nset-exclusive-or . cl-set-exclusive-or)
    (cl-nsublis           . cl-sublis))
"Alist mapping non-safe cl functions to semantically equivalent safe
alternatives. They may still require wrapping their function arguments.")

(defun relint--rx-safe (rx)
  "Return RX safe to translate; throw 'relint-eval 'no-value if not."
  (cond
   ((atom rx) rx)
   ;; These cannot contain rx subforms.
   ((memq (car rx) '(any in char not-char not backref
                     syntax not-syntax category))
    rx)
   ;; We ignore the differences in evaluation time between `eval' and
   ;; `regexp', and just use what environment we have.
   ((eq (car rx) 'eval)
    (let ((arg (relint--eval (cadr rx))))
      ;; For safety, make sure the result isn't another evaluating form.
      (when (and (consp arg)
                 (memq (car arg) '(literal eval regexp regex)))
        (throw 'relint-eval 'no-value))
      arg))
   ((memq (car rx) '(literal regexp regex))
    (let ((arg (relint--eval (cadr rx))))
      (if (stringp arg)
          (list (car rx) arg)
        (throw 'relint-eval 'no-value))))
   (t (cons (car rx) (mapcar #'relint--rx-safe (cdr rx))))))

(defun relint--eval-rx (args)
  "Evaluate an `rx-to-string' expression."
  (let ((safe-args (cons (relint--rx-safe (car args))
                         (cdr args))))
    (condition-case nil
        (apply #'rx-to-string safe-args)
      (error (throw 'relint-eval 'no-value)))))

(defun relint--apply (formals actuals body)
  "Bind FORMALS to ACTUALS and evaluate BODY."
  (let ((bindings nil))
    (while formals
      (cond
       ((eq (car formals) '&rest)
        (push (cons (cadr formals) (list actuals)) bindings)
        (setq formals nil))
       ((eq (car formals) '&optional)
        (setq formals (cdr formals)))
       (t
        (push (cons (car formals) (list (car actuals))) bindings)
        (setq formals (cdr formals))
        (setq actuals (cdr actuals)))))
    ;; This results in dynamic binding, but that doesn't matter for our
    ;; purposes.
    (let ((relint--locals (append bindings relint--locals))
          (relint--eval-mutables (append (mapcar #'car bindings)
                                         relint--eval-mutables)))
      (relint--eval-body body))))

(defun relint--no-value (&rest _)
  "A function that fails when called."
  (throw 'relint-eval 'no-value))

(defun relint--wrap-function (form)
  "Transform an evaluated function (typically a symbol or lambda expr)
into something that can be called safely."
  (cond
   ((symbolp form)
    (if (memq form relint--safe-functions)
        form
      (or (cdr (assq form relint--safe-alternatives))
          (let ((def (cdr (assq form relint--function-defs))))
            (if def
                (let ((formals (car def))
                      (body (cadr def)))
                  (lambda (&rest args)
                    (relint--apply formals args body)))
              'relint--no-value)))))
   ((and (consp form) (eq (car form) 'lambda))
    (let ((formals (cadr form))
          (body (cddr form)))
      (lambda (&rest args)
        (relint--apply formals args body))))
   (t 'relint--no-value)))

(defun relint--wrap-cl-keyword-args (args)
  "Wrap the function arguments :test, :test-not, :key in ARGS."
  (let ((test     (plist-get args :test))
        (test-not (plist-get args :test-not))
        (key      (plist-get args :key))
        (ret (copy-sequence args)))
    (when test
      (plist-put ret :test     (relint--wrap-function test)))
    (when test-not
      (plist-put ret :test-not (relint--wrap-function test-not)))
    (when key
      (plist-put ret :key      (relint--wrap-function key)))
    ret))

(defun relint--eval-to-binding (form)
  "Evaluate a form, returning (VALUE) on success or nil on failure."
  (let ((val (catch 'relint-eval
               (list (relint--eval form)))))
    (if (eq val 'no-value) nil val)))

(defun relint--eval-body (body)
  "Evaluate a list of forms; return result of last form."
  (if (consp body)
      (progn
        (while (consp (cdr body))
          (relint--eval (car body))
          (setq body (cdr body)))
        (if (cdr body)
            (throw 'relint-eval 'no-value)
          (relint--eval (car body))))
    (if body
        (throw 'relint-eval 'no-value)
      nil)))

(defun relint--eval (form)
  "Evaluate a form. Throw 'relint-eval 'no-value if something could
not be evaluated safely."
  (if (atom form)
      (cond
       ((booleanp form) form)
       ((keywordp form) form)
       ((symbolp form)
        (let ((local (assq form relint--locals)))
          (if local
              (if (cdr local)
                  (cadr local)
                (throw 'relint-eval 'no-value))
            (let ((binding (assq form relint--variables)))
              (if binding
                  (if (eq (cadr binding) 'val)
                      (caddr binding)
                    (let ((val (relint--eval (caddr binding))))
                      (setcdr binding (list 'val val))
                      val))
                  (throw 'relint-eval 'no-value))))))
       (t form))
    (let ((head (car form))
          (body (cdr form)))
      (cond
       ((eq head 'quote)
        (if (and (consp (car body))
                 (eq (caar body) '\,))     ; In case we are inside a backquote.
            (throw 'relint-eval 'no-value)
          (car body)))
       ((memq head '(function cl-function))
        ;; Treat cl-function like plain function (close enough).
        (car body))
       ((eq head 'lambda)
        form)

       ;; Functions considered safe.
       ((memq head relint--safe-functions)
        (let ((args (mapcar #'relint--eval body)))
          ;; Catching all errors isn't wonderful, but sometimes a global
          ;; variable argument has an unsuitable default value which is
          ;; supposed to have been changed at the expression point.
          (condition-case nil
              (apply head args)
            (error (throw 'relint-eval 'no-value)))))

       ;; replace-regexp-in-string: wrap the rep argument if it's a function.
       ((eq head 'replace-regexp-in-string)
        (let ((all-args (mapcar #'relint--eval body)))
          (let* ((rep-arg (cadr all-args))
                 (rep (if (stringp rep-arg)
                          rep-arg
                        (relint--wrap-function rep-arg)))
                 (args (append (list (car all-args) rep) (cddr all-args))))
            (condition-case nil
                (apply head args)
              (error (throw 'relint-eval 'no-value))))))

       ;; alist-get: wrap the optional fifth argument (testfn).
       ((eq head 'alist-get)
        (let* ((all-args (mapcar #'relint--eval body))
               (args (if (< (length all-args) 5)
                         all-args
                       (append (butlast all-args (- (length all-args) 4))
                               (list (relint--wrap-function
                                      (nth 4 all-args)))))))
          (condition-case nil
              (apply head args)
            (error (throw 'relint-eval 'no-value)))))

       ((eq head 'if)
        (let ((condition (relint--eval (car body))))
          (let ((then-part (nth 1 body))
                (else-tail (nthcdr 2 body)))
            (cond (condition
                   (relint--eval then-part))
                  (else-tail
                   (relint--eval-body else-tail))))))

       ((eq head 'and)
        (if body
            (let ((val (relint--eval (car body))))
              (if (and val (cdr body))
                  (relint--eval (cons 'and (cdr body)))
                val))
          t))

       ((eq head 'or)
        (if body
            (let ((val (relint--eval (car body))))
              (if (and (not val) (cdr body))
                  (relint--eval (cons 'or (cdr body)))
                val))
          nil))
       
       ((eq head 'cond)
        (and body
             (let ((clause (car body)))
               (if (consp clause)
                   (let ((val (relint--eval (car clause))))
                     (if val
                         (if (cdr clause)
                             (relint--eval-body (cdr clause))
                           val)
                       (relint--eval (cons 'cond (cdr body)))))
                 ;; Syntax error
                 (throw 'relint-eval 'no-value)))))

       ((memq head '(progn ignore-errors eval-when-compile eval-and-compile))
        (relint--eval-body body))

       ;; Hand-written implementation of `cl-assert' -- good enough.
       ((eq head 'cl-assert)
        (unless (relint--eval (car body))
          (throw 'relint-eval 'no-value)))

       ((eq head 'prog1)
        (let ((val (relint--eval (car body))))
          (relint--eval-body (cdr body))
          val))

       ((eq head 'prog2)
        (relint--eval (car body))
        (let ((val (relint--eval (cadr body))))
          (relint--eval-body (cddr body))
          val))

       ;; delete-dups: Work on a copy of the argument.
       ((eq head 'delete-dups)
        (let ((arg (relint--eval (car body))))
          (delete-dups (copy-sequence arg))))

       ;; Safe macros that expand to pure code, and their auxiliary macros.
       ;; FIXME: Some of these aren't actually safe at all, since they
       ;; may expand their arguments eagerly, running arbitrary code!
       ((memq head '(when unless
                     \` backquote-list*
                     pcase pcase-let pcase-let* pcase--flip
                     cl-case cl-loop cl-block cl-flet cl-flet* cl-labels))
        (relint--eval (macroexpand-1 form)))

       ;; catch: as long as nobody throws, this naïve code is fine.
       ((eq head 'catch)
        (relint--eval-body (cdr body)))

       ;; condition-case: as long as there is no error...
       ((eq head 'condition-case)
        (relint--eval (cadr body)))

       ;; cl--block-wrapper: works like identity, more or less.
       ((eq head 'cl--block-wrapper)
        (relint--eval (car body)))

       ;; Functions taking a function as first argument.
       ((memq head '(apply funcall mapconcat
                     cl-some cl-every cl-notany cl-notevery))
        (let ((fun (relint--wrap-function (relint--eval (car body))))
              (args (mapcar #'relint--eval (cdr body))))
          (condition-case nil
              (apply head fun args)
            (error (throw 'relint-eval 'no-value)))))
       
       ;; Functions with functions as keyword arguments :test, :test-not, :key
       ((memq head '(cl-remove-duplicates cl-remove cl-substitute cl-member
                     cl-find cl-position cl-count cl-mismatch cl-search
                     cl-union cl-intersection cl-set-difference
                     cl-set-exclusive-or cl-subsetp
                     cl-assoc cl-rassoc
                     cl-sublis))
        (let ((args (relint--wrap-cl-keyword-args
                     (mapcar #'relint--eval body))))
          (condition-case nil
              (apply head args)
            (error (throw 'relint-eval 'no-value)))))
       
       ;; Functions taking a function as first argument,
       ;; and with functions as keyword arguments :test, :test-not, :key
       ((memq head '(cl-reduce cl-remove-if cl-remove-if-not
                     cl-find-if cl-find-if not
                     cl-position-if cl-position-if-not
                     cl-count-if cl-count-if-not
                     cl-member-if cl-member-if-not
                     cl-assoc-if cl-assoc-if-not
                     cl-rassoc-if cl-rassoc-if-not))
        (let ((fun (relint--wrap-function (relint--eval (car body))))
              (args (relint--wrap-cl-keyword-args
                     (mapcar #'relint--eval (cdr body)))))
          (condition-case nil
              (apply head fun args)
            (error (throw 'relint-eval 'no-value)))))

       ;; mapcar, mapcan, mapc: accept missing items in the list argument.
       ((memq head '(mapcar mapcan mapc))
        (let* ((fun (relint--wrap-function (relint--eval (car body))))
               (arg (relint--eval-list (cadr body)))
               (seq (if (listp arg)
                        (remq nil arg)
                      arg)))
          (condition-case nil
              (funcall head fun seq)
            (error (throw 'relint-eval 'no-value)))))

       ;; sort: accept missing items in the list argument.
       ((eq head 'sort)
        (let* ((arg (relint--eval-list (car body)))
               (seq (cond ((listp arg) (remq nil arg))
                          ((sequencep arg) (copy-sequence arg))
                          (arg)))
               (pred (relint--wrap-function (relint--eval (cadr body)))))
          (condition-case nil
              (sort seq pred)
            (error (throw 'relint-eval 'no-value)))))

       ;; rx, rx-to-string: check for lisp expressions in constructs first,
       ;; then apply.
       ((eq head 'rx)
        (relint--eval-rx (list (cons 'seq body) t)))

       ((eq head 'rx-to-string)
        (let ((args (mapcar #'relint--eval body)))
          (relint--eval-rx args)))

       ;; setq: set local variables if permitted.
       ((eq head 'setq)
        (if (and (symbolp (car body)) (consp (cdr body)))
            (let* ((name (car body))
                   ;; FIXME: Consider using relint--eval-to-binding instead,
                   ;; tolerating unevaluatable expressions.
                   (val (relint--eval (cadr body))))
              ;; Somewhat dubiously, we ignore the side-effect for
              ;; non-local (or local non-mutable) variables and hope
              ;; it doesn't matter.
              (when (memq name relint--eval-mutables)
                (let ((local (assq name relint--locals)))
                  (setcdr local (list val))))
              (if (cddr body)
                  (relint--eval (cons 'setq (cddr body)))
                val))
          (throw 'relint-eval 'no-value)))  ; Syntax error.

       ((eq head 'push)
        (let* ((expr (car body))
               (name (cadr body))
               (local (assq name relint--locals)))
          (if (and (memq name relint--eval-mutables)
                   (cdr local))
              (let ((new-val (cons (relint--eval expr) (cadr local))))
                (setcdr local (list new-val))
                new-val)
            (throw 'relint-eval 'no-value))))

       ((eq head 'pop)
        (let* ((name (car body))
               (local (assq name relint--locals)))
          (if (and (memq name relint--eval-mutables)
                   (cdr local)
                   (consp (cadr local)))
              (let ((val (cadr local)))
                (setcdr local (list (cdr val)))
                (car val))
            (throw 'relint-eval 'no-value))))

       ;; let and let*: do not permit multi-expression bodies, since they
       ;; will contain necessary side-effects that we don't handle.
       ((eq head 'let)
        (let ((bindings
               (mapcar (lambda (binding)
                         (if (consp binding)
                             (cons (car binding)
                                   (relint--eval-to-binding (cadr binding)))
                           (cons binding (list nil))))
                       (car body))))
          (let ((relint--locals (append bindings relint--locals))
                (relint--eval-mutables (append (mapcar #'car bindings)
                                               relint--eval-mutables)))
            (relint--eval-body (cdr body)))))

       ((eq head 'let*)
        (let ((bindings (car body)))
          (if bindings
              (let* ((bindspec (car bindings))
                     (binding
                      (if (consp bindspec)
                          (cons (car bindspec)
                                (relint--eval-to-binding (cadr bindspec)))
                        (cons bindspec (list nil))))
                     (relint--locals (cons binding relint--locals))
                     (relint--eval-mutables
                      (cons (car binding) relint--eval-mutables)))
                (relint--eval `(let* ,(cdr bindings) ,@(cdr body))))
            (relint--eval-body (cdr body)))))

       ;; dolist: simulate its operation. We could also expand it,
       ;; but this is somewhat faster.
       ((eq head 'dolist)
        (unless (and (>= (length body) 2)
                     (consp (car body)))
          (throw 'relint-eval 'no-value))
        (let ((var (nth 0 (car body)))
              (seq-arg (nth 1 (car body)))
              (res-arg (nth 2 (car body))))
          (unless (symbolp var)
            (throw 'relint-eval 'no-value))
          (let ((seq (relint--eval-list seq-arg)))
            (while (consp seq)
              (let ((relint--locals (cons (list var (car seq))
                                          relint--locals)))
                (relint--eval-body (cdr body)))
              (setq seq (cdr seq))))
          (and res-arg (relint--eval res-arg))))

       ;; while: this slows down simulation noticeably, but catches some
       ;; mistakes.
       ((eq head 'while)
        (let ((condition (car body))
              (loops 0))
          (while (and (relint--eval condition)
                      (< loops 100))
            (relint--eval-body (cdr body))
            (setq loops (1+ loops)))
          nil))

       ;; Loose comma: can occur if we unwittingly stumbled into a backquote
       ;; form. Just eval the arg and hope for the best.
       ((eq head '\,)
        (relint--eval (car body)))

       ;; functionp: be optimistic, for determinism
       ((eq head 'functionp)
        (let ((arg (relint--eval (car body))))
          (cond
           ((symbolp arg) (not (memq arg '(nil t))))
           ((consp arg) (eq (car arg) 'lambda)))))

       ;; featurep: only handle features that we are reasonably sure about,
       ;; to avoid depending too much on the particular host Emacs.
       ((eq head 'featurep)
        (let ((arg (relint--eval (car body))))
          (cond ((eq arg 'xemacs) nil)
                ((memq arg '(emacs mule font-lock lisp-float-type)) t)
                (t (throw 'relint-eval 'no-value)))))

       ;; Locally defined functions: try evaluating.
       ((assq head relint--function-defs)
        (let* ((fn (cdr (assq head relint--function-defs)))
               (formals (car fn))
               (fn-body (cadr fn)))
          (let ((args (mapcar #'relint--eval body)))
            (relint--apply formals args fn-body))))

       ;; Locally defined macros: try expanding.
       ((assq head relint--macro-defs)
        (let ((args body))
          (let* ((macro (cdr (assq head relint--macro-defs)))
                 (formals (car macro))
                 (macro-body (cadr macro)))
            (relint--eval
             (relint--apply formals args macro-body)))))

       ;; Alias: substitute and try again.
       ((assq head relint--alias-defs)
        (relint--eval (cons (cdr (assq head relint--alias-defs))
                            body)))

       ((assq head relint--safe-alternatives)
        (relint--eval (cons (cdr (assq head relint--safe-alternatives))
                            body)))

       ((assq head relint--safe-cl-alternatives)
        (relint--eval (cons (cdr (assq head relint--safe-cl-alternatives))
                            body)))
       
       (t
        (throw 'relint-eval 'no-value))))))

(defun relint--eval-or-nil (form)
  "Evaluate FORM. Return nil if something prevents it from being evaluated."
  (let ((val (catch 'relint-eval (relint--eval form))))
    (if (eq val 'no-value)
        nil
      val)))

(defun relint--eval-list-body (body)
  (and (consp body)
       (progn
         (while (consp (cdr body))
           (relint--eval-list (car body))
           (setq body (cdr body)))
         (relint--eval-list (car body)))))

(defun relint--eval-list (form)
  "Evaluate a form as far as possible, attempting to keep its list structure
even if all subexpressions cannot be evaluated. Parts that cannot be
evaluated are nil."
  (cond
   ((symbolp form)
    (and form
         (let ((local (assq form relint--locals)))
           (if local
               (and (cdr local) (cadr local))
             (let ((binding (assq form relint--variables)))
               (and binding
                    (if (eq (cadr binding) 'val)
                        (caddr binding)
                      ;; Since we are only doing a list evaluation, don't
                      ;; update the variable here.
                      (relint--eval-list (caddr binding)))))))))
   ((atom form)
    form)
   ((memq (car form) '(progn ignore-errors eval-when-compile eval-and-compile))
    (relint--eval-list-body (cdr form)))

   ;; Pure structure-generating functions: Apply even if we cannot evaluate
   ;; all arguments (they will be nil), because we want a reasonable
   ;; approximation of the structure.
   ((memq (car form) '(list append cons reverse remove remq))
    (apply (car form) (mapcar #'relint--eval-list (cdr form))))

   ((eq (car form) 'delete-dups)
    (let ((arg (relint--eval-list (cadr form))))
      (delete-dups (copy-sequence arg))))

   ((memq (car form) '(purecopy copy-sequence copy-alist))
    (relint--eval-list (cadr form)))

   ((memq (car form) '(\` backquote-list*))
    (relint--eval-list (macroexpand-1 form)))

   ((assq (car form) relint--safe-alternatives)
    (relint--eval-list (cons (cdr (assq (car form) relint--safe-alternatives))
                             (cdr form))))

   (t
    (relint--eval-or-nil form))))

(defun relint--eval-list-iter (fun form path)
  "Evaluate FORM to a list and call FUN for each non-nil element
with (ELEM ELEM-PATH LITERAL) as arguments. ELEM-PATH is the best
approximation to a path to ELEM and has the same base position as
PATH; LITERAL is true if ELEM-PATH leads to a literal ELEM in the
source."
  (pcase form
    (`(quote ,arg)
     (when (consp arg)
       (let ((i 0)
             (p (cons 1 path)))
         (dolist (elem arg)
           (when elem
             (funcall fun elem (cons i p) t))
           (setq i (1+ i))))))
    (`(list . ,args)
     (let ((i 1))
       (dolist (expr args)
         (pcase expr
           ((pred stringp)
            (funcall fun expr (cons i path) t))
           (`(quote ,elem)
            (when elem
              (funcall fun elem (cons 1 (cons i path)) t)))
           (_ (let ((elem (relint--eval-or-nil expr)))
                (when elem
                  (funcall fun elem (cons i path) nil)))))
         (setq i (1+ i)))))
    (`(append . ,args)
     (let ((i 1))
       (dolist (arg args)
         (relint--eval-list-iter fun arg (cons i path))
         (setq i (1+ i)))))
    (`(\` ,args)
     (when (consp args)
       (let ((i 0))
         (let ((p0 (cons 1 path)))
           (dolist (arg args)
             (let* ((expanded (relint--eval-or-nil (list '\` arg)))
                    (values (if (and (consp arg)
                                     (eq (car arg) '\,@))
                                expanded
                              (list expanded)))
                    (p (cons i p0)))
               (dolist (elem values)
                 (when elem
                   (funcall fun elem p (equal arg expanded)))))
             (setq i (1+ i)))))))
    (`(eval-when-compile ,expr)
     (relint--eval-list-iter fun expr (cons 1 path)))
    (_
     ;; Fall back on `relint--eval-list', giving up on
     ;; element-specific source position.
     (let ((expr (relint--eval-list form)))
       (when (consp expr)
         (dolist (elem expr)
           (funcall fun elem path nil)))))))

(defun relint--get-string (form)
  "Convert something to a string, or nil."
  (let ((val (relint--eval-or-nil form)))
    (and (stringp val) val)))

(defun relint--check-re (form name pos path)
  (let ((re (relint--get-string form)))
    (when re
      (relint--check-re-string re name pos path))))

(defun relint--check-list (form name pos path is-file-name)
  "Check a list of regexps."
  (let ((check (if is-file-name
                   #'relint--check-file-name-re
                 #'relint--check-re-string)))
    (relint--eval-list-iter
     (lambda (elem elem-path _literal)
       (when (stringp elem)
         (funcall check elem name pos elem-path)))
     form path)))

(defun relint--check-list-any (form name pos path)
  "Check a list of regexps or conses whose car is a regexp."
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
     (cond
      ((stringp elem)
       (relint--check-re-string elem name pos elem-path))
      ((and (consp elem)
            (stringp (car elem)))
       (relint--check-re-string (car elem) name pos
                                (if literal (cons 0 elem-path) elem-path)))))
   form path))

(defun relint--check-alist-any (form name pos path)
  "Check an alist whose cars or cdrs may be regexps."
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
     (when (consp elem)
       (when (stringp (car elem))
         (relint--check-re-string (car elem) name pos
                                  (if literal (cons 0 elem-path) elem-path)))
       (when (stringp (cdr elem))
         (relint--check-re-string (cdr elem) name pos
                                  (if literal (cons 1 elem-path) elem-path)))))
   form path))

(defun relint--check-alist-cdr (form name pos path)
  "Check an alist whose cdrs are regexps."
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
     (when (and (consp elem)
                (stringp (cdr elem)))
       (relint--check-re-string (cdr elem) name pos
                                (if literal (cons 1 elem-path) elem-path))))
   form path))

(defun relint--check-font-lock-defaults (form name pos path)
  "Check a value for `font-lock-defaults'."
  (let ((val (relint--eval-or-nil form)))
    (when (consp val)
     (cond
      ((symbolp (car val))
       (unless (memq (car val) relint--checked-variables)
         (relint--check-font-lock-keywords (car val) name pos path)))
      ((consp (car val))
       (let ((keywords (car val)))
         (while keywords
           (when (and (symbolp (car keywords))
                      (not (memq (car keywords) relint--checked-variables)))
             (relint--check-font-lock-keywords (car keywords) name pos path))
           (setq keywords (cdr keywords)))))))))

(defun relint--check-font-lock-keywords (form name pos path)
  "Check a font-lock-keywords list.  A regexp can be found in an element,
or in the car of an element."
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
    (cond
     ((stringp elem)
      (relint--check-re-string elem name pos elem-path))
     ((and (consp elem)
           (stringp (car elem)))
      (let* ((tag (and (symbolp (cdr elem)) (cdr elem)))
             (ident (if tag (format "%s (%s)" name tag) name))
             (p (if literal
                    (cons 0 elem-path)
                  elem-path)))
        (relint--check-re-string (car elem) ident pos p)))))
   form path))

(defun relint--check-imenu-generic-expression (form name pos path)
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
     (when (and (consp elem) (consp (cdr elem)) (stringp (cadr elem)))
       (relint--check-re-string
        (cadr elem) name pos (if literal (cons 1 elem-path) elem-path))))
   form path))

(defun relint--check-compilation-error-regexp-alist-alist (form name pos path)
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
     (when (cadr elem)
       (relint--check-re-string
        (cadr elem)
        (format "%s (%s)" name (car elem))
        pos (if literal (cons 1 elem-path) elem-path))))
   form path))

(defun relint--check-file-name-re (form name pos path)
  (let ((re (relint--get-string form)))
    (when re
      (relint--check-file-re-string re name pos path))))

(defun relint--check-auto-mode-alist-expr (form name pos path)
  "Check a single element added to `auto-mode-alist'."
  (pcase form
    (`(quote (,(and (pred stringp) str) . ,_))
     (relint--check-file-re-string str name pos (cons 0 (cons 1 path))))
    (_
     (let ((val (relint--eval-or-nil form)))
       (when (and (consp val) (stringp (car val)))
         (relint--check-file-re-string (car val) name pos path))))))

(defun relint--check-auto-mode-alist (form name pos path)
  (relint--eval-list-iter
   (lambda (elem elem-path literal)
     (relint--check-file-name-re
      (car elem) name
      pos (if literal (cons 0 elem-path) elem-path)))
   form path))

(defun relint--check-rules-list (form name pos path)
  "Check a variable on `align-mode-rules-list' format"
  (relint--eval-list-iter
   (lambda (rule rule-path literal)
     (when (and (consp rule)
                (symbolp (car rule)))
       (let ((rule-name (car rule))
             (i 1))
         (dolist (clause (cdr rule))
           (when (and (consp clause) (eq (car clause) 'regexp)
                      (stringp (cdr clause)))
             (relint--check-re-string 
              (cdr clause) (format "%s (%s)" name rule-name) pos
              (if literal (cons 1 (cons i rule-path)) rule-path)))
           (setq i (1+ i))))))
   form path))

(defconst relint--known-regexp-variables
  '(page-delimiter paragraph-separate paragraph-start
    sentence-end comment-start-skip comment-end-skip)
  "List of known (global or buffer-local) regexp variables.")

(defconst relint--known-regexp-returning-functions
  '(regexp-quote regexp-opt regexp-opt-charset
    rx rx-to-string wildcard-to-regexp read-regexp
    char-fold-to-regexp find-tag-default-as-regexp
    find-tag-default-as-symbol-regexp sentence-end
    word-search-regexp)
  "List of functions known to return a regexp.")

;; List of functions believed to return a regexp.
(defvar relint--regexp-returning-functions)

(defun relint--regexp-generators (expr expanded)
  "List of regexp-generating functions and variables used in EXPR.
EXPANDED is a list of expanded functions, to prevent recursion."
  (cond
   ((symbolp expr)
    (and (not (memq expr '(nil t)))
         ;; Check both variable contents and name.
         (or (let ((def (assq expr relint--variables)))
               (and def
                    (eq (cadr def) 'expr)
                    (relint--regexp-generators (caddr def) expanded)))
             (and (or (memq expr relint--known-regexp-variables)
                      ;; This is guesswork, but effective.
                      (string-match-p
                       (rx (or (seq bos (or "regexp" "regex"))
                               (or "-regexp" "-regex" "-re"))
                           eos)
                       (symbol-name expr)))
                  (list expr)))))
   ((atom expr) nil)
   ((memq (car expr) relint--regexp-returning-functions)
    (list (car expr)))
   ((memq (car expr)
          ;; These forms never produce regexps at all, but are listed here
          ;; to prevent false positives since their bodies often do.
          '(while
            looking-at re-search-forward re-search-backward
            string-match string-match-p looking-back looking-at-p
            replace-regexp
            query-replace-regexp
            posix-looking-at posix-search-backward
            posix-search-forward
            posix-string-match
            search-forward-regexp search-backward-regexp
            kill-matching-buffers
            keep-lines flush-lines how-many
            delete-matching-lines delete-non-matching-lines
            count-matches
            s-matches? s-matches-p s-matched-positions-all
            s-count-matches s-count-matches-all))
    nil)
   ((null (cdr (last expr)))
    (let* ((head (car expr))
           (args
            (if (memq head
                      ;; These forms may generate regexps but the provenance
                      ;; of their first argument is irrelevant.
                      ;; This list, too, could be expanded vastly.
                      '(if when unless
                        replace-regexp-in-string
                        s-match-strings-all s-match s-slice-at
                        s-split s-split-up-to))
                     (cddr expr)
                   (cdr expr)))
           (alias (assq head relint--alias-defs)))
      (if alias
          (relint--regexp-generators (cons (cdr alias) (cdr expr)) expanded)
        (append (mapcan (lambda (x) (relint--regexp-generators x expanded))
                        args)
                (let ((fun (assq head relint--function-defs)))
                  (and fun (not (memq head expanded))
                       (mapcan (lambda (x)
                                 (relint--regexp-generators
                                  x (cons head expanded)))
                               (caddr fun))))))))))

(defun relint--check-non-regexp-provenance (skip-function form pos path)
  (let ((reg-gen (relint--regexp-generators form nil)))
    (when reg-gen
      (relint--warn
       pos path
       (format-message "`%s' cannot be used for arguments to `%s'"
                       (car reg-gen) skip-function)))))

(defun relint--check-format-mixup (template args pos path)
  "Look for a format expression that suggests insertion of a regexp
into a character alternative: [%s] where the corresponding format
parameter is regexp-generating."
  (let ((nargs (length args))
        (index 0)
        (start 0))
    (while (and (< index nargs)
                (string-match (rx
                               "%"
                               (opt (1+ digit) "$")
                               (0+ (any "+ #" ?-))
                               (0+ digit)
                               (opt "." (0+ digit))
                               (group (any "%sdioxXefgcS")))
                              template start))
      (let ((percent (match-beginning 0))
            (type (string-to-char (match-string 1 template)))
            (next (match-end 0)))
        (when (and (eq type ?s)
                   ;; Find preceding `[' before %s
                   (string-match-p
                    (rx
                     bos
                     (* (or (not (any "\\" "["))
                            (seq "\\" anything)))
                     "["
                     (* (not (any "]")))
                     eos)
                    (substring template start percent)))
          (let ((reg-gen (relint--regexp-generators (nth index args) nil)))
            (when reg-gen
              (relint--warn
               pos (cons (+ index 2) path)
               (format-message
                "Value from `%s' cannot be spliced into `[...]'"
                (car reg-gen))))))
        (unless (eq type ?%)
          (setq index (1+ index)))
        (setq start next)))))

(defun relint--check-concat-mixup (args pos path)
  "Look for concat args that suggest insertion of a regexp into a
character alternative: `[' followed by a regexp-generating expression."
  (let ((index 1))
    (while (consp args)
      (let ((arg (car args)))
        (when (and (stringp arg)
                   (cdr args)
                   (string-match-p (rx (or bos (not (any "\\")))
                                       (0+ "\\\\")
                                       "["
                                       (0+ (not (any "]")))
                                       eos)
                                   arg))
          (let ((reg-gen (relint--regexp-generators (cadr args) nil)))
            (when reg-gen
              (relint--warn
               pos (cons (1+ index) path)
               (format-message
                "Value from `%s' cannot be spliced into `[...]'"
                (car reg-gen)))))))
      (setq index (1+ index))
      (setq args (cdr args)))))

(defun relint--pretty-range (from to)
  (relint--escape-string
   (if (eq from to)
       (char-to-string from)
     (format "%c-%c" from to))
   nil))

(defun relint--intersecting-range (from to ranges)
  "Return a range in RANGES intersecting [FROM,TO], or nil if none.
RANGES is a list of (X . Y) representing the interval [X,Y]."
  (while (and ranges
              (let ((range (car ranges)))
                (not (and (<= from (cdr range))
                          (<= (car range) to)))))
    (setq ranges (cdr ranges)))
  (car ranges))

(defun relint--check-rx (item pos path exact-path)
  "Check the `rx' expression ITEM.
EXACT-PATH indicates whether PATH leads to ITEM exactly, rather
than just to a surrounding or producing expression."
  (pcase item
    (`(,(or ': 'seq 'sequence 'and 'or '|
            'not 'intersection 'repeat '= '>= '**
            'zero-or-more '0+ '* '*?
            'one-or-more '1+ '+ '+?
            'zero-or-one 'opt 'optional '\? ?\s '\?? ??
            'minimal-match 'maximal-match
            'group 'submatch
            'group-n 'submatch-n)
       . ,args)
     ;; Form with subforms: recurse.
     (let ((i 1))
       (dolist (arg args)
         (relint--check-rx arg pos (if exact-path (cons i path) path)
                           exact-path)
         (setq i (1+ i)))))

    (`(,(or 'any 'in 'char 'not-char) . ,args)
     ;; We don't bother checking for outright errors like "b-a", but
     ;; look for mistakes that rx itself doesn't complain about. We
     ;; assume a hand-written rx expression; machine-generated code
     ;; can break these rules.
     (let ((i 1)
           (classes nil)
           (ranges nil))
       (dolist (arg args)
         (cond
          ((characterp arg)
           (let ((overlap (relint--intersecting-range arg arg ranges)))
             (when overlap
               (relint--warn
                pos (if exact-path (cons i path) path)
                (if (eq (car overlap) (cdr overlap))
                    (format-message "Duplicated character `%s'"
                                    (relint--pretty-range arg arg))
                  (format-message "Character `%s' included in range `%s'"
                                  (relint--pretty-range arg arg)
                                  (relint--pretty-range (car overlap)
                                                        (cdr overlap)))))))
           (push (cons arg arg) ranges))

          ((stringp arg)
           ;; `string-to-multibyte' was marked obsolete in Emacs 26,
           ;; but no longer is.
           (let* ((s (with-no-warnings (string-to-multibyte arg)))
                  (j 0)
                  (len (length s)))
             (while (< j len)
               (let ((from (aref s j)))
                 (if (and (< (+ j 2) len)
                          (eq (aref s (1+ j)) ?-))
                     ;; Range.
                     (let ((to (aref s (+ j 2))))
                       (cond
                        ;; When people write "+-X" or "X-+" for some
                        ;; X, they rarely mean a range.
                        ((or (eq from ?+)
                             (eq to ?+))
                         (relint--warn
                          pos (if exact-path (cons i path) path)
                          (format-message "Suspect range `%s'"
                                          (relint--pretty-range from to))
                          s j))
                        ((= to from)
                         (relint--warn
                          pos (if exact-path (cons i path) path)
                          (format-message
                           "Single-character range `%s'"
                           (relint--escape-string (format "%c-%c" from to) nil))
                          s j))
                        ((= to (1+ from))
                         (relint--warn
                          pos (if exact-path (cons i path) path)
                          (format-message "Two-character range `%s'"
                                          (relint--pretty-range from to))
                          s j)))
                       ;; Take care to split ASCII-raw ranges; they do not
                       ;; include anything in-between.
                       (let* ((split (and (<= from #x7f) (>= to #x3fff80)))
                              (overlap
                               (if split
                                   (or (relint--intersecting-range
                                        from #x7f ranges)
                                       (relint--intersecting-range
                                        #x3fff80 to ranges))
                                 (relint--intersecting-range from to ranges))))
                         (when overlap
                           (relint--warn
                            pos (if exact-path (cons i path) path)
                            (format-message "Range `%s' overlaps previous `%s'"
                                            (relint--pretty-range from to)
                                            (relint--pretty-range
                                             (car overlap) (cdr overlap)))
                            s j))
                         (if split
                             (progn
                               (push (cons from #x7f) ranges)
                               (push (cons #x3fff80 to) ranges))
                           (push (cons from to) ranges)))
                       (setq j (+ j 3)))

                   ;; Single character.
                   (when (and (eq from ?-)
                              (< 0 j (1- len)))
                     (relint--warn
                      pos (if exact-path (cons i path) path)
                      (format-message "Literal `-' not first or last")
                      s j))
                   (let ((overlap
                          (relint--intersecting-range from from ranges)))
                     (when overlap
                       (relint--warn
                        pos (if exact-path (cons i path) path)
                        (if (eq (car overlap) (cdr overlap))
                            (format-message "Duplicated character `%s'"
                                            (relint--pretty-range from from))
                          (format-message
                           "Character `%s' included in range `%s'"
                           (relint--pretty-range from from)
                           (relint--pretty-range (car overlap) (cdr overlap))))
                        s j)))
                   (push (cons from from) ranges)
                   (setq j (1+ j)))))))

          ((consp arg)
           (let ((from (car arg))
                 (to (cdr arg)))
             (when (and (characterp from) (characterp to)
                        (<= from to))
               (let ((overlap
                      (relint--intersecting-range from to ranges)))
                 (when overlap
                   (relint--warn
                    pos (if exact-path (cons i path) path)
                    (format-message "Range `%s' overlaps previous `%s'"
                                    (relint--pretty-range from to)
                                    (relint--pretty-range
                                     (car overlap) (cdr overlap))))))
               (push (cons from to) ranges))))

          ((symbolp arg)
           (when (memq arg classes)
             (relint--warn pos (if exact-path (cons i path) path)
                           (format-message "Duplicated class `%s'" arg)))
           (push arg classes)))
         (setq i (1+ i)))))

    (`(,(or 'regexp 'regex) ,expr)
     (relint--check-re expr (format-message "rx `%s' form" (car item))
                       pos (if exact-path (cons 1 path) path)))

    ;; Evaluate unquote and unquote-splicing forms as if inside a
    ;; (single) backquote.
    (`(,(or 'eval '\,) ,expr)
     (let ((val (relint--eval-or-nil expr)))
       (when val
         (relint--check-rx val pos (if exact-path (cons 1 path) path) nil))))

    (`(\,@ ,expr)
     (let ((items (relint--eval-list expr)))
       (dolist (form items)
         (relint--check-rx form pos (if exact-path (cons 1 path) path) nil))))))

(defun relint--regexp-args-from-doc (doc-string)
  "Extract regexp arguments (as a list of symbols) from DOC-STRING."
  (let ((start 0)
        (found nil))
    (while (string-match (rx (any "rR")
                             (or (seq  "egex" (opt "p"))
                                 (seq "egular" (+ (any " \n\t")) "expression"))
                             (+ (any " \n\t"))
                             (group (+ (any "A-Z" ?-))))
                         doc-string start)
      (push (intern (downcase (match-string 1 doc-string))) found)
      (setq start (match-end 0)))
    found))

(defun relint--check-form-recursively-1 (form pos path)
  (pcase form
    (`(,(or 'defun 'defmacro 'defsubst)
       ,name ,args . ,body)
     (when (symbolp name)
       (let ((doc-args nil))
         (when (string-match-p (rx (or  "-regexp" "-regex" "-re") eos)
                               (symbol-name name))
           (push name relint--regexp-returning-functions))
         ;; Examine doc string if any.
         (when (stringp (car body))
           (setq doc-args (relint--regexp-args-from-doc (car body)))
           (when (and (not (memq name relint--regexp-returning-functions))
                      (let ((case-fold-search t))
                        (string-match-p
                         (rx (or bos
                                 (seq (or "return" "generate" "make")
                                      (opt "s")
                                      (+ (any " \n\t"))))
                             (opt (or "a" "the") (+ (any " \n\t")))
                             (or "regex"
                                 (seq "regular"
                                      (+ (any " \n\t"))
                                      "expression")))
                         (car body))))
             (push name relint--regexp-returning-functions))
           (setq body (cdr body)))
         ;; Skip declarations.
         (while (and (consp (car body))
                     (memq (caar body) '(interactive declare)))
           (setq body (cdr body)))
         ;; Save the function or macro for possible use.
         (push (list name args body)
               (if (eq (car form) 'defmacro)
                   relint--macro-defs
                 relint--function-defs))

         ;; If any argument looks like a regexp, remember it so that it can be
         ;; checked in calls.
         (when (consp args)
           (let ((indices nil)
                 (index 0))
             (while args
               (let ((arg (car args)))
                 (when (symbolp arg)
                   (cond
                    ((eq arg '&optional))   ; Treat optional args as regular.
                    ((eq arg '&rest)
                     (setq args nil))       ; Ignore &rest args.
                    (t
                     (when (or (memq arg doc-args)
                               (string-match-p
                                (rx (or (or "regexp" "regex" "-re"
                                            "pattern")
                                        (seq bos "re"))
                                    eos)
                                (symbol-name arg)))
                       (push index indices))
                     (setq index (1+ index)))))
                 (setq args (cdr args))))
             (when indices
               (push (cons name (reverse indices))
                     relint--regexp-functions)))))))
    (`(defalias ,name-arg ,def-arg . ,_)
     (let ((name (relint--eval-or-nil name-arg))
           (def  (relint--eval-or-nil def-arg)))
       (when (and name def)
         (push (cons name def) relint--alias-defs))))
    (_
     (let ((index 0))
       (while (consp form)
         (when (consp (car form))
           (relint--check-form-recursively-1
            (car form) pos (cons index path)))
         (setq form (cdr form))
         (setq index (1+ index)))))))

(defun relint--check-defcustom-type (type name pos path)
  (pcase type
    (`(,(or 'const 'string 'regexp) . ,rest)
     (while (consp rest)
       (cond ((eq (car rest) :value)
              (relint--check-re (cadr rest) name pos path))
             ((not (cdr rest))
              (relint--check-re (car rest) name pos path)))
       (setq rest (cddr rest))))
    (`(,(or 'choice 'radio) . ,choices)
     (dolist (choice choices)
       (relint--check-defcustom-type choice name pos path)))))

(defun relint--check-defcustom-re (form name pos path)
  (let ((args (nthcdr 4 form))
        (index 5))
    (while (consp args)
      (pcase args
        (`(:type ,type)
         (relint--check-defcustom-type (relint--eval-or-nil type)
                                       name pos (cons index path)))
        (`(:options ,options)
         (relint--check-list options name pos (cons index path) nil)))
      (setq index (+ index 2))
      (setq args (cddr args)))))

(defun relint--defcustom-type-regexp-p (type)
  "Whether the defcustom type TYPE indicates a regexp."
  (pcase type
    ('regexp t)
    (`(regexp . ,_) t)
    (`(string :tag ,tag . ,_)
     (let ((case-fold-search t))
       (string-match-p (rx bos
                           (opt (or "the" "a") " ")
                           (or "regex" "regular expression"))
                       tag)))
    (`(,(or 'choice 'radio) . ,rest)
     (cl-some #'relint--defcustom-type-regexp-p rest))))

(defun relint--check-and-eval-let-binding (binding mutables pos path)
  "Check the let-binding BINDING, which is probably (NAME EXPR) or NAME,
and evaluate EXPR. On success return (NAME VALUE); if evaluation failed,
return (NAME); on syntax error, return nil."
  (cond ((symbolp binding)
         (cons binding (list nil)))
        ((and (consp binding)
              (symbolp (car binding))
              (consp (cdr binding)))
         (relint--check-form-recursively-2
          (cadr binding) mutables pos (cons 1 path))
         (let ((val (catch 'relint-eval
                      (list (relint--eval (cadr binding))))))
           (when (and (consp val)
                      (stringp (car val))
                      (memq (car binding) relint--known-regexp-variables))
             ;; Setting a special buffer-local regexp.
             (relint--check-re (car val) (car binding) pos (cons 1 path)))
           (cons (car binding)
                 (if (eq val 'no-value)
                     nil
                   val))))))

(defun relint--check-let* (bindings body mutables pos path index)
  "Check the BINDINGS and BODY of a `let*' form."
  (if bindings
      (let ((b (relint--check-and-eval-let-binding
                (car bindings) mutables pos (cons index (cons 1 path)))))
        (if b
            (let ((relint--locals (cons b relint--locals)))
              (relint--check-let* (cdr bindings) body (cons (car b) mutables)
                                  pos path (1+ index)))
          (relint--check-let* (cdr bindings) body mutables
                              pos path (1+ index))))
    (let ((index 2))
      (while (consp body)
        (when (consp (car body))
          (relint--check-form-recursively-2
           (car body) mutables pos (cons index path)))
        (setq body (cdr body))
        (setq index (1+ index))))))

(defun relint--check-form-recursively-2 (form mutables pos path)
"Check FORM (at POS, PATH) recursively.
MUTABLES is a list of lexical variables in a scope which FORM may mutate
directly."
  (pcase form
    (`(let ,(and (pred listp) bindings) . ,body)
     (let* ((i 0)
            (bindings-path (cons 1 path))
            (new-bindings nil)
            (body-mutables mutables))
       (while (consp bindings)
         (let ((b (relint--check-and-eval-let-binding
                   (car bindings) mutables pos (cons i bindings-path))))
           (when b
             (push b new-bindings)
             (push (car b) body-mutables))
           (setq i (1+ i))
           (setq bindings (cdr bindings))))
       (let ((relint--locals
              (append new-bindings relint--locals))
             (index 2))
         (while (consp body)
           (when (consp (car body))
             (relint--check-form-recursively-2
              (car body) body-mutables pos (cons index path)))
           (setq body (cdr body))
           (setq index (1+ index))))))
    (`(let* ,(and (pred listp) bindings) . ,body)
     (relint--check-let* bindings body mutables pos path 0))
    (`(,(or 'setq 'setq-local) . ,args)
     ;; Only mutate lexical variables in the mutation list, which means
     ;; that this form will be executed exactly once during their remaining
     ;; lifetime. Other lexical vars will just be invalidated since we
     ;; don't know anything about the control flow.
     (let ((i 2))
       (while (and (consp args) (consp (cdr args)) (symbolp (car args)))
         (let ((name (car args))
               (expr (cadr args)))
           (relint--check-form-recursively-2
            expr mutables pos (cons i path))
           (cond
            ((memq name relint--known-regexp-variables)
             (relint--check-re expr name pos (cons i path)))
            ((and (symbolp name) (string-match-p (rx "font-lock-keywords")
                                                 (symbol-name name)))
             (relint--check-font-lock-keywords expr name pos (cons i path)))
            ((eq name 'font-lock-defaults)
             (relint--check-font-lock-defaults expr name pos (cons i path)))
            ((eq name 'imenu-generic-expression)
             (relint--check-imenu-generic-expression
              expr name pos (cons i path)))
            ((eq name 'auto-mode-alist)
             (pcase expr
               (`(cons ,item auto-mode-alist)
                (relint--check-auto-mode-alist-expr
                 item name pos (cons 1 (cons i path))))
               (`(append ,items auto-mode-alist)
                (relint--check-auto-mode-alist
                 items name pos (cons 1 (cons i path))))))
            (t
             ;; Invalidate the variable if it was local; otherwise, ignore.
             (let ((local (assq name relint--locals)))
               (when local
                 (setcdr local
                         (and (memq name mutables)
                              (let ((val (catch 'relint-eval
                                           (list (relint--eval expr)))))
                                (and (not (eq val 'no-value))
                                     val)))))))))
         (setq args (cddr args))
         (setq i (+ i 2)))))
    (`(push ,expr ,(and (pred symbolp) name))
     ;; Treat (push EXPR NAME) as (setq NAME (cons EXPR NAME)).
     (relint--check-form-recursively-2 expr mutables pos (cons 1 path))
     (when (eq name 'auto-mode-alist)
       (relint--check-auto-mode-alist-expr expr name pos (cons 1 path)))
     (let ((local (assq name relint--locals)))
       (when local
         (setcdr local
                 (let ((old-val (cdr local)))
                   (and old-val
                        (memq name mutables)
                        (let ((val (catch 'relint-eval
                                     (list (cons (relint--eval expr)
                                                 (car old-val))))))
                          (and (consp val)
                               val))))))))
    (`(pop ,(and (pred symbolp) name))
     ;; Treat (pop NAME) as (setq NAME (cdr NAME)).
     (let ((local (assq name relint--locals)))
       (when (and local (memq name mutables))
         (let ((old-val (cadr local)))
           (when (consp old-val)
             (setcdr local (list (cdr old-val))))))))
    (`(,(or 'if 'and 'or 'when 'unless) ,(and (pred consp) arg1) . ,rest)
     ;; Only first arg is executed unconditionally.
     ;; FIXME: A conditional in the tail position of its environment binding
     ;; has the exactly-once property wrt its body; use it!
     (relint--check-form-recursively-2 arg1 mutables pos (cons 1 path))
     (let ((i 2))
       (while (consp rest)
         (when (consp (car rest))
           (relint--check-form-recursively-2
            (car rest) nil pos (cons i path)))
         (setq rest (cdr rest))
         (setq i (1+ i)))))
    (`(,(or 'defun 'defsubst 'defmacro) ,_ ,(and (pred listp) arglist) . ,body)
     ;; Create local bindings for formal arguments (with unknown values).
     (let* ((argnames (mapcan (lambda (arg)
                                (and (symbolp arg)
                                     (not (memq arg '(&optional &rest)))
                                     (list arg)))
                              arglist))
            (relint--locals (append (mapcar #'list argnames) relint--locals)))
       (let ((i 3))
         (while (consp body)
           (when (consp (car body))
             (relint--check-form-recursively-2
              (car body) argnames pos (cons i path)))
           (setq body (cdr body))
           (setq i (1+ i))))))
    (`(lambda ,(and (pred listp) arglist) . ,body)
     ;; Create local bindings for formal arguments (with unknown values).
     (let* ((argnames (mapcan (lambda (arg)
                                (and (symbolp arg)
                                     (not (memq arg '(&optional &rest)))
                                     (list arg)))
                              arglist))
            (relint--locals (append (mapcar #'list argnames) relint--locals)))
       (let ((i 2))
         (while (consp body)
           (when (consp (car body))
             (relint--check-form-recursively-2
              (car body) argnames pos (cons i path)))
           (setq body (cdr body))
           (setq i (1+ i))))))
    (_ 
     (pcase form
       (`(,(or 'looking-at 're-search-forward 're-search-backward
               'string-match 'string-match-p 'looking-back 'looking-at-p
               'replace-regexp-in-string 'replace-regexp
               'query-replace-regexp
               'posix-looking-at 'posix-search-backward 'posix-search-forward
               'posix-string-match
               'search-forward-regexp 'search-backward-regexp
               'kill-matching-buffers
               'keep-lines 'flush-lines 'how-many
               'delete-matching-lines 'delete-non-matching-lines 'count-matches
               ;; From s.el
               's-matches? 's-matches-p 's-match-strings-all
               's-matched-positions-all 's-match 's-slice-at
               's-count-matches 's-count-matches-all 's-split 's-split-up-to)
          ,re-arg . ,_)
        (unless (and (symbolp re-arg)
                     (memq re-arg relint--checked-variables))
          (relint--check-re re-arg (format "call to %s" (car form))
                            pos (cons 1 path))))
       (`(load-history-filename-element ,re-arg)
        (relint--check-file-name-re re-arg (format "call to %s" (car form))
                                    pos (cons 1 path)))
       (`(directory-files-recursively ,_ ,re-arg . ,_)
        (relint--check-file-name-re re-arg (format "call to %s" (car form))
                                    pos (cons 2 path)))
       (`(,(or 'split-string 'split-string-and-unquote
               'string-trim-left 'string-trim-right 'string-trim)
          ,_ ,re-arg . ,rest)
        (unless (and (symbolp re-arg)
                     (memq re-arg relint--checked-variables))
          (relint--check-re re-arg (format "call to %s" (car form))
                            pos (cons 2 path)))
        ;; string-trim has another regexp argument (trim-right, arg 3)
        (when (and (eq (car form) 'string-trim)
                   (car rest))
          (let ((right (car rest)))
            (unless (and (symbolp right)
                         (memq right relint--checked-variables))
              (relint--check-re right (format "call to %s" (car form))
                                pos (cons 3 path)))))
        ;; split-string has another regexp argument (trim, arg 4)
        (when (and (eq (car form) 'split-string)
                   (cadr rest))
          (let ((trim (cadr rest)))
            (unless (and (symbolp trim)
                         (memq trim relint--checked-variables))
              (relint--check-re trim (format "call to %s" (car form))
                                pos (cons 4 path))))))
       (`(,(or 'directory-files 'directory-files-and-attributes)
          ,_ ,_ ,re-arg . ,_)
        (relint--check-file-name-re re-arg (format "call to %s" (car form))
                                    pos (cons 3 path)))
       (`(sort-regexp-fields ,_ ,record-arg ,key-arg . ,_)
        (let ((name (format "call to %s" (car form))))
          (relint--check-re record-arg name pos (cons 2 path))
          (let ((key-re (relint--eval-or-nil key-arg)))
            (when (and (stringp key-re) (not (equal key-re "\\&")))
              (relint--check-re key-re name pos (cons 3 path))))))
       (`(,(or 'skip-chars-forward 'skip-chars-backward)
          ,skip-arg . ,_)
        (let ((str (relint--get-string skip-arg)))
          (when str
            (relint--check-skip-set str (format "call to %s" (car form))
                                    pos (cons 1 path))))
        (relint--check-non-regexp-provenance
         (car form) skip-arg pos (cons 1 path))
        )
       (`(,(or 'skip-syntax-forward 'skip-syntax-backward) ,arg . ,_)
        (let ((str (relint--get-string arg)))
          (when str
            (relint--check-syntax-string str (format "call to %s" (car form))
                                         pos (cons 1 path))))
        (relint--check-non-regexp-provenance (car form) arg pos (cons 1 path))
        )
       (`(concat . ,args)
        (relint--check-concat-mixup args pos path))
       (`(format ,template-arg . ,args)
        (let ((template (relint--get-string template-arg)))
          (when template
            (relint--check-format-mixup template args pos path))))
       (`(,(or 'defvar 'defconst 'defcustom)
          ,name ,re-arg . ,rest)
        (let ((type (and (eq (car form) 'defcustom)
                         (relint--eval-or-nil (plist-get (cdr rest) :type)))))
          (when (symbolp name)
            (cond
             ((or (relint--defcustom-type-regexp-p type)
                  (string-match-p (rx (or "-regexp" "-regex" "-re" "-pattern")
                                      eos)
                                  (symbol-name name)))
              (relint--check-re re-arg name pos (cons 2 path))
              (when (eq (car form) 'defcustom)
                (relint--check-defcustom-re form name pos path))
              (push name relint--checked-variables))
             ((and (consp type)
                   (eq (car type) 'alist)
                   (relint--defcustom-type-regexp-p
                    (plist-get (cdr type) :key-type)))
              (relint--check-list-any re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ((and (consp type)
                   (eq (car type) 'alist)
                   (relint--defcustom-type-regexp-p
                    (plist-get (cdr type) :value-type)))
              (relint--check-alist-cdr re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ((or (and (consp type)
                       (eq (car type) 'repeat)
                       (relint--defcustom-type-regexp-p (cadr type)))
                  (string-match-p (rx (or (or "-regexps" "-regexes")
                                          (seq (or "-regexp" "-regex" "-re")
                                               "-list"))
                                      eos)
                                  (symbol-name name)))
              (relint--check-list re-arg name pos (cons 2 path) nil)
              (push name relint--checked-variables))
             ((and (symbolp name) (string-match-p (rx "font-lock-keywords")
                                                  (symbol-name name)))
              (relint--check-font-lock-keywords re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ((eq name 'compilation-error-regexp-alist-alist)
              (relint--check-compilation-error-regexp-alist-alist
               re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ((eq name 'auto-mode-alist)
              (relint--check-auto-mode-alist re-arg name pos (cons 2 path)))
             ((string-match-p (rx (or "-regexp" "-regex" "-re" "-pattern")
                                  "-alist" eos)
                              (symbol-name name))
              (relint--check-alist-any re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ((string-match-p (rx "-mode-alist" eos)
                              (symbol-name name))
              (relint--check-list-any re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ((string-match-p (rx "-rules-list" eos)
                              (symbol-name name))
              (relint--check-rules-list re-arg name pos (cons 2 path))
              (push name relint--checked-variables))
             ;; Doc string starting with "regexp" etc.
             ((and (stringp (car rest))
                   (let ((case-fold-search t))
                     (string-match-p
                      (rx bos
                          (opt (or "when" "if")
                               (* " ")
                               (or "not" "non")
                               (* (any "- "))
                               "nil"
                               (* (any " ,")))
                          (opt (or "specify" "specifies")
                               " ")
                          (opt (or "a" "the" "this") " ")
                          (or "regex" "regular expression"))
                      (car rest))))
              (relint--check-re re-arg name pos (cons 2 path))
              (when (eq (car form) 'defcustom)
                (relint--check-defcustom-re form name pos path))
              (push name relint--checked-variables))
             )

            (let* ((old (assq name relint--variables))
                   (new
                    (or (and old
                             ;; Redefinition of the same variable: eagerly
                             ;; evaluate the new expression in case it uses
                             ;; the old value.
                             (let ((val (catch 'relint-eval
                                          (list (relint--eval re-arg)))))
                               (and (consp val)
                                    (cons 'val val))))
                        (list 'expr re-arg))))
              (push (cons name new) relint--variables)))))
       (`(rx . ,items)
        (let ((i 1))
          (while (consp items)
            (relint--check-rx (car items) pos (cons i path) t)
            (setq items (cdr items))
            (setq i (1+ i)))))
       (`(rx-to-string (,(or 'quote '\`) ,arg) . ,_)
        (relint--check-rx arg pos (cons 1 (cons 1 path)) t))
       (`(font-lock-add-keywords ,_ ,keywords . ,_)
        (relint--check-font-lock-keywords
         keywords (car form) pos (cons 2 path)))
       (`(set (make-local-variable ',name) ,expr)
        (cond ((memq name relint--known-regexp-variables)
               (relint--check-re expr name pos (cons 2 path)))
              ((eq name 'font-lock-defaults)
               (relint--check-font-lock-defaults expr name pos (cons 2 path)))
              ((and (symbolp name) (string-match-p (rx "font-lock-keywords")
                                                   (symbol-name name)))
               (relint--check-font-lock-keywords expr name pos (cons 2 path)))
              ((eq name 'imenu-generic-expression)
               (relint--check-imenu-generic-expression
                expr name pos (cons 2 path)))))
       (`(define-generic-mode ,name ,_ ,_ ,font-lock-list ,auto-mode-list . ,_)
        (let ((origin (format "define-generic-mode %s" name)))
          (relint--check-font-lock-keywords font-lock-list origin
                                            pos (cons 4 path))
          (relint--check-list auto-mode-list origin pos (cons 5 path) t)))
       (`(,(or 'syntax-propertize-rules 'syntax-propertize-precompile-rules)
          . ,rules)
        (let ((index 1))
          (dolist (item rules)
            (when (consp item)
              (relint--check-re (car item)
                                (format "call to %s" (car form))
                                pos (cons 0 (cons index path))))
            (setq index (1+ index)))))
       (`(add-to-list 'auto-mode-alist ,elem . ,_)
        (relint--check-auto-mode-alist-expr
         elem (car form) pos (cons 2 path)))
       (`(modify-coding-system-alist ,type ,re-arg ,_)
        (funcall
         (if (eq (relint--eval-or-nil type) 'file)
             #'relint--check-file-name-re
           #'relint--check-re)
         re-arg (format "call to %s" (car form)) pos (cons 2 path)))
       (`(,name . ,args)
        (let ((alias (assq name relint--alias-defs)))
          (when alias
            (relint--check-form-recursively-2
             (cons (cdr alias) args) mutables pos path))))
       )

     ;; Check calls to remembered functions with regexp arguments.
     (when (consp form)
       (let ((indices (cdr (assq (car form) relint--regexp-functions))))
         (when indices
           (let ((index 0)
                 (args (cdr form)))
             (while (and indices (consp args))
               (when (= index (car indices))
                 (unless (and (symbolp (car args))
                              (memq (car args) relint--checked-variables))
                   (relint--check-re (car args)
                                     (format "call to %s" (car form))
                                     pos (cons (1+ index) path)))
                 (setq indices (cdr indices)))
               (setq args (cdr args))
               (setq index (1+ index)))))))

     ;; FIXME: All function applications, and some macros / special forms
     ;; (prog{1,2,n}, save-excursion...) could be scanned with full
     ;; mutables since all args are evaluated once.
     (let ((index 0))
       (while (consp form)
         (cond
          ((consp (car form))
           ;; Check subforms with the assumption that nothing can be mutated,
           ;; since we don't really know what is evaluated when.
           (relint--check-form-recursively-2
            (car form) nil pos (cons index path)))
          ((and (memq (car form) '(:regexp :regex))
                (consp (cdr form)))
           (relint--check-re (cadr form)
                             (format "%s parameter" (car form))
                             pos (cons (1+ index) path))))
         (setq form (cdr form))
         (setq index (1+ index)))))))

(defun relint--show-errors (error-buffer quiet)
  (unless (or noninteractive quiet (not error-buffer))
    (let ((pop-up-windows t))
      (display-buffer error-buffer)
      (sit-for 0))))

(defun relint--read-buffer ()
  "Read top-level forms from the current buffer.
Return a list of (FORM . STARTING-POSITION)."
  (goto-char (point-min))
  (let ((pos nil)
        (keep-going t)
        (read-circle nil)
        (forms nil))
    (while keep-going
      (setq pos (point))
      (let ((form nil))
        (condition-case err
            (setq form (read (current-buffer)))
          (end-of-file
           (setq keep-going nil))
          (invalid-read-syntax
           (cond
            ((equal (cadr err) "#")
             (goto-char pos)
             (forward-sexp 1))
            (t
             (relint--err (point) nil (prin1-to-string err))
             (setq keep-going nil))))
          (error
           (relint--err (point) nil (prin1-to-string err))
           (setq keep-going nil)))
        (when (consp form)
          (push (cons form pos) forms))))
    (nreverse forms)))

(defun relint--in-doc-string-p (pos)
  "Whether the string literal starting at POS is a doc string."
  (save-excursion
    (goto-char pos)
    ;; Go back to start of containing sexp, counting the steps.
    (let ((steps 0))
      (while (and (not (bobp))
                  (ignore-errors
                    (forward-sexp -1)
                    t)
                  (not (and (= steps 0)
                            (looking-at (rx ":documentation" symbol-end)))))
        (setq steps (1+ steps)))
      (or
       (and (= steps 0)
            (looking-at (rx ":documentation")))
       (and (= steps 3)
            (looking-at (rx (or "defun" "defmacro" "defsubst" "defalias"
                                "defconst" "defvar" "defcustom"
                                "autoload"
                                "cl-defun" "cl-defmacro" "cl-defmethod"
                                "ert-deftest"
                                ;; Specific to cc-mode
                                "c-lang-defvar"
                                ;; Specific to gnus
                                "defvoo"))))
       (and (= steps 2)
            (looking-at (rx (or "define-major-mode" "define-minor-mode"
                                ;; Specific to cc-mode
                                "c-lang-defconst"))))
       (and (= steps 4)
            (looking-at (rx (or "define-derived-mode"))))))))

(defun relint--suspicious-backslash (string-start)
  "With point at an ineffective backslash, emit an warning unless filtered out.
STRING-START is the start of the string literal (first double quote)."
  (let ((c (char-after (1+ (point)))))
    ;; Filter out escaped round and square brackets and apostrophes
    ;; inside doc strings, as well as anything in the leftmost column:
    ;; common for historical reasons and less likely to be mistakes.
    (unless (or (bolp)
                (and (memq c '(?\( ?\) ?\[ ?\] ?\'))
                     (relint--in-doc-string-p string-start)))
      (relint--warn (point) nil
                    (format-message
                     "Ineffective string escape `\\%s'"
                     (relint--escape-string (char-to-string c) nil))))))

(defun relint--check-for-misplaced-backslashes ()
  "Check for misplaced backslashes in the current buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (when (looking-at (rx (1+ (or (seq "?" (or (seq ?\\ anything)
                                               (not (any ?\\))))
                                  (seq "\\" anything)
                                  (seq ";" (0+ nonl))
                                  (not (any ?\" ?\; ?? ?\\))))))
      (goto-char (match-end 0)))
    (when (eq (following-char) ?\")
      (let ((string-start (point)))
        (forward-char)
        (while (not (looking-at (rx (or ?\" eot))))
          (when (looking-at
                 (rx (1+ (or (seq ?\\ (any "0-9" "xuUN" "abfnrtv"
                                           "des" "^" " "
                                           ?\\ ?\n ?\"
                                           "CM"))
                             (not (any ?\\ ?\"))))))
            (goto-char (match-end 0)))
          (when (eq (following-char) ?\\)
            (relint--suspicious-backslash string-start)
            (forward-char 2)))
        (unless (eobp)
          (forward-char 1))))))

(defun relint--scan-current-buffer ()
  (let* ((relint--suppression-count 0)
         (relint--complaints nil)
         (forms (relint--read-buffer))
         (relint--variables nil)
         (relint--checked-variables nil)
         (relint--regexp-functions nil)
         (relint--regexp-returning-functions
          relint--known-regexp-returning-functions)
         (relint--function-defs nil)
         (relint--macro-defs nil)
         (relint--alias-defs nil)
         (relint--locals nil)
         (case-fold-search nil))
    (dolist (form forms)
      (relint--check-form-recursively-1 (car form) (cdr form) nil))
    (dolist (form forms)
      (relint--check-form-recursively-2 (car form) nil (cdr form) nil))
    (relint--check-for-misplaced-backslashes)
    (let ((complaints (nreverse relint--complaints)))
      (cons
       (sort complaints
             ;; Sort by error position if available, expression position
             ;; otherwise.
             (lambda (a b)
               (let ((expr-pos-a (nth 1 a))
                     (expr-pos-b (nth 1 b))
                     (error-pos-a (nth 2 a))
                     (error-pos-b (nth 2 b)))
                 (< (or error-pos-a expr-pos-a)
                    (or error-pos-b expr-pos-b)))))
       relint--suppression-count))))

(defvar relint-last-target nil
  "The last file, directory or buffer on which relint was run.")

(defun relint--prepare-error-buffer (target base-dir error-buffer quiet)
  (when error-buffer
    (with-current-buffer error-buffer
      (unless quiet
        (let ((inhibit-read-only t))
          (insert (format "Relint results for %s\n" target)))
        (relint--show-errors error-buffer quiet))
      (setq relint-last-target target)
      (setq default-directory base-dir))))

(defun relint--finish (errors suppressed error-buffer quiet)
  (let* ((msg (format "%d error%s%s"
                      errors (if (= errors 1) "" "s")
                      (if (zerop suppressed)
                          ""
                        (format " (%s suppressed)" suppressed)))))
    (unless (or quiet (and noninteractive (zerop errors)))
      (unless noninteractive
        (relint--add-to-error-buffer error-buffer
                                     (format "\nFinished -- %s.\n" msg)))
      (message "relint: %s." msg))))

(defun relint-again ()
  "Re-run relint on the same file, directory or buffer as last time."
  (interactive)
  (cond ((bufferp relint-last-target)
         (with-current-buffer relint-last-target
           (relint-current-buffer)))
        ((file-directory-p relint-last-target)
         (relint-directory relint-last-target))
        ((file-readable-p relint-last-target)
         (relint-file relint-last-target))
        (t (error "No target"))))

(defvar relint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "g" 'relint-again)
    map)
  "Keymap for relint buffers.")

(define-compilation-mode relint-mode "Relint"
  "Mode for relint output."
  (setq-local relint-last-target nil))

(defun relint--scan-files (files target base-dir error-buffer)
  "Scan FILES in BASE-DIR; return (ERRORS . SUPPRESSED).
TARGET is the file or directory to use for a repeated run."
  (relint--prepare-error-buffer target base-dir error-buffer nil)
  (let ((total-errors 0)
        (total-suppressed 0)
        (nfiles (length files))
        (count 0))
    (dolist (file files)
      (when (and (not noninteractive)
                 (zerop (% count 50)))
        (message "Scanned %d/%d file%s..."
                 count nfiles (if (= nfiles 1) "" "s")))
      (setq count (1+ count))
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert-file-contents file)
        ;; Call file-relative-name lazily -- it is surprisingly expensive
        ;; on macOS, and the result only used for diagnostics output.
        (let* ((results (relint--scan-current-buffer))
               (complaints (car results))
               (suppressed (cdr results)))
          (when complaints
            (relint--output-complaints (current-buffer)
                                       (file-relative-name file base-dir)
                                       complaints error-buffer))
          (setq total-errors (+ total-errors (length complaints) suppressed))
          (setq total-suppressed (+ total-suppressed suppressed))
          (when complaints
            (relint--show-errors error-buffer nil)))))
    (relint--finish total-errors total-suppressed error-buffer nil)
    (cons total-errors total-suppressed)))

(defun relint--tree-files (dir)
  (let ((re (rx bos (not (any ".")) (* anything) ".el" eos)))
    (if (eval-when-compile (>= emacs-major-version 27))
        (directory-files-recursively
         dir re nil
         ;; Save time by not pointlessly descending into huge .git directories.
         (lambda (s) (not (string-suffix-p "/.git" s))))
      (directory-files-recursively dir re))))

(defun relint--scan-buffer (buffer)
  "Scan BUFFER; return (COMPLAINTS . SUPPRESSED) where
COMPLAINTS is a list of (unsuppressed) diagnostics each on the form
   (MESSAGE EXPR-POS ERROR-POS STRING STRING-IDX SEVERITY)
and SUPPRESSED is the number of suppressed diagnostics."
  (with-current-buffer buffer
    (unless (derived-mode-p 'emacs-lisp-mode)
      (error "Relint: can only scan elisp code (use emacs-lisp-mode)"))
    (save-excursion
      (relint--scan-current-buffer))))

(defun relint--buffer (buffer error-buffer quiet)
  ;; FIXME: With 0 errors, maybe don't pop up the error buffer at all?
  (let* ((results (relint--scan-buffer buffer))
         (complaints (car results))
         (suppressed (cdr results))
         (errors (+ (length complaints) suppressed)))
    (relint--prepare-error-buffer buffer default-directory error-buffer quiet)
    (when complaints
      (relint--output-complaints buffer (buffer-name buffer)
                                 complaints error-buffer))
    (relint--finish errors suppressed error-buffer quiet)))


;;;###autoload
(defun relint-file (file)
  "Scan FILE, an elisp file, for regexp-related errors."
  (interactive "fRelint elisp file: ")
  (relint--scan-files (list file) file (file-name-directory file)
                      (relint--get-error-buffer)))

;;;###autoload
(defun relint-directory (dir)
  "Scan all *.el files in DIR for regexp-related errors."
  (interactive "DRelint directory: ")
  (message "Finding .el files in %s..." dir)
  (let ((files (relint--tree-files dir)))
    (if files
        (relint--scan-files files dir dir (relint--get-error-buffer))
      (message "No .el files found."))))

;;;###autoload
(defun relint-current-buffer ()
  "Scan the current buffer for regexp errors.
The buffer must be in emacs-lisp-mode."
  (interactive)
  (relint--buffer (current-buffer) (relint--get-error-buffer) nil))

;;;###autoload
(defun relint-buffer (buffer)
  "Scan BUFFER for regexp errors. Return list of diagnostics.
Each element in the returned list has the form

  (MESSAGE EXPR-POS ERROR-POS STRING STRING-IDX SEVERITY),

where MESSAGE is the message string, EXPR-POS the location of the
flawed expression, ERROR-POS the exact position of the error or
nil if unavailable, STRING is nil or a string to which the
message pertains, STRING-IDX is nil or an index into STRING,
and SEVERITY is `error' or `warning'.
The intent is that ERROR-POS is the position in the buffer that
corresponds to STRING at STRING-IDX, if such a location can be
determined."
  (car (relint--scan-buffer buffer)))

(defun relint-batch ()
  "Scan elisp source files for regexp-related errors.
Call this function in batch mode with files and directories as
command-line arguments.  Files are scanned; directories are
searched recursively for *.el files to scan.
When done, Emacs terminates with a nonzero status if anything worth
complaining about was found, zero otherwise."
  (unless noninteractive
    (error "`relint-batch' is only for use with -batch"))
  (let* ((err-supp
          (relint--scan-files (mapcan (lambda (arg)
                                        (if (file-directory-p arg)
                                            (relint--tree-files arg)
                                          (list arg)))
                                      command-line-args-left)
                              nil default-directory nil))
         (errors (car err-supp))
         (suppressed (cdr err-supp)))
    (setq command-line-args-left nil)
    (kill-emacs (if (> errors suppressed) 1 0))))

(provide 'relint)

;;; relint.el ends here
