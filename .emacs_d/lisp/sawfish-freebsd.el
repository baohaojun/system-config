;;; sawfish.el --- Sawfish mode.
;; Copyright 1999,2000,2001,2002,2003,2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.32 $

;; sawfish.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; sawfish.el is an emacs mode for writing code for the sawfish window
;; manager <URL:http://sawmill.sourceforge.net/>. As well as providing a
;; programming mode it also allows for direct interaction with the running
;; window manager.
;;
;; The latest sawfish.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/#sawfish.el>

;;; THANKS:
;;
;; John Harper <john@dcs.warwick.ac.uk> for help regarding sawfish and rep.
;;
;; Stefan Monnier for finding the font-lock (or lack of) with derived modes
;; problem and providing a fix for GNU Emacs.
;;
;; Jan Vroonhof for his invaluable pointers regarding XEmacs.
;;
;; Hubert Selhofer for the code to syntax highlight "#||#" comments, for the
;; GNU emacs font-lock code to provide support for various rep and sawfish
;; "keywords" and for the GNU emacs emacs-lisp menu removal kludge.
;;
;; Kai Grossjohann for his enhancments to `sawfish-console'.
;;
;; Markus Holmberg for the code that improves integration with info.

;;; BUGS:
;;
;; o The handling of the apropos buffer totally breaks down under XEmacs.
;;
;; o sawfish.el needs a total rewrite. When I started this mode rep (the
;;   lisp that sawfish is based around) was an elisp-a-like. Since then it
;;   has turned into a scheme-a-like that happens to retain some
;;   elisp-a-like bits. Ideally a new sawfish.el would be written in terms
;;   of a librep.el which would be a ground-up-rewritten mode for dealing
;;   with rep.

;;; INSTALLATION:
;;
;; o Drop sawfish.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add autoloads for the various sawfish functions to ~/.emacs. At the
;;   very least you want to do something like:
;;
;;   (autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
;;
;; o Add the following to ~/.emacs to ensure that sawfish mode is used when
;;   you go to edit sawfish code:
;;
;;   (setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
;;         auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
;;         auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))

;;; Code:

;; Things we need:
(eval-when-compile
  (require 'cl)
  (require 'info))
(require 'thingatpt)
(require 'font-lock)
(require 'regexp-opt)
(require 'pp)
(require 'easymenu)
(require 'inf-lisp)

;; Shut the compiler up.
(eval-when-compile

  ;; Keep everyone quiet.
  (defvar sawfish-mode-map)
  (defvar sawfish-mode-menu)

  ;; Things to keep XEmacs quiet.
  (unless (boundp 'font-lock-defaults-alist)
    (defvar font-lock-defaults-alist))

  ;; Things to keep GNU Emacs quiet.
  (unless (boundp 'delete-menu-item)
    (defun delete-menu-item (path)
      nil)))

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup sawfish nil
  "Mode for editing the configuration of and interacting with the sawfish
window manager."
  :group 'languages
  :prefix "sawfish-")

(defcustom sawfish-client "sawfish-client"
  "*Command for interacting with the window manager."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-exec-parameter "-e"
  "*Parameter for `sawfish-client' that tells it to eval a form and exit."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-interactive-parameter "-"
  "*Interactive mode parameter for `sawfish-client'."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-result-buffer "*sawfish*"
  "*Name of the long result display buffer."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-help-buffer "*sawfish-help*"
  "*Name of the sawfish help buffer."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-apropos-buffer "*sawfish-apropos*"
  "*Name of the sawfish apropos buffer."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-scratch-buffer "*sawfish-scratch*"
  "*Name of the sawfish scratch buffer."
  :type  'string
  :group 'sawfish)

(defcustom sawfish-buffer-symbol-lists t
  "*Buffer the lists of function and variable names?"
  :type  'boolean
  :group 'sawfish)

(defcustom sawfish-apropos-searches-info-files t
  "*Search info files for apropos \"one-liner\" help?

This variable controls the action of the sawfish apropos functions. When nil
the apropos functions won't go looking in the sawfish info files for a
one-line doc-string to display in the apropos buffer if the symbol doesn't
have a doc-string. This will make apropos calls a lot faster."
  :type  'boolean
  :group 'sawfish)

(defcustom sawfish-mode-hook nil
  "*List of hooks to execute on entry to sawfish-mode."
  :type  'hook
  :group 'sawfish)

(defcustom sawfish-info-files '(("sawfish" "Function Index" "Variable Index")
                                ("librep"  "Function Index" "Variable Index"))
  "*List of info files to search when looking for info documentation.

This is a list of lists. Each entry in the list is of the format:

  (INFO-FILE FUNCTION-INDEX VARIABLE-INDEX)"
  :type  '(repeat (list    :tag "Info file information"
                   (string :tag "Info file name")
                   (string :tag "Function index name")
                   (string :tag "Variable index name")))
  :group 'sawfish)

(defcustom sawfish-comint-prompt "^sawfish% "
  "*Regular expression for matching the sawfish-client prompt."
  :type  'regexp
  :group 'sawfish)

(defcustom sawfish-extra-keyword-list
  '("add-frame-style" "call-after-load" "call-after-property-changed"
    "call-after-state-changed" "custom-set-property")
  "List of extra keywords for Sawfish used in highlighting.
Highlight these expressions with `font-lock-keyword-face'."
  :group 'sawfish
  :type '(repeat (string :tag "Keyword: ")))

(defcustom sawfish-warning-keyword-list
  '("fixme" "FIXME" "Fixme" "fix me" "Fix me" "!!!" "Grrr" "Bummer")
  "List of keywords for Sawfish used in highlighting.
Highlight these expressions with `font-lock-warning-face' even if
already fontified."
  :group 'sawfish
  :type '(repeat (string :tag "Keyword: ")))

;; Non customising variables.

(defvar sawfish-function-list nil
  "List of sawfish functions.")

(defvar sawfish-variable-list nil
  "List of sawfish variables.")

(defvar sawfish-function-p '(lambda (s)
                             (and
                              (boundp s)
                              (or
                               (functionp (symbol-value s))
                               (macrop (symbol-value s))
                               (special-form-p (symbol-value s)))))
  "Closure to pass to sawfish-client for testing if a symbol is a function.")

(defvar sawfish-variable-p `(lambda (s)
                             (and (boundp s)
                              (not (,sawfish-function-p s))))
  "Closure to pass to sawfish-client for testing if a symbol is a variable.")

(defvar sawfish-read-expression-map nil
  "Minibuffer keymap used for reading sawfish lisp expressions.")

(defvar sawfish-interaction-mode-map nil
  "Keymap for use with `sawfish-interaction'.")

(defvar sawfish-read-expression-history nil
  "History list for `sawfish-eval-expression'.")

(defvar sawfish-describe-symbol
  '(lambda (s)
    (if (boundp s)
        (cond ((special-form-p      (symbol-value s)) "Special form")
              ((macrop              (symbol-value s)) "Macro")
              ((subrp               (symbol-value s)) "Built-in function")
              ((commandp            (symbol-value s)) "Command")
              ((functionp           (symbol-value s)) "Function")
              ((binding-immutable-p               s ) "Constant")
              (t                                      "Variable"))
      "Symbol"))
  "Closure to pass to sawfish-client that will describe a symbol's binding.")

;; Constants.

(defconst sawfish-defines-regexp
    (concat "(\\("
            (regexp-opt
             ;; A cute way to obtain the list below would be:
             ;; (sawfish-code (mapcar symbol-name (apropos "^define")))
             ;;
             ;; It would, however, mean that you'd have a list of "keywords"
             ;; define in your running instance of sawfish. It would also
             ;; mean that you'd have to have sawfish running at the time
             ;; that this constant is defined.
             (list
              "define" "define-command-args" "define-command-to-screen"
              "define-custom-deserializer" "define-custom-serializer"
              "define-custom-setter" "define-datum-printer"
              "define-file-handler" "define-focus-mode"
              "define-frame-class" "define-frame-type-mapper"
              "define-interface" "define-linear-viewport-commands"
              "define-match-window-formatter"
              "define-match-window-group" "define-match-window-property"
              "define-match-window-setter" "define-parse"
              "define-placement-mode" "define-record-type"
              "define-record-discloser" "define-scan-body"
              "define-scan-form" "define-scan-internals"
              "define-structure" "define-value"
              "define-window-animator"))
            "\\)\\>[ \t'(]*\\(\\sw+\\)?")
  "List of define-structures known by Sawfish.")

(defconst sawfish-additional-keywords
  (append lisp-font-lock-keywords-2
          (list
           ;; highlight define-*
           (list
            sawfish-defines-regexp
            '(1 font-lock-keyword-face)
            `(,(regexp-opt-depth sawfish-defines-regexp)
              font-lock-variable-name-face nil t))
           ;; extra keywords
           (if sawfish-extra-keyword-list
               (list (concat "\\<"
                             `,(regexp-opt sawfish-extra-keyword-list)
                             "\\>")
                     '(0 font-lock-keyword-face)))
           ;; highlight warnings
           (if sawfish-warning-keyword-list
               (list (concat "\\<"
                             `,(regexp-opt sawfish-warning-keyword-list)
                             "\\>")
                     '(0 font-lock-warning-face prepend)))))
  "Some additonal keywords to highlight in `sawfish-mode'.")

;; Main code:

;;;###autoload
(define-derived-mode sawfish-mode emacs-lisp-mode "Sawfish"
  "Major mode for editing sawfish files and for interacting with sawfish.

Special commands:

\\{sawfish-mode-map}"
  ;; `define-derived-mode' in both GNU Emacs and XEmacs doesn't appear to
  ;; derive the font-lock settings. So, depending on the editor in use we
  ;; need to drag those settings down to us in different ways (hmm)....
  (if (and (boundp 'running-xemacs) (symbol-value 'running-xemacs))
      ;; XEmacs appears to do something like this...
      (put 'sawfish-mode 'font-lock-defaults
           (get 'emacs-lisp-mode 'font-lock-defaults))
    ;; ...with GNU Emacs we need to pull it from `font-lock-defaults-alist'.
    (unless font-lock-defaults
      (set (make-local-variable 'font-lock-defaults)
           (cdr (assoc 'emacs-lisp-mode font-lock-defaults-alist)))
      ;; Add the additional font-lock pattern to `font-lock-defaults'
      ;; only once
      (unless (memq 'sawfish-additional-keywords (car font-lock-defaults))
        (setq font-lock-defaults (copy-alist font-lock-defaults))
        (setcar font-lock-defaults
                (append (car font-lock-defaults)
                        '(sawfish-additional-keywords))))))
  ;; Menu stuff.
  (if (and (boundp 'running-xemacs) (symbol-value 'running-xemacs))
      ;; XEmacs.
      (progn
        ;; For some odd reason `delete-menu-item' doesn't seem to always work.
        ;; Anyone know why?
        (delete-menu-item '("Emacs-Lisp"))
        ;; XEmacs seems to require that you add the menu yourself.
        (easy-menu-add sawfish-mode-menu))
    ;; See the end of this file for the code that removes the emacs lisp
    ;; menu.
    )
  ;; Add support for #| ... |# style comments (call it style b) see GNU
  ;; Emacs Lisp Reference Manual (Rev. 2.5), p. 673-675
  (modify-syntax-entry ?# "' 14b")      ; quote or comment (style b)
  (modify-syntax-entry ?| "_ 23b")      ; symbol or comment (style b)
  (modify-syntax-entry ?\n ">a")        ; end comment (style a)
  ;; The following adds some indentation information to help sawfish-mode
  ;; (rep is a sort of elisp/scheme hybrid with some extra stuff of its own,
  ;; we inherit from emacs-lisp-mode so we need to add a sprinkle of scheme
  ;; support).
  (loop for sym in '((define                  . 1)
                     (define-interface        . 1)
                     (define-record-discloser . 1)
                     (define-record-type      . 1)
                     (define-structure        . 3)
                     (letrec                  . 1)
                     (structure               . 2)
                     (with-output-to-screen   . 0))
        do (unless (get (car sym) 'lisp-indent-function)
             (put (car sym) 'lisp-indent-function (cdr sym)))))

(defun sawfish-eval (sexp &optional target-buffer)
  "Pass SEXP to sawfish for evaluation.

SEXP can either be a list or a string.

If passed the result of the evaluation is inserted into TARGET-BUFFER."
  (call-process sawfish-client nil target-buffer nil sawfish-exec-parameter
                (if (stringp sexp) sexp (format "%S" sexp))))

(defun sawfish-string-readable-p (sexp)
  "Can string SEXP be safely `read'?"
  (not (string-match "#<\\w+" sexp)))

(defun sawfish-buffer-readable-p (&optional buffer)
  "Can the content of BUFFER be safely `read'?"
  (sawfish-string-readable-p
   (with-current-buffer (or buffer (current-buffer))
     (buffer-string))))

(defun sawfish-eval-noread (sexp)
  "Eval SEXP and return the result without `read'ing it."
  (with-temp-buffer
    (sawfish-eval sexp t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun sawfish-eval-read (sexp)
  "Eval SEXP and return the result of `read'ing the result.

SEXP can either be a list or a string."
  (let ((result (sawfish-eval-noread sexp)))
    (if (sawfish-string-readable-p result)
        (read result)
      result)))

;;;###autoload
(defun sawfish-eval-region (start end &optional target-buffer)
  "Evaluate the region bounded by START and END.

TARGET-BUFFER is the optional target for the return value of the
evaluation."
  (interactive "r")
  (sawfish-eval (buffer-substring-no-properties start end) target-buffer))

;;;###autoload
(defun sawfish-eval-buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (sawfish-eval-region (point-min) (point-max) nil))

;;;###autoload
(defun sawfish-eval-defun (insert-value)
  "Evaluate the top level form at or near `point'.

INSERT-VALUE is a prefix parameter, if it is non-NIL the value of the
expression is inserted into the buffer after the form."
  (interactive "P")
  (save-restriction
    (save-excursion
      (narrow-to-defun)
      (setf (point) (point-max))
      (let ((result (sawfish-eval-last-sexp nil)))
        (if insert-value
            (let ((standard-output (current-buffer)))
              (setf (point) (point-min))
              (end-of-defun)
              (unless (bolp)
                (terpri))
              (princ result)
              (terpri))
          (sawfish-output result))))))

;;;###autoload
(defun sawfish-eval-expression (sexp &optional insert-value)
  "Evaluate SEXP and display the value in the minibuffer.

If the optional parameter INSERT-VALUE is supplied as a non-NIL value the
value of SEXP will be inserted into the current buffer."
  (interactive
   (list
    (read-from-minibuffer "Sawfish Eval: " nil sawfish-read-expression-map t 'sawfish-read-expression-history)
    current-prefix-arg))
  (let ((result (sawfish-eval-noread sexp)))
    (if insert-value
        (let ((standard-output (current-buffer)))
          (princ result))
      (sawfish-output result))))

(defun sawfish-output (output)
  "Display output either in mini-buffer or a seperate buffer.

If the output is empty then the string \"No output\" is displayed.

If the output is one line long and the length of the line is less than the
`frame-width' then it is displayed using `message'.

If the output has multiple lines or is longer than `frame-width' then a new
buffer is opened and the text is displayed there. The name of the buffer is
set by the variable `sawfish-result-buffer'"
  (with-temp-buffer
    (let ((temp-buffer (current-buffer)))
      (insert output)
      (let ((lines (count-lines (point-min) (point-max))))
        (cond
          ((zerop lines)                ; Nothing to display.
           (message "No output"))
          ((and (= 1 lines)             ; If there is only one line
                (< (- (point-max)       ; and it isn't too wide for
                      (point-min))      ; the display.
                   (frame-width)))
           (setf (point) (point-min))
           (replace-string "\n" "")     ; Strip any trailing EOLs.
           (when (get-buffer-window sawfish-result-buffer)
             ;; The long result buffer is visible, delete it.
             (delete-window (get-buffer-window sawfish-result-buffer)))
           (message "%s" (buffer-string)))
          (t                            ; Too large for message area, use a buffer.
           (with-output-to-temp-buffer sawfish-result-buffer
             (with-current-buffer sawfish-result-buffer
               (if (sawfish-string-readable-p output)
                   (pp (read output) (current-buffer))
                 (setf (buffer-string) (format "%s" (with-current-buffer temp-buffer
                                                      (buffer-string)))))
               (shrink-window-if-larger-than-buffer (display-buffer (current-buffer))))
             (bury-buffer (current-buffer)))))))))

(defun sawfish-insert (string)
  "Insert STRING into `current-buffer', pretty print if at all possible."
  (if (sawfish-string-readable-p string)
      (pp (read string) (current-buffer))
    (insert string)))

;;;###autoload
(defun sawfish-eval-last-sexp (to-buffer)
  "Version of `eval-last-sexp' that interacts with sawfish."
  (interactive "P")
  (let ((home-buffer (current-buffer)))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer home-buffer
          (sawfish-eval-region (save-excursion
                                 (backward-sexp)
                                 (point))
                               (point)
                               temp-buffer)
          (funcall (if to-buffer
                       #'sawfish-insert
                     #'sawfish-output)
                   (with-current-buffer temp-buffer (buffer-string))))))))

;;;###autoload
(defun sawfish-eval-print-last-sexp ()
  (interactive)
  (insert "\n")
  (sawfish-eval-last-sexp t))

(defmacro sawfish-code (&rest body)
  "Pass BODY to sawfish for evaluation."
  `(sawfish-eval-read (cons 'progn (quote ,body))))

(defun sawfish-load-helpers ()
  "Load modules that help us work with sawfish."
  (sawfish-code
    (require 'rep.structures)
    (require 'lisp-doc)))

(defun sawfish-load-symbols (&optional force)
  "Loads the names of the sawfish functions and variables."
  (unless (and (not (or force (not sawfish-buffer-symbol-lists)))
               sawfish-function-list sawfish-variable-list)
    (setq sawfish-function-list nil
          sawfish-variable-list nil)
    (flet ((sawfish-fun-p (sym) (second sym))
           (sawfish-var-p (sym) (third sym)))
      (loop for sym in (sawfish-eval-read
                        `(mapcar (lambda (sym)
                                   (list
                                    (symbol-name sym)
                                    (or (macrop sym) (,sawfish-function-p sym))
                                    (,sawfish-variable-p sym)))
                          (apropos ".")))
            if (sawfish-fun-p sym) do (push (list (car sym)) sawfish-function-list)
            if (sawfish-var-p sym) do (push (list (car sym)) sawfish-variable-list)))))

(defun sawfish-documentation (symbol &optional is-variable)
  "Get the documentation for SYMBOL."
  (sawfish-eval-read `(documentation (quote ,symbol) ,is-variable)))

(defun sawfish-funcall-at-point ()
  "Try and work out the function being called at or near `point'."
  ;; `thing-at-point', when trying to grab a list, doesn't appear to do what
  ;; I need most of the time. I need to figure out what is wrong or write
  ;; something better.
  (let ((list (thing-at-point 'list)))
    (when list
      (let ((fun (symbol-name (car (read list)))))
        (when (assoc fun sawfish-function-list)
          fun)))))

(defun sawfish-variable-at-point ()
  "Try and work out the variable being called at or near `point'."
  (let ((sym (thing-at-point 'symbol)))
    (when sym
      (let ((var (symbol-name (read sym))))
        (when (assoc var sawfish-variable-list)
          var)))))

(defun sawfish-describe-ask (default description lookups)
  "Ask the user for a symbol.

The symbol will be described as DESCRIPTION with a completing read using
LOOKUPS for the completion. DEFAULT should be a function that returns a
default value for the read."
  (sawfish-load-symbols)
  (intern (completing-read (format "Sawfish %s: " description)
                           (symbol-value lookups)
                           nil
                           t
                           (funcall default))))

(defun sawfish-describe-ask-function ()
  "Ask for a function name."
  (sawfish-describe-ask #'sawfish-funcall-at-point "function" 'sawfish-function-list))

(defun sawfish-describe-ask-variable ()
  "Ask for a variable name."
  (sawfish-describe-ask #'sawfish-variable-at-point "variable" 'sawfish-variable-list))

(defun sawfish-info-function-index (info-file)
  "Return the name of the function index from INFO-FILE.

This function is used to pull information from the entries found in the
variable `sawfish-info-files'."
  (cadr info-file))

(defun sawfish-info-variable-index (info-file)
  "Return the name of the variable index from INFO-FILE.

This function is used to pull information from the entries found in the
variable `sawfish-info-files'."
  (car (cddr info-file)))

(defun sawfish-info-index-function (is-variable)
  "Return the a function for accessing the info file list."
  (if is-variable #'sawfish-info-variable-index #'sawfish-info-function-index))

(defun sawfish-describe-show (symbol &optional is-variable)
  "Show the sawfish description for SYMBOL."
  (with-output-to-temp-buffer sawfish-help-buffer
    (princ (format "`%s' is a %s" symbol
                   (sawfish-eval-read `(,sawfish-describe-symbol (quote ,symbol)))))
    (when is-variable
      (princ "\n\nValue:\n\n")
      (pp (sawfish-eval-read symbol)))
    (princ "\n\nDocumentation:\n\n")
    (let ((doc (or (sawfish-documentation symbol is-variable)
                   (sawfish-search-and-grab-info (sawfish-info-index-function is-variable) symbol))))
      (if doc
          (princ doc)
        (princ (format "%s is undocumented" symbol))))
    (let ((plist (sawfish-eval-read `(symbol-plist (quote ,symbol)))))
      (when (and plist (listp plist))
        (princ "\n\nProperty list for symbol:\n")
        (loop for prop on plist by #'cddr
              do (princ (format "\n%s: %S" (car prop) (cadr prop))))))))

;;;###autoload
(defun sawfish-describe-function (function)
  "Display the doc-string for FUNCTION."
  (interactive (list (sawfish-describe-ask-function)))
  (sawfish-load-helpers)
  (sawfish-describe-show function))

;;;###autoload
(defun sawfish-describe-variable (variable)
  "Display the doc-string for VARIABLE."
  (interactive (list (sawfish-describe-ask-variable)))
  (sawfish-load-helpers)
  (sawfish-describe-show variable t))

(defun sawfish-find-info-entry (info-file node symbol)
  "Try to find SYMBOL in NODE of INFO-FILE.

If the symbol isn't found the Info buffer is killed and the function returns
nil, otherwise the Info buffer is left as the `current-buffer'."
  (condition-case nil
      (progn
        (require 'info)
        (Info-find-node info-file node)
        (Info-menu (format "%s" symbol))
        t)
    (error
     (when (string= (buffer-name) "*info*")
       (kill-buffer (current-buffer)))
     nil)))

(defun sawfish-jump-to-info-documentaiton (symbol)
  "Jump to the documentation for SYMBOL in an info buffer.

Returns NIL if the documentation could not be found. Note that the
`current-buffer' must be the info buffer you are searching."
  (prog1
      (search-forward-regexp (format "^ - .*: %s" symbol) nil t)
    (beginning-of-line)))

(defun sawfish-extract-info-entry (symbol)
  "Extract the info documentation for SYMBOL as a string."
  (when (sawfish-jump-to-info-documentaiton symbol)
    ;; For some odd reason, in XEmacs, the `current-buffer' inside
    ;; `with-output-to-string' is the string output buffer, not your
    ;; `current-buffer' before the call to `with-output-to-string'. Bizarre!
    ;; GNU emacs does the right thing.
    ;;
    ;; Anyway, to get round this odd behaviour you'll see lots of pointless
    ;; calls to `with-current-buffer' <sigh>.
    (let ((info-buffer (current-buffer)))
      (with-output-to-string nil
        (princ (with-current-buffer info-buffer
                 (buffer-substring-no-properties
                  (+ (point) 3)         ; Strip the leading " - ".
                  (save-excursion
                    (end-of-line)
                    (point)))))
        (terpri)
        (terpri)
        (with-current-buffer info-buffer
          (forward-line))
        (loop while (with-current-buffer info-buffer
                      ;; I'm not 100% sure what to look for when trying to
                      ;; find the end of a info entry. This seems to work.
                      (and (not (eobp))
                           (or (looking-at "^     ")
                               (looking-at "^ *$"))))
              do (let ((eol (with-current-buffer info-buffer
                              (save-excursion
                                (end-of-line)
                                (point)))))
                   (princ (with-current-buffer info-buffer
                            (buffer-substring-no-properties
                             (min (+ (point) 5) eol) ; Strip the leading white space.
                             eol))))
              (terpri)
              (with-current-buffer info-buffer
                (forward-line)))))))

(defun sawfish-search-and-grab-info (index-function symbol)
  "Look for SYMBOL in all the sawfish info files, return the docs.

INDEX-FUNCTION is used to decide which index name will be searched. The
function is used to access the lists in `sawfish-info-files'."
  (save-excursion
    (loop for info-file in sawfish-info-files
          if (sawfish-find-info-entry (car info-file) (funcall index-function info-file) symbol)
          return (prog1 (sawfish-extract-info-entry symbol) (kill-buffer (current-buffer)))
          finally return nil)))

(defun sawfish-search-info-files (index-function symbol)
  "Look for SYMBOL in all the sawfish info files.

INDEX-FUNCTION is used to decide which index name will be searched. The
function is used to access the lists in `sawfish-info-files'."
  (loop for info-file in sawfish-info-files
        if (sawfish-find-info-entry (car info-file) (funcall index-function info-file) symbol) return t
        finally (error "No info documentation found for %s" symbol)))

(defun sawfish-search-info-files-for-function (function)
  "Search for info documentation for FUNCTION."
  (sawfish-search-info-files #'sawfish-info-function-index function))

(defun sawfish-search-info-files-for-variable (variable)
  "Search for info documentation for VARIABLE."
  (sawfish-search-info-files #'sawfish-info-variable-index variable))

;;;###autoload
(defun sawfish-info-function (function)
  "Display the Info documentation for FUNCTION."
  (interactive (list (sawfish-describe-ask-function)))
  (sawfish-search-info-files-for-function function)
  (sawfish-jump-to-info-documentaiton function))

;;;###autoload
(defun sawfish-info-variable (variable)
  "Display the Info documentation for VARIABLE."
  (interactive (list (sawfish-describe-ask-variable)))
  (sawfish-search-info-files-for-variable variable)
  (sawfish-jump-to-info-documentaiton variable))

(defsubst sawfish-apropos-symbol (sym)
  "`sawfish-apropos' support function."
  (nth 0 sym))

(defsubst sawfish-apropos-symbol-name (sym)
  "`sawfish-apropos' support function."
  (symbol-name (sawfish-apropos-symbol sym)))

(defsubst sawfish-apropos-description (sym)
  "`sawfish-apropos' support function."
  (nth 1 sym))

(defsubst sawfish-apropos-variable-p (sym)
  "`sawfish-apropos' support function."
  (nth 2 sym))

(defsubst sawfish-apropos-doc-string (sym)
  "`sawfish-apropos' support function."
  (nth 3 sym))

(defun sawfish-doc-string-first-line (doc-string)
  "Given doc string DOC-STRING return the first line.

If the doc-string is NIL (no documentation) then \"Undocumented\" is
returned."
  (if doc-string
      (with-temp-buffer
        (insert doc-string)
        (setf (point) (point-min))
        (end-of-line)
        (buffer-substring-no-properties (point-min) (point)))
    "Undocumented"))

(defun sawfish-remove-info-one-liner-intro (doc-string)
  "Remove the leading symbol type text from an info derived doc-string."
  (when doc-string
    (with-temp-buffer
      (insert doc-string)
      (setf (point) (point-min))
      (if (search-forward-regexp ": +" nil t)
          (buffer-substring-no-properties (point) (point-max))
        doc-string))))

(defun sawfish-apropos-insert-link (sym)
  "Insert a documentation link for SYM into the apropos buffer."
  (let ((start (point)))
    (insert (sawfish-apropos-symbol-name sym))
    (put-text-property start (point) 'face 'bold))
  (insert "\n  ")
  (let ((start (point)))
    (insert (sawfish-apropos-description sym) ":")
    (put-text-property start (point) 'mouse-face 'highlight)
    (let ((local-map (make-sparse-keymap))
          (desc      `(lambda ()
                        (interactive)
                        (,(if (sawfish-apropos-variable-p sym)
                              #'sawfish-describe-variable #'sawfish-describe-function)
                         (quote ,(sawfish-apropos-symbol sym))))))
      (define-key local-map [mouse-2] desc)
      (define-key local-map [return] desc)
      (put-text-property (- start 2) (point) 'local-map local-map)))
  (insert " "
          (sawfish-doc-string-first-line (or (sawfish-apropos-doc-string sym)
                                             (and sawfish-apropos-searches-info-files
                                                  (sawfish-remove-info-one-liner-intro
                                                   (sawfish-search-and-grab-info
                                                    (sawfish-info-index-function
                                                     (sawfish-apropos-variable-p sym))
                                                    (sawfish-apropos-symbol sym))))))
          "\n"))

;;;###autoload
(defun sawfish-apropos (regexp)
  "Show all bound sawfish symbols whose names match REGEXP."
  (interactive "sSawfish Apropos (regexp): ")
  (sawfish-load-helpers)
  (let ((hits (sort (sawfish-eval-read
                     `(progn
                       (require (quote lisp-doc))
                       (mapcar
                        (lambda (s)
                          (list s
                                (,sawfish-describe-symbol s)
                                (,sawfish-variable-p s)
                                (documentation s (,sawfish-variable-p s))))
                        (apropos ,regexp))))
                    (lambda (symX symY)
                      (string< (sawfish-apropos-symbol-name symX)
                               (sawfish-apropos-symbol-name symY))))))
    (if (not (zerop (length hits)))
        (with-output-to-temp-buffer sawfish-apropos-buffer
          (with-current-buffer sawfish-apropos-buffer
            (setf (buffer-string) "")
            (loop for sym in hits do (sawfish-apropos-insert-link sym))))
      (message "No apropos matches for `%s'" regexp))))

;;;###autoload
(defun sawfish-complete-symbol ()
  "Attempt to complete the symbol at `point'."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym
      (let* ((sym        (symbol-name (read sym)))
             (sym-re     (concat "^" (regexp-quote sym)))
             (completion (sawfish-eval-read
                          `(complete-string ,sym (mapcar symbol-name (apropos ,sym-re))))))
        (if completion
            (if (equal completion sym)
                (let ((sym-list (sawfish-eval-read `(mapcar symbol-name (apropos ,(format "^%s" sym))))))
                  (when (> (length sym-list) 1)
                    (with-output-to-temp-buffer "*Completions*"
                      (display-completion-list
                       (sawfish-eval-read `(mapcar symbol-name (apropos ,sym-re)))))))
              (let ((bounds (bounds-of-thing-at-point 'symbol)))
                (delete-region (car bounds) (cdr bounds))
                (insert completion)))
          (error "No completion for `%s'" sym))))))

;;;###autoload
(defun sawfish-info ()
  "View the sawfish info file."
  (interactive)
  (info "sawfish"))

;;;###autoload
(defun sawfish-rep-info ()
  "View the librep info file."
  (interactive)
  (info "librep"))

(define-derived-mode sawfish-console-mode inferior-lisp-mode
  "*sawfish-console*" nil
  (make-local-variable 'inferior-lisp-prompt)
  (setq inferior-lisp-prompt sawfish-comint-prompt))

;;;###autoload
(defun sawfish-console ()
  "Run the sawfish client as an inferior lisp."
  (interactive)
  ;; TODO: How to set lisp-*-command variables for this particular
  ;; instantiation of the inferior lisp buffer?
  (unless (comint-check-proc "*sawfish-client*")
    (set-buffer (make-comint "sawfish-client" sawfish-client nil sawfish-interactive-parameter))
    (sawfish-console-mode))
  (set (make-local-variable 'inferior-lisp-buffer) "*sawfish-client*")
  (pop-to-buffer "*sawfish-client*"))

(defun sawfish-interaction-mode ()
  "Extend `sawfish-mode' for use with `sawfish-interaction'."
  (sawfish-mode)
  (setq major-mode 'sawfish-interaction-mode
        mode-name  "sawfish interaction")
  (use-local-map sawfish-interaction-mode-map))

;;;###autoload
(defun sawfish-interaction ()
  "Create a sawfish interaction buffer."
  (interactive)
  (let ((new-buffer (not (get-buffer sawfish-scratch-buffer))))
    (switch-to-buffer (get-buffer-create sawfish-scratch-buffer))
    (when new-buffer
      (insert ";; This buffer is for interacting with the sawfish window manager.\n\n")))
  (sawfish-interaction-mode))

;; Define the sawfish-mode keymap.
(define-key sawfish-mode-map [(control x) (control e)]             #'sawfish-eval-last-sexp)
(define-key sawfish-mode-map [(meta control x)]                    #'sawfish-eval-defun)
(define-key sawfish-mode-map [(meta :)]                            #'sawfish-eval-expression)
(define-key sawfish-mode-map [(control c) (control h) ?a]          #'sawfish-apropos)
(define-key sawfish-mode-map [(control c) (control h) ?f]          #'sawfish-describe-function)
(define-key sawfish-mode-map [(control c) (control h) (control f)] #'sawfish-info-function)
(define-key sawfish-mode-map [(control c) (control h) ?v]          #'sawfish-describe-variable)
(define-key sawfish-mode-map [(control c) (control h) (control v)] #'sawfish-info-variable)
(define-key sawfish-mode-map [(meta tab)]                          #'sawfish-complete-symbol)
(define-key sawfish-mode-map [(control c) (control h) ?i]          #'sawfish-info)
(define-key sawfish-mode-map [(control meta :)]                    #'eval-expression)

;; Define the minibuffer keymap.
(unless sawfish-read-expression-map
  (setq sawfish-read-expression-map (make-sparse-keymap))
  (set-keymap-parent sawfish-read-expression-map minibuffer-local-map)
  (define-key sawfish-read-expression-map [(meta tab)] #'sawfish-complete-symbol))

;; Define the sawfish-interaction keymap.
(unless sawfish-interaction-mode-map
  (setq sawfish-interaction-mode-map (make-sparse-keymap))
  (set-keymap-parent sawfish-interaction-mode-map sawfish-mode-map)
  (define-key sawfish-interaction-mode-map [(control j)] #'sawfish-eval-print-last-sexp))

;; Further define the sawfish-console-mode keymap. It is initialised already
;; because of define-derived-mode.
(define-key sawfish-console-mode-map [(tab)]                               #'sawfish-complete-symbol)
(define-key sawfish-console-mode-map [(control c) (control h) ?a]          #'sawfish-apropos)
(define-key sawfish-console-mode-map [(control c) (control h) ?f]          #'sawfish-describe-function)
(define-key sawfish-console-mode-map [(control c) (control h) (control f)] #'sawfish-info-function)
(define-key sawfish-console-mode-map [(control c) (control h) ?v]          #'sawfish-describe-variable)
(define-key sawfish-console-mode-map [(control c) (control h) (control v)] #'sawfish-info-variable)

;; Indentation hints for macros and functions provided by sawfish.el
(put 'sawfish-code 'lisp-indent-function 0)

;;; Menus

;; GNU Emacs/XEmacs difference crap.
(defun sawfish-region-active-p ()
  "Is there an active region?"
  (if (and (boundp 'running-xemacs) (symbol-value 'running-xemacs))
      (funcall (symbol-function 'region-exists-p))
    (symbol-value 'mark-active)))

(easy-menu-define sawfish-mode-menu sawfish-mode-map "sawfish commands"
  '("Sawfish"
    ["Indent Line"                     lisp-indent-line          t]
    ["Indent Region"                   indent-region             (sawfish-region-active-p)]
    ["Comment Out Region"              comment-region            (sawfish-region-active-p)]
    "----"
    ["Evaluate Last S-expression"      sawfish-eval-last-sexp    t]
    ["Evaluate Top Level Form"         sawfish-eval-defun        t]
    ["Evaluate Region"                 sawfish-eval-region       (sawfish-region-active-p)]
    ["Evaluate Buffer"                 sawfish-eval-buffer       t]
    ["Evaluate Expression"             sawfish-eval-expression   t]
    "----"
    ["Describe Sawfish Variable"       sawfish-describe-variable t]
    ["Describe Sawfish Function"       sawfish-describe-function t]
    ["Info on Variable"                sawfish-info-variable     t]
    ["Info on Function"                sawfish-info-function     t]
    ["Apropos"                         sawfish-apropos           t]
    "----"
    ["Open Sawfish Interaction Buffer" sawfish-interaction       t]
    ["Open Sawfish Console"            sawfish-console           t]
    "----"
    ["Read Sawfish Documentation"      sawfish-info              t]
    ["Read librep Documentation"       sawfish-rep-info          t]))

;; GNU emacs emacs-lisp menu removal kludge.

(defvar sawfish-gnu-emacs-menu-kludged nil
  "Check if we've kludged the menu in GNU emacs.")

(unless (and (boundp 'running-xemacs) (symbol-value 'running-xemacs))
  (unless sawfish-gnu-emacs-menu-kludged
    (let ((old-emacs-lisp-mode-map (copy-keymap emacs-lisp-mode-map)))
      ;; Remove the binding for the emacs-lisp menu.
      (define-key emacs-lisp-mode-map [menu-bar emacs-lisp] 'undefinded)
      ;; Initialise sawfish-mode.
      (with-temp-buffer (sawfish-mode))
      ;; Restore the emacs-lisp-mode keymap.
      (setq emacs-lisp-mode-map (copy-keymap old-emacs-lisp-mode-map)))
    (setq sawfish-gnu-emacs-menu-kludged t)))

;; Helpful hints for info lookups (provided by Markus Holmberg).

(eval-after-load "info-look"
  '(info-lookup-maybe-add-help
    :mode 'sawfish-mode
    :regexp "[^()`',\" \t\n]+"
    :doc-spec '(("(sawfish)Function Index" nil "^ - [^:]+: " "\\b")
                ("(sawfish)Variable Index" nil "^ - [^:]+: " "\\b")
                ("(librep)Function Index"  nil "^ - [^:]+: " "\\b")
                ("(librep)Variable Index"  nil "^ - [^:]+: " "\\b"))))

(provide 'sawfish-freebsd)

;;; sawfish.el ends here
