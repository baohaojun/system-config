;;; nsi-mode.el --- Major mode for editing and compiling Nsi programs (nsis scripts)

;; This is just a cannibalization of python mode because I wanted to have
;; something to edit and compile nsis scripts in emacs (Hans van Dam)
;; Compilation of the current buffer is performed using Ctrl-f9 (just like in the nsis-editor).

;; Copyright (C) 2005  Tim Peters

;; Author: 1995-1998 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: nsi-mode@nsi.org
;; Created:    Feb 1992
;; Keywords:   nsi languages oop

(defconst nsi-version "3.105"
  "`nsi-mode' version number.")

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;;; Commentary:

;; 
;; 
;; This is a major mode for editing Nsi programs.  It was developed
;; by Tim Peters after an original idea by Michael A. Guravage.  Tim
;; subsequently left the net; in 1995, Barry Warsaw inherited the mode
;; and is the current maintainer.  Tim's now back but disavows all
;; responsibility for the mode.  Smart Tim :-)

;; This version of nsi-mode.el is no longer compatible with Emacs
;; 18.  I am striving to maintain compatibility with the X/Emacs 19
;; lineage but as time goes on that becomes more and more difficult.
;; I current recommend that you upgrade to the latest stable released
;; version of your favorite branch: Emacs 20.3 or better, or XEmacs
;; 20.4 or better (XEmacs 21.0 is in beta testing as of this writing
;; 27-Oct-1998 appears to work fine with this version of
;; nsi-mode.el).  Even Windows users should be using at least
;; NTEmacs 20.3, and XEmacs 21.0 will work very nicely on Windows when
;; it is released.

;; FOR MORE INFORMATION:

;; For more information on installing nsi-mode.el, especially with
;; respect to compatibility information, please see
;;
;;     http://www.nsi.org/emacs/nsi-mode/
;;
;; This site also contains links to other packages that you might find 
;; useful, such as pdb interfaces, OO-Browser links, etc.

;; BUG REPORTING:

;; To submit bug reports, use C-c C-b.  Please include a complete, but
;; concise code sample and a recipe for reproducing the bug.  Send
;; suggestions and other comments to nsi-mode@nsi.org.

;; When in a Nsi mode buffer, do a C-h m for more help.  It's
;; doubtful that a texinfo manual would be very useful, but if you
;; want to contribute one, I'll certainly accept it!

;; TO DO LIST:

;; - Better integration with pdb.py and gud-mode for debugging.
;; - Rewrite according to GNU Emacs Lisp standards.
;; - have nsi-execute-region on indented code act as if the region is
;;   left justified.  Avoids syntax errors.
;; - add a nsi-goto-block-down, bound to C-c C-d

;;; Code:

(require 'comint)
(require 'custom)
(eval-when-compile
  (require 'cl)
  (if (not (and (condition-case nil
		    (require 'custom)
		  (error nil))
		;; Stock Emacs 19.34 has a broken/old Custom library
		;; that does more harm than good.  Fortunately, it is
		;; missing defcustom
		(fboundp 'defcustom)))
      (error "STOP! STOP! STOP! STOP!

The Custom library was not found or is out of date.  A more current
version is required.  Please download and install the latest version
of the Custom library from:

    <http://www.dina.kvl.dk/~abraham/custom/>

See the Nsi Mode home page for details:

    <http://www.nsi.org/emacs/nsi-mode>
")))



;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup nsi nil
  "Support for the Nsi programming language, <http://www.nsi.org/>"
  :group 'languages
  :prefix "nsi-")

(defcustom nsi-nsi-command "nsi"
  "*Shell command used to start nsi compiler."
  :type 'string
  :group 'nsi)

(defcustom nsi-default-interpreter 'cnsi
  "*Which Nsi interpreter is used by default.
The value for this variable can be either `cnsi' or `jnsi'.


ote that this variable is consulted only the first time that a Nsi
mode buffer is visited during an Emacs session.  After that, use
\\[nsi-toggle-shells] to change the interpreter shell."
  :type '(choice (const :tag "Nsi (a.k.a. CNsi)" cnsi)
		 (const :tag "JNsi" jnsi))
  :group 'nsi)


(defcustom nsi-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[nsi-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Nsi code."
  :type 'integer
  :group 'nsi)

(defcustom nsi-smart-indentation t
  "*Should `nsi-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `nsi-mode':

    1. `nsi-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `nsi-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `nsi-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Nsi mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `nsi-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `nsi-smart-indentation' to nil in your `nsi-mode-hook'."
  :type 'boolean
  :group 'nsi)

(defcustom nsi-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
		 (const :tag "Align to column zero" nil))
  :group 'nsi)

(defcustom nsi-block-comment-prefix ";;"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'nsi)

(defcustom nsi-honor-comment-indentation t
  "*Controls how comment lines influence subsequent indentation.

When nil, all comment lines are skipped for indentation purposes, and
if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

When t, lines that begin with a single `;' are a hint to subsequent
line indentation.  If the previous line is such a comment line (as
opposed to one that starts with `nsi-block-comment-prefix'), then its
indentation is used as a hint for this line's indentation.  Lines that
begin with `nsi-block-comment-prefix' are ignored for indentation
purposes.

When not nil or t, comment lines that begin with a `#' are used as
indentation hints, unless the comment character is in column zero."
  :type '(choice
	  (const :tag "Skip all comment lines (fast)" nil)
	  (const :tag "Single ; `sets' indentation for next line" t)
	  (const :tag "Single ; `sets' indentation except at column zero"
		 other)
	  )
  :group 'nsi)

(defcustom nsi-temp-directory
  (let ((ok '(lambda (x)
	       (and x
		    (setq x (expand-file-name x)) ; always true
		    (file-directory-p x)
		    (file-writable-p x)
		    x))))
    (or (funcall ok (getenv "TMPDIR"))
	(funcall ok "/usr/tmp")
	(funcall ok "/tmp")
	(funcall ok  ".")
	(error
	 "Couldn't find a usable temp directory -- set `nsi-temp-directory'")))
  "*Directory used for temp files created by a *Nsi* process.
By default, the first directory from this list that exists and that you
can write into:  the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, or the current directory."
  :type 'string
  :group 'nsi)

(defcustom nsi-beep-if-tab-change t
  "*Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `nsi-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'nsi)

(defcustom nsi-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'nsi)

(defcustom nsi-backspace-function 'backward-delete-char-untabify
  "*Function called by `nsi-electric-backspace' when deleting backwards."
  :type 'function
  :group 'nsi)

(defcustom nsi-delete-function 'delete-char
  "*Function called by `nsi-electric-delete' when deleting forwards."
  :type 'function
  :group 'nsi)

(defcustom nsi-imenu-show-method-args-p nil 
  "*Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'nsi)
(make-variable-buffer-local 'nsi-indent-offset)

;; Not customizable
(defvar nsi-master-file nil
  "If non-nil, execute the named file instead of the buffer's file.
The intent is to allow you to set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # nsi-master-file: \"master.py\"
    # End:

so that typing \\[nsi-execute-buffer] in that buffer executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.")
(make-variable-buffer-local 'nsi-master-file)



;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defconst nsi-emacs-features
  (let (features)
   ;; NTEmacs 19.34.6 has a broken make-temp-name; it always returns
   ;; the same string.
   (let ((tmp1 (make-temp-name ""))
	 (tmp2 (make-temp-name "")))
     (if (string-equal tmp1 tmp2)
	 (push 'broken-temp-names features)))
   ;; return the features
   features)
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, with different levels of
support for features needed by `nsi-mode'.")

(defvar nsi-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
			'("Section"      "Function"   "FunctionEnd" "SectionEnd" 
			  )
			"\\|"))
	)
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw1 "\\)\\b[ \n\t(]") 1)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     ;; classes
     '("\\bclass[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-type-face)
     ;; functions
     '("\\bdef[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ))
  "Additional expressions to highlight in Nsi mode.")
(put 'nsi-mode 'font-lock-defaults '(nsi-font-lock-keywords))

;; have to bind nsi-file-queue before installing the kill-emacs-hook
;; Constants

(defconst nsi-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"	; single-quoted
   "\\|"				; or
   "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
   )
  "Regular expression matching a Nsi string literal.")

(defconst nsi-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^;'\"\n\\]" "\\|" nsi-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching Nsi backslash continuation lines.")
  
(defconst nsi-blank-or-comment-re "[ \t]*\\($\\|;\\)"
  "Regular expression matching a blank or comment line.")

(defconst nsi-outdent-re
  (concat "\\(" (mapconcat 'identity
			   '("else:"
			     "except\\(\\s +.*\\)?:"
			     "finally:"
			     "elif\\s +.*:")
			   "\\|")
	  "\\)")
  "Regular expression matching statements to be dedented one level.")
  
(defconst nsi-block-closing-keywords-re
  "\\(return\\|raise\\|break\\|continue\\|pass\\)"
  "Regular expression matching keywords which typically close a block.")

(defconst nsi-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
	      (list "try:"
		    "except\\(\\s +.*\\)?:"
		    "while\\s +.*:"
		    "for\\s +.*:"
		    "if\\s +.*:"
		    "elif\\s +.*:"
		    (concat nsi-block-closing-keywords-re "[ \t\n]")
		    )
	      "\\|")
	  "\\)")
  "Regular expression matching lines not to dedent after.")

(defconst nsi-defun-start-re
  "^\\([ \t]*\\)def[ \t]+\\([a-zA-Z_0-9]+\\)\\|\\(^[a-zA-Z_0-9]+\\)[ \t]*="
  ;; If you change this, you probably have to change nsi-current-defun
  ;; as well.  This is only used by nsi-current-defun to find the name
  ;; for add-log.el.
  "Regular expression matching a function, method, or variable assignment.")

(defconst nsi-class-start-re "^class[ \t]*\\([a-zA-Z_0-9]+\\)"
  ;; If you change this, you probably have to change nsi-current-defun
  ;; as well.  This is only used by nsi-current-defun to find the name
  ;; for add-log.el.
  "Regular expression for finding a class name.")

(defconst nsi-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")



;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar nsi-mode-abbrev-table nil
  "Abbrev table in use in `nsi-mode' buffers.")
(define-abbrev-table 'nsi-mode-abbrev-table nil)

(defvar nsi-mode-hook nil
  "*Hook called by `nsi-mode'.")

;; In previous version of nsi-mode.el, the hook was incorrectly
;; called nsi-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'nsi-mode-hook 'nsi-mode-hook))

(defvar nsi-mode-map ()
  "Keymap used in `nsi-mode' buffers.")
(if nsi-mode-map
    nil
  (setq nsi-mode-map (make-sparse-keymap))
  ;; electric keys
  ;; indentation level modifiers
  ;; subprocess commands
  (define-key nsi-mode-map [C-f9] 'nsi-execute-buffer)
  ;; Caution!  Enter here at your own risk.  We are trying to support
  ;; several behaviors and it gets disgusting. :-( This logic ripped
  ;; largely from CC Mode.
  ;;
  ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
  ;; backwards deletion behavior to DEL, which both Delete and
  ;; Backspace get translated to.  There's no way to separate this
  ;; behavior in a clean way, so deal with it!  Besides, it's been
  ;; this way since the dawn of time.
  (if (not (boundp 'delete-key-deletes-forward))
      (define-key nsi-mode-map "\177" 'nsi-electric-backspace)
    ;; However, XEmacs 20 actually achieved enlightenment.  It is
    ;; possible to sanely define both backward and forward deletion
    ;; behavior under X separately (TTYs are forever beyond hope, but
    ;; who cares?  XEmacs 20 does the right thing with these too).
    (define-key nsi-mode-map [delete]    'nsi-electric-delete)
    (define-key nsi-mode-map [backspace] 'nsi-electric-backspace))
  ;; Separate M-BS from C-M-h.  The former should remain
  ;; backward-kill-word.
  ;; stuff that is `standard' but doesn't interface well with
  ;; nsi-mode, which forces us to rebind to special commands
  (define-key nsi-mode-map "\C-xnd"    'nsi-narrow-to-defun)
  ;; information
  (define-key nsi-mode-map "\C-c\C-b" 'nsi-submit-bug-report)
  (define-key nsi-mode-map "\C-c\C-v" 'nsi-version)
  ;; shadow global bindings for newline-and-indent w/ the nsi- version.
  ;; BAW - this is extremely bad form, but I'm not going to change it
  ;; for now.
  (mapcar #'(lambda (key)
	      (define-key nsi-mode-map key 'nsi-newline-and-indent))
	  (where-is-internal 'newline-and-indent))
  ;; Force RET to be nsi-newline-and-indent even if it didn't get
  ;; mapped by the above code.  motivation: Emacs' default binding for
  ;; RET is `newline' and C-j is `newline-and-indent'.  Most Nsieers
  ;; expect RET to do a `nsi-newline-and-indent' and any Emacsers who
  ;; dislike this are probably knowledgeable enough to do a rebind.
  ;; However, we do *not* change C-j since many Emacsers have already
  ;; swapped RET and C-j and they don't want C-j bound to `newline' to 
  ;; change.
  (define-key nsi-mode-map "\C-m" 'nsi-newline-and-indent)
  )

(defvar nsi-mode-output-map nil
  "Keymap used in *Nsi Output* buffers.")
(if nsi-mode-output-map
    nil
  (setq nsi-mode-output-map (make-sparse-keymap))
  (define-key nsi-mode-output-map [button2]  'nsi-mouseto-exception)
  (define-key nsi-mode-output-map "\C-c\C-c" 'nsi-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Nsi Output* buffer being read-only
  (mapcar #' (lambda (key)
	       (define-key nsi-mode-output-map key
		 #'(lambda () (interactive) (beep))))
	     (where-is-internal 'self-insert-command))
  )

(defvar nsi-mode-syntax-table nil
  "Syntax table used in `nsi-mode' buffers.")
(if nsi-mode-syntax-table
    nil
  (setq nsi-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" nsi-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" nsi-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" nsi-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" nsi-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" nsi-mode-syntax-table)
  (modify-syntax-entry ?\} "){" nsi-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\% "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\& "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\* "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\- "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\< "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\= "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\> "."  nsi-mode-syntax-table)
  (modify-syntax-entry ?\| "."  nsi-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to nsi-forward-into-nomenclature and
  ;; nsi-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  nsi-mode-syntax-table)
  ;; Both single quote and double quote are string delimiters
  (modify-syntax-entry ?\' "\"" nsi-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" nsi-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  nsi-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  nsi-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  nsi-mode-syntax-table)
  )



;; Utilities

(defmacro nsi-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defsubst nsi-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst nsi-highlight-line (from to file line)
  (cond
   ((fboundp 'make-extent)
    ;; XEmacs
    (let ((e (make-extent from to)))
      (set-extent-property e 'mouse-face 'highlight)
      (set-extent-property e 'nsi-exc-info (cons file line))
      (set-extent-property e 'keymap nsi-mode-output-map)))
   (t
    ;; Emacs -- Please port this!
    )
   ))

;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar nsi-menu nil
  "Menu for Nsi Mode.
This menu will get created automatically if you have the `easymenu'
package.  Note that the latest X/Emacs releases contain this package.")

(and (nsi-safe (require 'easymenu) t)
     (easy-menu-define
      nsi-menu nsi-mode-map "Nsi Mode menu"
      '("Nsi"
	["Execute buffer"       nsi-execute-buffer t]
	["Describe mode"        nsi-describe-mode t]
	)))



;; Imenu definitions
(defvar nsi-imenu-class-regexp
  (concat				; <<classes>>
   "\\("				;
   "^[ \t]*"				; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"	; class name
					; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"				; and the final :
   "\\)"				; >>classes<<
   )
  "Regexp for Nsi classes for use with the Imenu package."
  )

(defvar nsi-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ; 
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
					;   function arguments...
;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\([^:#]*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Nsi methods/functions for use with the Imenu package."
  )

(defvar nsi-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Nsi regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable `nsi-imenu-show-method-args-p' for more
information.")

(defvar nsi-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Nsi regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable `nsi-imenu-show-method-args-p' for more
information.")

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.
(defvar nsi-imenu-generic-expression
  (cons
   (concat 
    nsi-imenu-class-regexp
    "\\|"				; or...
    nsi-imenu-method-regexp
    )
   nsi-imenu-method-no-arg-parens)
  "Generic Nsi expression which may be used directly with Imenu.
Used by setting the variable `imenu-generic-expression' to this value.
Also, see the function \\[nsi-imenu-create-index] for a better
alternative for finding the index.")

;; These next two variables are used when searching for the Nsi
;; class/definitions. Just saving some time in accessing the
;; generic-nsi-expression, really.
(defvar nsi-imenu-generic-regexp nil)
(defvar nsi-imenu-generic-parens nil)


(defun nsi-imenu-create-index-function ()
  "Nsi interface function for the Imenu package.
Finds all Nsi classes and functions/methods. Calls function
\\[nsi-imenu-create-index-engine].  See that function for the details
of how this works."
  (setq nsi-imenu-generic-regexp (car nsi-imenu-generic-expression)
	nsi-imenu-generic-parens (if nsi-imenu-show-method-args-p
				    nsi-imenu-method-arg-parens
				  nsi-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (nsi-imenu-create-index-engine nil))

(defun nsi-imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Nsi.

Finds all definitions (classes, methods, or functions) in a Nsi
file for the Imenu package.

Returns a possibly nested alist of the form

	(INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

	(INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[nsi-imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Nsi classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
	sub-method-alist
	looking-p
	def-name prev-name
	cur-indent def-pos
	(class-paren (first  nsi-imenu-generic-parens)) 
	(def-paren   (second nsi-imenu-generic-parens)))
    (setq looking-p
	  (re-search-forward nsi-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
	;; used to set def-name to this value but generic-extract-name
	;; is new to imenu-1.14. this way it still works with
	;; imenu-1.11
	;;(imenu--generic-extract-name nsi-imenu-generic-parens))
	(let ((cur-paren (if (match-beginning class-paren)
			     class-paren def-paren)))
	  (setq def-name
		(buffer-substring-no-properties (match-beginning cur-paren)
						(match-end cur-paren))))
	(save-match-data
	  (nsi-beginning-of-def-or-class 'either))
	(beginning-of-line)
	(setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
	    (or (match-beginning class-paren)
		(match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
	  (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
	  (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
	(push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
	;; the point is currently on the expression we're supposed to
	;; start on, so go back to the last expression. The recursive
	;; call will find this place again and add it to the correct
	;; list
	(re-search-backward nsi-imenu-generic-regexp (point-min) 'move)
	(setq sub-method-alist (nsi-imenu-create-index-engine cur-indent))
	(if sub-method-alist
	    ;; we put the last element on the index-alist on the start
	    ;; of the submethod alist so the user can still get to it.
	    (let ((save-elmt (pop index-alist)))
	      (push (cons prev-name
			  (cons save-elmt sub-method-alist))
		    index-alist))))
       ;; found less indented expression, we're done.
       (t 
	(setq looking-p nil)
	(re-search-backward nsi-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
	   (setq looking-p
		 (re-search-forward nsi-imenu-generic-regexp
				    (point-max) 'move))))
    (nreverse index-alist)))


;;;###autoload
(defun nsi-mode ()
  "Major mode for editing Nsi files.
To submit a problem report, enter `\\[nsi-submit-bug-report]' from a
`nsi-mode' buffer.  Do `\\[nsi-describe-mode]' for detailed
documentation.  To see what version of `nsi-mode' you are running,
enter `\\[nsi-version]'.

This mode knows about Nsi indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{nsi-mode-map}
VARIABLES

nsi-indent-offset\t\tindentation increment
nsi-block-comment-prefix\t\tcomment string used by `comment-region'
nsi-nsi-command\t\tshell command to invoke Nsi interpreter
nsi-temp-directory\t\tdirectory used for temp files (if needed)
nsi-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  ;;
  (set-syntax-table nsi-mode-syntax-table)
  (setq major-mode              'nsi-mode
	mode-name               "Nsi"
	local-abbrev-table      nsi-mode-abbrev-table
	font-lock-defaults      '(nsi-font-lock-keywords)
	paragraph-separate      "^[ \t]*$"
	paragraph-start         "^[ \t]*$"
	require-final-newline   t
	comment-start           "; "
	comment-end             ""
	comment-start-skip      "; *"
	comment-column          40
	comment-indent-function 'nsi-comment-indent-function
	indent-region-function  'nsi-indent-region
	indent-line-function    'nsi-indent-line
	;; tell add-log.el how to find the current function/method/variable
	add-log-current-defun-function 'nsi-current-defun
	)
  (use-local-map nsi-mode-map)
  ;; add the menu
  (if nsi-menu
      (easy-menu-add nsi-menu))
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  ;; Install Imenu if available
  (when (nsi-safe (require 'imenu))
    (setq imenu-create-index-function #'nsi-imenu-create-index-function)
    (setq imenu-generic-expression nsi-imenu-generic-expression)
    (if (fboundp 'imenu-add-to-menubar)
	(imenu-add-to-menubar (format "%s-%s" "IM" mode-name)))
    )
  ;; Run the mode hook.  Note that nsi-mode-hook is deprecated.
  (if nsi-mode-hook
      (run-hooks 'nsi-mode-hook)
    (run-hooks 'nsi-mode-hook))
  ;; Now do the automagical guessing
  (if nsi-smart-indentation
    (let ((offset nsi-indent-offset))
      ;; It's okay if this fails to guess a good value
      (if (and (nsi-safe (nsi-guess-indent-offset))
	       (<= nsi-indent-offset 8)
	       (>= nsi-indent-offset 2))
	  (setq offset nsi-indent-offset))
      (setq nsi-indent-offset offset)
      ;; Only turn indent-tabs-mode off if tab-width !=
      ;; nsi-indent-offset.  Never turn it on, because the user must
      ;; have explicitly turned it off.
      (if (/= tab-width nsi-indent-offset)
	  (setq indent-tabs-mode nil))
      ))
  ;; Set the default shell if not already set
  (when (null nsi-which-shell)
    (nsi-toggle-shells nsi-default-interpreter))
  )


;; electric characters
(defun nsi-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
		(looking-at nsi-outdent-re))
	 ;; short circuit infloop on illegal construct
	 (not (bobp))
	 (progn (forward-line -1)
		(nsi-goto-initial-line)
		(back-to-indentation)
		(while (or (looking-at nsi-blank-or-comment-re)
			   (bobp))
		  (backward-to-indentation 1))
		(not (looking-at nsi-no-outdent-re)))
	 )))
      
(defun nsi-electric-colon (arg)
  "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  ;; are we in a string or comment?
  (if (save-excursion
	(let ((pps (parse-partial-sexp (save-excursion
					 (nsi-beginning-of-def-or-class)
					 (point))
				       (point))))
	  (not (or (nth 3 pps) (nth 4 pps)))))
      (save-excursion
	(let ((here (point))
	      (outdent 0)
	      (indent (nsi-compute-indentation t)))
	  (if (and (not arg)
		   (nsi-outdent-p)
		   (= indent (save-excursion
			       (nsi-next-statement -1)
			       (nsi-compute-indentation t)))
		   )
	      (setq outdent nsi-indent-offset))
	  ;; Don't indent, only dedent.  This assumes that any lines
	  ;; that are already dedented relative to
	  ;; nsi-compute-indentation were put there on purpose.  It's
	  ;; highly annoying to have `:' indent for you.  Use TAB, C-c
	  ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
	  ;; determine this???
	  (if (< (current-indentation) indent) nil
	    (goto-char here)
	    (beginning-of-line)
	    (delete-horizontal-space)
	    (indent-to (- indent outdent))
	    )))))


;; Nsi subprocess utilities and filters
(defun nsi-execute-file (proc filename)
  "Send to Nsi interpreter process PROC \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
	(procbuf (process-buffer proc))
;	(comint-scroll-to-bottom-on-output t)
	(msg (format "## working on region in file %s...\n" filename))
	(cmd (format "execfile(r'%s')\n" filename)))
    (unwind-protect
	(save-excursion
	  (set-buffer procbuf)
	  (goto-char (point-max))
	  (move-marker (process-mark proc) (point))
	  (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

;; (defun nsi-comint-output-filter-function (string)
;;   "Watch output for Nsi prompt and exec next file waiting in queue.
;; This function is appropriate for `comint-output-filter-functions'."
;;   ;; TBD: this should probably use split-string
;;   (when (and (or (string-equal string ">>> ")
;; 		 (and (>= (length string) 5)
;; 		      (string-equal (substring string -5) "\n>>> ")))
;; 	     nsi-file-queue)
;;     (nsi-safe (delete-file (car nsi-file-queue)))
;;     (setq nsi-file-queue (cdr nsi-file-queue))
;;     (if nsi-file-queue
;; 	(let ((pyproc (get-buffer-process (current-buffer))))
;; 	  (nsi-execute-file pyproc (car nsi-file-queue))))
;;     ))

(defun nsi-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (beginning-of-buffer)
      (while (re-search-forward nsi-traceback-line-re nil t)
	(setq file (match-string 1)
	      line (string-to-int (match-string 2))
	      bol (nsi-point 'bol))
	(nsi-highlight-line bol (nsi-point 'eol) file line)))
    (when (and nsi-jump-on-exception line)
      (beep)
      (nsi-jump-to-exception file line)
      (setq err-p t))
    err-p))



;;; Subprocess commands

;; only used when (memq 'broken-temp-names nsi-emacs-features)
(defvar nsi-serial-number 0)
(defvar nsi-exception-buffer nil)
(defconst nsi-output-buffer "*Nsi Output*")
(make-variable-buffer-local 'nsi-output-buffer)

;; for toggling between CNsi and JNsi
(defvar nsi-which-shell nil)
(defvar nsi-which-bufname "Nsi")
(make-variable-buffer-local 'nsi-which-shell)
(make-variable-buffer-local 'nsi-which-args)
(make-variable-buffer-local 'nsi-which-bufname)

(defun nsi-toggle-shells (arg)
  "Toggles between the CNsi and JNsi shells.

With positive argument ARG (interactively \\[universal-argument]),
uses the CNsi shell, with negative ARG uses the JNsi shell, and
with a zero argument, toggles the shell.

Programmatically, ARG can also be one of the symbols `cnsi' or
`jnsi', equivalent to positive arg and negative arg respectively."
  (interactive "P")
  ;; default is to toggle
  (if (null arg)
      (setq arg 0))
  ;; preprocess arg
  (cond
   ((equal arg 0)
    ;; toggle
    (if (string-equal nsi-which-bufname "Nsi")
	(setq arg -1)
      (setq arg 1)))
   ((equal arg 'cnsi) (setq arg 1))
   ((equal arg 'jnsi) (setq arg -1)))
  (let (msg)
    (cond
     ((< 0 arg)
      ;; set to CNsi
      (setq nsi-which-shell nsi-nsi-command
	    nsi-which-bufname "Nsi"
	    msg "CNsi"
	    mode-name "Nsi"))
     ((> 0 arg)
      (setq nsi-which-shell nsi-jnsi-command
	    nsi-which-bufname "JNsi"
	    msg "JNsi"
	    mode-name "JNsi"))
     )
    (message "Using the %s shell" msg)
    (setq nsi-output-buffer (format "*%s Output*" nsi-which-bufname))))

;;;###autoload
(defun nsi-shell (&optional argprompt)
  "Start an interactive Nsi interpreter in another window.
This is like Shell mode, except that Nsi is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Nsi*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Nsi interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CNsi interpreter and the
JNsi interpreter by hitting \\[nsi-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*JNsi*' or `*Nsi*' buffers (the
latter is the name used for the CNsi buffer).

Warning: Don't use an interactive Nsi if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `nsi-mode' can't
distinguish your output from Nsi's output, and assumes that `>>> '
at the start of a line is a prompt from Nsi.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Nsi prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Nsi, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Nsi process buffers using the default (Emacs-supplied) process
filter."
  (interactive "P")
  ;; Set the default shell if not already set
  (when (null nsi-which-shell)
    (nsi-toggle-shells nsi-default-interpreter))
  (let ((args nsi-which-args))
    (when (and argprompt
	       (interactive-p)
	       (fboundp 'split-string))
      ;; TBD: Perhaps force "-i" in the final list?
      (setq args (split-string
		  (read-string (concat nsi-which-bufname
				       " arguments: ")
			       (concat
				(mapconcat 'identity nsi-which-args " ") " ")
			       ))))
    (switch-to-buffer-other-window
     (apply 'make-comint nsi-which-bufname nsi-which-shell nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp "^>>> \\|^[.][.][.] \\|^(pdb) ")
    (add-hook 'comint-output-filter-functions
	      'nsi-comint-output-filter-function)
    (set-syntax-table nsi-mode-syntax-table)
    (use-local-map nsi-shell-map)
    ))

;; (defun nsi-clear-queue ()
;;   "Clear the queue of temporary files waiting to execute."
;;   (interactive)
;;   (let ((n (length nsi-file-queue)))
;;     (mapcar 'delete-file nsi-file-queue)
;;     (setq nsi-file-queue nil)
;;     (message "%d pending files de-queued." n)))


(defun nsi-execute-region (start end &optional async)
  "Execute the region in a Nsi interpreter.

The region is first copied into a temporary file (in the directory
`nsi-temp-directory').  If there is no Nsi interpreter shell
running, this file is executed synchronously using
`shell-command-on-region'.  If the program is long running, use
\\[universal-argument] to run the command asynchronously in its own
buffer.

When this function is used programmatically, arguments START and END
specify the region to execute, and optional third argument ASYNC, if
non-nil, specifies to run the command asynchronously in its own
buffer.

If the Nsi interpreter shell is running, the region is execfile()'d
in that shell.  If you try to execute regions too quickly,
`nsi-mode' will queue them up and execute them one at a time when
it sees a `>>> ' prompt from Nsi.  Each time this happens, the
process buffer is popped into a window (if it's not already in some
window) so you can see it, and a comment of the form

    \t## working on region in file <name>...

is inserted at the end.  See also the command `nsi-clear-queue'."
  (interactive "r\nP")
  (or (< start end)
      (error "Region is empty"))
	(shell-command-on-region start end cmd nsi-output-buffer)
)

;; Code execution commands
(defun nsi-execute-buffer (&optional async)
  "Send the contents of the buffer to a Nsi interpreter.
If the file local variable `nsi-master-file' is non-nil, execute the
named file instead of the buffer's file.

If there is a *Nsi* process buffer it is used.  If a clipping
restriction is in effect, only the accessible portion of the buffer is
sent.  A trailing newline will be supplied if needed.

See the `\\[nsi-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (let ((nsi-file-name (buffer-file-name)))
  (save-buffer)
  (shell-command (concat "C:/Progra~1/nsis/makensisw.exe " nsi-file-name))))
  
(defun nsi-execute-import-or-reload (&optional async)
  "Import the current buffer's file in a Nsi interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `nsi-execute-buffer'
instead.

If the file local variable `nsi-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `nsi-execute-import-or-reload-save-p'.

See the `\\[nsi-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `\\[nsi-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Nsi debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable nsi-master-file
  (if nsi-master-file
      (let* ((filename (expand-file-name nsi-master-file))
             (buffer (or (get-file-buffer filename)
                         (find-file-noselect filename))))
        (set-buffer buffer)))
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
	  ;; Maybe save some buffers
	  (save-some-buffers (not nsi-ask-about-save) nil)
          (nsi-execute-string
           (if (string-match "\\.py$" file)
               (let ((f (file-name-sans-extension
			 (file-name-nondirectory file))))
                 (format "if globals().has_key('%s'):\n    reload(%s)\nelse:\n    import %s\n"
                         f f f))
             (format "execfile(r'%s')\n" file))
           async))
      ;; else
      (nsi-execute-buffer async))))


(defun nsi-execute-def-or-class (&optional async)
  "Send the current function or class definition to a Nsi interpreter.

If there is a *Nsi* process buffer it is used.

See the `\\[nsi-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (save-excursion
    (nsi-mark-def-or-class)
    ;; mark is before point
    (nsi-execute-region (mark) (point) async)))


(defun nsi-execute-string (string &optional async)
  "Send the argument STRING to a Nsi interpreter.

If there is a *Nsi* process buffer it is used.

See the `\\[nsi-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "sExecute Nsi command: ")
  (save-excursion
    (set-buffer (get-buffer-create
                 (generate-new-buffer-name " *Nsi Command*")))
    (insert string)
    (nsi-execute-region (point-min) (point-max) async)))



(defun nsi-jump-to-exception (file line)
  "Jump to the Nsi code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
		       (if (consp nsi-exception-buffer)
			   (cdr nsi-exception-buffer)
			 nsi-exception-buffer))
		      ((and (consp nsi-exception-buffer)
			    (string-equal file (car nsi-exception-buffer)))
		       (cdr nsi-exception-buffer))
		      ((nsi-safe (find-file-noselect file)))
		      ;; could not figure out what file the exception
		      ;; is pointing to, so prompt for it
		      (t (find-file (read-file-name "Exception file: "
						    nil
						    file t))))))
    (pop-to-buffer buffer)
    ;; Force Nsi mode
    (if (not (eq major-mode 'nsi-mode))
	(nsi-mode))
    (goto-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(defun nsi-mouseto-exception (event)
  "Jump to the code which caused the Nsi exception at EVENT.
EVENT is usually a mouse click."
  (interactive "e")
  (cond
   ((fboundp 'event-point)
    ;; XEmacs
    (let* ((point (event-point event))
	   (buffer (event-buffer event))
	   (e (and point buffer (extent-at point buffer 'nsi-exc-info)))
	   (info (and e (extent-property e 'nsi-exc-info))))
      (message "Event point: %d, info: %s" point info)
      (and info
	   (nsi-jump-to-exception (car info) (cdr info)))
      ))
   ;; Emacs -- Please port this!
   ))

(defun nsi-goto-exception ()
  "Go to the line indicated by the traceback."
  (interactive)
  (let (file line)
    (save-excursion
      (beginning-of-line)
      (if (looking-at nsi-traceback-line-re)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (if (not file)
	(error "Not on a traceback line"))
    (nsi-jump-to-exception file line)))

(defun nsi-find-next-exception (start buffer searchdir errwhere)
  "Find the next Nsi exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (nsi-point start))
      (if (funcall searchdir nsi-traceback-line-re nil t)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (if (and file line)
	(nsi-jump-to-exception file line)
      (error "%s of traceback" errwhere))))

(defun nsi-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Nsi"))
	 (buffer (if proc "*Nsi*" nsi-output-buffer)))
    (if bottom
	(nsi-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (nsi-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun nsi-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Nsi"))
	 (buffer (if proc "*Nsi*" nsi-output-buffer)))
    (if top
	(nsi-find-next-exception 'bob buffer 're-search-forward "Top")
      (nsi-find-next-exception 'bol buffer 're-search-backward "Top"))))


;; Electric deletion
(defun nsi-electric-backspace (arg)
  "Delete preceding character or levels of indentation.
Deletion is performed by calling the function in `nsi-backspace-function'
with a single argument (the number of characters to delete).

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the leftmost non-whitespace character of a
line that is neither a continuation line nor a non-indenting comment
line, or if point is at the end of a blank line, this command reduces
the indentation to match that of the line that opened the current
block of code.  The line that opened the block is displayed in the
echo area to help you keep track of where you are.  With
\\[universal-argument] dedents that many blocks (but not past column
zero).

Otherwise the preceding character is deleted, converting a tab to
spaces if needed so that only a single column position is deleted.
\\[universal-argument] specifies how many characters to delete;
default is 1.

When used programmatically, argument ARG specifies the number of
blocks to dedent, or the number of characters to delete, as indicated
above."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (nsi-continuation-line-p)
;	  (not nsi-honor-comment-indentation)
;	  (looking-at "#[^ \t\n]")	; non-indenting #
	  )
      (funcall nsi-backspace-function arg)
    ;; else indent the same as the colon line that opened the block
    ;; force non-blank so nsi-goto-block-up doesn't ignore it
    (insert-char ?* 1)
    (backward-char)
    (let ((base-indent 0)		; indentation of base line
	  (base-text "")		; and text of base line
	  (base-found-p nil))
      (save-excursion
	(while (< 0 arg)
	  (condition-case nil		; in case no enclosing block
	      (progn
		(nsi-goto-block-up 'no-mark)
		(setq base-indent (current-indentation)
		      base-text   (nsi-suck-up-leading-text)
		      base-found-p t))
	    (error nil))
	  (setq arg (1- arg))))
      (delete-char 1)			; toss the dummy character
      (delete-horizontal-space)
      (indent-to base-indent)
      (if base-found-p
	  (message "Closes block: %s" base-text)))))


(defun nsi-electric-delete (arg)
  "Delete preceding or following character or levels of whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen and non-XEmacs versions), then this
function behaves identically to \\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction, by calling the
function in `nsi-delete-function'.

\\[universal-argument] (programmatically, argument ARG) specifies the
number of characters to delete (default is 1)."
  (interactive "*p")
  (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
	       (delete-forward-p))
	  (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
	       delete-key-deletes-forward))
      (funcall nsi-delete-function arg)
    (nsi-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'nsi-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'nsi-electric-backspace 'pending-delete   'supersede) ;pending-del
(put 'nsi-electric-delete    'delete-selection 'supersede) ;delsel
(put 'nsi-electric-delete    'pending-delete   'supersede) ;pending-del



(defun nsi-indent-line (&optional arg)
  "Fix the indentation of the current line according to Nsi rules.
With \\[universal-argument] (programmatically, the optional argument
ARG non-nil), ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive "P")
  (let* ((ci (current-indentation))
	 (move-to-indentation-p (<= (current-column) ci))
	 (need (nsi-compute-indentation (not arg))))
    ;; see if we need to dedent
    (if (nsi-outdent-p)
	(setq need (- need nsi-indent-offset)))
    (if (/= ci need)
	(save-excursion
	  (beginning-of-line)
	  (delete-horizontal-space)
	  (indent-to need)))
    (if move-to-indentation-p (back-to-indentation))))

(defun nsi-newline-and-indent ()
  "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Nsi code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented."
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))		; if point beyond indentation
	(newline-and-indent)
      ;; else try to act like newline-and-indent "normally" acts
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))

(defun nsi-compute-indentation (honor-block-close-p)
  "Compute Nsi indentation.
When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of
dedenting."
  (save-excursion
    (beginning-of-line)
    (let* ((bod (nsi-point 'bod))
	   (pps (parse-partial-sexp bod (point)))
	   (boipps (parse-partial-sexp bod (nsi-point 'boi)))
	   placeholder)
      (cond
       ;; are we inside a multi-line string or comment?
       ((or (and (nth 3 pps) (nth 3 boipps))
	    (and (nth 4 pps) (nth 4 boipps)))
	(save-excursion
	  (if (not nsi-align-multiline-strings-p) 0
	    ;; skip back over blank & non-indenting comment lines
	    ;; note: will skip a blank or non-indenting comment line
	    ;; that happens to be a continuation line too
	    (re-search-backward "^[ \t]*\\([^ \t\n;]\\|;[ \t\n]\\)" nil 'move)
	    (back-to-indentation)
	    (current-column))))
       ;; are we on a continuation line?
       ((nsi-continuation-line-p)
	(let ((startpos (point))
	      (open-bracket-pos (nsi-nesting-level))
	      endpos searching found state)
	  (if open-bracket-pos
	      (progn
		;; align with first item in list; else a normal
		;; indent beyond the line with the open bracket
		(goto-char (1+ open-bracket-pos)) ; just beyond bracket
		;; is the first list item on the same line?
		(skip-chars-forward " \t")
		(if (null (memq (following-char) '(?\n ?# ?\\)))
					; yes, so line up with it
		    (current-column)
		  ;; first list item on another line, or doesn't exist yet
		  (forward-line 1)
		  (while (and (< (point) startpos)
			      (looking-at "[ \t]*[#\n\\\\]")) ; skip noise
		    (forward-line 1))
		  (if (and (< (point) startpos)
			   (/= startpos
			       (save-excursion
				 (goto-char (1+ open-bracket-pos))
				 (forward-comment (point-max))
				 (point))))
		      ;; again mimic the first list item
		      (current-indentation)
		    ;; else they're about to enter the first item
		    (goto-char open-bracket-pos)
		    (setq placeholder (point))
		    (nsi-goto-initial-line)
		    (nsi-goto-beginning-of-tqs
		     (save-excursion (nth 3 (parse-partial-sexp
					     placeholder (point)))))
		    (+ (current-indentation) nsi-indent-offset))))

	    ;; else on backslash continuation line
	    (forward-line -1)
	    (if (nsi-continuation-line-p) ; on at least 3rd line in block
		(current-indentation)	; so just continue the pattern
	      ;; else started on 2nd line in block, so indent more.
	      ;; if base line is an assignment with a start on a RHS,
	      ;; indent to 2 beyond the leftmost "="; else skip first
	      ;; chunk of non-whitespace characters on base line, + 1 more
	      ;; column
	      (end-of-line)
	      (setq endpos (point)  searching t)
	      (back-to-indentation)
	      (setq startpos (point))
	      ;; look at all "=" from left to right, stopping at first
	      ;; one not nested in a list or string
	      (while searching
		(skip-chars-forward "^=" endpos)
		(if (= (point) endpos)
		    (setq searching nil)
		  (forward-char 1)
		  (setq state (parse-partial-sexp startpos (point)))
		  (if (and (zerop (car state)) ; not in a bracket
			   (null (nth 3 state))) ; & not in a string
		      (progn
			(setq searching nil) ; done searching in any case
			(setq found
			      (not (or
				    (eq (following-char) ?=)
				    (memq (char-after (- (point) 2))
					  '(?< ?> ?!)))))))))
	      (if (or (not found)	; not an assignment
		      (looking-at "[ \t]*\\\\")) ; <=><spaces><backslash>
		  (progn
		    (goto-char startpos)
		    (skip-chars-forward "^ \t\n")))
	      (1+ (current-column))))))

       ;; not on a continuation line
       ((bobp) (current-indentation))

       ;; Dfn: "Indenting comment line".  A line containing only a
       ;; comment, but which is treated like a statement for
       ;; indentation calculation purposes.  Such lines are only
       ;; treated specially by the mode; they are not treated
       ;; specially by the Nsi interpreter.

       ;; The rules for indenting comment lines are a line where:
       ;;   - the first non-whitespace character is `#', and
       ;;   - the character following the `#' is whitespace, and
       ;;   - the line is dedented with respect to (i.e. to the left
       ;;     of) the indentation of the preceding non-blank line.

       ;; The first non-blank line following an indenting comment
       ;; line is given the same amount of indentation as the
       ;; indenting comment line.

       ;; All other comment-only lines are ignored for indentation
       ;; purposes.

       ;; Are we looking at a comment-only line which is *not* an
       ;; indenting comment line?  If so, we assume that it's been
       ;; placed at the desired indentation, so leave it alone.
       ;; Indenting comment lines are aligned as statements down
       ;; below.
       ((and (looking-at "[ \t]*#[^ \t\n]")
	     ;; NOTE: this test will not be performed in older Emacsen
	     (fboundp 'forward-comment)
	     (<= (current-indentation)
		 (save-excursion
		   (forward-comment (- (point-max)))
		   (current-indentation))))
	(current-indentation))

       ;; else indentation based on that of the statement that
       ;; precedes us; use the first line of that statement to
       ;; establish the base, in case the user forced a non-std
       ;; indentation for the continuation lines (if any)
       (t
	;; skip back over blank & non-indenting comment lines note:
	;; will skip a blank or non-indenting comment line that
	;; happens to be a continuation line too.  use fast Emacs 19
	;; function if it's there.
	(if (and (eq nsi-honor-comment-indentation nil)
		 (fboundp 'forward-comment))
	    (forward-comment (- (point-max)))
	  (let ((prefix-re (concat nsi-block-comment-prefix "[ \t]*"))
		done)
	    (while (not done)
	      (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#\\)" nil 'move)
	      (setq done (or (bobp)
			     (and (eq nsi-honor-comment-indentation t)
				  (save-excursion
				    (back-to-indentation)
				    (not (looking-at prefix-re))
				    ))
			     (and (not (eq nsi-honor-comment-indentation t))
				  (save-excursion
				    (back-to-indentation)
				    (not (zerop (current-column)))))
			     ))
	      )))
	;; if we landed inside a string, go to the beginning of that
	;; string. this handles triple quoted, multi-line spanning
	;; strings.
	(nsi-goto-beginning-of-tqs (nth 3 (parse-partial-sexp bod (point))))
	;; now skip backward over continued lines
	(setq placeholder (point))
	(nsi-goto-initial-line)
	;; we may *now* have landed in a TQS, so find the beginning of
	;; this string.
	(nsi-goto-beginning-of-tqs
	 (save-excursion (nth 3 (parse-partial-sexp
				 placeholder (point)))))
	(+ (current-indentation)
	   (if (nsi-statement-opens-block-p)
	       nsi-indent-offset
	     (if (and honor-block-close-p (nsi-statement-closes-block-p))
		 (- nsi-indent-offset)
	       0)))
	)))))

(defun nsi-guess-indent-offset (&optional global)
  "Guess a good value for, and change, `nsi-indent-offset'.

By default, make a buffer-local copy of `nsi-indent-offset' with the
new value, so that other Nsi buffers are not affected.  With
\\[universal-argument] (programmatically, optional argument GLOBAL),
change the global value of `nsi-indent-offset'.  This affects all
Nsi buffers (that don't have their own buffer-local copy), both
those currently existing and those created later in the Emacs session.

Some people use a different value for `nsi-indent-offset' than you use.
There's no excuse for such foolishness, but sometimes you have to deal
with their ugly code anyway.  This function examines the file and sets
`nsi-indent-offset' to what it thinks it was when they created the
mess.

Specifically, it searches forward from the statement containing point,
looking for a line that opens a block of code.  `nsi-indent-offset' is
set to the difference in indentation between that line and the Nsi
statement following it.  If the search doesn't succeed going forward,
it's tried again going backward."
  (interactive "P")			; raw prefix arg
  (let (new-value
	(start (point))
	(restart (point))
	(found nil)
	colon-indent)
    (nsi-goto-initial-line)
    (while (not (or found (eobp)))
      (when (and (re-search-forward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
		 (not (nsi-in-literal restart)))
	(setq restart (point))
	(nsi-goto-initial-line)
	(if (nsi-statement-opens-block-p)
	    (setq found t)
	  (goto-char restart))))
    (unless found
      (goto-char start)
      (nsi-goto-initial-line)
      (while (not (or found (bobp)))
	(setq found (and
		     (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
		     (or (nsi-goto-initial-line) t) ; always true -- side effect
		     (nsi-statement-opens-block-p)))))
    (setq colon-indent (current-indentation)
	  found (and found (zerop (nsi-next-statement 1)))
	  new-value (- (current-indentation) colon-indent))
    (goto-char start)
    (if (not found)
	(error "Sorry, couldn't guess a value for nsi-indent-offset")
      (funcall (if global 'kill-local-variable 'make-local-variable)
	       'nsi-indent-offset)
      (setq nsi-indent-offset new-value)
      (or noninteractive
	  (message "%s value of nsi-indent-offset set to %d"
		   (if global "Global" "Local")
		   nsi-indent-offset)))
    ))

(defun nsi-comment-indent-function ()
  "Nsi version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (nsi-point 'eol)))
      (and comment-start-skip
	   (re-search-forward comment-start-skip eol t)
	   (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1)))
      )))

(defun nsi-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `nsi-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (nsi-end-of-def-or-class class)
    (let ((end (point)))
      (nsi-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))


(defun nsi-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun nsi-shift-region-left (start end &optional count)
  "Shift region of Nsi code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `nsi-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
	 (m (mark))
	 (arg current-prefix-arg))
     (if m
	 (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
	       (not (looking-at "\\s *$")))
	  (error "Region is at left edge"))
      (forward-line 1)))
  (nsi-shift-region start end (- (prefix-numeric-value
				 (or count nsi-indent-offset))))
  (nsi-keep-region-active))

(defun nsi-shift-region-right (start end &optional count)
  "Shift region of Nsi code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `nsi-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
	 (m (mark))
	 (arg current-prefix-arg))
     (if m
	 (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (nsi-shift-region start end (prefix-numeric-value
			      (or count nsi-indent-offset)))
  (nsi-keep-region-active))

(defun nsi-indent-region (start end &optional indent-offset)
  "Reindent a region of Nsi code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `nsi-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Nsi), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")			; region; raw prefix arg
  (save-excursion
    (goto-char end)   (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((nsi-indent-offset (prefix-numeric-value
			     (or indent-offset nsi-indent-offset)))
	  (indents '(-1))		; stack of active indent levels
	  (target-column 0)		; column to which to indent
	  (base-shifted-by 0)		; amount last base line was shifted
	  (indent-base (if (looking-at "[ \t\n]")
			   (nsi-compute-indentation t)
			 0))
	  ci)
      (while (< (point) end)
	(setq ci (current-indentation))
	;; figure out appropriate target column
	(cond
	 ((or (eq (following-char) ?#)	; comment in column 1
	      (looking-at "[ \t]*$"))	; entirely blank
	  (setq target-column 0))
	 ((nsi-continuation-line-p)	; shift relative to base line
	  (setq target-column (+ ci base-shifted-by)))
	 (t				; new base line
	  (if (> ci (car indents))	; going deeper; push it
	      (setq indents (cons ci indents))
	    ;; else we should have seen this indent before
	    (setq indents (memq ci indents)) ; pop deeper indents
	    (if (null indents)
		(error "Bad indentation in region, at line %d"
		       (save-restriction
			 (widen)
			 (1+ (count-lines 1 (point)))))))
	  (setq target-column (+ indent-base
				 (* nsi-indent-offset
				    (- (length indents) 2))))
	  (setq base-shifted-by (- target-column ci))))
	;; shift as needed
	(if (/= ci target-column)
	    (progn
	      (delete-horizontal-space)
	      (indent-to target-column)))
	(forward-line 1))))
  (set-marker end nil))

(defun nsi-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start nsi-block-comment-prefix))
    (comment-region beg end arg)))


;; Functions for moving point
(defun nsi-previous-statement (count)
  "Go to the start of the COUNTth preceding Nsi statement.
By default, goes to the previous statement.  If there is no such
statement, goes to the first statement.  Return count of statements
left to move.  `Statements' do not include blank, comment, or
continuation lines."
  (interactive "p")			; numeric prefix arg
  (if (< count 0) (nsi-next-statement (- count))
    (nsi-goto-initial-line)
    (let (start)
      (while (and
	      (setq start (point))	; always true -- side effect
	      (> count 0)
	      (zerop (forward-line -1))
	      (nsi-goto-statement-at-or-above))
	(setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun nsi-next-statement (count)
  "Go to the start of next Nsi statement.
If the statement at point is the i'th Nsi statement, goes to the
start of statement i+COUNT.  If there is no such statement, goes to the
last statement.  Returns count of statements left to move.  `Statements'
do not include blank, comment, or continuation lines."
  (interactive "p")			; numeric prefix arg
  (if (< count 0) (nsi-previous-statement (- count))
    (beginning-of-line)
    (let (start)
      (while (and
	      (setq start (point))	; always true -- side effect
	      (> count 0)
	      (nsi-goto-statement-below))
	(setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun nsi-goto-block-up (&optional nomark)
  "Move up to start of current block.
Go to the statement that starts the smallest enclosing block; roughly
speaking, this will be the closest preceding statement that ends with a
colon and is indented less than the statement you started on.  If
successful, also sets the mark to the starting point.

`\\[nsi-mark-block]' can be used afterward to mark the whole code
block, if desired.

If called from a program, the mark will not be set if optional argument
NOMARK is not nil."
  (interactive)
  (let ((start (point))
	(found nil)
	initial-indent)
    (nsi-goto-initial-line)
    ;; if on blank or non-indenting comment line, use the preceding stmt
    (if (looking-at "[ \t]*\\($\\|;[^ \t\n]\\)")
	(progn
	  (nsi-goto-statement-at-or-above)
	  (setq found (nsi-statement-opens-block-p))))
    ;; search back for colon line indented less
    (setq initial-indent (current-indentation))
    (if (zerop initial-indent)
	;; force fast exit
	(goto-char (point-min)))
    (while (not (or found (bobp)))
      (setq found
	    (and
	     (re-search-backward ":[ \t]*\\($\\|[;\\]\\)" nil 'move)
	     (or (nsi-goto-initial-line) t) ; always true -- side effect
	     (< (current-indentation) initial-indent)
	     (nsi-statement-opens-block-p))))
    (if found
	(progn
	  (or nomark (push-mark start))
	  (back-to-indentation))
      (goto-char start)
      (error "Enclosing block not found"))))

(defun nsi-beginning-of-def-or-class (&optional class count)
  "Move point to start of `def' or `class'.

Searches back for the closest preceding `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth start of `def'.

If point is in a `def' statement already, and after the `d', simply
moves point to the start of the statement.

Otherwise (i.e. when point is not in a `def' statement, or at or
before the `d' of a `def' statement), searches for the closest
preceding `def' statement, and leaves point at its start.  If no such
statement can be found, leaves point at the start of the buffer.

Returns t iff a `def' statement is found by these rules.

Note that doing this command repeatedly will take you closer to the
start of the buffer each time.

To mark the current `def', see `\\[nsi-mark-def-or-class]'."
  (interactive "P")			; raw prefix arg
  (setq count (or count 1))
  (let ((at-or-before-p (<= (current-column) (current-indentation)))
	(start-of-line (goto-char (nsi-point 'bol)))
	(start-of-stmt (goto-char (nsi-point 'bos)))
	(start-re (cond ((eq class 'either) "^[ \t]*\\(class\\|def\\)\\>")
			(class "^[ \t]*class\\>")
			(t "^[ \t]*def\\>")))
	)
    ;; searching backward
    (if (and (< 0 count)
	     (or (/= start-of-stmt start-of-line)
		 (not at-or-before-p)))
	(end-of-line))
    ;; search forward
    (if (and (> 0 count)
	     (zerop (current-column))
	     (looking-at start-re))
	(end-of-line))
    (if (re-search-backward start-re nil 'move count)
	(goto-char (match-beginning 0)))))

;; Backwards compatibility
(defalias 'beginning-of-nsi-def-or-class 'nsi-beginning-of-def-or-class)

(defun nsi-end-of-def-or-class (&optional class count)
  "Move point beyond end of `def' or `class' body.

By default, looks for an appropriate `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth end of `def'.

If point is in a `def' statement already, this is the `def' we use.

Else, if the `def' found by `\\[nsi-beginning-of-def-or-class]'
contains the statement you started on, that's the `def' we use.

Otherwise, we search forward for the closest following `def', and use that.

If a `def' can be found by these rules, point is moved to the start of
the line immediately following the `def' block, and the position of the
start of the `def' is returned.

Else point is moved to the end of the buffer, and nil is returned.

Note that doing this command repeatedly will take you closer to the
end of the buffer each time.

To mark the current `def', see `\\[nsi-mark-def-or-class]'."
  (interactive "P")			; raw prefix arg
  (if (and count (/= count 1))
      (nsi-beginning-of-def-or-class (- 1 count)))
  (let ((start (progn (nsi-goto-initial-line) (point)))
	(which (cond ((eq class 'either) "\\(class\\|def\\)")
		     (class "class")
		     (t "def")))
	(state 'not-found))
    ;; move point to start of appropriate def/class
    (if (looking-at (concat "[ \t]*" which "\\>")) ; already on one
	(setq state 'at-beginning)
      ;; else see if nsi-beginning-of-def-or-class hits container
      (if (and (nsi-beginning-of-def-or-class class)
	       (progn (nsi-goto-beyond-block)
		      (> (point) start)))
	  (setq state 'at-end)
	;; else search forward
	(goto-char start)
	(if (re-search-forward (concat "^[ \t]*" which "\\>") nil 'move)
	    (progn (setq state 'at-beginning)
		   (beginning-of-line)))))
    (cond
     ((eq state 'at-beginning) (nsi-goto-beyond-block) t)
     ((eq state 'at-end) t)
     ((eq state 'not-found) nil)
     (t (error "Internal error in `nsi-end-of-def-or-class'")))))

;; Backwards compabitility
(defalias 'end-of-nsi-def-or-class 'nsi-end-of-def-or-class)


;; Functions for marking regions
(defun nsi-mark-block (&optional extend just-move)
  "Mark following block of lines.  With prefix arg, mark structure.
Easier to use than explain.  It sets the region to an `interesting'
block of succeeding lines.  If point is on a blank line, it goes down to
the next non-blank line.  That will be the start of the region.  The end
of the region depends on the kind of line at the start:

 - If a comment, the region will include all succeeding comment lines up
   to (but not including) the next non-comment line (if any).

 - Else if a prefix arg is given, and the line begins one of these
   structures:

     if elif else try except finally for while def class

   the region will be set to the body of the structure, including
   following blocks that `belong' to it, but excluding trailing blank
   and comment lines.  E.g., if on a `try' statement, the `try' block
   and all (if any) of the following `except' and `finally' blocks
   that belong to the `try' structure will be in the region.  Ditto
   for if/elif/else, for/else and while/else structures, and (a bit
   degenerate, since they're always one-block structures) def and
   class blocks.

 - Else if no prefix argument is given, and the line begins a Nsi
   block (see list above), and the block is not a `one-liner' (i.e.,
   the statement ends with a colon, not with code), the region will
   include all succeeding lines up to (but not including) the next
   code statement (if any) that's indented no more than the starting
   line, except that trailing blank and comment lines are excluded.
   E.g., if the starting line begins a multi-statement `def'
   structure, the region will be set to the full function definition,
   but without any trailing `noise' lines.

 - Else the region will include all succeeding lines up to (but not
   including) the next blank line, or code or indenting-comment line
   indented strictly less than the starting line.  Trailing indenting
   comment lines are included in this case, but not trailing blank
   lines.

A msg identifying the location of the mark is displayed in the echo
area; or do `\\[exchange-point-and-mark]' to flip down to the end.

If called from a program, optional argument EXTEND plays the role of
the prefix arg, and if optional argument JUST-MOVE is not nil, just
moves to the end of the block (& does not set mark or display a msg)."
  (interactive "P")			; raw prefix arg
  (nsi-goto-initial-line)
  ;; skip over blank lines
  (while (and
	  (looking-at "[ \t]*$")	; while blank line
	  (not (eobp)))			; & somewhere to go
    (forward-line 1))
  (if (eobp)
      (error "Hit end of buffer without finding a non-blank stmt"))
  (let ((initial-pos (point))
	(initial-indent (current-indentation))
	last-pos			; position of last stmt in region
	(followers
	 '((if elif else) (elif elif else) (else)
	   (try except finally) (except except) (finally)
	   (for else) (while else)
	   (def) (class) ) )
	first-symbol next-symbol)

    (cond
     ;; if comment line, suck up the following comment lines
     ((looking-at "[ \t]*;")
      (re-search-forward "^[ \t]*[^ \t;]" nil 'move) ; look for non-comment
      (re-search-backward "^[ \t]*;")	; and back to last comment in block
      (setq last-pos (point)))

     ;; else if line is a block line and EXTEND given, suck up
     ;; the whole structure
     ((and extend
	   (setq first-symbol (nsi-suck-up-first-keyword) )
	   (assq first-symbol followers))
      (while (and
	      (or (nsi-goto-beyond-block) t) ; side effect
	      (forward-line -1)		; side effect
	      (setq last-pos (point))	; side effect
	      (nsi-goto-statement-below)
	      (= (current-indentation) initial-indent)
	      (setq next-symbol (nsi-suck-up-first-keyword))
	      (memq next-symbol (cdr (assq first-symbol followers))))
	(setq first-symbol next-symbol)))

     ;; else if line *opens* a block, search for next stmt indented <=
     ((nsi-statement-opens-block-p)
      (while (and
	      (setq last-pos (point))	; always true -- side effect
	      (nsi-goto-statement-below)
	      (> (current-indentation) initial-indent))
	nil))

     ;; else plain code line; stop at next blank line, or stmt or
     ;; indenting comment line indented <
     (t
      (while (and
	      (setq last-pos (point))	; always true -- side effect
	      (or (nsi-goto-beyond-final-line) t)
	      (not (looking-at "[ \t]*$")) ; stop at blank line
	      (or
	       (>= (current-indentation) initial-indent)
	       (looking-at "[ \t]*;[^ \t\n]"))) ; ignore non-indenting #
	nil)))

    ;; skip to end of last stmt
    (goto-char last-pos)
    (nsi-goto-beyond-final-line)

    ;; set mark & display
    (if just-move
	()				; just return
      (push-mark (point) 'no-msg)
      (forward-line -1)
      (message "Mark set after: %s" (nsi-suck-up-leading-text))
      (goto-char initial-pos))))

(defun nsi-mark-def-or-class (&optional class)
  "Set region to body of def (or class, with prefix arg) enclosing point.
Pushes the current mark, then point, on the mark ring (all language
modes do this, but although it's handy it's never documented ...).

In most Emacs language modes, this function bears at least a
hallucinogenic resemblance to `\\[nsi-end-of-def-or-class]' and
`\\[nsi-beginning-of-def-or-class]'.

And in earlier versions of Nsi mode, all 3 were tightly connected.
Turned out that was more confusing than useful: the `goto start' and
`goto end' commands are usually used to search through a file, and
people expect them to act a lot like `search backward' and `search
forward' string-search commands.  But because Nsi `def' and `class'
can nest to arbitrary levels, finding the smallest def containing
point cannot be done via a simple backward search: the def containing
point may not be the closest preceding def, or even the closest
preceding def that's indented less.  The fancy algorithm required is
appropriate for the usual uses of this `mark' command, but not for the
`goto' variations.

So the def marked by this command may not be the one either of the
`goto' commands find: If point is on a blank or non-indenting comment
line, moves back to start of the closest preceding code statement or
indenting comment line.  If this is a `def' statement, that's the def
we use.  Else searches for the smallest enclosing `def' block and uses
that.  Else signals an error.

When an enclosing def is found: The mark is left immediately beyond
the last line of the def block.  Point is left at the start of the
def, except that: if the def is preceded by a number of comment lines
followed by (at most) one optional blank line, point is left at the
start of the comments; else if the def is preceded by a blank line,
point is left at its start.

The intent is to mark the containing def/class and its associated
documentation, to make moving and duplicating functions and classes
pleasant."
  (interactive "P")			; raw prefix arg
  (let ((start (point))
	(which (cond ((eq class 'either) "\\(class\\|def\\)")
		     (class "class")
		     (t "def"))))
    (push-mark start)
    (if (not (nsi-go-up-tree-to-keyword which))
	(progn (goto-char start)
	       (error "Enclosing %s not found"
		      (if (eq class 'either)
			  "def or class"
			which)))
      ;; else enclosing def/class found
      (setq start (point))
      (nsi-goto-beyond-block)
      (push-mark (point))
      (goto-char start)
      (if (zerop (forward-line -1))	; if there is a preceding line
	  (progn
	    (if (looking-at "[ \t]*$")	; it's blank
		(setq start (point))	; so reset start point
	      (goto-char start))	; else try again
	    (if (zerop (forward-line -1))
		(if (looking-at "[ \t]*;") ; a comment
		    ;; look back for non-comment line
		    ;; tricky: note that the regexp matches a blank
		    ;; line, cuz \n is in the 2nd character class
		    (and
		     (re-search-backward "^[ \t]*[^ \t;]" nil 'move)
		     (forward-line 1))
		  ;; no comment, so go back
		  (goto-char start)))))))
  (exchange-point-and-mark)
  (nsi-keep-region-active))

;; ripped from cc-mode
(defun nsi-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With \\[universal-argument] (programmatically, optional argument ARG), 
do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
	(re-search-forward
	 "\\(\\W\\|[_]\\)*\\([A-Z]*[a-z0-9]*\\)"
	 (point-max) t arg)
      (while (and (< arg 0)
		  (re-search-backward
		   "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_]\\)\\w+"
		   (point-min) 0))
	(forward-char 1)
	(setq arg (1+ arg)))))
  (nsi-keep-region-active))

(defun nsi-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (nsi-forward-into-nomenclature (- arg))
  (nsi-keep-region-active))



;; Documentation functions

;; dump the long form of the mode blurb; does the usual doc escapes,
;; plus lines of the form ^[vc]:name$ to suck variable & command docs
;; out of the right places, along with the keys they're on & current
;; values
(defun nsi-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
	  funckind funcname func funcdoc
	  (start 0) mstart end
	  keys )
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
	(setq mstart (match-beginning 0)  end (match-end 0)
	      funckind (substring str (match-beginning 1) (match-end 1))
	      funcname (substring str (match-beginning 2) (match-end 2))
	      func (intern funcname))
	(princ (substitute-command-keys (substring str start mstart)))
	(cond
	 ((equal funckind "c")		; command
	  (setq funcdoc (documentation func)
		keys (concat
		      "Key(s): "
		      (mapconcat 'key-description
				 (where-is-internal func nsi-mode-map)
				 ", "))))
	 ((equal funckind "v")		; variable
	  (setq funcdoc (documentation-property func 'variable-documentation)
		keys (if (assq func locals)
			 (concat
			  "Local/Global values: "
			  (prin1-to-string (symbol-value func))
			  " / "
			  (prin1-to-string (default-value func)))
		       (concat
			"Value: "
			(prin1-to-string (symbol-value func))))))
	 (t				; unexpected
	  (error "Error in nsi-dump-help-string, tag `%s'" funckind)))
	(princ (format "\n-> %s:\t%s\t%s\n\n"
		       (if (equal funckind "c") "Command" "Variable")
		       funcname keys))
	(princ funcdoc)
	(terpri)
	(setq start end))
      (princ (substitute-command-keys (substring str start))))
    (print-help-return-message)))

(defun nsi-describe-mode ()
  "Dump long form of Nsi-mode docs."
  (interactive)
  (nsi-dump-help-string "Major mode for editing Nsi files.
Knows about Nsi indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING NSI CODE

\\[nsi-execute-import-or-reload]\timports or reloads the file in the Nsi interpreter
\\[nsi-execute-buffer]\tsends the entire buffer to the Nsi interpreter
\\[nsi-execute-region]\tsends the current region
\\[nsi-execute-def-or-class]\tsends the current function or class definition
\\[nsi-execute-string]\tsends an arbitrary string
\\[nsi-shell]\tstarts a Nsi interpreter window; this will be used by
\tsubsequent Nsi execution commands
%c:nsi-execute-import-or-reload
%c:nsi-execute-buffer
%c:nsi-execute-region
%c:nsi-execute-def-or-class
%c:nsi-execute-string
%c:nsi-shell

@VARIABLES

nsi-indent-offset\tindentation increment
nsi-block-comment-prefix\tcomment string used by comment-region

nsi-nsi-command\tshell command to invoke Nsi interpreter
nsi-temp-directory\tdirectory used for temp files (if needed)

nsi-beep-if-tab-change\tring the bell if tab-width is changed
%v:nsi-indent-offset
%v:nsi-block-comment-prefix
%v:nsi-nsi-command
%v:nsi-temp-directory
%v:nsi-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Nsi, Nsi mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b   # a very wordy single-line comment that ends up being
\t        #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Nsi mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The Nsi-mode commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.


@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[nsi-newline-and-indent]\t insert newline, then indent
\t\\[nsi-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[nsi-guess-indent-offset]\t guess nsi-indent-offset from file content; change locally
\t\\[universal-argument] \\[nsi-guess-indent-offset]\t ditto, but change globally

\t\\[nsi-indent-region]\t reindent region to match its context
\t\\[nsi-shift-region-left]\t shift region left by nsi-indent-offset
\t\\[nsi-shift-region-right]\t shift region right by nsi-indent-offset

Unlike most programming languages, Nsi uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by Nsi-mode is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[nsi-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
nsi-indent-offset is 4, after you enter
\tif a > 0: \\[nsi-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t    _
If you then enter `c = d' \\[nsi-newline-and-indent], the cursor will move
to
\tif a > 0:
\t    c = d
\t    _
Nsi-mode cannot know whether that's what you intended, or whether
\tif a > 0:
\t    c = d
\t_
was your intent.  In general, Nsi-mode either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra nsi-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[nsi-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Nsi-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented nsi-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:nsi-newline-and-indent
%c:nsi-electric-backspace


The next function may be handy when editing code you didn't write:
%c:nsi-guess-indent-offset


The remaining `indent' functions apply to a region of Nsi code.  They
assume the block structure (equals indentation, in Nsi) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:nsi-indent-region
%c:nsi-shift-region-left
%c:nsi-shift-region-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[nsi-mark-block]\t mark block of lines
\\[nsi-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[nsi-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:nsi-mark-block
%c:nsi-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[nsi-previous-statement]\t move to statement preceding point
\\[nsi-next-statement]\t move to statement following point
\\[nsi-goto-block-up]\t move up to start of current block
\\[nsi-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[nsi-beginning-of-def-or-class]\t move to start of class
\\[nsi-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[nsi-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[nsi-next-statement]\t to skip over initial comments and blank lines
Or do `\\[nsi-previous-statement]' with a huge prefix argument.
%c:nsi-previous-statement
%c:nsi-next-statement
%c:nsi-goto-block-up
%c:nsi-beginning-of-def-or-class
%c:nsi-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN NSI MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq  nsi-indent-offset  4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Nsi mode calls with no arguments the value of the variable
`nsi-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `nsi-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When nsi-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to nsi-newline-and-indent."))


;; Helper functions
(defvar nsi-parse-state-re
  (concat
   "^[ \t]*\\(if\\|elif\\|else\\|while\\|def\\|class\\)\\>"
   "\\|"
   "^[^ ;\t\n]"))

(defun nsi-parse-state ()
  "Return the parse state at point (see `parse-partial-sexp' docs)."
  (save-excursion
    (let ((here (point))
	  pps done)
      (while (not done)
	;; back up to the first preceding line (if any; else start of
	;; buffer) that begins with a popular Nsi keyword, or a
	;; non- whitespace and non-comment character.  These are good
	;; places to start parsing to see whether where we started is
	;; at a non-zero nesting level.  It may be slow for people who
	;; write huge code blocks or huge lists ... tough beans.
	(re-search-backward nsi-parse-state-re nil 'move)
	(beginning-of-line)
	;; In XEmacs, we have a much better way to test for whether
	;; we're in a triple-quoted string or not.  Emacs does not
	;; have this built-in function, which is its loss because
	;; without scanning from the beginning of the buffer, there's
	;; no accurate way to determine this otherwise.
	(if (not (fboundp 'buffer-syntactic-context))
	    ;; Emacs
	    (progn
	      (save-excursion (setq pps (parse-partial-sexp (point) here)))
	      ;; make sure we don't land inside a triple-quoted string
	      (setq done (or (not (nth 3 pps))
			     (bobp)))
	      ;; Just go ahead and short circuit the test back to the
	      ;; beginning of the buffer.  This will be slow, but not
	      ;; nearly as slow as looping through many
	      ;; re-search-backwards.
	      (if (not done)
		  (goto-char (point-min))))
	  ;; XEmacs
	  (setq done (or (not (buffer-syntactic-context))
			 (bobp)))
	  (when done
	    (setq pps (parse-partial-sexp (point) here)))
	  ))
      pps)))

(defun nsi-nesting-level ()
  "Return the buffer position of the last unclosed enclosing list.
If nesting level is zero, return nil."
  (let ((status (nsi-parse-state)))
    (if (zerop (car status))
	nil				; not in a nest
      (car (cdr status)))))		; char# of open bracket

(defun nsi-backslash-continuation-line-p ()
  "Return t iff preceding line ends with backslash that is not in a comment."
  (save-excursion
    (beginning-of-line)
    (and
     ;; use a cheap test first to avoid the regexp if possible
     ;; use 'eq' because char-after may return nil
     (eq (char-after (- (point) 2)) ?\\ )
     ;; make sure; since eq test passed, there is a preceding line
     (forward-line -1)			; always true -- side effect
     (looking-at nsi-continued-re))))

(defun nsi-continuation-line-p ()
  "Return t iff current line is a continuation line."
  (save-excursion
    (beginning-of-line)
    (or (nsi-backslash-continuation-line-p)
	(nsi-nesting-level))))

(defun nsi-goto-beginning-of-tqs (delim)
  "Go to the beginning of the triple quoted string we find ourselves in.
DELIM is the TQS string delimiter character we're searching backwards
for."
  (let ((skip (and delim (make-string 1 delim))))
    (when skip
      (save-excursion
	(nsi-safe (search-backward skip))
	(if (and (eq (char-before) delim)
		 (eq (char-before (1- (point))) delim))
	    (setq skip (make-string 3 delim))))
      ;; we're looking at a triple-quoted string
      (nsi-safe (search-backward skip)))))

(defun nsi-goto-initial-line ()
  "Go to the initial line of the current statement.
Usually this is the line we're on, but if we're on the 2nd or
following lines of a continuation block, we need to go up to the first
line of the block."
  ;; Tricky: We want to avoid quadratic-time behavior for long
  ;; continued blocks, whether of the backslash or open-bracket
  ;; varieties, or a mix of the two.  The following manages to do that
  ;; in the usual cases.
  ;;
  ;; Also, if we're sitting inside a triple quoted string, this will
  ;; drop us at the line that begins the string.
  (let (open-bracket-pos)
    (while (nsi-continuation-line-p)
      (beginning-of-line)
      (if (nsi-backslash-continuation-line-p)
	  (while (nsi-backslash-continuation-line-p)
	    (forward-line -1))
	;; else zip out of nested brackets/braces/parens
	(while (setq open-bracket-pos (nsi-nesting-level))
	  (goto-char open-bracket-pos)))))
  (beginning-of-line))

(defun nsi-goto-beyond-final-line ()
  "Go to the point just beyond the fine line of the current statement.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines."
  ;; Tricky: Again we need to be clever to avoid quadratic time
  ;; behavior.
  ;;
  ;; XXX: Not quite the right solution, but deals with multi-line doc
  ;; strings
  (if (looking-at (concat "[ \t]*\\(" nsi-stringlit-re "\\)"))
      (goto-char (match-end 0)))
  ;;
  (forward-line 1)
  (let (state)
    (while (and (nsi-continuation-line-p)
		(not (eobp)))
      ;; skip over the backslash flavor
      (while (and (nsi-backslash-continuation-line-p)
		  (not (eobp)))
	(forward-line 1))
      ;; if in nest, zip to the end of the nest
      (setq state (nsi-parse-state))
      (if (and (not (zerop (car state)))
	       (not (eobp)))
	  (progn
	    (parse-partial-sexp (point) (point-max) 0 nil state)
	    (forward-line 1))))))

(defun nsi-statement-opens-block-p ()
  "Return t iff the current statement opens a block.
I.e., iff it ends with a colon that is not in a comment.  Point should 
be at the start of a statement."
  (save-excursion
    (let ((start (point))
	  (finish (progn (nsi-goto-beyond-final-line) (1- (point))))
	  (searching t)
	  (answer nil)
	  state)
      (goto-char start)
      (while searching
	;; look for a colon with nothing after it except whitespace, and
	;; maybe a comment
	(if (re-search-forward ":\\([ \t]\\|\\\\\n\\)*\\(;.*\\)?$"
			       finish t)
	    (if (eq (point) finish)	; note: no `else' clause; just
					; keep searching if we're not at
					; the end yet
		;; sure looks like it opens a block -- but it might
		;; be in a comment
		(progn
		  (setq searching nil)	; search is done either way
		  (setq state (parse-partial-sexp start
						  (match-beginning 0)))
		  (setq answer (not (nth 4 state)))))
	  ;; search failed: couldn't find another interesting colon
	  (setq searching nil)))
      answer)))

(defun nsi-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (nsi-goto-initial-line)
    (back-to-indentation)
    (prog1
	(looking-at (concat nsi-block-closing-keywords-re "\\>"))
      (goto-char here))))

(defun nsi-goto-beyond-block ()
  "Go to point just beyond the final line of block begun by the current line.
This is the same as where `nsi-goto-beyond-final-line' goes unless
we're on colon line, in which case we go to the end of the block.
Assumes point is at the beginning of the line."
  (if (nsi-statement-opens-block-p)
      (nsi-mark-block nil 'just-move)
    (nsi-goto-beyond-final-line)))

(defun nsi-goto-statement-at-or-above ()
  "Go to the start of the first statement at or preceding point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (nsi-goto-initial-line)
  (if (looking-at nsi-blank-or-comment-re)
      ;; skip back over blank & comment lines
      ;; note:  will skip a blank or comment line that happens to be
      ;; a continuation line too
      (if (re-search-backward "^[ \t]*[^ \t;\n]" nil t)
	  (progn (nsi-goto-initial-line) t)
	nil)
    t))

(defun nsi-goto-statement-below ()
  "Go to start of the first statement following the statement containing point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (beginning-of-line)
  (let ((start (point)))
    (nsi-goto-beyond-final-line)
    (while (and
	    (looking-at nsi-blank-or-comment-re)
	    (not (eobp)))
      (forward-line 1))
    (if (eobp)
	(progn (goto-char start) nil)
      t)))

(defun nsi-go-up-tree-to-keyword (key)
  "Go to begining of statement starting with KEY, at or preceding point.

KEY is a regular expression describing a Nsi keyword.  Skip blank
lines and non-indenting comments.  If the statement found starts with
KEY, then stop, otherwise go back to first enclosing block starting
with KEY.  If successful, leave point at the start of the KEY line and 
return t.  Otherwise, leav point at an undefined place and return nil."
  ;; skip blanks and non-indenting #
  (nsi-goto-initial-line)
  (while (and
	  (looking-at "[ \t]*\\($\\|;[^ \t\n]\\)")
	  (zerop (forward-line -1)))	; go back
    nil)
  (nsi-goto-initial-line)
  (let* ((re (concat "[ \t]*" key "\\b"))
	 (case-fold-search nil)		; let* so looking-at sees this
	 (found (looking-at re))
	 (dead nil))
    (while (not (or found dead))
      (condition-case nil		; in case no enclosing block
	  (nsi-goto-block-up 'no-mark)
	(error (setq dead t)))
      (or dead (setq found (looking-at re))))
    (beginning-of-line)
    found))

(defun nsi-suck-up-leading-text ()
  "Return string in buffer from start of indentation to end of line.
Prefix with \"...\" if leading whitespace was skipped."
  (save-excursion
    (back-to-indentation)
    (concat
     (if (bolp) "" "...")
     (buffer-substring (point) (progn (end-of-line) (point))))))

(defun nsi-suck-up-first-keyword ()
  "Return first keyword on the line as a Lisp symbol.
`Keyword' is defined (essentially) as the regular expression
([a-z]+).  Returns nil if none was found."
  (let ((case-fold-search nil))
    (if (looking-at "[ \t]*\\([a-z]+\\)\\b")
	(intern (buffer-substring (match-beginning 1) (match-end 1)))
      nil)))

(defun nsi-current-defun ()
  "Nsi value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable."
  (save-excursion
    (if (re-search-backward nsi-defun-start-re nil t)
	(or (match-string 3)
	    (let ((method (match-string 2)))
	      (if (and (not (zerop (length (match-string 1))))
		       (re-search-backward nsi-class-start-re nil t))
		  (concat (match-string 1) "." method)
		method)))
      nil)))


(defconst nsi-help-address "nsi-mode@nsi.org"
  "Address accepting submission of bug reports.")

(defun nsi-version ()
  "Echo the current version of `nsi-mode' in the minibuffer."
  (interactive)
  (message "Using `nsi-mode' version %s" nsi-version)
  (nsi-keep-region-active))

;; only works under Emacs 19
;(eval-when-compile
;  (require 'reporter))

(defun nsi-submit-bug-report (enhancement-p)
  "Submit via mail a bug report on `nsi-mode'.
With \\[universal-argument] (programmatically, argument ENHANCEMENT-P
non-nil) just submit an enhancement request."
  (interactive
   (list (not (y-or-n-p
	       "Is this a bug report (hit `n' to send other comments)? "))))
  (let ((reporter-prompt-for-summary-p (if enhancement-p
					   "(Very) brief summary: "
					 t)))
    (require 'reporter)
    (reporter-submit-bug-report
     nsi-help-address			;address
     (concat "nsi-mode " nsi-version)	;pkgname
     ;; varlist
     (if enhancement-p nil
       '(nsi-nsi-command
	 nsi-indent-offset
	 nsi-block-comment-prefix
	 nsi-temp-directory
	 nsi-beep-if-tab-change))
     nil				;pre-hooks
     nil				;post-hooks
     "Dear Barry,")			;salutation
    (if enhancement-p nil
      (set-mark (point))
      (insert 
"Please replace this text with a sufficiently large code sample\n\
and an exact recipe so that I can reproduce your problem.  Failure\n\
to do so may mean a greater delay in fixing your bug.\n\n")
      (exchange-point-and-mark)
      (nsi-keep-region-active))))


;; (defun nsi-kill-emacs-hook ()
;;   "Delete files in `nsi-file-queue'.
;; These are Nsi temporary files awaiting execution."
;;   (mapcar #'(lambda (filename)
;; 	      (nsi-safe (delete-file filename)))
;; 	  nsi-file-queue))

;; ;; arrange to kill temp files when Emacs exists
;; (add-hook 'kill-emacs-hook 'nsi-kill-emacs-hook)



(provide 'nsi-mode)
;;; nsi-mode.el ends here
