;;; csharp-mode.el --- C# mode derived mode

;; Author     : Dylan R. E. Moonfire (original)
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;; Created    : February 2005
;; Modified   : 2018
;; Version    : 0.9.2
;; Package-Version: 20200402.919
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/josteink/csharp-mode
;; Last-saved : 2018-Jul-08

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a major mode for editing C# code.  It performs automatic
;;    indentation of C# syntax; font locking; and integration with
;;    imenu.el.
;;
;;    csharp-mode requires CC Mode 5.30 or later.  It works with
;;    cc-mode 5.31.3, which is current at this time.
;;
;; Features:
;;
;;   - font-lock and indent of C# syntax including:
;;       all c# keywords and major syntax
;;       attributes that decorate methods, classes, fields, properties
;;       enum types
;;       #if/#endif  #region/#endregion
;;       instance initializers
;;       anonymous functions and methods
;;       verbatim literal strings (those that begin with @)
;;       generics
;;
;;   - automagic code-doc generation when you type three slashes.
;;
;;   - compatible with electric-pair-mode for intelligent insertion
;;     of matched braces, quotes, etc.
;;
;;   - imenu integration - generates an index of namespaces, classes,
;;     interfaces, methods, and properties for easy navigation within
;;     the buffer.
;;


;; Installation instructions
;; --------------------------------
;;
;; Put csharp-mode.el somewhere in your load path, optionally byte-compile
;; it, and add the following to your .emacs file:
;;
;;   (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;   (setq auto-mode-alist
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;;
;;
;; Optionally, define and register a mode-hook function.  To do so, use
;; something like this in your .emacs file:
;;
;;   (defun my-csharp-mode-fn ()
;;      "function that runs when csharp-mode is initialized for a buffer."
;;      (turn-on-auto-revert-mode)
;;      (setq indent-tabs-mode nil)
;;      ...insert more code here...
;;      ...including any custom key bindings you might want ...
;;   )
;;   (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)
;;
;;
;;  General
;;  ----------------------------
;;
;;  Mostly C# mode will "just work."  Use `describe-mode' to see the
;;  default keybindings and the highlights of the mode.
;;
;;
;;  imenu integration
;;  -----------------------------
;;
;;  This should just work.  For those who don't know what imenu is, it
;;  allows navigation to different points within the file from an
;;  "Index" menu, in the window's menubar.  csharp-mode computes the
;;  menu containing the namespaces, classes, methods, and so on, in the
;;  buffer.  This happens at the time the file is loaded; for large
;;  files it takes a bit of time to complete the scan.  If you don't
;;  want this capability, set `csharp-want-imenu' to nil.
;;
;;


;;; Known Bugs:
;;
;;   The imenu scan is text-based and naive. For example, if you
;;   intersperse comments between the name of a class/method/namespace,
;;   and the curly brace, the scan will not recognize the thing being
;;   declared. This is fixable - would need to extract the buffer
;;   substring then remove comments before doing the regexp checks - but
;;   it would make the scan much slower.  Also, the scan doesn't deal
;;   with preproc symbol definitions and #if/#else. Those things are
;;   invisible to the scanner csharp-mode uses to build the imenu menu.
;;
;;   Leading identifiers are no longer being fontified, for some reason.
;;   See matchers-before. (Not sure this is still a problem - 19 may
;;   2011 DPC)
;;
;;   Method names with a preceding attribute are not fontified.
;;
;;   The symbol following #if is not fontified.  It should be treated like
;;   define and get font-lock-variable-name-face .
;;
;;   This code doesn't seem to work when you compile it, then
;;   load/require in the emacs file. You will get an error (error
;;   "`c-lang-defconst' must be used in a file") which happens because
;;   cc-mode doesn't think it is in a buffer while loading directly
;;   from the init. However, if you call it based on a file extension,
;;   it works properly. Interestingly enough, this doesn't happen if
;;   you don't byte-compile cc-mode.
;;
;;
;;
;;  Todo:
;;
;;   imenu should scan for and find delegates and events, in addition
;;   to the classes, structs, properties and methods it does currently.
;;
;;   Get csharp-mode.el accepted as part of the emacs standard distribution.
;;   Must contact monnier at iro.umontreal.ca to make this happen.
;;
;;   Add refactoring capabilities?
;;     - extract as method - extract a block of code into a method
;;     - extract as Func<> - extract a block of code into an Action<T>
;;
;;   More code-gen power:
;;     - interface implementation - I think would require csharp-shell
;;
;;
;;  Acknowledgements:
;;
;;    Thanks to Alan Mackenzie and Stefan Monnier for answering questions
;;    and making suggestions. And to Trey Jackson for sharing his
;;    knowledge of emacs lisp.
;;
;;

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fixed the identification on the "enum" keyword.
;;          - Fixed the font-lock on the "base" keyword
;;    0.3.0 - Added a regex to fontify attributes. It isn't the
;;            the best method, but it handles single-like attributes
;;            well.
;;          - Got "super" not to fontify as a keyword.
;;          - Got extending classes and interfaces to fontify as something.
;;    0.4.0 - Removed the attribute matching because it broke more than
;;            it fixed.
;;          - Corrected a bug with namespace not being properly identified
;;            and treating the class level as an inner object, which screwed
;;            up formatting.
;;          - Added "partial" to the keywords.
;;    0.5.0 - Found bugs with compiled cc-mode and loading from init files.
;;          - Updated the eval-when-compile to code to let the mode be
;;            compiled.
;;    0.6.0 - Added the c-filter-ops patch for 5.31.1 which made that
;;            function in cc-langs.el unavailable.
;;          - Added a csharp-lineup-region for indentation #region and
;;            #endregion block differently.
;;    0.7.0 - Added autoload so update-directory-autoloads works
;;            (Thank you, Nikolaj Schumacher)
;;          - Fontified the entire #region and #endregion lines.
;;          - Initial work to get get, set, add, remove font-locked.
;;    0.7.1 - Added option to indent #if/endif with code
;;          - Fixed c-opt-cpp-prefix defn (it must not include the BOL
;;            char (^).
;;          - proper fontification and indent of classes that inherit
;;            (previously the colon was confusing the parser)
;;          - reclassified namespace as a block beginner
;;          - removed $ as a legal symbol char - not legal in C#.
;;          - added struct to c-class-decl-kwds so indent is correct
;;            within a struct.
;;    0.7.2 - Added automatic codedoc insertion.
;;    0.7.3 - Instance initializers (new Type { ... } ) and
;;            (new Type() { ...} ) are now indented properly.
;;          - proper fontification and indent of enums as brace-list-*,
;;            including special treatment for enums that explicitly
;;            inherit from an int type. Previously the colon was
;;            confusing the parser.
;;          - proper fontification of verbatim literal strings,
;;            including those that end in slash. This edge case was not
;;            handled at all before; it is now handled correctly.
;;          - code cleanup and organization; removed the formfeed.
;;          - intelligent curly-brace insertion with
;;            `csharp-insert-open-brace'
;;    0.7.4 - added a C# style
;;          - using is now a keyword and gets fontified correctly
;;          - fixed a bug that had crept into the codedoc insertion.
;;    0.7.5 - now fontify namespaces in the using statements. This is
;;            done in the csharp value for c-basic-matchers-before .
;;          - also fontify the name following namespace decl.
;;            This is done in the csharp value for c-basic-matchers-after .
;;          - turn on recognition of generic types. They are now
;;            fontified correctly.
;;          - <> are now treated as syntactic parens and can be jumped
;;            over with c-forward-sexp.
;;          - Constructors are now fontified.
;;          - Field/Prop names inside object initializers are now fontified.
;;
;;    0.7.7 - relocate running c-run-mode-hooks to the end of
;;            csharp-mode, to allow user to modify key bindings in a
;;            hook if he doesn't like the defaults.
;;
;;    0.7.8 - redefine csharp-log to insert timestamp.
;;          - Fix byte-compile errors on emacs 23.2 ?  Why was
;;            c-filter-ops duplicated here?  What was the purpose of its
;;            presence here, I am not clear.
;;
;;    0.8.0 - include flymake magic into this module.
;;          - include yasnippet integration
;;
;;    0.8.2 2011 April DPC
;;          - small tweaks; now set a one-time bool for flymake installation
;;          - some doc updates on flymake
;;
;;    0.8.3 2011 May 17  DPC
;;          - better help on csharp-mode
;;          - csharp-move-* functions for manual navigation.
;;          - imenu integration for menu-driven navigation - navigate to
;;            named methods, classes, etc.
;;          - adjusted the flymake regexp to handle output from fxcopcmd,
;;            and extended the help to provide examples how to use this.
;;
;;    0.8.4 DPC 2011 May 18
;;          - fix a basic bug in the `csharp-yasnippet-fixup' fn.
;;
;;    0.8.5 DPC 2011 May 21
;;          - imenu: correctly parse Properties that are part of an
;;            explicitly specified interface. Probably need to do this
;;            for methods, too.
;;          - fontify the optional alias before namespace in a using (import).
;;          - Tweak open-curly magic insertion for object initializers.
;;          - better fontification of variables and references
;;          - "sealed" is now fontified as a keyword
;;          - imenu: correctly index ctors that call this or base.
;;          - imenu: correctly index Extension methods (this System.Enum e)
;;          - imenu: correctly scan  method params tagged with out, ref, params
;;          - imenu scan: now handle curlies within strings.
;;          - imenu: split menus now have better labels, are sorted correctly.
;;
;;    0.8.6 DPC 2011 May ??
;;          - extern keyword
;;
;;    0.8.7 2014 November 29
;;          - Fix broken cl-dependency in emacs24.4 and defadvice for tooltips.
;;
;;    0.8.8 2014 December 3
;;          - Fix broken byte-compile.
;;          - Add extra C# keywords.
;;          - Call prog-mode hooks.
;;
;;    0.8.9 2015 March 15
;;          - (Re)add compilation-mode support for msbuild and xbuild.
;;
;;    0.8.10 2015 May 31th
;;          - Imenu: Correctly handle support for default-values in paramlist.
;;
;;    0.8.11 2015 November 21st
;;          - Make mode a derived mode. Improve evil-support.
;;          - Add support for devenv compilation-output.
;;          - Fix all runtime warnings
;;          - Fix error with string-values in #region directives.
;;
;;    0.8.12 2016 January 6th
;;          - Various fixes and improvements for imenu indexing.
;;
;;    0.9.0 2016 September 9th
;;          - Fix issues with compilation-mode and lines with arrays.
;;          - Fontification of compiler directives.
;;          - Much faster, completely rewritten imenu-implementation.
;;          - Fix indentation issues.
;;          - Fix Emacs-25 related bugs.
;;          - Cleaned up dead code.
;;
;;    0.9.1 2017
;;          - Fix indentation for generic type-initializers.
;;          - Fix fontification of using and namespace-statements with
;;            underscores in them.
;;          - Fixes for indentation for many kinds of type-initializers.
;;
;;    0.9.2 2018 July
;;          - Try to fix some breakage introduced by changes in Emacs 27.
;;
;;; Code:

(require 'cc-mode)
(require 'cc-fonts)
(require 'cl-lib)

;; prevent warnings like
;; csharp-mode.el:4134:21:Warning: reference to free variable
;; `compilation-error-regexp-alist-alist'
(require 'compile)

;; Work around emacs bug#23053
(eval-when-compile
  (require 'cc-langs))

;; Work around emacs bug#18845
(eval-when-compile
  (when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
    (require 'cl)))

(require 'imenu)

;; ==================================================================
;; c# upfront stuff
;; ==================================================================

;; This is a copy of the function in cc-mode which is used to handle the
;; eval-when-compile which is needed during other times.
;;
;; NB: I think this is needed to satisfy requirements when this module
;; calls `c-lang-defconst'. (DPC)

;; (defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
;;   ;; See cc-langs.el, a direct copy.
;;   (unless (listp (car-safe ops))
;;     (setq ops (list ops)))
;;   (cond ((eq opgroup-filter t)
;;          (setq opgroup-filter (lambda (opgroup) t)))
;;         ((not (functionp opgroup-filter))
;;          (setq opgroup-filter `(lambda (opgroup)
;;                                  (memq opgroup ',opgroup-filter)))))
;;   (cond ((eq op-filter t)
;;          (setq op-filter (lambda (op) t)))
;;         ((stringp op-filter)
;;          (setq op-filter `(lambda (op)
;;                             (string-match ,op-filter op)))))
;;   (unless xlate
;;     (setq xlate 'identity))
;;   (c-with-syntax-table (c-lang-const c-mode-syntax-table)
;;     (delete-duplicates
;;      (mapcan (lambda (opgroup)
;;                (when (if (symbolp (car opgroup))
;;                          (when (funcall opgroup-filter (car opgroup))
;;                            (setq opgroup (cdr opgroup))
;;                            t)
;;                        t)
;;                  (mapcan (lambda (op)
;;                            (when (funcall op-filter op)
;;                              (let ((res (funcall xlate op)))
;;                                (if (listp res) res (list res)))))
;;                          opgroup)))
;;              ops)
;;      :test 'equal)))


(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)


;; Custom variables
;; ensure all are defined before using ...;

(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'csharp)

;; The following fn allows this:
;;    (csharp-log 3 "scan result...'%s'" state)

(defcustom csharp-log-level 0
  "The current log level for CSharp-mode-specific operations.
This is used in particular by the verbatim-literal
string scanning.

Most other csharp functions are not instrumented.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG, 4 = SHUTUP ALREADY."
  :type 'integer
  :group 'csharp)


(defcustom csharp-want-imenu t
  "*Whether to generate a buffer index via imenu for C# buffers."
  :type 'boolean :group 'csharp)





;; These are only required at compile time to get the sources for the
;; language constants.  (The load of cc-fonts and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)

(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; ==================================================================
  ;; constants used in this module
  ;; ==================================================================

  (defconst csharp-type-initializer-statement-re
    (concat
     "\\<new[ \t\n\r\f\v]+"
     "\\([[:alpha:]_][[:alnum:]_<>\\.]*\\)")
    "Regexp that captures a type-initializer statement in C#")

  (defconst csharp-enum-decl-re
    (concat
     "\\<enum[ \t\n\r\f\v]+"
     "\\([[:alpha:]_][[:alnum:]_]*\\)"
     "[ \t\n\r\f\v]*"
     "\\(:[ \t\n\r\f\v]*"
     "\\("
     (c-make-keywords-re nil
       (list "sbyte" "byte" "short" "ushort" "int" "uint" "long" "ulong"))
     "\\)"
     "\\)?")
    "Regex that captures an enum declaration in C#")

  ;; ==================================================================

  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'csharp-mode 'java-mode))

;; ==================================================================
;; end of c# upfront stuff
;; ==================================================================











;; ==================================================================
;; csharp-mode utility and feature defuns
;; ==================================================================

(defun csharp--at-vsemi-p (&optional pos)
  "Determines if there is a virtual semicolon at POS or point.
It returns t if at a position where a virtual-semicolon is.
Otherwise nil.

This is the C# version of the function.  It gets set into
the variable `c-at-vsemi-p-fn'.

A vsemi is a cc-mode concept implying the end of a statement,
where no actual end-of-statement signifier character ( semicolon,
close-brace) appears.  The concept is used to allow proper
indenting of blocks of code: Where a vsemi appears, the following
line will not indent further.

A vsemi appears in 2 cases in C#:

 - after an attribute that decorates a class, method, field, or
   property.

 - in an object initializer, before the open-curly?

An example of the former is  [WebMethod] or [XmlElement].

Providing this function allows the indenting in `csharp-mode'
to work properly with code that includes attributes."
  (save-excursion
    (let ((pos-or-point (progn (if pos (goto-char pos)) (point))))

      (cond

       ;; before open curly in object initializer. new Foo* { }
       ((and (looking-back
              (concat "\\<new[ \t\n\f\v\r]+"
                      ;; typename
                      "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
                      "[A-Za-z_][[:alnum:]]*"
                      ;; simplified generic constraint.
                      ;; handles generic sub-types.
                      ;; { is optional because otherwise initializers with
                      ;; bracket on same line will indent wrongly.
                      "\\(?:<[[:alnum:], <>]+>[ \t\n\f\v\r]*{?\\)?"
                      ;; optional array-specifier
                      "\\(?:\\[\\]\\)?"
                      ;; spacing
                      "[\ t\n\f\v\r]*") nil)
             (looking-at "[ \t\n\f\v\r]*{"))
        t)

       ;; put a vsemi after an attribute, as with
       ;;   [XmlElement]
       ;; Except when the attribute is used within a line of code, as
       ;; specifying something for a parameter.
       ((c-safe (backward-sexp) t)
        (cond
         ((re-search-forward
           (concat
            "\\(\\["
            "[ \t\n\r\f\v]*"
            "\\("
            "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
            "[A-Za-z_][[:alnum:]]*"
            "\\)"
            "[^]]*\\]\\)"
            )
           (1+ pos-or-point) t)

          (c-safe (backward-sexp))
          (c-backward-syntactic-ws)
          (cond

           ((eq (char-before) 93) ;; close sq brace (a previous attribute)
            (csharp--at-vsemi-p (point))) ;; recurse

           ((or
             (eq (char-before) 59) ;; semicolon
             (eq (char-before) 123) ;; open curly
             (eq (char-before) 125)) ;; close curly
            t)

           ;; attr is used within a line of code
           (t nil)))

         (t nil)))

       (t nil))
      )))

(defun csharp--at-lambda-header ()
  "Determines if there is lambda header at point"
  (or (looking-at "([[:alnum:][:space:]_,]*)[ \t\n]*=>[ \t\n]*{")
      (looking-at "[[:alnum:]_]+[ \t\n]*=>[ \t\n]*{")))

;; ==================================================================
;; end of csharp-mode utility and feature defuns
;; ==================================================================



;; ==================================================================
;; c# values for "language constants" defined in cc-langs.el
;; ==================================================================

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp--at-vsemi-p)


;; This c-opt-after-id-concat-key is a regexp that matches
;; dot.  In other words: "\\(\\.\\)"
;; Not sure why this needs to be so complicated.
;; This const is now internal (obsolete); need to move to
;; c-after-id-concat-ops.  I don't yet understand the meaning
;; of that variable, so for now. . .  .

;; (c-lang-defconst c-opt-after-id-concat-key
;;   csharp (if (c-lang-const c-opt-identifier-concat-key)
;;              (c-lang-const c-symbol-start)))

(c-lang-defconst c-opt-after-id-concat-key
  csharp "[[:alpha:]_]" )




;; The matchers elements can be of many forms.  It gets pretty
;; complicated.  Do a describe-variable on font-lock-keywords to get a
;; description.  (Why on font-lock-keywords? I don't know, but that's
;; where you get the help.)
;;
;; Aside from the provided documentation, the other option of course, is
;; to look in the source code as an example for what to do.  The source
;; in cc-fonts uses a defun c-make-font-lock-search-function to produce
;; most of the matchers.  Called this way:
;;
;;   (c-make-font-lock-search-function  regexp '(A B c))
;;
;; The REGEXP is used in re-search-forward, and if there's a match, then
;; A is called within a save-match-data. If B and C are non-nil, they
;; are called as pre and post blocks, respecitvely.
;;
;; Anyway the c-make-font-lock-search-function works for a single regex,
;; but more complicated scenarios such as those intended to match and
;; fontify object initializers, call for a hand-crafted lambda.
;;
;; The object initializer is special because matching on it must
;; allow nesting.
;;
;; In c#, the object initializer block is used directly after a
;; constructor, like this:
;;
;;     new MyType
;;     {
;;        Prop1 = "foo"
;;     }
;;
;; csharp-mode needs to fontify the properties in the
;; initializer block in font-lock-variable-name-face. The key thing is
;; to set the text property on the open curly, using type c-type and
;; value c-decl-id-start. This apparently allows `parse-partial-sexp' to
;; do the right thing, later.
;;
;; This simple case is easy to handle in a regex, using the basic
;; `c-make-font-lock-search-function' form.  But the general syntax for a
;; constructor + object initializer in C# is more complex:
;;
;;     new MyType(..arglist..) {
;;        Prop1 = "foo"
;;     }
;;
;; A simple regex match won't satisfy here, because the ..arglist.. can
;; be anything, including calls to other constructors, potentially with
;; object initializer blocks. This may nest arbitrarily deeply, and the
;; regex in emacs doesn't support balanced matching.  Therefore there's
;; no way to match on the "outside" pair of parens, to find the relevant
;; open curly.  What's necessary is to do the match on "new MyType" then
;; skip over the sexp defined by the parens, then set the text property on
;; the appropriate open-curly.
;;
;; To make that happen, it's good to have insight into what the matcher
;; really does.  The output of `c-make-font-lock-search-function' before
;; byte-compiling, is:
;;
;; (lambda (limit)
;;   (let ((parse-sexp-lookup-properties
;;          (cc-eval-when-compile
;;            (boundp 'parse-sexp-lookup-properties))))
;;     (while (re-search-forward REGEX limit t)
;;       (unless
;;           (progn
;;             (goto-char (match-beginning 0))
;;             (c-skip-comments-and-strings limit))
;;         (goto-char (match-end 0))
;;         (progn
;;           B
;;           (save-match-data A)
;;           C ))))
;;   nil)
;;
;; csharp-mode uses this hand-crafted form of a matcher to handle the
;; general case for constructor + object initializer, within
;; `c-basic-matchers-after' .
;;




;; (defun c-make-font-lock-search-function (regexp &rest highlights)
;;     ;; This function makes a byte compiled function that works much like
;;     ;; a matcher element in `font-lock-keywords'.  It cuts out a little
;;     ;; bit of the overhead compared to a real matcher.  The main reason
;;     ;; is however to pass the real search limit to the anchored
;;     ;; matcher(s), since most (if not all) font-lock implementations
;;     ;; arbitrarily limits anchored matchers to the same line, and also
;;     ;; to insulate against various other irritating differences between
;;     ;; the different (X)Emacs font-lock packages.
;;     ;;
;;     ;; REGEXP is the matcher, which must be a regexp.  Only matches
;;     ;; where the beginning is outside any comment or string literal are
;;     ;; significant.
;;     ;;
;;     ;; HIGHLIGHTS is a list of highlight specs, just like in
;;     ;; `font-lock-keywords', with these limitations: The face is always
;;     ;; overridden (no big disadvantage, since hits in comments etc are
;;     ;; filtered anyway), there is no "laxmatch", and an anchored matcher
;;     ;; is always a form which must do all the fontification directly.
;;     ;; `limit' is a variable bound to the real limit in the context of
;;     ;; the anchored matcher forms.
;;     ;;
;;     ;; This function does not do any hidden buffer changes, but the
;;     ;; generated functions will.  (They are however used in places
;;     ;; covered by the font-lock context.)
;;
;;     ;; Note: Replace `byte-compile' with `eval' to debug the generated
;;     ;; lambda easier.
;;     (byte-compile
;;      `(lambda (limit)
;;         (let (;; The font-lock package in Emacs is known to clobber
;;               ;; `parse-sexp-lookup-properties' (when it exists).
;;               (parse-sexp-lookup-properties
;;                (cc-eval-when-compile
;;                  (boundp 'parse-sexp-lookup-properties))))
;;           (while (re-search-forward ,regexp limit t)
;;             (unless (progn
;;                       (goto-char (match-beginning 0))
;;                       (c-skip-comments-and-strings limit))
;;               (goto-char (match-end 0))
;;               ,@(mapcar
;;                  (lambda (highlight)
;;                    (if (integerp (car highlight))
;;                        (progn
;;                          (unless (eq (nth 2 highlight) t)
;;                            (error
;;                             "The override flag must currently be t in %s"
;;                             highlight))
;;                          (when (nth 3 highlight)
;;                            (error
;;                             "The laxmatch flag may currently not be set in %s"
;;                             highlight))
;;                          `(save-match-data
;;                             (c-put-font-lock-face
;;                              (match-beginning ,(car highlight))
;;                              (match-end ,(car highlight))
;;                              ,(elt highlight 1))))
;;                      (when (nth 3 highlight)
;;                        (error "Match highlights currently not supported in %s"
;;                               highlight))
;;                      `(progn
;;                         ,(nth 1 highlight)
;;                         (save-match-data ,(car highlight))
;;                         ,(nth 2 highlight))))
;;                  highlights))))
;;         nil))
;;     )


(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;;;; Font-lock the attributes by searching for the
           ;;;; appropriate regex and marking it as TODO.
           ;;,`(,(concat "\\(" csharp-attribute-regex "\\)")
           ;;   0 font-lock-function-name-face)

           ;; Put a warning face on the opener of unclosed strings that
           ;; can't span lines.  Later font
           ;; lock packages have a `font-lock-syntactic-face-function' for
           ;; this, but it doesn't give the control we want since any
           ;; fontification done inside the function will be
           ;; unconditionally overridden.
           ,(c-make-font-lock-search-function
             ;; Match a char before the string starter to make
             ;; `c-skip-comments-and-strings' work correctly.
             (concat ".\\(" c-string-limit-regexp "\\)")
             '((if (fboundp 'c-font-lock-invalid-string)
                   (c-font-lock-invalid-string)
                 (csharp-mode-font-lock-invalid-string))))


           ;; Fontify keyword constants.
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil
                           (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))


           ;; Fontify the namespaces that follow using statements.
           ;; This regex handles the optional alias, as well.
           ,`(,(concat
                "\\<\\(using\\)[ \t\n\f\v\r]+"
                "\\(?:"
                "\\([A-Za-z0-9_]+\\)"
                "[ \t\n\f\v\r]*="
                "[ \t\n\f\v\r]*"
                "\\)?"
                "\\(\\(?:[A-Za-z0-9_]+\\.\\)*[A-Za-z0-9_]+\\)"
                "[ \t\n\f\v\r]*;")
              (2 font-lock-constant-face t t)
              (3 font-lock-constant-face))


           ;; Fontify all keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)
           ))



(c-lang-defconst c-basic-matchers-after
  csharp `(

           ;; option 1:
           ;;            ,@(when condition
           ;;                `((,(byte-compile
           ;;                     `(lambda (limit) ...
           ;;
           ;; option 2:
           ;;            ,`((lambda (limit) ...
           ;;
           ;; I don't know how to avoid the (when condition ...) in the
           ;; byte-compiled version.
           ;;
           ;; X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+

           ;; Case 1: invocation of constructor + maybe an object
           ;; initializer.  Some possible examples that satisfy:
           ;;
           ;;   new Foo ();
           ;;
           ;;   new Foo () { };
           ;;
           ;;   new Foo {  };
           ;;
           ;;   new Foo { Prop1= 7 };
           ;;
           ;;   new Foo {
           ;;     Prop1= 7
           ;;   };
           ;;
           ;;   new Foo {
           ;;     Prop1= 7,
           ;;     Prop2= "Fred"
           ;;   };
           ;;
           ;;   new Foo {
           ;;      Prop1= new Bar()
           ;;   };
           ;;
           ;;   new Foo {
           ;;      Prop1= new Bar { PropA = 5.6F }
           ;;   };
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))

                         (while (re-search-forward
                                 ,(concat "\\<new"
                                          "[ \t\n\r\f\v]+"
                                          "\\(\\(?:"
                                          (c-lang-const c-symbol-key)
                                          "\\.\\)*"
                                          (c-lang-const c-symbol-key)
                                          "\\)"
                                          )
                                 limit t)
                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))

                             (csharp-log 3 "ctor invoke? at %d" (match-beginning 1))

                             (save-match-data
                               ;; next thing could be: [] () <> or {} or nothing (semicolon, comma).

                               ;; fontify the typename
                               (c-put-font-lock-face (match-beginning 1)
                                                     (match-end 1)
                                                     'font-lock-type-face)

                               (goto-char (match-end 0))
                               (c-forward-syntactic-ws limit)
                               (if (eq (char-after) ?<) ;; ctor for generic type
                                   (progn
                                     (csharp-log 3 " - this is a generic type")
                                     ;; skip over <> safely
                                     (c-safe (c-forward-sexp 1) t)
                                     (c-forward-syntactic-ws)))

                               ;; now, could be [] or (..) or {..} or semicolon.

                               (csharp-log 3 " - looking for sexp")

                               (if (or
                                    (eq (char-after) ?{) ;; open curly
                                    ;; is square parenthesis block? - start
                                    (let* ((start (point)) ;; used to hold our position, so that we know that
                                           (end))          ;; our code isn't stuck trying to look for a non-existent sexp.
                                      (and (eq (char-after) 91) ;; open square
                                           (while (and (eq (char-after) 91)
                                                       (not (eq start end)))
                                             (c-safe (c-forward-sexp 1))
                                             (setq end (point)))
                                           (eq (char-before) 93)))
                                    ;; is square parenthesis block? - end
                                    (and (eq (char-after) 40) ;; open paren
                                         (c-safe (c-forward-sexp 1) t)))

                                   (progn
                                     ;; at this point we've jumped over any intervening s-exp,
                                     ;; like sq brackets or parens.
                                     (c-forward-syntactic-ws)
                                     (csharp-log 3 " - after fwd-syn-ws point(%d)" (point))
                                     (csharp-log 3 " - next char:  %c" (char-after))
                                     (if (eq (char-after) ?{)
                                         (let ((start (point))
                                               (end (if (c-safe (c-forward-sexp 1) t)
                                                        (point) 0)))
                                           (csharp-log 3 " -  open curly gets c-decl-id-start %d" start)
                                           (c-put-char-property start
                                                                'c-type
                                                                'c-decl-id-start)
                                           (goto-char start)
                                           (if (> end start)
                                               (progn
                                                 (forward-char 1) ;; step over open curly
                                                 (c-forward-syntactic-ws)
                                                 (while (> end (point))
                                                   ;; now, try to fontify/assign variables to any properties inside the curlies
                                                   (csharp-log 3 " - inside open curly  point(%d)" (point))
                                                   (csharp-log 3 " -   next char:  %c" (char-after))
                                                   ;; fontify each property assignment
                                                   (if (re-search-forward
                                                        (concat "\\(" (c-lang-const c-symbol-key) "\\)\\s*=")
                                                        end t)
                                                       (progn
                                                         (csharp-log 3 " -   found variable  %d-%d"
                                                                     (match-beginning 1)
                                                                     (match-end 1))
                                                         (c-put-font-lock-face (match-beginning 1)
                                                                               (match-end 1)
                                                                               'font-lock-variable-name-face)
                                                         (goto-char (match-end 0))
                                                         (c-forward-syntactic-ws)
                                                         ;; advance to the next assignment, if possible
                                                         (if (eq (char-after) ?@)
                                                             (forward-char 1))

                                                         (if (c-safe (c-forward-sexp 1) t)
                                                             (progn
                                                               (forward-char 1)
                                                               (c-forward-syntactic-ws))))

                                                     ;; else
                                                     (csharp-log 3 " -   no more assgnmts found")
                                                     (goto-char end)))))
                                           )))))

                             (goto-char (match-end 0))
                             )))
                       nil))
                  )))


           ;; Case 2: declaration of enum with or without an explicit
           ;; base type.
           ;;
           ;; Examples:
           ;;
           ;;  public enum Foo { ... }
           ;;
           ;;  public enum Foo : uint { ... }
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))
                         (while (re-search-forward
                                 ,(concat csharp-enum-decl-re
                                          "[ \t\n\r\f\v]*"
                                          "{")
                                 limit t)

                           (csharp-log 3 "enum? at %d" (match-beginning 0))

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))
                             (progn
                               (save-match-data
                                 (goto-char (match-end 0))
                                 (c-put-char-property (1- (point))
                                                      'c-type
                                                      'c-decl-id-start)
                                 (c-forward-syntactic-ws))
                               (save-match-data
                                 (with-no-warnings
                                   (condition-case nil
                                       (c-font-lock-declarators limit t nil)
                                     (wrong-number-of-arguments
                                      (c-font-lock-declarators limit t nil nil)))))
                               (goto-char (match-end 0))
                               )
                             )))
                       nil))
                  )))

           ;; Case 4: using clause. Without this, using (..) gets fontified as a fn.
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))
                         (while (re-search-forward
                                 ,(concat "\\<\\(using\\)"
                                          "[ \t\n\r\f\v]*"
                                          "(")
                                 limit t)

                           (csharp-log 3 "using clause p(%d)" (match-beginning 0))

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))

                             (save-match-data
                               (c-put-font-lock-face (match-beginning 1)
                                                     (match-end 1)
                                                     'font-lock-keyword-face)
                               (goto-char (match-end 0))))))
                       nil))
                  )))

           ;; Case 5: attributes
           ,`((lambda (limit)
                (let ((parse-sexp-lookup-properties
                       (cc-eval-when-compile
                         (boundp 'parse-sexp-lookup-properties))))

                  (while (re-search-forward
                          ,(concat "[ \t\n\r\f\v]+"
                                   "\\(\\["
                                   "[ \t\n\r\f\v]*"
                                   "\\(?:\\(?:return\\|assembly\\)[ \t]*:[ \t]*\\)?"
                                   "\\("
                                   "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
                                   "[A-Za-z_][[:alnum:]]*"
                                   "\\)"
                                   "[^]]*\\]\\)"
                                   )
                          limit t)

                    (csharp-log 3 "attribute? - %d limit(%d)" (match-beginning 1)
                                limit)

                    (unless
                        (progn
                          (goto-char (match-beginning 1))
                          (c-skip-comments-and-strings limit))

                      (let ((b2 (match-beginning 2))
                            (e2 (match-end 2))
                            (is-attr nil))
                        (csharp-log 3 " - type match: %d - %d"
                                    b2 e2)
                        (save-match-data
                          (c-backward-syntactic-ws)
                          (setq is-attr (or
                                         (eq (char-before) 59) ;; semicolon
                                         (eq (char-before) 93) ;; close square brace
                                         (eq (char-before) 123) ;; open curly
                                         (eq (char-before) 125) ;; close curly
                                         (save-excursion
                                           (c-beginning-of-statement-1)
                                           (looking-at
                                            "#\\s *\\(pragma\\|endregion\\|region\\|if\\|else\\|endif\\)"))
                                         )))

                        (if is-attr
                            (progn
                              (if (<= 3 csharp-log-level)
                                  (csharp-log 3 " - attribute: '%s'"
                                              (buffer-substring-no-properties b2 e2)))
                              (c-put-font-lock-face b2 e2 'font-lock-type-face)))))
                    (goto-char (match-end 0))
                    ))
                nil))

           ;; Fontify labels after goto etc.
           ,@(when (c-lang-const c-before-label-kwds)
               `( ;; (Got three different interpretation levels here,
                 ;; which makes it a bit complicated: 1) The backquote
                 ;; stuff is expanded when compiled or loaded, 2) the
                 ;; eval form is evaluated at font-lock setup (to
                 ;; substitute c-label-face-name correctly), and 3) the
                 ;; resulting structure is interpreted during
                 ;; fontification.)
                 (eval
                  . ,(let* ((c-before-label-re
                             (c-make-keywords-re nil
                               (c-lang-const c-before-label-kwds))))
                       `(list
                         ,(concat "\\<\\(" c-before-label-re "\\)\\>"
                                  "\\s *"
                                  "\\(" ; identifier-offset
                                  (c-lang-const c-symbol-key)
                                  "\\)")
                         (list ,(+ (regexp-opt-depth c-before-label-re) 2)
                               c-label-face-name nil t))))))



           ;; Fontify the clauses after various keywords.
           ,@(when (or (c-lang-const c-type-list-kwds)
                       (c-lang-const c-ref-list-kwds)
                       (c-lang-const c-colon-type-list-kwds)
                       (c-lang-const c-paren-type-kwds))
               `((,(c-make-font-lock-search-function
                    (concat "\\<\\("
                            (c-make-keywords-re nil
                              (append (c-lang-const c-type-list-kwds)
                                      (c-lang-const c-ref-list-kwds)
                                      (c-lang-const c-colon-type-list-kwds)
                                      (c-lang-const c-paren-type-kwds)))
                            "\\)\\>")
                    '((c-fontify-types-and-refs ((c-promote-possible-types t))
                        (c-forward-keyword-clause 1)
                        (if (> (point) limit) (goto-char limit))))))))


           ;; Fontify the name that follows each namespace declaration
           ;; this needs to be done in the matchers-after because
           ;; otherwise the namespace names get the font-lock-type-face,
           ;; due to the energetic efforts of c-forward-type.
           ,`("\\<\\(namespace\\)[ \t\n\r\f\v]+\\(\\(?:[A-Za-z0-9_]+\\.\\)*[A-Za-z0-9_]+\\)"
              2 font-lock-constant-face t)


           ;; Highlight function-invocation.
           ;; (this may in the future use font-lock-function-call-face, if standardized)
           ,`(,"\\.\\([A-Za-z0-9_]+\\)("
              1 font-lock-function-name-face t)


           ))

;; verbatim string literals can be multiline
(c-lang-defconst c-multiline-string-start-char
  csharp ?@)

(defun csharp-mode-syntax-propertize-function (beg end)
  "Apply syntax table properties to special constructs in region BEG to END.
Currently handled:

- Fontify verbatim literal strings correctly
- Highlight text after #region or #pragma as comment"
  (save-excursion
    (goto-char beg)
    (while (search-forward "@\"" end t)
      (let ((in-comment-or-string-p (save-excursion
                                      (goto-char (match-beginning 0))
                                      (or (nth 3 (syntax-ppss))
                                          (nth 4 (syntax-ppss))))))
        (when (not in-comment-or-string-p)
          (let (done)
            (while (and (not done) (< (point) end))
              (skip-chars-forward "^\"\\\\" end)
              (cond
               ((= (following-char) ?\\)
                (put-text-property (point) (1+ (point))
                                   'syntax-table (string-to-syntax "."))
                (forward-char 1))
               ((= (following-char) ?\")
                (forward-char 1)
                (if (= (following-char) ?\")
                    (progn
                      (put-text-property (1- (point)) (1+ (point))
                                         'syntax-table (string-to-syntax "/"))
                      (forward-char 1))
                  (setq done t)))))))))

    (goto-char beg)
    (while (re-search-forward "^\\s *#\\s *\\(region\\|pragma\\)\\s " end t)
      (when (looking-at "\\s *\\S ")
        ;; mark the whitespace separating the directive from the comment
        ;; text as comment starter to allow correct word movement
        (put-text-property (1- (point)) (point)
                           'syntax-table (string-to-syntax "< b"))))))

;; C# does generics.  Setting this to t tells the parser to put
;; parenthesis syntax on angle braces that surround a comma-separated
;; list.
(c-lang-defconst c-recognize-<>-arglists
  csharp t)


(c-lang-defconst c-identifier-key
  csharp (concat "\\([[:alpha:]_][[:alnum:]_]*\\)" ; 1
                 "\\("
                 "[ \t\n\r\f\v]*"
                 "\\(\\.\\)"             ;;(c-lang-const c-opt-identifier-concat-key)
                 "[ \t\n\r\f\v]*"
                 "\\(\\([[:alpha:]_][[:alnum:]_]*\\)\\)"
                 "\\)*"))

;; C# has a few rules that are slightly different than Java for
;; operators. This also removed the Java's "super" and replaces it
;; with the C#'s "base".
(c-lang-defconst c-operators
  csharp `((prefix "base")))


;; C# uses CPP-like prefixes to mark #define, #region/endregion,
;; #if/else/endif, and #pragma.  This regexp matches the prefix, not
;; including the beginning-of-line (BOL), and not including the term
;; after the prefix (define, pragma, region, etc).  This regexp says
;; whitespace, followed by the prefix, followed by maybe more
;; whitespace.

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")


;; there are no message directives in C#
(c-lang-defconst c-cpp-message-directives
  csharp nil)

(c-lang-defconst c-cpp-expr-directives
  csharp '("if"))

(c-lang-defconst c-opt-cpp-macro-define
  csharp "define")

;; $ is not a legal char in an identifier in C#.  So we need to
;; create a csharp-specific definition of this constant.
(c-lang-defconst c-symbol-chars
  csharp (concat c-alnum "_"))

;; c-identifier-syntax-modifications by default defines $ as a word
;; syntax, which is not legal in C#.  So, define our own lang-specific
;; value.
(c-lang-defconst c-identifier-syntax-modifications
  csharp '((?_ . "w")))



(c-lang-defconst c-colon-type-list-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-block-prefix-disallowed-chars

  ;; Allow ':' for inherit list starters.
  csharp (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                            '(?: ?,)))


(c-lang-defconst c-assignment-operators
  csharp '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

(c-lang-defconst c-primitive-type-kwds
  ;; ECMA-344, S8
  csharp '("object" "string" "sbyte" "short" "int" "long" "byte"
           "ushort" "uint" "ulong" "float" "double" "bool" "char"
           "decimal" "void"))

;; The keywords that define that the following is a type, such as a
;; class definition.
(c-lang-defconst c-type-prefix-kwds
  ;; ECMA-344, S?
  csharp '("class" "interface" "struct"))  ;; no enum here.
;; we want enum to be a brace list.


;; Type modifier keywords. They appear anywhere in types, but modify
;; instead of create one.
(c-lang-defconst c-type-modifier-kwds
  ;; EMCA-344, S?
  csharp '("readonly" "const" "volatile" "new"))


;; Tue, 20 Apr 2010  16:02
;; need to verify that this works for lambdas...
(c-lang-defconst c-special-brace-lists
  csharp '((?{ . ?}) ))



;; dinoch
;; Thu, 22 Apr 2010  18:54
;;
;; No idea why this isn't getting set properly in the first place.
;; In cc-langs.el, it is set to the union of a bunch of things, none
;; of which include "new", or "enum".
;;
;; But somehow both of those show up in the resulting derived regexp.
;; This breaks indentation of instance initializers, such as
;;
;;         var x = new Foo { ... };
;;
;; Based on my inspection, the existing c-lang-defconst should work!
;; I don't know how to fix this c-lang-defconst, so I am re-setting this
;; variable here, to provide the regex explicitly.
;;
(c-lang-defconst c-decl-block-key
  csharp '"\\(namespace\\)\\([^[:alnum:]_]\\|$\\)\\|\\(class\\|interface\\|struct\\)\\([^[:alnum:]_]\\|$\\)" )


;; Thu, 22 Apr 2010  14:29
;; I want this to handle    var x = new Foo[] { ... };
;; not sure if necessary.
(c-lang-defconst c-inexpr-brace-list-kwds
  csharp '("new"))


;; ;;(c-lang-defconst c-inexpr-class-kwds
;; ;; csharp '("new"))



(c-lang-defconst c-class-decl-kwds
  ;; EMCA-344, S?
  ;; don't include enum here, because we want it to be fontified as a brace
  ;; list, with commas delimiting the values. see c-brace-list-decl-kwds
  ;; below.
  csharp '("class" "interface" "struct" ))  ;; no "enum"!!


;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  csharp '("public" "partial" "private" "const" "abstract" "sealed"
           "protected" "ref" "out" "static" "virtual"
           "implicit" "explicit" "fixed"
           "override" "params" "internal" "async" "extern" "unsafe"))


;; Thu, 22 Apr 2010  23:02
;; Based on inspection of the cc-mode code, the c-protection-kwds
;; c-lang-const is used only for objective-c.  So the value is
;; irrelevant for csharp.
(c-lang-defconst c-protection-kwds
  csharp nil
  ;; csharp '("private" "protected" "public" "internal")
  )

(c-lang-defconst c-opt-op-identifier-prefix
  "Regexp matching the token before the ones in
`c-overloadable-operators' when operators are specified in their \"identifier form\".

This regexp is assumed to not match any non-operator identifier."
  csharp (c-make-keywords-re t '("operator")))

;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  csharp '("struct" "class" "interface" "is" "as" "operator"
           "delegate" "event" "set" "get" "add" "remove"))

;; Handle typeless variable declaration
(c-lang-defconst c-typeless-decl-kwds
  csharp '("var"))

;; Sets up the enum to handle the list properly, and also the new
;; keyword to handle object initializers.  This requires a modified
;; c-basic-matchers-after (see above) in order to correctly fontify C#
;; 3.0 object initializers.
(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))


;; Statement keywords followed directly by a substatement.
;; catch is not one of them, because catch has a paren (typically).
(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "else" "try" "finally"))


;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "using"
           "fixed"
           "checked" "unchecked" "lock"))


;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  csharp '("return" "continue" "break" "throw" "goto" ))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  csharp nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  csharp '("true" "false" "null" "value"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base" "operator"))

;; Treat namespace as an outer block so class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  csharp '("namespace"))

(c-lang-defconst c-other-kwds
  csharp '("sizeof" "typeof" "is" "as" "yield"
           "where" "select" "in" "from" "let" "orderby" "ascending" "descending"
           "await" "async"))

(c-lang-defconst c-overloadable-operators
  ;; EMCA-344, S14.2.1
  csharp '("+" "-" "*" "/" "%" "&" "|" "^"
           "<<" ">>" "==" "!=" ">" "<" ">=" "<="))


;; This c-cpp-matchers stuff is used for fontification.
;; see cc-font.el
;;

;; There's no preprocessor in C#, but there are still compiler
;; directives to fontify: "#pragma", #region/endregion, #define, #undef,
;; #if/else/endif.  (The definitions for the extra keywords above are
;; enough to incorporate them into the fontification regexps for types
;; and keywords, so no additional font-lock patterns are required for
;; keywords.)

(c-lang-defconst c-cpp-matchers
  csharp (cons
          ;; Use the eval form for `font-lock-keywords' to be able to use
          ;; the `c-preprocessor-face-name' variable that maps to a
          ;; suitable face depending on the (X)Emacs version.
          '(eval . (list "^\\s *#\\s *\\(pragma\\|undef\\|define\\)\\>\\(.*\\)"
                         (list 1 c-preprocessor-face-name)
                         '(2 font-lock-string-face)))
          ;; There are some other things in `c-cpp-matchers' besides the
          ;; preprocessor support, so include it.
          (c-lang-const c-cpp-matchers)))


;; allow strings as switch-case values by leaving out string
;; delimiters in this definition
(c-lang-defconst c-nonlabel-token-key
  csharp (c-make-keywords-re t
           (cl-set-difference (c-lang-const c-keywords)
                              (append (c-lang-const c-label-kwds)
                                      (c-lang-const c-protection-kwds))
                              :test 'string-equal)))

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal highlighting for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")


(defvar csharp-mode-syntax-table nil
  "Syntax table used in ‘csharp-mode’ buffers.")
(or csharp-mode-syntax-table
    (setq csharp-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table csharp))))

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in ‘csharp-mode’ buffers.")
(c-define-abbrev-table 'csharp-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar csharp-mode-map (let ((map (c-make-inherited-keymap)))
                          ;; Add bindings which are only useful for C#
                          map)
  "Keymap used in ‘csharp-mode’ buffers.")


;; TODO
;; Defines our constant for finding attributes.
;;(defconst csharp-attribute-regex "\\[\\([XmlType]+\\)(")
;;(defconst csharp-attribute-regex "\\[\\(.\\)")
;; This doesn't work because the string regex happens before this point
;; and getting the font-locking to work before and after is fairly difficult
;;(defconst csharp-attribute-regex
;;  (concat
;;   "\\[[a-zA-Z][ \ta-zA-Z0-9.]+"
;;   "\\((.*\\)?"
;;))


;; ==================================================================
;; end of c# values for "language constants" defined in cc-langs.el
;; ==================================================================


;; ========================================================================
;; moving

;; alist of regexps for various structures in a csharp source file.
(defconst csharp--regexp-alist
  (list
   `(func-start
     ,(concat
       "^[ \t\n\r\f\v]*"                            ;; leading whitespace
       "\\("
       "public\\(?: static\\)?\\|"                  ;; 1. access modifier
       "private\\(?: static\\)?\\|"
       "protected\\(?: internal\\)?\\(?: static\\)?\\|"
       "static\\|"
       "\\)"
       "[ \t\n\r\f\v]+"
       "\\(?:override[ \t\n\r\f\v]+\\)?"            ;; optional
       "\\([[:alpha:]_][^\t\(\n]+\\)"               ;; 2. return type - possibly generic
       "[ \t\n\r\f\v]+"
       "\\("                                        ;; 3. begin name of func
       "\\(?:[A-Za-z_][[:alnum:]_]*\\.\\)*"         ;; possible prefix interface
       "[[:alpha:]_][[:alnum:]_]*"                  ;; actual func name
       "\\(?:<\\(?:[[:alpha:]][[:alnum:]]*\\)\\(?:[, ]+[[:alpha:]][[:alnum:]]*\\)*>\\)?"  ;; (with optional generic type parameter(s)
       "\\)"                                        ;; 3. end of name of func
       "[ \t\n\r\f\v]*"
       "\\(\([^\)]*\)\\)"                           ;; 4. params w/parens
       "\\(?:[ \t]*/[/*].*\\)?"                     ;; optional comment at end of line
       "[ \t\n\r\f\v]*"
       ))

   `(class-start
     ,(concat
       "^[ \t]*"                                    ;; leading whitespace
       "\\("
       "public\\(?: \\(?:static\\|sealed\\)\\)?[ \t]+\\|"  ;; access modifiers
       "internal\\(?: \\(?:static\\|sealed\\)\\)?[ \t]+\\|"
       "static\\(?: internal\\)?[ \t]+\\|"
       "sealed\\(?: internal\\)?[ \t]+\\|"
       "static[ \t]+\\|"
       "sealed[ \t]+\\|"
       "\\)"
       "\\(\\(?:partial[ \t]+\\)?class\\|struct\\)" ;; class/struct keyword
       "[ \t]+"
       "\\([[:alpha:]_][[:alnum:]]*\\)"             ;; type name
       "\\("
       "[ \t\n]*:[ \t\n]*"                          ;; colon
       "\\([[:alpha:]_][^\t\(\n]+\\)"               ;; base / intf - poss generic
       "\\("
       "[ \t\n]*,[ \t\n]*"
       "\\([[:alpha:]_][^\t\(\n]+\\)"               ;; addl interface - poss generic
       "\\)*"
       "\\)?"                                       ;; possibly
       "[ \t\n\r\f\v]*"
       ))

   `(namespace-start
     ,(concat
       "^[ \t\f\v]*"                                ;; leading whitespace
       "\\(namespace\\)"
       "[ \t\n\r\f\v]+"
       "\\("
       "\\(?:[A-Za-z_][[:alnum:]_]*\\.\\)*"          ;; name of namespace
       "[A-Za-z_][[:alnum:]]*"
       "\\)"
       "[ \t\n\r\f\v]*"
       ))

   ))


(defun csharp--regexp (symbol)
  "Retrieve a regexp from `csharp--regexp-alist' corresponding to SYMBOL."
  (let ((elt (assoc symbol csharp--regexp-alist)))
    (if elt (cadr elt) nil)))


(defun csharp-move-back-to-beginning-of-block ()
  "Move to the previous open curly."
  (interactive)
  (re-search-backward "{" (point-min) t))


(defun csharp--move-back-to-beginning-of-something (must-match &optional must-not-match)
  "Move back to the open-curly that begin *something*.
*something* is defined by MUST-MATCH, a regexp which must match
immediately preceding the curly.  If MUST-NOT-MATCH is non-nil,
it is treated as a regexp that must not match immediately
preceding the curly.

This is a helper fn for `csharp-move-back-to-beginning-of-defun' and
`csharp-move-back-to-beginning-of-class'"
  (interactive)
  (let (done
        (found (point))
        (need-to-backup (not (looking-at "{"))))
    (while (not done)
      (if need-to-backup
          (setq found (csharp-move-back-to-beginning-of-block)))
      (if found
          (setq done (and (looking-back must-match nil)
                          (or (not must-not-match)
                              (not (looking-back must-not-match nil))))
                need-to-backup t)
        (setq done t)))
    found))



(defun csharp-move-back-to-beginning-of-defun ()
  "Move back to the open-curly that start the enclosing method.
If point is outside a method, then move back to the
beginning of the prior method.

See also, `csharp-move-fwd-to-end-of-defun'."
  (interactive)
  (cond

   ((bobp) nil)

   (t
    (let (found)
      (save-excursion
        ;; handle the case where we're at the top of a fn now.
        ;; if the user is asking to move back, then obviously
        ;; he wants to move back to a *prior* defun.
        (if (and (looking-at "{")
                 (looking-back (csharp--regexp 'func-start) nil)
                 (not (looking-back (csharp--regexp 'namespace-start) nil)))
            (forward-char -1))

        ;; now do the real work
        (setq found (csharp--move-back-to-beginning-of-something
                     (csharp--regexp 'func-start)
                     (csharp--regexp 'namespace-start))))
      (if found
          (goto-char found))))))


(defun csharp--on-defun-open-curly-p ()
  "Return t when point is on the open-curly of a method."
  (and (looking-at "{")
       (not (looking-back (csharp--regexp 'class-start) nil))
       (not (looking-back (csharp--regexp 'namespace-start) nil))
       (looking-back (csharp--regexp 'func-start) nil)))


(defun csharp--on-class-open-curly-p ()
  "Return t when point is on the open-curly of a class."
  (and (looking-at "{")
       (not (looking-back (csharp--regexp 'namespace-start) nil))
       (looking-back (csharp--regexp 'class-start) nil)))


(defun csharp-move-fwd-to-end-of-defun ()
  "Move forward to the close-curly that ends the enclosing method.
If point is outside a method, moves forward to the close-curly that
defines the end of the next method.

See also, `csharp-move-back-to-beginning-of-defun'."
  (interactive)

  (let ((really-move
         (lambda ()
           (let ((start (point))
                 dest-char)
             (save-excursion
               (csharp-move-back-to-beginning-of-defun)
               (forward-sexp)
               (if (>= (point) start)
                   (setq dest-char (point))))
             (if dest-char
                 (goto-char dest-char))))))

    (cond

     ;; case 1: end of buffer.  do nothing.
     ((eobp) nil)

     ;; case 2: we're at the top of a class
     ((csharp--on-class-open-curly-p)
      (let (found-it)
        (save-excursion
          (forward-char 1) ;; get off the curly
          (setq found-it
                (and ;; look for next open curly
                 (re-search-forward "{" (point-max) t)
                 (funcall really-move))))
        (if found-it
            (goto-char found-it))))


     ;; case 3: we're at the top of a fn now.
     ((csharp--on-defun-open-curly-p)
      (forward-sexp))


     ;; case 4: we're at the bottom of a fn now (possibly
     ;; after just calling csharp-move-fwd-to-end-of-defun.
     ((and (looking-back "}" nil)
           (save-excursion
             (forward-sexp -1)
             (csharp--on-defun-open-curly-p)))

      (let (found-it)
        (save-excursion
          (setq found-it
                (and (re-search-forward "{" (point-max) t)
                     (funcall really-move))))
        (if found-it
            (goto-char found-it))))


     ;; case 5: we're at none of those places.
     (t
      (funcall really-move)))))




(defun csharp-move-back-to-beginning-of-class ()
  "Move back to the open-curly that begin the enclosing class.
If point is outside a class, then move back to the
beginning of the prior class.

See also, `csharp-move-fwd-to-end-of-defun'."
  (interactive)

  (cond
   ((bobp) nil)

   (t
    (let (found)
      (save-excursion
        ;; handle the case where we're at the top of a class now.
        ;; if the user is asking to move back, then obviously
        ;; he wants to move back to a *prior* defun.
        (if (and (looking-at "{")
                 (looking-back (csharp--regexp 'class-start) nil)
                 (not (looking-back (csharp--regexp 'namespace-start) nil)))
            (forward-char -1))

        ;; now do the real work
        (setq found (csharp--move-back-to-beginning-of-something
                     (csharp--regexp 'class-start)
                     (csharp--regexp 'namespace-start))))
      (if found
          (goto-char found))))))




(defun csharp-move-fwd-to-end-of-class ()
  "Move forward to the close-curly that ends the enclosing class.

See also, `csharp-move-back-to-beginning-of-class'."
  (interactive)
  (let ((start (point))
        dest-char)
    (save-excursion
      (csharp-move-back-to-beginning-of-class)
      (forward-sexp)
      (if (>= (point) start)
          (setq dest-char (point))))

    (if dest-char
        (goto-char dest-char))))



(defun csharp-move-back-to-beginning-of-namespace ()
  "Move back to the open-curly that begins the enclosing namespace.
If point is outside a namespace, then move back
to the beginning of the prior namespace."
  (interactive)
  (cond

   ((bobp) nil)

   (t
    (let (found)
      (save-excursion
        ;; handle the case where we're at the top of a namespace now.
        ;; if the user is asking to move back, then obviously
        ;; he wants to move back to a *prior* defun.
        (if (and (looking-at "{")
                 (looking-back (csharp--regexp 'namespace-start) nil))
            (forward-char -1))

        ;; now do the real work
        (setq found (csharp--move-back-to-beginning-of-something
                     (csharp--regexp 'namespace-start))))
      (if found
          (goto-char found))))))

;; moving
;; ========================================================================




;; ==================================================================
;;; imenu stuff

(defconst csharp--imenu-expression
  (let* ((single-space                   "[ \t\n\r\f\v]")
         (optional-space                 (concat single-space "*"))
         (bol                            "^[ \t]*") ;; BOL shouldn't accept lineshift.
         (space                          (concat single-space "+"))
         (access-modifier (regexp-opt '( "public" "private" "protected" "internal"
                                         "static" "sealed" "partial" "override" "virtual"
                                         "abstract" "async" "new" "unsafe")))
         ;; this will allow syntactically invalid combinations of modifiers
         ;; but that's a compiler problem, not a imenu-problem
         (access-modifier-list           (concat "\\(?:" access-modifier space "\\)"))
         (access-modifiers (concat access-modifier-list "*"))
         (basic-type                     (concat
                                          ;; typename
                                          "\\(?:[A-Za-z_][[:alnum:]_]*\\.\\)*"
                                          "[A-Za-z_][[:alnum:]_]*"
                                          ))
         (type                           (concat
                                          basic-type
                                          ;; simplified, optional generic constraint.
                                          ;; handles generic sub-types.
                                          "\\(?:<[[:alnum:],<> \t\n\f\v\r]+>\\)?"))
         (return-type                    (concat
                                          type
                                          ;; optional array-specifier
                                          "\\(?:\\[\\]\\)?"))
         (interface-prefix               (concat "\\(?:" type "\\.\\)"))
         ;; param-list with parens
         (parameter-list "\\(?:\([^!\)]*\)\\)")
         (inheritance-clause (concat "\\(?:"
                                     optional-space
                                     ":"
                                     optional-space type
                                     "\\(?:" optional-space "," optional-space type "\\)*"
                                     "\\)?")))

    (list (list "namespace"
                (concat bol "namespace" space
                        "\\(" basic-type "\\)") 1)
          ;; not all these are classes, but they can hold other
          ;; members, so they are treated uniformly.
          (list "class"
                (concat bol
                        access-modifiers
                        "\\("
                        (regexp-opt '("class" "struct" "interface")) space
                        type inheritance-clause "\\)")  1)
          (list "enum"
                (concat bol
                        access-modifiers
                        "\\(" "enum" space
                        basic-type "\\)")  1)
          (list "ctor"
                (concat bol
                        ;; ctor MUST have access modifiers, or else we pick
                        ;; every if statement in the file...
                        access-modifier-list "+"
                        "\\("
                        basic-type
                        optional-space
                        parameter-list
                        "\\)"
                        "\\(?:"
                        optional-space
                        ":"
                        optional-space
                        "\\(?:this\\|base\\)"
                        optional-space
                        parameter-list
                        "\\)?"
                        optional-space "{") 1)
          (list "method"
                (concat bol
                        ;; we MUST require modifiers, or else we cannot reliably
                        ;; identify declarations, without also dragging in lots of
                        ;; if statements and what not.
                        access-modifier-list "+"
                        return-type space
                        "\\("
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        ;; optional // or /* comment at end
                        "\\(?:[ \t]*/[/*].*\\)?"
                        optional-space
                        "{") 1)
          (list "method-inf"
                (concat bol
                        return-type space
                        "\\("
                        interface-prefix
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        ;; optional // or /* comment at end
                        "\\(?:[ \t]*/[/*].*\\)?"
                        optional-space
                        "{") 1)
          (list "method-abs-ext"
                (concat bol
                        access-modifier-list "+"
                        (regexp-opt '("extern" "abstract")) space
                        return-type space
                        "\\("
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        optional-space
                        ;; abstract/extern methods are terminated with ;
                        ";") 1)
          ;; delegates are almost like abstract methods, so pick them up here
          (list "delegate"
                (concat bol
                        access-modifiers
                        "delegate" space
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space
                        parameter-list
                        ;; optional // or /* comment at end
                        optional-space
                        ";") 1)
          (list "prop"
                (concat bol
                        ;; must require access modifiers, or else we
                        ;; pick up pretty much anything.
                        access-modifiers
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))
                        ) 1)
          (list "prop-inf"
                (concat bol
                        return-type space
                        "\\("
                        interface-prefix
                        type
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))
                        ) 1)
          ;; adding fields... too much?
          (list "field"
                (concat bol
                        access-modifier-list "+"
                        ;; fields can be readonly/const/volatile
                        "\\(?:" (regexp-opt '("readonly" "const" "volatile")) space "\\)?"
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space
                        ;; optional assignment
                        "\\(?:=[^;]+\\)?"
                        ";") 1)
          (list "indexer"
                (concat bol
                        access-modifiers
                        return-type space
                        "this" optional-space
                        "\\("
                        ;; opening bracket
                        "\\[" optional-space
                        ;; type
                        "\\([^\]]+\\)" optional-space
                        type
                        ;; closing brackets
                        "\\]"
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))) 1)
          (list "event"
                (concat bol
                        access-modifier-list "+"
                        optional-space "event" optional-space
                        "\\("
                        return-type space
                        type
                        "\\)"
                        optional-space
                        ";") 1))))

(defun csharp--imenu-get-pos (pair)
  "Return `position' from a (title . position) cons-pair `PAIR'.

   The position may be a integer, or a marker (as returned by
   imenu-indexing).  This function ensures what is returned is an
   integer which can be used for easy comparison."
  (let ((pos (cdr pair)))
    (if (markerp pos)
        (marker-position pos)
      pos)))

(defun csharp--imenu-get-container (item containers previous)
  "Return the container which `ITEM' belongs to.

   `ITEM' is a (title . position) cons-pair.  `CONTAINERS' is a
   list of such.  `PREVIOUS' is the name of the previous
   container found when recursing through `CONTAINERS'.

   The final result is based on item's position relative to those
   found in `CONTAINERS', or nil if none is found."
  (if (not containers)
      previous
    (let* ((item-pos (csharp--imenu-get-pos item))
           (container (car containers))
           (container-pos (csharp--imenu-get-pos container))
           (rest      (cdr containers)))
      (if (and container-pos
               (< item-pos container-pos))
          previous
        (csharp--imenu-get-container item rest container)))))

(defun csharp--imenu-get-container-name (item containers)
  "Return the name of the container which `ITEM' belongs to.

   `ITEM' is a (title . position) cons-pair.
   `CONTAINERS' is a list of such.

   The name is based on the results from
   `csharp--imenu-get-container'."
  (let ((container (csharp--imenu-get-container item containers nil)))
    (if (not container)
        nil
      (let ((container-p1 (car (split-string (car container))))   ;; namespace
            (container-p2 (cadr (split-string (car container))))) ;; class/interface
        ;; use p1 (namespace) when there is no p2
        (if container-p2
            container-p2
          container-p1)))))

(defun csharp--imenu-sort (items)
  "Sort an imenu-index list `ITEMS' by the string-portion."
  (sort items (lambda (item1 item2)
                (string< (car item1) (car item2)))))

(defun csharp--imenu-get-class-name (class namespaces)
  "Gets a name for a imenu-index `CLASS'.

   Result is based on its own name and `NAMESPACES' found in the same file."
  (let ((namespace (csharp--imenu-get-container-name class namespaces))
        (class-name (car class)))
    (if (not namespace)
        class-name
      ;; reformat to include namespace
      (let* ((words (split-string class-name))
             (type  (car words))
             (name  (cadr words)))
        (concat type " " namespace "." name)))))

(defun csharp--imenu-get-class-nodes (classes namespaces)
  "Create a new alist with CLASSES as root nodes with NAMESPACES added.

   Each class will have one imenu index-entry \"( top)\" added by
   default."

  (mapcar (lambda (class)
            (let ((class-name (csharp--imenu-get-class-name class namespaces))
                  (class-pos  (cdr class)))
              ;; construct a new alist-entry where value is itself
              ;; a list of alist-entries with -1- entry which the top
              ;; of the class itself.
              (cons class-name
                    (list
                     (cons "( top )" class-pos)))))
          classes))

(defun csharp--imenu-get-class-node (result item classes namespaces)
  "Get the class-node in `RESULT' which an `ITEM' should be inserted into.

   For this calculation, the original index items `CLASSES' and `NAMESPACES'
   is needed."
  (let* ((class-item (csharp--imenu-get-container item classes nil))
         (class-name (csharp--imenu-get-class-name class-item namespaces)))
    (assoc class-name result)))

(defun csharp--imenu-format-item-node (item type)
  "Format an ITEM with a specified TYPE as an imenu item to be inserted into the index."
  (cons
   (concat "(" type ") " (car item))
   (cdr item)))

(defun csharp--imenu-append-items-to-menu (result key name index classes namespaces)
  "Formats the imenu-index using the provided values.

This is done by modifying the contents of `RESULT' in place."
  ;; items = all methods, all events, etc based on "type"
  (let* ((items (cdr (assoc key index))))
    (dolist (item items)
      (let ((class-node (csharp--imenu-get-class-node result item classes namespaces))
            (item-node  (csharp--imenu-format-item-node item name)))
        (nconc class-node (list item-node))))))

(defun csharp--imenu-transform-index (index)
  "Transform an imenu INDEX based on `IMENU-GENERIC-EXPRESSION'.

  The resulting structure should be based on full type-names, with
  type-members nested hierarchially below its parent.

  See `csharp-mode-tests.el' for examples of expected behaviour
  of such transformations."
  (let* ((result nil)
         (namespaces (cdr (assoc "namespace" index)))
         (classes    (cdr (assoc "class"     index)))
         (class-nodes (csharp--imenu-get-class-nodes classes namespaces)))
    ;; be explicit about collection variable
    (setq result class-nodes)
    (dolist (type '(("ctor")
                    ("method")
                    ("method-inf" "method")
                    ("method-abs-ext" "method")
                    ("prop")
                    ("prop-inf" "prop")
                    ("field")
                    ("event")
                    ("indexer")))
      (let* ((key (car type))
             (name (car (last type))))
        (csharp--imenu-append-items-to-menu result key name index classes namespaces)))

    ;; add enums and delegates to main result list, as own items.
    ;; We don't support nested types. EOS.
    ;;
    ;; This has the issue that they get reported as "function" in
    ;; `helm-imenu', but there's nothing we can do about that.
    ;; The alternative is making it a menu with -1- submenu which
    ;; says "( top )" but that will be very clicky...

    ;; before adding delegates, we need to pad the entry so that it
    ;; matches the "<type> <name>" signature used by all the other
    ;; imenu entries
    (let ((delegates (cdr (assoc "delegate" index))))
      (dolist (delegate delegates)
        (setf (car delegate) (concat "delegate " (car delegate)))))

    (dolist (type '("enum" "delegate"))
      (dolist (item (cdr (assoc type index)))
        (let ((item-name (csharp--imenu-get-class-name item namespaces)))
          (setq result (cons (cons item-name (cdr item))
                             result)))))

    ;; sort individual sub-lists
    (dolist (item result)
      (when (listp (cdr item))
        (setf (cdr item) (csharp--imenu-sort (cdr item)))))

    ;; sort main list
    ;; (Enums always sort last though, because they don't have
    ;; sub-menus)
    (csharp--imenu-sort result)))

(defun csharp--imenu-create-index-function ()
  "Create an imenu index."
  (csharp--imenu-transform-index
   (imenu--generic-function csharp--imenu-expression)))

(defun csharp--setup-imenu ()
  "Set up `imenu' for `csharp-mode'."

  ;; There are two ways to do imenu indexing. One is to provide a
  ;; function, via `imenu-create-index-function'.  The other is to
  ;; provide imenu with a list of regexps via
  ;; `imenu-generic-expression'; imenu will do a "generic scan" for you.
  ;;
  ;; We use both.
  ;;
  ;; First we use the `imenu-generic-expression' to build a index for
  ;; us, but we do so inside a `imenu-create-index-function'
  ;; implementation which allows us to tweak the results slightly
  ;; before returning it to Emacs.
  (setq imenu-create-index-function #'csharp--imenu-create-index-function)
  (imenu-add-menubar-index))





;; ==================================================================
;; C# code-doc insertion magic
;; ==================================================================
;;
;; In Visual Studio, if you type three slashes, it immediately expands into
;; an inline code-documentation fragment.  The following method does the
;; same thing.
;;
;; This is the kind of thing that could be handled by YASnippet or
;; another similarly flexible snippet framework. But I don't want to
;; introduce a dependency on yasnippet to csharp-mode. So the capability
;; must live within csharp-mode itself.

(defun csharp-maybe-insert-codedoc (arg)
  "Insert an xml code documentation template on third consecutive slash.
This fn gets bound to / (the slash key), in
‘csharp-mode’.  If the slash being inserted is not the third
consecutive slash, the slash is inserted as normal.  If it is the
third consecutive slash, then a xml code documentation template
may be inserted in some cases.  For example,

  a <summary> template is inserted if the prior line is empty,
        or contains only an open curly brace;
  a <remarks> template is inserted if the prior word
        closes the <summary> element;
  a <returns> template is inserted if the prior word
        closes the <remarks> element;
  an <example> template is inserted if the prior word closes
        the <returns> element;
  a <para> template is inserted if the prior word closes
        a <para> element.

In all other cases the slash is inserted as normal.

The prefix argument ARG is passed on to `self-insert-command'
when the code documentation template isn't triggered.  This makes
sure that M-10 / still produces 10 consecutive slashes as expected.

If you want the default cc-mode behavior, which implies no automatic
insertion of xml code documentation templates, then use this in
your `csharp-mode-hook' function:

     (local-set-key (kbd \"/\") 'c-electric-slash)"
  (interactive "*p")
  ;;(message "csharp-maybe-insert-codedoc")
  (let (
        (cur-point (point))
        (char last-command-event)
        (cb0 (char-before (- (point) 0)))
        (cb1 (char-before (- (point) 1)))
        is-first-non-whitespace
        did-auto-insert
        )

    ;; check if two prior chars were slash, in other words,
    ;; check if this is the third slash in a row.
    (if (and (= char ?/) cb0 (= ?/ cb0) cb1 (= ?/ cb1))

        (progn
          ;;(message "yes - this is the third consecutive slash")
          (setq is-first-non-whitespace
                (save-excursion
                  (back-to-indentation)
                  (= cur-point (+ (point) 2))))

          (if is-first-non-whitespace
              ;; This is a 3-slash sequence.  It is the first non-whitespace text
              ;; on the line. Now we need to examine the surrounding context
              ;; in order to determine which xml cod doc template to insert.
              (let (word-back char0 char1
                              word-fore char-0 char-1
                              text-to-insert         ;; text to insert in lieu of slash
                              fn-to-call     ;; func to call after inserting text
                              (preceding-line-is-empty (or
                                                        (= (line-number-at-pos) 1)
                                                        (save-excursion
                                                          (forward-line -1)
                                                          (beginning-of-line)
                                                          (looking-at "[ \t]*$\\|[ \t]*{[ \t]*$"))))
                              (flavor 0) ;; used only for diagnostic purposes
                              )

                ;;(message "starting a 3-slash comment")
                ;; get the prior word, and the 2 chars preceding it.
                (backward-word)

                (setq word-back (thing-at-point 'word)
                      char0 (char-before (- (point) 0))
                      char1 (char-before (- (point) 1)))

                ;; restore prior position
                (goto-char cur-point)

                ;; get the following word, and the 2 chars preceding it.
                (forward-word)
                (backward-word)
                (setq word-fore (thing-at-point 'word)
                      char-0 (char-before (- (point) 0))
                      char-1 (char-before (- (point) 1)))

                ;; restore prior position again
                (goto-char cur-point)

                (cond
                 ;; The preceding line is empty, or all whitespace, or
                 ;; contains only an open-curly.  In this case, insert a
                 ;; summary element pair.
                 (preceding-line-is-empty
                  (setq text-to-insert  "/ <summary>\n ///   \n /// </summary>"
                        flavor 1) )

                 ;; The preceding word closed a summary element.  In this case,
                 ;; if the forward word does not open a remarks element, then
                 ;; insert a remarks element.
                 ((and (string-equal word-back "summary") (eq char0 ?/)  (eq char1 ?<))
                  (if (not (and (string-equal word-fore "remarks") (eq char-0 ?<)))
                      (setq text-to-insert "/ <remarks>\n ///   <para>\n ///     \n ///   </para>\n /// </remarks>"
                            flavor 2)))

                 ;; The preceding word closed the remarks section.  In this case,
                 ;; insert an example element.
                 ((and (string-equal word-back "remarks")  (eq char0 ?/)  (eq char1 ?<))
                  (setq text-to-insert "/ <example>\n ///   \n /// </example>"
                        flavor 3))

                 ;; The preceding word closed the example section.  In this
                 ;; case, insert an returns element.  This isn't always
                 ;; correct, because sometimes the xml code doc is attached to
                 ;; a class or a property, neither of which has a return
                 ;; value. A more intelligent implementation would inspect the
                 ;; syntax state and only inject a returns element if
                 ;; appropriate.
                 ((and (string-equal word-back "example")  (eq char0 ?/)  (eq char1 ?<))
                  (setq text-to-insert "/ <returns></returns>"
                        fn-to-call (lambda ()
                                     (backward-word)
                                     (backward-char)
                                     (backward-char)
                                     (c-indent-line-or-region)
                                     )
                        flavor 4))

                 ;; The preceding word opened the remarks section, or it
                 ;; closed a para section. In this case, insert a para
                 ;; element, using appropriate indentation with respect to the
                 ;; prior tag.
                 ((or
                   (and (string-equal word-back "remarks")  (eq char0 ?<)  (or (eq char1 32) (eq char1 9)))
                   (and (string-equal word-back "para")     (eq char0 ?/)  (eq char1 ?<)))

                  (let (prior-point spacer)
                    (save-excursion
                      (backward-word)
                      (backward-char)
                      (backward-char)
                      (setq prior-point (point))
                      (skip-chars-backward "\t ")
                      (setq spacer (buffer-substring (point) prior-point))
                      ;;(message (format "pt(%d) prior(%d) spacer(%s)" (point) prior-point spacer))
                      )

                    (if (string-equal word-back "remarks")
                        (setq spacer (concat spacer "   ")))

                    (setq text-to-insert (format "/%s<para>\n ///%s  \n ///%s</para>"
                                                 spacer spacer spacer)
                          flavor 6)))

                 ;; The preceding word opened a para element.  In this case, if
                 ;; the forward word does not close the para element, then
                 ;; close the para element.
                 ;; --
                 ;; This is a nice idea but flawed.  Suppose I have a para element with some
                 ;; text in it. If I position the cursor at the first line, then type 3 slashes,
                 ;; I get a close-element, and that would be inappropriate.  Not sure I can
                 ;; easily solve that problem, so the best thing might be to simply punt, and
                 ;; require people to close their own elements.
                 ;;
                 ;;              ( (and (string-equal word-back "para")  (eq char0 60)  (or (eq char1 32) (eq char1 9)))
                 ;;                (if (not (and (string-equal word-fore "para") (eq char-0 47) (eq char-1 60) ))
                 ;;                    (setq text-to-insert "/   \n/// </para>\n///"
                 ;;                          fn-to-call (lambda ()
                 ;;                                       (previous-line)
                 ;;                                       (end-of-line)
                 ;;                                       )
                 ;;                          flavor 7) )
                 ;;                )

                 ;; the default case - do nothing
                 (t nil))

                (if text-to-insert
                    (progn
                      ;;(message (format "inserting special text (f(%d))" flavor))

                      ;; set the flag, that we actually inserted text
                      (setq did-auto-insert t)

                      ;; save point of beginning of insertion
                      (setq cur-point (point))

                      ;; actually insert the text
                      (insert text-to-insert)

                      ;; indent the inserted string, and re-position point, either through
                      ;; the case-specific fn, or via the default progn.
                      (if fn-to-call
                          (funcall fn-to-call)

                        (let ((newline-count 0) (pos 0) ix)

                          ;; count the number of newlines in the inserted string
                          (while (string-match "\n" text-to-insert pos)
                            (setq pos (match-end 0)
                                  newline-count (+ newline-count 1) )
                            )

                          ;; indent what we just inserted
                          (c-indent-region cur-point (point) t)

                          ;; move up n/2 lines. This assumes that the
                          ;; inserted text is ~symmetric about the halfway point.
                          ;; The assumption holds if the xml code doc uses a
                          ;; begin-elt and end-elt on a new line all by themselves,
                          ;; and a blank line in between them where the point should be.
                          ;; A more intelligent implementation would use a specific
                          ;; marker string, like @@DOT, to note the desired point.
                          (forward-line (- 0 (/ newline-count 2)))
                          (end-of-line)))))))))

    (if (not did-auto-insert)
        (self-insert-command (prefix-numeric-value arg)))))

;; ==================================================================
;; end of c# code-doc insertion magic
;; ==================================================================

(defun csharp-time ()
  "Return the time of day as a string.  Used in the `csharp-log' function."
  (substring (current-time-string) 11 19)) ;24-hr time


(defun csharp-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `csharp-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level csharp-log-level)
      (let* ((msg (apply 'format text args)))
        (message "C# %s %s" (csharp-time) msg))))

;; ==================================================================
;; C#-specific optimizations of cc-mode funcs
;; ==================================================================

;; There's never a need to move over an Obj-C directive in csharp-mode.
(defadvice c-forward-objc-directive (around
                                     csharp-mode-advice-2
                                     compile activate)
  "Make `c-forward-objc-directive' a no-op in `csharp-mode'."
  (if (c-major-mode-is 'csharp-mode)
      nil
    ad-do-it)
  )

;; ==================================================================
;; end of C#-specific optimizations of cc-mode funcs
;; ==================================================================








;; ==================================================================
;; c# - monkey-patching of basic parsing logic
;; ==================================================================
;;
;; The following 2 defuns redefine functions from cc-mode, to add
;; special cases for C#.  These primarily deal with indentation of
;; instance initializers, which are somewhat unique to C#.  I couldn't
;; figure out how to get cc-mode to do what C# needs, without modifying
;; these defuns.
;;

;; verabatim copy of c-font-lock-invalid-string before it was removed
;; from emacs/cc-mode in Git commit bb591f139f0602af292c772f974dcc14dabb1deb.

(defun csharp-mode-font-lock-invalid-string ()
  ;; Assuming the point is after the opening character of a string,
  ;; fontify that char with `font-lock-warning-face' if the string
  ;; decidedly isn't terminated properly.
  ;;
  ;; This function does hidden buffer changes.
  (let ((start (1- (point))))
    (save-excursion
      (and (eq (elt (parse-partial-sexp start (c-point 'eol)) 8) start)
	   (if (if (eval-when-compile (integerp ?c))
		   ;; Emacs
		   (integerp c-multiline-string-start-char)
		 ;; XEmacs
		 (characterp c-multiline-string-start-char))
	       ;; There's no multiline string start char before the
	       ;; string, so newlines aren't allowed.
	       (not (eq (char-before start) c-multiline-string-start-char))
	     ;; Multiline strings are allowed anywhere if
	     ;; c-multiline-string-start-char is t.
	     (not c-multiline-string-start-char))
	   (if c-string-escaped-newlines
	       ;; There's no \ before the newline.
	       (not (eq (char-before (point)) ?\\))
	     ;; Escaped newlines aren't supported.
	     t)
	   (c-put-font-lock-face start (1+ start) 'font-lock-warning-face)))))

(advice-add 'c-looking-at-inexpr-block
            :around 'csharp--c-looking-at-inexpr-block-hack)

(defun csharp--c-looking-at-inexpr-block-hack (orig-fun &rest args)
  (apply
   (if (eq major-mode 'csharp-mode)
       #'csharp--c-looking-at-inexpr-block
     orig-fun)
   args))

(defun csharp--c-looking-at-inexpr-block (lim containing-sexp &optional check-at-end)
  ;; Return non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.
  ;;
  ;; LIM limits the backward search.  CONTAINING-SEXP is the start
  ;; position of the closest containing list.  If it's nil, the
  ;; containing paren isn't used to decide whether we're inside an
  ;; expression or not.  If both LIM and CONTAINING-SEXP are used, LIM
  ;; needs to be farther back.
  ;;
  ;; If CHECK-AT-END is non-nil then extra checks at the end of the
  ;; brace block might be done.  It should only be used when the
  ;; construct can be assumed to be complete, i.e. when the original
  ;; starting position was further down than that.
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (let ((res 'maybe) passed-paren
          (closest-lim (or containing-sexp lim (point-min)))
          ;; Look at the character after point only as a last resort
          ;; when we can't disambiguate.
          (block-follows (and (eq (char-after) ?{) (point))))

      (while (and (eq res 'maybe)
                  (progn (c-backward-syntactic-ws)
                         (> (point) closest-lim))
                  (not (bobp))
                  (progn (backward-char)
                         (or (looking-at "[\]\).]\\|\w\\|\\s_")
                             (looking-at ">")))
                  (c-safe (forward-char)
                          (goto-char (scan-sexps (point) -1))))

        (setq res
              (if (looking-at c-keywords-regexp)
                  (let ((kw-sym (c-keyword-sym (match-string 1))))
                    (cond
                     ((and block-follows
                           (c-keyword-member kw-sym 'c-inexpr-class-kwds))
                      (and (not (eq passed-paren ?\[))

                           ;; dinoch Thu, 22 Apr 2010  18:20
                           ;; ============================================
                           ;; looking at new MyType() { ... }
                           ;; means this is a brace list, so, return nil,
                           ;; implying NOT looking-at-inexpr-block
                           (not
                            (and (c-major-mode-is 'csharp-mode)
                                 (looking-at "new[ \t\n\f\v\r]+\\([[:alnum:]_]+\\)\\b")))

                           (or (not (looking-at c-class-key))
                               ;; If the class instantiation is at the start of
                               ;; a statement, we don't consider it an
                               ;; in-expression class.
                               (let ((prev (point)))
                                 (while (and
                                         (= (c-backward-token-2 1 nil closest-lim) 0)
                                         (eq (char-syntax (char-after)) ?w))
                                   (setq prev (point)))
                                 (goto-char prev)
                                 (not (c-at-statement-start-p)))
                               ;; Also, in Pike we treat it as an
                               ;; in-expression class if it's used in an
                               ;; object clone expression.
                               (save-excursion
                                 (and check-at-end
                                      (c-major-mode-is 'pike-mode)
                                      (progn (goto-char block-follows)
                                             (zerop (c-forward-token-2 1 t)))
                                      (eq (char-after) ?\())))
                           (cons 'inexpr-class (point))))
                     ((c-keyword-member kw-sym 'c-inexpr-block-kwds)
                      (when (not passed-paren)
                        (cons 'inexpr-statement (point))))
                     ((c-keyword-member kw-sym 'c-lambda-kwds)
                      (when (or (not passed-paren)
                                (eq passed-paren ?\())
                        (cons 'inlambda (point))))
                     ((c-keyword-member kw-sym 'c-block-stmt-kwds)
                      nil)
                     (t
                      'maybe)))

                (if (looking-at "\\s(")
                    (if passed-paren
                        (if (and (eq passed-paren ?\[)
                                 (eq (char-after) ?\[))
                            ;; Accept several square bracket sexps for
                            ;; Java array initializations.
                            'maybe)
                      (setq passed-paren (char-after))
                      'maybe)
                  'maybe)

                (if (csharp--at-lambda-header)
                    (cons 'inexpr (point))))))

      (if (eq res 'maybe)
          (when (and c-recognize-paren-inexpr-blocks
                     block-follows
                     containing-sexp
                     (eq (char-after containing-sexp) ?\())
            (goto-char containing-sexp)
            (if (or (save-excursion
                      (c-backward-syntactic-ws lim)
                      (and (> (point) (or lim (point-min)))
                           (c-on-identifier)))
                    (and c-special-brace-lists
                         (c-looking-at-special-brace-list)))
                nil
              (cons 'inexpr-statement (point))))

        res))))

(advice-add 'c-inside-bracelist-p
            :around 'csharp-inside-bracelist-or-c-inside-bracelist-p)

(defun csharp-inside-bracelist-or-c-inside-bracelist-p (command &rest args)
  "Run `csharp-inside-bracelist-p' if in `csharp-mode'.

Otherwise run `c-inside-bracelist-p'."
  (if (eq major-mode 'csharp-mode)
      (csharp-inside-bracelist-p (nth 0 args) (nth 1 args))
    (apply command args)))

(defun csharp-inside-bracelist-p (containing-sexp paren-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  PAREN-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; placed in inconvenient locations.  It's a trade-off we make for
  ;; speed.
  ;;
  ;; This function might do hidden buffer changes.
  (or
   ;; This will pick up brace list declarations.
   (c-safe
     (save-excursion
       (goto-char containing-sexp)
       (c-safe (c-forward-sexp -1))
       (let (bracepos)
         (if (and (or (looking-at c-brace-list-key)

                      (progn
                        (c-safe (c-forward-sexp -1))
                        (looking-at c-brace-list-key))

                      (and (c-major-mode-is 'csharp-mode)
                           (or
                            ;; dinoch Thu, 22 Apr 2010  18:20
                            ;; ============================================
                            ;; looking enum Foo : int
                            ;; means this is a brace list, so, return nil,
                            ;; implying NOT looking-at-inexpr-block
                            (progn
                              (c-safe (c-forward-sexp -1))
                              (looking-at csharp-enum-decl-re))

                            ;; type-initializers are not properly detected and
                            ;; indented unless we help out. (no need to forward
                            ;; when looking here, because enum-check already did
                            ;; it!)
                            (looking-at csharp-type-initializer-statement-re))))

                  (setq bracepos (c-down-list-forward (point)))
                  (or
                   (not (c-crosses-statement-barrier-p (point)
                                                       (- bracepos 2)))
                   ;; this little hack (combined with the regexp-check above)
                   ;; fixes indentation for all type-initializers.
                   (c-major-mode-is 'csharp-mode)))
             (point)))))

   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let ((class-key
            ;; Pike can have class definitions anywhere, so we must
            ;; check for the class key here.
            (and (c-major-mode-is 'pike-mode)
                 c-decl-block-key))
           bufpos braceassignp lim next-containing)
       (while (and (not bufpos)
                   containing-sexp)
         (when paren-state
           (if (consp (car paren-state))
               (setq lim (cdr (car paren-state))
                     paren-state (cdr paren-state))
             (setq lim (car paren-state)))
           (when paren-state
             (setq next-containing (car paren-state)
                   paren-state (cdr paren-state))))
         (goto-char containing-sexp)
         (if (c-looking-at-inexpr-block next-containing next-containing)
             ;; We're in an in-expression block of some kind.  Do not
             ;; check nesting.  We deliberately set the limit to the
             ;; containing sexp, so that c-looking-at-inexpr-block
             ;; doesn't check for an identifier before it.
             (setq containing-sexp nil)
           ;; see if the open brace is preceded by = or [...] in
           ;; this statement, but watch out for operator=
           (setq braceassignp 'dontknow)
           (c-backward-token-2 1 t lim)
           ;; Checks to do only on the first sexp before the brace.
           (when (and c-opt-inexpr-brace-list-key
                      (eq (char-after) ?\[))
             ;; In Java, an initialization brace list may follow
             ;; directly after "new Foo[]", so check for a "new"
             ;; earlier.
             (while (eq braceassignp 'dontknow)
               (setq braceassignp
                     (cond ((/= (c-backward-token-2 1 t lim) 0) nil)
                           ((looking-at c-opt-inexpr-brace-list-key) t)
                           ((looking-at "\\sw\\|\\s_\\|[.[]")
                            ;; Carry on looking if this is an
                            ;; identifier (may contain "." in Java)
                            ;; or another "[]" sexp.
                            'dontknow)
                           (t nil)))))
           ;; Checks to do on all sexps before the brace, up to the
           ;; beginning of the statement.
           (while (eq braceassignp 'dontknow)
             (cond ((eq (char-after) ?\;)
                    (setq braceassignp nil))
                   ((and class-key
                         (looking-at class-key))
                    (setq braceassignp nil))
                   ((eq (char-after) ?=)
                    ;; We've seen a =, but must check earlier tokens so
                    ;; that it isn't something that should be ignored.
                    (setq braceassignp 'maybe)
                    (while (and (eq braceassignp 'maybe)
                                (zerop (c-backward-token-2 1 t lim)))
                      (setq braceassignp
                            (cond
                             ;; Check for operator =
                             ((and c-opt-op-identifier-prefix
                                   (looking-at c-opt-op-identifier-prefix))
                              nil)
                             ;; Check for `<opchar>= in Pike.
                             ((and (c-major-mode-is 'pike-mode)
                                   (or (eq (char-after) ?`)
                                       ;; Special case for Pikes
                                       ;; `[]=, since '[' is not in
                                       ;; the punctuation class.
                                       (and (eq (char-after) ?\[)
                                            (eq (char-before) ?`))))
                              nil)
                             ((looking-at "\\s.") 'maybe)
                             ;; make sure we're not in a C++ template
                             ;; argument assignment
                             ((and
                               (c-major-mode-is 'c++-mode)
                               (save-excursion
                                 (let ((here (point))
                                       (pos< (progn
                                               (skip-chars-backward "^<>")
                                               (point))))
                                   (and (eq (char-before) ?<)
                                        (not (c-crosses-statement-barrier-p
                                              pos< here))
                                        (not (c-in-literal))
                                        ))))
                              nil)
                             (t t))))))
             (if (and (eq braceassignp 'dontknow)
                      (/= (c-backward-token-2 1 t lim) 0))
                 (setq braceassignp nil)))
           (if (not braceassignp)
               (if (eq (char-after) ?\;)
                   ;; Brace lists can't contain a semicolon, so we're done.
                   (setq containing-sexp nil)
                 ;; Go up one level.
                 (setq containing-sexp next-containing
                       lim nil
                       next-containing nil))
             ;; we've hit the beginning of the aggregate list
             (c-beginning-of-statement-1
              (c-most-enclosing-brace paren-state))
             (setq bufpos (point))))
         )
       bufpos))
   ))

;; ==================================================================
;; end of monkey-patching of basic parsing logic
;; ==================================================================




;;(easy-menu-define csharp-menu csharp-mode-map "C# Mode Commands"
;;                ;; Can use `csharp' as the language for `c-mode-menu'
;;                ;; since its definition covers any language.  In
;;                ;; this case the language is used to adapt to the
;;                ;; nonexistence of a cpp pass and thus removing some
;;                ;; irrelevant menu alternatives.
;;                (cons "C#" (c-lang-const c-mode-menu csharp)))

;;; Compilation regexps
;; When invoked by MSBuild, csc’s errors look like this:
;; subfolder\file.cs(6,18): error CS1006: Name of constructor must
;; match name of class [c:\Users\user\project.csproj]

(defun csharp--compilation-error-file-resolve ()
  "Resolve an msbuild error to a (filename . dirname) cons cell."
  ;; http://stackoverflow.com/a/18049590/429091
  (cons (match-string 1) (file-name-directory (match-string 4))))

(defconst csharp-compilation-re-msbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "error [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation error from msbuild.")

(defconst csharp-compilation-re-msbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "warning [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation warning from msbuild.")

;; Notes on xbuild and devenv commonalities
;;
;; These regexes were tailored for xbuild, but apart from the concurrent
;; build-marker ("1>") they share exactly the same match-markers.
;;
;; If we don't exclude the match-markers explicitly, these regexes
;; will also be used to match for devenv as well, including the build-marker
;; in the file-name, causing the lookup to fail.
;;
;; So if we don't want devenv to fail, we actually need to handle it in our
;; xbuild-regexes, but then we automatically get devenv-support for free.

(defconst csharp-compilation-re-xbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)*): "
   "error [[:alnum:]]+: .+$")
  "Regexp to match compilation error from xbuild.")

(defconst csharp-compilation-re-xbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)?*): "
   "warning [[:alnum:]]+: .+$")
  "Regexp to match compilation warning from xbuild.")

(eval-after-load 'compile
  (lambda ()
    (dolist
        (regexp
         `((xbuild-error
            ,csharp-compilation-re-xbuild-error
            1 2 3 2)
           (xbuild-warning
            ,csharp-compilation-re-xbuild-warning
            1 2 3 1)
           (msbuild-error
            ,csharp-compilation-re-msbuild-error
            csharp--compilation-error-file-resolve
            2
            3
            2
            nil
            (1 compilation-error-face)
            (4 compilation-error-face))
           (msbuild-warning
            ,csharp-compilation-re-msbuild-warning
            csharp--compilation-error-file-resolve
            2
            3
            1
            nil
            (1 compilation-warning-face)
            (4 compilation-warning-face))))
      (add-to-list 'compilation-error-regexp-alist-alist regexp)
      (add-to-list 'compilation-error-regexp-alist (car regexp)))))


;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))


(c-add-style "C#"
             '("Java"
               (c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . (
                                   (access-label          . -)
                                   (arglist-close         . c-lineup-arglist)
                                   (arglist-cont          . 0)
                                   (arglist-cont-nonempty . c-lineup-arglist)
                                   (arglist-intro         . c-lineup-arglist-intro-after-paren)
                                   (block-close           . 0)
                                   (block-open            . 0)
                                   (brace-entry-open      . 0)
                                   (brace-list-close      . 0)
                                   (brace-list-entry      . 0)
                                   (brace-list-intro      . +)
                                   (brace-list-open       . 0)
                                   (c                     . c-lineup-C-comments)
                                   (case-label            . +)
                                   (catch-clause          . 0)
                                   (class-close           . 0)
                                   (class-open            . 0)
                                   (comment-intro         . c-lineup-comment)
                                   (cpp-macro             . [0])
                                   (cpp-macro-cont        . c-lineup-dont-change)
                                   (defun-block-intro     . +)
                                   (defun-close           . 0)
                                   (defun-open            . 0)
                                   (do-while-closure      . 0)
                                   (else-clause           . 0)
                                   (extern-lang-close     . 0)
                                   (extern-lang-open      . 0)
                                   (friend                . 0)
                                   (func-decl-cont        . +)
                                   (inclass               . +)
                                   (inexpr-class          . 0)
                                   (inexpr-statement      . 0)
                                   (inextern-lang         . +)
                                   (inher-cont            . c-lineup-multi-inher)
                                   (inher-intro           . +)
                                   (inlambda              . c-lineup-inexpr-block)
                                   (inline-close          . 0)
                                   (inline-open           . 0)
                                   (innamespace           . +)
                                   (knr-argdecl           . 0)
                                   (knr-argdecl-intro     . 5)
                                   (label                 . 0)
                                   (lambda-intro-cont     . +)
                                   (member-init-cont      . c-lineup-multi-inher)
                                   (member-init-intro     . +)
                                   (namespace-close       . 0)
                                   (namespace-open        . 0)
                                   (statement             . 0)
                                   (statement-block-intro . +)
                                   (statement-case-intro  . +)
                                   (statement-case-open   . +)
                                   (statement-cont        . +)
                                   (stream-op             . c-lineup-streamop)
                                   (string                . c-lineup-dont-change)
                                   (substatement          . +)
                                   (substatement-open     . 0)
                                   (template-args-cont c-lineup-template-args +)
                                   (topmost-intro         . 0)
                                   (topmost-intro-cont    . +)
                                   ))
               ))


;;;###autoload
(define-derived-mode csharp-mode prog-mode "C#"
  "Major mode for editing C# code.

The mode provides fontification and indent for C# syntax, as well
as some other handy features.

At mode startup, there are two interesting hooks that run:
`prog-mode-hook' is run with no args, then `csharp-mode-hook' is run after
that, also with no args.

To run your own logic after csharp-mode starts, do this:

  (defun my-csharp-mode-fn ()
    \"my function that runs when csharp-mode is initialized for a buffer.\"
    (turn-on-font-lock)
    (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio
    (setq indent-tabs-mode nil) ;; tabs are evil
    ....your own code here...
  )
  (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


The function above is just a suggestion.


Imenu Integration
===============================

Check the menubar for menu entries for Imenu; it is labelled
\"Index\".

The Imenu index gets computed when the file is .cs first opened and loaded.
This may take a moment or two.  If you don't like this delay and don't
use Imenu, you can turn this off with the variable `csharp-want-imenu'.



Key bindings:
\\{csharp-mode-map}"
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (c-initialize-cc-mode t)

  ;; define underscore as part of a word in the Csharp syntax table
  (modify-syntax-entry ?_ "w" csharp-mode-syntax-table)

  ;; define @ as an expression prefix in Csharp syntax table
  (modify-syntax-entry ?@ "'" csharp-mode-syntax-table)

  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars csharp-mode)

  ;; Use our predefined "C#" style unless a file local or default
  ;; style is found. This is done by rebinding `c-default-style'
  ;; during the `c-common-init' call. 'c-common-init' will initialize
  ;; the buffer's style using the value of `c-default-style'.
  (let ((c-default-style (if (or c-file-style
                                 (stringp c-default-style)
                                 (assq 'csharp-mode c-default-style))
                             c-default-style
                           "C#")))
    ;; `c-common-init' initializes most of the components of a CC Mode
    ;; buffer, including setup of the mode menu, font-lock, etc.
    ;; There's also a lower level routine `c-basic-common-init' that
    ;; only makes the necessary initialization to get the syntactic
    ;; analysis and similar things working.
    (c-common-init 'csharp-mode))

  (define-key csharp-mode-map (kbd "/") 'csharp-maybe-insert-codedoc)

  ;; Need the following for parse-partial-sexp to work properly with
  ;; verbatim literal strings Setting this var to non-nil tells
  ;; `parse-partial-sexp' to pay attention to the syntax text
  ;; properties on the text in the buffer.  If csharp-mode attaches
  ;; text syntax to @"..." then, `parse-partial-sexp' will treat those
  ;; strings accordingly.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  ;; Allow fill-paragraph to work on xml code doc
  ;; This setting gets overwritten quietly by c-run-mode-hooks,
  ;; so I put it afterwards to make it stick.
  (make-local-variable 'paragraph-separate)

  ;; Speedbar handling
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension '(".cs"))) ;; idempotent

  (c-update-modeline)

  ;; maybe do imenu scan after hook returns
  (when csharp-want-imenu
    (csharp--setup-imenu))

  ;; The paragraph-separate variable was getting stomped by
  ;; other hooks, so it must reside here.
  (setq-local paragraph-separate
              "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

  (setq-local beginning-of-defun-function 'csharp-move-back-to-beginning-of-defun)
  ;; `end-of-defun-function' can remain forward-sexp !!

  (set (make-local-variable 'comment-auto-fill-only-comments) t)

  (set (make-local-variable 'syntax-propertize-function)
       'csharp-mode-syntax-propertize-function)

  ;; required since Emacs git master
  ;; https://github.com/emacs-mirror/emacs/commit/edcdf64960a2ab4e8d9ce4419874e43b6d3ccee4
  ;;
  ;; Basically syntax-propertize-function is a construct which belongs
  ;; to font-lock.  But correct indentation depends on
  ;; syntax-properties of the text, and that should ideally be
  ;; independent of font-lock being activated or not.
  ;;
  ;; For csharp-mode, this means that with font-lock disabled, we wont
  ;; have our syntax-properties set correctly, and indentation will
  ;; suffer.
  ;;
  ;; To patch our way around this, we issue a syntax-propertize call
  ;; manually, font-lock enabled or not.
  (with-silent-modifications
    (csharp-mode-syntax-propertize-function (point-min) (point-max))))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
