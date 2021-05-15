;;; purescript-font-lock.el --- Font locking module for PureScript Mode

;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998  Graeme E Moss, and Tommy Thorn

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;; Keywords: faces files PureScript

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;; To support fontification of standard PureScript keywords, symbols,
;; functions, etc.  Supports full PureScript 1.4 as well as LaTeX- and
;; Bird-style literate scripts.
;;
;; Installation:
;;
;; To turn font locking on for all PureScript buffers under the PureScript
;; mode of Moss&Thorn, add this to .emacs:
;;
;;    (add-hook 'purescript-mode-hook 'turn-on-purescript-font-lock)
;;
;; Otherwise, call `turn-on-purescript-font-lock'.
;;
;;
;; Customisation:
;;
;; The colours and level of font locking may be customised.  See the
;; documentation on `turn-on-purescript-font-lock' for more details.
;;
;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; . Debatable whether `()' `[]' `(->)' `(,)' `(,,)' etc.  should be
;;   highlighted as constructors or not.  Should the `->' in
;;   `id :: a -> a' be considered a constructor or a keyword?  If so,
;;   how do we distinguish this from `\x -> x'?  What about the `\'?
;;
;; . XEmacs can support both `--' comments and `{- -}' comments
;;   simultaneously.  If XEmacs is detected, this should be used.
;;
;; . Support for GreenCard?
;;
;;
;; All functions/variables start with
;; `(turn-(on/off)-)purescript-font-lock' or `purescript-fl-'.

;;; Change Log:

;; Version 1.3:
;;   From Dave Love:
;;   Support for proper behaviour (including with Unicode identifiers)
;;   in Emacs 21 only hacked in messily to avoid disturbing the old
;;   stuff.  Needs integrating more cleanly.  Allow literate comment
;;   face to be customized.  Some support for fontifying definitions.
;;   (I'm not convinced the faces should be customizable -- fontlock
;;   faces are normally expected to be consistent.)
;;
;; Version 1.2:
;;   Added support for LaTeX-style literate scripts.  Allow whitespace
;;   after backslash to end a line for string continuations.
;;
;; Version 1.1:
;;   Use own syntax table.  Use backquote (neater).  Stop ''' being
;;   highlighted as quoted character.  Fixed `\"' fontification bug
;;   in comments.
;;
;; Version 1.0:
;;   Brought over from PureScript mode v1.1.

;;; Code:

(require 'font-lock)
(require 'cl-lib)

(defcustom purescript-font-lock-prettify-symbols-alist
  `(("/\\" . ,(decode-char 'ucs #X2227))
    ("\\" . ,(decode-char 'ucs 955))
    ("not" . ,(decode-char 'ucs 172))
    ("->" . ,(decode-char 'ucs 8594))
    ("<-" . ,(decode-char 'ucs 8592))
    ("=>" . ,(decode-char 'ucs 8658))
    ("()" . ,(decode-char 'ucs #X2205))
    ("==" . ,(decode-char 'ucs #X2261))
    ("<<<" . ,(decode-char 'ucs 9675))
    ("/=" . ,(decode-char 'ucs #X2262))
    (">=" . ,(decode-char 'ucs #X2265))
    ("<=" . ,(decode-char 'ucs #X2264))
    ("!!" . ,(decode-char 'ucs #X203C))
    ("&&" . ,(decode-char 'ucs #X2227))
    ("||" . ,(decode-char 'ucs #X2228))
    ("sqrt" . ,(decode-char 'ucs #X221A))
    ("undefined" . ,(decode-char 'ucs #X22A5)) ;; Not really needed for Purescript
    ("pi" . ,(decode-char 'ucs #X3C0))
    ("~>" . ,(decode-char 'ucs 8669)) ;; Omega language
    ("-<" . ,(decode-char 'ucs 8610)) ;; Paterson's arrow syntax
    ("::" . ,(decode-char 'ucs 8759))
    ("forall" . ,(decode-char 'ucs 8704)))
  "A set of symbol compositions for use as `prettify-symbols-alist'."
  :group 'purescript
  :type '(repeat (cons string character)))

;; Use new vars for the font-lock faces.  The indirection allows people to
;; use different faces than in other modes, as before.
(defvar purescript-keyword-face 'font-lock-keyword-face)
(defvar purescript-constructor-face 'font-lock-type-face)
;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defvar purescript-definition-face 'font-lock-function-name-face)
;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `purescript-definition-face'.
(defvar purescript-operator-face 'font-lock-variable-name-face)
(defvar purescript-default-face nil)
(defvar purescript-literate-comment-face 'font-lock-doc-face
  "Face with which to fontify literate comments.
Set to `default' to avoid fontification of them.")

;; The font lock regular expressions.
(defun purescript-font-lock-keywords-create (literate)
  "Create fontification definitions for PureScript scripts.
Returns keywords suitable for `font-lock-keywords'."
  (let* (;; Bird-style literate scripts start a line of code with
         ;; "^>", otherwise a line of code starts with "^".
         (line-prefix (if (eq literate 'bird) "^> ?" "^"))

         ;; Most names are borrowed from the lexical syntax of the PureScript
         ;; report.
         ;; Some of these definitions have been superseded by using the
         ;; syntax table instead.

         ;; (ASCsymbol "-!#$%&*+./<=>?@\\\\^|~")
         ;; Put the minus first to make it work in ranges.

         ;; We allow _ as the first char to fit GHC
         (varid "\\b[[:lower:]_][[:alnum:]'_]*\\b")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "\\b'?[[:upper:]][[:alnum:]'_]*\\b")
         (modid (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid (concat modid "\\." varid))
         (qconid (concat modid "\\." conid))
         (sym
          ;; We used to use the below for non-Emacs21, but I think the
          ;; regexp based on syntax works for other emacsen as well.  -- Stef
          ;; (concat "[" symbol ":]+")
          ;; Add backslash to the symbol-syntax chars.  This seems to
          ;; be thrown for some reason by backslash's escape syntax.
          "\\(\\s_\\|\\\\\\)+")

         ;; Reserved operations
         (reservedsym
          (concat "\\S_"
                  ;; (regexp-opt '(".." "::" "=" "\\" "|" "<-" "->"
                  ;;            "@" "~" "=>") t)
                  "\\(->\\|\\.\\.\\|::\\|∷\\|<-\\|=>\\|[=@\\|~]\\)"
                  "\\S_"))
         ;; Reserved identifiers
         (reservedid
          ;; `as', `hiding', and `qualified' are part of the import
          ;; spec syntax, but they are not reserved.
          ;; `_' can go in here since it has temporary word syntax.
          (regexp-opt
           '("case" "class" "data" "default" "deriving" "do"
             "else" "if" "import" "in" "infix" "infixl"
             "infixr" "instance" "let" "module" "newtype" "of"
             "then" "type" "where" "_") 'words))

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\)\\s-*"
                  ;; optionally allow for a single newline after identifier
                  ;; NOTE: not supported for bird-style .lpurs files
                  (if (eq literate 'bird) nil "\\([\n]\\s-+\\)?")
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (::) or (∷), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|::\\|∷\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^<<<<<<< .*$" 0 'font-lock-warning-face t)
            ("^=======" 0 'font-lock-warning-face t)
            ("^>>>>>>> .*$" 0 'font-lock-warning-face t)
            ("^#.*$" 0 'font-lock-preprocessor-face t)

            (,reservedid 1 (symbol-value 'purescript-keyword-face))
            (,reservedsym 1 (symbol-value 'purescript-operator-face))
            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 (symbol-value 'purescript-keyword-face) nil lax)
             (2 (symbol-value 'purescript-keyword-face) nil lax)
             (3 (symbol-value 'purescript-keyword-face) nil lax)
             (4 (symbol-value 'purescript-keyword-face) nil lax))

            (,reservedsym 1 (symbol-value 'purescript-operator-face))
            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 (symbol-value 'purescript-keyword-face) nil lax)
             (2 (symbol-value 'purescript-keyword-face) nil lax)
             (3 (symbol-value 'purescript-keyword-face) nil lax)
             (4 (symbol-value 'purescript-keyword-face) nil lax))

            (,reservedsym 1 (symbol-value 'purescript-operator-face))
            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 (symbol-value 'purescript-keyword-face) nil lax)
             (2 (symbol-value 'purescript-keyword-face) nil lax)
             (3 (symbol-value 'purescript-keyword-face) nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 (symbol-value 'purescript-definition-face)))
            (,topdecl-var2 (2 (symbol-value 'purescript-definition-face)))
            (,topdecl-sym  (2 (symbol-value 'purescript-definition-face)))
            (,topdecl-sym2 (1 (symbol-value 'purescript-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 (symbol-value 'purescript-constructor-face))
            ("\\[\\]" 0 (symbol-value 'purescript-constructor-face))
            ;; Expensive.
            (,qvarid 0 (symbol-value 'purescript-default-face))
            (,qconid 0 (symbol-value 'purescript-constructor-face))
            (,(concat "\`" varid "\`") 0 (symbol-value 'purescript-operator-face))
            ;; Expensive.
            (,conid 0 (symbol-value 'purescript-constructor-face))

            ;; Very expensive.
            (,sym 0 (if (eq (char-after (match-beginning 0)) ?:)
                        purescript-constructor-face
                      purescript-operator-face))))
    (unless (boundp 'font-lock-syntactic-keywords)
      (cl-case literate
        (bird
         (setq keywords
               `(("^[^>\n].*$" 0 purescript-comment-face t)
                 ,@keywords
                 ("^>" 0 purescript-default-face t))))
        ((latex tex)
         (setq keywords
               `((purescript-fl-latex-comments 0 'font-lock-comment-face t)
                 ,@keywords)))))
    keywords))

;; The next three aren't used in Emacs 21.

(defvar purescript-fl-latex-cache-pos nil
  "Position of cache point used by `purescript-fl-latex-cache-in-comment'.
Should be at the start of a line.")

(defvar purescript-fl-latex-cache-in-comment nil
  "If `purescript-fl-latex-cache-pos' is outside a
\\begin{code}..\\end{code} block (and therefore inside a comment),
this variable is set to t, otherwise nil.")

(defun purescript-fl-latex-comments (end)
  "Sets `match-data' according to the region of the buffer before end
that should be commented under LaTeX-style literate scripts."
  (let ((start (point)))
    (if (= start end)
        ;; We're at the end.  No more to fontify.
        nil
      (if (not (eq start purescript-fl-latex-cache-pos))
          ;; If the start position is not cached, calculate the state
          ;; of the start.
          (progn
            (setq purescript-fl-latex-cache-pos start)
            ;; If the previous \begin{code} or \end{code} is a
            ;; \begin{code}, then start is not in a comment, otherwise
            ;; it is in a comment.
            (setq purescript-fl-latex-cache-in-comment
                  (if (and
                       (re-search-backward
                        "^\\(\\(\\\\begin{code}\\)\\|\\(\\\\end{code}\\)\\)$"
                        (point-min) t)
                       (match-end 2))
                      nil t))
            ;; Restore position.
            (goto-char start)))
      (if purescript-fl-latex-cache-in-comment
          (progn
            ;; If start is inside a comment, search for next \begin{code}.
            (re-search-forward "^\\\\begin{code}$" end 'move)
            ;; Mark start to end of \begin{code} (if present, till end
            ;; otherwise), as a comment.
            (set-match-data (list start (point)))
            ;; Return point, as a normal regexp would.
            (point))
        ;; If start is inside a code block, search for next \end{code}.
        (if (re-search-forward "^\\\\end{code}$" end t)
            ;; If one found, mark it as a comment, otherwise finish.
            (point))))))

(defconst purescript-basic-syntactic-keywords
  '(;; Character constants (since apostrophe can't have string syntax).
    ;; Beware: do not match something like 's-}' or '\n"+' since the first '
    ;; might be inside a comment or a string.
    ;; This still gets fooled with "'"'"'"'"'"', but ... oh well.
    ("\\Sw\\('\\)\\([^\\'\n]\\|\\\\.[^\\'\n \"}]*\\)\\('\\)" (1 "|") (3 "|"))
    ;; The \ is not escaping in \(x,y) -> x + y.
    ("\\(\\\\\\)(" (1 "."))
    ;; The second \ in a gap does not quote the subsequent char.
    ;; It's probably not worth the trouble, tho.
    ;; ("^[ \t]*\\(\\\\\\)" (1 "."))
    ;; Deal with instances of `--' which don't form a comment
    ("\\s_\\{3,\\}" (0 (cond ((numberp (nth 4 (syntax-ppss)))
                              ;; There are no such instances inside nestable comments
                              nil)
                             ((string-match "\\`-*\\'" (match-string 0))
                              ;; Sequence of hyphens.  Do nothing in
                              ;; case of things like `{---'.
                              nil)
                             (t "_")))) ; other symbol sequence
    ))

(defconst purescript-bird-syntactic-keywords
  (cons '("^[^\n>]"  (0 "<"))
        purescript-basic-syntactic-keywords))

(defconst purescript-latex-syntactic-keywords
  (append
   '(("^\\\\begin{code}\\(\n\\)" 1 "!")
     ;; Note: buffer is widened during font-locking.
     ("\\`\\(.\\|\n\\)" (1 "!"))               ; start comment at buffer start
     ("^\\(\\\\\\)end{code}$" 1 "!"))
   purescript-basic-syntactic-keywords))

(defcustom purescript-font-lock-docstrings (boundp 'font-lock-doc-face)
  "If non-nil try to highlight docstring comments specially."
  :type 'boolean
  :group 'purescript)

(defvar purescript-font-lock-seen-docstring nil)
(make-variable-buffer-local 'purescript-font-lock-seen-docstring)

(defvar purescript-literate)

(defun purescript-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for PureScript."
  (cond
   ((nth 3 state) font-lock-string-face) ; as normal
   ;; Else comment.  If it's from syntax table, use default face.
   ((or (eq 'syntax-table (nth 7 state))
        (and (eq purescript-literate 'bird)
             (memq (char-before (nth 8 state)) '(nil ?\n))))
    purescript-literate-comment-face)
   ;; Try and recognize docstring comments.  From what I gather from its
   ;; documentation, its comments can take the following forms:
   ;; a) {-| ... -}
   ;; b) {-^ ... -}
   ;; c) -- | ...
   ;; d) -- ^ ...
   ;; e) -- ...
   ;; Where `e' is the tricky one: it is only a docstring comment if it
   ;; follows immediately another docstring comment.  Even an empty line
   ;; breaks such a sequence of docstring comments.  It is not clear if `e'
   ;; can follow any other case, so I interpreted it as following only cases
   ;; c,d,e (not a or b).  In any case, this `e' is expensive since it
   ;; requires extra work for each and every non-docstring comment, so I only
   ;; go through the more expensive check if we've already seen a docstring
   ;; comment in the buffer.
   ((and purescript-font-lock-docstrings
         (save-excursion
           (goto-char (nth 8 state))
           (or (looking-at "\\(-- \\|{-\\)[ \\t]*[|^]")
               (and purescript-font-lock-seen-docstring
                    (looking-at "-- ")
                    (let ((doc nil)
                          pos)
                      (while (and (not doc)
                                  (setq pos (line-beginning-position))
                                  (forward-comment -1)
                                  (eq (line-beginning-position 2) pos)
                                  (looking-at "--\\( [|^]\\)?"))
                        (setq doc (match-beginning 1)))
                      doc)))))
    (set (make-local-variable 'purescript-font-lock-seen-docstring) t)
    font-lock-doc-face)
   (t font-lock-comment-face)))

(defconst purescript-font-lock-keywords
  (purescript-font-lock-keywords-create nil)
  "Font lock definitions for non-literate PureScript.")

(defconst purescript-font-lock-bird-literate-keywords
  (purescript-font-lock-keywords-create 'bird)
  "Font lock definitions for Bird-style literate PureScript.")

(defconst purescript-font-lock-latex-literate-keywords
  (purescript-font-lock-keywords-create 'latex)
  "Font lock definitions for LaTeX-style literate PureScript.")

;;;###autoload
(defun purescript-font-lock-choose-keywords ()
  (cl-case (bound-and-true-p purescript-literate)
    (bird purescript-font-lock-bird-literate-keywords)
    ((latex tex) purescript-font-lock-latex-literate-keywords)
    (t purescript-font-lock-keywords)))

(defun purescript-font-lock-choose-syntactic-keywords ()
  (cl-case (bound-and-true-p purescript-literate)
    (bird purescript-bird-syntactic-keywords)
    ((latex tex) purescript-latex-syntactic-keywords)
    (t purescript-basic-syntactic-keywords)))

(defun purescript-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for PureScript."
  (set (make-local-variable 'font-lock-defaults)
       '(purescript-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . purescript-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . purescript-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t))))

;; The main functions.
(defun turn-on-purescript-font-lock ()
  "Turns on font locking in current buffer for PureScript 1.4 scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

   `purescript-keyword-face'      for reserved keywords and syntax,
   `purescript-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `purescript-operator-face'     for symbolic and alphanumeric operators,
   `purescript-default-face'      for ordinary code.

The variables are initialised to the following font lock default faces:

   `purescript-keyword-face'      `font-lock-keyword-face'
   `purescript-constructor-face'  `font-lock-type-face'
   `purescript-operator-face'     `font-lock-function-name-face'
   `purescript-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((purescript-mode . 2) (t . 0)))

uses level two fontification for `purescript-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'purescript-font-lock-hook
      (lambda ()
          (set-face-foreground 'purescript-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all PureScript buffers, add this to .emacs:

  (add-hook 'purescript-mode-hook 'turn-on-purescript-font-lock)

To turn font locking on for the current buffer, call
`turn-on-purescript-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-purescript-font-lock'.

Bird-style literate PureScript scripts are supported: If the value of
`purescript-literate-bird-style' (automatically set by the PureScript mode
of Moss&Thorn) is non-nil, a Bird-style literate script is assumed.

Invokes `purescript-font-lock-hook' if not nil."
  (purescript-font-lock-defaults-create)
  (run-hooks 'purescript-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-purescript-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))

;; Provide ourselves:

(provide 'purescript-font-lock)

;; Local Variables:
;; tab-width: 8
;; End:

;;; purescript-font-lock.el ends here
