;;; pretty-mode.el --- Redisplay parts of the buffer as pretty symbols.
;;; -*- coding: utf-8 -*-

;; Copyright © March 2008 Arthur Danskin <arthurdanskin@gmail.com>
;; Copyright © January 2013 Grant Rettke <grettke@acm.org>
;; Copyright © April 2013 Dmitri Akatov <akatov@gmail.com>

;; Filename: pretty-mode.el
;; Description: Redisplay parts of the buffer as pretty symbols.
;; Author: Arthur Danskin <arthurdanskin@gmail.com>
;; Maintainer: Dmitri Akatov <akatov@gmail.com>
;; URL: https://github.com/akatov/pretty-mode
;; Keywords: pretty, unicode, symbols
;; Version: 20141207.1152
;; X-Original-Version: 2.0.3

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;;
;; to install:
;;
;; (require 'pretty-mode)
;; and
;; (global-pretty-mode 1)
;; or
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)

;;; Code:

(require 'cl)

(defvar pretty-syntax-types '(?_ ?w))

;; modified from `sml-mode'
(defun pretty-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntax (char-syntax (char-after start))))
    (if (or (if (memq syntax pretty-syntax-types)
               (or (memq (char-syntax (char-before start)) pretty-syntax-types)
                  (memq (char-syntax (char-after end)) pretty-syntax-types))
             (memq (char-syntax (char-before start)) '(?. ?\\)))
           (memq (get-text-property start 'face)
                 '(font-lock-doc-face font-lock-string-face
                                      font-lock-comment-face)))
        (remove-text-properties start end '(composition))
      (compose-region start end (cdr (assoc (match-string 0) alist)))
;;;       (add-text-properties start end `(display ,repl)))
      ))
  ;; Return nil because we're not adding any face property.
  nil)

(defvar pretty-modes-aliases
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (literate-haskell-mode . haskell-mode)
    (inf-haskell-mode . haskell-mode) ; is this still used??
    (inferior-haskell-mode . haskell-mode)
    (nrepl-mode . clojure-mode)
    (cider-repl-mode . clojure-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (js-mode . javascript-mode)
    (js2-mode . javascript-mode)
    (enh-ruby-mode . ruby-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from modes that should have the same substitution
patterns as to the mode they are mapping to. Usually these are
inferior process interaction modes corresponding to their main
script editing modes.")

(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(regexp-opt (mapcar 'car alist))
       (0 (pretty-font-lock-compose-symbol
           ',alist))))))

(defun ensure-list (arg)
  "Return ARG if it is a list or pack it inside one if it isn't."
  (if (listp arg)
      arg
    (list arg)))

(defun ensure-mode (mode)
  "Return MODE if it is a symbol ending in \"-mode\", or derive the
implied mode from MODE and return it."
  (let* ((name (if (stringp mode)
                   mode
                 (symbol-name mode)))
         (name (if (string= ":" (substring name 0 1))
                   (substring name 1)
                 name)))
    (intern (if (string= "mode"
                         (car (last (split-string name "-"))))
                name
              (concat name "-mode")))))

(defvar pretty-default-groups
  '(:function
    :greek-capitals :greek-lowercase
    ;; turn on :greek manually
    :equality
    :ordering :ordering-double :ordering-triple
    :logic
    ;; turn on :logic-nary manually
    :nil
    :sets :sets-operations :sets-relations
    ;; turn on :sets-operations-nary manually
    :arrows :arrows-twoheaded
    ;; turn on :arrows-tails and :arrows-tails-double manually
    :arithmetic :arithmetic-double
    ;; turn on :arithmetic-triple and :arithmetic-nary manually
    :punctuation
    :subscripts :superscripts
    ;; turn on :sub-and-superscripts manually
    ;; turn on :parentheses manually
    ;; turn on :types manually
    )
  "A list of groups that should be activated by default.")

(defvar pretty-supported-modes
  '(ruby-mode
    ess-mode java-mode octave-mode tuareg-mode
    python-mode sml-mode jess-mode clips-mode clojure-mode
    lisp-mode emacs-lisp-mode scheme-mode sh-mode
    perl-mode c++-mode c-mode haskell-mode
    javascript-mode coffee-mode groovy-mode fsharp-mode)
  "A list of all supported modes.")

(defun ensure-modes (modes)
  "Return a list of symbols ending in \"-mode\". If MODES is empty,
returns all modes, otherwise it calls `ensure-mode' on every member
of MODES."
  (if (null modes)
      pretty-supported-modes
    (mapcar* 'ensure-mode (ensure-list modes))))

(defvar pretty-active-groups
  nil
  "Alist mapping from a mode to a list of active groups for that
mode. An entry has the form (MODE . (GROUP1 ...)), where each
GROUP is a keyword.")

(defvar pretty-active-patterns
  nil
  "Alist mapping from a mode to a list of active patterns for that
mode that should be used, even though their group(s) aren't active.
An entry has the form (MODE . (PATTERN1 ...)), where each PATTERN
 is either a keyword or a string.")

(defvar pretty-deactivated-patterns
  nil
  "Alist mapping from a mode to a list of deactivated patterns for
that mode that should be not be used, even though their group(s) may
be active. An entry has the form (MODE . (PATTERN1 ...)), where each
PATTERN is either a keyword or a string.")

(defun pretty-defaults ()
  (setq pretty-active-groups
        (mapcar* (lambda (mode)
                   (cons mode (copy-sequence pretty-default-groups)))
                 pretty-supported-modes))
  (setq pretty-active-patterns
        (mapcar* (lambda (mode)
                   (cons mode nil))
                 pretty-supported-modes))
  (setq pretty-deactivated-patterns
        (mapcar* (lambda (mode)
                   (cons mode nil))
                 pretty-supported-modes)))

(pretty-defaults)

(defun pretty-is-active-pattern (symbol-name groups name mode)
  "Checks whether a given pattern is currently active according to the
pretty-active-patterns/groups and pretty-deactivated-patterns variables."
  (let ((active-groups     (cdr (assoc mode pretty-active-groups)))
        (active-patterns   (cdr (assoc mode pretty-active-patterns)))
        (inactive-patterns (cdr (assoc mode pretty-deactivated-patterns)))
        (patterns          (list symbol-name name)))
    (or (intersection patterns active-patterns)
       (and (subsetp groups active-groups)
          (not (intersection patterns inactive-patterns))))))

(defun pretty-activate-groups (groups &optional modes)
  "Add GROUPS to each entry in `pretty-active-groups' for every entry
in MODES. If MODES is empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (groups (ensure-list groups)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-groups)))
            (setcdr cell (union (cdr cell) groups))))))

(defun pretty-deactivate-groups (groups &optional modes)
  "Remove all members of GROUPS from every entry in
`pretty-active-groups' associated with each entry in MODES. If MODES is
empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (groups (ensure-list groups)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-groups)))
            (setcdr cell (set-difference (cdr cell) groups))))))

(defun pretty-activate-patterns (patterns &optional modes)
  "Add PATTERNS to each entry in `pretty-active-patterns' for every entry
in MODES and remove them from `pretty-deactivated-patterns'. If MODES is
empty, assumes that all modes should be affected."
  (let ((modes (ensure-modes modes))
        (patterns (ensure-list patterns)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-patterns)))
            (setcdr cell (union (cdr cell) patterns)))
          (let ((cell (assq mode pretty-deactivated-patterns)))
            (setcdr cell (set-difference (cdr cell) patterns))))))

(defun pretty-deactivate-patterns (patterns &optional modes)
  "Remove all members of PATTERNS from every entry in
`pretty-active-patterns' associated with each entry in MODES and add them
to `pretty-deactivated-patterns'. If MODES is empty, assumes that all
modes should be affected."
  (let ((modes (ensure-modes modes))
        (patterns (ensure-list patterns)))
    (loop for mode in modes do
          (let ((cell (assq mode pretty-active-patterns)))
            (setcdr cell (set-difference (cdr cell) patterns)))
          (let ((cell (assq mode pretty-deactivated-patterns)))
            (setcdr cell (union (cdr cell) patterns))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode (pretty-patterns))
                   (assoc (cdr-safe
                           (assoc mode pretty-modes-aliases))
                          (pretty-patterns))))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

;;;###autoload
(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
                                        ;  :lighter " λ"
  (if pretty-mode
      (progn
        (font-lock-add-keywords nil (pretty-keywords) t)
        (when font-lock-mode
          (font-lock-fontify-buffer)))
    (font-lock-remove-keywords nil (pretty-keywords))
    (remove-text-properties (point-min) (point-max) '(composition nil))))

(defun turn-on-pretty-if-desired ()
  "Turn on `pretty-mode' if the current major mode supports it."
  (if (pretty-keywords)
      (pretty-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

;;;###autoload
(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))

;;;###autoload
(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defun pretty-compile-patterns (patterns)
  "Set pretty patterns in a convenient way.

PATTERNS should be of the form ((GLYPH NAMES GROUPS (REGEXP MODE ...) ...)
...). GLYPH should be a character. NAMES and GROUPS should both be lists of keywords,
MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph symbol-name groups . triples) in patterns do
          (loop for (name regexp . major-modes) in triples do
                (loop for mode in major-modes do
                      (let* ((mode (ensure-mode mode))
                             (assoc-pair (assoc mode pretty-patterns))
                             (entry (cons regexp glyph)))
                        (when (pretty-is-active-pattern symbol-name groups
                                                        name mode)
                          (if assoc-pair
                              (push entry (cdr assoc-pair))
                            (push (cons mode (list entry))
                                  pretty-patterns)))))))
    pretty-patterns))

(defun pretty-patterns ()
    "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)"
  (let* ((lispy '(scheme emacs-lisp lisp clojure jess clips))
         (mley '(haskell tuareg sml fsharp))
         (c-like '(c c++ perl sh python java ess ruby javascript coffee groovy))
         (all (append lispy mley c-like (list 'octave))))
    (pretty-compile-patterns
     `(
       ;;; Values taken directly from `The Unicode Standard, Version 5.2' documented
       ;;; in `U0080.pdf', located at http://unicode.org/charts/PDF/U0080.pdf
       ;;; in `U0370.pdf', located at http://unicode.org/charts/PDF/U0370.pdf
       ;;; in `U2000.pdf', located at http://unicode.org/charts/PDF/U2000.pdf
       ;;; in `U2070.pdf', located at http://unicode.org/charts/PDF/U2070.pdf
       ;;; in `U2100.pdf', located at http://unicode.org/charts/PDF/U2100.pdf
       ;;; in `U2190.pdf', located at http://unicode.org/charts/PDF/U2190.pdf
       ;;; in `U2200.pdf', located at http://unicode.org/charts/PDF/U2200.pdf
       ;;; in `U27C0.pdf', located at http://unicode.org/charts/PDF/U2900.pdf
       ;;; in `U2900.pdf', located at http://unicode.org/charts/PDF/U2900.pdf
       ;;; in `U2980.PDF', located at http://unicode.org/charts/PDF/U2980.pdf

       ;;; Ordering

       ;;; 226A ≪ MUCH LESS-THAN
       (?\u226A :ll (:ordering :ordering-double)
                (:<< "<<" haskell ruby c c++ java javascript coffee))
       ;;; 226B ≫ MUCH GREATER-THAN
       (?\u226B :gg (:ordering :ordering-double)
                (:>> ">>" haskell ruby c c++ java javascript coffee))
       ;;; 2264 ≤ LESS-THAN OR EQUAL TO
       (?\u2264 :leq (:ordering)
                (:<= "<=" ,@all))
       ;;; 2265 ≥ GREATER-THAN OR EQUAL TO
       (?\u2265 :geq (:ordering)
                (:>= ">=" ,@all))
       ;;; 22D8 ⋘ VERY MUCH LESS-THAN
       (?\u22D8 :lll (:ordering :ordering-triple)
                (:<<< "<<<" haskell))        ; Control.Arrow
       ;;; 22D9 ⋙ VERY MUCH GREATER-THAN
       (?\u22D9 :ggg (:ordering :ordering-triple)
                (:>>> ">>>" haskell ruby c c++ java javascript coffee))        ; Control.Arrow

       ;;; Equality

       ;;; 2260 ≠ NOT EQUAL TO
       (?\u2260 :neq (:equality)
                (:!= "!=" ,@c-like scheme octave)
                (:!== "!==" javascript)
                (:not= "not=" clojure)
                (:<> "<>" tuareg octave fsharp)
                (:~= "~=" octave)
                (:/= "/=" haskell))

       ;;; 2A75 ⩵ TWO CONSECUTIVE EQUALS SIGNS
       (?\u2A75 :== (:equality)
                (:== "==" ,@c-like haskell))

       ;;; 2A76 ⩶ THREE CONSECUTIVE EQUALS SIGNS
       (?\u2A76 :=== (:equality)
                (:=== "===" ruby javascript))

       ;; 2245 ≅ APPROXIMATELY EQUAL TO
       (?\u2245 :=~ (:equality)
                (:=~ "=~" ruby))

       ;; ≇ NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
       (?\u2247 :!~ (:equality)
                (:!~ "!~" ruby))

       ;;; Logic

       ;;; 00AC ¬ NOT SIGN
       (?\u00AC :neg (:logic)
                (:! "!" c c++ perl sh ruby javascript)
                (:not "not" ,@lispy haskell sml fsharp))

       ;;; 2227 ∧ LOGICAL AND
       (?\u2227 :wedge (:logic)
                (:and "and" ,@lispy python ruby coffee)
                (:andalso "andalso" sml)
                (:&& "&&" c c++ perl haskell ruby javascript coffee fsharp))

       ;;; 22AB ⊫ DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
       (?\u22AB :models (:logic :logic-extended)
                (:||= "||=" ruby coffee))

       ;;; 2228 ∨ LOGICAL OR
       (?\u2228 :vee (:logic)
                (:or "or" ,@lispy python ruby coffee)
                (:orelse "orelse" sml)
                (:|| "||" c c++ perl haskell ruby javascript coffee fsharp))

       ;;; 22C0 ⋀ N-ARY LOGICAL AND
       (?\u22C0 :bigwedge (:logic :logic-nary)
                (:and "and" haskell))

       ;;; 22C1 ⋁ N-ARY LOGICAL OR
       (?\u22C1 :bigvee (:logic :logic-nary)
                (:or "or" haskell))

       ;;; Sets

       ;;; 2208 ∈ ELEMENT OF
       (?\u2208 :in (:sets :sets-relations)
                (:elem "`elem`" haskell)
                (:in "in" python coffee javascript))

       ;;; 2209 ∉ NOT AN ELEMENT OF
       (?\u2209 :notin (:sets :sets-relations)
                (:notElem "`notElem`" haskell)
                (:not_in "not in" python))

       ;;; 2229 ∩ INTERSECTION
       (?\u2229 :cap (:sets :sets-operations)
                (:intersect "`intersect`" haskell)     ; Data.List
                (:intersection "`intersection`" haskell)) ; Data.Set

       ;;; 222A ∪ UNION
       (?\u222A :cup (:sets :sets-operations)
                (:union "`union`" haskell))    ; Data.List, Data.Set

       ;; 2282 ⊂ SUBSET OF
       (?\u2282 :subset (:sets :sets-relations)
                (:isProperSubsetOf "`isProperSubsetOf`" haskell)
                (:subset "subset" ess))

       ;; 2286 ⊆ SUBSET OF OR EQUAL TO
       (?\u2286 :subseteq (:sets :sets-relations)
                (:isSubsetOf"`isSubsetOf`" haskell)) ; Data.Set

       ;; 22C3 ⋃ N-ARY UNION
       (?\u22C3 :bigcup (:sets :sets-operations :sets-operations-nary)
                (:unions "unions" haskell))     ; Data.Set

       ;; 29F5 ⧵ REVERSE SOLIDUS OPERATOR
       (?\u29F5 :setminus (:sets :sets-operations)
                (:\\\\ "\\\\" haskell))

       ;;; Subscripts and Superscripts

       ;;; 2080 ₀ SUBSCRIPT ZERO
       (?\u2080 :sub-0 (:sub-and-superscripts :subscripts)
                (:\[0\] "[0]" ,@c-like)
                (:\(0\) "(0)" octave)
                (:.\(0\) ".(0)" tuareg)
                (:!!0 "!!0" haskell))

       ;;; 2081 ₁ SUBSCRIPT ONE
       (?\u2081 :sub-1 (:sub-and-superscripts :subscripts)
                (:\[1\] "[1]" ,@c-like)
                (:\(1\) "(1)" octave)
                (:.\(1\) ".(1)" tuareg)
                (:!!1 "!!1" haskell))

       ;;; 2082 ₂ SUBSCRIPT TWO
       (?\u2082 :sub-2 (:sub-and-superscripts :subscripts)
                (:\[2\] "[2]" ,@c-like)
                (:\(2\) "(2)" octave)
                (:.\(2\) ".(2)" tuareg)
                (:!!2 "!!2" haskell))

       ;;; 2083 ₃ SUBSCRIPT THREE
       (?\u2083 :sub-3  (:sub-and-superscripts :subscripts)
                (:\[3\] "[3]" ,@c-like)
                (:\(3\) "(3)" octave)
                (:.\(3\) ".(3)" tuareg)
                (:!!3 "!!3" haskell))

       ;;; 2084 ₄ SUBSCRIPT FOUR
       (?\u2084 :sub-4  (:sub-and-superscripts :subscripts)
                (:\[4\] "[4]" ,@c-like)
                (:\(4\) "(4)" octave)
                (:.\(4\) ".(4)" tuareg)
                (:!!4 "!!4" haskell))

       ;;; 00B2 ² SUPERSCRIPT TWO
       (?\u00B2 :sup-2 (:sub-and-superscripts :superscripts)
                (:**2 "**2" python tuareg octave ruby)
                (:^2 "^2" haskell))

       ;;; 00B3 ³ SUPERSCRIPT THREE
       (?\u00B3 :sup-3 (:sub-and-superscripts :superscripts)
                (:**3 "**3" python tuareg octave ruby)
                (:^3 "^3" haskell))

       ;; 207F ⁿ SUPERSCRIPT LATIN SMALL LETTER N
       (?\u207F :sup-n (:sub-and-superscripts :superscripts)
                (:**n "**n" python tuareg octave ruby)
                (:^n "^n" haskell))

       ;;; Function

       ;;; 03BB λ GREEK SMALL LETTER LAMDA
       (?\u03BB :function (:function)
                (:fn "fn" sml clojure)
                (:fun "fun" tuareg)
                (:fun "fun" fsharp)
                (:function "function" javascript ess)
                (:lambda "lambda" scheme lisp emacs-lisp ruby)
                (:\\ "\\" haskell))

       ;;; 039B Λ GREEK CAPITAL LETTER LAMDA
       (?\u039B :Function (:function)
                (:FN "FN" sml)
                (:FUN "FUN" tuareg))

       ;;; 23CE ⏎ RETURN SYMBOL
       (?\u23CE :return (:function)
                (:return "return" ess))
                
       ;;; 2B9E ⮞ BLACK RIGHTWARDS EQUILATERAL ARROWHEAD
       (?\u2B9E :ForwardPipe (:function)
                (:ForwardPipe "%>%" ess))

       ;;; 2666 ♦ BLACK DIAMOND SUIT
       (?\u2666 :CompoundAssignmentPipe (:function)
                (:CompoundAssignmentPipe "%<>%" ess))

       ;;; Greek Letters

       ;;; 0391 Α GREEK CAPITAL LETTER ALPHA
       (?\u0391 :Alpha (:greek :greek-capitals)
                (:ALPHA "ALPHA" ,@all)
                (:Alpha "Alpha" ,@all))

       ;;; 0392 Β GREEK CAPITAL LETTER BETA
       (?\u0392 :Beta (:greek :greek-capitals)
                (:BETA "BETA" ,@all)
                (:Beta "Beta" ,@all))

       ;;; 0393 Γ GREEK CAPITAL LETTER GAMMA
       (?\u0393 :Gamma (:greek :greek-capitals)
                (:GAMMA "GAMMA" ,@all)
                (:Gamma "Gamma" ,@all))

       ;;; 0394 Δ GREEK CAPITAL LETTER DELTA
       (?\u0394 :Delta (:greek :greek-capitals)
                (:DELTA "DELTA" ,@all)
                (:Delta "Delta" ,@all))

       ;;; 0395 Ε GREEK CAPITAL LETTER EPSILON
       (?\u0395 :Epsilon (:greek :greek-capitals)
                (:EPSILON "EPSILON" ,@all)
                (:Epsilon "Epsilon" ,@all))

       ;;; 0396 Ζ GREEK CAPITAL LETTER ZETA
       (?\u0396 :Zeta (:greek :greek-capitals)
                (:ZETA "ZETA" ,@all)
                (:Zeta "Zeta" ,@all))

       ;;; 0397 Η GREEK CAPITAL LETTER ETA
       (?\u0397 :Eta (:greek :greek-capitals)
                (:ETA "ETA" ,@all)
                (:Eta "Eta" ,@all))

       ;; 0398 Θ GREEK CAPITAL LETTER THETA
       (?\u0398 :Theta (:greek :greek-capitals)
                (:THETA "THETA" ,@all)
                (:Theta "Theta" ,@all))

       ;; 0399 Ι GREEK CAPITAL LETTER IOTA
       (?\u0399 :Iota (:greek :greek-capitals)
                (:IOTA "IOTA" ,@all)
                (:Iota "Iota" ,@all))

       ;; 039A Κ GREEK CAPITAL LETTER KAPPA
       (?\u039A :Kappa (:greek :greek-capitals)
                (:KAPPA "KAPPA" ,@all)
                (:Kappa "Kappa" ,@all))

       ;; 039B Λ GREEK CAPITAL LETTER LAMDA
       (?\u039B :Lambda (:greek :greek-capitals)
                (:LAMBDA "LAMBDA" ,@all)
                (:Lambda "Lambda" ,@all))

       ;; 039C Μ GREEK CAPITAL LETTER MU
       (?\u039C :Mu (:greek :greek-capitals)
                (:MU "MU" ,@all)
                (:Mu "Mu" ,@all))

       ;; 039D Ν GREEK CAPITAL LETTER NU
       (?\u039D :Nu (:greek :greek-capitals)
                (:NU "NU" ,@all)
                (:Nu "Nu" ,@all))

       ;; 039E Ξ GREEK CAPITAL LETTER XI
       (?\u039E :Xi (:greek :greek-capitals)
                (:XI "XI" ,@all)
                (:Xi "Xi" ,@all))

       ;; 039F Ο GREEK CAPITAL LETTER OMICRON
       (?\u039F :Omicron (:greek :greek-capitals)
                (:OMICRON "OMICRON" ,@all)
                (:Omicron "Omicron" ,@all))

       ;; 03A0 Π GREEK CAPITAL LETTER PI
       (?\u03A0 :Pi (:greek :greek-capitals)
                (:PI "PI" ,@all)
                (:Pi "Pi" ,@all))

       ;; 03A1 Ρ GREEK CAPITAL LETTER RHO
       (?\u03A1 :Rho (:greek :greek-capitals)
                (:RHO "RHO" ,@all)
                (:Rho "Rho" ,@all))

       ;; 03A3 Σ GREEK CAPITAL LETTER SIGMA
       (?\u03A3 :Sigma (:greek :greek-capitals)
                (:SIGMA "SIGMA" ,@all)
                (:Sigma "Sigma" ,@all))

       ;; 03A4 Τ GREEK CAPITAL LETTER TAU
       (?\u03A4 :Tau (:greek :greek-capitals)
                (:TAU "TAU" ,@all)
                (:Tau "Tau" ,@all))

       ;; 03A5 Υ GREEK CAPITAL LETTER UPSILON
       (?\u03A5 :Upsilon (:greek :greek-capitals)
                (:UPSILON "UPSILON" ,@all)
                (:Upsilon "Upsilon" ,@all))

       ;; 03A6 Φ GREEK CAPITAL LETTER PHI
       (?\u03A6 :Phi (:greek :greek-capitals)
                (:PHI "PHI" ,@all)
                (:Phi "Phi" ,@all))

       ;; 03A7 Χ GREEK CAPITAL LETTER CHI
       (?\u03A7 :Chi (:greek :greek-capitals)
                (:CHI "CHI" ,@all)
                (:Chi "Chi" ,@all))

       ;; 03A8 Ψ GREEK CAPITAL LETTER PSI
       (?\u03A8 :Psi (:greek :greek-capitals)
                (:PSI "PSI" ,@all)
                (:Psi "Psi" ,@all))

       ;; 03A9 Ω GREEK CAPITAL LETTER OMEGA
       (?\u03A9 :Omega (:greek :greek-capitals)
                (:OMEGA "OMEGA" ,@all)
                (:Omega "Omega" ,@all))

       ;; 03B1 α GREEK SMALL LETTER ALPHA
       (?\u03B1 :alpha (:greek :greek-lowercase)
                (:alpha "alpha" ,@all)
                (:\'a "'a" ,@mley))

       ;; 03B2 β GREEK SMALL LETTER BETA
       (?\u03B2 :beta (:greek :greek-lowercase)
                (:beta "beta" ,@all)
                (:\'b "'b" ,@mley))

       ;; 03B3 γ GREEK SMALL LETTER GAMMA
       (?\u03B3 :gamma (:greek :greek-lowercase)
                (:gamma "gamma" ,@all)
                (:\'c "'c" ,@mley))

       ;; 03B4 δ GREEK SMALL LETTER DELTA
       (?\u03B4 :delta (:greek :greek-lowercase)
                (:delta "delta" ,@all)
                (:\'d "'d" ,@mley))

       ;; 03B5 ε GREEK SMALL LETTER EPSILON
       (?\u03B5 :epsilon (:greek :greek-lowercase)
                (:epsilon "epsilon" ,@all)
                (:\'e "'e" ,@mley))

       ;; 03B6 ζ GREEK SMALL LETTER ZETA
       (?\u03B6 :zeta (:greek :greek-lowercase)
                (:zeta "zeta" ,@all))

       ;; 03B7 η GREEK SMALL LETTER ETA
       (?\u03B7 :eta (:greek :greek-lowercase)
                (:eta "eta" ,@all))

       ;; 03B8 θ GREEK SMALL LETTER THETA
       (?\u03B8 :theta (:greek :greek-lowercase)
                (:theta "theta" ,@all))

       ;; 03B9 ι GREEK SMALL LETTER IOTA
       (?\u03B9 :iota (:greek :greek-lowercase)
                (:iota "iota" ,@all))

       ;; 03BA κ GREEK SMALL LETTER KAPPA
       (?\u03BA :kappa (:greek :greek-lowercase)
                (:kappa "kappa" ,@all))

       ;; 03BB λ GREEK SMALL LETTER LAMDA
       (?\u03BB :lambda (:greek :greek-lowercase)
                (:lambda "lambda" ,@all))

       ;; 03BC μ GREEK SMALL LETTER MU
       (?\u03BC :mu (:greek :greek-lowercase)
                (:mu "mu" ,@all))

       ;; 03BD ν GREEK SMALL LETTER NU
       (?\u03BD :nu (:greek :greek-lowercase)
                (:nu "nu" ,@all))

       ;; 03BE ξ GREEK SMALL LETTER XI
       (?\u03BE :xi (:greek :greek-lowercase)
                (:xi "xi" ,@all))

       ;; 03BF ο GREEK SMALL LETTER OMICRON
       (?\u03BF :omicron (:greek :greek-lowercase)
                (:omicron "omicron" ,@all))

       ;; 03C0 π GREEK SMALL LETTER PI
       (?\u03C0 :pi (:greek :greek-lowercase)
                (:pi "pi" ,@all)
                (:M_PI "M_PI" c c++))

       ;; 03C1 ρ GREEK SMALL LETTER RHO
       (?\u03C1 :rho (:greek :greek-lowercase)
                (:rho "rho" ,@all))

       ;; 03C3 σ GREEK SMALL LETTER SIGMA
       (?\u03C3 :sigma (:greek :greek-lowercase)
                (:sigma "sigma" ,@all))

       ;; 03C4 τ GREEK SMALL LETTER TAU
       (?\u03C4 :tau (:greek :greek-lowercase)
                (:tau "tau" ,@all))

       ;; 03C5 υ GREEK SMALL LETTER UPSILON
       (?\u03C5 :upsilon (:greek :greek-lowercase)
                (:upsilon "upsilon" ,@all))

       ;; 03C6 φ GREEK SMALL LETTER PHI
       (?\u03C6 :phi (:greek :greek-lowercase)
                (:phi "phi" ,@all))

       ;; 03C7 χ GREEK SMALL LETTER CHI
       (?\u03C7 :chi (:greek :greek-lowercase)
                (:chi "chi" ,@all))

       ;; 03C8 ψ GREEK SMALL LETTER PSI
       (?\u03C8 :psi (:greek :greek-lowercase)
                (:psi "psi" ,@all))

       ;; 03C9 ω GREEK SMALL LETTER OMEGA
       (?\u03C9 :omega (:greek :greek-lowercase)
                (:omega "omega" ,@all))

       ;;; Punctuation

       ;; 2025 ‥ TWO DOT LEADER
       (?\u2025 :.. (:punctuation)
                (:.. ".." haskell ruby))

       ;; 2026 … HORIZONTAL ELLIPSIS
       (?\u2026 :dots (:punctuation)
                (:... "..." scheme ruby ess))

       ;; 203C ‼ DOUBLE EXCLAMATION MARK
       (?\u203C :!! (:punctuation)
                (:!! "!!" haskell))

       ;; 2218 ∘ RING OPERATOR
       (?\u2218 :circ (:punctuation)
                (:. "\." haskell))

       ;; 2237 ∷ PROPORTION
       (?\u2237 :Proportion (:punctuation)
                (::: "::" haskell))

       ;;; Types

       ;; 2124 ℤ DOUBLE-STRUCK CAPITAL Z
       (?\u2124 :Z (:types)
                (:Integer "Integer" haskell))

       ;;; Arrows

       ;; 2190 ← LEFTWARDS ARROW
       (?\u2190 :leftarrow (:arrows)
                (:<- "<-" ,@mley ess ,@lispy))

       ;; 219E ↞ LEFTWARDS TWO HEADED ARROW
       (?\u219E :twoheadleftarrow (:arrows :arrows-twoheaded)
                (:<<- "<<-" ess))

       ;; 2191 ↑ UPWARDS ARROW
       (?\u2191 :uparrow (:arrows)
                (:\\^ "\\^" tuareg))

       ;; 2192 → RIGHTWARDS ARROW
       (?\u2192 :rightarrow (:arrows)
                (:-> "->" ,@mley ess c c++ perl ,@lispy coffee groovy))

       ;; 21A0 ↠ RIGHTWARDS TWO HEADED ARROW
       (?\u21A0 :twoheadrightarrow (:arrows :arrows-twoheaded)
                (:->> "->>" ,@lispy ess))

       ;; 21D2 ⇒ RIGHTWARDS DOUBLE ARROW
       (?\u21D2 :Rightarrow (:arrows)
                (:=> "=>" sml perl ruby ,@lispy haskell coffee))

       ;; 21D4 ⇔ LEFT RIGHT DOUBLE ARROW
       (?\u21D4 :eftrightarrow (:arrows)
                (:<=> "<=>" groovy))

       ;; 2919 ⤙ LEFTWARDS ARROW-TAIL
       (?\u2919 :-< (:arrows :arrows-tails)
                (:-< "-<" haskell))

       ;; 291A ⤚ RIGHTWARDS ARROW-TAIL
       (?\u291A :>- (:arrows :arrows-tails)
                (:>- ">-" haskell))

       ;; 291B ⤛ LEFTWARDS DOUBLE ARROW-TAIL
       (?\u291B :-<< (:arrows :arrows-tails :arrows-tails-double)
                (:-<< "-<<" haskell))

       ;; 291C ⤜ RIGHTWARDS DOUBLE ARROW-TAIL
       (?\u291C :>>- (:arrows :arrows-tails :arrows-tails-double)
                (:>>- ">>-" haskell))

       ;;; Quantifiers

       ;; 2200 ∀ FOR ALL
       (?\u2200 :forall (:quantifiers)
                (:forall "forall" haskell))

       ;; 2203 ∃ THERE EXISTS
       (?\u2203 :exists (:quantifiers)
                (:exists "exists" haskell))

       ;;; Nil

       ;; 2205 ∅ EMPTY SET
       (?\u2205 :emptyset (:nil)
                (:nil "nil" emacs-lisp ruby clojure)
                (:null "null" scheme java coffee javascript)
                (:\'\(\) "'()" scheme)
                (:empty "empty" scheme)
                (:NULL "NULL" c c++ ess)
                (:None "None" python)
                (:\(\) "()" ,@mley)
                (:\[\] "[]" ,@mley))

       ;;; Arithmetic

       ;; 220F ∏ N-ARY PRODUCT
       (?\u220F :prod (:arithmetic :arithmetic-nary)
                (:product "product" haskell))

       ;; 2211 Σ N-ARY SUMMATION
       (?\u2211 :sum (:arithmetic :arithmetic-nary)
                (:sum "sum" python haskell))

       ;; 221a √ SQUARE ROOT
       (?\u221A :sqrt (:arithmetic)
                (:sqrt "sqrt" ,@all)
                (:Math.sqrt "Math.sqrt" javascript coffee ruby))

       ;; 29FA ⧺ DOUBLE PLUS
       (?\u29FA :++ (:arithmetic :arithmetic-double)
                (:++ "++" haskell c c++ java javascript coffee))

       ;; 29FB ⧻ TRIPLE PLUS
       (?\u29FB :+++ (:arithmetic :arithmetic-triple)
                (:+++ "+++" haskell))        ; Control.Arrow

       ;;; Undefined

       ;; 22A5 ⊥ UP TACK
       (?\u22A5 :bot (:undefined)
                (:undefined "undefined" haskell javascript coffee)
                (:void0 "void 0" javascript))

       ;;; Parentheses

       ;; 27E6 ⟦ MATHEMATICAL LEFT WHITE SQUARE BRACKET
       (?\u27E6 :llbracket (:parentheses)
                (:\[| "[|" haskell)
                (:\[\[ "[[" ess))

       ;; 27E7 ⟧ MATHEMATICAL RIGHT WHITE SQUARE BRACKET
       (?\u27E7 :rrbracket (:parentheses)
                (:|\] "|]" haskell)
                (:\]\] "]]" ess))

       ;; 2987 ⦇ Z NOTATION LEFT IMAGE BRACKET
       (?\u2987 :limg (:parentheses) ; \Lparen is actually a different symbol
                (:\(| "(|" haskell))

       ;; 2988 ⦈ Z NOTATION RIGHT IMAGE BRACKET
       (?\u2988 :rimg (:parentheses) ; \Rparen is actually a different symbol
                (:|\) "|)" haskell))

       ;;; Other

       ;; 2AF4 ⫴ TRIPLE VERTICAL BAR BINARY RELATION
       (?\u2AF4 :VERT (:other)
                (:||| "|||" haskell))        ; Control.Arrow
       ))))

(defun pretty-add-keywords (mode keywords)
  "Add pretty character KEYWORDS to MODE

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil. If nil, pretty keywords are added to the current
buffer. KEYWORDS should be a list where each element has the
form (REGEXP . CHAR). REGEXP will be replaced with CHAR in the
relevant buffer(s)."
  (font-lock-add-keywords
   mode (mapcar (lambda (kw) `(,(car kw)
                          (0 (prog1 nil
                               (compose-region (match-beginning 0)
                                               (match-end 0)
                                               ,(cdr kw))))))
                keywords)))

(defun pretty-regexp (regexp glyph)
  "Replace REGEXP with GLYPH in buffer."
  (interactive "MRegexp to replace:
MCharacter to replace with: ")
  (pretty-add-keywords nil `((,regexp . ,(string-to-char glyph))))
  (font-lock-fontify-buffer))

(provide 'pretty-mode)

;;; pretty-mode.el ends here
