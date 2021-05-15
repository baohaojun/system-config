;;; nim-smie.el --- Nim’s indent support library using smie.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuta Yamada
;; Author: Yuta Yamada

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
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is Nim’s indent calculation library using SMIE.
;;
;; TODO: remove copied if-let in nim-helper.el (after 25 is majored)
;;; Code:

(require 'subr-x nil t) ; for if-let (from Emacs 25.1)
(require 'nim-helper)
(require 'smie)   ; Emacs’ indentation library
(require 'let-alist)

;; INTERNAL VARIABLES
(defvar nim-smie--line-info nil)
(defvar nim-smie--defuns
  '("proc" "func" "method" "iterator" "template" "macro" "converter"))

(defconst nim-mode-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (any)
       ;; SMIE emits an warning if I use a token multiple times in
       ;; opener/neither/closer. So, I changed ; to __;__.
       ;; Could be used it to separate wrong associative?
       (module (stmt "__;__" stmt) (stmt))
       (stmt (exp))
       (exp (id) (exp) (virtual-indents))
       (virtual-indents (stmt "__after_break"))
       (& (exp "&" exp) (&))
       (concept ("concept" exp-comma))
       (import ("import" exp-comma))
       (exp-comma (exp "," exp exp-comma ";") (exp-comma))
       (var   ("var"   vlc-body))
       (let   ("let"   vlc-body))
       (const ("const" vlc-body))
       (vlc-body (vlc-body)
        (exp ":" exp vlc-body ";")
        (exp "=" exp vlc-body ";")
        (exp ":" exp "=" exp vlc-body ";"))
       (type  ("type"  type-constituent))
       (exp-colon    (exp ":" exp exp-colon ";") (exp-colon))
       (exp-eq       (exp "=" exp exp-eq    ";") (exp-eq))
       (exp-cl-eq    (exp-eq) (exp-colon))
       (enum-eq-comma (exp "=" exp "," enum-eq-comma) (enum-eq-comma))
       (type-constituent (exp-cl-eq)
                         (exp "=" "object" exp-colon)
                         (exp "=" "object" "of" exp-cl-eq)
                         (exp "=" "enum" enum-eq-comma)
                         (exp "=" "tuple" exp-colon)
                         (type-constituent))
       (func ("proc" func-body) ("func" func-body) ("method" func-body) ("iterator" func-body)
             ("template" func-body) ("macro" func-body) ("converter" func-body))
       (func-body (any "=" ";"))
       (inst3
        ("if" exp "elif" exp "else" ":" stmt)
        ("when" exp "elif" exp "else" ":" stmt)
        ("case" exp "of" exp "else" ":" stmt)
        ("case" exp "of" exp "elif" exp "else" ":" stmt)
        ("case" exp "elif" exp "else" ":" stmt)
        ("try" exp "except" exp "except" exp "finally" ":" stmt)
        ("while" any ":" stmt)
        ("for" any ":" stmt)
        ("block" any ":" stmt))
       (using ("using" exp-colon)))
     ;; You can choose: `assoc', `left', `right' or `nonassoc'.
     '((nonassoc "if" "when" "case" "for" "try")
       (assoc "of") (assoc "elif") (assoc "else"))
     '((assoc "case") (assoc "else") (assoc ":"))
     '((nonassoc "case" "object") (assoc "of"))
     '((assoc "for") (assoc ":"))
     '((assoc "try") (assoc "except") (assoc "finally") (assoc  ":"))
     '((assoc "=") (assoc "object" "concept"))
     ;; Functions
     `((assoc ,@nim-smie--defuns) (assoc "="))
     ;; While
     '((nonassoc "while" "block" "for") (assoc "break"))
     '((assoc "=") (nonassoc "block" "while"))
     '((assoc "if" "when") (assoc "elif") (assoc "else") (assoc ":"))
     ;; operators from nim manual
     '((assoc "$" "^") (assoc "*" "%" "\\" "/" "div" "mod" "shl" "shr")
       (assoc "+" "-" "~" "|"
              ;; Not sure this place is ok...
              "+%" "-%" "*%" "/%" "%%" "<%" "<=%")
       (right "&") (assoc "." "..")
       (assoc "=" "<" ">" "!"
              "==" "<=" "<" "!=" "in" "notin" "is" "isnot" "not" "of")
       (assoc "and") (assoc "or" "xor") (assoc "@" ":" "?")
       (assoc "+=" "*=") ("->" "=>"))
     '((assoc "=" ";") (assoc "__after_break"))))))

(defun nim-mode-smie-rules (kind token)
  "Nim-mode’s indent rules.
See also ‘smie-rules-function’ about KIND and TOKEN."
  (if-let* ((ind (nim-smie--condition-after-equal-p)))
      (cons 'column ind)
    (pcase (cons kind token)
      ;; Proc
      (`(:list-intro . "proc") (nim-smie--list-intro-proc))
      ;; Parenthesis
      (`(,(or :before :after) . ,(or "(" "{" "["))
       (nim-smie--paren kind token))
      (`(:close-all . ,(or "]" "}" ")"))
       (nim-smie--close-all token))
      ;; Comma
      (`(:before . ",") (nim-smie--comma))
      ;; "="
      (`(,(or :before :after :list-intro) . "=")
       (nim-smie--equal kind))
      ;; Conditions
      (`(:list-intro . ,(or "if" "when" "while" "elif" "block" "else" "of"))
       (nim-smie--list-intro-conditions))
      ;; object of/ case’s of
      (`(,_ . "object")
       (nim-smie--object kind))
      (`(,_ . "of")
       (nim-smie--of kind))
      ;; else
      (`(:before . "else") (nim-smie-rule-adjust-else-stmt))
      ;; var/let/const/type/import/using
      (`(:after . ,(or "var" "let" "const" "type" "import" "using"))
       nim-indent-offset)
      (`(:list-intro . ,(or "var" "let" "const" "type" "import"))
       (nim-smie--list-intro-vlcti token))
      (`(:list-intro . "concept")
       (nim-set-force-indent
        (save-excursion (back-to-indentation)
                        (+ (current-column) nim-indent-offset))))
      ;; enum
      (`(:before . "enum")
       (save-excursion (back-to-indentation)
                       (cons 'column (+ (current-column) nim-indent-offset))))
      ;; tuple
      (`(:before . "tuple")
       ;; ignore tuple inside proc’s args
       (if (member (nth 2 (smie-indent--parent)) nim-smie--defuns)
           0
         (save-excursion (back-to-indentation)
                         (cons 'column (+ (current-column) nim-indent-offset)))))
      ;; Colon
      (`(,(or :before :after :list-intro) . ":")
       (nim-smie--colon kind token))
      ;; &
      (`(,(or :after :list-intro) . "&")
       (nim-smie--& kind))
      ;; ‘empty-line-token’
      (`(:elem . empty-line-token)
       ;; This has to return token; not indent number.
       ;; see ‘smie-indent-keyword’.
       nil)
      (`(:elem . basic)
       (current-indentation))
      ;; break
      (`(,(or :before :after) . ,(or "break" "__after_break"))
       (nim-smie--break kind))
      ;; other keywords
      (`(:before . ,_))
      ;; Don’t make ambiguous indentation
      (_ 0))))

(defun nim-set-force-indent (indent &optional override)
  (when (or override (not (cdr (assoc :force-indent nim-smie--line-info))))
    (setf (cdr (assoc :force-indent nim-smie--line-info)) indent))
  nil)

(defun nim-traverse ()
  (when (looking-back "= +" nil)
    (search-backward "="))
  (while (nim-line-contain-p '(?\} ?\) ?\]) nil t)
    (condition-case nil (backward-sexp)))
  (goto-char (+ (point-at-bol) (current-indentation))))

(defun nim-same-closer-line-p ()
  (if-let* ((closer-line (assoc-default :closer-line nim-smie--line-info)))
      (= (line-number-at-pos) closer-line)))

(defun nim-smie--condition-after-equal-p ()
  "Check something like ’let x = if/when/case’ or not."
  (let-alist nim-smie--line-info
    (when (and .first-token.tk (equal "else" .first-token.tk))
      (let ((parent (and (smie-rule-parent-p "if" "when" "case")
                         (smie-indent--parent))))
        (when parent
          (save-excursion
            (goto-char (nth 1 parent))
            (when (looking-back (rx (1+ " " "=" (1+ " "))) nil)
              (current-indentation))))))))

(defun nim-smie--paren (kind token)
  (cl-case kind
    (:after
     (cons 'column (+ (current-indentation) nim-indent-offset)))
    (:before
     (nim-smie-before-paren-opener kind token))))

(defun nim-smie-before-paren-opener (_kind token)
  (if (or (and (equal "{" token)
               (eq ?. (char-after (1+ (point)))))
          (not (equal "{" token)))
      (let ((parent (when (member (nth 2 (smie-indent--parent)) nim-smie--defuns)
                      (smie-indent--parent)))
            (prev-info (nim-smie--get-prev-info)))
        (if parent
            (save-excursion
              (goto-char (nth 1 parent))
              (cond
               ((and (< (line-number-at-pos)
                        (assoc-default :line prev-info))
                     (= (current-indentation)
                        (assoc-default :indent prev-info)))
                (current-indentation))
               ((or (nim-smie--anonymous-proc-p)
                    (nim-smie--anonymous-proc-p nil t))
                (nim-traverse)
                (cons 'column (current-indentation)))
               ((nim-same-closer-line-p)
                (nim-traverse)
                (current-indentation))
               ((and (< (line-number-at-pos)
                        (assoc-default :line nim-smie--line-info))
                     (equal "{" token))
                (if (member "}" (assoc-default :closers nim-smie--line-info))
                    (current-indentation)
                  nim-smie-function-indent))))
          (nim-traverse)
          (cons 'column (current-indentation))))))

(defun nim-smie--close-all (token)
  (add-to-list 'nim-smie--line-info
               (cons :closer-line (line-number-at-pos)))
  (if (assoc-default :closers nim-smie--line-info)
      (setf (cdr (assoc-default :closers nim-smie--line-info))
            (append (assoc-default :closers nim-smie--line-info)
                    (list token)))
    (add-to-list 'nim-smie--line-info (cons :closers (list token))))
  t)

(defun nim-smie--get-prev-info ()
  (save-excursion
    (goto-char (assoc-default :start-pos nim-smie--line-info))
    (when (nim-line-move -1)
      (list (cons :line (line-number-at-pos))
            (cons :indent (current-indentation))))))

(defun nim-smie--anonymous-proc-p (&optional pos back)
  (save-excursion
    (when pos (goto-char pos))
    (if back
        (and (looking-at-p "(")
             (looking-back "[:=] *proc *" nil))
      (and (looking-at-p "proc *(")
           (looking-back "[:=] *" nil)))))

(defun nim-smie--list-intro-proc ()
  (cond
   ((nim-smie--anonymous-proc-p)
    (nim-set-force-indent (current-indentation)))
   ((save-excursion
      (goto-char (assoc-default :start-pos nim-smie--line-info))
      (back-to-indentation)
      (or (nim-line-empty-p)
          ;; forward declaration
          (looking-at-p "proc[ (]")))
    (nim-set-force-indent (current-indentation)))))

(defun nim-smie--colon (kind token)
  (cl-case kind
    (:after
     (cond
      ;; In nested ‘if’ and ‘when’ statement, we can't trust parent’s
      ;; indent because users only know the right place to indent.
      ((and (smie-rule-prev-p "else")
            (smie-rule-parent-p "if" "when"))
       (nim-traverse)
       (nim-set-force-indent (+ (current-indentation) nim-indent-offset)))
      ;; else after case statement
      ((and (smie-rule-prev-p "else")
            (smie-rule-parent-p "case"))
       (cons 'column (+ (cdr (nim-smie-rule-adjust-else-stmt))
                        nim-indent-offset)))
      ;; Proc
      ((member (nth 2 (smie-indent--parent)) nim-smie--defuns)
       (cond
        ;; if there is "=" until the proc, it means it’s
        ;; other thing’s ":". (not proc’s return type)
        ((assoc-default 'end-eq nim-smie--line-info)
         (nim-traverse)
         (cons 'column (+ (current-indentation) nim-indent-offset)))
        ((save-excursion
           (goto-char (nth 1 (smie-indent--parent)))
           (looking-back ": +" nil))
         (save-excursion
           (goto-char (nth 1 (smie-indent--parent)))
           (nim-set-force-indent (current-indentation))))))
      ;; tuple
      ((and (smie-rule-parent-p "tuple" "var"))
       (smie-rule-parent))
      ;; colon
      ((smie-rule-parent-p ":")
       (if (looking-at-p (nim-rx ":" (0+ " ") (or comment line-end)))
           (cons 'column (+ (current-indentation) nim-indent-offset))
         (smie-rule-parent)))
      ;; object
      ((smie-rule-parent-p "object")
       (let ((offset
              (if (save-excursion
                    (goto-char (assoc-default :start-pos nim-smie--line-info))
                    (nim-line-contain-p ?= (point-at-bol)))
                  0
                nim-indent-offset)))
         (save-excursion
           (goto-char (nth 1 (smie-indent--parent)))
           (nim-traverse)
           (cons 'column (+ (current-indentation) offset)))))
      ;; ‘object of’ doesn't use ‘=’, so we can safely dendent them.
      ((and (smie-rule-parent-p "of")
            (save-excursion (goto-char (nth 1 (smie-indent--parent)))
                            (looking-back "object +of +" nil)))
       (nim-smie--handle-object-of token))
      ;; General ":" rule
      (t
       (if-let* ((parent (smie-rule-parent nim-indent-offset)))
           parent
         (nim-traverse)
         (nim-set-force-indent (+ (current-indentation) nim-indent-offset))))))
    (:before
     ;; Indent after ":" for Nim’s control statements and macros
     (let-alist nim-smie--line-info
       (cond ((and (smie-rule-prev-p "else")
                   (smie-rule-parent-p "case"))
              (cons 'column (+ (cdr (nim-smie-rule-adjust-else-stmt))
                               nim-indent-offset)))
             ((and (smie-rule-parent-p "iterator")
                   (equal "=" .first-token.tk)
                   .first-token.eol)
              (smie-rule-parent))
             ((and (smie-rule-parent-p "var" "let" "const")
                   (not (looking-at (nim-rx ":" (0+ " ") (or comment line-end)))))
              (smie-rule-parent
               (save-excursion
                 (let* ((offset 0))
                   (goto-char (nth 1 (smie-indent--parent)))
                   (let ((parent-line (line-number-at-pos)))
                     (unless (= parent-line .first-token.line)
                       (setq offset (+ offset nim-indent-offset)))
                     (when (and (not (= .:line .first-token.line))
                                (member .first-token.tk '(":" "="))
                                .first-token.eol)
                       (setq offset (+ offset nim-indent-offset)))
                     (nim-set-force-indent (+ (current-indentation) offset)))))))
             (t nim-indent-offset))))
    (:list-intro
     (let-alist nim-smie--line-info
       (cond
        ;; ":" placed end of the line
        ((looking-at-p (nim-rx ":" (0+ " ") (or comment line-end)))
         (save-excursion
           (if (and (not (= .:line .first-token.line))
                    (= (current-indentation) .first-token.indent))
               (cons 'column (current-indentation)) ; set previous line’s indent
             (nim-traverse)
             (nim-set-force-indent (+ (current-indentation) nim-indent-offset)))))
        ;; single line var/let/const
        ((nim-get-indent-start-p '("var" "let" "const"))
         (nim-set-force-indent (current-indentation)))
        (t ; fallback (infix colon and there is no parent)
         (goto-char .first-token.pos)
         (nim-traverse)
         (nim-set-force-indent (current-indentation))))))))

(defun nim-smie--equal (kind)
  (cl-case kind
    ((:before :after)
     (if-let* ((parent (smie-rule-parent nim-indent-offset)))
         (save-excursion
           (let ((pos (point))
                 ;; check if current indentation is closer
                 ;; after the = {, (, or [
                 ;; (ex: after-open-bracket.nim)
                 (paren-offset
                  (when (looking-at-p
                         (rx "=" (0+ " ") (group (or "[" "{" "("))))
                    (skip-chars-forward " =")
                    (forward-sexp)
                    (when (eq (line-number-at-pos)
                              (assoc-default :line nim-smie--line-info))
                      0))))
             (goto-char (nth 1 (smie-indent--parent)))
             (let-alist nim-smie--line-info
               (cond
                ((nim-smie--anonymous-proc-p)
                 (save-excursion
                   (nim-traverse)
                   (cons 'column (+ (current-indentation) nim-indent-offset))))
                ((smie-rule-parent-p "var" "let" "const" "type")
                 (goto-char pos)
                 (nim-traverse)
                 (nim-set-force-indent
                  (+ (current-indentation)
                     (or paren-offset nim-indent-offset))))
                ;; after ":"
                ((and (equal ":" .first-token.tk)
                      .first-token.eol
                      (< (line-number-at-pos) .first-token.line)
                      (or
                       ;; if it doesn't relate to parent’s functions,
                       ;; follow ":"s indent.
                       (and .end-eq
                            (member (nth 2 (smie-indent--parent))
                                    nim-smie--defuns))
                       ;; on nimble file(nimscript), this pair (son:
                       ;; "=", parent "=") makes wrong indent.
                       (equal "=" (nth 2 (smie-indent--parent)))))
                 (save-excursion
                   (goto-char .first-token.pos)
                   (nim-traverse)
                   (nim-set-force-indent
                    (+ (current-indentation) nim-indent-offset))))
                (t
                 ;; There are certain situations what we should
                 ;; follow parent’s indention. (ex: iterator2.nim)
                 parent)))))
       nim-indent-offset))
    (:list-intro
     (save-excursion
       (nim-traverse)
       (if (nim-get-indent-start-p nil t)
           (nim-set-force-indent (+ (current-indentation) nim-indent-offset))
         (nim-set-force-indent (current-indentation)))))))

(defun nim-smie--comma ()
  (let ((ppss (syntax-ppss)))
    (when (< 0 (nth 0 ppss))
      (let (offset)
        (save-excursion
          (goto-char (nth 1 ppss))
          (if (looking-at-p
               (nim-rx (or "[" "{." "{" "(") (0+ " ") (or comment line-end)))
              (setq offset (+ (current-indentation) nim-indent-offset))
            (setq offset
                  (if (looking-at "{.")
                      (+ 2 (current-column))
                    (while (progn
                              (forward-char)
                              (looking-at (rx blank)))
                    (current-column)))))
          (cons 'column offset))))))

(defun nim-smie--of (kind)
  (cl-case kind
    (:after
     (when (smie-rule-prev-p "object") ; this is called from :elem args
       (nim-smie--handle-object-of)))
    (:before
     (let-alist nim-smie--line-info
       (cond
        ;; For case statement
        ((smie-rule-prev-p ":")
         nim-indent-offset)
        ;; detect "object of", but it's not really related to
        ;; current line.
        ((and (smie-rule-prev-p "object")
              (equal "=" .first-token.tk)
              .first-token.eol)
         (save-excursion
           (goto-char .first-token.pos)
           (nim-traverse)
           (nim-set-force-indent
            (+ (current-indentation) nim-indent-offset) t))))))))

(defun nim-smie--handle-object-of (&optional token)
  (let-alist nim-smie--line-info
    (if (save-excursion
          (goto-char .:start-pos)
          (nim-line-contain-p ?= (point-at-bol)))
        (save-excursion
          (goto-char (nth 1 (smie-indent--parent)))
          (nim-set-force-indent
           (cond
            ((member (nth 2 (smie-indent--parent)) '("type"))
             (+ (current-indentation) nim-indent-offset))
            ((member (nth 2 (smie-indent--parent)) '(":"))
             (goto-char .first-token.pos)
             (current-indentation))
            (t (current-indentation)))))
      (nim-set-force-indent
       (+ (current-indentation)
          (if (equal ":" token)
              0
            nim-indent-offset))))))

(defun nim-smie--object (kind)
  (let-alist nim-smie--line-info
    (cl-case kind
    (:before
     (cond
      ((and (smie-rule-prev-p "=")
            (smie-rule-parent-p "type"))
       (smie-rule-parent (* 2 nim-indent-offset)))
      ((and (or
             ;; most likely, this means detecting no related
             ;; token.
             (smie-rule-parent-p "of")
             ;; there is "object" on current line after "="
             (= .:line (line-number-at-pos)))
            .first-token.eol
            (equal "=" .first-token.tk))
       (save-excursion
         (goto-char .first-token.pos)
         (nim-traverse)
         (nim-set-force-indent (+ (current-indentation) nim-indent-offset))))
      (t
       (save-excursion
         (nim-traverse)
         (nim-set-force-indent (+ (current-column) nim-indent-offset))))))
    (:after
     (cond
      ((not (smie-rule-prev-p "=")) nil)
      ((smie-rule-parent-p "=" "type")
       ;; This means the "=" and "object" are on different lines.
       (when (smie-rule-bolp)
         (let ((dedent
                ;; if current line has "=", which means it’s not property of
                ;; the object and it should be dedented.
                (save-excursion
                  (goto-char (assoc-default :start-pos nim-smie--line-info))
                  (when (nim-line-contain-p ?= (point-at-bol))
                    (- nim-indent-offset)))))
           (cons 'column (+ (current-indentation)
                            (if dedent dedent nim-indent-offset)))))))))))

(defun nim-smie--list-intro-conditions ()
  ;; If it’s completed as one line, set indent forcefully
  (save-excursion
    (when (and
           (looking-at-p (rx (or "if" "when" "elif" "while" "else" "of")))
           (nim-line-contain-p ?: (point-at-eol) t))
      (nim-traverse)
      (nim-set-force-indent (current-indentation))))
  ;; When it’s non-nil, it organizes condition parts.
  ;; ex:
  ;;      if ...long multiple conditions...
  ;;         ...long multiple conditions...:
  ;;        ↑ indent like this
  t)

(defun nim-smie--list-intro-vlcti (token)
  (save-excursion
    (cond
     ;; ‘var’ type inside proc args has to be treated differently
     ((and (equal "var" token)
           (not (smie-rule-bolp)))
      (let ((ppss (syntax-ppss)))
        (if-let* ((open-paren (nth 1 ppss)))
            (when (< 0 (nth 0 ppss)) ; depth
              (goto-char open-paren)
              (if (eq ?\( (char-after (point)))
                  (nim-set-force-indent (1+ (current-column))))))))
     (t
      (if (looking-at-p (format "%s *\\(\s<.*\\)?*$" token))
          (nim-set-force-indent (+ (current-indentation) nim-indent-offset))
        (nim-set-force-indent (current-indentation)))))))

(defun nim-smie--& (kind)
  (cl-case kind
    (:after
     (save-excursion
       (condition-case _err
           (if (nim-previous-line-end-with '("&"))
               (cons 'column (+ (current-indentation)))
             (cons 'column (+ (current-indentation) nim-indent-offset)))
         (error
          ;; list-intro & -> :elem args -> here
          0))))
    (:list-intro
     (if (looking-at-p (nim-rx "&" (0+ " ") (? comment) line-end))
         nil
       (let ((offset (if (= (1+ (line-number-at-pos))
                            (assoc-default :line nim-smie--line-info))
                         0
                       (- nim-indent-offset))))
         (nim-traverse)
         (nim-set-force-indent (+ (current-indentation) offset)))))))

(defun nim-mode-forward-token ()
  (let ((_pos (point)))
    (skip-chars-forward " \t")
    (forward-comment (point-max))
    (let* ((tok (smie-default-forward-token)))
      ;; Put a terminator to restrict calculation
      (if (< (assoc-default :line nim-smie--line-info)
             (line-number-at-pos))
          (setq tok ";"))
      tok)))

(defun nim-mode-backward-token ()
  (let ((pos (point)))
    (forward-comment (- pos))
    (skip-chars-backward " \t")
    (let* ((tok (smie-default-backward-token))
           (tok1-pos (point)))
      (let ((tok2 (progn
                    (forward-comment (- pos))
                    (skip-chars-backward " \t")
                    (smie-default-backward-token)))
            (tok2-pos (point)))
        (goto-char tok1-pos)
        (cond
         ;; inside backquotes (`...`)
         ((and (eq ?` (char-before tok1-pos))
               (eq ?` (char-after (+ tok1-pos (length tok)))))
          (setq tok ""))
         ;; detect ‘object’ before ‘of’
         ((equal "of" tok2)
          (save-excursion
            (goto-char tok2-pos)
            (when (looking-back "object +" nil)
              (goto-char pos)
              (setq tok "of"))))
         ;; ‘break’
         ((or (equal "break" tok)
              (and (equal tok2 "break")
                   (looking-back "break +" nil)))
          (setq tok "__after_break"))
         ;; Functions
         ((member tok nim-smie--defuns)
          ;; check current token is not return type of function signature.
          ;; if so, avoid the token.
          (unless (eq (point) (+ (point-at-bol) (current-indentation)))
            (if-let* ((data (nim-get-indent-start-p nim-smie--defuns)))
                (progn (goto-char (car data))
                       (setq tok ".")))))
         ;; ignore dot
         ((equal "." tok)
          (setq tok ""))
         ;; ignore ‘,’ without suffix’s
         ((equal "," tok)
          (unless (looking-at-p (nim-rx "," (0+ " ") (or comment line-end)))
            (setq tok "")))
         ;; ignore ‘var’ if it starts with middle of the line to prevent
         ;; wrong indent calculation like:
         ;;   1. type a = concept var XXX
         ;;   2. proc x(a: var int): ...
         ;;   3. maybe more since nim can use the ‘var’ anywhere
         ((and (equal "var" tok)
               (not (nim-get-indent-start-p '("var"))))
          (setq tok "."))
         ;; Infix colon
         ((member tok2 '(":"))
          (if-let* ((data (nim-get-indent-start-p nil t)))
              (cond
               ((and (member (cdr data) '("if" "when" "elif" "while" "else" "of"))
                     ;; if the line is pair with else, don’t swap the token
                     (looking-back (rx symbol-start "else" symbol-end (0+ " ")) nil))
                (goto-char (car data))
                (setq tok (cdr data))))
            (cond
             ((and (not (assoc tok smie-closer-alist))
                   (not (rassoc tok smie-closer-alist))
                   (save-excursion
                     (goto-char tok2-pos)
                     (and
                      (not (nim-get-indent-start-p '("var" "let" "const")))
                      (not (looking-at-p
                            (nim-rx ":" (0+ " ") (or comment line-end)))))))
              (setq tok tok2)))))
         ((equal tok ":")
          (if-let* ((data (nim-get-indent-start-p nil t)))
              (when (member (cdr data) nim-smie--defuns)
                (setq tok "."))))
         ((equal tok "=")
          ;; keep info whether this function passed "="
          (when (looking-at (nim-rx "=" (0+ " ") (or comment line-end)))
            (add-to-list 'nim-smie--line-info (cons 'end-eq t))))
         ;; if the line is something like ‘else: ...’,
         ;; set "else" to tok.
         ((and
           (not (assoc-default 'first-token nim-smie--line-info))
           ;; check the else sentence is one line sentence
           (< (current-indentation) (- (current-column) 5))
           (not (equal "__after_break" tok)))
          (let ((data (nim-get-indent-start-p '("else"))))
            (when data
              (goto-char (car data))
              (setq tok "else"))))))
      (unless (assoc-default 'first-token nim-smie--line-info)
        (add-to-list 'nim-smie--line-info
                     (cons 'first-token
                           (list
                            `(tk     . ,tok)
                            `(pos    . ,(point))
                            `(indent . ,(current-indentation))
                            `(line   . ,(line-number-at-pos))
                            `(eol    . ,(looking-at (nim-rx any (0+ " ") (or comment line-end))))))))
      tok)))

(defun nim-get-indent-start-p (member &optional use-closer-alist)
  (save-excursion
    (goto-char (+ (point-at-bol)
                  (current-indentation)))
    (when (looking-at "\\(\\sw+\\)")
      (let ((match (match-string 1)))
        (if use-closer-alist
            (when (or (assoc match smie-closer-alist)
                      (rassoc match smie-closer-alist)
                      (member match nim-smie--defuns))
              (cons (point) match))
          (when (member match member)
            (cons (point) match)))))))

(defun nim-smie-rule-adjust-else-stmt ()
  "If case statement ends with colon, it should be indented."
  (when (smie-rule-parent-p "case")
    (save-excursion
      (let ((parent (smie-indent--parent)))
        (cond
         ((and (equal "case" (nth 2 parent))
               ;; Don’t search if the "case" is single line stmt.
               (< (nth 1 parent) (point-at-bol)))
          (goto-char (nth 1 parent))
          (let* (target-token)
            (while (and (not (member (car target-token) '("of" "else")))
                        (not (eobp)))
              (setq target-token (smie-indent-forward-token)))
            (when (equal "of" (car target-token))
              (cons 'column (- (current-column) 2))))))))))

(defun nim-smie--break (kind)
  (cl-case kind
    (:after ; __after_break
     ;; dedent after break. Note that this token (break -> __after_break)
     ;; conversion is needed following case:
     ;;
     ;;   while true:
     ;;     echo "process"
     ;;     if true:
     ;;       echo "something"
     ;;       break
     ;;     echo "<- after break should be dedented"
     ;;
     ;; If you use "break" instead, it destructs if/elif/else’s
     ;; indent logic after break.
     (cond ((and (smie-rule-hanging-p)
                 (nim-line-contain-p '(?: ?=) (point) t))
            (if (nim-get-indent-start-p nim-smie-dedent-after-break)
                ;; if the line is "else: break" statement (in one line),
                ;; next line will be dedented.
                (nim-set-force-indent
                 (- (current-indentation) nim-indent-offset))
              (nim-traverse)
              (cons 'column (+ (current-indentation)))))
           (t (nim-set-force-indent
               (- (current-indentation) nim-indent-offset)))))
    (:before ; break
     (if-let* ((parent (smie-rule-parent-p "while" "block" "for")))
         (smie-rule-parent
          (cond
           ((smie-rule-prev-p ":")
            ;; For:
            ;;   for x in foo:
            ;;     if true: break
            ;;     echo "close if's indent"
            (let ((bol (point-at-bol)))
              (save-excursion
                (nim-mode-backward-token)
                (if (< (point) bol)
                    (* nim-indent-offset 2)
                  ;; TODO: make sure this case
                  nim-indent-offset))))
           (t
            ;; For:
            ;;   for x in foo:
            ;;     ...
            ;;     if condition:
            ;;       xxx
            ;;       break <- indent until if’s scope
            (save-excursion
              (goto-char (1- (point-at-bol)))
              (let ((previous-line (current-indentation))
                    (parent-indent
                     (progn (goto-char (nth 1 (smie-indent--parent)))
                            (current-column))))
                (- previous-line parent-indent))))))
       (when (eq (assoc-default :line nim-smie--line-info)
                 (line-number-at-pos))
         (save-excursion
           (when (nim-line-move -1)
             (nim-traverse)
             (nim-set-force-indent (current-indentation)))))))))

(defun nim-previous-line-end-with (strings)
  (save-excursion
    (let ((start-line (line-number-at-pos)))
      (goto-char (point-at-bol))
      (forward-comment (- (point)))
      (when (< (line-number-at-pos) start-line)
        (let ((c (char-before (point))))
          (when c (member (char-to-string c) strings)))))))

(defun nim-get-comment-indent ()
  "Return indent number for comment.
This works if only current line starts from comment."
  (save-excursion
    (goto-char (assoc-default :start-pos nim-smie--line-info))
    (if-let* ((column (and (not (eq (point-min) (point-at-bol)))
                          (not (eq ?\\ (char-before (- (point-at-bol) 1))))
                          (nim-line-comment-p nil (- (point-at-bol) 2)))))
        ;; when it is called from :list-intro, need to indent forcefully
        column
      (cond
       ((and (not (nim-line-comment-p -1))
             (eq ?# (char-after (+ 1 (point-at-bol) (current-indentation))))
             (nim-line-move -1))
        (goto-char (point-at-eol))
        (nim-traverse)
        (+ (current-indentation) nim-indent-offset))
       ((nim-line-empty-p -1)
        (if (nim-line-empty-p -2)
            0
          (when (nim-line-move -1)
            (nim-smie-indent-calculate))))
       ((nim-previous-line-end-with '(":" "="))
        (when (nim-line-move -1)
          (+ (current-indentation) nim-indent-offset)))
       (t
        (when (nim-line-move -1)
          (nim-traverse)
          (current-indentation)))))))

(defun nim-get-comment-start-point ()
  "Return comment starting point."
  (if-let* ((ppss (and
                  (not (eq (point-min) (point-at-bol)))
                  (save-excursion (syntax-ppss (- (point-at-bol) 2))))))
      (when (eq t (nth 4 ppss))
        (nth 8 ppss))))

(defun nim--indent-line-core (&optional previous)
  "Internal implementation of `nim-indent-line'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (if-let* ((indent (nim-indent-calculate-indentation previous)))
          (progn
            (indent-line-to indent)
            (run-hooks 'nim-smie-after-indent-hook))))
    (when follow-indentation-p
      (back-to-indentation))))

(defun nim-indent-line ()
  "`indent-line-function' for Nim mode.
When the variable `last-command' is equal to one of the symbols
inside `nim-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (nim--indent-line-core
   (and (memq this-command nim-indent-trigger-commands)
        (eq last-command this-command))))

(defun nim-get-empty-line-indent ()
  (save-excursion
    (let (empty-line)
      (when (or (nim-line-empty-p -1)
                (when (nim-line-comment-p -1)
                  (save-excursion
                    (forward-line 0)
                    (catch 'exit
                      (while (and (not (eq (point-at-bol) (point-min)))
                                  (nim-line-move -1))
                        (when (not (looking-at "^ *?\\s<"))
                          (if (nim-line-empty-p)
                              (setq empty-line (point)))
                          (throw 'exit t))))
                    empty-line)))
        (goto-char (point-at-bol))
        (when (looking-at "^ ?+\\(\\sw+\\)")
          (let ((str (match-string 1)))
            (cond
             ((and nim-smie-indent-stoppers
                   (if empty-line
                       (save-excursion
                         (goto-char empty-line)
                         (nim-line-empty-p -1 t))
                     (nim-line-empty-p -2 t))
                   (member str nim-smie-indent-stoppers))
              0)
             ((and
               nim-smie-indent-dedenters
               (or (eq 'all-dedent nim-smie-indent-dedenters)
                   (member str nim-smie-indent-dedenters)))
              (save-excursion
                (if empty-line
                    (goto-char empty-line)
                  (nim-line-move -1))
                (when (nim-line-move -1)
                  (let ((reference (current-indentation)))
                    (unless (eq 0 reference)
                      (while (and (nim-line-move -1)
                                  (not (< (current-indentation) reference)))
                        nil))
                    (current-indentation))))))))))))

(defun nim-smie-indent-calculate ()
  (setq nim-smie--line-info
        (list (cons :start-pos (point))
              (cons :comment (nim-line-comment-p))
              (cons :force-indent (nim-get-empty-line-indent))
              (cons :line (line-number-at-pos))))
  (if-let* ((empty-line-indent
             (assoc-default :force-indent nim-smie--line-info)))
      empty-line-indent
    (let* ((savep (point))
           (indent (or (with-demoted-errors
                           (save-excursion
                             (forward-line 0)
                             (skip-chars-forward " \t")
                             (if (>= (point) savep) (setq savep nil))
                             (or (smie-indent-calculate) 0)))
                       0)))
      (cond ((eq 'noindent indent)
             ;; For inside string
             nil)
            ((assoc-default :comment nim-smie--line-info)
             ;; Comment
             (nim-get-comment-indent))
            ((or (not (numberp indent))
                 (< indent 0))
             0)
            (t (or (assoc-default :force-indent nim-smie--line-info) indent))))))

(defun nim-indent-calculate-levels ()
  "Return possible indentation levels."
  (nim-indent--calculate-levels
   (nim-smie-indent-calculate)))

(defun nim-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (if-let* ((indentation (nim-smie-indent-calculate)))
      (let ((levels (nim-indent--calculate-levels indentation)))
        (if previous
            (nim-indent--previous-level levels (current-indentation))
          (if levels
              (apply #'max levels)
            0)))))

(defun nim-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun nim-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (let* ((remainder (% indentation nim-indent-offset))
           (steps (/ (- indentation remainder) nim-indent-offset))
           (levels (mapcar (lambda (step)
                             (* nim-indent-offset step))
                           (number-sequence steps 0 -1))))
      (reverse
       (if (not (zerop remainder))
           (cons indentation levels)
         levels)))))

;;; For debug
;; or you can use ‘smie-edebug’.
(defun nim-debug-smie-rules (kind token)
  (let ((fmt (concat "kind(%s)-Token(%s)-Point(%d)\n"
                     "sibling(%s)-bolp(%s)\n"
                     "parent(%s)-hanging(%s)\n"
                     "line-info(%s)\n")))
    (message (format fmt kind token (point)
                     (ignore-errors (smie-rule-sibling-p))
                     (ignore-errors (smie-rule-bolp))
                     (ignore-errors (smie-indent--parent))
                     (ignore-errors (smie-rule-hanging-p))
                     nim-smie--line-info))))
;; (advice-add 'nim-mode-smie-rules :before #'nim-debug-smie-rules)

(provide 'nim-smie)
;;; nim-smie.el ends here
