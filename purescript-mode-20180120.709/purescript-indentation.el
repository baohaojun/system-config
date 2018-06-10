;;; purescript-indentation.el -- indentation module for PureScript Mode

;; Copyright (C) 2009  Kristof Bastiaensen

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>

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

;; Installation:
;;
;; To turn indentation on for all PureScript buffers under PureScript mode
;; <http://www.purescript.org/purescript-mode/> add this to .emacs:
;;
;;    (add-hook purescript-mode-hook 'turn-on-purescript-indentation)
;;
;; Otherwise, call `purescript-indentation-mode'.

;;; Code:

(require 'syntax)
(with-no-warnings (require 'cl))

(defvar delete-active-region)

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)
(defvar purescript-literate)

(defgroup purescript-indentation nil
  "PureScript indentation."
  :link '(custom-manual "(purescript-mode)Indentation")
  :group 'purescript
  :prefix "purescript-indentation-")

(defcustom purescript-indentation-cycle-warn t
  "Warn before moving to the leftmost indentation, if you tab at the rightmost one."
  :type 'boolean
  :group 'purescript-indentation)

(defcustom purescript-indentation-delete-backward-indentation t
  "Delete backward removes indentation."
  :type 'boolean
  :group 'purescript-indentation)

(defcustom purescript-indentation-delete-backward-jump-line nil
  "Delete backward jumps to the previous line when removing last indentation."
  :type 'boolean
  :group 'purescript-indentation)

(defcustom purescript-indentation-delete-indentation t
  "Delete removes indentation."
  :type 'boolean
  :group 'purescript-indentation)

(defcustom purescript-indentation-layout-offset 2
  "Extra indentation to add before expressions in a purescript layout list."
  :type 'integer
  :group 'purescript-indentation)

(defcustom purescript-indentation-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'purescript-indentation)

(defcustom purescript-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'purescript-indentation)

(defcustom  purescript-indentation-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'purescript-indentation)

(defcustom purescript-indentation-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'purescript-indentation)

(defcustom purescript-indentation-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'purescript-indentation)

(defcustom purescript-indentation-birdtrack-extra-space t
  "Append a space after every birdtrack in literate mode."
  :type 'boolean
  :group 'purescript-indentation)


;; Avoid a global bogus definition (which the original run-time
;; `defun' made), and support Emacs 21 without the syntax.el add-on.
(eval-when-compile
  (unless (fboundp 'syntax-ppss)
    (defsubst syntax-ppss (&rest pos)
      (parse-partial-sexp (point-min) (or pos (point))))))

(defconst purescript-indentation-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\r] 'purescript-newline-and-indent)
    (define-key keymap [backspace] 'purescript-indentation-delete-backward-char)
    (define-key keymap [?\C-d] 'purescript-indentation-delete-char)
    keymap))

;; Used internally
(defvar purescript-indent-last-position)

;;;###autoload
(define-minor-mode purescript-indentation-mode
  "PureScript indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode."
  :lighter " Ind"
  :keymap purescript-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when purescript-indentation-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function)
         'purescript-indentation-indent-line)
    (set (make-local-variable 'normal-auto-fill-function)
         'purescript-indentation-auto-fill-function)
    (set (make-local-variable 'purescript-indent-last-position)
         nil)))

;;;###autoload
(defun turn-on-purescript-indentation ()
  "Turn on the purescript-indentation minor mode."
  (interactive)
  (purescript-indentation-mode t))

(put 'parse-error
     'error-conditions
     '(error parse-error))
(put 'parse-error 'error-message "Parse error")

(defun parse-error (&rest args)
  (signal 'parse-error (apply 'format args)))

(defmacro on-parse-error (except &rest body)
  `(condition-case parse-error-string
       (progn ,@body)
     (parse-error
      ,except
      (message "%s" (cdr parse-error-string)))))

(defun purescript-current-column ()
  "Compute current column according to purescript syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (incf cc))
        (forward-char))
      cc)))

(defun kill-indented-line (&optional arg)
  "`kill-line' for indented text.
Preserves indentation and removes extra whitespace"
  (interactive "P")
  (let ((col (purescript-current-column))
        (old-point (point)))
    (cond ((or (and (numberp arg) (< arg 0))
               (and (not (looking-at "[ \t]*$"))
                    (or (not (numberp arg)) (zerop arg))))
                                        ;use default behavior when calling with a negative argument
                                        ;or killing (once) from the middle of a line
           (kill-line arg))
          ((and (skip-chars-backward " \t") ;always true
                (bolp)
                (save-excursion
                  (forward-line arg)
                  (not (looking-at "[ \t]*$"))))
                                        ; killing from an empty line:
                                        ; preserve indentation of the next line
           (kill-region (point)
                        (save-excursion
                          (forward-line arg)
                          (point)))
           (skip-chars-forward " \t")
           (if (> (purescript-current-column) col)
               (move-to-column col)))
          (t                            ; killing from not empty line:
                                        ; kill all indentation
           (goto-char old-point)
           (kill-region (point)
                        (save-excursion
                          (forward-line arg)
                          (skip-chars-forward " \t")
                          (point)))))))

(defun purescript-indentation-auto-fill-function ()
  (when (> (purescript-current-column) fill-column)
    (while (> (purescript-current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
          (indent (car (last (purescript-indentation-find-indentations)))))
      (newline)
      (when (eq purescript-literate 'bird)
        (insert ">"))
      (indent-to indent)
      (end-of-line))))

(defun purescript-indentation-reindent (col)
  (beginning-of-line)
  (delete-region (point)
                 (progn
                   (when (and (eq purescript-literate 'bird)
                              (eq (char-after) ?>))
                     (forward-char))
                   (skip-syntax-forward "-")
                   (point)))
  (when (eq purescript-literate 'bird)
    (insert ">"))
  (indent-to col))

(defun purescript-indentation-current-indentation ()
  (if (eq purescript-literate 'bird)
      (save-excursion
        (beginning-of-line)
        (forward-char)
        (skip-syntax-forward "-")
        (current-column))
    (current-indentation)))

(defun purescript-indentation-outside-bird-line ()
  (and (eq purescript-literate 'bird)
       (or (< (current-column) 2)
           (save-excursion
             (beginning-of-line)
             (not (eq (char-after) ?>))))))

(defun purescript-newline-and-indent ()
  (interactive)
  (if (purescript-indentation-outside-bird-line)
      (progn
        (delete-horizontal-space)
        (newline))
    (on-parse-error
     (newline)
     (let* ((cc (purescript-current-column))
            (ci (purescript-indentation-current-indentation))
            (indentations (purescript-indentation-find-indentations)))
       (skip-syntax-forward "-")
       (if (prog1 (and (eolp)
                       (not (= (purescript-current-column) ci)))
             (delete-horizontal-space)
             (if (not (eq purescript-literate 'bird))
                 (newline)
               (when purescript-indentation-birdtrack-extra-space
                 (indent-to 2))
               (newline)
               (insert "> ")))
           (purescript-indentation-reindent
            (max (purescript-indentation-butlast indentations)
                 (purescript-indentation-matching-indentation
                  ci indentations)))
         (purescript-indentation-reindent (purescript-indentation-matching-indentation
                                        cc indentations)))))))

(defun purescript-indentation-one-indentation (col indentations)
  (let* ((last-pair (last indentations)))
    (cond ((null indentations)
           col)
          ((null (cdr indentations))
           (car indentations))
          ((<= col (car last-pair))
           col)
          (t (car last-pair)))))

(defun purescript-indentation-butlast (indentations)
  (when (consp (cdr indentations))
    (while (cddr indentations)
      (setq indentations (cdr indentations))))
  (car indentations))

(defun purescript-indentation-next-indentation (col indentations)
  "Find the lefmost indentation which is greater than COL."
  (catch 'return
    (while indentations
      (if (or (< col (car indentations))
              (null (cdr indentations)))
          (throw 'return (car indentations))
        (setq indentations (cdr indentations))))
    col))

(defun purescript-indentation-previous-indentation (col indentations)
  "Find the rightmost indentation which is less than COL."
  (and indentations
       (> col (car indentations))
       (catch 'return
         (while indentations
           (if (or (null (cdr indentations))
                   (<= col (cadr indentations)))
               (throw 'return (car indentations))
             (setq indentations (cdr indentations))))
         col)))

(defun purescript-indentation-matching-indentation (col indentations)
  "Find the leftmost indentation which is greater than or equal to COL."
  (catch 'return
    (while indentations
      (if (or (<= col (car indentations))
              (null (cdr indentations)))
          (throw 'return (car indentations))
        (setq indentations (cdr indentations))))
    col))

(defun purescript-indentation-indent-line ()
  (when (save-excursion
          (beginning-of-line)
          (not (nth 8 (syntax-ppss))))
    (let ((ci (purescript-indentation-current-indentation))
          (start-column (purescript-current-column)))
      (cond ((> (purescript-current-column) ci)
             (save-excursion
               (move-to-column ci)
               (purescript-indentation-reindent
                (purescript-indentation-one-indentation
                 ci (purescript-indentation-find-indentations)))))

            ((= (purescript-current-column) ci)
             (purescript-indentation-reindent
              (purescript-indentation-next-indentation
               ci (purescript-indentation-find-indentations))))

            (t (move-to-column ci)
               (purescript-indentation-reindent
                (purescript-indentation-matching-indentation
                 ci (purescript-indentation-find-indentations)))))
      (cond ((not (= (purescript-current-column) start-column))
             (setq purescript-indent-last-position nil))
            ((not purescript-indentation-cycle-warn)
             (purescript-indentation-reindent
              (purescript-indentation-next-indentation
               -1
               (purescript-indentation-find-indentations))))
            ((not (equal (point) purescript-indent-last-position))
             (message "Press TAB again to go to the leftmost indentation")
             (setq purescript-indent-last-position (point)))
            (t
             (purescript-indentation-reindent
              (purescript-indentation-next-indentation
               -1
               (purescript-indentation-find-indentations))))))))

(defun purescript-indentation-delete-backward-char (n)
  (interactive "p")
  (on-parse-error
   (delete-char (- n))
   (cond
    ((purescript-indentation-outside-bird-line)
     (delete-char (- n)))
    ((and (use-region-p)
          delete-active-region
          (not (= (point) (mark))))
     (delete-region (mark) (point)))
    ((or (= (purescript-current-column) 0)
         (> (purescript-current-column) (purescript-indentation-current-indentation))
         (nth 8 (syntax-ppss)))
     (delete-char (- n)))
    (purescript-indentation-delete-backward-indentation
     (let* ((ci (purescript-indentation-current-indentation))
            (pi (purescript-indentation-previous-indentation
                 ci (purescript-indentation-find-indentations))))
       (save-excursion
         (cond (pi
                (move-to-column pi)
                (delete-region (point)
                               (progn (move-to-column ci)
                                      (point))))
               (t
                (if (not purescript-indentation-delete-backward-jump-line)
                    (delete-char (- 1))
                  (beginning-of-line)
                  (delete-region (max (point-min) (- (point) 1))
                                 (progn (move-to-column ci)
                                        (point)))))))))
    (t (delete-char (- n))))))

(defun purescript-indentation-delete-char (n)
  (interactive "p")
  (if (purescript-indentation-outside-bird-line)
      (delete-char n)
    (on-parse-error (delete-char n)
                    (cond
                     ((and delete-selection-mode
                           mark-active
                           (not (= (point) (mark))))
                      (delete-region (mark) (point)))
                     ((and (eq purescript-literate 'bird)
                           (looking-at "\n> "))
                      (delete-char (+ n 2)))
                     ((or (eolp)
                          (>= (purescript-current-column) (purescript-indentation-current-indentation))
                          (nth 8 (syntax-ppss)))
                      (delete-char n))
                     (purescript-indentation-delete-indentation
                      (let* ((ci (purescript-indentation-current-indentation))
                             (pi (purescript-indentation-previous-indentation
                                  ci (purescript-indentation-find-indentations))))
                        (save-excursion
                          (if (and pi (> pi (purescript-current-column)))
                              (move-to-column pi))
                          (delete-region (point)
                                         (progn (move-to-column ci)
                                                (point))))))
                     (t (delete-char (- n)))))))

(defun purescript-indentation-goto-least-indentation ()
  (beginning-of-line)
  (if (eq purescript-literate 'bird)
      (catch 'return
        (while t
          (when (not (eq (char-after) ?>))
            (forward-line)
            (forward-char 2)
            (throw 'return nil))
          (let ((ps (nth 8 (syntax-ppss))))
            (when ps ;; inside comment or string
              (goto-char ps)
              (beginning-of-line)))
          (when (and (>= 2 (purescript-indentation-current-indentation))
                     (not (looking-at ">\\s-*$")))
            (forward-char 2)
            (throw 'return nil))
          (when (bobp)
            (forward-char 2)
            (throw 'return nil))
          (forward-line -1)))
    ;; not bird style
    (catch 'return
      (while (not (bobp))
        (forward-comment (- (buffer-size)))
        (beginning-of-line)
        (let ((ps (nth 8 (syntax-ppss))))
          (when ps ;; inside comment or string
            (goto-char ps)))
        (when (= 0 (purescript-indentation-current-indentation))
          (throw 'return nil))))
    (beginning-of-line)
    (when (bobp)
      (forward-comment (buffer-size)))))

(defun purescript-indentation-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (parse-line-number 0)
          (current-indent purescript-indentation-layout-offset)
          (starter-indent purescript-indentation-layout-offset)
          (left-indent purescript-indentation-layout-offset)
          (case-fold-search nil)
          current-token
          following-token
          possible-indentations)
      (purescript-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
          (purescript-indentation-first-indentation)
        (setq current-token (purescript-indentation-peek-token))
        (catch 'parse-end
          (purescript-indentation-toplevel)
          (unless (eq current-token 'end-tokens)
            (parse-error "Illegal token: %s" current-token)))
        possible-indentations))))

(defun purescript-indentation-first-indentation ()
  (if (eq purescript-literate 'bird) '(2) '(0)))

(defun purescript-indentation-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (purescript-indentation-first-indentation))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (purescript-indentation-parse-to-indentations)
        (purescript-indentation-first-indentation)))
     (t
      (purescript-indentation-parse-to-indentations)))))

(defconst purescript-indentation-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("↢" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("↣" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation dictionary from UnicodeSyntax tokens to their ASCII representation.")

(defconst purescript-indentation-toplevel-list
  '(("module" . purescript-indentation-module)
    ("data" . (lambda () (purescript-indentation-statement-right #'purescript-indentation-data)))
    ("type" . (lambda () (purescript-indentation-statement-right #'purescript-indentation-data)))
    ("newtype" . (lambda () (purescript-indentation-statement-right #'purescript-indentation-data)))
    ("class" . purescript-indentation-class-declaration)
    ("instance" . purescript-indentation-class-declaration )))

(defconst purescript-indentation-type-list
  '(("::"    . (lambda () (purescript-indentation-with-starter
                           (lambda () (purescript-indentation-separated #'purescript-indentation-type "->" nil)) nil)))
    ("("     . (lambda () (purescript-indentation-list #'purescript-indentation-type
                                                    ")" "," nil)))
    ("["     . (lambda () (purescript-indentation-list #'purescript-indentation-type
                                                    "]" "," nil)))
    ("{"     . (lambda () (purescript-indentation-list #'purescript-indentation-type
                                                    "}" "," nil)))))

(defconst purescript-indentation-expression-list
  '(("data" . purescript-indentation-data)
    ("type" . purescript-indentation-data)
    ("newtype" . purescript-indentation-data)
    ("if"    . (lambda () (purescript-indentation-phrase
                           '(purescript-indentation-expression
                             "then" purescript-indentation-expression
                             "else" purescript-indentation-expression))))
    ("let"   . (lambda () (purescript-indentation-phrase
                           '(purescript-indentation-declaration-layout
                             "in" purescript-indentation-expression))))
    ("do"    . (lambda () (purescript-indentation-with-starter
                           #'purescript-indentation-expression-layout nil)))
    ("mdo"   . (lambda () (purescript-indentation-with-starter
                           #'purescript-indentation-expression-layout nil)))
    ("rec"   . (lambda () (purescript-indentation-with-starter
                           #'purescript-indentation-expression-layout nil)))
    ("case"  . (lambda () (purescript-indentation-phrase
                           '(purescript-indentation-expression
                             "of" purescript-indentation-case-layout))))
    ("\\"    . (lambda () (purescript-indentation-with-starter
                           #'purescript-indentation-lambda-maybe-lambdacase nil)))
    ("proc"  . (lambda () (purescript-indentation-phrase
                           '(purescript-indentation-expression
                             "->" purescript-indentation-expression))))
    ("where" . (lambda () (purescript-indentation-with-starter
                           #'purescript-indentation-declaration-layout nil t)))
    ("::"    . (lambda () (purescript-indentation-with-starter
                           (lambda () (purescript-indentation-separated #'purescript-indentation-type "->" nil)) nil)))
    ("="     . (lambda () (purescript-indentation-statement-right #'purescript-indentation-expression)))
    ("<-"    . (lambda () (purescript-indentation-statement-right #'purescript-indentation-expression)))
    ("("     . (lambda () (purescript-indentation-list #'purescript-indentation-expression
                                                    ")" '(list "," "->") nil)))
    ("["     . (lambda () (purescript-indentation-list #'purescript-indentation-expression
                                                    "]" "," "|")))
    ("{"     . (lambda () (purescript-indentation-list #'purescript-indentation-expression
                                                    "}" "," nil)))))

(defun purescript-indentation-expression-layout ()
  (purescript-indentation-layout #'purescript-indentation-expression))

(defun purescript-indentation-declaration-layout ()
  (purescript-indentation-layout #'purescript-indentation-declaration))

(defun purescript-indentation-case-layout ()
  (purescript-indentation-layout #'purescript-indentation-case))

;; After a lambda (backslash) there are two possible cases:
;;   - the new lambdacase expression, that can be recognized by the
;;     next token being "case",
;;   - or simply an anonymous function definition in the form of
;;     "expression -> expression".
(defun purescript-indentation-lambda-maybe-lambdacase ()
  (if (string= current-token "case")
      (purescript-indentation-with-starter
       #'purescript-indentation-case-layout nil)
    (purescript-indentation-phrase-rest
     '(purescript-indentation-expression "->" purescript-indentation-expression))))

(defun purescript-indentation-fundep ()
  (purescript-indentation-with-starter
   (lambda () (purescript-indentation-separated
               #'purescript-indentation-fundep1 "," nil))
   nil))

(defun purescript-indentation-fundep1 ()
  (let ((current-indent (purescript-current-column)))
    (while (member current-token '(value "->"))
      (purescript-indentation-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (purescript-indentation-add-indentation current-indent))))

(defun purescript-indentation-toplevel ()
  (purescript-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token purescript-indentation-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (purescript-indentation-declaration))))))

(defun purescript-indentation-type ()
  (let ((current-indent (purescript-current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->"))
          (purescript-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "->" "(" "[" "{" "::"))
            (purescript-indentation-add-indentation current-indent))
          (throw 'return nil))

         (t (let ((parser (assoc current-token purescript-indentation-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

(defun purescript-indentation-data ()
  (purescript-indentation-with-starter
   (lambda ()
     (when (string= current-token "instance")
       (purescript-indentation-read-next-token))
     (purescript-indentation-type)
     (cond ((string= current-token "=")
            (purescript-indentation-with-starter
             (lambda () (purescript-indentation-separated #'purescript-indentation-type "|" "deriving"))
             nil))
           ((string= current-token "where")
            (purescript-indentation-with-starter
             #'purescript-indentation-expression-layout nil))))
   nil))

(defun purescript-indentation-class-declaration ()
  (purescript-indentation-with-starter
   (lambda ()
     (purescript-indentation-type)
     (when (string= current-token "|")
       (purescript-indentation-fundep))
     (when (string= current-token "where")
       (purescript-indentation-with-starter
        #'purescript-indentation-expression-layout nil)))
   nil))

(defun purescript-indentation-module ()
  (purescript-indentation-with-starter
   (lambda ()
     (let ((current-indent (purescript-current-column)))
       (purescript-indentation-read-next-token)
       (when (string= current-token "(")
         (purescript-indentation-list
          #'purescript-indentation-module-export
          ")" "," nil))
       (when (eq current-token 'end-tokens)
         (purescript-indentation-add-indentation current-indent)
         (throw 'parse-end nil))
       (when (string= current-token "where")
         (purescript-indentation-read-next-token)
         (when (eq current-token 'end-tokens)
           (purescript-indentation-add-layout-indent)
           (throw 'parse-end nil))
         (purescript-indentation-layout #'purescript-indentation-toplevel))))
   nil))

(defun purescript-indentation-module-export ()
  (cond ((string= current-token "module")
         (let ((current-indent (purescript-current-column)))
           (purescript-indentation-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (purescript-indentation-add-indentation current-indent))
                 ((eq current-token 'value)
                  (purescript-indentation-read-next-token)))))
        (t (purescript-indentation-type))))

(defun purescript-indentation-list (parser end sep stmt-sep)
  (purescript-indentation-with-starter
   `(lambda () (purescript-indentation-separated #',parser
                                              ,sep
                                              ,stmt-sep))
   end))

(defun purescript-indentation-with-starter (parser end &optional where-expr?)
  (let ((starter-column (purescript-current-column))
        (current-indent current-indent)
        (left-indent (if (= (purescript-current-column) (purescript-indentation-current-indentation))
                         (purescript-current-column) left-indent)))
    (purescript-indentation-read-next-token)
    (when (eq current-token 'end-tokens)
      (if (equal following-token end)
          (purescript-indentation-add-indentation starter-column)
        (if where-expr?
            (purescript-indentation-add-where-post-indent left-indent)
          (purescript-indentation-add-indentation
           (+ left-indent purescript-indentation-left-offset))))
      (throw 'parse-end nil))
    (let* ((current-indent (purescript-current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent (if end (+ current-indent purescript-indentation-starter-offset)
                          left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               (purescript-indentation-add-indentation starter-indent))
             (when end (throw 'parse-end nil))) ;; add no indentations
            ((equal current-token end)
             (purescript-indentation-read-next-token)) ;; continue
            (end (parse-error "Illegal token: %s" current-token))))))

(defun purescript-indentation-case ()
  (purescript-indentation-expression)
  (cond ((eq current-token 'end-tokens)
         (purescript-indentation-add-indentation current-indent))
        ((string= current-token "|")
         (purescript-indentation-with-starter
          (lambda () (purescript-indentation-separated #'purescript-indentation-case "|" nil))
          nil))
        ((string= current-token "->")
         (purescript-indentation-statement-right #'purescript-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun purescript-indentation-statement-right (parser)
  (purescript-indentation-read-next-token)
  (when (eq current-token 'end-tokens)
    (purescript-indentation-add-indentation
     (+ left-indent purescript-indentation-left-offset))
    (throw 'parse-end nil))
  (let ((current-indent (purescript-current-column)))
    (funcall parser)))

(defun purescript-indentation-simple-declaration ()
  (purescript-indentation-expression)
  (cond ((string= current-token "=")
         (purescript-indentation-statement-right #'purescript-indentation-expression))
        ((string= current-token "::")
         (purescript-indentation-statement-right #'purescript-indentation-type))
        ((and (eq current-token 'end-tokens)
              (string= following-token "="))
         (purescript-indentation-add-indentation current-indent)
         (throw 'parse-end nil))))

(defun purescript-indentation-declaration ()
  (purescript-indentation-expression)
  (cond ((string= current-token "|")
         (purescript-indentation-with-starter
          (lambda () (purescript-indentation-separated #'purescript-indentation-expression "," "|"))
          nil))
        ((eq current-token 'end-tokens)
         (when (member following-token '("|" "=" "::" ","))
           (purescript-indentation-add-indentation current-indent)
           (throw 'parse-end nil)))))

(defun purescript-indentation-layout (parser)
  (if (string= current-token "{")
      (purescript-indentation-list parser "}" ";" nil)
    (purescript-indentation-implicit-layout-list parser)))

(defun purescript-indentation-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" "::"
                  value operator no-following-token)))

(defun purescript-indentation-expression ()
  (let ((current-indent (purescript-current-column)))
    (catch 'return
      (while t
        (cond
         ((memq current-token '(value operator))
          (purescript-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (purescript-indentation-add-where-pre-indent))
                ((purescript-indentation-expression-token following-token)
                 (purescript-indentation-add-indentation
                  current-indent)))
          (throw 'return nil))

         (t (let ((parser (assoc current-token purescript-indentation-expression-list)))
              (when (null parser)
                (throw 'return nil))
              (funcall (cdr parser))
              (when (and (eq current-token 'end-tokens)
                         (string= (car parser) "let")
                         (= purescript-indentation-layout-offset current-indent)
                         (purescript-indentation-expression-token following-token))
                ;; inside a layout, after a let construct
                (purescript-indentation-add-layout-indent)
                (throw 'parse-end nil))
              (unless (member (car parser) '("(" "[" "{" "do" "case"))
                (throw 'return nil)))))))))

(defun purescript-indentation-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (purescript-indentation-find-indentations)))
        (str "")
        (pos 0))
    (while indentations
      (when (>= (car indentations) pos)
        (setq str (concat str (make-string (- (car indentations) pos) ?\ )
                          "|"))
        (setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun purescript-indentation-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
             (purescript-indentation-at-separator))

            ((equal current-token stmt-separator)
             (setq starter-indent (purescript-current-column))
             (purescript-indentation-at-separator))

            ((eq current-token 'end-tokens)
             (cond ((or (equal following-token separator)
                        (equal following-token stmt-separator))
                    (purescript-indentation-add-indentation starter-indent)
                    (throw 'parse-end nil)))
             (throw 'return nil))

            (t (throw 'return nil))))))

(defun purescript-indentation-at-separator ()
  (let ((separator-column
         (and (= (purescript-current-column) (purescript-indentation-current-indentation))
              (purescript-current-column))))
    (purescript-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (purescript-indentation-add-indentation current-indent)
           (throw 'return nil))
          (separator-column ;; on the beginning of the line
           (setq current-indent (purescript-current-column))
           (setq starter-indent separator-column)))))

(defun purescript-indentation-implicit-layout-list (parser)
  (let* ((layout-indent (purescript-current-column))
         (current-indent (purescript-current-column))
         (left-indent (purescript-current-column)))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-next ";"))
               (purescript-indentation-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (purescript-indentation-expression-token following-token)
                         (string= following-token ";"))
                 (purescript-indentation-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put purescript-indentation-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (purescript-indentation-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun purescript-indentation-phrase (phrase)
  (purescript-indentation-with-starter
   `(lambda () (purescript-indentation-phrase-rest ',phrase))
   nil))

(defun purescript-indentation-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (purescript-current-column)))
      (funcall (car phrase)))
    (cond
     ((eq current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
            ((equal following-token (cadr phrase))
             (purescript-indentation-add-indentation starter-indent)
             (throw 'parse-end nil))
            ((string= (cadr phrase) "in")
             (when (= left-indent layout-indent)
               (purescript-indentation-add-layout-indent)
               (throw 'parse-end nil)))
            (t (throw 'parse-end nil))))

     ((null (cdr phrase)))

     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (purescript-current-column) (purescript-indentation-current-indentation)))
             (lines-between (- parse-line-number starter-line))
             (left-indent (if (<= lines-between 0)
                              left-indent
                            starter-indent)))
        (purescript-indentation-read-next-token)
        (when (eq current-token 'end-tokens)
          (purescript-indentation-add-indentation
           (cond ((member (cadr phrase) '("then" "else"))
                  (+ starter-indent purescript-indentation-ifte-offset))
                 ((member (cadr phrase) '("in" "->"))
                  ;; expression ending in another expression
                  (if on-new-line
                      (+ left-indent purescript-indentation-starter-offset)
                    left-indent))
                 (t (+ left-indent purescript-indentation-left-offset))))
          (throw 'parse-end nil))
        (purescript-indentation-phrase-rest (cddr phrase))))

     ((string= (cadr phrase) "in")) ;; fallthrough
     (t (parse-error "Expecting %s" (cadr phrase))))))

(defun purescript-indentation-add-indentation (indent)
  (purescript-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent purescript-indentation-layout-offset)
     indent)))

(defun purescript-indentation-add-layout-indent ()
  (purescript-indentation-push-indentation layout-indent))

(defun purescript-indentation-add-where-pre-indent ()
  (purescript-indentation-push-indentation
   (+ layout-indent purescript-indentation-where-pre-offset))
  (if (= layout-indent purescript-indentation-layout-offset)
      (purescript-indentation-push-indentation
       purescript-indentation-where-pre-offset)))

(defun purescript-indentation-add-where-post-indent (indent)
  (purescript-indentation-push-indentation
   (+ indent purescript-indentation-where-post-offset)))

(defun purescript-indentation-push-indentation (indent)
  (when (or (null possible-indentations)
            (< indent (car possible-indentations)))
    (setq possible-indentations
          (cons indent possible-indentations))))

(defun purescript-indentation-token-test ()
  (let ((current-token nil)
        (following-token nil)
        (layout-indent 0)
        (parse-line-number 0)
        (indentation-point (mark)))
    (purescript-indentation-read-next-token)))

(defun purescript-indentation-read-next-token ()
  (cond ((eq current-token 'end-tokens)
         'end-tokens)
        ((eq current-token 'layout-end)
         (cond ((> layout-indent (purescript-current-column))
                'layout-end)
               ((= layout-indent (purescript-current-column))
                (setq current-token 'layout-next))
               ((< layout-indent (purescript-current-column))
                (setq current-token (purescript-indentation-peek-token)))))
        ((eq current-token 'layout-next)
         (setq current-token (purescript-indentation-peek-token)))
        ((> layout-indent (purescript-current-column))
         (setq current-token 'layout-end))
        (t
         (purescript-indentation-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (= (point) indentation-point)
                         (purescript-indentation-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (purescript-current-column) (purescript-indentation-current-indentation))
             ;; on a new line
             (setq current-indent (purescript-current-column))
             (setq left-indent (purescript-current-column))
             (setq parse-line-number (+ parse-line-number 1)))
           (cond ((> layout-indent (purescript-current-column))
                  (setq current-token 'layout-end))
                 ((= layout-indent (purescript-current-column))
                  (setq current-token 'layout-next))
                 (t (setq current-token (purescript-indentation-peek-token))))))))

(defun purescript-indentation-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok purescript-indentation-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun purescript-indentation-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))

    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at         ; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
      ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))
    (while (and (eq purescript-literate 'bird)
                (bolp)
                (eq (char-after) ?>))
      (forward-char)
      (forward-comment (buffer-size)))))

(provide 'purescript-indentation)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 8
;; End:

;;; purescript-indentation.el ends here
