;;; tlc --- Major mode for editing tlc files
;;
;; Author: Eric M. Ludlam <eludlam@mathworks.com>
;; Keywords: tlc
;; X-Abstract: Major mode for editing tlc files

(defvar tlc-version "1.3"
  "The current version of TLC mode.")

;;
;; Copyright 1997-2021 Eric Ludlam
;;
;; This program is derived from free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;;  This is a major mode for editing Target Language Compiler, TLC, programs.
;;  TLC programs are an advanced template system which are used to create
;;  source code for compilers, e.g. to produce C code or C++ code.
;;
;;  TLC programs consist of :
;;     - Keywords of the form %function, %if, etc.
;;     - Built-in functions of form EXISTS().
;;     - Statement continuation using "...", e.g.
;;          %assign a = b + ...
;;              c
;;     - Single line comments are of the from: %% line
;;     - "%%{", "%%{{", "%%}", "%%}}", etc. 'indent comment shift operators' adjust indentation
;;       by 4, 8, -4, -8, etc.
;;     - "%%{N}" 'indent comment shift operators' adjust what follows to indent to column N
;;     - Block comments are of form: /% ... %/
;;     - All other content is sent to the output to create target language source code, e.g. C.
;;
;;  TLC programs therefore mix TLC semantics with target language semantics, e.g. C code:
;;
;;     %function Foo(mode) output
;;         %if mode.loop
;;             int i;
;;             for (i = 0; i < %<mode.loopLimit>; i++) {
;;                 %<Out(mode)>[i] = %<In(mode)>[i];
;;             }
;;         %else
;;             %<Out(mode)> = %<In(mode)>;
;;         %endif
;;     %endfunction
;;
;;  The mix of TLC code and C code can result in challenges for semantic indentation. In this case
;;  you can use the special "%%{", "%%}", etc. comments (called indent comment shift operators) to
;;  adjust indentation as in:
;;
;;      %if mode == "standard"
;;          if (a > 1) {
;;              if (b > 0) {
;;                  %<v> = %<v> + b;
;;      %else
;;          if (x > 0) {
;;              if (y > 0) {
;;                  %<v> = %<v> + y;
;;      %endif
;;      %%{{{                                      ;; this adjusts indentation by 12
;;                  %<w> = %<v>
;;              }
;;          }
;;      %%}                                        ;; this adjusts indentation by -4
;;
;;  Another use of the indent comment shift operators is with %openfile to write
;;  target language output comments, e.g.
;;
;;      %function GetHeader() void
;;          %openfile header
;;      %%}
;;      Unindented C
;;      comment text
;;      %%{
;;          %closefile header
;;          %return header
;;      %endfunction
;;
;;  Using %openfile/%closefile is helpful when creating messages and this often requires that the
;;  content starts at a specific column, e.g. column 1 (zero based), such that when the lines of the
;;  message are combined into a single line the words at the line boundaries are joined by spaces,
;;  e.g.
;;
;;   %function GetMessage() void
;;       %openfile message
;;   %%{1}
;;    This is a message
;;    that spans multiple lines
;;   %%{4}
;;       %closefile
;;   %endfunction
;;
;; Issues:
;;   1. Font's for continuations multiline statements are not correct, i.e.
;;        %assign a = 1 + ...
;;            2 + ...
;;            3
;;      will have the "2 + ..." and "3" line shown using tlc-font-lock-output-code.
;;      To fix, we probably need to use Multiline Font Lock constructs.

;;; History:
;;
;;  10Sep1998 by Eric M.  Ludlam <eludlam@mathworks.com>
;;    Posted First revision onto the FTP site.
;;
;;  06Oct2005 Peter S galbraith <psg@debian.org>
;;    Minor changes for:
;;    - support customization.
;;    - added autoload cookies.
;;    - CVS storage elsewhere without changing the version number.
;;
;;  Recent history is in the ChangeLog and the matlab-emacs repository.

;;; Code:
(defun tlc-version ()
  "Display the current version of TLC mode."
  (interactive)
  (message tlc-version))

(defgroup tlc nil
  "Major mode for editing tlc files."
  :group 'languages)

(defcustom tlc-mode-hook nil
  "*List of functions to call on entry to TLC mode."
  :group 'tlc
  :type 'hook)

(defvar tlc-syntax-table nil
  "Syntax table used in an TLC file.")

(unless tlc-syntax-table
  (setq tlc-syntax-table (make-syntax-table (standard-syntax-table)))
  ;; Multiline comments:   /% ... %/
  ;; Single line comments: %% ...
  (modify-syntax-entry ?/  ". 14c" tlc-syntax-table)
  (modify-syntax-entry ?%  ". 123" tlc-syntax-table)
  (modify-syntax-entry ?\n ">" tlc-syntax-table)
  ;; Strings
  (modify-syntax-entry ?\" "\"" tlc-syntax-table)
  ;; %<variable> support:
  (modify-syntax-entry ?< "(>" tlc-syntax-table)
  (modify-syntax-entry ?> ")>" tlc-syntax-table))

(defvar tlc-mode-map
  (let ((km  (make-sparse-keymap)))
    (define-key km "\C-m" 'tlc-return)
    (define-key km [return] 'tlc-return)
    (define-key km "\C-i" 'tlc-indent)
    km)
  "Keymap for `tlc-mode'.")

(defvar tlc-font-lock-output-code 'tlc-font-lock-output-code
  "Face for output code.")

(defface tlc-font-lock-output-code
  '((((class grayscale) (background light))
     (:foreground "DimGray" :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :underline t))
    (((class color) (background light)) (:foreground "DarkGreen"))
    (((class color) (background dark))  (:foreground "chartreuse"))
    (t (:underline t)))
  "Font Lock mode face used to highlight tlc keywords."
  :group 'tlc)

(defcustom tlc-keywords
  '("CAST" "EXISTS" "FEVAL" "FILE_EXISTS" "FORMAT"
    "FIELDNAMES" "GETFIELD" "GENERATE"
    "GENERATE_FILENAME" "GENERATE_FORMATTED_VALUE"
    "GENERATE_FUNCTION_EXISTS" "GENERATE_TYPE"
    "GENERATE_TYPE_FUNCTION_EXISTS" "GET_COMMAND_SWITCH"
    "IDNUM" "IMAG"
    "INT8MAX" "INT8MIN"
    "INT16MAX" "INT16MIN"
    "INT32MAX" "INT32MIN"
    "ISEQUAL" "ISFIELD" "ISINF" "ISNAN" "ISFINITE"
    "NULL_FILE" "NUMTLCFILES"
    "OUTPUT_LINES" "SIZE" "STDOUT" "STRING" "STRINGOF"
    "SYSNAME" "TLCFILES" "TLC_TIME"
    "TLC_FALSE" "TLC_TRUE"
    "TLC_VERSION" "TYPE"
    "UINT8MAX" "UINT16MAX" "UINT32MAX"
    "UINTWHITE_SPACE" "WILL_ROLL")
  "Built-in function keywords to highlight in TLC."
  :type '(repeat (string :tag "keyword"))
  :group 'tlc)

(defvar tlc-font-lock-keywords
  (list
   ;; %function keyword
   '("^%function\\s-+\\(\\sw+\\)\\s-*(" 1 font-lock-function-name-face)
   '("^%function\\s-+\\(\\sw+\\)\\s-*("
     ("\\s-*\\(\\sw+\\)\\s-*[,)]" nil nil
      (1 font-lock-variable-name-face)))
   ;; Single line comments: %% text
   ;; Special "%%{", "%%}", etc. comments
   '("\\(?:%%\\)\\({+\\|{+[0-9]+}\\)\\s-*$" 1 'bold prepend)
   '("\\(?:%%\\)\\(}+\\)\\s-*$" 1 'bold prepend)
   ;; Target language output code
   '("\\(^[ \t]*\\([^ \n\t%]\\|%<\\)[^\n]*\\)$" 1 tlc-font-lock-output-code append)
   ;; Keywords, e.g., %if
   '("\\(^\\|\\s-\\)\\(%[^% \t(\n>]+\\)\\>" 2 font-lock-keyword-face)
   ;; %assign keyword
   '("%assign\\s-+:*\\([_a-zA-Z0-9.]+\\)\\s-*\\($\\|=\\)" 1 font-lock-variable-name-face)
   ;; %exit, %warning, %error, %trace keywords
   '("%\\(exit\\|warning\\|error\\|trace\\) \\([^\n]+\\)$" 2 font-lock-string-face prepend)
   ;; %<var> expansions
   '("\\(%<[^%\n>]+>\\)" 1 font-lock-reference-face prepend)
   ;; Built-in functions, e.g. EXISTS
   (list (concat "\\<\\(" (regexp-opt tlc-keywords) "\\)\\>")
	 1 'font-lock-type-face)
   '("[^.]\\(\\.\\.\\.\\)$" 1 'underline prepend)
   )
  "List of keywords for nicely coloring X defaults.")

;;;###autoload
(define-derived-mode tlc-mode prog-mode "TLC" ()
  "Major mode for editing Tlc files, or files found in tlc directories."
  (kill-all-local-variables)
  (setq major-mode 'tlc-mode)
  (setq mode-name "TLC")
  (use-local-map tlc-mode-map)
  (set-syntax-table tlc-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "%% "
	comment-end   "")
  (setq comment-start-skip "%%\\|/%")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tlc-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((tlc-font-lock-keywords)
			     nil ; do not do string/comment highlighting
			     nil ; keywords are case sensitive.
			     ;; This puts _ as a word constituent,
			     ;; simplifying our keywords significantly
			     ((?_ . "w"))))
  (tlc-version)
  (save-excursion
    (goto-char (point-min))
    (run-hooks 'tlc-mode-hook)))

(defun tlc-return ()
  "Handle carriage return in `tlc-mode'."
  (interactive)
  (delete-horizontal-space)
  (newline)
  (tlc-indent))

(defun tlc-indent ()
  "Indent the current line to the indentation of the previous line."
  (interactive)
  (let (curr-indent
        new-indent)
    (save-excursion
      (save-restriction
        (setq curr-indent (current-indentation))
        (setq new-indent (tlc--calc-indentation))))
    (if (= curr-indent new-indent)
        (when (< (current-column) curr-indent)
          (goto-char (+ (line-beginning-position) curr-indent)))
      ;; else indent
      (save-restriction
        (let ((curr-column (current-column)))
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to new-indent) ;; now current-column is new-indent
          (when (> curr-column curr-indent)
            ;; Suppose prior to tlc-indent, we had the following with cursor at column 17:
            ;;     0123456789012345678901234567890
            ;;        %assign var = MyFcn(p1)
            ;;                      ^
            ;; after indentation, we move the %assign to column 8 (because it is within another
            ;; context which requires we have indentation of 8):
            ;;     0123456789012345678901234567890
            ;;             %assign var = MyFcn(p1)
            ;;                           ^
            ;; we want the cursor to remain at 'M' and that's what the goto-char does.
            ;;
            ;; If the cursor was anywhere before the %assign, we want the cursor to be at the
            ;; '%' and that's what the indent-to function does, which is why we guard the goto-char
            ;; with (> curr-column curr-indent).
            (goto-char (+ (line-beginning-position) new-indent (- curr-column curr-indent)))))))))

(defvar tlc--indent-because-of-continuation nil)

(defun tlc--calc-indentation ()
  "Calculate the indentation of this line."
  (beginning-of-line)

  (if (and (looking-at "\\s-*%%{[0-9]+}\\s-*$")
           (not (tlc--in-multiline-comment)))
      ;; %%{N} means the place %%{N} at column 0 and the following line goes to column N
      0
    ;; Else calculate indentation based on current line PLUS the context of the prior line
    (let ((i-col (cond
	          ((and (looking-at
	                 "\\s-*\\(?:\
\\(?:\\(?:%end\\(switch\\|roll\\|with\\|for\\|foreach\\|while\\|function\\)\\)\\>\\)\
\\|}\\)")
                        (not (tlc--in-multiline-comment)))
	           -4)
                  ((and (looking-at "\\s-*\\(%case\\|%default\\)\\>")
                        (not (tlc--in-multiline-comment)))
                   -2)
                  ;; %%} means shift by -4, %%}} means shift by -8, etc.
                  ((and (looking-at "\\s-*%%\\(}+\\)\\s-*$")
                        (not (tlc--in-multiline-comment)))
                   (* -4 (- (match-end 1) (match-beginning 1))))
                  ;;
	          (t 0)))
          (is-tlc-if-part (and (looking-at "\\s-*%\\(?:else\\|elseif\\|endif\\)") ;; part of a %if?
                               (not (tlc--in-multiline-comment))))
          (percent-in-multiline-comment (and (looking-at "\\s-*%") (tlc--in-multiline-comment))))

      ;; Walk up to calculate the indent based on the construct we are within and then add it to
      ;; i-col for the current construct.
      (setq tlc--indent-because-of-continuation nil)
      (if (bobp)
          (current-indentation)
        (save-excursion
          (tlc--indent-move-up is-tlc-if-part)
	  (cond ((bobp)
                 (setq i-col (+ i-col (tlc--calc-next-indentation))))
                ;; '%' line following a "/%" line, if so add 1
	        ((and percent-in-multiline-comment (looking-at "\\s-*/%"))
                 (setq i-col (1+ (current-indentation))))
                ;; Align %elsif, %else, %endif with corresponding %if?
                (is-tlc-if-part
                 (setq i-col (current-indentation)))
	        (t
	         (setq i-col (+ (current-indentation)
			        (if (and tlc--indent-because-of-continuation
				         (or (> 0 i-col)
                                             is-tlc-if-part))
				    i-col
			          (+ i-col (tlc--calc-next-indentation)))))
	         (if (< i-col 0) (setq i-col 0))))
	  i-col)))))

(defun tlc--indent-move-up (is-tlc-if-part)
  "Move to first prior non-blank line or matching %if,  %else, %endif when IS-TLC-IF-PART is t.
Specify IS-TLC-IF-PART as t, if current line is %else, %elsif, %endif to align the %if statements."
  (let ((n-if-statements-to-skip 0)  ;; num %if statements to skip over when is-tlc-if-part is t
        done)
    (while (not done)
      ;;
      ;; Move up to first non-blank line
      ;;
      (forward-line -1)
      (beginning-of-line)
      (while (and (not (bobp))
                  (looking-at "^\\s-*$")) ;; skip blank lines
        (forward-line -1))
      ;;
      ;; Align %elseif, %else, %endif with the prior TLC statement
      ;;
      (if (and (not (bobp)) is-tlc-if-part)
          ;; If within a non-TLC statement, look up until we find the matching if "part"
          (if (looking-at "\\s-*%endif\\>")
              (setq n-if-statements-to-skip (1+ n-if-statements-to-skip))
            (if (> n-if-statements-to-skip 0)
                (if (looking-at "\\s-*%if\\>")
                    (setq n-if-statements-to-skip (1- n-if-statements-to-skip)))
              (setq  done (looking-at "\\s-*%\\(?:if\\|elseif\\|else\\)\\>"))))
        (setq done t)))))

(defun tlc--calc-next-indentation ()
  "Calculate indentation for the next line based on the current line."
  (if (and (looking-at "\\s-*%%")
           (not (tlc--in-multiline-comment)))
      (cond ((looking-at "\\s-*%%\\({+\\)\\s-*$")
             ;; %%{ means shift by 4, %%{{ means shift by 8, etc.
             (* 4 (- (match-end 1) (match-beginning 1))))
            ((looking-at "\\s-*%%{\\([0-9]+\\)}\\s-*$")
             ;; %%{N} means the place %%{N} at column 0 and the following line goes to column N
             (string-to-number (buffer-substring (match-beginning 1) (match-end 1))))
            (t 0))
    ;; Else compute indent based on language element
    (+
     ;; Include offset based on language element, e.g. %function means to indent by 4
     (cond ((save-excursion
              (and (not (tlc--assignment-continuation-p))
                   (tlc--beginning-of-statement))
              (or
               (and (looking-at "\\s-*%\\(?:switch\\|roll\\|with\\|if\\|for\\|\
foreach\\|while\\|else\\|elseif\\|function\\)\\>")
                    ;; Do not indent "tlc guards", i.e.
                    ;;    %if EXISTS(::_FILE_NAME_) == 0
                    ;;    %assign _FILE_NAME_ = 1
                    ;;    <tlc file body starts at first column, 0>
                    ;;    %endif
                    (not (looking-at "\\s-*%if\\s-+\\(?:\
EXISTS(\"?\\(?:::\\)?_[A-Z_0-9]+_\"?)\\s-*==\\s-*\\(?:0\\|TLC_FALSE\\)\\|\
!EXISTS(\"?\\(?:::\\)?_[A-Z_0-9]+_\"?)\\)"))
                    (not (tlc--in-multiline-comment)))
               (and (looking-at "\\s-*/%")
                    (tlc--in-multiline-comment))))
            4)
           ((and (save-excursion
                   (and (not (tlc--assignment-continuation-p))
                        (tlc--beginning-of-statement))
                   (looking-at "\\s-*%\\(?:case\\|default\\)\\>"))
                 (not (tlc--in-multiline-comment)))
            2)
           ;; End of multiline comment?
           ((and (looking-at "\\s-*%/")
                 (tlc--in-multiline-comment))
            (if (= (% (current-indentation) 2) 1)
                ;; When current-indentation is odd, we've shifted the "%/" by 1, so need to shift it
                ;; back
                -1
              0))
           ;; Continuation?
           ((and (tlc--assignment-continuation-p)
                 (save-excursion (forward-line -1)
                                 (not (tlc--assignment-continuation-p)))
                 (not (tlc--in-multiline-comment)))
            (setq tlc--indent-because-of-continuation t)
            4)
           ;; Open bracket target language line, e.g.  "void foo() {", "struct {\", etc.
           ((and (save-excursion
                   (end-of-line)
                   (re-search-backward "[^[:space:]]" (line-beginning-position) t)
                   (when (and (> (current-column) 0) ;; Do we have line ending in \, if so remove it
                              (looking-at "\\\\"))
                     (backward-char)
                     (when (looking-at "\\s-")
                       (re-search-backward "[^[:space:]]" (line-beginning-position) t)))
                   (looking-at "{"))
                 (not (tlc--in-multiline-comment)))
            4)
           (t 0))
     ;; Don't include continuation if not in a continuation block
     (if (and (not (tlc--line-special))
              (not (tlc--assignment-continuation-p))
              (save-excursion (forward-line -1)
                              (tlc--assignment-continuation-p))
              (not (tlc--in-multiline-comment)))
         -4
       0))))

(defun tlc--beginning-of-statement ()
  "Goto the beginning of a statement, skipping over continuation lines."
  (beginning-of-line)
  (if (not (save-excursion (forward-line -1) (tlc--assignment-continuation-p)))
      nil
    (forward-line -1)
    (while (tlc--assignment-continuation-p)
      (forward-line -1))
    (forward-line 1)
    (beginning-of-line)))

(defun tlc--line-special ()
  "Return t if the current line is a special language line."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at "\\s-*\\(?:%[^<]\\|}\\)"))))

(defun tlc--assignment-continuation-p ()
  "See if continuation lines should be indented."
  (save-excursion
    (end-of-line)
    (when (> (current-column) 2)
        (forward-char -3)
        (looking-at "\\.\\.\\."))))

(defun tlc--in-multiline-comment ()
  "Return t we are in a multiline comment."
  ;; One way to do this is to use
  ;;   (save-excursion (and (re-search-backward "/%\\|%/" nil t) (looking-at "/%")))
  ;; however this fails on something like the following where /% is in a string
  ;;    %assign foo = "dir/%<file>.h"
  ;;    %if a
  ;;    ^------- point
  ;; From
  ;; https://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
  ;; we can use syntax-ppss
  (nth 4 (syntax-ppss)))

;;; Add to mode list
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tlc\\'" . tlc-mode))

(provide 'tlc)

;;; tlc.el ends here

;; LocalWords:  Ludlam eludlam galbraith psg debian defun defcustom setq keymap defface grayscale
;; LocalWords:  IDNUM NUMTLCFILES STRINGOF SYSNAME TLCFILES UINTWHITE repeat:nil prog calc endswitch
;; LocalWords:  bobp nexti progn alist el stackexchange ppss openfile closefile
