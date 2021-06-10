;;; matlab-cgen.el --- In buffer code generation features (templates, etc)
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; This library supports tempo templates, and other misc functions designed
;; to create code in an Emacs buffer.

(require 'matlab)
(require 'tempo)

;;; Code:

(defvar matlab-tempo-tags nil
  "List of templates used in MATLAB mode.")

;; This trick allows this file to be autoloaded ONLY when the user uses the insert prefix.
;; 
;;;###autoload (autoload 'matlab-insert-map-fcn "matlab-cgen" "Keymap for C-c C-c in matlab-mode" t 'keymap)

(defvar matlab-insert-map
  (let ((km (make-sparse-keymap)))
    (define-key km "c" 'matlab-insert-next-case)
    (define-key km "e" 'matlab-insert-end-block)
    (define-key km "i" 'tempo-template-matlab-if)
    (define-key km "I" 'tempo-template-matlab-if-else)
    (define-key km "f" 'tempo-template-matlab-for)
    (define-key km "s" 'tempo-template-matlab-switch)
    (define-key km "t" 'tempo-template-matlab-try)
    (define-key km "w" 'tempo-template-matlab-while)
    (define-key km "F" 'tempo-template-matlab-function)
    (define-key km "'" 'matlab-stringify-region)
    ;; Not really inserts, but auto coding stuff
    (define-key km "\C-s" 'matlab-ispell-strings-and-comments)
    km)
  "Keymap used for inserting simple texts based on context.")

(defvar matlab-insert-map-fcn nil
  "Keymap for C-c C-c in matlab-mode.")

(fset 'matlab-insert-map-fcn (setq matlab-insert-map-fcn matlab-insert-map))

(add-hook 'matlab-mode-hook #'matlab-cgen-hook-fcn t)

(defun matlab-cgen-hook-fcn ()
  "Hook run in `matlab-mode-hook' needed for cgen support."
  ;; Tempo tags
  (make-local-variable 'tempo-local-tags)
  (setq tempo-local-tags (append matlab-tempo-tags tempo-local-tags))
  )

;; We might load late, so loop over all matlab buffers, and run the hook.
(dolist (B (buffer-list))
  (with-current-buffer B
    (when (eq major-mode 'matlab-mode)
      (matlab-cgen-hook-fcn))))

;;; Templates and smart code gen features
;;
(defun matlab-insert-end-block (&optional reindent)
  "Insert and END block based on the current syntax.
Optional argument REINDENT indicates if the specified block should be re-indented."
  (interactive "P")
  (if (not (matlab-ltype-empty)) (progn (end-of-line) (insert "\n")))
  (let ((valid t) (begin nil))
    (save-excursion
      (condition-case nil
	  (progn
	    (matlab-backward-sexp t)
	    (setq begin (point)
		  valid (buffer-substring-no-properties
			 (point) (save-excursion
				   (re-search-forward "[\n,;.]" nil t)
				   (point)))))
	(error (setq valid nil))))
    (if (not valid)
	(error "No block to end")
      (insert "end")
      (if (stringp valid) (insert " % " valid))
      (matlab-indent-line)
      (if reindent (indent-region begin (point) nil)))))

(tempo-define-template
 "matlab-for"
 '("for " p "=" p "," > n>
     r> &
     "end" > %)
 "for"
 "Insert a MATLAB for statement"
 'matlab-tempo-tags
 )

(tempo-define-template
 "matlab-while"
 '("while (" p ")," > n>
     r> &
     "end" > %)
 "while"
 "Insert a MATLAB while statement"
 'matlab-tempo-tags
 )

(tempo-define-template
 "matlab-if"
 '("if " p > n
     r>
     "end" > n)
 "if"
 "Insert a MATLAB if statement"
 'matlab-tempo-tags
 )

(tempo-define-template
 "matlab-if-else"
 '("if " p > n
     r>
     "else" > n
     "end" > n)
 "if"
 "Insert a MATLAB if statement"
 'matlab-tempo-tags
 )

(tempo-define-template
 "matlab-try"
 '("try " > n
     r>
     "catch" > n
     p > n
     "end" > n)
 "try"
 "Insert a MATLAB try catch statement"
 'matlab-tempo-tags
 )

(tempo-define-template
 "matlab-switch"
 '("switch " p > n
     "otherwise" > n
     r>
     "end" > n)
 "switch"
 "Insert a MATLAB switch statement with region in the otherwise clause."
 'matlab-tempo-tags)

(defun matlab-insert-next-case ()
  "Insert a case statement inside this switch statement."
  (interactive)
  ;; First, make sure we are where we think we are.
  (let ((valid t))
    (save-excursion
      (condition-case nil
	  (progn
	   (matlab-backward-sexp t)
	   (setq valid (looking-at "switch")))
	(error (setq valid nil))))
    (if (not valid)
	(error "Not in a switch statement")))
  (if (not (matlab-ltype-empty)) (progn (end-of-line) (insert "\n")))
  (indent-to 0)
  (insert "case ")
  (matlab-indent-line))

(tempo-define-template
 "matlab-function"
 '("function "
     (P "output argument(s): " output t)
     ;; Insert brackets only if there is more than one output argument
     (if (string-match "," (tempo-lookup-named 'output))
	 '(l "[" (s output) "]")
       '(l (s output)))
     ;; Insert equal sign only if there is output argument(s)
     (if (= 0 (length (tempo-lookup-named 'output))) nil
       " = ")
     ;; The name of a function, as defined in the first line, should
     ;; be the same as the name of the file without .m extension
     (if (= 1 (count-lines 1 (point)))
	 (tempo-save-named
	  'fname
	  (file-name-nondirectory (file-name-sans-extension
				   (buffer-file-name))))
       '(l (P "function name: " fname t)))
     (tempo-lookup-named 'fname)
     "("  (P "input argument(s): ") ")" n
     "% " (upcase (tempo-lookup-named 'fname)) " - " (P "H1 line: ") n
     "%   " p n
     (if matlab-functions-have-end
         '(l "end" n)))
 "function"
 "Insert a MATLAB function statement"
 'matlab-tempo-tags
 )

(defun matlab-stringify-region (begin end)
  "Put MATLAB 's around region, and quote all quotes in the string.
Stringification allows you to type in normal MATLAB code, mark it, and
then turn it into a MATLAB string that will output exactly what's in
the region.  BEGIN and END mark the region to be stringified."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (if (re-search-forward "\n" end t)
	(error
	 "You may only stringify regions that encompass less than one line"))
    (let ((m (make-marker)))
      (move-marker m end)
      (goto-char begin)
      (insert "'")
      (while (re-search-forward "'" m t)
	(insert "'"))
      (goto-char m)
      (insert "'"))))

;;; SPELLING
;;

(defun matlab-ispell-strings-and-comments-region (begin end)
  "Spell check valid strings in region with Ispell.
Argument BEGIN and END mark the region boundary."
  (interactive "r")
  (require 'ispell)
  (save-excursion
    (goto-char begin)
    ;; Here we use the font lock function for finding strings.
    ;; Its cheap, fast, and accurate.
    ;; NOTE: This now also does comments
    (while (and (matlab-font-lock-allstring-comment-match-normal end)
		(ispell-region (match-beginning 0) (match-end 0))))))

(defun matlab-ispell-strings-and-comments ()
  "Spell check valid strings in the current buffer with Ispell.
Calls `matlab-ispell-strings-region'"
  (interactive)
  (matlab-ispell-strings-and-comments-region (point-min) (point-max)))

;;; Printing
;;

;;;###autoload
(defun matlab-generate-latex ()
  "Convert a MATLAB M file into a Latex document for printing.
Author: Uwe Brauer oub@eucmos.sim.ucm.es
Created: 14 Feb 2002"
  (interactive "*")
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (insert "\\documentclass[12pt]{report}\n
\\usepackage{listings}
\\lstloadlanguages{Matlab}
\\lstset{language=Matlab,keywordstyle=\\bfseries,labelstep=1,escapechar=\\#}
\\begin{document}
\\begin{lstlisting}{}")
      (newline)
      (goto-char (point-max))
      (insert "\n\\end{lstlisting}\n\\end{document}")
      (widen)))
  (font-lock-mode nil)
  (LaTeX-mode)
  (font-lock-mode nil))


(provide 'matlab-cgen)

;;; matlab-cgen.el ends here
