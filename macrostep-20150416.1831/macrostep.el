;;; macrostep.el --- interactive macro stepper for Emacs Lisp

;; Copyright (C) 2012-2014 Jonathan Oddie <j.j.oddie@gmail.com>

;; Author:     joddie <j.j.oddie@gmail.com>
;; Maintainer: joddie <j.j.oddie@gmail.com>
;; Created:    16 January 2012
;; Updated:    11 May 2014
;; Version:    0.8
;; Package-Version: 20150416.1831
;; Keywords:   lisp, languages, macro, debugging
;; Url:        https://github.com/joddie/macrostep

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; 1 Overview
;; ==========

;;   This is a minor mode for interactively stepping through the expansion
;;   of macros in Emacs Lisp source code. It lets you see exactly what
;;   happens at each step of the expansion process by pretty-printing the
;;   expanded forms inline in the source buffer, which is read-only while
;;   macro expansions are visible. You can expand and collapse macro forms
;;   one step at a time, and evaluate or instrument them for debugging with
;;   Edebug as normal (but see "Bugs and known limitations",
;;   below). Single-stepping through the expansion is useful for debugging
;;   macros that expand into another macro form, especially one like
;;   `lexical-let' that does significant rewriting. These can be difficult
;;   to debug with Emacs' built-in `macroexpand', because `macroexpand'
;;   continues expansion until the top-level form is no longer a macro
;;   call.

;;   The mode also adds some simple additional fontification to
;;   macro-expanded code. The heads of macro sub-forms are fontified using
;;   `macrostep-macro-face'. Uninterned symbols (gensyms) are fontified
;;   based on which step in the expansion created them, to distinguish them
;;   from normal symbols and from other gensyms with the same print
;;   name. Use `customize-group' with the `macrostep' group to customize
;;   these faces.

;;   Both macros defined by `defmacro' and local macros created by
;;   `macrolet' and `cl-macrolet' can be expanded.


;; 2 Key-bindings and usage
;; ========================

;;   The standard macrostep-mode keybindings are the following:

;;   e, =, RET : expand the macro form following point one step
;;   c, u, DEL : collapse the form following point
;;   q, C-c C-c: collapse all expanded forms and exit macrostep-mode
;;   n, TAB    : jump to the next macro form in the expansion
;;   p, M-TAB  : jump to the previous macro form in the expansion

;;   It's not very useful to enable and disable macrostep-mode
;;   directly. Instead, bind `macrostep-expand' to a key in
;;   `emacs-lisp-mode-map', for example C-c e:

;;   ,----
;;   | (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
;;   `----

;;   You can then enter macrostep-mode and expand a macro form completely
;;   by typing `C-c e e e ...' as many times as necessary.

;;   Exit macrostep-mode either with `q', `C-c C-c', or by successively
;;   typing `c' to collapse all expanded forms back to their original text.


;; 3 Expanding sub-forms
;; =====================

;;   By moving point around in the macro expansion (perhaps using the `n'
;;   and `p' keys), you can macro-expand sub-forms before fully expanding
;;   the enclosing form. This can be useful in some cases, but you should
;;   keep in mind that it does not correspond to the way Emacs actually
;;   expands macro calls when evaluating or compiling your code.  Macro
;;   expansion in Emacs Lisp always proceeds by fully expanding the outer
;;   form to a non-macro form before doing anything with the sub-forms.

;;   For example, with `cl' loaded, try expanding the following form:

;;   ,----
;;   | (dolist (l list-of-lists)
;;   |  (incf (car l)))
;;   `----

;;   to produce the following:

;;   ,----
;;   | (block nil
;;   |   (let
;;   |       ((--cl-dolist-temp-- list-of-lists)
;;   |        l)
;;   |     (while --cl-dolist-temp--
;;   |       (setq l
;;   |             (car --cl-dolist-temp--))
;;   |       (incf
;;   |        (car l))
;;   |       (setq --cl-dolist-temp--
;;   |             (cdr --cl-dolist-temp--)))
;;   |     nil))
;;   `----

;;   where the forms beginning `block' and `incf' are both macro calls.

;;   At this point, you can either continue expanding the `block' form,
;;   which corresponds to the real order of macro expansion in evaluation,
;;   or type `n' to move point to the unexpanded `incf' and expand it to a
;;   `callf' form and finally to a `let*' form.  If you then move point
;;   back to the `block' and expand it, an unexpanded `incf' form appears
;;   again in the result.  This might look visually confusing, but it does
;;   at least correspond to the way real macro expansion works.

;;   Why allow expanding sub-forms out of order like this at all? The main
;;   reason is for debugging macros which expand into another macro, like
;;   `lexical-let', that programmatically expands its contents in order to
;;   rewrite them.  In this case, expanding the sub-forms first allows you
;;   to see what `lexical-let' would compute via `cl-macroexpand-all'.


;; 4 Bugs and known limitations
;; ============================

;;   You can evaluate and edebug macro-expanded forms and step through the
;;   macro-expanded version, but the form that `eval-defun' and friends
;;   read from the buffer won't have the uninterned symbols of the real
;;   macro expansion.  This will probably work OK with CL-style gensyms,
;;   but may cause problems with `make-symbol' symbols if they have the
;;   same print name as another symbol in the expansion. It's possible that
;;   using `print-circle' and `print-gensym' could get around this.

;;   The macro stepper doesn't bother trying to determine whether or not a
;;   sub-form is in an evaluated position before highlighting it as a
;;   macro. It does exclude `lambda' from treatment as a macro, since that
;;   leads to an endless series of expansions: `(function (function
;;   ... ))'. It would be better to recognise `function', `quote' and other
;;   special forms using their `edebug-form-spec' property.

;;   Please send other bug reports and feature requests to the author.


;; 5 Acknowledgements
;; ==================

;;   Thanks to:
;;   - John Wiegley for fixing a bug with the face definitions under Emacs
;;     24 & for plugging macrostep in his [EmacsConf presentation]!
;;   - George Kettleborough for bug reports, and patches to highlight the
;;     expanded region and properly handle backquotes.
;;   - Nic Ferrier for suggesting support for local definitions within
;;     macrolet forms


;;   [EmacsConf presentation] http://youtu.be/RvPFZL6NJNQ


;; 6 Changelog
;; ===========

;;   - v0.8, 2014-05-29: fix a bug with printing the first element of lists
;;   - v0.7, 2014-05-11: expand locally-defined macros within (cl-)macrolet
;;     forms
;;   - v0.6, 2013-05-04: better handling of quote and backquote
;;   - v0.5, 2013-04-16: highlight region, maintain cleaner buffer state
;;   - v0.4, 2013-04-07: only enter macrostep-mode on successful
;;     macro-expansion
;;   - v0.3, 2012-10-30: print dotted lists correctly. autoload
;;     definitions.

;;; Code:

;; We use `pp-buffer' to pretty-print macro expansions
(require 'pp)
(require 'ring)
;; `cl-macs' is needed at run-time to support `cl-macrolet'
(require 'cl-macs)
(eval-when-compile
  (require 'cl)
  (require 'pcase))


;;; Constants and dynamically bound variables
(defvar macrostep-overlays nil
  "List of all macro stepper overlays in the current buffer.")
(make-variable-buffer-local 'macrostep-overlays)

(defvar macrostep-gensym-depth nil
  "Number of macro expansion levels that have introduced gensyms so far.")
(make-variable-buffer-local 'macrostep-gensym-depth)

(defvar macrostep-gensyms-this-level nil
  "t if gensyms have been encountered during current level of macro expansion.")
(make-variable-buffer-local 'macrostep-gensyms-this-level)

(defvar macrostep-saved-undo-list nil
  "Saved value of buffer-undo-list upon entering macrostep mode.")
(make-variable-buffer-local 'macrostep-saved-undo-list)

(defvar macrostep-saved-read-only nil
  "Saved value of buffer-read-only upon entering macrostep mode.")
(make-variable-buffer-local 'macrostep-saved-read-only)

(defvar macrostep-environment nil
  "Local macro-expansion environment, including macros declared by `cl-macrolet'.")

(defvar macrostep-expansion-buffer nil
  "Non-nil if the current buffer is a macro-expansion buffer.")
(make-variable-buffer-local 'macrostep-expansion-buffer)

(defvar macrostep-outer-environment nil
  "Outermost macro-expansion environment to use in a dedicated macro-expansion buffers.

This variable is used to save information about any enclosing
`cl-macrolet' context when a macro form is expanded in a separate
buffer.")
(make-variable-buffer-local 'macrostep-outer-environment)

;;; Customization options and faces
(defgroup macrostep nil
  "Interactive macro stepper for Emacs Lisp."
  :group 'lisp
  :link '(emacs-commentary-link :tag "commentary" "macrostep.el")
  :link '(emacs-library-link :tag "lisp file" "macrostep.el")
  :link '(url-link :tag "web page" "https://github.com/joddie/macrostep"))

(defface macrostep-gensym-1
  '((((min-colors 16581375)) :foreground "#8080c0" :box t :bold t)
    (((min-colors 8)) :background "cyan")
    (t :inverse-video t))
  "Face for gensyms created in the first level of macro expansion."
  :group 'macrostep)

(defface macrostep-gensym-2
  '((((min-colors 16581375)) :foreground "#8fbc8f" :box t :bold t)
    (((min-colors 8)) :background "#00cd00")
    (t :inverse-video t))
  "Face for gensyms created in the second level of macro expansion."
  :group 'macrostep)

(defface macrostep-gensym-3
  '((((min-colors 16581375)) :foreground "#daa520" :box t :bold t)
    (((min-colors 8)) :background "yellow")
    (t :inverse-video t))
  "Face for gensyms created in the third level of macro expansion."
  :group 'macrostep)

(defface macrostep-gensym-4
  '((((min-colors 16581375)) :foreground "#cd5c5c" :box t :bold t)
    (((min-colors 8)) :background "red")
    (t :inverse-video t))
  "Face for gensyms created in the fourth level of macro expansion."
  :group 'macrostep)

(defface macrostep-gensym-5
  '((((min-colors 16581375)) :foreground "#da70d6" :box t :bold t)
    (((min-colors 8)) :background "magenta")
    (t :inverse-video t))
  "Face for gensyms created in the fifth level of macro expansion."
  :group 'macrostep)

(defface macrostep-expansion-highlight-face
  '((((min-colors 16581375) (background light)) :background "#eee8d5")
    (((min-colors 16581375) (background dark)) :background "#222222"))
  "Face for macro-expansion highlight."
  :group 'macrostep)

(defface macrostep-macro-face
  '((t :underline t))
  "Face for macros in macro-expanded code."
  :group 'macrostep)

(defcustom macrostep-expand-in-separate-buffer nil
  "When non-nil, show expansions in a separate buffer instead of inline."
  :group 'macrostep
  :type 'boolean)

;; Need the following for making the ring of faces
(defun macrostep-make-ring (&rest items)
  "Make a ring containing all of ITEMS with no empty slots."
  (let ((ring (make-ring (length items))))
    (mapc (lambda (item) (ring-insert ring item)) (reverse items))
    ring))

(defvar macrostep-gensym-faces
  (macrostep-make-ring
   'macrostep-gensym-1 'macrostep-gensym-2 'macrostep-gensym-3
   'macrostep-gensym-4 'macrostep-gensym-5)
  "Ring of all macrostepper faces for fontifying gensyms.")


;;; Define keymap and minor mode
(defvar macrostep-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'macrostep-expand)
    (define-key map "=" 'macrostep-expand)
    (define-key map "e" 'macrostep-expand)

    (define-key map (kbd "DEL") 'macrostep-collapse)
    (define-key map "u" 'macrostep-collapse)
    (define-key map "c" 'macrostep-collapse)

    (define-key map (kbd "TAB") 'macrostep-next-macro)
    (define-key map "n" 'macrostep-next-macro)
    (define-key map (kbd "M-TAB") 'macrostep-prev-macro)
    (define-key map "p" 'macrostep-prev-macro)

    (define-key map "q" 'macrostep-collapse-all)
    (define-key map (kbd "C-c C-c") 'macrostep-collapse-all)
    map)
  "Keymap for `macrostep-mode'.")

;;;###autoload
(define-minor-mode macrostep-mode
  "Minor mode for inline expansion of macros in Emacs Lisp source buffers.

\\<macrostep-keymap>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].
Use \\[macrostep-collapse-all] or collapse all visible expansions to
quit and return to normal editing.

\\{macrostep-keymap}"
  nil " Macro-Stepper"
  :keymap macrostep-keymap
  :group macrostep
  (if macrostep-mode
      (progn
        ;; Disable recording of undo information
        (setq macrostep-saved-undo-list buffer-undo-list
              buffer-undo-list t)
        ;; Remember whether buffer was read-only
        (setq macrostep-saved-read-only buffer-read-only
              buffer-read-only t)
        ;; Set up post-command hook to bail out on leaving read-only
        (add-hook 'post-command-hook 'macrostep-command-hook nil t)
        (message
         (substitute-command-keys
          "\\<macrostep-keymap>Entering macro stepper mode. Use \\[macrostep-expand] to expand, \\[macrostep-collapse] to collapse, \\[macrostep-collapse-all] to exit.")))

    ;; Exiting mode
    (if macrostep-expansion-buffer
        ;; Kill dedicated expansion buffers
        (quit-window t)
      ;; Collapse any remaining overlays
      (when macrostep-overlays (macrostep-collapse-all))
      ;; Restore undo info & read-only state
      (setq buffer-undo-list macrostep-saved-undo-list
            buffer-read-only macrostep-saved-read-only
            macrostep-saved-undo-list nil)
      ;; Remove our post-command hook
      (remove-hook 'post-command-hook 'macrostep-command-hook t))))

;; Post-command hook: bail out of macrostep-mode if the user types C-x
;; C-q to make the buffer writable again.
(defun macrostep-command-hook ()
  (if (not buffer-read-only)
      (macrostep-mode 0)))


;;; Interactive functions
;;;###autoload
(defun macrostep-expand ()
  "Expand the Elisp macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only. If macrostep-mode is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any."
  (interactive)
  (let ((sexp (macrostep-sexp-at-point))
        (macrostep-environment (macrostep-environment-at-point)))
    (when (not (macrostep-macro-form-p sexp))
      (condition-case nil
	  (progn
	    (macrostep-next-macro)
	    (setq sexp (macrostep-sexp-at-point)))
	(error
	 (if (consp sexp)
	     (error "(%s ...) is not a macro form" (car sexp))
	   (error "Text at point is not a macro form.")))))

    ;; Create a dedicated macro-expansion buffer and copy the text to
    ;; be expanded into it, if required
    (when (and macrostep-expand-in-separate-buffer
               (not macrostep-expansion-buffer))
      (let ((buffer
             (get-buffer-create (generate-new-buffer-name "*macro expansion*"))))
        (set-buffer buffer)
        (emacs-lisp-mode)
        (setq macrostep-expansion-buffer t)
        (setq macrostep-outer-environment macrostep-environment)
        (save-excursion
          (let ((print-level nil)
                (print-length nil))
            (print sexp (current-buffer))))
        (pop-to-buffer buffer)))
    
    (let* ((inhibit-read-only t)
	   (expansion (macrostep-expand-1 sexp))
	   (existing-ol (macrostep-overlay-at-point))
	   (macrostep-gensym-depth macrostep-gensym-depth)
	   (macrostep-gensyms-this-level nil)
	   text priority)
      (unless macrostep-mode (macrostep-mode t))
      (if existing-ol			; expanding an expansion
	  (setq text sexp
		priority (1+ (overlay-get existing-ol 'priority))

		macrostep-gensym-depth
		(overlay-get existing-ol 'macrostep-gensym-depth))
	;; expanding buffer text
	(setq text (buffer-substring (point) (scan-sexps (point) 1))
	      priority 1
	      macrostep-gensym-depth -1))

      (with-silent-modifications
        (atomic-change-group
          (macrostep-replace-sexp-at-point expansion)
          (let ((new-ol
                 (make-overlay (point)
                               (scan-sexps (point) 1))))
            ;; Move overlay over newline to make it prettier
            (when (equal (char-after (overlay-end new-ol)) ?\n)
              (move-overlay new-ol
                            (overlay-start new-ol) (+ (overlay-end new-ol) 1)))
            ;; Highlight the overlay in original source buffers only
            (unless macrostep-expansion-buffer
              (overlay-put new-ol 'face 'macrostep-expansion-highlight-face))
            (overlay-put new-ol 'evaporate t)
            (overlay-put new-ol 'priority priority)
            (overlay-put new-ol 'macrostep-original-text text)
            (overlay-put new-ol 'macrostep-gensym-depth macrostep-gensym-depth)
            (push new-ol macrostep-overlays)))))))

(defun macrostep-collapse ()
  "Collapse the innermost macro expansion near point to its source text.

If no more macro expansions are visible after this, exit
`macrostep-mode'."
  (interactive)
  (let ((overlay (macrostep-overlay-at-point)))
    (when (not overlay) (error "No macro expansion at point"))
    (let ((buffer-read-only nil))
      (with-silent-modifications
        (atomic-change-group
          (macrostep-collapse-overlay overlay)))))
  (if (not macrostep-overlays)
      (macrostep-mode 0)))

(defun macrostep-collapse-all ()
  "Collapse all visible macro expansions and exit `macrostep-mode'."
  (interactive)
  (let ((buffer-read-only nil))
    (with-silent-modifications
      (dolist (overlay macrostep-overlays)
        (let ((outermost (= (overlay-get overlay 'priority) 1)))
          ;; We only need restore the original text for the outermost
          ;; overlays
          (macrostep-collapse-overlay overlay (not outermost))))))
  (setq macrostep-overlays nil)
  (macrostep-mode 0))

(defun macrostep-next-macro ()
  "Move point forward to the next macro form in macro-expanded text."
  (interactive)
  (let* ((start 
	 (if (get-text-property (point) 'macrostep-expanded-text)
	     (1+ (point))
	   (point)))
	 (next (next-single-property-change start 'macrostep-expanded-text)))
    (if next
	(goto-char next)
      (error "No more macro forms found"))))

(defun macrostep-prev-macro ()
  "Move point back to the previous macro form in macro-expanded text."
  (interactive)
  (let (prev)
    (save-excursion
      (while
	  (progn
	    (setq prev
		  (previous-single-property-change (point) 'macrostep-expanded-text))
	    (if (or (not prev)
		    (get-text-property (1- prev) 'macrostep-expanded-text))
		nil
	      (prog1 t (goto-char prev))))))
    (if prev
	(goto-char (1- prev))
      (error "No previous macro form found"))))



;;; Utility functions
(defun macrostep-macro-form-p (form)
  "Return t if FORM is a sexp that would be evaluated via macro expansion."
  (if (or (not (consp form))
	  (eq (car form) 'lambda)) 	; hack
      nil
    (condition-case err
        (or
         ;; Locally bound as a macro?
         (assq (car form) macrostep-environment)
         ;; Globally defined?
         (let ((fun (indirect-function (car form))))
           (and (consp fun)
                (or (eq (car fun) 'macro)
                    (and
                     (eq (car fun) 'autoload)
                     (eq (nth 4 fun) 'macro))))))
      (error nil))))

(defun macrostep-macro-definition (form)
  "Return, as a function, the macro definition to apply in expanding FORM."
  (or
   ;; Locally bound by `macrolet'
   (cdr (assq (car form) macrostep-environment))
   ;; Globally defined
   (let ((fun (indirect-function (car form))))
     (if (consp fun)
         (case (car fun)
           ((macro)
            (cdr fun))

           ((autoload)
            (load-library (nth 1 fun))
            (macrostep-macro-definition form)))
       (error "(%s ...) is not a macro form" form)))))

(defun macrostep-expand-1 (form)
  "Return result of macro-expanding the top level of FORM by exactly one step.
Unlike `macroexpand', this function does not continue macro
expansion until a non-macro-call results."
  (if (not (macrostep-macro-form-p form)) form
    (apply (macrostep-macro-definition form) (cdr form))))

(defun macrostep-environment-at-point ()
  "Return the local macro-expansion environment at point, if any.

The local environment includes macros declared by any `macrolet'
or `cl-macrolet' forms surrounding point.

The return value is an alist of elements (NAME . FUNCTION), where
NAME is the symbol locally bound to the macro and FUNCTION is the
lambda expression that returns its expansion."
  (save-excursion
    (let
        ((enclosing-form
          (ignore-errors
            (backward-up-list)
            (read (copy-marker (point))))))
      (pcase enclosing-form
        (`(,(or `macrolet `cl-macrolet) ,bindings . ,_)
          (let ((binding-environment
                 (macrostep-bindings-to-environment bindings))
                (enclosing-environment
                 (macrostep-environment-at-point)))
            (append binding-environment enclosing-environment)))
        (`nil
         (if macrostep-expansion-buffer
             macrostep-outer-environment
           nil))
        (_
         (macrostep-environment-at-point))))))

(defun macrostep-bindings-to-environment (bindings)
  "Return the macro-expansion environment declared by BINDINGS as an alist.

BINDINGS is a list in the form expected by `macrolet' or
`cl-macrolet'.  The return value is an alist, as described in
`macrostep-environment-at-point'."
  ;; So that the later elements of bindings properly shadow the
  ;; earlier ones in the returned environment, we must reverse the
  ;; list before mapping over it.
  (cl-loop for (name . forms) in (reverse bindings)
           collect
           ;; Adapted from the definition of `cl-macrolet':
           (let ((res (cl--transform-lambda forms name)))
             (eval (car res))
             (cons name `(lambda ,@(cdr res))))))

(defun macrostep-overlay-at-point ()
  "Return the innermost macro stepper overlay at point."
  (let ((result
	 (get-char-property-and-overlay (point) 'macrostep-original-text)))
    (cdr result)))

(defun macrostep-sexp-at-point ()
  "Return the sexp near point for purposes of macro-stepper expansion.

If the sexp near point is part of a macro expansion, returns the
saved text of the macro expansion, and does not read from the
buffer. This preserves uninterned symbols in the macro expansion,
so that they can be colored consistently. See also
`macrostep-print-sexp'.

Also moves point to the beginning of the returned s-expression."
  (if (not (looking-at "[(`]"))
      (backward-up-list 1))
  (if (equal (char-before) ?`)
      (backward-char))
  (or (get-text-property (point) 'macrostep-expanded-text)
      (progn
	;; use scan-sexps for the side-effect of producing an error
	;; message for unbalanced parens, etc.
	(scan-sexps (point) 1)
	(sexp-at-point))))


(defun macrostep-collapse-overlay (overlay &optional no-restore-p)
  "Collapse a macro-expansion overlay and restore the unexpanded source text.

As a minor optimization, does not restore the original source
text if NO-RESTORE-P is non-nil. This is safe to do when
collapsing all the sub-expansions of an outer overlay, since the
outer overlay will restore the original source itself.

Also removes the overlay from `macrostep-overlays'."
  (when (and (overlay-start overlay)
	     (eq (overlay-buffer overlay) (current-buffer)))
      ;; If we're cleaning up we don't need to bother restoring text
      ;; or checking for inner overlays to delete
      (unless no-restore-p
	(macrostep-collapse-overlays-in
	 (overlay-start overlay) (overlay-end overlay))
	(let ((text (overlay-get overlay 'macrostep-original-text)))
	  (goto-char (overlay-start overlay))
	  (macrostep-replace-sexp-at-point text (stringp text))))
      ;; Remove overlay from the list and delete it
      (setq macrostep-overlays
	    (delq overlay macrostep-overlays))
      (delete-overlay overlay)))

(defun macrostep-collapse-overlays-in (start end)
  "Collapse all macrostepper overlays that are strictly between START and END.

Will not collapse overlays that begin at START and end at END."
  (dolist (ol (overlays-in start end))
    (if (and (> (overlay-start ol) start)
	     (< (overlay-end ol) end)
	     (overlay-get ol 'macrostep-original-text))
	(macrostep-collapse-overlay ol t))))

(defun macrostep-replace-sexp-at-point (sexp &optional original)
  "Replace the form following point with SEXP.

If ORIGINAL is non-nil, SEXP is assumed to be a string
representing the original source text, and inserted verbatim as a
replacement for the form following point. Otherwise, if ORIGINAL
is nil, SEXP is treated as the macro expansion of the source,
inserted using `macrostep-print-sexp' and pretty-printed using
`pp-buffer'."
  (let ((print-quoted t))
    (save-excursion
      ;; Insert new text first so that existing overlays don't
      ;; evaporate
      (if original
	  (insert sexp)                 ; insert original source text
	(macrostep-print-sexp sexp))
      ;; Delete the old form and remove any sub-form overlays in it
      (let ((start (point)) (end (scan-sexps (point) 1)))
	(macrostep-collapse-overlays-in start end)
	(delete-region start end)))
	
    (unless original                   ; inserting macro expansion
      (save-restriction
        ;; point is now before the expanded form; pretty-print it
        (narrow-to-region (point) (scan-sexps (point) 1))
        (save-excursion
          (pp-buffer)
          ;; remove the extra newline that pp-buffer inserts
          (goto-char (point-max))
          (delete-region
           (point)
           (save-excursion (skip-chars-backward " \t\n") (point))))
        (widen)
        (indent-sexp)))))

(defun macrostep-get-gensym-face (symbol)
  "Return the face to use in fontifying SYMBOL in printed macro expansions.

All symbols introduced in the same level of macro expansion are
fontified using the same face (modulo the number of faces; see
`macrostep-gensym-faces')."
  (or (get symbol 'macrostep-gensym-face)
      (progn
	(if (not macrostep-gensyms-this-level)
	    (setq macrostep-gensym-depth (1+ macrostep-gensym-depth)
		  macrostep-gensyms-this-level t))
	(let ((face (ring-ref macrostep-gensym-faces macrostep-gensym-depth)))
	  (put symbol 'macrostep-gensym-face face)
	  face))))

(defmacro macrostep-propertize (form &rest plist)
  "Evaluate FORM, applying syntax properties in PLIST to any inserted text."
  (declare (indent 1)
           (debug (&rest form)))
  (let ((start (make-symbol "start")))
    `(let ((,start (point)))
       (prog1
           ,form
         ,@(loop for (key value) on plist by #'cddr
                 collect `(put-text-property ,start (point)
                                             ,key ,value))))))

(defun macrostep-print-sexp (sexp &optional quoted-form-p)
  "Pretty-print SEXP, a macro expansion, in the current buffer.

Fontifies uninterned symbols and macro forms using
`font-lock-face' property, and saves the actual text of SEXP's
sub-forms as the `macrostep-expanded-text' text property so that
any uninterned symbols can be reused in macro expansions of the
sub-forms.  If QUOTED-FORM-P is non-nil then any macros in the
expansion will not be fontified.  See also
`macrostep-sexp-at-point'."
  (cond
   ((symbolp sexp)
    ;; Fontify gensyms
    (if (not (eq sexp (intern-soft (symbol-name sexp))))
        (macrostep-propertize
            (prin1 sexp (current-buffer))
          'font-lock-face (macrostep-get-gensym-face sexp))
      ;; Print other symbols as normal
      (prin1 sexp (current-buffer))))

   ((listp sexp)
    ;; Print quoted and quasiquoted forms nicely.
    (let ((head (car sexp)))
      (cond ((and (eq head 'quote)	; quote
		  (= (length sexp) 2))
	     (insert "'")
	     (macrostep-print-sexp (cadr sexp) t))

            ((and (eq head '\`)         ; backquote
                  (= (length sexp) 2))
             ;; Treat backquote as a macro
             (macrostep-propertize
                 (insert "`")
               'macrostep-expanded-text sexp
               'font-lock-face 'macrostep-macro-face)
             (macrostep-print-sexp (cadr sexp) t))

	    ((and (memq head '(\, \,@)) ; unquote
		  (= (length sexp) 2))
	     (princ head (current-buffer))
	     (macrostep-print-sexp (cadr sexp)))

	    (t				; other list form
             ;; If the sexp is a (cl-)macrolet form, the
             ;; macro-expansion environment should be extended using
             ;; its bindings while printing the body forms in order to
             ;; correctly mark any uses of locally-bound macros. (See
             ;; `with-js' in `js.el.gz' for an example of a macro that
             ;; works this way).
             (let ((extended-environment
                    (pcase sexp
                      (`(,(or `cl-macrolet `macrolet) ,bindings . ,_)
                        (append (macrostep-bindings-to-environment bindings)
                                macrostep-environment))
                      (_ macrostep-environment))))
               
               ;; Is it an (unquoted) macro form?
               (if (and (not quoted-form-p)
                        (macrostep-macro-form-p sexp))
                   (progn
                     ;; Save the real expansion as a text property on the
                     ;; opening paren
                     (macrostep-propertize
                         (insert "(")
                       'macrostep-expanded-text sexp)
                     ;; Fontify the head of the macro
                     (macrostep-propertize
                         (macrostep-print-sexp head)
                       'font-lock-face 'macrostep-macro-face))
                 ;; Not a macro form
                 (insert "(")
                 (macrostep-print-sexp head quoted-form-p))

               ;; Print remaining list elements
               (setq sexp (cdr sexp))
               (when sexp (insert " "))
               ;; macrostep-environment will be setq'd after printing
               ;; the second element of the list (i.e., the binding
               ;; list in a macrolet form)
               (let ((macrostep-environment macrostep-environment))
                 (while sexp
                   (if (listp sexp)
                       (progn
                         (macrostep-print-sexp (car sexp) quoted-form-p)
                         (when (cdr sexp) (insert " "))
                         (setq sexp (cdr sexp))
                         ;; At this point the first and second
                         ;; elements of the list have been printed, so
                         ;; it is time to extend the macro-expansion
                         ;; environment inside a macrolet for the body
                         ;; forms.
                         (setq macrostep-environment extended-environment))
                     ;; Print tail of dotted list
                     (insert ". ")
                     (macrostep-print-sexp sexp)
                     (setq sexp nil))))
               (insert ")"))))))

   ;; Print everything except symbols and lists as normal
   (t (prin1 sexp (current-buffer)))))



(provide 'macrostep)

;;; macrostep.el ends here
