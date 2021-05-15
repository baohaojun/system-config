;;; slime-company.el --- slime completion backend for company mode -*-lexical-binding:t-*-
;;
;; Copyright (C) 2009-2021  Ole Arndt
;;
;; Author: Ole Arndt <anwyn@sugarshark.com>
;; Keywords: convenience, lisp, abbrev
;; Package-Version: 20210124.1627
;; Package-Commit: f20ecc4104d4c35052696e7e760109fb02060e72
;; Version: 1.6
;; Package-Requires: ((emacs "24.4") (slime "2.13") (company "0.9.0"))
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a backend implementation for the completion package
;; company-mode by Nikolaj Schumacher. More info about this package
;; is available at http://company-mode.github.io/
;;
;; As of version 1.0 this completion backend supports the normal and
;; the fuzzy completion modes of SLIME.
;;
;;; Installation:
;;
;;  Put this file somewhere into your load-path
;;  (or just into slime-path/contribs) and then call
;;
;;   (slime-setup '(slime-company))
;;
;; I also have the following, IMO more convenient key bindings for
;; company mode in my .emacs:
;;
;;   (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;   (define-key company-active-map (kbd "M-.") 'company-show-location)
;;
;;; Code:

(require 'slime)
(require 'company)
(require 'cl-lib)
(require 'eldoc)
(require 'subr-x)

(define-slime-contrib slime-company
  "Interaction between slime and the company completion mode."
  (:license "GPL")
  (:authors "Ole Arndt <anwyn@sugarshark.com>")
  (:swank-dependencies swank-arglists)
  (:on-load
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (add-hook h 'slime-company-maybe-enable)))
  (:on-unload
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (remove-hook h 'slime-company-maybe-enable))
   (slime-company-disable)))

;;; ----------------------------------------------------------------------------
;;; * Customization

(defgroup slime-company nil
  "Interaction between slime and the company completion mode."
  :group 'company
  :group 'slime)

(defcustom slime-company-after-completion nil
  "What to do after a successful completion.
In addition to displaying the arglist slime-company will also do one of:

- `nil':  nothing,
- insert a space. Useful if space does not select the completion candidate.
  Works best if you also call `delete-horizontal-space' before closing
  parentheses to remove excess whitespace.
- call an arbitrary function with the completion string as the first parameter.
  Do not call company-complete inside this function, company doesn't like to
  be invoked recursively.
"
  :group 'slime-company
  :type '(choice
          (const :tag "Do nothing" nil)
          (const :tag "Insert space" slime-company-just-one-space)
          (function :tag "Custom function" nil)))

(defcustom slime-company-transform-arglist 'downcase
  "Before echoing the arglist it is passed to this function for transformation."
  :group 'slime-company
  :type '(choice
          (const :tag "Downcase" downcase)
          (const :tag "Do nothing" identity)
          (function :tag "Custom function" nil)))

(defcustom slime-company-display-arglist nil
  "Whether to display the arglist of a function in the company popup."
  :group 'slime-company
  :type '(choice
          (const :tag "Hide arglist" nil)
          (const :tag "Show arglist" t)))

(defcustom slime-company-display-flags t
  "Whether to display the symbol's flags in the company popup.
Symbol flags are only returned with the `fuzzy' completion type."
  :group 'slime-company
  :type '(choice
          (const :tag "Hide flags" nil)
          (const :tag "Show flags" t)))

(defcustom slime-company-completion 'simple
  "Which Slime completion method to use: `simple' or `fuzzy'.

`simple' just displays the completion candidate,
`fuzzy' also displays the classification flags as an annotation,
alignment of annotations via `company-tooltip-align-annotations'
is recommended. This method also can complete package names.
"
  :group 'slime-company
  :type '(choice
          (const simple)
          (const fuzzy)))

(defcustom slime-company-complete-in-comments-and-strings nil
  "Should slime-company also complete in comments and strings."
  :group 'slime-company
  :type 'boolean)

(defcustom slime-company-major-modes
  '(lisp-mode clojure-mode slime-repl-mode scheme-mode)
  "List of major modes in which slime-company should be active.
Slime-company actually calls `derived-mode-p' on this list, so it will
be active in derived modes as well."
  :group 'slime-company
  :type '(repeat symbol))

(defun slime-company-just-one-space (completion-string)
  (unless (string-suffix-p ":" completion-string)
    (just-one-space)))

(defsubst slime-company-active-p ()
  "Test if the slime-company backend should be active in the current buffer."
  (apply #'derived-mode-p slime-company-major-modes))

(define-derived-mode slime-company-doc-mode help-mode "Doc"
  "Documentation mode for slime-company."
  (setq font-lock-defaults
        '((("^\\([^ ]\\{4,\\}\\)\\b" . (1 font-lock-function-name-face t))
           ("^[ 	]*\\b\\([A-Z][A-Za-z0-9_ 	%\\*\\-]+:\\)\\([ 	]\\|$\\)"
            . (1 font-lock-doc-face))
           ("^\\([A-Z][A-Za-z ]+:\\)\\([ 	]\\|$\\)"
            . (1 font-lock-doc-face t))
           ("(\\(FUNCTION\\|VALUES\\|OR\\|EQL\\|LAMBDA\\)\\b"
            . (1 font-lock-keyword-face))
           ("[	 (]+\\(&[A-Z0-9\\-]+\\)\\b" . (1 font-lock-type-face))
           ("[	 (]+\\(:[A-Z0-9\\-]+\\)\\b" . (1 font-lock-builtin-face))
           ("\\b\\(T\\|t\\|NIL\\|nil\\|NULL\\|null\\)\\b" . (1 font-lock-constant-face))
           ("\\b[+-]?[0-9/\\.]+[sdeSDE]?\\+?[0-9]*\\b" . font-lock-constant-face)
           ("#[xX][+-]?[0-9A-F/]+\\b" . font-lock-constant-face)
           ("#[oO][+-]?[0-7/]+\\b" . font-lock-constant-face)
           ("#[bB][+-]?[01/]+\\b" . font-lock-constant-face)
           ("#[0-9]+[rR][+-]?[0-9A-Z/]+\\b" . font-lock-constant-face)
           ("\\b\\([A-Z0-9:+%<>#*\\.\\-]\\{2,\\}\\)\\b"
            . (1 font-lock-variable-name-face))))))

;;; ----------------------------------------------------------------------------
;;; * Activation

(defun slime-company-maybe-enable ()
  (when (slime-company-active-p)
    (company-mode 1)
    (add-to-list 'company-backends 'company-slime)
    (unless (slime-find-contrib 'slime-fuzzy)
      (setq slime-company-completion 'simple))))

(defun slime-company-disable ()
  (setq company-backends (remove 'company-slime company-backends)))

;;; ----------------------------------------------------------------------------
;;; * Internals

(defun slime-company--fetch-candidates-async (prefix)
  (when (slime-connected-p)
    (cl-ecase slime-company-completion
      (simple (slime-company--fetch-candidates-simple prefix))
      (fuzzy (slime-company--fetch-candidates-fuzzy prefix)))))

(defun slime-company--fetch-candidates-simple (prefix)
  (let ((slime-current-thread :repl-thread)
        (package (slime-current-package)))
    (cons :async
          (lambda (callback)
            (slime-eval-async
                `(swank:simple-completions ,prefix ',package)
              (lambda (result)
                (funcall callback (car result)))
              package)))))

(defun slime-company--fetch-candidates-fuzzy (prefix)
  (let ((slime-current-thread :repl-thread)
        (package (slime-current-package)))
    (cons :async
          (lambda (callback)
            (slime-eval-async
                `(swank:fuzzy-completions ,prefix ',package)
              (lambda (result)
                (funcall callback
                         (mapcar
                          (lambda (completion)
                            (cl-destructuring-bind (sym score _ flags)
                                completion
                              (propertize sym 'score score 'flags flags)))
                          (car result))))
              package)))))

(defun slime-company--fontify-lisp-buffer ()
  "Return a buffer in lisp-mode usable for fontifying lisp expressions."
  (let ((buffer-name " *slime-company-fontify*"))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (unless (derived-mode-p 'lisp-mode)
            ;; Advice from slime: Just calling (lisp-mode) will turn slime-mode
            ;; on in that buffer, which may interfere with the calling function
            (setq major-mode 'lisp-mode)
            (lisp-mode-variables t))
          (current-buffer)))))

(defun slime-company--fontify-lisp (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  ;; copied functionality from slime, trimmed somewhat
  (with-current-buffer (slime-company--fontify-lisp-buffer)
    (erase-buffer)
    (insert (funcall slime-company-transform-arglist string))
    (let ((font-lock-verbose nil))
      (font-lock-fontify-region (point-min) (point-max)))
    (goto-char (point-min))
    (buffer-substring (point-min) (point-max))))

(defun slime-company--format (doc)
  (let ((doc (slime-company--fontify-lisp doc)))
    (cond ((eq eldoc-echo-area-use-multiline-p t) doc)
	  (t (slime-oneliner (replace-regexp-in-string "[ \n\t]+" " " doc))))))

(defun slime-company--arglist (arg)
  (let ((arglist (slime-eval
                  `(swank:operator-arglist ,arg ,(slime-current-package)))))
    (when arglist
      (slime-company--format arglist))))

(defun slime-company--arglist-only (arg)
  (let ((arglist (slime-eval
                  `(swank:operator-arglist ,arg ,(slime-current-package)))))
    (when arglist
      (replace-regexp-in-string
       (concat "(" (funcall slime-company-transform-arglist arg) " ")
       " (" (funcall slime-company-transform-arglist arglist) t t))))

(defun slime-company--echo-arglist (arg)
  (slime-eval-async `(swank:operator-arglist ,arg ,(slime-current-package))
    (lambda (arglist)
      (when arglist
        (slime-message "%s" (slime-company--format arglist))))))

(defun slime-company--package-name (pkg)
  "Convert a string into into a uninterned symbol name, if it looks
like a package name, i.e. if it has a trailing colon.
Returns NIL if the string does not look like a package name."
  (when (string-suffix-p ":" pkg)
    (format "#:%s" (string-remove-suffix ":" (string-remove-suffix ":" pkg)))))

(defun slime-company--build-describe-request (candidate &optional verbose)
  (let ((pkg-name (slime-company--package-name candidate)))
    (cond (pkg-name
           `(swank::describe-to-string
             (cl:find-package
              (cl:symbol-name (cl:read-from-string ,pkg-name)))))
          (verbose
           `(swank:describe-symbol ,candidate))
          (t
           `(swank:documentation-symbol ,candidate)))))

(defun slime-company--fontify-doc-buffer (&optional doc)
  "Return a buffer in `slime-compary-doc-mode' usable for fontifying documentation."
  (with-current-buffer (company-doc-buffer)
      (slime-company-doc-mode)
      (setq buffer-read-only nil)
      (when doc
        (insert doc))
      (goto-char (point-min))
      (current-buffer)))

(defun slime-company--doc-buffer (candidate)
  "Show the Lisp symbol documentation for CANDIDATE in a buffer.
Shows more type info than `slime-company--quickhelp-string'."
  (let* ((slime-current-thread :repl-thread))
    (slime-company--fontify-doc-buffer
     (slime-eval (slime-company--build-describe-request candidate t)
                 (slime-current-package)))))

(defun slime-company--quickhelp-string (candidate)
  "Retrieve the Lisp symbol documentation for CANDIDATE.
This function does not fontify and displays the result of SWANK's
`documentation-symbol' function, instead of the more verbose `describe-symbol'."
  (let ((slime-current-thread :repl-thread))
    (slime-eval (slime-company--build-describe-request candidate)
                (slime-current-package))))

(defun slime-company--location (candidate)
  (let ((source-buffer (current-buffer)))
    (save-window-excursion
      (slime-edit-definition candidate)
      (let ((buffer (if (eq source-buffer (current-buffer))
                        slime-xref-last-buffer
                      (current-buffer))))
        (when (buffer-live-p buffer)
          (cons buffer (with-current-buffer buffer
                         (point))))))))

(defun slime-company--post-completion (candidate)
  (slime-company--echo-arglist candidate)
  (when (functionp slime-company-after-completion)
    (funcall slime-company-after-completion candidate)))

(defun slime-company--in-string-or-comment ()
  "Return non-nil if point is within a string or comment.
In the REPL we disregard anything not in the current input area."
  (save-restriction
    (when (derived-mode-p 'slime-repl-mode)
      (narrow-to-region slime-repl-input-start-mark (point)))
    (let* ((sp (syntax-ppss))
           (beg (nth 8 sp)))
      (when (or (eq (char-after beg) ?\")
                (nth 4 sp))
        beg))))

;;; ----------------------------------------------------------------------------
;;; * Company backend function

(defvar *slime-company--meta-request* nil
  "Workaround lock for company-quickhelp, which invokes 'quickhelp-string' or
doc-buffer' while a 'meta' request is running, causing SLIME to cancel requests.")

(defun company-slime (command &optional arg &rest ignored)
  "Company mode backend for slime."
  (let ((candidate (and arg (substring-no-properties arg))))
    (cl-case command
      (init
       (slime-company-active-p))
      (prefix
       (when (and (slime-company-active-p)
                  (slime-connected-p)
                  (or slime-company-complete-in-comments-and-strings
                      (null (slime-company--in-string-or-comment))))
         (company-grab-symbol)))
      (candidates
       (slime-company--fetch-candidates-async candidate))
      (meta
       (let ((*slime-company--meta-request* t))
         (slime-company--arglist candidate)))
      (annotation
       (concat (when slime-company-display-arglist
                 (slime-company--arglist-only candidate))
               (when slime-company-display-flags
                 (concat " " (get-text-property 0 'flags arg)))))
      (doc-buffer
       (unless *slime-company--meta-request*
         (slime-company--doc-buffer candidate)))
      (quickhelp-string
       (unless *slime-company--meta-request*
         (slime-company--quickhelp-string candidate)))
      (location
       (slime-company--location candidate))
      (post-completion
       (slime-company--post-completion candidate))
      (sorted
       (eq slime-company-completion 'fuzzy)))))

(provide 'slime-company)

;;; slime-company.el ends here
