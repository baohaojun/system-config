;;; ivy-todo.el --- Manage org-mode TODOs with ivy   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/ivy-todo
;; Package-Version: 20170315.334
;; Version: 1.00
;; Keywords: convenience
;; Package-Requires: ((ivy "0.8.0") (emacs "24.3"))

;;; Commentary:

;; The file `ivy-todo-file' should be an `org-mode' file containing TODO lists.
;; These lists can be modified using `ivy-todo'. The last used TODO list while
;; be buffer-locally saved. ivy-todo will try to guess the TODO list (based on
;; the project you're working on) if `ivy-todo-guess-list' is t.

;;; Code:
(require 'org-element)
(require 'ivy)

(defvar ivy-todo-file (expand-file-name "ivy-todo.org" org-directory)
  "The `org-mode' file upon which ivy-todo operates.
Level 1 headlines are recognized as TODO lists.")

(defvar ivy-todo-headline nil
  "The level 1 headline in `ivy-todo-file` which contain the active TODO-list.
Normally use `ivy-todo--buffer-headline-name' instead of accessing this variable.")
(make-variable-buffer-local 'ivy-todo-headline)

(defvar ivy-todo-guess-list t
  "Whether to guess TODO list based on the current project.")

(defun ivy-todo--headlines ()
  "Return an list of level 1 headlines in `ivy-todo-file'."
  (org-element-map (ivy-todo--ast) 'headline
    (lambda (h1)
      (when (= (org-element-property :level h1) 1)
        (org-element-property :raw-value h1)))))

(defun ivy-todo--get-headline (headline)
  "Return HEADLINE if HEADLINE exists in `ivy-todo-file'.
Ask if the user want to add it if it doesn't exist."
  (when headline
    (or
     (org-element-map (ivy-todo--ast) 'headline
       (lambda (h1)
         (when (and (= (org-element-property :level h1) 1)
                    (equal (org-element-property :raw-value h1) headline))
           (cons headline (org-element-property :begin h1))))
       nil t)
     (if (y-or-n-p (concat headline " doesn't exist. Would you like to created it?"))
         (progn
           (ivy-todo--replace-ast
            (org-element-adopt-elements
             (ivy-todo--ast)
             (org-element-create 'headline `(:level 1 ,:title (,headline)))))
           (ivy-todo--get-headline headline))
       nil))))

(defun ivy-todo--guess-headline-name ()
  "Guess the headline name associated with the current buffer.
Return nil if no headline is found, else a string."
  (cond ((and (require 'projectile nil t)
              (projectile-project-name)
              (not (equal (projectile-project-name) "-")))
         (ivy-todo--get-headline (projectile-project-name)))
        ((require 'find-file-in-project nil t)
         (ivy-todo--get-headline (file-name-base (directory-file-name (ffip-get-project-root-directory)))))
        ((and (version<= "25" emacs-version)
              (require 'vc)
              (vc-root-dir))
         (ivy-todo--get-headline (file-name-base (directory-file-name (vc-root-dir)))))
        (t nil)))

(defun ivy-todo--buffer-headline-name ()
  "Get the name of the headline associated with the current buffer.
Set `ivy-todo-headline` to the headline name."
  (if ivy-todo-headline
      ivy-todo-headline
    (setq ivy-todo-headline
          (let ((headlines (ivy-todo--headlines)))
            (if ivy-todo-guess-list
                (or (ivy-todo--guess-headline-name)
                    (ivy-todo--get-headline (completing-read "TODO list: " headlines)))
              (ivy-todo--get-headline (completing-read "TODO list: " headlines)))))))

(defun ivy-todo--list-items ()
  "Return alist of todo items of `ivy-todo-headline'.
The car is the name and the cdr is the position in `ivy-todo-file'."
  (org-element-map (ivy-todo--ast) 'headline
    (lambda (h1)
      (when (and (= (org-element-property :level h1) 1)
                 (string-equal (car (ivy-todo--buffer-headline-name))
                               (org-element-property :raw-value h1)))
        (org-element-map (org-element-contents h1) 'headline
          (lambda (todo-item)
            (when (org-element-property :todo-type todo-item)
              (cons (car (split-string (org-element-interpret-data todo-item) "\n"))
                    (org-element-property :begin todo-item)))))))
    nil t))

(defun ivy-todo--ast ()
  "Get the AST for the TODO file."
  (delay-mode-hooks
    (with-current-buffer (find-file-noselect ivy-todo-file)
      (org-element-parse-buffer))))

(defun ivy-todo--replace-ast (ast)
  "Replace AST of `ivy-todo-file' and save the file."
  (with-temp-buffer
    (insert (org-element-interpret-data ast))
    (write-file ivy-todo-file)))

(defun ivy-todo--old-or-new-item (item pos)
  "If ITEM exist in `ivy-todo-file' goto its position, else insert it after POS.
Meant to be used with `ivy-todo-file' as `current-buffer'."
  (if (stringp item)
      (progn
        (goto-char pos)
        (goto-char (line-end-position))
        (org-insert-todo-subheading 1)
        (insert item))
    (goto-char (cdr item))))

;;;###autoload
(defun ivy-todo (&optional arg)
  "Read and manipulate entries in `ivy-todo-file'.
The default action changes the TODO state of the selected entry.
With a `\\[universal-argument]' ARG, first change the active TODO list.
With a `\\[universal-argument] \\[universal-argument]' ARG, change `ivy-todo-file'."
  (interactive "p")
  (when (= arg 16)
    (setq ivy-todo-file (expand-file-name (read-file-name "TODO file: " org-directory ivy-todo-file))))
  (when (> arg 1)
    (setq ivy-todo-headline
          (ivy-todo--get-headline (completing-read "TODO list: " (ivy-todo--headlines)))))
  (let ((items (ivy-todo--list-items))
        (pos (cdr ivy-todo-headline)))
    (ivy-read
     (concat "\"" (car ivy-todo-headline) "\" items: ")
     items
     :action
      (lambda (x)
        (delay-mode-hooks
          (with-current-buffer (find-file-noselect ivy-todo-file)
            (if (stringp x)
                (progn
                  (goto-char pos)
                  (goto-char (line-end-position))
                  (org-insert-todo-subheading 1)
                  (insert x))
              (goto-char (cdr x))
              (org-todo))
            (save-buffer)))))))

(defun ivy-todo-archive (headline)
  "Goto HEADLINE in `ivy-todo-file' and archive it.
HEADLINE is a string or a cons (\"headline\" . buffer-pos)."
  (let ((headline-pos (cdr ivy-todo-headline)))
    (delay-mode-hooks
      (with-current-buffer (find-file-noselect ivy-todo-file)
        (ivy-todo--old-or-new-item headline headline-pos)
        (org-archive-subtree)
        (save-buffer)))))

(defun ivy-todo-set-priority (headline)
  "Goto HEADLINE in `ivy-todo-file' and call `org-priority'.
HEADLINE is a string or a cons (\"headline\" . buffer-pos)."
  (let ((headline-pos (cdr ivy-todo-headline)))
    (delay-mode-hooks
      (with-current-buffer (find-file-noselect ivy-todo-file)
        (ivy-todo--old-or-new-item headline headline-pos)
        (org-priority)
        (save-buffer)))))

(defun ivy-todo-set-property (headline)
  "Goto HEADLINE in `ivy-todo-file' and call `org-set-property'.
HEADLINE is a string or a cons (\"headline\" . buffer-pos)."
  (let ((headline-pos (cdr ivy-todo-headline)))
    (delay-mode-hooks
      (with-current-buffer (find-file-noselect ivy-todo-file)
        (ivy-todo--old-or-new-item headline headline-pos)
        (call-interactively 'org-set-property)
        (save-buffer)))))

(defun ivy-todo-set-tags (headline)
  "Goto HEADLINE in `ivy-todo-file' and call `org-set-tags'.
HEADLINE is a string or a cons (\"headline\" . buffer-pos)."
  (let ((headline-pos (cdr ivy-todo-headline)))
    (delay-mode-hooks
      (with-current-buffer (find-file-noselect ivy-todo-file)
        (ivy-todo--old-or-new-item headline headline-pos)
        (org-set-tags)
        (save-buffer)))))

(defun ivy-todo-set-effort (headline)
  "Goto HEADLINE in `ivy-todo-file' and call `org-set-effort'.
HEADLINE is a string or a cons (\"headline\" . buffer-pos)."
  (let ((headline-pos (cdr ivy-todo-headline)))
    (delay-mode-hooks
      (with-current-buffer (find-file-noselect ivy-todo-file)
        (ivy-todo--old-or-new-item headline headline-pos)
        (org-set-effort)
        (save-buffer)))))

(defun ivy-todo-refile (headline)
  "Goto HEADLINE in `ivy-todo-file' and call `org-refile'.
HEADLINE is a string or a cons (\"headline\" . buffer-pos)."
  (let ((headline-pos (cdr ivy-todo-headline)))
    (delay-mode-hooks
      (with-current-buffer (find-file-noselect ivy-todo-file)
        (ivy-todo--old-or-new-item headline headline-pos)
        (org-refile)
        (save-buffer)))))

(ivy-set-actions
 'ivy-todo
 '(("," ivy-todo-set-priority "priority")
   ("a" ivy-todo-archive "archive")
   ("e" ivy-todo-set-effort "effort")
   ("p" ivy-todo-set-property "property")
   ("r" ivy-todo-refile "refile")
   ("t" ivy-todo-set-tags "tags")))

(provide 'ivy-todo)
;;; ivy-todo.el ends here
