;;; company-terraform.el --- A company backend for terraform

;; Copyright (C) 2017 Rafał Cieślak

;; Author: Rafał Cieślak <rafalcieslak256@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (company "0.8.12") (terraform-mode "0.06"))
;; Created: 10 August 2017
;; Keywords: abbrev, convenience, terraform, company
;; URL: https://github.com/rafalcieslak/emacs-company-terraform

;;; Commentary:

;; company-terraform provides a company backend for terraform files.  It enables
;; context-aware autocompletion for terraform sources.  This includes resource
;; and data arguments and attributes, both in resource and data blocks as well
;; as in interpolations, built-in functions and top-level keywords.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'subr-x)
(require 'terraform-mode)

(require 'company-terraform-data)

(defun company-terraform--scan-resources (dir)
  "Search .tf files in DIR for resource data and variable blocks."
  (let* ((files (directory-files dir t "\\.tf$"))
         (datas     (make-hash-table :test 'equal))
         (resources (make-hash-table :test 'equal))
         (variables '())
         (outputs '())
         (locals '())
         (modules '())
         (modules-with-dirs (make-hash-table :test 'equal)))
    (dolist (file files)
      (with-temp-buffer
        (if (find-buffer-visiting file)
            ;; If this file is being edited, use the current (possibly unsaved) version.
            (insert (with-current-buffer (find-buffer-visiting file) (buffer-string)))
          ;; Otherwise just open the file from file system.
          (ignore-errors (insert-file-contents file)))
        (goto-char 1) ; Start by searching for data and resource blocks.
        (while (re-search-forward "\\(resource\\|data\\)[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*{" nil t)
          (let* ((kind (intern (match-string-no-properties 1)))
                 (hash-to-use (cl-case kind
                                ('data datas)
                                ('resource resources)))
                 (type (match-string-no-properties 2))
                 (name (match-string-no-properties 3)))
            (when (eq 'empty (gethash type hash-to-use 'empty))
              (puthash type '() hash-to-use))
            (push name (gethash type hash-to-use))))
        (goto-char 1) ; Then search for variable blocks.
        (while (re-search-forward "variable[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*{" nil t)
          (push (match-string-no-properties 1) variables))
        (goto-char 1) ; Then search for output blocks.
        (while (re-search-forward "output[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*{" nil t)
          (push (match-string-no-properties 1) outputs))
        (goto-char 1) ; Then search for locals
        (while (re-search-forward "locals[[:space:]\n]*{" nil t)
          (let ((end (save-excursion (backward-char) (forward-sexp) (point))))
            ;; TODO: This will also find sub-keys for locals which are nested dicts.
            (while (re-search-forward "\n[[:space:]]*\\([^[:space:]\n#]*\\)[[:space:]]*=" end t)
              (push (match-string-no-properties 1) locals))
            ))
        (goto-char 1) ; Then search for modules
        (while (re-search-forward "module[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*{" nil t)
          (let ((module-name (match-string-no-properties 1))
                (end (save-excursion (backward-char) (forward-sexp) (point))))
            (push module-name modules)
            ;; Search for module source path
            (while (re-search-forward "\n[[:space:]]*source[[:space:]]*=[[:space:]]*\"\\([^\"]*\\)\"" end t)
              (let* ((module-dir-hash (secure-hash 'md5 (concat "1." module-name ";" (match-string-no-properties 1))))
                     (module-dir (concat dir ".terraform/modules/" module-dir-hash)))
                ;; TODO: If the dir does not exist, use data straight from source dir
                (puthash module-name module-dir modules-with-dirs)))
            ))))
    (list datas resources variables outputs locals modules modules-with-dirs)))

(defconst company-terraform-perdir-resource-cache
  (make-hash-table :test 'equal))

(defun company-terraform-get-resource-cache (kind &optional dir)
  "Return several dictionaries gathering names used in the project.
KIND specifies the block type requested and mey be 'resource,
'data or 'variable.  Searches for blocks in DIR or buffer's
directory if DIR is nil.  If available, uses a cached version
which lasts serval seconds."
  (nth (cl-case kind
         ('data 0)
         ('resource 1)
         ('variable 2)
         ('output 3)
         ('local 4)
         ('module 5)
         ('module-dir 6))
       (let* ((dir (or dir (file-name-directory (buffer-file-name))))
              (v (gethash dir company-terraform-perdir-resource-cache))
              (cache-time (car v))
              (resource-data (cdr v)))
         (if (and v
                  (< (- (float-time) cache-time) 20))
             resource-data
           (progn
             (message "Regenerating company-terraform resource cache for %s..." dir)
             (let ((resource-data (company-terraform--scan-resources dir)))
               (puthash dir (cons (float-time) resource-data) company-terraform-perdir-resource-cache)
               resource-data))))))

(defun company-terraform-get-context ()
  "Guess the context in terraform description where point is."
  (let ((nest-level (nth 0 (syntax-ppss)))
        (curr-ppos (nth 1 (syntax-ppss)))
        (string-state (nth 3 (syntax-ppss)))
        (string-ppos (nth 8 (syntax-ppss))))
    (cond
     ;; Resource/data type
     ((and string-state
           (save-excursion
             (goto-char string-ppos)
             (re-search-backward "\\(resource\\|data\\)[[:space:]\n]*\\=" nil t)))
      (list 'object-type (intern (match-string-no-properties 1))))
     ;; String interpolation
     ((and (> nest-level 0)
           string-state
           (save-excursion
             (re-search-backward "\\${[^\"]*\\=" nil t)))
      (list 'interpolation
            (buffer-substring
             (point)
             (save-excursion
               (with-syntax-table (make-syntax-table (syntax-table))
                 ;; Minus, asterisk and dot characters are part of the object path.
                 (modify-syntax-entry ?- "w")
                 (modify-syntax-entry ?. "w")
                 (modify-syntax-entry ?* "w")
                 (skip-syntax-backward "w")
                 (point))))))
     ;; Inside resource/data block
     ((and (eq ?{ (char-after curr-ppos))
           (save-excursion
             (goto-char curr-ppos)
             (re-search-backward "\\(resource\\|data\\|module\\)[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*\\(\"[^\"]*\"[[:space:]\n]*\\)?\\=" nil t)))
      
      (list 'block (intern (match-string-no-properties 1)) (match-string-no-properties 2)))
     ;; Top level
     ((eq 0 nest-level) 'top-level)
     (t 'no-idea))))

(defun company-terraform-test-context ()
  "Echoes a message naming the current context in a terraform file.  Useful for diagnostics."
  (interactive)
  (message "company-terraform-context: %s" (company-terraform-get-context)))

(defun company-terraform--prefix ()
  "Return the text before point that is part of a completable symbol.
Check function ‘company-mode’ docs for the details on how this
function's result is interpreted."
  (if (eq major-mode 'terraform-mode)
      (let ((context (company-terraform-get-context)))
        (pcase context
         ('no-idea nil)
         ('top-level (company-grab-symbol))
         (`(interpolation . ,_) (cons (car (last (split-string (nth 1 context) "\\."))) t))
         (`(object-type . ,_) (company-grab-symbol-cons "\"" 1))
         (`(resource . ,_) (company-grab-symbol))
         (`(data . ,_) (company-grab-symbol))
         (_ (company-grab-symbol))))))

(defun company-terraform--make-candidate (candidate)
  "Annotates a completion suggestion from a name-doc list CANDIDATE."
  (let ((text (nth 0 candidate))
        (doc (nth 1 candidate)))
    (propertize text 'doc doc)))

(defun company-terraform--filterdoc (prefix lists &optional multi)
  "Filters candidates for a PREFIX.
The candidates are provided either as a single list of a list of
LISTS if MULTI is non-nil.  Each candidate is either a single
string of a pair of string and documentation."
  (if (not multi) (setq lists (list lists)))
  (cl-loop
   for l in lists
   append (cl-loop
           for item in l
           if (and (stringp item) (string-prefix-p prefix item))
           collect item
           else if (and (listp item) (string-prefix-p prefix (car item)))
           collect (company-terraform--make-candidate item))))

(defun company-terraform-is-resource-n (string)
  "True iff STRING is an integer or a literal * character."
  (if (string-match "\\`\\([0-9]+\\)\\|*\\'" string) t nil))

(defun company-terraform-candidates (prefix)
  "Prepare a list of autocompletion candidates for the given PREFIX."
  (let ((context (company-terraform-get-context)))
    (pcase context
     ('top-level
      (company-terraform--filterdoc prefix company-terraform-toplevel-keywords))
     (`(object-type resource)
      (company-terraform--filterdoc prefix company-terraform-resources-list))
     (`(object-type data)
      (company-terraform--filterdoc prefix company-terraform-data-list))
     (`(block resource ,type)
      (company-terraform--filterdoc prefix
                                    (list (gethash type company-terraform-resource-arguments-hash)
                                          company-terraform-resource-extra)
                                    t))
     (`(block data ,type)
      (company-terraform--filterdoc prefix
                                    (list (gethash type company-terraform-data-arguments-hash)
                                          company-terraform-data-extra)
                                    t))
     (`(block module ,module-name)
      (company-terraform--filterdoc prefix (company-terraform-get-resource-cache
                                            'variable
                                            (gethash module-name (company-terraform-get-resource-cache 'module-dir)))))
     (`(interpolation ,pathstr)
      ;; Within interpolation
      (pcase (split-string pathstr "\\.")
        (`(,x)
         ;; Complete function name or resource type.
         (company-terraform--filterdoc x
                                       (list company-terraform-interpolation-functions
                                             (hash-table-keys (company-terraform-get-resource-cache 'resource))
                                             company-terraform-interpolation-extra)
                                       t))
        (`("count" ,x)
         ;; Complete count metadata
         (company-terraform--filterdoc x company-terraform-count-extra))
        (`("var" ,x)
         ;; Complete variable name.
         (company-terraform--filterdoc x (company-terraform-get-resource-cache 'variable)))
        (`("local" ,x)
         ;; Complete locals name.
         (company-terraform--filterdoc x (company-terraform-get-resource-cache 'local)))
        (`("module" ,x)
         ;; Complete module name.
         (company-terraform--filterdoc x (company-terraform-get-resource-cache 'module)))
        (`("data" ,x)
         ;; Complete data source type.
         (company-terraform--filterdoc x (hash-table-keys (company-terraform-get-resource-cache 'data))))
        (`("data" ,data-type ,x)
         ;; Complete data name.
         (company-terraform--filterdoc x
                                    (gethash data-type (company-terraform-get-resource-cache 'data))))
        (`("data" ,data-type ,data-name . ,(or `(,x)
                                               `(,(pred company-terraform-is-resource-n) ,x)))
         ;; Complete data arguments/attributes
         (company-terraform--filterdoc x
                                       (list (gethash data-type company-terraform-data-arguments-hash)
                                             (gethash data-type company-terraform-data-attributes-hash))
                                       t))
        (`("module" ,module-name ,x)
         ;; Complete module output
         (company-terraform--filterdoc x
                                       (company-terraform-get-resource-cache
                                        'output
                                        (gethash module-name (company-terraform-get-resource-cache 'module-dir)))
                                       ))
        (`(,resource-type ,x)
         ;; Complete resource name.
         (company-terraform--filterdoc x
                                    (gethash resource-type (company-terraform-get-resource-cache 'resource))))
        (`(,resource-type ,resource-name . ,(or `(,x)
                                                 `(,(pred company-terraform-is-resource-n) ,x)))
         ;; Complete resource arguments/attributes
         (company-terraform--filterdoc x
                                       (list (gethash resource-type company-terraform-resource-arguments-hash)
                                             (gethash resource-type company-terraform-resource-attributes-hash))
                                       t)))))))

(defun company-terraform-doc (candidate)
  "Return the documentation of a completion CANDIDATE."
  (get-text-property 0 'doc candidate))

(defun company-terraform-docbuffer (candidate)
  "Prepare a temporary buffer with completion CANDIDATE documentation."
  (company-doc-buffer (company-terraform-doc candidate)))

;;;###autoload
(defun company-terraform (command &optional arg &rest ignored)
  "Main entry point for a company backend.
Read `company-mode` function docs for the semantics of this function."
  (cl-case command
    (interactive (company-begin-backend 'company-test-backend))
    (prefix      (company-terraform--prefix))
    (candidates  (company-terraform-candidates arg))
    (meta        (company-terraform-doc        arg))
    (doc-buffer  (company-terraform-docbuffer  arg))))


;;;###autoload
(defun company-terraform-init ()
  "Add terraform to the company backends."
  (interactive)
  (add-to-list 'company-backends 'company-terraform))

(provide 'company-terraform)

;;; company-terraform.el ends here
