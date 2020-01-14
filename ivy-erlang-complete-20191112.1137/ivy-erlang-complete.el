;;; ivy-erlang-complete.el --- Erlang completion at point using ivy. -*- lexical-binding: t -*-


;; Copyright (C) 2016 Sergey Kostyaev

;; Author: Sergey Kostyaev <feo.me@ya.ru>
;; Version: 1.0.0
;; Keywords: languages tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `ivy-erlang-complete' is context sensitive erlang completion package with `ivy' as frontend.

;;; Code:
(require 'ivy)
(require 'subr-x)
(require 'cl-lib)
(require 'erlang)
(require 'imenu)
(require 'counsel)
(require 'simple)
(require 'async)
(require 'thingatpt)
(require 'xref)

(defconst ivy-erlang-complete--base (file-name-directory load-file-name))

(defgroup ivy-erlang-complete nil
  "`ivy-erlang-complete' - context sensitive erlang completion and more with `ivy' frontend.")

(defcustom ivy-erlang-complete-erlang-root "/usr/lib/erlang/"
  "Path to erlang root." :type 'string :group 'ivy-erlang-complete)

(defvar ivy-erlang-complete-project-root nil
  "Path to erlang project root.")

(defvar-local ivy-erlang-complete--file-suffix "-U -G '\\.[eh]rl'"
  "Regular expression for erlang files (*.erl *.hrl)")

(defvar-local ivy-erlang-complete-candidates nil
  "Candidates for completion.")

(defvar-local ivy-erlang-complete-predicate nil
  "Completion predicate.")

(defvar-local ivy-erlang-complete-records nil
  "Records accessible in current buffer.")

(defvar-local ivy-erlang-complete-macros nil
  "Macros accessible in current buffer.")

(defvar-local ivy-erlang-complete--record-names nil
  "Record names accessible in current buffer.")

(defvar-local ivy-erlang-complete--local-functions nil
  "Local functions in current buffer.")

(defvar-local ivy-erlang-complete--record nil
  "Current erlang record.")

(defvar-local ivy-erlang-complete--local-vars nil
  "Local variables for current cursor position.")

(defvar ivy-erlang-complete--comment-regexp
  "%.*$")

(defvar-local ivy-erlang-complete--record-parsing-in-progress nil
  "Sync variable for async records parsing.")

(defvar-local ivy-erlang-complete--macros-parsing-in-progress nil
  "Sync variable for async macros parsing.")

(defvar ivy-erlang-complete--behaviours
  (make-hash-table :test 'equal)
  "Memoizaton for behaviours.")

(defvar-local ivy-erlang-complete--eldocs nil
  "Memoizaton for eldocs.")

(defvar ivy-erlang-complete--global-project-root nil
  "Global variable for use with async commands.")

(defcustom ivy-erlang-complete--setup-flycheck t
  "Enable automatic setup flycheck."
  :type 'boolean :group 'ivy-erlang-complete)

(defcustom ivy-erlang-complete-use-default-keys t
  "Enable default keybindings in init."
  :type 'boolean :group 'ivy-erlang-complete)

(defcustom ivy-erlang-complete-enable-eldoc t
  "Enable eldoc."
  :type 'boolean :group 'ivy-erlang-complete)

(defcustom ivy-erlang-complete-enable-autosave t
  "Enable autosave.  If this option is set, buffer will be saved before try to get eldoc asynchronously."
  :type 'boolean :group 'ivy-erlang-complete)

(defcustom ivy-erlang-complete-enable-fun-head-eldoc t
  "Enable function head as eldoc if no specs or callbacks at point."
  :type 'boolean :group 'ivy-erlang-complete)

(defcustom ivy-erlang-complete-enable-notification nil
  "Enable notification on updated completions."
  :type 'boolean :group 'ivy-erlang-complete)

(defcustom ivy-erlang-complete-ignore-dirs
  '(".git" "_build")
  "Exclude directory patterns from this list while search definition/references."
  :type 'list :group 'ivy-erlang-complete)

(defun ivy-erlang-complete--notify ()
  "Notify about updated completions if enabled."
  (if ivy-erlang-complete-enable-notification
      (message "Erlang completions updated")))

(defun ivy-erlang-complete--executable (name)
  "Return path to executable with NAME."
  (concat ivy-erlang-complete--base "bin/" name))

(when (featurep 'flycheck)
  (defvar flycheck-erlang-include-path)
  (defvar flycheck-erlang-library-path)
  (defun ivy-erlang-complete-setup-flycheck (project-root)
    "Setup flycheck for use correct paths for erlang PROJECT-ROOT with deps."
    (setq-local flycheck-erlang-include-path
                (append
                 (split-string
                  (shell-command-to-string
                   (concat "find "
                           project-root
                           "/* "
                           (ivy-erlang-complete--filter-find)
                           " -type d -name include -print")))
                 (list project-root
                       (concat project-root "/include")
                       (concat project-root "/deps")
                       default-directory
                       (concat
                        (locate-dominating-file
                         default-directory
                         "src") "include")
                       (concat
                        (locate-dominating-file
                         default-directory
                         "src") "deps"))))
    (let ((code-path
           (split-string (shell-command-to-string
                          (concat "find " project-root " " (ivy-erlang-complete--filter-find)
                                  " -type d -name ebin -print")))))
      (setq-local flycheck-erlang-library-path code-path))))

;;;###autoload
(defun ivy-erlang-complete-autosetup-project-root ()
  "Automatically setup erlang project root."
  (interactive)
  (if ivy-erlang-complete-project-root
      ivy-erlang-complete-project-root
    (setq-local ivy-erlang-complete-project-root
                (ivy-erlang-complete--find-root-by-deps))
    (if (and ivy-erlang-complete--setup-flycheck (featurep 'flycheck))
        (ivy-erlang-complete-setup-flycheck
         ivy-erlang-complete-project-root))
    ivy-erlang-complete-project-root))

(defun ivy-erlang-complete--find-root-by-deps (&optional start-dir)
  "Find project root as directory with rebar dependencies start from START-DIR."
  (let ((dir (locate-dominating-file (or (expand-file-name ".." start-dir)
                                         default-directory)
                                     "rebar.config")))
    (if dir
        (let ((test-dir (expand-file-name
                         (shell-command-to-string
                          (format "cd %s && %s" dir
                                  (ivy-erlang-complete--executable
                                   "find-deps-dir"))) dir)))
          (or (if (and (file-exists-p test-dir)
                       (directory-files test-dir))
                  (expand-file-name dir)
                nil)
              (ivy-erlang-complete--find-root-by-deps dir)))
      (expand-file-name (or start-dir default-directory)))))

;;;###autoload
(defun ivy-erlang-complete-init ()
  "Config ivy-erlang-complete by default."
  (interactive)
  (ivy-erlang-complete-autosetup-project-root)
  (setq-local ivy-erlang-complete--eldocs
              (make-hash-table :test 'equal))
  (ivy-erlang-complete-reparse)
  (if ivy-erlang-complete-use-default-keys
      (progn
        (define-key erlang-mode-map (kbd "C-:")
          'ivy-erlang-complete)
        (define-key erlang-mode-map (kbd "C-c C-h")
          'ivy-erlang-complete-show-doc-at-point)
        (define-key erlang-mode-map (kbd "C-c C-e")
          #'ivy-erlang-complete-set-project-root)
        (define-key erlang-mode-map (kbd "M-.")
          #'ivy-erlang-complete-find-definition)
        (define-key erlang-mode-map (kbd "M-?")
          #'ivy-erlang-complete-find-references)
        (define-key erlang-mode-map (kbd "C-c C-f")
          #'ivy-erlang-complete-find-spec)
        (define-key erlang-mode-map (kbd "M-,")
          #'xref-pop-marker-stack)
        (define-key erlang-mode-map (kbd "C-c C-o")
          #'ivy-erlang-complete-find-file)))
  (if ivy-erlang-complete-enable-eldoc
      (set (make-local-variable 'eldoc-documentation-function)
           'ivy-erlang-complete-eldoc)))

;;;###autoload
(defun ivy-erlang-complete-show-doc-at-point ()
  "Show doc for function from standart library."
  (interactive)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (if (not (string-match-p ":" thing))
        (message "Can't find docs for %s." thing)
      (let* ((erl-thing (split-string thing ":"))
             (module (car erl-thing))
             (function (car (cdr erl-thing)))
             (arity (erlang-get-function-arity))
             (candidates
              (cl-mapcar (lambda (s) (concat module ":" s))
                         (ivy-erlang-complete--find-functions module))))
        (if (not module)
            (message "module at point not found")
          (ivy-read "Counsel-erl cand:" candidates
                    :require-match t
                    :initial-input (if (not arity) thing
                                     (concat module ":" function "/"
                                             (format "%d" arity)))
                    :action (lambda (s)
                              (let ((result (split-string s "[:/]")))
                                (browse-url
                                 (format
                                  "http://erlang.org/doc/man/%s.html#%s-%s"
                                  (nth 0 result)
                                  (nth 1 result)
                                  (nth 2 result)))))))))))

(defvar ivy-erlang-complete--export-regexp
  "-export([[:space:]\n]*\\[[\n[:space:]]*[a-z/0-9\n[:space:]\,_]*][\n[:space:]]*).")

(defvar-local ivy-erlang-complete--last-erlang-comment nil)
(defun ivy-erlang-complete--copy-buffer-no-comments ()
  "Copy current buffer to new one with removing comments."
  (let* ((old-buf (buffer-name))
         (new-buf (concat "*" old-buf "*"))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (pos (point)))
    (if (buffer-live-p new-buf)
        (kill-buffer new-buf))
    (with-current-buffer (get-buffer-create new-buf)
      (insert content)
      (goto-char (point-min))
      (while (search-forward-regexp
              ivy-erlang-complete--comment-regexp
              ivy-erlang-complete--last-erlang-comment t)
        (replace-match (make-string (length (match-string 0)) ?\ )))
      (goto-char pos))
    new-buf))

(defun ivy-erlang-complete--find-functions (module)
  "Find functions in MODULE."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (split-string
   (shell-command-to-string
    (string-join
     (list
      (ivy-erlang-complete--executable "exported-funcs.sh")
      module
      ivy-erlang-complete-project-root ivy-erlang-complete-erlang-root)
     " "))
   "\n"))

(defun ivy-erlang-complete--exported-types (module)
  "Find functions in MODULE."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (split-string
   (shell-command-to-string
    (string-join
     (list
      (ivy-erlang-complete--executable "exported-types.sh")
      module
      ivy-erlang-complete-project-root ivy-erlang-complete-erlang-root)
     " "))
   "\n"))

(defun ivy-erlang-complete--find-modules ()
  "Find modules."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (cl-mapcar (lambda (s) (concat s ":"))
             (split-string
              (shell-command-to-string
               (string-join
                (list
                 "find" ivy-erlang-complete-project-root
                 ivy-erlang-complete-erlang-root
                 (ivy-erlang-complete--filter-find)
                 "-iname '*.erl' -print | xargs basename -a |"
                 "sed -E -e 's/\\.erl//g'") " "))
              "\n")))

(defun ivy-erlang-complete--extract-records (file)
  "Extract all records from FILE."
  (if (not ivy-erlang-complete-project-root)
      (ivy-erlang-complete-set-project-root))
  (cl-mapcar (lambda (s) (concat s ")."))
             (split-string
              (shell-command-to-string
               (string-join
                (list "find" ivy-erlang-complete-project-root " "
                      (ivy-erlang-complete--filter-find) " -name" file "-print" "|"
                      "xargs" "sed" "-E" "-n" "'/-record\\(/,/}\\)./p'") " "))
              ")\\.")))

(defun ivy-erlang-complete--parse-record (record)
  "Parse RECORD and set it acessable in current buffer."
  (let ((res (read (shell-command-to-string
                    (concat
                     (ivy-erlang-complete--executable "parse-record")
                     " \"\"\"" record "\"\"\"")))))
    (ignore-errors (puthash (car res)
                            (cdr res) ivy-erlang-complete-records))))
(defun ivy-erlang-complete--find-local-functions ()
  "Find all local functions."
  (if (not ivy-erlang-complete--local-functions)
      (setq ivy-erlang-complete--local-functions
            (progn
              (let ((pos (point)))
                (ignore-errors (imenu--make-index-alist))
                (goto-char pos))
              (cl-mapcar (lambda (elem) (car elem)) imenu--index-alist))))
  ivy-erlang-complete--local-functions)

(defun ivy-erlang-complete--find-local-vars ()
  "Find local variables at point."
  (let* ((pos (point))
         (pos2 (progn (backward-word) (point)))
         (function-begin (search-backward-regexp "^[a-z]" nil t))
         (search-string (if function-begin
                            (buffer-substring-no-properties function-begin pos2)
                          "")))
    (goto-char pos)
    (cl-remove-if (lambda (s)
                    (member (concat "?" s) ivy-erlang-complete-macros))
                  (with-temp-buffer
                    (insert search-string)
                    (goto-char (point-min))
                    (setq case-fold-search nil)
                    (setq-local ivy-erlang-complete--local-vars '())
                    (while
                        (search-forward-regexp "[A-Z][A-Za-z_0-9]*" nil t)
                      (add-to-list 'ivy-erlang-complete--local-vars
                                   (match-string 0)))
                    ivy-erlang-complete--local-vars))))

(defun ivy-erlang-complete-thing-at-point ()
  "Return the erlang thing at point, or nil if none is found."
  (when (thing-at-point-looking-at "\??#?['A-Za-z0-9_:]*")
    (match-string-no-properties 0)))

(defun ivy-erlang-complete-record-at-point ()
  "Return the erlang record at point, or nil if none is found."
  (when (thing-at-point-looking-at
         (format "#\\(%s\\)\\([\s-]*{[^{^}^#]*}?\\)*[.]*" erlang-atom-regexp)
         500)
    (match-string-no-properties 0)))

(defun ivy-erlang-complete-export-at-point ()
  "Return the erlang export at point, or nil if none is found."
  (with-current-buffer (get-buffer-create
                        (ivy-erlang-complete--copy-buffer-no-comments))
    (when (thing-at-point-looking-at
           ivy-erlang-complete--export-regexp
           500)
      (let ((result (match-string-no-properties 0)))
        (kill-buffer)
        result))))

(defun ivy-erlang-complete--is-type-at-point ()
  "Return t if type at point."
  (save-excursion
    (let ((starting-point (point))
          (case-fold-search nil)
          (state nil))
      (erlang-beginning-of-clause)
      (while (< (point) starting-point)
        (setq state (erlang-partial-parse (point) starting-point state)))
      (string-equal "::" (cl-caaar state)))))

(defun ivy-erlang-complete--is-guard-at-point ()
  "Return t if guard at point."
  (save-excursion
    (let ((starting-point (point))
          (case-fold-search nil)
          (state nil))
      (erlang-beginning-of-clause)
      (while (< (point) starting-point)
        (setq state (erlang-partial-parse (point) starting-point state)))
      (string-equal "when" (cl-caaar state)))))

(defun ivy-erlang-complete--get-included-files ()
  "Get included files for current buffer."
  (cl-mapcar (lambda (m) (file-name-nondirectory m))
             (let ((regexp "-include[_lib]*([:space:]*\"\\([^\"]+\\)")
                   (string (buffer-substring-no-properties 1 (point-max))))
               (save-match-data
                 (let ((pos 0)
                       matches)
                   (while (string-match regexp string pos)
                     (push (match-string 1 string) matches)
                     (setq pos (match-end 0)))
                   matches)))))

;;;###autoload
(defun ivy-erlang-complete-reparse ()
  "Reparse macros and recors for completion in current buffer."
  (interactive)
  (if (string-equal major-mode "erlang-mode")
      (progn
        (setq ivy-erlang-complete--local-functions nil)
        (ivy-erlang-complete--find-local-functions)
        (ivy-erlang-complete--async-parse-macros)
        (setq-local ivy-erlang-complete--behaviours
                    (make-hash-table :test 'equal))
        (ivy-erlang-complete--async-parse-records))))

(defun ivy-erlang-complete--parse-records ()
  "Parse erlang records in FILE."
  (setq-local ivy-erlang-complete-records (make-hash-table :test 'equal))
  (cl-mapcar
   'ivy-erlang-complete--parse-record
   (ivy-erlang-complete--flatten
    (cl-mapcar 'ivy-erlang-complete--extract-records
               (append (ivy-erlang-complete--get-included-files)
                       (list
                        (file-name-nondirectory (buffer-file-name)))))))
  ivy-erlang-complete-records)

(defun ivy-erlang-complete--flatten (x)
  "Flatten tree X into a single list."
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
    (rec x nil)))

(defun ivy-erlang-complete--async-parse-records ()
  "Async parse erlang records for current buffer."
  (if (not ivy-erlang-complete--record-parsing-in-progress)
      (progn
        (setq ivy-erlang-complete--record-parsing-in-progress t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "load-path")
            ,(async-inject-variables "ivy-erlang-complete-project-root")
            ,(async-inject-variables "ivy-erlang-complete-erlang-root")
            (require 'ivy-erlang-complete)
            (find-file ,(buffer-file-name))
            (setq ivy-erlang-complete-project-root
                  ,ivy-erlang-complete-project-root)
            (setq eval-expression-print-length nil)
            (setq print-length nil)
            (prin1-to-string (ivy-erlang-complete--parse-records))
            )
         `(lambda (res)
            (with-current-buffer ,(buffer-name)
              (setq ivy-erlang-complete-records (read res))
              (setq ivy-erlang-complete--record-parsing-in-progress nil))
            (ivy-erlang-complete--notify))))))

(defun ivy-erlang-complete--get-record-names ()
  "Return list of acceptable record names."
  (if (not ivy-erlang-complete-records)
      (progn
        (ivy-erlang-complete-reparse)
        (message "Please wait for record parsing")
        nil)
    (setq ivy-erlang-complete--record-names nil)
    (maphash (lambda (key _)
               (push (concat "#" key) ivy-erlang-complete--record-names))
             ivy-erlang-complete-records)
    ivy-erlang-complete--record-names))

(defun ivy-erlang-complete--get-record-fields (record)
  "Return list of RECORD fields."
  (if (not ivy-erlang-complete-records)
      (progn
        (ivy-erlang-complete-reparse)
        (message "Please wait for record parsing")
        nil)
    (cl-mapcar
     (lambda (s)
       (concat
        (car s)
        (if (cdr s)
            (let ((type
                   (concat "\t\t:: " (string-join (ivy-erlang-complete--flatten
                                                   (cdr s))  " | "))))
              (set-text-properties 0 (length type) '(face success) type)
              type))))
     (gethash record ivy-erlang-complete-records))))

(defun ivy-erlang-complete--extract-macros (file)
  "Extract erlang macros from FILE."
  (cl-delete-duplicates
   (cl-mapcar
    (lambda (s)
      (concat
       "?"
       (car
        (split-string
         (car (split-string (string-remove-prefix "-define(" s) ","))
         "("))))
    (split-string
     (string-trim
      (shell-command-to-string
       (string-join
        (list
         "find" ivy-erlang-complete-project-root
         (ivy-erlang-complete--filter-find) "-name" file
         " -print | xargs grep -h -e '^-define('") " ")))
     "\n"))))

(defun ivy-erlang-complete--get-macros ()
  "Return list of acceptable erlang macros."
  (if (not ivy-erlang-complete-macros)
      (setq ivy-erlang-complete-macros
            (cl-delete-duplicates
             (ivy-erlang-complete--flatten
              (append
               (list "?MODULE" "?MODULE_STRING" "?FILE" "?LINE" "?MACHINE")
               (cl-mapcar
                'ivy-erlang-complete--extract-macros
                (append
                 (ivy-erlang-complete--get-included-files)
                 (list (concat (file-name-base (buffer-file-name))
                               "."
                               (file-name-extension (buffer-file-name)))))))))))
  ivy-erlang-complete-macros)

(defun ivy-erlang-complete--async-parse-macros ()
  "Async parse erlang macros for current buffer."
  (if (not ivy-erlang-complete--macros-parsing-in-progress)
      (progn
        (setq ivy-erlang-complete--macros-parsing-in-progress t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "load-path")
            ,(async-inject-variables "ivy-erlang-complete-project-root")
            ,(async-inject-variables "ivy-erlang-complete-erlang-root")
            (require 'ivy-erlang-complete)
            (find-file ,(buffer-file-name))
            (setq ivy-erlang-complete-project-root
                  ,ivy-erlang-complete-project-root)
            (setq eval-expression-print-length nil)
            (setq print-length nil)
            (prin1-to-string (ivy-erlang-complete--get-macros))
            )
         `(lambda (res)
            (with-current-buffer ,(buffer-name)
              (setq ivy-erlang-complete-macros (read res))
              (setq ivy-erlang-complete--macros-parsing-in-progress nil))
            (ivy-erlang-complete--notify))))))


;;;###autoload
(defun ivy-erlang-complete-set-project-root ()
  "Set root for current project."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq ivy-erlang-complete-project-root dir)
    dir))

(defun ivy-erlang-complete--insert-candidate (candidate)
  "Insert CANDIDATE at point."
  (cond
   ((string-match "\\(.*\\)\t\t\\(::.*\\)" candidate)
    (let ((type (match-string-no-properties 2 candidate)))
      (ivy-erlang-complete--insert-candidate
       (match-string-no-properties 1 candidate))
      (message type)))
   ((ivy-erlang-complete-export-at-point)
    (ivy-completion-in-region-action candidate))
   ((and (string-prefix-p "?" candidate)
         (thing-at-point-looking-at "\?['A-Za-z0-9_:]+"))
    (ivy-completion-in-region-action (string-remove-prefix "?" candidate)))
   ((string-match "\\([^/]+\\)/\\([0-9]+\\)" candidate)
    (let ((arity (string-to-number
                  (substring candidate
                             (match-beginning 2) (match-end 2)))))
      (ivy-completion-in-region-action
       (concat (replace-regexp-in-string "/[0-9]+" "" candidate)
               "("
               (make-string (if (= 0 arity) arity (- arity 1)) ?,)
               ")"))
      (goto-char (- (point) arity))))
   ((and (string-match ".*()$" candidate)
         (ivy-erlang-complete--is-guard-at-point))
    (ivy-completion-in-region-action candidate)
    (goto-char (- (point) 1)))
   (t (ivy-completion-in-region-action candidate))))

(defun ivy-erlang-complete--get-export ()
  "Return formated exported functions with arity."
  (cl-mapcar (lambda (x)
               (format "%s/%d" (car x) (cdr x)))
             (erlang-get-export)))

;;;###autoload
(defun ivy-erlang-complete ()
  "Erlang completion at point."
  (interactive)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (cond
     ((and thing (string-match "#?\\([^\:]+\\)\:\\([^\:]*\\)" thing))
      (let ((erl-prefix (substring thing (match-beginning 1) (match-end 1))))
        (setq ivy-erlang-complete-candidates
              (if (ivy-erlang-complete--is-type-at-point)
                  (ivy-erlang-complete--exported-types erl-prefix)
                (ivy-erlang-complete--find-functions erl-prefix)))
        (setq ivy-erlang-complete-predicate
              (string-remove-prefix (concat erl-prefix ":") thing))))
     ((ivy-erlang-complete-export-at-point)
      (setq ivy-erlang-complete--local-functions nil)
      (setq ivy-erlang-complete-candidates
            (cl-remove-if
             (lambda (el)
               (member el (ivy-erlang-complete--get-export)))
             (ivy-erlang-complete--find-local-functions))))
     ((ivy-erlang-complete--is-guard-at-point)
      (setq ivy-erlang-complete-candidates
            (append (cl-mapcar (lambda (g) (format "%s()" g))
                               erlang-guards)
                    erlang-operators)))
     ((ivy-erlang-complete-record-at-point)
      (setq ivy-erlang-complete-candidates
            (append
             (ivy-erlang-complete--get-record-fields
              (buffer-substring-no-properties
               (match-beginning 1) (match-end 1)))
             (ivy-erlang-complete--find-local-vars)
             (ivy-erlang-complete--find-local-functions)
             (ivy-erlang-complete--get-record-names)
             (ivy-erlang-complete--find-modules)
             (ivy-erlang-complete--get-macros))))
     ((ivy-erlang-complete--is-type-at-point)
      (setq ivy-erlang-complete-candidates
            (append
             (cl-mapcar (lambda (s) (concat s "()")) erlang-predefined-types)
             (ivy-erlang-complete--get-defined-types)
             (ivy-erlang-complete--find-modules))))
     (t
      (setq ivy-erlang-complete-candidates
            (append
             (ivy-erlang-complete--find-local-vars)
             (ivy-erlang-complete--find-local-functions)
             (ivy-erlang-complete--get-record-names)
             (ivy-erlang-complete--find-modules)
             (ivy-erlang-complete--get-macros)))))
    (setq ivy-erlang-complete-predicate (string-remove-prefix
                                         "?"
                                         (if (string-match-p ":" thing)
                                             ivy-erlang-complete-predicate
                                           thing))))
  (when (looking-back ivy-erlang-complete-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "erlang cand:" ivy-erlang-complete-candidates
            :initial-input ivy-erlang-complete-predicate
            :action #'ivy-erlang-complete--insert-candidate))

;;;###autoload
(defun ivy-erlang-complete--find-definition (thing)
  "Search THING definition in DIRECTORY-PATH."
  (cond
   ((string-match-p ":" thing)
    (let* ((thing2 (split-string thing ":"))
           (module (car thing2))
           (func (car (cdr thing2))))
      (ivy-erlang-complete--find-def (concat module ".erl")
                                     (concat "^" func"("))))
   ((string-prefix-p "?" thing)
    (ivy-erlang-complete--find-def
     (append
      (ivy-erlang-complete--get-included-files)
      (list (file-name-nondirectory (buffer-file-name))))
     (concat "^-define(" (string-remove-prefix "?" thing))))
   ((setq ivy-erlang-complete--record (ivy-erlang-complete-record-at-point))
    (ivy-erlang-complete--find-def
     (append
      (ivy-erlang-complete--get-included-files)
      (list (file-name-nondirectory (buffer-file-name))))
     (concat "^-record(" (match-string-no-properties 1) ",")))
   ((thing-at-point-looking-at "-behaviour(\\([a-z_]+\\)).")
    (ivy-erlang-complete--find-def
     ".erl$"
     (concat "^-module(" (match-string-no-properties 1) ").")))
   ((ivy-erlang-complete--is-type-at-point)
    (if (member thing erlang-predefined-types)
        (message "predefined type")
      (ivy-erlang-complete--find-def
       (append
        (ivy-erlang-complete--get-included-files)
        (list (file-name-nondirectory (buffer-file-name))))
       (concat "^-type " thing "("))))
   ((string-match-p "^[a-z]" thing)
    (ivy-erlang-complete--find-def
     (file-name-nondirectory (buffer-file-name))
     (concat "^" thing"(")))
   (t  (message "Can't find definition"))))

(defun ivy-erlang-complete--prepare-find-args (args)
  "Prepare ARGS for `ivy-erlang-complete--find-grep-spec-function'."
  (if (not (listp args)) (ivy-erlang-complete--prepare-find-args (list args))
    (string-join
     (cl-mapcar (lambda (s) (format "-name '%s\\.erl'" s))
                (ivy-erlang-complete--flatten args)) " -o ")))

(defun ivy-erlang-complete--prepare-def-find-args (args)
  "Prepare ARGS for `ivy-erlang-complete--find-grep-spec-function'."
  (if (not (listp args)) (ivy-erlang-complete--prepare-def-find-args
                          (list args))
    (string-join
     (cl-mapcar (lambda (s) (format "-name '%s'" s))
                (ivy-erlang-complete--flatten args)) " -print -o ")))

(defun ivy-erlang-complete--find-grep-spec-function (string extra-args)
  "Grep in the project directory for STRING.
If non-nil, EXTRA-ARGS string is appended to command."
  (when (null extra-args)
    (setq-local extra-args "*"))
  (if (not (listp extra-args))
      (setq extra-args (list extra-args)))
  (if (< (length string) 3)
      (counsel-more-chars)
    (let ((qregex (shell-quote-argument
                   (counsel--elisp-to-pcre
                    (setq ivy--old-re
                          (ivy--regex string))))))
      (let
          ((cmd
            (if (ivy-erlang-complete-rg-installed?)
                (format
                 "rg -i --no-heading --line-number --max-columns 150 -uu -t erlang %s %s -e '^-callback %s\\(' -e '^-spec %s\\(' %s %s"
                 (string-join
                  (cl-mapcar
                   (lambda (s) (format "-g '!%s'" s))
                   ivy-erlang-complete-ignore-dirs)
                  " ")
                 (string-join
                  (cl-mapcar
                   (lambda (s) (format "-g '%s.erl'" s))
                   extra-args)
                  " ")
                 qregex
                 qregex
                 ivy-erlang-complete-erlang-root
                 ivy-erlang-complete--global-project-root)
              (format
               "find %s %s %s %s -print | xargs grep -H -n -e '^-callback %s(' -e '^-spec %s('"
               ivy-erlang-complete-erlang-root
               ivy-erlang-complete--global-project-root
               (ivy-erlang-complete--filter-find)
               (ivy-erlang-complete--prepare-find-args extra-args)
               qregex qregex))))
        (counsel--async-command cmd))
      nil)))

(ivy-set-display-transformer
 'ivy-erlang-complete--find-grep-spec-function 'counsel-git-grep-transformer)
(counsel-set-async-exit-code
 'ivy-erlang-complete--find-grep-spec-function 123 "No matches found")
(counsel-set-async-exit-code
 'ivy-erlang-complete--find-grep-spec-function 1 "No matches found")
(ivy-set-occur 'ivy-erlang-complete--find-grep-spec-function 'counsel-ag-occur)

(defun ivy-erlang-complete--filter-find ()
  "Exclude ignored dirs from find command."
  (string-join
   (cl-mapcar (lambda (dir)
                (format "-path '*/%s' -prune -o" dir))
              ivy-erlang-complete-ignore-dirs)
   " "))

(defun ivy-erlang-complete--find-grep-def-function (string files)
  "Grep in the project directory for STRING in FILES."
  (when (null files)
    (setq-local files "*"))
  (if (not (listp files))
      (setq files (list files)))
  (if (< (length string) 3)
      (counsel-more-chars)
    (let ((cmd
           (if (ivy-erlang-complete-rg-installed?)
               (format
                "rg -i --no-heading --line-number --max-columns 150 -uu -t erlang %s %s -e '%s' -e '-type %s' %s %s"
                (string-join
                 (cl-mapcar
                  (lambda (s) (format "-g '!%s'" s))
                  ivy-erlang-complete-ignore-dirs)
                 " ")
                (string-join
                 (cl-mapcar
                  (lambda (s) (format "-g '%s'" s))
                  files)
                 " ")
                (replace-regexp-in-string "(" "\\\\(" string)
                (replace-regexp-in-string "(" "\\\\(" (string-remove-prefix "^" string))
                ivy-erlang-complete-erlang-root
                ivy-erlang-complete--global-project-root)
             (format
              "find %s %s %s %s -print | xargs grep -H -n -e \"\"\"%s\"\"\" -e \"\"\"-type %s\"\"\""
              ivy-erlang-complete-erlang-root
              ivy-erlang-complete--global-project-root
              (ivy-erlang-complete--filter-find)
              (ivy-erlang-complete--prepare-def-find-args files)
              string (string-remove-prefix "^" string)))))
      (counsel--async-command cmd))
    nil))

(defun ivy-erlang-complete--get-defined-types ()
  "Return types that declared in current file and included libraries."
  (ivy-erlang-complete--find-grep-types-function
   (append (list (file-name-nondirectory (buffer-file-name)))
           (ivy-erlang-complete--get-included-files))))

(defun ivy-erlang-complete--find-grep-types-function (files)
  "Grep in the project directory for types defined in FILES."
  (let
      ((cmd
        (format
         "find %s %s %s %s -print | xargs grep -H -n -e '-type' | sed -E 's/::*//' | awk '{print $2}'"
         ivy-erlang-complete-erlang-root
         ivy-erlang-complete-project-root
         (ivy-erlang-complete--filter-find)
         (ivy-erlang-complete--prepare-def-find-args files))))
    (split-string (shell-command-to-string cmd) "\n")))

(ivy-set-display-transformer
 'ivy-erlang-complete--find-grep-def-function 'counsel-git-grep-transformer)
(counsel-set-async-exit-code
 'ivy-erlang-complete--find-grep-def-function 123 "No matches found")
(counsel-set-async-exit-code
 'ivy-erlang-complete--find-grep-def-function 1 "No matches found")
(ivy-set-occur 'ivy-erlang-complete--find-def-spec-function 'counsel-ag-occur)

(defun ivy-erlang-complete--find-def (filename regex)
  "Find definition in FILENAME by REGEX."
  (setq ivy-erlang-complete--global-project-root
        ivy-erlang-complete-project-root)
  (ivy-read "find definition  "
            (lambda (string)
              (ivy-erlang-complete--find-grep-def-function string filename))
            :initial-input regex
            :dynamic-collection t
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'ivy-erlang-complete--find-grep-def-function))

(defun ivy-erlang-complete--extract-behaviours (file)
  "Return FILE behaviours."
  (or (gethash file ivy-erlang-complete--behaviours nil)
      (puthash file (cl-mapcar
                     (lambda (s)
                       (string-match "-behaviour(\\([^\.]+\\))\." s)
                       (match-string 1 s))
                     (split-string
                      (shell-command-to-string
                       (format "grep -e '^-behaviour(' %s" file)) "\n"
                      t))
               ivy-erlang-complete--behaviours)))

(defun ivy-erlang-complete--find-spec (thing)
  "Search spec for THING."
  (setq ivy-erlang-complete--global-project-root
        ivy-erlang-complete-project-root)
  (if (string-match-p ":" thing)
      (let* ((splitted-thing (split-string thing ":"))
             (module (car splitted-thing))
             (func (cadr splitted-thing)))
        (ivy-read "find spec  "
                  (lambda (string)
                    (ivy-erlang-complete--find-grep-spec-function string
                                                                  module))
                  :initial-input func
                  :dynamic-collection t
                  :action #'counsel-git-grep-action
                  :unwind (lambda ()
                            (counsel-delete-process)
                            (swiper--cleanup))
                  :caller 'ivy-erlang-complete--find-grep-spec-function))
    (let ((file (buffer-file-name)))
      (ivy-read "find spec  "
                (lambda (string)
                  (ivy-erlang-complete--find-grep-spec-function
                   string
                   (append (ivy-erlang-complete--extract-behaviours
                            file)
                           (list (file-name-base file)))))
                :initial-input thing
                :dynamic-collection t
                :action #'counsel-git-grep-action
                :unwind (lambda ()
                          (counsel-delete-process)
                          (swiper--cleanup))
                :caller 'ivy-erlang-complete--find-grep-spec-function))))

;;;###autoload
(defun ivy-erlang-complete-find-spec ()
  "Find spec at point.  It also find callback definition."
  (interactive)
  (xref-push-marker-stack)
  (ivy-erlang-complete--find-spec
   (ivy-erlang-complete-thing-at-point)))

;;;###autoload
(defun ivy-erlang-complete-find-definition ()
  "Find erlang definition."
  (interactive)
  (xref-push-marker-stack)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (ivy-erlang-complete--find-definition thing)))

(defvar ivy-erlang-complete--rg-installed? 'unset)
(defun ivy-erlang-complete-rg-installed? ()
  "Return t if rg is installed."
  (if (eq ivy-erlang-complete--rg-installed? 'unset)
      (setq ivy-erlang-complete--rg-installed?
            (not (null (string-match-p "ripgrep"
                                       (shell-command-to-string
                                        "rg --version")))))
    ivy-erlang-complete--rg-installed?))

(defun ivy-erlang-complete--find-references (thing)
  "Find THING references in project directory."
  (if (ivy-erlang-complete-rg-installed?)
      (counsel-rg
       thing
       ivy-erlang-complete-project-root
       (format "-t erlang -uu %s"
               (string-join
                (cl-mapcar
                 (lambda (s) (format "-g '!%s'" s))
                 ivy-erlang-complete-ignore-dirs)
                " "))
       "find references")
    (counsel-ag
     thing
     ivy-erlang-complete-project-root
     (format "%s %s" ivy-erlang-complete--file-suffix
             (concat "--ignore "
                     (string-join ivy-erlang-complete-ignore-dirs " --ignore ")))
     "find references")))

;;;###autoload
(defun ivy-erlang-complete-find-references ()
  "Find erlang references."
  (interactive)
  (xref-push-marker-stack)
  (let ((thing (ivy-erlang-complete-thing-at-point)))
    (cond
     ((string-match-p ":" thing)
      (ivy-erlang-complete--find-references (concat thing "(")))
     ((string-prefix-p "?" thing)
      (ivy-erlang-complete--find-references (replace-regexp-in-string "?" "\\\?" thing)))
     ((thing-at-point-looking-at "-record(\\(['A-Za-z0-9_:.]+\\),")
      (ivy-erlang-complete--find-references (concat "#" (match-string-no-properties 1))))
     ((thing-at-point-looking-at "-define(\\(['A-Za-z0-9_:.]+\\),")
      (ivy-erlang-complete--find-references (concat "\\\?" (match-string-no-properties 1))))
     ((ivy-erlang-complete-record-at-point)
      (ivy-erlang-complete--find-references (concat "#" (match-string-no-properties 1))))
     ((thing-at-point-looking-at "-behaviour(\\([a-z_]+\\)).")
      (ivy-erlang-complete--find-references (concat "^" (match-string-no-properties 0))))
     ((let* ((pos (point))
             (is-function
              (progn
                (goto-char (line-beginning-position))
                (or (search-forward-regexp
                     (concat "^" thing "(")
                     (point-max) t 1)
                    (search-backward-regexp
                     (concat "^" thing "(")
                     (point-min) t 1)))))
        (goto-char pos)
        is-function)
      (ivy-erlang-complete--find-references (concat (file-name-base (buffer-file-name))
                                                    ":" thing "(")))
     ((cl-find-if (lambda (x) (string-match-p (format "%s/[0-9]+" thing) x))
                  (ivy-erlang-complete--exported-types
                   (file-name-base (buffer-file-name))))
      (ivy-erlang-complete--find-references (concat (file-name-base (buffer-file-name)) ":" thing "(")))
     ((and
       (string-equal "hrl" (file-name-extension (buffer-file-name)))
       (thing-at-point-looking-at (format "^-type[ \t\n]+\\(%s\\)"
                                          erlang-atom-regexp)))
      (ivy-erlang-complete--find-references (concat (match-string-no-properties 1) "(")))
     (t (ivy-erlang-complete--find-references thing)))))

;;;###autoload
(defun ivy-erlang-complete-find-file ()
  "Find file in current project."
  (interactive)
  (xref-push-marker-stack)
  (counsel-file-jump (format "%s"
                             (or
                              (and
                               (thing-at-point-looking-at "\"\\([^\n]+\\)\"")
                               (match-string-no-properties 1))
                              (symbol-at-point)
                              ""))
                     ivy-erlang-complete-project-root))

(defun ivy-erlang-complete-eldoc ()
  "Function for async support eldoc."
  (let* ((pos (point))
         (mod-fun (progn
                    (ignore-errors (backward-up-list))
                    (ivy-erlang-complete-thing-at-point)))
         (arity (progn
                  (forward-char)
                  (erlang-get-arity)))
         (key (format "%s/%s" mod-fun arity))
         (eldoc (gethash key ivy-erlang-complete--eldocs)))
    (goto-char pos)
    (cond
     ((not arity)
      nil)
     ((string-equal eldoc "in-progress")
      nil)
     ((eq eldoc nil)
      (puthash key "in-progress" ivy-erlang-complete--eldocs)
      (if ivy-erlang-complete-enable-autosave (save-buffer))
      (async-start
       `(lambda ()
          ,(async-inject-variables "load-path")
          ,(async-inject-variables "ivy-erlang-complete-project-root")
          ,(async-inject-variables "ivy-erlang-complete-erlang-root")
          (require 'ivy-erlang-complete)
          (find-file ,(buffer-file-name))
          (setq eval-expression-print-length nil)
          (setq print-length nil)
          (goto-char ,(point))
          (ivy-erlang-complete--eldoc))
       `(lambda (res)
          (with-current-buffer ,(buffer-name)
            (puthash ,key res ivy-erlang-complete--eldocs))))
      nil)
     (t (ivy-erlang-complete--highlight-nth-arg (ivy-erlang-complete--cur-arg-num) eldoc)))))

;; TODO: add eldoc support for macros
(defun ivy-erlang-complete--eldoc ()
  "Function for support eldoc."
  (let* ((pos (point))
         (mod-fun (progn
                    (ignore-errors (backward-up-list))
                    (ivy-erlang-complete-thing-at-point)))
         (arity (progn
                  (forward-char)
                  (erlang-get-arity)))
         (info (cond ((string-match "#?\\([^\:]+\\)\:\\([^\:]*\\)" mod-fun)
                      (let ((mod (match-string-no-properties 1 mod-fun))
                            (fun (match-string-no-properties 2 mod-fun)))
                        (or (ivy-erlang-complete--match-by-arity
                             (cl-mapcar (lambda (s) (concat (string-trim s) "."))
                                        (split-string
                                         (shell-command-to-string
                                          (format "find %s %s %s -name '%s.erl' -print | xargs awk '/^-spec %s\\(/,/\\.$/'"
                                                  ivy-erlang-complete-project-root
                                                  ivy-erlang-complete-erlang-root
                                                  (ivy-erlang-complete--filter-find)
                                                  mod fun)) "\\\.$"))
                             arity)
                            (when ivy-erlang-complete-enable-fun-head-eldoc
                              (ivy-erlang-complete--match-by-arity
                               (cl-mapcar (lambda (s) (string-trim s))
                                          (split-string
                                           (shell-command-to-string
                                            (format "find %s %s %s -name '%s.erl' -print | xargs awk '/^%s\\(/,/->/' | sed -E 's/->.*$/->/'"
                                                    ivy-erlang-complete-project-root
                                                    ivy-erlang-complete-erlang-root
                                                    (ivy-erlang-complete--filter-find)
                                                    mod fun)) "->"))
                               arity)))))
                     ((ivy-erlang-complete--extract-behaviours (buffer-file-name))
                      (or (ivy-erlang-complete--match-by-arity
                           (cl-mapcar (lambda (s) (concat (string-trim s) "."))
                                      (split-string
                                       (shell-command-to-string
                                        (format "find %s %s %s %s -print | xargs awk '/^-callback %s\\(/,/\\.$/'"
                                                ivy-erlang-complete-project-root
                                                ivy-erlang-complete-erlang-root
                                                (ivy-erlang-complete--filter-find)
                                                (ivy-erlang-complete--prepare-find-args
                                                 (ivy-erlang-complete--extract-behaviours (buffer-file-name)))
                                                mod-fun)) "\\\.$"))
                           arity)
                          (ivy-erlang-complete--match-by-arity
                           (cl-mapcar (lambda (s) (concat (string-trim s) "."))
                                      (split-string
                                       (shell-command-to-string
                                        (format "awk '/^-spec %s\\(/,/\\.$/' '%s'"
                                                mod-fun (expand-file-name (buffer-file-name)))) "\\\.$"))
                           arity)
                          (when ivy-erlang-complete-enable-fun-head-eldoc
                            (ivy-erlang-complete--match-by-arity
                             (cl-mapcar (lambda (s) (string-trim s))
                                        (split-string
                                         (shell-command-to-string
                                          (format "awk '/^%s\\(/,/->/' '%s' | sed -E 's/->.*$/->/'"
                                                  mod-fun (expand-file-name (buffer-file-name)))) "->"))
                             arity))))
                     (t (or
                         (ivy-erlang-complete--match-by-arity
                          (cl-mapcar (lambda (s) (concat (string-trim s) "."))
                                     (split-string
                                      (shell-command-to-string
                                       (format "awk '/^-spec %s\\(/,/\\.$/' '%s'"
                                               mod-fun (expand-file-name (buffer-file-name)))) "\\\.$"))
                          arity)
                         (when ivy-erlang-complete-enable-fun-head-eldoc
                           (ivy-erlang-complete--match-by-arity
                            (cl-mapcar (lambda (s) (string-trim s))
                                       (split-string
                                        (shell-command-to-string
                                         (format "awk '/^%s\\(/,/->/' '%s' | sed -E 's/->.*$/->/'"
                                                 mod-fun (expand-file-name (buffer-file-name)))) "->"))
                            arity)))))))
    (goto-char pos)
    (if (not (and (stringp info)
                  (string-equal "." info)))
        info
      "")))

(defun ivy-erlang-complete--match-by-arity (list arity)
  "Return first element from LIST matched by ARITY."
  (cl-find-if
   (lambda (el)
     (with-temp-buffer
       (insert el)
       (goto-char 0)
       (equal arity (erlang-get-arity-after-regexp "[^(]*(")))) list))

(defun ivy-erlang-complete--cur-arg-num ()
  "Return the number of current argument in argument list at point, or nil."
  (condition-case nil
      (let ((cur-pos (point))
            (res 0)
            (cont t))
        (ignore-errors (backward-up-list))
        (forward-char 1)
        (while cont
          (cond ((eobp)
                 (setq res nil)
                 (setq cont nil))
                ((> (point) cur-pos)
                 (setq cont nil))
                ((looking-at "\\s *)")
                 (setq cont nil))
                ((looking-at "\\s *\\($\\|%\\)")
                 (forward-line 1))
                ((looking-at "\\s *<<[^>]*?>>")
                 (when (zerop res)
                   (setq res (+ 1 res)))
                 (goto-char (match-end 0)))
                ((looking-at "\\s *,")
                 (setq res (+ 1 res))
                 (goto-char (match-end 0)))
                (t
                 (when (zerop res)
                   (setq res (+ 1 res)))
                 (forward-sexp 1))))
        (goto-char cur-pos)
        (if (equal (char-after) ?\,)
            (- res 1)
          res))
    (error nil)))

(defun ivy-erlang-complete--highlight-nth-arg (n exp)
  "Highlight Nth argument in EXP."
  (if (or (zerop n) (not exp))
      exp
    (with-temp-buffer
      (insert exp)
      (goto-char 0)
      (search-forward-regexp "[^(]*(" nil t)
      (let ((cont t)
            (begin nil)
            (end nil))
        (if (equal n 1)
            (setq begin (point)))
        (forward-sexp)
        (while cont
          (cond ((eobp)
                 (setq cont nil))
                ((and (not begin)
                      (equal n 1))
                 (setq begin (point)))
                ((equal n 0)
                 (setq cont nil))
                ((looking-at "\\s *)")
                 (setq cont nil))
                ((looking-at "\\s *\\($\\|%\\)")
                 (forward-line 1))
                ((looking-at "\\s *<<[^>]*?>>")
                 (goto-char (match-end 0)))
                ((looking-at "\\s *,")
                 (setq n (- n 1))
                 (goto-char (match-end 0)))
                (t
                 (forward-sexp 1))))
        (setq end (point))
        (if begin (add-text-properties begin end '(face eldoc-highlight-function-argument))))
      (buffer-substring (point-min) (point-max)))))

(provide 'ivy-erlang-complete)
;;; ivy-erlang-complete.el ends here
