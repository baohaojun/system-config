;;; lsp-eslint.el --- lsp-mode eslint integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'lsp-protocol)
(require 'lsp-mode)

(defconst lsp-eslint/status-ok 1)
(defconst lsp-eslint/status-warn 2)
(defconst lsp-eslint/status-error 3)

(defgroup lsp-eslint nil
  "ESlint language server group."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Microsoft/vscode-eslint"))

(defcustom lsp-eslint-unzipped-path (f-join lsp-server-install-dir "eslint/unzipped")
  "The path to the file in which `eslint' will be stored."
  :type 'file
  :group 'lsp-eslint
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-eslint-server-command `("node"
                                       "~/server/out/eslintServer.js"
                                       "--stdio")
  "Command to start eslint server."
  :risky t
  :type '(repeat string)
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-enable t
  "Controls whether eslint is enabled for JavaScript files or not."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-package-manager "npm"
  "The package manager you use to install node modules."
  :type '(choice (const :tag "npm" "npm")
                 (const :tag "yarn" "yarn")
                 (const :tag "pnpm" "pnpm")
                 (string :tag "other"))
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-format t
  "Whether to perform format."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-node-path nil
  "A path added to NODE_PATH when resolving the eslint module."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-node "node"
  "Path to nodejs."
  :type 'file
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-eslint-options nil
  "The eslint options object to provide args normally passed to
  eslint when executed from a command line (see
  http://eslint.org/docs/developer-guide/nodejs-api#cliengine)."
  :type 'alist)

(defcustom lsp-eslint-trace-server "off"
  "Traces the communication between VSCode and the eslint linter service."
  :type 'string)

(defcustom lsp-eslint-run "onType"
  "Run the linter on save (onSave) or on type (onType)"
  :type '(choice (const :tag "onSave" "onSave")
                 (const :tag "onType" "onType"))
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-auto-fix-on-save nil
  "Turns auto fix on save on or off."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-fix-all-problem-type
  "all"
  "Determines which problems are fixed when running the
source.fixAll code action."
  :type '(choice
          (const "all")
          (const "problems")
          string)
  :package-version '(lsp-mode . "7.0.1"))

(defcustom lsp-eslint-quiet nil
  "Turns on quiet mode, which ignores warnings."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-working-directories []
  ""
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-validate ["javascript" "javascriptreact"]
  "An array of language ids which should be validated by ESLint"
  :type 'lsp-string-vector
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-provide-lint-task nil
  "Controls whether a task for linting the whole workspace will be available."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-lint-task-enable nil
  "Controls whether a task for linting the whole workspace will be available."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-lint-task-options "."
  "Command line options applied when running the task for linting the whole
workspace (see https://eslint.org/docs/user-guide/command-line-interface)."
  :type 'string
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-runtime nil
  "The location of the node binary to run ESLint under."
  :type '(repeat string)
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-code-action-disable-rule-comment '((enable . t) (location . "separateLine"))
  ""
  :type 'alist
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-code-action-show-documentation '((enable . t))
  ""
  :type 'alist)

(defcustom lsp-eslint-experimental-incremental-sync t
  "Controls whether the new incremental text document synchronization should
be used."
  :type 'boolean
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-eslint-save-library-choices t
  "Controls whether to remember choices made to permit or deny ESLint libraries
from running."
  :type 'boolean
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-eslint-library-choices-file (expand-file-name (locate-user-emacs-file ".lsp-eslint-choices"))
  "The file where choices to permit or deny ESLint libraries from running is
stored."
  :type 'string
  :package-version '(lsp-mode . "7.1"))

(defun lsp--find-eslint ()
  (or
   (when-let ((workspace-folder (lsp-find-session-folder (lsp-session) default-directory)))
     (let ((eslint-local-path (f-join workspace-folder "node_modules" ".bin"
                                      (if (eq system-type 'windows-nt) "eslint.cmd" "eslint"))))
       (when (f-exists? eslint-local-path)
         eslint-local-path)))
   "eslint"))

(defun lsp-eslint-create-default-configuration ()
  "Create default eslint configuration."
  (interactive)
  (unless (lsp-session-folders (lsp-session))
    (user-error "There are no workspace folders"))
  (pcase (->> (lsp-session)
              lsp-session-folders
              (-filter (lambda (dir)
                         (-none?
                          (lambda (file) (f-exists? (f-join dir file)))
                          '(".eslintrc.js" ".eslintrc.yaml" ".eslintrc.yml" ".eslintrc" ".eslintrc.json")))))
    (`nil (user-error "All workspace folders contain eslint configuration"))
    (folders (let ((default-directory (completing-read "Select project folder: " folders nil t)))
               (async-shell-command (format "%s --init" (lsp--find-eslint)))))))

(lsp-defun lsp-eslint-status-handler (workspace (&eslint:StatusParams :state))
  (setf (lsp--workspace-status-string workspace)
        (propertize "ESLint"
                    'face (cond
                           ((eq state lsp-eslint/status-error) 'error)
                           ((eq state lsp-eslint/status-warn) 'warn)
                           (t 'success)))))

(lsp-defun lsp-eslint--configuration (_workspace (&ConfigurationParams :items))
  (->> items
       (seq-map (-lambda ((&ConfigurationItem :scope-uri?))
                  (-when-let* ((file (lsp--uri-to-path scope-uri?))
                               (buffer (find-buffer-visiting file))
                               (workspace-folder (lsp-find-session-folder (lsp-session) file)))
                    (with-current-buffer buffer
                      (list :validate "probe"
                            :packageManager lsp-eslint-package-manager
                            :codeActionOnSave (list :enable t
                                                    :mode lsp-eslint-fix-all-problem-type)
                            :format (lsp-json-bool lsp-eslint-format)
                            :options (or lsp-eslint-options (ht))
                            :run (or lsp-eslint-run "onType")
                            :nodePath lsp-eslint-node-path
                            :onIgnoredFiles "off"
                            :quiet (lsp-json-bool lsp-eslint-quiet)
                            :workspaceFolder (list :uri (lsp--path-to-uri workspace-folder)
                                                   :name (f-filename workspace-folder))
                            :codeAction (list
                                         :disableRuleComment (or lsp-eslint-code-action-disable-rule-comment
                                                                 (list :enable t
                                                                       :location "separateLine"))
                                         :showDocumentation (or lsp-eslint-code-action-show-documentation
                                                                (list :enable t))))))))
       (apply #'vector)))

(lsp-defun lsp-eslint--open-doc (_workspace (&eslint:OpenESLintDocParams :url))
  "Open documentation."
  (browse-url url))

(defun lsp-eslint-apply-all-fixes ()
  "Apply all autofixes in the current buffer."
  (interactive)
  (lsp-send-execute-command "eslint.applyAllFixes" (vector (lsp--versioned-text-document-identifier))))

;; XXX: replace with `lsp-make-interactive-code-action' macro
;; (lsp-make-interactive-code-action eslint-fix-all "source.fixAll.eslint")

(defun lsp-eslint-fix-all ()
  "Perform the source.fixAll.eslint code action, if available."
  (interactive)
  (condition-case nil
      (lsp-execute-code-action-by-kind "source.fixAll.eslint")
    (lsp-no-code-actions
     (when (called-interactively-p 'any)
       (lsp--info "source.fixAll.eslint action not available")))))

(defun lsp-eslint-server-command ()
  (if (lsp-eslint-server-exists? lsp-eslint-server-command)
      lsp-eslint-server-command
    `(,lsp-eslint-node ,(f-join lsp-eslint-unzipped-path
                                "extension/server/out/eslintServer.js")
                       "--stdio")))

(defun lsp-eslint-server-exists? (eslint-server-command)
  (let* ((command-name (f-base (f-filename (cl-first eslint-server-command))))
         (first-argument (cl-second eslint-server-command))
         (first-argument-exist (and first-argument (file-exists-p first-argument))))
    (if (equal command-name lsp-eslint-node)
        first-argument-exist
      (executable-find (cl-first eslint-server-command)))))

(defvar lsp-eslint--stored-libraries (ht)
  "Hash table defining if a given path to an ESLint library is allowed to run.
If the value for a key is 4, it will be allowed. If it is 1, it will not. If a
value does not exist for the key, or the value is nil, the user will be prompted
to allow or deny it.")

(when (and (file-exists-p lsp-eslint-library-choices-file)
           lsp-eslint-save-library-choices)
  (setq lsp-eslint--stored-libraries (lsp--read-from-file lsp-eslint-library-choices-file)))

(lsp-defun lsp-eslint--confirm-local (_workspace (&eslint:ConfirmExecutionParams :library-path) callback)
  (if-let ((option-alist '(("Always" 4 . t)
                           ("Yes" 4 . nil)
                           ("No" 1 . nil)
                           ("Never" 1 . t)))
           (remembered-answer (gethash library-path lsp-eslint--stored-libraries)))
      (funcall callback remembered-answer)
    (lsp-ask-question
     (format "Allow lsp-mode to execute %s?" library-path)
     (mapcar 'car option-alist)
     (lambda (response)
       (let ((option (cdr (assoc response option-alist))))
         (when (cdr option)
           (puthash library-path (car option) lsp-eslint--stored-libraries)
           (when lsp-eslint-save-library-choices
             (lsp--persist lsp-eslint-library-choices-file lsp-eslint--stored-libraries)))
         (funcall callback (car option)))))))

(defun lsp-eslint--probe-failed (_workspace _message)
  "Called when the server detects a misconfiguration in ESLint."
  (lsp--error "ESLint is not configured correctly. Please ensure your eslintrc is set up for the languages you are using."))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (lambda () (lsp-eslint-server-command))
   (lambda () (lsp-eslint-server-exists? (lsp-eslint-server-command))))
  :activation-fn (lambda (filename &optional _)
                   (when lsp-eslint-enable
                     (or (string-match-p (rx (one-or-more anything) "."
                                             (or "ts" "js" "jsx" "tsx" "html" "vue")eos)
                                         filename)
                         (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'html-mode))))
  :priority -1
  :completion-in-comments? t
  :add-on? t
  :multi-root t
  :notification-handlers (ht ("eslint/status" #'lsp-eslint-status-handler))
  :request-handlers (ht ("workspace/configuration" #'lsp-eslint--configuration)
                        ("eslint/openDoc" #'lsp-eslint--open-doc)
                        ("eslint/probeFailed" #'lsp-eslint--probe-failed))
  :async-request-handlers (ht ("eslint/confirmESLintExecution" #'lsp-eslint--confirm-local))
  :server-id 'eslint
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "random-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            `[,(lsp-make-file-system-watcher
                                                :glob-pattern "**/.eslintr{c.js,c.yaml,c.yml,c,c.json}")
                                              ,(lsp-make-file-system-watcher
                                                :glob-pattern "**/.eslintignore")
                                              ,(lsp-make-file-system-watcher
                                                :glob-pattern "**/package.json")])))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (let ((tmp-zip (make-temp-file "ext" nil ".zip")))
                          (delete-file tmp-zip)
                          (lsp-download-install
                           (lambda (&rest _)
                             (condition-case err
                                 (progn
                                   (lsp-unzip tmp-zip lsp-eslint-unzipped-path)
                                   (funcall callback))
                               (error (funcall error-callback err))))
                           error-callback
                           :url (lsp-vscode-extension-url "dbaeumer" "vscode-eslint" "2.1.14")
                           :store-path tmp-zip)))))

(provide 'lsp-eslint)
;;; lsp-eslint.el ends here
