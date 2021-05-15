;;; lsp-mssql.el --- MSSQL LSP bindings              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski
;; Version: 0.1

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: data, languages
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.2") (dash "2.14.1") (f "0.20.0") (ht "2.0") (lsp-treemacs "0.1"))
;; URL: https://github.com/emacs-lsp/lsp-mssql

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
;; lsp-mode client for MSSQL language server.

;;; Code:

(require 'lsp-mode)
(require 'lsp-treemacs)
(require 'gnutls)

(defgroup lsp-mssql nil
  "LSP support for the MSSQL server."
  :link '(url-link "https://github.com/microsoft/sqltoolsservice")
  :group 'lsp-mode)

(defcustom  lsp-mssql-download-location
  (expand-file-name "mssql/" user-emacs-directory)
  "Server download location."
  :type 'directory)

(defcustom lsp-mssql-default-batch-size 10
  "Default number of items to oad per query."
  :type 'number)

(defconst lsp-mssql-server-download-url
  "https://download.microsoft.com/download/c/2/f/c9857f58-e569-4677-ad24-f180e83a8252/microsoft.sqltools.servicelayer-%s")

(defconst lsp-mssql-executable-files
  '("MicrosoftSqlToolsServiceLayer.exe" "MicrosoftSqlToolsServiceLayer" "MicrosoftSqlToolsServiceLayer.dll"))

(declare-function org-mode "ext:org" ())
(declare-function org-show-all "ext:org" (types))
(declare-function org-table-align "ext:org-table" ())

(defvar lsp-mssql--connection-name->session-id (ht))

(defvar-local lsp-mssql-buffer-status nil
  "SQL buffer status.")

(put 'lsp-mssql-buffer-status 'risky-local-variable t)
(add-to-list 'global-mode-string (list '(t lsp-mssql-buffer-status)))

(defun lsp-mssql--extract (filename target-dir)
  "Extracts FILENAME into TARGET-DIR."
  (cond
   ((eq system-type 'windows-nt)
    ;; on windows, we attempt to use powershell v5+, available on Windows 10+
    (let ((powershell-version (substring
                               (shell-command-to-string "powershell -command \"(Get-Host).Version.Major\"")
                               0 -1)))
      (if (>= (string-to-number powershell-version) 5)
          (call-process "powershell"
                        nil
                        nil
                        nil
                        "-command"
                        (concat "add-type -assembly system.io.compression.filesystem;"
                                "[io.compression.zipfile]::ExtractToDirectory(\"" filename "\", \"" target-dir "\")"))

        (message (concat "lsp-csharp: for automatic server installation procedure"
                         " to work on Windows you need to have powershell v5+ installed")))))

   ((or (eq system-type 'gnu/linux)
        (eq system-type 'darwin))
    (call-process "tar" nil nil t "xf" filename "-C" target-dir))

   (t (error "MSSQL server cannot extract \"%s\" on platform %s (yet)" filename system-type))))

(defun lsp-mssql--download (url filename)
  "Downloads file from URL as FILENAME.
Will not do anything should the file exist already."
  (let ((gnutls-algorithm-priority
         (if (and (not gnutls-algorithm-priority)
                  (boundp 'libgnutls-version)
                  (>= libgnutls-version 30603)
                  (version<= emacs-version "26.2"))
             "NORMAL:-VERS-TLS1.3"
           gnutls-algorithm-priority)))
    (url-copy-file url filename t)))


(defun lsp-mssql-download-server ()
  "Download mssql server.
Uses `powershell' on windows and `tar' on Linux to extract the server binary."
  (interactive)
  (let* ((result (cond ((eq system-type 'darwin)  "osx-x64-netcoreapp2.2.tar.gz")
                       ((eq system-type 'gnu/linux)  "rhel-x64-netcoreapp2.2.tar.gz")
                       ((eq system-type 'windows-nt) "win-x64-netcoreapp2.2.zip")
                       (t (error (format "Unsupported system: %s" system-type)))))
         (download-location (f-join temporary-file-directory result))
         (url (format lsp-mssql-server-download-url result)))
    (lsp--info "Starting downloading mssql server in %s from %s." download-location url)
    (lsp-mssql--download url download-location)
    (mkdir lsp-mssql-download-location t)
    (lsp-mssql--extract download-location lsp-mssql-download-location)

    (seq-doseq (file lsp-mssql-executable-files)
      (let ((target-file (f-join lsp-mssql-download-location file)))
        (when (f-exists? target-file)
          (chmod target-file #o755)))))
  (lsp--info "Downloading mssql server finished."))

(defun lsp-mssql--expand-completed (workspace params)
  "Expand completed handler.
WORKSPACE is the active workspace.
PARAMS expand completed params."
  (-let [(&hash "nodePath" node-path "nodes") params]
    (puthash (cons node-path nil)
             (append nodes nil)
             (gethash "explorer" (lsp--workspace-metadata workspace)) )
    (lsp-mssql-object-explorer)))

(defun lsp-mssql--session-created (workspace params)
  "Session created handler.
WORKSPACE is the active workspace.
PARAMS Session created handler."
  (-let [(&hash "rootNode" root-node "sessionId" session-id) params]
    (puthash (cons session-id t)
             (list root-node)
             (gethash "explorer" (lsp--workspace-metadata workspace)) )
    (lsp-mssql-object-explorer)))

(defcustom lsp-mssql-log-debug-info nil
  "[Optional] Log debug output to the VS Code console (Help -> Toggle Developer Tools)."
  :type 'boolean)

(define-widget 'lsp-connection-vector 'lazy
  "Connection vector."
  :offset 4
  :tag "Connections Vector"
  :type '(restricted-sexp
          :match-alternatives (lambda (candidate)
                                (and
                                 (vectorp candidate)
                                 (seq-every-p #'listp candidate)))))

(defcustom lsp-mssql-connections ['(:server "{{put-server-name-here}}"
                                            :database  "{{put-database-name-here}}"
                                            :user  "{{put-username-here}}"
                                            :password  "{{put-password-here}}")]
  "Connection profiles defined in 'User Settings' are shown under 'MS SQL: Connect' command in the command palette."
  :type 'lsp-connection-vector)

(defcustom lsp-mssql-intelli-sense-enable-intelli-sense t
  "Should IntelliSense be enabled."
  :type 'boolean)

(defcustom lsp-mssql-intelli-sense-enable-error-checking t
  "Should IntelliSense error checking be enabled."
  :type 'boolean)

(defcustom lsp-mssql-intelli-sense-enable-suggestions t
  "Should IntelliSense suggestions be enabled."
  :type 'boolean)

(defcustom lsp-mssql-intelli-sense-enable-quick-info t
  "Should IntelliSense quick info be enabled."
  :type 'boolean)

(defcustom lsp-mssql-intelli-sense-lower-case-suggestions t
  "Should IntelliSense suggestions be lowercase."
  :type 'boolean)

(lsp-register-custom-settings
 '(("mssql.intelliSense.lowerCaseSuggestions" lsp-mssql-intelli-sense-lower-case-suggestions t)
   ("mssql.intelliSense.enableQuickInfo" lsp-mssql-intelli-sense-enable-quick-info t)
   ("mssql.intelliSense.enableSuggestions" lsp-mssql-intelli-sense-enable-suggestions t)
   ("mssql.intelliSense.enableErrorChecking" lsp-mssql-intelli-sense-enable-error-checking t)
   ("mssql.intelliSense.enableIntelliSense" lsp-mssql-intelli-sense-enable-intelli-sense t)
   ("mssql.connections" lsp-mssql-connections)
   ("mssql.logDebugInfo" lsp-mssql-log-debug-info t)))

(defmacro lsp-mssql-with-result-buffer (&rest body)
  "Evaluate BODY in result buffer."
  `(with-current-buffer (get-buffer-create "*SQL Results*")
     (prog1 (save-excursion ,@body)
       (org-show-all '(headings blocks)))))

(defun lsp-mssql--connection-complete (_workspace params)
  "Connection completed handler.
PARAMS connection completed params."
  ;; (setq params my/params)
  (if (gethash "connectionId" params)

      (-let [(&hash "ownerUri" owner
                    "connectionSummary" (&hash "serverName" server-name  "databaseName" database-name)) params]
        (lsp--info "Successfully connected to %s, database: %s" server-name database-name)
        (when-let (buffer (find-buffer-visiting (lsp--uri-to-path owner)))
          (with-current-buffer buffer
            (setq lsp-mssql-buffer-status (propertize (format "database::%s:%s" server-name database-name)
                                                      'face 'success)))))
    (-let [(&hash "errorMessage" error-message
                  "errorNumber" error-number) params]
      (lsp--error "Failed to connect with the following error error number: %s\n%s" error-number error-message))))

(defun lsp-mssql--result-set-available (_workspace _params)
  "Result set available handler.")

(defun lsp-mssql--message (_workspace params)
  "Message handler.
PARAMS the params."
  (-let [(&hash "message" (&hash "message" "time" "isError" is-error)) params]
    (funcall (if is-error 'lsp--error 'lsp--info) (format "%s %s" time message))))

(defun lsp-mssql--result-set-updated (_workspace _params)
  "Result set update handler.")

(defun lsp-mssql--render-table (result)
  "Render RESULT as a table."
  (->> result
       (gethash "resultSubset")
       (gethash "rows")
       (seq-map (lambda (item)
                  (->> item
                       (seq-map (-lambda ((&hash "displayValue"))
                                  (concat "|" displayValue)))
                       (apply #'concat))))
       (s-join "|\n")))

(defun lsp-mssql--result-set-complete (workspace params)
  "Result set complete handler.
WORKSPACE is the active workspace.
PARAMS the params."
  (-let* ((marker (lsp-mssql-with-result-buffer
                   (goto-char (point-max))
                   (insert (format "|%s|\n"(s-join "|" (seq-map (-lambda ((&hash "columnName" name))
                                                                  name)
                                                                (gethash "columnInfo" (gethash "resultSetSummary" params))))))
                   (insert "|-")
                   (org-table-align)
                   (goto-char (point-at-eol))
                   (copy-marker (point))))
          ((&hash "ownerUri" owner-uri
                  "resultSetSummary"(&hash "rowCount" row-count "batchId" "id")) params)
          (loaded-index 0)
          (to-load lsp-mssql-default-batch-size)
          more-items-marker)
    (cl-labels ((subset-params
                 ()
                 (if (< loaded-index row-count)
                     (let ((to-load (if (> (+ loaded-index to-load) row-count)
                                        (- row-count loaded-index)
                                      to-load)))
                       `(:ownerUri ,owner-uri
                                   :resultSetIndex ,id
                                   :rowsCount ,to-load
                                   :rowsStartIndex ,(prog1 loaded-index
                                                      (setf loaded-index (+ loaded-index to-load)))
                                   :batchIndex ,batchId))
                   (lsp--info "All items are loaded")
                   nil)))
      (when-let ((params (subset-params)))
        (with-lsp-workspace workspace
          (lsp-request-async
           "query/subset"
           params
           (lambda (result)
             (lsp-mssql-with-result-buffer
              (goto-char (marker-position marker))
              (insert "\n")
              (insert (lsp-mssql--render-table result))
              (setq more-items-marker (copy-marker (point)))
              (insert "\n")
              (let ((start (point)))
                (insert (format "Load %s more... (%s/%s)" to-load loaded-index row-count))
                (make-button
                 start (point)
                 'action (lambda (&rest _)
                           (interactive)
                           (when-let ((params (subset-params)))
                             (with-lsp-workspace workspace
                               (lsp-request-async
                                "query/subset"
                                params
                                (lambda (result)
                                  (lsp-mssql-with-result-buffer
                                   (goto-char (1- (marker-position more-items-marker)))
                                   (insert "\n")
                                   (insert (lsp-mssql--render-table result))
                                   (org-table-align)))
                                :mode 'detached))))
                 'keymap (-doto (make-sparse-keymap)
                           (define-key [M-return] 'push-button)
                           (define-key [mouse-2] 'push-button))
                 'help-echo "mouse-2, M-RET: Load more items."))
              (insert "\n\n")
              (org-mode)
              (goto-char (marker-position marker))
              (org-table-align)
              (pop-to-buffer (current-buffer))))
           :mode 'detached))))))

(defun lsp-mssql--batch-complete (_workspace _params)
  "Hanler for batch complete.")

(defun lsp-mssql--complete (_workspace _params)
  "Hanler for complete."
  (lsp-mssql-with-result-buffer))

(defvar-local lsp-mssql--markers (ht))

(defun lsp-mssql--batch-start (_workspace params)
  "Batch start handler.
PARAMS batch handler params."
  (lsp-mssql-with-result-buffer
   (-let [(&hash "batchSummary" (&hash "executionStart" execution-start
                                       ;; "selection" (&hash "startColumn"
                                       ;;                    "startLine"
                                       ;;                    "endLine"
                                       ;;                    "endColumn")
                                       ;; "id"
                                       )
                 ;; "ownerUri" owner-uri
                 ) params]
     ;; (insert (format "* Batch %s\n" id))
     (lsp--info "%s Started execution" execution-start)
     ;; (insert "#+BEGIN_SRC sql\n")
     ;; (insert (with-current-buffer (find-buffer-visiting (lsp--uri-to-path owner-uri))
     ;;           (buffer-substring (lsp--position-to-point (ht ("line" startLine)
     ;;                                                         ("character" startColumn)))
     ;;                             (lsp--position-to-point (ht ("line" endLine)
     ;;                                                         ("character" endColumn))))))
     ;; (insert "#+END_SRC\n\n")
     )
   (display-buffer-in-side-window  (current-buffer) '((side . bottom)))))

(defun lsp-mssql--connection-changed (_workspace _params)
  "Hanler for batch complete.")

(defun lsp-mssql--after-open-fn ()
  "After open handler."
  (setq lsp-mssql-buffer-status (propertize "SQL Server disconnected"
                                            'face 'warning
                                            'local-map (make-mode-line-mouse-map
                                                        'mouse-1 'lsp-mssql-connect))))
(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   (lambda ()
                     (let ((server (f-join lsp-mssql-download-location "MicrosoftSqlToolsServiceLayer")))
                       (if (executable-find server)
                           (list server)
                         (if (y-or-n-p "The mssql server is not present. Do you want to download it? ")
                             (progn
                               (lsp-mssql-download-server)
                               (list server))
                           (user-error "Server is not installed? "))))))
                  :major-modes '(sql-mode)
                  :priority -1
                  :multi-root t
                  :notification-handlers (ht ("objectexplorer/expandCompleted" #'lsp-mssql--expand-completed)
                                             ("objectexplorer/sessioncreated" 'lsp-mssql--session-created)
                                             ("connection/complete" #'lsp-mssql--connection-complete)
                                             ("connection/connectionchanged" 'lsp-mssql--connection-changed)
                                             ("telemetry/sqlevent" 'ignore)
                                             ("textDocument/intelliSenseReady" 'ignore)
                                             ("query/resultSetAvailable" 'lsp-mssql--result-set-available)
                                             ("query/message" 'lsp-mssql--message)
                                             ("query/resultSetUpdated" 'lsp-mssql--result-set-updated)
                                             ("query/resultSetComplete" 'lsp-mssql--result-set-complete)
                                             ("query/batchComplete"  'lsp-mssql--batch-complete)
                                             ("query/batchStart"  'lsp-mssql--batch-start)
                                             ("query/complete" 'lsp-mssql--complete))
                  :server-id 'sql
                  :initialized-fn (lambda (workspace)
                                    (ht-clear lsp-mssql--connection-name->session-id)
                                    (puthash "explorer" (ht) (lsp--workspace-metadata workspace))
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "mssql"))))
                  :after-open-fn #'lsp-mssql--after-open-fn))

(defun lsp-mssql-connect (connection)
  "Connect current buffer to a server using CONNECTION spec."
  (interactive (list (lsp--completing-read
                      "Select connection: "
                      (append lsp-mssql-connections nil)
                      (lambda (connection)
                        (plist-get connection :server))
                      nil
                      t)))
  (lsp-request "connection/connect"
               `(:ownerUri ,(lsp--buffer-uri)
                           :connection (:options ,connection))))

(defun lsp-mssql-disconnect ()
  "Disconnect buffer from sql server."
  (interactive)
  (lsp-request "connection/disconnect" `(:ownerUri ,(lsp--buffer-uri)))
  (lsp-mssql--after-open-fn))

(defun lsp-mssql-execute-region (start end)
  "Execute selected region START to END."
  (interactive "r")
  (lsp-mssql-with-result-buffer
   (org-mode)
   (erase-buffer))
  (-let (((&plist :line start-line
                  :character start-character) (lsp--point-to-position start))
         ((&plist :line end-line
                  :character end-character) (lsp--point-to-position end)))
    (lsp-request
     "query/executeDocumentSelection"
     (list :ownerUri (lsp--buffer-uri)
           :querySelection (list :startLine start-line
                                 :startColumn start-character
                                 :endLine end-line
                                 :endColumn end-character)))))

(defun lsp-mssql-execute-buffer ()
  "Execute the SQL code in the buffer."
  (interactive)
  (lsp-mssql-with-result-buffer
   (org-mode)
   (erase-buffer))
  (lsp-request "query/executeDocumentSelection" (list :ownerUri (lsp--buffer-uri))))


;; object explorer

(treemacs-modify-theme "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "images")
  :config
  (progn
    (treemacs-create-icon :file "AggregateFunction.png" :extensions (AggregateFunction) :fallback "-")
    (treemacs-create-icon :file "AggregateFunctionParameter_Input.png" :extensions (AggregateFunctionParameter_Input) :fallback "-")
    (treemacs-create-icon :file "AggregateFunctionParameter_Output.png" :extensions (AggregateFunctionParameter_Output) :fallback "-")
    (treemacs-create-icon :file "AggregateFunctionParameter_Return.png" :extensions (AggregateFunctionParameter_Return) :fallback "-")
    (treemacs-create-icon :file "ApplicationRole.png" :extensions (ApplicationRole) :fallback "-")
    (treemacs-create-icon :file "Assembly.png" :extensions (Assembly) :fallback "-")
    (treemacs-create-icon :file "AsymmetricKey.png" :extensions (AsymmetricKey) :fallback "-")
    (treemacs-create-icon :file "BrokerPriority.png" :extensions (BrokerPriority) :fallback "-")
    (treemacs-create-icon :file "Certificate.png" :extensions (Certificate) :fallback "-")
    (treemacs-create-icon :file "Column.png" :extensions (Column) :fallback "-")
    (treemacs-create-icon :file "ColumnEncryptionKey.png" :extensions (ColumnEncryptionKey) :fallback "-")
    (treemacs-create-icon :file "ColumnMasterKey.png" :extensions (ColumnMasterKey) :fallback "-")
    (treemacs-create-icon :file "Constraint.png" :extensions (Constraint) :fallback "-")
    (treemacs-create-icon :file "Contract.png" :extensions (Contract) :fallback "-")
    (treemacs-create-icon :file "Database.png" :extensions (Database) :fallback "-")
    (treemacs-create-icon :file "DatabaseAndQueueEventNotification.png" :extensions (DatabaseAndQueueEventNotification) :fallback "-")
    (treemacs-create-icon :file "DatabaseAuditSpecification.png" :extensions (DatabaseAuditSpecification) :fallback "-")
    (treemacs-create-icon :file "DatabaseEncryptionKey.png" :extensions (DatabaseEncryptionKey) :fallback "-")
    (treemacs-create-icon :file "DatabaseRole.png" :extensions (DatabaseRole) :fallback "-")
    (treemacs-create-icon :file "DatabaseScopedCredential.png" :extensions (DatabaseScopedCredential) :fallback "-")
    (treemacs-create-icon :file "DatabaseTrigger.png" :extensions (DatabaseTrigger) :fallback "-")
    (treemacs-create-icon :file "Database_Unavailable.png" :extensions (Database_Unavailable) :fallback "-")
    (treemacs-create-icon :file "DefaultIcon.png" :extensions (DefaultIcon) :fallback "-")
    (treemacs-create-icon :file "ExternalDataSource.png" :extensions (ExternalDataSource) :fallback "-")
    (treemacs-create-icon :file "ExternalFileFormat.png" :extensions (ExternalFileFormat) :fallback "-")
    (treemacs-create-icon :file "FileGroupFile.png" :extensions (FileGroupFile) :fallback "-")
    (treemacs-create-icon :file "Folder.png" :extensions (Folder) :fallback "-")
    (treemacs-create-icon :file "FullTextCatalog.png" :extensions (FullTextCatalog) :fallback "-")
    (treemacs-create-icon :file "FullTextStopList.png" :extensions (FullTextStopList) :fallback "-")
    (treemacs-create-icon :file "HDFSFolder.png" :extensions (HDFSFolder) :fallback "-")
    (treemacs-create-icon :file "Index.png" :extensions (Index) :fallback "-")
    (treemacs-create-icon :file "Key_ForeignKey.png" :extensions (Key_ForeignKey) :fallback "-")
    (treemacs-create-icon :file "Key_PrimaryKey.png" :extensions (Key_PrimaryKey) :fallback "-")
    (treemacs-create-icon :file "Key_UniqueKey.png" :extensions (Key_UniqueKey) :fallback "-")
    (treemacs-create-icon :file "MasterKey.png" :extensions (MasterKey) :fallback "-")
    (treemacs-create-icon :file "MessageType.png" :extensions (MessageType) :fallback "-")
    (treemacs-create-icon :file "PartitionFunction.png" :extensions (PartitionFunction) :fallback "-")
    (treemacs-create-icon :file "PartitionScheme.png" :extensions (PartitionScheme) :fallback "-")
    (treemacs-create-icon :file "Queue.png" :extensions (Queue) :fallback "-")
    (treemacs-create-icon :file "RemoteServiceBinding.png" :extensions (RemoteServiceBinding) :fallback "-")
    (treemacs-create-icon :file "Route.png" :extensions (Route) :fallback "-")
    (treemacs-create-icon :file "ScalarValuedFunction.png" :extensions (ScalarValuedFunction) :fallback "-")
    (treemacs-create-icon :file "ScalarValuedFunctionParameter_Input.png" :extensions (ScalarValuedFunctionParameter_Input) :fallback "-")
    (treemacs-create-icon :file "ScalarValuedFunctionParameter_Output.png" :extensions (ScalarValuedFunctionParameter_Output) :fallback "-")
    (treemacs-create-icon :file "ScalarValuedFunctionParameter_Return.png" :extensions (ScalarValuedFunctionParameter_Return) :fallback "-")
    (treemacs-create-icon :file "Schema.png" :extensions (Schema) :fallback "-")
    (treemacs-create-icon :file "SearchPropertyList.png" :extensions (SearchPropertyList) :fallback "-")
    (treemacs-create-icon :file "SecurityPolicy.png" :extensions (SecurityPolicy) :fallback "-")
    (treemacs-create-icon :file "Sequence.png" :extensions (Sequence) :fallback "-")
    (treemacs-create-icon :file "ServerLevelCredential.png" :extensions (ServerLevelCredential) :fallback "-")
    (treemacs-create-icon :file "ServerLevelCryptographicProvider.png" :extensions (ServerLevelCryptographicProvider) :fallback "-")
    (treemacs-create-icon :file "ServerLevelEndpoint.png" :extensions (ServerLevelEndpoint) :fallback "-")
    (treemacs-create-icon :file "ServerLevelLinkedServer.png" :extensions (ServerLevelLinkedServer) :fallback "-")
    (treemacs-create-icon :file "ServerLevelLinkedServerLogin.png" :extensions (ServerLevelLinkedServerLogin) :fallback "-")
    (treemacs-create-icon :file "ServerLevelLinkedServerLogin_Disabled.png" :extensions (ServerLevelLinkedServerLogin_Disabled) :fallback "-")
    (treemacs-create-icon :file "ServerLevelLogin.png" :extensions (ServerLevelLogin) :fallback "-")
    (treemacs-create-icon :file "ServerLevelLogin_Disabled.png" :extensions (ServerLevelLogin_Disabled) :fallback "-")
    (treemacs-create-icon :file "ServerLevelServerAudit.png" :extensions (ServerLevelServerAudit) :fallback "-")
    (treemacs-create-icon :file "ServerLevelServerAuditSpecification.png" :extensions (ServerLevelServerAuditSpecification) :fallback "-")
    (treemacs-create-icon :file "ServerLevelServerRole.png" :extensions (ServerLevelServerRole) :fallback "-")
    (treemacs-create-icon :file "ServerLevelServerTrigger.png" :extensions (ServerLevelServerTrigger) :fallback "-")
    (treemacs-create-icon :file "Server_green.png" :extensions (Server) :fallback "-")
    (treemacs-create-icon :file "Server_red.png" :extensions (Server_red) :fallback "-")
    (treemacs-create-icon :file "Service.png" :extensions (Service) :fallback "-")
    (treemacs-create-icon :file "SqlLogFile.png" :extensions (SqlLogFile) :fallback "-")
    (treemacs-create-icon :file "Statistic.png" :extensions (Statistic) :fallback "-")
    (treemacs-create-icon :file "StoredProcedure.png" :extensions (StoredProcedure) :fallback "-")
    (treemacs-create-icon :file "StoredProcedureParameter_Input.png" :extensions (StoredProcedureParameter_Input) :fallback "-")
    (treemacs-create-icon :file "StoredProcedureParameter_Output.png" :extensions (StoredProcedureParameter_Output) :fallback "-")
    (treemacs-create-icon :file "StoredProcedureParameter_Return.png" :extensions (StoredProcedureParameter_Return) :fallback "-")
    (treemacs-create-icon :file "SymmetricKey.png" :extensions (SymmetricKey) :fallback "-")
    (treemacs-create-icon :file "Synonym.png" :extensions (Synonym) :fallback "-")
    (treemacs-create-icon :file "SystemApproximateNumeric.png" :extensions (SystemApproximateNumeric) :fallback "-")
    (treemacs-create-icon :file "SystemBinaryString.png" :extensions (SystemBinaryString) :fallback "-")
    (treemacs-create-icon :file "SystemCharacterString.png" :extensions (SystemCharacterString) :fallback "-")
    (treemacs-create-icon :file "SystemClrDataType.png" :extensions (SystemClrDataType) :fallback "-")
    (treemacs-create-icon :file "SystemContract.png" :extensions (SystemContract) :fallback "-")
    (treemacs-create-icon :file "SystemDateAndTime.png" :extensions (SystemDateAndTime) :fallback "-")
    (treemacs-create-icon :file "SystemExactNumeric.png" :extensions (SystemExactNumeric) :fallback "-")
    (treemacs-create-icon :file "SystemMessageType.png" :extensions (SystemMessageType) :fallback "-")
    (treemacs-create-icon :file "SystemOtherDataType.png" :extensions (SystemOtherDataType) :fallback "-")
    (treemacs-create-icon :file "SystemQueue.png" :extensions (SystemQueue) :fallback "-")
    (treemacs-create-icon :file "SystemService.png" :extensions (SystemService) :fallback "-")
    (treemacs-create-icon :file "SystemSpatialDataType.png" :extensions (SystemSpatialDataType) :fallback "-")
    (treemacs-create-icon :file "SystemUnicodeCharacterString.png" :extensions (SystemUnicodeCharacterString) :fallback "-")
    (treemacs-create-icon :file "Table.png" :extensions (Table) :fallback "-")
    (treemacs-create-icon :file "TableValuedFunction.png" :extensions (TableValuedFunction) :fallback "-")
    (treemacs-create-icon :file "TableValuedFunctionParameter_Input.png" :extensions (TableValuedFunctionParameter_Input) :fallback "-")
    (treemacs-create-icon :file "TableValuedFunctionParameter_Output.png" :extensions (TableValuedFunctionParameter_Output) :fallback "-")
    (treemacs-create-icon :file "TableValuedFunctionParameter_Return.png" :extensions (TableValuedFunctionParameter_Return) :fallback "-")
    (treemacs-create-icon :file "Table_Temporal.png" :extensions (Table_Temporal) :fallback "-")
    (treemacs-create-icon :file "Trigger.png" :extensions (Trigger) :fallback "-")
    (treemacs-create-icon :file "Trigger_Disabled.png" :extensions (Trigger_Disabled) :fallback "-")
    (treemacs-create-icon :file "User.png" :extensions (User) :fallback "-")
    (treemacs-create-icon :file "UserDefinedDataType.png" :extensions (UserDefinedDataType) :fallback "-")
    (treemacs-create-icon :file "UserDefinedTableType.png" :extensions (UserDefinedTableType) :fallback "-")
    (treemacs-create-icon :file "UserDefinedTableTypeColumn.png" :extensions (UserDefinedTableTypeColumn) :fallback "-")
    (treemacs-create-icon :file "UserDefinedTableTypeConstraint.png" :extensions (UserDefinedTableTypeConstraint) :fallback "-")
    (treemacs-create-icon :file "UserDefinedType.png" :extensions (UserDefinedType) :fallback "-")
    (treemacs-create-icon :file "User_Disabled.png" :extensions (User_Disabled) :fallback "-")
    (treemacs-create-icon :file "View.png" :extensions (View) :fallback "-")
    (treemacs-create-icon :file "XmlSchemaCollection.png" :extensions (XmlSchemaCollection) :fallback "-")
    (treemacs-create-icon :file "add.png" :extensions (add) :fallback "-")
    (treemacs-create-icon :file "add_inverse.png" :extensions (add_inverse) :fallback "-")))

(defvar lsp-mssql-object-explorer-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "R")  #'lsp-mssql-object-explorer-refresh)
    (define-key (kbd "C")  #'lsp-mssql-object-explorer-connect)
    (define-key (kbd "K")  #'lsp-mssql-object-explorer-disconnect))
  "Keymap for `lsp-mssql-object-explorer-mode'.")

(define-minor-mode lsp-mssql-object-explorer-mode
  "Minor mode for SQL Object Explorer."
  nil nil lsp-mssql-object-explorer-map)

(defun lsp-mssql-object-explorer-disconnect ()
  "Disconnect placeholder."
  (interactive)
  (user-error "To be implemented? "))

(defun lsp-mssql-object-explorer-refresh ()
  "Refresh object explorer."
  (interactive)
  (-let [(&plist :session-id :node-path) (button-get (treemacs-node-at-point) :item)]
    (with-lsp-workspace (lsp-find-workspace 'sql)
                        (lsp-request "objectexplorer/refresh"
                                     `(:sessionId ,session-id :nodePath ,node-path)))))

(defun lsp-mssql--to-node (nodes &optional session-id node)
  "Convert NODE to lsp-treemacs generic node.
SESSION-ID - the session id.
NODES - all nodes."
  (setq session-id (or session-id (gethash "sessionId" node)))

  (-let [(&hash "label" "nodeType" node-type "nodePath" node-path "isLeaf" leaf?) node]
    `(:label ,label
             :key ,label
             :icon ,(intern node-type)
             :node-path ,node-path
             :session-id ,session-id
             :actions ,(if (string= node-type "Server")
                           '(["Refresh" lsp-mssql-object-explorer-refresh]
                             ["Disconnect" lsp-mssql-object-explorer-disconnect])
                         '(["Refresh" lsp-mssql-object-explorer-refresh]))
             ,@(unless leaf?
                 (list :children
                       (lambda (_node)
                         (let ((children (gethash (list node-path) nodes :empty)))
                           (if (not (eq :empty children))
                               (-map (-partial #'lsp-mssql--to-node nodes session-id) children)
                             (with-lsp-workspace (lsp-find-workspace 'sql nil)
                               (lsp-request "objectexplorer/expand"
                                            `(:sessionId ,session-id :nodePath ,node-path))
                               nil)))))))))

(defun lsp-mssql--show-explorer (tree title)
  "Show explorer.
TREE is the data to display, TITLE will be used for the
modeline in the result buffer."
  (with-current-buffer (get-buffer-create "*SQL Object explorer*")
    (lsp-treemacs-initialize)
    (setq-local lsp-treemacs-tree tree)
    (setq-local face-remapping-alist '((button . default)))
    (lsp-treemacs-generic-refresh)
    (display-buffer-in-side-window (current-buffer) '((side . right)))
    (setq-local mode-name title)
    (lsp-mssql-object-explorer-mode)))



(defun lsp-mssql-object-explorer-connect (&rest _)
  "Open connection."
  (interactive)
  (-let [(connection &as &plist :server :user) (plist-get (button-get (treemacs-node-at-point) :item) :connection)]
    (with-lsp-workspace (lsp-find-workspace 'sql)
      (puthash
       (concat server " : " user)
       (gethash "sessionId"
                (lsp-request "objectexplorer/createsession"
                             (list :options connection)))
       lsp-mssql--connection-name->session-id))))

(defun lsp-mssql-object-explorer ()
  "Show server explorer."
  (interactive)
  (lsp-mssql--show-explorer
   (append
    (let ((all-nodes (-some->> (lsp-find-workspace 'sql nil)
                       lsp--workspace-metadata
                       (gethash "explorer"))))
      (->> all-nodes
           (ht->alist)
           (-keep (-lambda (((id . is-session?) . (node)))
                    (when is-session?
                      (lsp-mssql--to-node all-nodes id node))))))
    (->> lsp-mssql-connections
         (seq-map
          (-lambda ((connection &as &plist :server :user))
            (let ((connection-name (concat server " : " user)))
              (unless (gethash connection-name
                               lsp-mssql--connection-name->session-id)
                `(:label ,connection-name
                         :icon Server_red
                         :key ,connection-name
                         :connection ,connection
                         :actions (["Connect" lsp-mssql-object-explorer-connect]))))))
         (seq-filter #'identity)))
   "*MSSQL Object Explorer*"))



(provide 'lsp-mssql)
;;; lsp-mssql.el ends here
