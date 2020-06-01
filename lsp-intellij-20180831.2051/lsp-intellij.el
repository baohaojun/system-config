;;; lsp-intellij.el --- intellij lsp client                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Ian Pickering

;; Author: Ruin0x11 <ipickering2@gmail.com>
;; Keywords: languages processes tools
;; Package-Version: 20180831.2051
;; Package-Commit: cf30f0ac63bd0140e758840b8ab070e8313697b2
;; Package-Requires: ((emacs "25.1") (lsp-mode "4.1"))
;; Version: 0.1
;; URL: https://github.com/Ruin0x11/lsp-intellij

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

;; lsp-mode client for intellij-lsp-server.
;; After installing lsp-mode, you can use it as follows:

;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-intellij)
;;   (add-hook 'java-mode-hook #'lsp-intellij-enable))

;; Then, after opening and configuring a project in your instance of
;; IntelliJ that has intellij-lsp-server, navigate to a Java file tracked
;; by that project.

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)

(defvar lsp-intellij-server-port 8080)
(defvar lsp-intellij--config-options (make-hash-table))
(defvar-local lsp-intellij--progress-state (make-hash-table :test 'equal))

(defcustom lsp-intellij-use-topmost-root t
  "Whether or not to use the topmost IntelliJ project in a nested hierarchy.

If non-nil, `lsp-intellij' will attempt to locate the topmost IntelliJ project
in a nested hierarchy if a nested subproject is opened and set the LSP project
root to it. Otherwise, `lsp-intellij' will set the project root to be the
project furthest up the directory hierarchy."
  :type 'boolean
  :group 'lsp-intellij
  :safe 'booleanp)

;; copied from projectile to avoid a dependency
(defun lsp-intellij--parent (path)
  "Return the parent directory of PATH.
PATH may be a file or directory and directory paths may end with a slash."
  (directory-file-name (file-name-directory (directory-file-name (expand-file-name path)))))

;; copied from projectile to avoid a dependency
(defun lsp-intellij--root-top-down-recurring (dir list)
  "Find a project root in DIR by recurring top-down search for files in LIST.
Return the last (bottommost) matched directory in the topmost sequence
of matched directories.  Nil otherwise."
  (cl-some
   (lambda (f)
     (locate-dominating-file
      dir
      (lambda (dir)
        (and (file-exists-p (expand-file-name f dir))
             (or (string-match locate-dominating-stop-dir-regexp (lsp-intellij--parent dir))
                 (not (file-exists-p (expand-file-name f (lsp-intellij--parent dir)))))))))
   list))

(defun lsp-intellij--any-value-in-hash (hash-table)
  "Return any value in HASH-TABLE."
  (block nil
    (maphash (lambda (k v) (return v))
             hash-table)))

(defun lsp-intellij--was-file-extracted-from-jar (filename)
  "Test if the file FILENAME was extracted from a .jar.

This is when the buffer name indicates the file is a temporary buffer created by
`archive-mode' or the FILENAME exists in `temporary-file-directory'."
  (or
   ;; extracted from archive-mode (prevents error when LSP mode hooks run)
   (and (string-match-p "\.jar:[a-zA-Z]+" filename) (not (file-exists-p filename)))
   ;; extracted using lsp-intellij
   (string-match-p (regexp-quote temporary-file-directory) filename)))

(defun lsp-intellij--get-root ()
  "Get the project root for IntelliJ's LSP server for the current file.

It looks for an .idea folder in a parent directory of the file and uses that by
default. if `lsp-intellij-use-topmost-root' is non-nil, it will look for
the root highest up in the directory hierarchy. If not, it will stop at the
first root found.

If the file was extracted from a .jar, an arbitrary LSP workspace root will be
used."
  (if (lsp-intellij--was-file-extracted-from-jar (buffer-file-name))
      (lsp--workspace-root (lsp-intellij--any-value-in-hash lsp--workspaces))
    (let ((file (locate-dominating-file (buffer-file-name)
                                        (lambda (parent)
                                          (when (directory-name-p parent)
                                            (directory-files parent nil ".*.idea"))))))
      (when (not file)
        (error "No root found"))
      (let* ((pom (directory-files (file-name-directory file) nil "pom.xml"))
             (has-pom (> (length pom) 0))
             (root (if (and has-pom lsp-intellij-use-topmost-root)
                       (lsp-intellij--root-top-down-recurring file '("pom.xml"))
                     file)))
        (file-name-directory root)))))

(defun lsp-intellij--jar-root-dir (jar-path)
  "Return the root directory to extract files from the .jar at JAR-PATH to.

This is the temporary directory plus 'lsp-intellij' followed by the .jar file's
basename."
  (let* ((jar-filename (file-name-base jar-path))
         (root-dir (expand-file-name (concat "lsp-intellij/" jar-filename) temporary-file-directory)))
    root-dir))

(defun lsp-intellij--make-jar-temp-path (jar-path internal-path)
  "Return a temporary path for the file in the jar at JAR-PATH, INTERNAL-PATH, to be extracted to."
  (let* ((root-dir (lsp-intellij--jar-root-dir jar-path))
         (internal-dir (file-name-directory internal-path))
         (temp-path (expand-file-name internal-dir root-dir)))
    temp-path))

(defun lsp-intellij--write-jar-metadata (archive-path dest)
  "Write a metadata file that points to .jar file ARCHIVE-PATH at DEST.

Used for allowing IntelliJ to find the actual .jar an extracted .jar file is contained in."
  (with-temp-buffer
    (insert (lsp--path-to-uri archive-path))
    (write-file (expand-file-name "jarpath" dest) nil)))

(defun lsp-intellij--extract-archive-file (source-archive internal-path dest)
  "Extracts the file inside a .jar SOURCE-ARCHIVE at INTERNAL-PATH to DEST."
  (let* ((internal-dir (substring (file-name-directory internal-path) 1))
         (internal-file (file-name-nondirectory internal-path))
         (internal-name (file-name-sans-extension internal-file))
         (search-string (concat internal-dir internal-name))
         (jarpath-dest (lsp-intellij--jar-root-dir source-archive)))
    (save-window-excursion
      (find-file source-archive)
      (let ((archive-buffer (current-buffer)))
        (goto-char (point-min))
        (re-search-forward search-string)
        (archive-extract)
        (let ((extract-buffer (current-buffer))
              (outpath (expand-file-name (file-name-nondirectory (buffer-file-name)) dest)))
          (mkdir (file-name-directory outpath) t)
          (lsp-intellij--write-jar-metadata source-archive jarpath-dest)
          (write-file outpath nil)
          (kill-buffer archive-buffer)
          (kill-buffer extract-buffer)
          outpath)))))

(defconst lsp-intellij--file-extracted-from-jar-regex
  "\\\.\\(java\\|kt\\|scala\\|xml\\|MF\\)$")

(defun lsp-intellij--extracted-file-exists (basename temp-path)
  "Test if a file containing BASENAME at TEMP-PATH is from a .jar.

Used for finding the corresponding .java/.kt file from a .jar's .class file.
Return the file path if found, nil otherwise."
  (when-let
      ((filename
        (and (file-exists-p temp-path)
             (cl-find-if (lambda (s) (and (equal (file-name-sans-extension s) basename)
                                          (string-match-p lsp-intellij--file-extracted-from-jar-regex s)))
                         (directory-files temp-path)))))
    (expand-file-name filename temp-path)))

(defun lsp-intellij--visit-jar-uri (uri)
  "Visit a URI with the jar:// protocol by extracting the file from the .jar and visiting it."
  (let* ((url (url-generic-parse-url uri))
         (drive-letter (url-host url))
         (raw (url-filename url))
         (paths (split-string raw "!/"))
         (jar-path (if (and (not drive-letter) (string= "" drive-letter)) (concat drive-letter ":" (car paths)) (car paths)))
         (internal-path (cadr paths))
         (temp-path (lsp-intellij--make-jar-temp-path jar-path internal-path))
         (is-source-file (string-match-p lsp-intellij--file-extracted-from-jar-regex internal-path))
         ;; For now, ask the user if we should still visit if no sources are found.
         ;; In the future we could request IntelliJ to give us the decompiled .class file source.
         (should-visit (or is-source-file
                           (yes-or-no-p (format "%s doesn't seem to be a source file. Visit anyway?"
                                                internal-path)))))

    (if (and should-visit (file-exists-p jar-path))
        (if-let ((existing-file
                  (lsp-intellij--extracted-file-exists (file-name-base internal-path) temp-path)))
            existing-file
          (lsp-intellij--extract-archive-file jar-path internal-path temp-path))
      (error "No sources found for file in JAR: %s" uri))))

(defun lsp-intellij--project-run-configurations ()
  "Get the list of project run configurations."
  (lsp--send-request (lsp--make-request
                      "idea/runConfigurations"
                      (lsp--text-document-position-params))))

(defun lsp-intellij--run-config-to-name (config)
  "Provide a displayable name for a CONFIG returned from idea/runConfigurations."
  (format "[%s] %s" (gethash "configType" config) (gethash "name" config)))

(defun lsp-intellij--choose-run-configuration ()
  "Prompt the user to choose a run configuration."
  (when-let ((configs (lsp-intellij--project-run-configurations)))
    (let* ((display-names (mapcar #'lsp-intellij--run-config-to-name configs))
           (completions (mapcar* #'cons display-names configs))
           (chosen (cdr (assoc
                         (completing-read "Run configuration: " completions)
                         completions))))
      chosen)))

(defun lsp-intellij-run-project ()
  "Run a project using an IntelliJ run configuration."
  (interactive)
  (save-some-buffers t nil)
  (if-let ((config (lsp-intellij--choose-run-configuration)))
      (lsp-intellij--do-run-project config)
     (message "No run configurations were found.")))

(defun lsp-intellij--get-run-command (config)
  "Get the run command for a given IntelliJ run configuration CONFIG."
  (lsp--send-request
   (lsp--make-request
    "idea/runProject"
    (list :textDocument (lsp-text-document-identifier)
          :id (gethash "id" config)))))

(defun lsp-intellij--do-run-project (config)
  "Request the command line for CONFIG and run it."
  (let ((command (lsp-intellij--get-run-command config)))
         (cond
          ((or (not command) (not (gethash "command" command)))
           (error "Run configuration unsupported: %s" (gethash "name" config)))

          ((not (gethash "isUpToDate" command))
           (progn
             (setq lsp-intellij--run-after-build-command command)
             (lsp-intellij--do-build-project config)))

          (t (lsp-intellij--run-project-command command)))))

(defun lsp-intellij--run-project-command (command)
  "Run a command line COMMAND from idea/runProject in a compilation buffer."
  (let ((default-directory (gethash "workingDirectory" command))
        (command-str (replace-regexp-in-string "\n" " "
                                               (gethash "command" command))))

    (let ((process-environment
           (lsp-intellij--prepare-process-environment command)))
      (compile command-str))))

(defun lsp-intellij--prepare-process-environment (command)
  "Create a process environment with classpath for running COMMAND."
  (let* ((vars (gethash "environment" command))
        (prepared-vars (mapcar (lambda (v)
                                 (format "%s=%s"
                                         (gethash "name" v)
                                         (gethash "value" v)))
                               vars))
        (env-with-classpath
         (cons (concat "CLASSPATH=" (gethash "classpath" command))
               process-environment)))
    (append prepared-vars env-with-classpath)))

(defvar lsp-intellij--run-after-build-command nil
  "The run configuration to run after the current build finishes.")

(defun lsp-intellij-build-project ()
  "Start building a project using an IntelliJ run configuration."
  (interactive)
  (save-some-buffers t nil)
  (if-let ((config (lsp-intellij--choose-run-configuration)))
      (lsp-intellij--do-build-project config)
    (message "No run configurations were found.")))

(defun lsp-intellij--do-build-project (config)
  "Request the server to start a project build using CONFIG."
  (let ((buffer (get-buffer-create "*lsp-intellij-build-output*"))
        (command (lsp--send-request
                  (lsp--make-request
                   "idea/buildProject"
                   (list :textDocument (lsp-text-document-identifier)
                         :id (gethash "id" config)
                         :forceMakeProject nil
                         :ignoreErrors nil)))))
    (with-current-buffer buffer
      (erase-buffer))
    (if (not (gethash "started" command))
        (error "Build failed to start")
      (progn
        (lsp-intellij--set-progress-state "building" t)
        (message "Build started.")))))

(defun lsp-intellij-run-at-point ()
  "Run the item (main class, unit test) at point."
  (interactive)
  (if-let ((lens (lsp-intellij--most-local-code-lens)))
      (lsp-intellij--run-project-from-code-lens lens)
    (user-error "No configurations at point")))

(defun lsp-intellij-run-buffer-class ()
  "Run the configuration for the buffer's class.

This will run all tests if the class is a test class."
  (interactive)
  (if-let ((lens (lsp-intellij--run-buffer-lens)))
      (lsp-intellij--run-project-from-code-lens lens)
    (user-error "No configurations for running buffer found")))

(defun lsp-intellij--run-project-from-code-lens (code-lens)
  "Obtain the run configuration from CODE-LENS and run it."
  (when code-lens
    (let* ((data (gethash "data" code-lens))
           (config (gethash "configuration" data)))
      (lsp-intellij--do-run-project config))))

(defun lsp-intellij--run-buffer-lens ()
  "Find a code lens used for running a class.

Return the code lens if found, nil otherwise."
  (cl-find-if (lambda (lens)
             (let* ((data (gethash "data" lens))
                    (state (gethash "state" data)))
               (= state 1))) ;; RunClass
           lsp-code-lenses))

(defun lsp-intellij--min-by (f coll)
  "Return the x for which (F x), a number or marker, is least in COLL."
  (when (listp coll)
    (cl-reduce (lambda (min this)
                (if (> (funcall f min) (funcall f this)) this min))
            coll)))

(defun lsp-intellij--code-lenses-at-point ()
  "Get the code lenses under the current point."
  (seq-filter (lambda (lens)
                (let* ((range (gethash "range" lens))
                       (start (lsp--position-to-point (gethash "start" range)))
                       (end (lsp--position-to-point (gethash "end" range))))
                  (lsp--point-is-within-bounds-p start end)))
              lsp-code-lenses))

(defun lsp-intellij--most-local-code-lens ()
  "Find the code lens with the smallest size at point."
  (when-let ((lenses (lsp-intellij--code-lenses-at-point)))
    (lsp-intellij--min-by (lambda (lens)
                            (let* ((range (gethash "range" lens))
                                   (start (lsp--position-to-point (gethash "start" range)))
                                   (end (lsp--position-to-point (gethash "end" range))))
                              (- end start)))
                          lenses)))

(defvar lsp-intellij--code-lens-overlays (make-hash-table :test 'eq))

(defun lsp-intellij--remove-cur-code-lens-overlays ()
  "Remove the code lens overlays for the current buffer."
  (let ((overlays lsp-intellij--code-lens-overlays)
        (buf (current-buffer)))
    (dolist (overlay (gethash buf overlays))
      (delete-overlay overlay))
    (remhash buf overlays)))

(defconst lsp-intellij--code-lens-kind-face
  '((0 . lsp-intellij-face-code-lens-run)
    (2 . lsp-intellij-face-code-lens-test)
    (3 . lsp-intellij-face-code-lens-test-pass)
    (4 . lsp-intellij-face-code-lens-test-fail)
    (5 . lsp-intellij-face-code-lens-test-unknown)))

(defun lsp-intellij--render-code-lenses (lenses)
  "Create a callback to process a code lenses response LENSES."
  (let ((buf (current-buffer)))
    (cl-check-type buf buffer)
    (with-current-buffer buf
      (lsp-intellij--remove-cur-code-lens-overlays)
      (when (and lenses (/= (length lenses) 0))
        (let* ((overlays lsp-intellij--code-lens-overlays)
               (buf-overlays (gethash (current-buffer) overlays)))
          (save-restriction
            (widen)
            (dolist (lens lenses)
              (let* ((range (gethash "range" lens nil))
                     (data (gethash "data" lens))
                     (state (gethash "state" data 0))
                     (start (gethash "start" range))
                     (end (gethash "end" range))
                     overlay)
                (when (not (= state 1)) ;; not RunClass
                  (setq overlay (make-overlay (lsp--position-to-point start)
                                              (lsp--position-to-point end)))
                  (overlay-put overlay 'face
                               (cdr (assq state lsp-intellij--code-lens-kind-face)))
                  (push overlay buf-overlays)
                  (puthash (current-buffer) buf-overlays overlays))))))))))

(defun lsp-intellij--on-build-messages (_workspace params)
  "Displays the build messages in PARAMS in a dedicated buffer."
  (let ((buffer (get-buffer-create "*lsp-intellij-build-output*")))
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
        (mapc (lambda (mes)
                (let ((path (if (string-blank-p (gethash "uri" mes))
                                "<unknown>"
                              (lsp--uri-to-path (gethash "uri" mes))))
                      (diags (gethash "diagnostics" mes)))
                  (lsp-intellij--insert-build-messages diags path)))
              params))
      (fundamental-mode)
      (compilation-shell-minor-mode))))

(defun lsp-intellij--insert-build-messages (diags path)
  "Insert diagnostics DIAGS for PATH from IntelliJ's builder."
  (mapc (lambda (d) (lsp-intellij--insert-build-message (lsp--make-diag d) path)) diags))

(defun lsp-intellij--insert-build-message (diag path)
  "Insert the diagnostic DIAG for PATH formatted so error jumping works."
  (let ((line (lsp-diagnostic-line diag))
        (column (lsp-diagnostic-column diag))
        (severity
         (pcase (lsp-diagnostic-severity diag)
           (1 'error)
           (2 'warning)
           (_ 'info)))
        (message (lsp-diagnostic-message diag))
        (source (lsp-diagnostic-source diag)))
    (goto-char (point-max))
    ;; use GCC's line format
    (insert (format "%s:%s:%s: %s: %s\n" path line column severity message))))

(defun lsp-intellij--on-build-finished (_workspace params)
  "Handle the finished build status in PARAMS.

If the build finished successfully and a run command for the
build was set, run it. If the build failed, display the list of
messages from the builder received from idea/buildMessages
notifications."
  (lsp-intellij--set-progress-state "building" nil)
  (let ((errors (gethash "errors" params))
        (warnings (gethash "warnings" params))
        (is-aborted (gethash "isAborted" params))
        (command lsp-intellij--run-after-build-command))
    (setq lsp-intellij--run-after-build-command nil)
    (cond
     ((> errors 0)
      (progn
        (message "Build failed with %s errors and %s warnings." errors warnings)
        (pop-to-buffer (get-buffer-create "*lsp-intellij-build-output*") 'other-window)
        (goto-char (point-min))))

     (is-aborted (message "Build was aborted."))

     (t (progn
          (message "Build finished with %s warnings." warnings)
          (when command
            (lsp-intellij--run-project-command command)))))))

(defun lsp-intellij-open-project-structure ()
  "Open the Project Structure dialog for the current project."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command "openProjectStructure" nil))

(defun lsp-intellij-open-run-configurations ()
  "Open the Run/Debug Configurations dialog for the current project."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command "openRunConfigurations" nil))

(defun lsp-intellij-toggle-frame-visibility ()
  "Toggle visibility of the current project's frame."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command "toggleFrameVisibility" nil))

(defun lsp-intellij--render-string (string mode)
  "Render STRING using the font lock for MODE."
  (condition-case nil
      (with-temp-buffer
        (delay-mode-hooks (funcall mode))
        (insert string)
        (font-lock-ensure)
        (buffer-string))
    (error string)))

(defconst lsp-intellij-dummy-executable
  (if (eq system-type 'windows-nt)
      '("cmd")
    '("sh"))
  "Program that lsp-mode will open when initializing lsp-intellij.

lsp-mode requires a process to be opened when starting a server over
TCP, even if it isn't the one being communicated with.")

(defconst lsp-intellij--notification-handlers
  '(("idea/indexStarted" .
     (lambda (_w _p)
       (message "Indexing started.")
       (lsp-intellij--set-progress-state "indexing" t)))
    ("idea/indexFinished" .
     (lambda (_w _p)
       (message "Indexing finished.")
       (lsp-intellij--set-progress-state "indexing" nil)
       (lsp--update-code-lenses 'lsp-intellij--render-code-lenses)))
    ("idea/buildFinished" .
     (lambda (w p)
       (lsp-intellij--on-build-finished w p)))
    ("idea/buildMessages" .
     (lambda (w p)
       (lsp-intellij--on-build-messages w p)))))

(defun lsp-intellij--refresh-status (status)
  "Set the lsp mode line display using the hash map STATUS."
  (if (hash-table-empty-p status)
      (setq lsp-status "")
    (let ((result))
      (maphash
       (lambda (k v)
         (when v
           (setq result (if result (concat result " " k) k))))
       status)
      (setq lsp-status (format "(%s)" result)))))

(defun lsp-intellij--set-progress-state (key value)
  "Show or hide the string KEY on the mode line with VALUE.

If VALUE is non-nil, update the lsp mode line display to show the name of the
status. If VALUE is nil, remove the status from the display."
  (puthash key value lsp-intellij--progress-state)
  (lsp-intellij--refresh-status lsp-intellij--progress-state))

(defconst lsp-intellij--request-handlers
  '(("idea/temporaryDirectory" .
     (lambda (_w _p)
       (list :directory (lsp--path-to-uri temporary-file-directory))))))

(defun lsp-intellij--initialize-client (client)
  "Initialize CLIENT with the required `lsp-mode' handlers."
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p)))
          lsp-intellij--notification-handlers)
  (mapcar #'(lambda (p) (lsp-client-on-request client (car p) (cdr p)))
          lsp-intellij--request-handlers)

  ;; Emacs strips out the \r in \r\n by default, even with lsp-mode,
  ;; so the proper coding system needs to be set to capture the \r\n.
  (setq-local default-process-coding-system (cons 'utf-8 'utf-8))
  (setq-local coding-system-for-read 'binary)
  (setq-local coding-system-for-write 'binary)
  ;; Ensure the client uses the server's sync method
  (setq-local lsp-document-sync-method nil)

  (lsp-provide-marked-string-renderer client "java" (lambda (s) (lsp-intellij--render-string s 'java-mode)))
  (lsp-provide-marked-string-renderer client "kotlin" (lambda (s) (lsp-intellij--render-string s 'kotlin-mode)))
  (lsp-client-register-uri-handler client "jar" 'lsp-intellij--visit-jar-uri))

(defun lsp-intellij-server-start-fun (port)
  `(,lsp-intellij-dummy-executable))

(flet ((lsp--find-available-port (host port) 8080))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tcp-connection
                                     (lambda (port) 'lsp-intellij-server-start-fun))
                    :major-modes '(java-mode)
                    :priority -1
                    :server-id 'intellij
                    :initialized-fn #'lsp-intellij--initialize-client)))


(defun lsp-intellij--set-configuration ()
  "Set the lsp configuration from the current map of config options."
  (lsp--set-configuration `(:intellij ,lsp-intellij--config-options)))

(add-hook 'lsp-after-initialize-hook 'lsp-intellij--set-configuration)

(add-hook 'lsp-after-diagnostics-hook (lambda () (lsp--update-code-lenses 'lsp-intellij--render-code-lenses)))


(defun lsp-intellij-set-config (option value)
  "Set a config OPTION to VALUE in the config option map."
  (puthash option value lsp-intellij--config-options))

(defun lsp-intellij-set-temporary-directory (directory)
  "Set the temporary directory for extracted .jar files to DIRECTORY."
  (lsp-intellij-set-config "temporaryDirectory" directory))

(lsp-intellij-set-temporary-directory (lsp--path-to-uri temporary-file-directory))

;;;###autoload
(defface lsp-intellij-face-code-lens-run
  '((((background dark))  :background "dark green")
    (((background light)) :background "green"))
  "Face used for areas with a run configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test
  '((((background dark))  :background "saddle brown")
    (((background light)) :background "yellow"))
  "Face used for areas with a test configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test-pass
  '((((background dark))  :background "sea green")
    (((background light)) :background "green"))
  "Face used for areas with a passing test configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test-fail
  '((((background dark))  :background "firebrick")
    (((background light)) :background "red"))
  "Face used for areas with a failing test configuration."
  :group 'lsp-intellij-faces)

;;;###autoload
(defface lsp-intellij-face-code-lens-test-unknown
  '((((background dark))  :background "saddle brown")
    (((background light)) :background "yellow"))
  "Face used for areas with a test configuration an with unknown state."
  :group 'lsp-intellij-faces)

(provide 'lsp-intellij)
;;; lsp-intellij.el ends here
