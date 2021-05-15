;;; lsp-python-ms.el --- The lsp-mode client for Microsoft python-language-server -*- lexical-binding: t -*-

;; Author: Charl Botha
;; Maintainer: Andrew Christianson, Vincent Zhang
;; Version: 0.7.2
;; Package-Version: 20210513.1019
;; Package-Commit: 5856c08d6393c10951f39b993a8d7bf2d506b44f
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.1"))
;; Homepage: https://github.com/emacs-lsp/lsp-python-ms
;; Keywords: languages tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;;; Code:
(require 'cl-lib)
(require 'conda nil 'noerror)
(require 'find-file-in-project nil 'noerror)
(require 'json)
(require 'lsp-mode)
(require 'projectile nil 'noerror)
(require 'project nil 'noerror)


;; Forward declare functions
(declare-function conda-env-name-to-dir "ext:conda")
(declare-function ffip-get-project-root-directory "ext:find-file-in-project")

;; Forward declare variable
(defvar conda-env-executables-dir)
(defvar lsp-render-markdown-markup-content)

;; Group declaration
(defgroup lsp-python-ms nil
  "LSP support for python using the Microsoft Python Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/Microsoft/python-language-server"))

(defcustom lsp-python-ms-dir (f-join lsp-server-install-dir "mspyls/")
  "The directory of the Microsoft Python Language Server."
  :type 'directory
  :group 'lsp-python-ms)

;; not used since ms-pyls 0.2.92+
;; see https://github.com/microsoft/vscode-python/blob/master/src/client/activation/languageServer/analysisOptions.ts#L93
;; (defcustom lsp-python-ms-cache-dir
;;   (directory-file-name (locate-user-emacs-file ".lsp-python/"))
;;   "Path to directory where the server will write cache files.

;; If this is nil, the language server will write cache files in a directory
;; sibling to the root of every project you visit")

(defcustom lsp-python-ms-guess-env t
  "Should the language server guess the paths.

If true, check for pyenv environment/version files, then conda
environment files, then project-local virtual environments, then
fall back to the python on the head of PATH.  Otherwise, just use
the python on the head of PATH."
  :type 'boolean
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-python-executable nil
  "Path to specify the Python executable for the Microsoft Python Language Server."
  :type '(choice (const :tag "None" nil) file)
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-extra-paths []
  "A list of additional paths to search for python packages.

This should be a list of paths corresponding to additional python
library directories you want to search for completions.  Paths
should be as they are (or would appear) in sys.path.  Paths will
be prepended to the search path, and so will shadow duplicate
names in search paths returned by the interpreter."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)
(make-variable-buffer-local 'lsp-python-ms-extra-paths)

(defcustom lsp-python-ms-python-executable-cmd "python"
  "Command to specify the Python command for the Microsoft Python Language Server.

Similar to the `python-shell-interpreter', but used only with mspyls.
Useful when there are multiple python versions in system.
e.g, there are `python2' and `python3', both in system PATH,
and the default `python' links to python2,
set as `python3' to let ms-pyls use python 3 environments."
  :type 'string
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-prefer-remote-env t
  "If Non-nil, will prefer remote python environment.
Only available in Emacs 27 and above."
  :type 'boolean
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-executable (concat lsp-python-ms-dir
                                            "Microsoft.Python.LanguageServer"
                                            (if (eq system-type 'windows-nt) ".exe" ""))
  "Path to the Microsoft Python LanguageServer binary."
  :type '(file :must-match t)
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-auto-install-server t
  "Install Microsoft Python Language Server automatically."
  :type 'boolean
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-nupkg-channel "stable"
  "The channel of nupkg for the Microsoft Python Language Server:
stable, beta or daily."
  :type 'string
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-completion-add-brackets t
  "Whether to add brackets after completion of functions."
  :type 'boolean
  :group 'lsp-python-ms)

;; See https://github.com/microsoft/python-language-server/blob/master/src/Analysis/Ast/Impl/Definitions/AnalysisOptions.cs
(defcustom lsp-python-ms-cache "None"
  "The cache level of analysis for Microsoft Python Language Server."
  :type '(choice
          (const "None")
          (const "System")
          (const "Library"))
  :group 'lsp-python-ms)

;; See https://github.com/microsoft/python-language-server for more diagnostics
(defcustom lsp-python-ms-errors ["unknown-parameter-name"
                                 "undefined-variable"
                                 "parameter-missing"
                                 "positional-argument-after-keyword"
                                 "too-many-function-arguments"]
  "Microsoft Python Language Server Error types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-warnings ["unresolved-import"
                                   "parameter-already-specified"
                                   "too-many-positional-arguments-before-star"]
  "Microsoft Python Language Server Warning types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-information []
  "Microsoft Python Language Server Information types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-disabled []
  "Microsoft Python Language Server Disabled types."
  :type 'lsp-string-vector
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-parse-dot-env-enabled t
  "Automatically parse .env file in the project root if non-nil."
  :type 'boolean
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-base-url "https://pvsc.blob.core.windows.net"
  "The base url to get nupkg package.
The alternative is `https://pvsc.azureedge.net'."
  :type 'string
  :group 'lsp-python-ms)

(defcustom lsp-python-ms-log-level "Error"
  "Log Level definition."
  :type 'string
  :group 'lsp-python-ms
  :options (list "Trace"
                 "Info"
                 "Information"
                 "Error"
                 "Warning"))

(defcustom lsp-python-ms-extra-major-modes '()
  "A list of additional major modes in which to activate.

In addition to the `python-mode', you may wish the Microsoft Python
Language Server to activate in other major modes.  If so, list them
here."
  :type 'list
  :group 'lsp-python-ms)

(defun lsp-python-ms-latest-nupkg-url (&optional channel)
  "Get the nupkg url through CHANNEL from Microsoft Python Language Server."
  (let ((channel (or channel "stable")))
    (unless (member channel '("stable" "beta" "daily"))
      (user-error "Unknown channel: %s" channel))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "%s/python-language-server-%s?restype=container&comp=list&prefix=Python-Language-Server-%s-x64"
                 lsp-python-ms-base-url
                 channel
                 (cond ((eq system-type 'darwin)  "osx")
                       ((eq system-type 'gnu/linux) "linux")
                       ((eq system-type 'windows-nt) "win")
                       (t (user-error "Unsupported system: %s" system-type)))))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (pcase (xml-parse-region (point) (point-max))
        (`((EnumerationResults
            ((ContainerName . ,_))
            (Prefix nil ,_)
            (Blobs nil . ,blobs)
            (NextMarker nil)))
         (cdar
          (sort
           (mapcar (lambda (blob)
                     (pcase blob
                       (`(Blob
                          nil
                          (Name nil ,_)
                          (Url nil ,url)
                          (Properties nil (Last-Modified nil ,last-modified) . ,_))
                        (cons (apply #'encode-time (parse-time-string last-modified)) url))))
                   blobs)
           (lambda (t1 t2)
             (time-less-p (car t2) (car t1))))))))))

(defun lsp-python-ms--install-server (_client callback error-callback update?)
  "Downloading Microsoft Python Language Server to the specified path."
  (unless (and (not update?)
               (f-exists? lsp-python-ms-executable))
    (let* ((temp-file (make-temp-file "mspyls" nil ".zip"))
           (install-dir (expand-file-name lsp-python-ms-dir))
           (unzip-script (cond ((executable-find "unzip")
                                (format "mkdir -p %s && unzip -qq %s -d %s"
                                        install-dir temp-file install-dir))
                               ((executable-find "powershell")
                                (format "powershell -noprofile -noninteractive \
  -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'" temp-file install-dir))
                               (t (user-error "Unable to extract '%s' to '%s'! \
  Please check unzip, powershell or extract manually." temp-file install-dir)))))

      (lsp--info "Downloading Microsoft Python Language Server...")

      (url-retrieve
       (lsp-python-ms-latest-nupkg-url lsp-python-ms-nupkg-channel)
       (lambda (_data)
         ;; Skip http header
         (re-search-forward "\r?\n\r?\n")

         ;; Save to the temp file
         (let ((coding-system-for-write 'binary))
           (write-region (point) (point-max) temp-file))

         (lsp--info "Downloading Microsoft Python Language Server...done")

         ;; Extract the archive
         (lsp--info "Extracting Microsoft Python Language Server...")
         (f-delete install-dir t)

         (lsp-async-start-process
          (lambda ()
            (when (f-exists? lsp-python-ms-executable)
              (lsp--info "Extracting Microsoft Python Language Server...done")
              ;; Make the binary executable
              (chmod lsp-python-ms-executable #o755)
              ;; Start LSP if need
              (and lsp-mode (lsp)))
            (funcall callback))
          error-callback
          (if (executable-find "unzip") "sh" "cmd")
          (if (executable-find "unzip") "-c" "/c")
          unzip-script))))))

;;;###autoload
(defun lsp-python-ms-update-server ()
  "Update Microsoft Python Language Server.

On Windows, if the server is running, the updating will fail.
After stopping or killing the process, retry to update."
  (interactive)
  (lsp-python-ms--install-server nil #'ignore #'lsp--error t))

(defun lsp-python-ms--venv-dir (dir)
  "Check if the directory contains a virtualenv."
  (let ((dirs (and dir (f-directories dir))))
    (car (seq-filter #'lsp-python-ms--venv-python dirs))))

(defun lsp-python-ms--venv-python (dir)
  "Check if a directory is a virtualenv."
  (let* ((python? (and t (f-expand "bin/python" dir)))
         (python3? (and python? (f-expand "bin/python3" dir)))
         (python (and python3?
                      (cond ((f-executable? python?) python?)
                            ((f-executable? python3?) python3?)
                            (t nil))))
         (not-system (and python
                          (let ((dir-parent (f-parent dir)))
                            (not (or (string-equal dir-parent (expand-file-name "~"))
                                     (string-equal dir-parent "/")))))))
    (and not-system python)))

(defun lsp-python-ms--dominating-venv-python (&optional dir)
  "Look for directories that look like venvs."
  (when-let ((dominating-venv
              (or (locate-dominating-file (or dir default-directory) #'lsp-python-ms--venv-python)
                  (lsp-python-ms--venv-dir (locate-dominating-file (or dir default-directory) #'lsp-python-ms--venv-dir)))))
    (lsp-python-ms--venv-python dominating-venv)))

(defun lsp-python-ms--dominating-conda-python (&optional dir)
  "Locate dominating conda environment."
  (let* ((path (or dir default-directory))
         (yamls (and path
                     '("environment.yml" "environment.yaml"
                       "env.yml" "env.yaml" "dev-environment.yml"
                       "dev-environment.yaml")))
         (dominating-yaml (and yamls
                               (seq-map (lambda (file)
                                          (when (locate-dominating-file path file)
                                            (expand-file-name file
                                                              (locate-dominating-file path file))))
                                        yamls)))
         (dominating-yaml-file (and dominating-yaml
                                    (car (seq-filter
                                          (lambda (file) file) dominating-yaml))))
         (dominating-conda-name (and dominating-yaml-file
                                     (fboundp 'conda--get-name-from-env-yml)
                                     (or (bound-and-true-p conda-env-current-name)
                                         (conda--get-name-from-env-yml dominating-yaml-file)))))
    (when dominating-conda-name
      (expand-file-name
       (file-name-nondirectory lsp-python-ms-python-executable-cmd)
       (expand-file-name conda-env-executables-dir
                         (conda-env-name-to-dir dominating-conda-name))))))

(defun lsp-python-ms--dominating-pyenv-python (&optional dir)
  "Locate dominating pyenv-managed python."
  (let ((dir (or dir default-directory)))
    (when (locate-dominating-file dir ".python-version")
      (string-trim (shell-command-to-string "pyenv which python")))))

(defun lsp-python-ms--dominating-asdf-python (&optional dir)
  "Locate dominating asdf-managed python"
  (let ((dir (or dir default-directory)))
    (when (locate-dominating-file dir ".tool-versions")
      (string-trim (shell-command-to-string "asdf which python")))))

(defun lsp-python-ms--valid-python (path)
  (and path (f-executable? path) path))

(defun lsp-python-ms-locate-python (&optional dir)
  "Look for virtual environments local to the workspace."
  (let* ((pyenv-python (lsp-python-ms--dominating-pyenv-python dir))
         (asdf-python (lsp-python-ms--dominating-asdf-python dir))
         (venv-python (lsp-python-ms--dominating-venv-python dir))
         (conda-python (lsp-python-ms--dominating-conda-python dir))
         (sys-python
          (with-no-warnings
            (if (>= emacs-major-version 27)
                (executable-find lsp-python-ms-python-executable-cmd lsp-python-ms-prefer-remote-env)
              ;; This complains in Windows' Emacs 26.1, see #141
              (executable-find lsp-python-ms-python-executable-cmd)))))
    ;; pythons by preference: local pyenv version, local conda version

    (if lsp-python-ms-guess-env
        (cond ((lsp-python-ms--valid-python lsp-python-ms-python-executable))
              ((lsp-python-ms--valid-python venv-python))
              ((lsp-python-ms--valid-python asdf-python))
              ((lsp-python-ms--valid-python pyenv-python))
              ((lsp-python-ms--valid-python conda-python))
              ((lsp-python-ms--valid-python sys-python)))
      (cond ((lsp-python-ms--valid-python sys-python))))))

;; it's crucial that we send the correct Python version to MS PYLS,
;; else it returns no docs in many cases furthermore, we send the
;; current Python's (can be virtualenv) sys.path as searchPaths
(defun lsp-python-ms--get-python-ver-and-syspath (&optional workspace-root)
  "Return list with pyver-string and list of python search paths.

The WORKSPACE-ROOT will be prepended to the list of python search
paths and then the entire list will be json-encoded."
  (let* ((python (and t (lsp-python-ms-locate-python)))
         (workspace-root (and python (or workspace-root ".")))
         (default-directory (and workspace-root workspace-root))
         (init (and default-directory
                    "from __future__ import print_function; import sys; sys.path = list(filter(lambda p: p != '', sys.path)); import json;"))
         (ver (and init "v=(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));"))
         (sp (and ver (concat "sys.path.insert(0, '" workspace-root "'); p=sys.path;")))
         (ex (and sp "e=sys.executable;"))
         (val (and ex "print(json.dumps({\"version\":v,\"paths\":p,\"executable\":e}))")))
    (when val
      (with-temp-buffer
        (call-process python nil t nil "-c"
                      (concat init ver sp ex val))
        (let* ((json-array-type 'vector)
               (json-key-type 'string)
               (json-object-type 'hash-table)
               (json-string (buffer-string))
               (json-hash (json-read-from-string json-string)))
          (list
           (gethash "version" json-hash)
           (gethash "paths" json-hash)
           (gethash "executable" json-hash)))))))

(defun lsp-python-ms--workspace-root ()
  "Get the path of the root of the current workspace.

Use `lsp-workspace-root', which is pressent in the \"new\"
lsp-mode and works when there's an active session.  Next try ffip
or projectile, or just return `default-directory'."
  (cond
   ((fboundp #'lsp-workspace-root) (lsp-workspace-root))
   ((fboundp #'ffip-get-project-root-directory) (ffip-get-project-root-directory))
   ((fboundp #'projectile-project-root) (projectile-project-root))
   ((fboundp #'project-current) (when-let ((project (project-current)))
                                  (car (or (and (fboundp 'project-root) (project-root project))
                                           ;; Function `project-roots' is obsolete, by having
                                           ;; just to make compatible to older `project.el' package.
                                           (with-no-warnings (project-roots project))))))
   (t default-directory)))

;; I based most of this on the vs.code implementation:
;; https://github.com/microsoft/vscode-python/blob/master/src/client/activation/languageServer/analysisOptions.ts
;; (it still took quite a while to get right, but here we are!)
(defun lsp-python-ms--extra-init-params (&optional workspace)
  "Return form describing parameters for language server.

Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
  (let ((workspace-root (or (if workspace
                                (lsp--workspace-root workspace)
                              (lsp-python-ms--workspace-root))
                            default-directory)))
    (when lsp-python-ms-parse-dot-env-enabled
      (lsp-python-ms--parse-dot-env workspace-root))
    (cl-destructuring-bind (pyver pysyspath pyintpath)
      (lsp-python-ms--get-python-ver-and-syspath workspace-root)
      `(:interpreter
        (:properties
         (:InterpreterPath ,pyintpath :UseDefaultDatabase t :Version ,pyver))
        ;; preferredFormat "markdown" or "plaintext"
        ;; experiment to find what works best -- over here mostly plaintext
        :displayOptions (:preferredFormat
                         "markdown"
                         :trimDocumentationLines :json-false
                         :maxDocumentationLineLength 0
                         :trimDocumentationText :json-false
                         :maxDocumentationTextLength 0)
        :searchPaths ,(vconcat lsp-python-ms-extra-paths pysyspath)
        :analysisUpdates t
        :asyncStartup t
        :logLevel ,lsp-python-ms-log-level
        :typeStubSearchPaths ,(vector (expand-file-name (f-join lsp-python-ms-dir "Typeshed")))))))

(defun lsp-python-ms--filter-nbsp (str)
  "Filter nbsp entities from STR."
  (let ((rx "&nbsp;"))
    (when (eq system-type 'windows-nt)
      (setq rx (concat rx "\\|\r")))
    (when str
      (replace-regexp-in-string rx " " str))))

(defun lsp-python-ms--parse-dot-env (root &optional envvar)
  "Set environment variable (default PYTHONPATH) from .env file if this file exists in the project root."
  (let* ((envvar (or envvar "PYTHONPATH"))
         (file (f-join (file-name-as-directory root) ".env"))
         (rx (concat "^[:blank:]*" envvar "[:blank:]*=[:blank:]*"))
         val)
    (when (and (f-exists? file) (f-file? file) (f-readable? file))
      (with-temp-buffer
        (insert-file-contents file)
        (keep-lines rx (point-min) (point-max))
        (when (string-match (concat rx "\\(.*\\)") (buffer-string))
          (setq val (match-string 1 (buffer-string)))
          (unless (string-empty-p val)
            (setenv envvar val)))))))

(defun lsp-python-ms--language-server-started-callback (_workspace _params)
  "Handle the python/languageServerStarted message.

WORKSPACE is just used for logging and _PARAMS is unused."
   (lsp--info "Microsoft Python language server started"))

;; this gets called when we do lsp-describe-thing-at-point
;; see lsp-methods.el. As always, remove Microsoft's unwanted entities :(
(setq lsp-render-markdown-markup-content #'lsp-python-ms--filter-nbsp)

;; lsp-ui-doc--extract gets called when hover docs are requested
;; as always, we have to remove Microsoft's unnecessary &nbsp; entities
(advice-add 'lsp-ui-doc--extract
            :filter-return #'lsp-python-ms--filter-nbsp)

;; lsp-ui-sideline--format-info gets called when lsp-ui wants to show
;; hover info in the sideline again &nbsp; has to be removed
(advice-add 'lsp-ui-sideline--format-info
            :filter-return #'lsp-python-ms--filter-nbsp)

(defun lsp-python-ms--report-progress-callback (_workspace params)
  "Log progress information."
  (when (and (arrayp params) (> (length params) 0))
    (lsp-log (aref params 0))))

(defun lsp-python-ms--begin-progress-callback (workspace &rest _)
  (with-lsp-workspace workspace
    (--each (lsp--workspace-buffers workspace)
      (when (buffer-live-p it)
        (with-current-buffer it
          (lsp--spinner-start)))))
  (lsp--info "Microsoft Python language server is analyzing..."))

(defun lsp-python-ms--end-progress-callback (workspace &rest _)
  (with-lsp-workspace workspace
    (--each (lsp--workspace-buffers workspace)
      (when (buffer-live-p it)
        (with-current-buffer it
          (lsp--spinner-stop))))
    (lsp--info "Microsoft Python language server is analyzing...done")))

(lsp-register-custom-settings
 `(("python.autoComplete.addBrackets" lsp-python-ms-completion-add-brackets t)
   ("python.analysis.cachingLevel" lsp-python-ms-cache)
   ("python.analysis.errors" lsp-python-ms-errors)
   ("python.analysis.warnings" lsp-python-ms-warnings)
   ("python.analysis.information" lsp-python-ms-information)
   ("python.analysis.disabled" lsp-python-ms-disabled)
   ("python.analysis.autoSearchPaths" (lambda () (<= (length lsp-python-ms-extra-paths) 0)) t)
   ("python.autoComplete.extraPaths" lsp-python-ms-extra-paths)))

(dolist (mode lsp-python-ms-extra-major-modes)
  (add-to-list 'lsp-language-id-configuration `(,mode . "python")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-python-ms-executable)
                                        (lambda () (f-exists? lsp-python-ms-executable)))
  :major-modes (append '(python-mode) lsp-python-ms-extra-major-modes)
  :server-id 'mspyls
  :priority 1
  :initialization-options 'lsp-python-ms--extra-init-params
  :notification-handlers (lsp-ht ("python/languageServerStarted" 'lsp-python-ms--language-server-started-callback)
                                 ("telemetry/event" 'ignore)
                                 ("python/reportProgress" 'lsp-python-ms--report-progress-callback)
                                 ("python/beginProgress" 'lsp-python-ms--begin-progress-callback)
                                 ("python/endProgress" 'lsp-python-ms--end-progress-callback))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "python"))))
  :download-server-fn (lambda (client callback error-callback update?)
                        (when lsp-python-ms-auto-install-server
                          (lsp-python-ms--install-server client callback error-callback update?)))))

(provide 'lsp-python-ms)

;;; lsp-python-ms.el ends here
