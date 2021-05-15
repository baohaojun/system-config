;;; lsp-clojure.el --- Clojure Client settings -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Benedek Fazekas

;; Author: Benedek Fazekas <benedek.fazekas@gmail.com>
;; Keywords: languages,tools

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

;; lsp-clojure client

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)

(defgroup lsp-clojure nil
  "LSP support for Clojure."
  :link '(url-link "https://github.com/snoe/clojure-lsp")
  :group 'lsp-mode
  :tag "Lsp Clojure")

(define-obsolete-variable-alias 'lsp-clojure-server-command
  'lsp-clojure-custom-server-command  "lsp-mode 7.1")

(defcustom lsp-clojure-custom-server-command nil
  "The clojure-lisp server command."
  :group 'lsp-clojure
  :risky t
  :type '(repeat string))

(defcustom lsp-clojure-server-download-url
  (format "https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-%s-amd64.zip"
          (pcase system-type
            ('gnu/linux "linux")
            ('darwin "macos")
            ('windows-nt "windows")))
  "Automatic download url for lsp-clojure."
  :type 'string
  :group 'lsp-clojure
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-clojure-server-store-path
  (f-join lsp-server-install-dir
          "clojure"
          (if (eq system-type 'windows-nt)
              "clojure-lsp.exe"
            "clojure-lsp"))
  "The path to the file in which `clojure-lsp' will be stored."
  :type 'file
  :group 'lsp-clojure
  :package-version '(lsp-mode . "7.1"))

(defcustom lsp-clojure-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "LSP clojure workspace directory."
  :group 'lsp-clojure
  :risky t
  :type 'directory)

(defcustom lsp-clojure-workspace-cache-dir (expand-file-name ".cache/" lsp-clojure-workspace-dir)
  "LSP clojure workspace cache directory."
  :group 'lsp-clojure
  :risky t
  :type 'directory)

;; Internal

(lsp-dependency
 'clojure-lsp
 `(:download :url lsp-clojure-server-download-url
             :decompress :zip
             :store-path lsp-clojure-server-store-path
             :set-executable? t)
 '(:system "clojure-lsp"))

;; Refactorings

(defun lsp-clojure--execute-command (command &optional args)
  "Send an executeCommand request for COMMAND with ARGS."
  (lsp--cur-workspace-check)
  (lsp-send-execute-command command (apply #'vector args)))

(defun lsp-clojure--refactoring-call (refactor-name &rest additional-args)
  "Send an executeCommand request for REFACTOR-NAME with ADDITIONAL-ARGS.
If there are more arguments expected after the line and column numbers."
  (lsp--cur-workspace-check)
  (lsp-clojure--execute-command refactor-name (cl-list* (lsp--buffer-uri)
                                                        (- (line-number-at-pos) 1) ;; clojure-lsp expects line numbers to start at 0
                                                        (current-column)
                                                        additional-args)))

(defun lsp-clojure-add-import-to-namespace (import-name)
  "Add to IMPORT-NAME to :import form."
  (interactive "MImport name: ")
  (lsp-clojure--refactoring-call "add-import-to-namespace" import-name))

(defun lsp-clojure-add-missing-libspec ()
  "Apply add-missing-libspec refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "add-missing-libspec"))

(defun lsp-clojure-clean-ns ()
  "Apply clean-ns refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "clean-ns"))

(defun lsp-clojure-cycle-coll ()
  "Apply cycle-coll refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "cycle-coll"))

(defun lsp-clojure-cycle-privacy ()
  "Apply cycle-privacy refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "cycle-privacy"))

(defun lsp-clojure-expand-let ()
  "Apply expand-let refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "expand-let"))

(defun lsp-clojure-extract-function (function-name)
  "Move form at point into a new function named FUNCTION-NAME."
  (interactive "MFunction name: ") ;; Name of the function
  (lsp-clojure--refactoring-call "extract-function" function-name))

(defun lsp-clojure-inline-symbol ()
  "Apply inline-symbol refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "inline-symbol"))

(defun lsp-clojure-introduce-let (binding-name)
  "Move form at point into a new let binding as BINDING-NAME."
  (interactive "MBinding name: ") ;; Name of the let binding
  (lsp-clojure--refactoring-call "introduce-let" binding-name))

(defun lsp-clojure-move-to-let (binding-name)
  "Move form at point into nearest existing let binding as BINDING-NAME."
  (interactive "MBinding name: ") ;; Name of the let binding
  (lsp-clojure--refactoring-call "move-to-let" binding-name))

(defun lsp-clojure-thread-first ()
  "Apply thread-first refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-first"))

(defun lsp-clojure-thread-first-all ()
  "Apply thread-first-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-first-all"))

(defun lsp-clojure-thread-last ()
  "Apply thread-last refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-last"))

(defun lsp-clojure-thread-last-all ()
  "Apply thread-last-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "thread-last-all"))

(defun lsp-clojure-unwind-all ()
  "Apply unwind-all refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "unwind-all"))

(defun lsp-clojure-unwind-thread ()
  "Apply unwind-thread refactoring at point."
  (interactive)
  (lsp-clojure--refactoring-call "unwind-thread"))

(defun lsp-clojure-server-info ()
  "Request server info."
  (interactive)
  (lsp-clojure--execute-command "server-info"))

(defun lsp-clojure-cursor-info ()
  "Request cursor info at point."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-clojure--execute-command "cursor-info" (list (lsp--buffer-uri)
                                                    (- (line-number-at-pos) 1) ;; clojure-lsp expects line numbers to start at 0
                                                    (current-column))))

(defun lsp-clojure--ask-macro-to-resolve ()
  "Ask to user the macro to resolve."
  (lsp--completing-read
   "Select how LSP should resolve this macro:"
   '("clojure.core/def"
     "clojure.core/defn"
     "clojure.core/let"
     "clojure.core/for"
     "clojure.core/->"
     "clojure.core/->>"
     "clj-kondo.lint-as/def-catch-all")
   #'identity
   nil
   t))

(defun lsp-clojure--ask-clj-kondo-config-dir ()
  "Ask to user the clj-kondo config dir path."
  (lsp--completing-read
   "Select where LSP should save this setting:"
   (list (f-join (expand-file-name "~/") ".clj-kondo/config.edn")
         (f-join (or (lsp-workspace-root) "project") ".clj-kondo/config.edn"))
   #'identity
   nil
   t))

(defun lsp-clojure-resolve-macro-as ()
  "Ask to user how the unresolved macro should be resolved."
  (interactive)
  (lsp--cur-workspace-check)
  (lsp-clojure--execute-command "resolve-macro-as"
                                (list (lsp--buffer-uri)
                                      (- (line-number-at-pos) 1) ;; clojure-lsp expects line numbers to start at 0
                                      (current-column)
                                      (lsp-clojure--ask-macro-to-resolve)
                                      (lsp-clojure--ask-clj-kondo-config-dir))))

(lsp-defun lsp-clojure--resolve-macro-as ((&Command :command :arguments?))
  "Intercept resolve-macro-as command and send all necessary data."
  (let ((chosen-macro (lsp-clojure--ask-macro-to-resolve))
        (clj-kondo-config-path (lsp-clojure--ask-clj-kondo-config-dir)))
    (lsp-clojure--execute-command command (append arguments? (list chosen-macro clj-kondo-config-path)))))

(defun lsp-clojure--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path t)))

(defun lsp-clojure--get-metadata-location (file-location)
  "Given a FILE-LOCATION return the file containing the metadata for the file."
  (format "%s.%s.metadata"
          (file-name-directory file-location)
          (file-name-base file-location)))

(defun lsp-clojure--file-in-jar (uri)
  "Check URI for a valid jar and include it in workspace."
  (string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" uri)
  (let* ((ns-path (match-string 3 uri))
         (ns (s-replace "/" "." ns-path))
         (file-location (concat lsp-clojure-workspace-cache-dir ns)))
    (unless (file-readable-p file-location)
      (lsp-clojure--ensure-dir (file-name-directory file-location))
      (with-lsp-workspace (lsp-find-workspace 'clojure-lsp nil)
        (let ((content (lsp-send-request (lsp-make-request "clojure/dependencyContents" (list :uri uri)))))
          (with-temp-file file-location
            (insert content))
          (with-temp-file (lsp-clojure--get-metadata-location file-location)
            (insert uri)))))
    file-location))

(defun lsp-clojure--server-executable-path ()
  "Return the clojure-lsp server command."
  (or (executable-find "clojure-lsp")
      (lsp-package-path 'clojure-lsp)))

(lsp-register-client
 (make-lsp-client
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'clojure-lsp callback error-callback))
  :semantic-tokens-faces-overrides '(:types (("macro" . font-lock-keyword-face)
                                             ("keyword" . clojure-keyword-face)))
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (or lsp-clojure-custom-server-command
                         `(,(lsp-clojure--server-executable-path))))
                   (lambda ()
                     (or lsp-clojure-custom-server-command
                         (lsp-clojure--server-executable-path))))
  :major-modes '(clojure-mode clojurec-mode clojurescript-mode)
  :library-folders-fn (lambda (_workspace) (list lsp-clojure-workspace-cache-dir))
  :uri-handlers (lsp-ht ("jar" #'lsp-clojure--file-in-jar))
  :action-handlers (lsp-ht ("resolve-macro-as" #'lsp-clojure--resolve-macro-as))
  :initialization-options '(:dependency-scheme "jar")
  :server-id 'clojure-lsp))

(provide 'lsp-clojure)
;;; lsp-clojure.el ends here
