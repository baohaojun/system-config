;;; lsp-csharp.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jostein Kjønigsen, Saulius Menkevicius

;; Author: Saulius Menkevicius <saulius.menkevicius@fastmail.com>
;; Keywords:

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

;; lsp-csharp client

;;; Code:

(require 'lsp-mode)
(require 'gnutls)
(require 'f)

(defgroup lsp-csharp nil
  "LSP support for C#, using the Omnisharp Language Server.
Version 1.34.3 minimum is required."
  :group 'lsp-mode
  :link '(url-link "https://github.com/OmniSharp/omnisharp-roslyn"))

(defcustom lsp-csharp-server-install-dir
  (f-join lsp-server-install-dir "omnisharp-roslyn/")
  "Installation directory for OmniSharp Roslyn server."
  :group 'lsp-csharp
  :type 'directory)

(defcustom lsp-csharp-server-path
  nil
  "The path to the OmniSharp Roslyn language-server binary.
Set this if you have the binary installed or have it built yourself."
  :group 'lsp-csharp
  :type '(string :tag "Single string value or nil"))

(defcustom lsp-csharp-test-run-buffer-name
  "*lsp-csharp test run*"
  "The name of buffer used for outputing lsp-csharp test run results."
  :group 'lsp-csharp
  :type 'string)

(defcustom lsp-csharp-solution-file
  nil
  "Solution to load when starting the server.
Usually this is to be set in your .dir-locals.el on the project root directory."
  :group 'lsp-csharp
  :type 'string)

(defcustom lsp-csharp-omnisharp-roslyn-download-url
  (concat "https://github.com/omnisharp/omnisharp-roslyn/releases/latest/download/"
          (cond ((eq system-type 'windows-nt)
                 ; On Windows we're trying to avoid a crash starting 64bit .NET PE binaries in
                 ; Emacs by using x86 version of omnisharp-roslyn on older (<= 26.4) versions
                 ; of Emacs. See https://lists.nongnu.org/archive/html/bug-gnu-emacs/2017-06/msg00893.html"
                 (if (and (string-match "^x86_64-.*" system-configuration)
                          (version<= "26.4" emacs-version))
                     "omnisharp-win-x64.zip"
                   "omnisharp-win-x86.zip"))

                ((eq system-type 'darwin)
                 "omnisharp-osx.zip")

                ((and (eq system-type 'gnu/linux)
                      (or (eq (string-match "^x86_64" system-configuration) 0)
                          (eq (string-match "^i[3-6]86" system-configuration) 0)))
                 "omnisharp-linux-x64.zip")

                (t "omnisharp-mono.zip")))
  "Automatic download url for omnisharp-roslyn."
  :group 'lsp-csharp
  :type 'string)

(defcustom lsp-csharp-omnisharp-roslyn-store-path
  (f-join lsp-csharp-server-install-dir "latest" "omnisharp-roslyn.zip")
  "The path where omnisharp-roslyn .zip archive will be stored."
  :group 'lsp-csharp
  :type 'file)

(defcustom lsp-csharp-omnisharp-roslyn-server-dir
  (f-join lsp-csharp-server-install-dir "latest" "omnisharp-roslyn")
  "The path where omnisharp-roslyn .zip archive will be extracted."
  :group 'lsp-csharp
  :type 'file)

(lsp-dependency
 'omnisharp-roslyn
 `(:download :url lsp-csharp-omnisharp-roslyn-download-url
             :store-path lsp-csharp-omnisharp-roslyn-store-path))

(defun lsp-csharp--download-server (_client callback error-callback _update?)
  "Download zip package for omnisharp-roslyn and install it.
Will invoke CALLBACK on success, ERROR-CALLBACK on error."
  (lsp-package-ensure
   'omnisharp-roslyn
   (lambda ()
     (lsp-unzip lsp-csharp-omnisharp-roslyn-store-path
                lsp-csharp-omnisharp-roslyn-server-dir)
     (unless (eq system-type 'windows-nt)
       (let ((run-script (f-join lsp-csharp-omnisharp-roslyn-server-dir "run")))
         (when (not (f-exists-p run-script))
           ; create the `run' script when missing (e.g. when server binaries are extracted from omnisharp-mono.zip)
           ; NOTE: we do not check for presence or version of mono in the system
           (with-temp-file run-script
             (insert "#!/bin/bash\n")
             (insert "BASEDIR=$(dirname \"$0\")\n")
             (insert "exec mono $BASEDIR/OmniSharp.exe $@\n")))
         (set-file-modes run-script #o755)))
     (funcall callback))
   error-callback))

(defun lsp-csharp--language-server-path ()
  "Resolve path to use to start the server."
  (if lsp-csharp-server-path
      lsp-csharp-server-path
    (let ((server-dir lsp-csharp-omnisharp-roslyn-server-dir))
      (when (f-exists? server-dir)
        (f-join server-dir (cond ((eq system-type 'windows-nt) "OmniSharp.exe")
                                 (t "run")))))))

(defun lsp-csharp--language-server-command ()
  "Resolves path and arguments to use to start the server."
  (append
   (list (lsp-csharp--language-server-path) "-lsp")
   (when lsp-csharp-solution-file (list "-s" (expand-file-name lsp-csharp-solution-file)))))

(lsp-defun lsp-csharp-open-project-file ()
  "Open corresponding project file  (.csproj) for the current file."
  (interactive)
  (-let* ((project-info-req (lsp-make-omnisharp-project-information-request :file-name (buffer-file-name)))
          (project-info (lsp-request "o#/project" project-info-req))
          ((&omnisharp:ProjectInformation :ms-build-project) project-info)
          ((&omnisharp:MsBuildProject :path) ms-build-project))
    (find-file path)))

(defun lsp-csharp--get-buffer-code-elements ()
  "Retrieve code structure by calling into the /v2/codestructure endpoint.
Returns :elements from omnisharp:CodeStructureResponse."
  (-let* ((code-structure (lsp-request "o#/v2/codestructure"
                                       (lsp-make-omnisharp-code-structure-request :file-name (buffer-file-name))))
          ((&omnisharp:CodeStructureResponse :elements) code-structure))
    elements))

(defun lsp-csharp--inspect-code-elements-recursively (fn elements)
  "Invoke FN for every omnisharp:CodeElement found recursively in ELEMENTS."
  (seq-each
   (lambda (el)
     (funcall fn el)
     (-let (((&omnisharp:CodeElement :children) el))
       (lsp-csharp--inspect-code-elements-recursively fn children)))
   elements))

(defun lsp-csharp--collect-code-elements-recursively (predicate elements)
  "Flatten the omnisharp:CodeElement tree in ELEMENTS matching PREDICATE."
  (let ((results nil))
    (lsp-csharp--inspect-code-elements-recursively (lambda (el)
                                                     (when (funcall predicate el)
                                                       (setq results (cons el results))))
                                                   elements)
    results))

(lsp-defun lsp-csharp--l-c-within-range (l c (&omnisharp:Range :start :end))
  "Determine if L (line) and C (column) are within RANGE."
  (-let* (((&omnisharp:Point :line start-l :column start-c) start)
          ((&omnisharp:Point :line end-l :column end-c) end))
    (or (and (= l start-l) (>= c start-c) (or (> end-l start-l) (<= c end-c)))
        (and (> l start-l) (< l end-l))
        (and (= l end-l) (<= c end-c)))))

(defun lsp-csharp--code-element-stack-on-l-c (l c elements)
  "Return omnisharp:CodeElement stack at L (line) and C (column) in ELEMENTS tree."
  (when-let ((matching-element (seq-find (lambda (el)
                                           (-when-let* (((&omnisharp:CodeElement :ranges) el)
                                                        ((&omnisharp:RangeList :full?) ranges))
                                             (lsp-csharp--l-c-within-range l c full?)))
                                         elements)))
    (-let (((&omnisharp:CodeElement :children) matching-element))
      (cons matching-element (lsp-csharp--code-element-stack-on-l-c l c children)))))

(defun lsp-csharp--code-element-stack-at-point ()
  "Return omnisharp:CodeElement stack at point as a list."
  (let ((pos-line (plist-get (lsp--cur-position) :line))
        (pos-col (plist-get (lsp--cur-position) :character)))
    (lsp-csharp--code-element-stack-on-l-c pos-line
                                           pos-col
                                           (lsp-csharp--get-buffer-code-elements))))

(lsp-defun lsp-csharp--code-element-test-method-p (element)
  "Return test method name and test framework for a given ELEMENT."
  (when element
    (-when-let* (((&omnisharp:CodeElement :properties) element)
                 ((&omnisharp:CodeElementProperties :test-method-name? :test-framework?) properties))
      (list test-method-name? test-framework?))))

(defun lsp-csharp--reset-test-buffer (present-buffer)
  "Create new or reuse an existing test result output buffer.
PRESENT-BUFFER will make the buffer be presented to the user."
  (with-current-buffer (get-buffer-create lsp-csharp-test-run-buffer-name)
    (compilation-mode)
    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (when present-buffer
      (display-buffer lsp-csharp-test-run-buffer-name)))

(defun lsp-csharp--start-tests (test-method-framework test-method-names)
  "Run test(s) identified by TEST-METHOD-NAMES using TEST-METHOD-FRAMEWORK."
  (if (and test-method-framework test-method-names)
      (let ((request-message (lsp-make-omnisharp-run-tests-in-class-request
                              :file-name (buffer-file-name)
                              :test-frameworkname test-method-framework
                              :method-names (vconcat test-method-names))))
        (lsp-csharp--reset-test-buffer t)
        (lsp-session-set-metadata "last-test-method-framework" test-method-framework)
        (lsp-session-set-metadata "last-test-method-names" test-method-names)
        (lsp-request-async "o#/v2/runtestsinclass"
                           request-message
                           (-lambda ((&omnisharp:RunTestResponse))
                             (message "lsp-csharp: Test run has started"))))
    (message "lsp-csharp: No test methods to run")))

(defun lsp-csharp--test-message (message)
  "Emit a MESSAGE to lsp-csharp test run buffer."
  (when-let ((existing-buffer (get-buffer lsp-csharp-test-run-buffer-name))
             (inhibit-read-only t))
    (with-current-buffer existing-buffer
      (save-excursion
        (goto-char (point-max))
        (insert message "\n")))))

(defun lsp-csharp-run-test-at-point ()
  "Start test run at current point (if any)."
  (interactive)
  (let* ((stack (lsp-csharp--code-element-stack-at-point))
         (element-on-point (car (last stack)))
         (test-method (lsp-csharp--code-element-test-method-p element-on-point))
         (test-method-name (car test-method))
         (test-method-framework (car (cdr test-method))))
    (lsp-csharp--start-tests test-method-framework (list test-method-name))))

(defun lsp-csharp-run-all-tests-in-buffer ()
  "Run all test methods in the current buffer."
  (interactive)
  (let* ((elements (lsp-csharp--get-buffer-code-elements))
         (test-methods (lsp-csharp--collect-code-elements-recursively 'lsp-csharp--code-element-test-method-p elements))
         (test-method-framework (car (cdr (lsp-csharp--code-element-test-method-p (car test-methods)))))
         (test-method-names (mapcar (lambda (method)
                                      (car (lsp-csharp--code-element-test-method-p method)))
                                    test-methods)))
    (lsp-csharp--start-tests test-method-framework test-method-names)))

(defun lsp-csharp-run-test-in-buffer ()
  "Run selected test in current buffer."
  (interactive)
  (when-let* ((elements (lsp-csharp--get-buffer-code-elements))
              (test-methods (lsp-csharp--collect-code-elements-recursively 'lsp-csharp--code-element-test-method-p elements))
              (test-method-framework (car (cdr (lsp-csharp--code-element-test-method-p (car test-methods)))))
              (test-method-names (mapcar (lambda (method)
                                           (car (lsp-csharp--code-element-test-method-p method)))
                                         test-methods))
              (selected-test-method-name (lsp--completing-read "Select test:" test-method-names 'identity)))
    (lsp-csharp--start-tests test-method-framework (list selected-test-method-name))))

(defun lsp-csharp-run-last-tests ()
  "Re-run test(s) that were run last time."
  (interactive)
  (if-let ((last-test-method-framework (lsp-session-get-metadata "last-test-method-framework"))
           (last-test-method-names (lsp-session-get-metadata "last-test-method-names")))
      (lsp-csharp--start-tests last-test-method-framework last-test-method-names)
    (message "lsp-csharp: No test method(s) found to be ran previously on this workspace")))

(lsp-defun lsp-csharp--handle-os-error (_workspace (&omnisharp:ErrorMessage :file-name :text))
  "Handle the 'o#/error' (interop) notification by displaying a message with lsp-warn."
  (lsp-warn "%s: %s" file-name text))

(lsp-defun lsp-csharp--handle-os-testmessage (_workspace (&omnisharp:TestMessageEvent :message))
  "Handle the 'o#/testmessage and display test message on lsp-csharp
test output buffer."
  (lsp-csharp--test-message message))

(lsp-defun lsp-csharp--handle-os-testcompleted (_workspace (&omnisharp:DotNetTestResult
                                                            :method-name
                                                            :outcome
                                                            :error-message
                                                            :error-stack-trace
                                                            :standard-output
                                                            :standard-error))
  "Handle the 'o#/testcompleted' message from the server.

Will display the results of the test on the lsp-csharp test output buffer."
  (let ((passed (string-equal "passed" outcome)))
    (lsp-csharp--test-message
     (format "[%s] %s "
             (propertize (upcase outcome) 'font-lock-face (if passed 'success 'error))
             method-name))

    (unless passed
      (lsp-csharp--test-message error-message)

      (when error-stack-trace
        (lsp-csharp--test-message error-stack-trace))

      (unless (seq-empty-p standard-output)
        (lsp-csharp--test-message "STANDARD OUTPUT:")
        (seq-doseq (stdout-line standard-output)
          (lsp-csharp--test-message stdout-line)))

      (unless (seq-empty-p standard-error)
        (lsp-csharp--test-message "STANDARD ERROR:")
        (seq-doseq (stderr-line standard-error)
          (lsp-csharp--test-message stderr-line))))))

(lsp-defun lsp-csharp--action-client-find-references ((&Command :arguments?))
  "Read first argument from ACTION as Location and display xrefs for that location
using the `textDocument/references' request."
  (-if-let* (((&Location :uri :range) (lsp-seq-first arguments?))
             ((&Range :start range-start) range)
             (find-refs-params (append (lsp--text-document-position-params (list :uri uri) range-start)
                                       (list :context (list :includeDeclaration json-false))))
             (locations-found (lsp-request "textDocument/references" find-refs-params)))
      (lsp-show-xrefs (lsp--locations-to-xref-items locations-found) nil t)
    (message "No references found")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'lsp-csharp--language-server-command
                                   (lambda ()
                                     (when-let ((binary (lsp-csharp--language-server-path)))
                                       (f-exists? binary))))

                  :major-modes '(csharp-mode csharp-tree-sitter-mode)
                  :server-id 'csharp
                  :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                  :notification-handlers (ht ("o#/projectadded" 'ignore)
                                             ("o#/projectchanged" 'ignore)
                                             ("o#/projectremoved" 'ignore)
                                             ("o#/packagerestorestarted" 'ignore)
                                             ("o#/msbuildprojectdiagnostics" 'ignore)
                                             ("o#/packagerestorefinished" 'ignore)
                                             ("o#/unresolveddependencies" 'ignore)
                                             ("o#/error" 'lsp-csharp--handle-os-error)
                                             ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                             ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                             ("o#/projectconfiguration" 'ignore)
                                             ("o#/projectdiagnosticstatus" 'ignore))
                  :download-server-fn #'lsp-csharp--download-server))

(provide 'lsp-csharp)
;;; lsp-csharp.el ends here
