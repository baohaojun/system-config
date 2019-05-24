;;; elm-test-runner.el --- Enhanced support for running elm-test

;; Copyright (C) 2017  Juan Edi

;; Author: Juan Edi
;; Version: 0.0.1
;; Package-Version: 20190105.1923
;; URL: https://github.com/juanedi/elm-test-runner
;; Package-Requires: ((emacs "24.4"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; At least for the moment, this package just sets up a few fuctions to run elm
;; tests from inside Emacs when using elm-mode.
;;
;; Most of it is just a copy of the fantastic rspec-mode
;; (https://github.com/pezra/rspec-mode)

;;; Code:

(require 'compile)
(require 'ansi-color)

(defgroup elm-test nil
  "elm-test integration"
  :group 'languages)

(defcustom elm-test-runner-command nil
  "Use to explicitly set the command that will be used to run elm-test.
If nil,will try to use a locally installed node runner and fallback to a globally installed 'elm-test' executable"
  :type 'string
  :group 'elm-test)

(defcustom elm-test-runner-command-options nil
  "Default options used with ‘elm-test-runner-command’."
  :type 'string
  :group 'elm-test)

(defcustom elm-test-runner-preferred-test-suffix "Test"
  "Preferred suffix for test files. Useful if, for example, you prefer the tests for module 'Foo' to live in 'FooSpecs' instead of 'FooTest'."
  :type 'string
  :group 'elm-test)

(defvar elm-test-runner-run-directory-for-file 'elm-test-runner--standard-project-root-for-file)
(defvar elm-test-runner-project-root-for-file 'elm-test-runner--standard-project-root-for-file)
(defvar elm-test-runner-template-for-module 'elm-test-runner--default-template-for-module)
(defvar elm-test-runner--last-arguments)
(defvar elm-test-runner--last-directory)

(define-compilation-mode elm-test-runner-compilation-mode "Elm Test Compilation"
  "Compilation mode for elm-test output."
  (add-hook 'compilation-filter-hook 'elm-test-runner--colorize-compilation-buffer nil t))

;;;###autoload
(defun elm-test-runner-run ()
  "Run elm-test on the current buffer's file."
  (interactive)
  (elm-test-runner--run-target
   (buffer-file-name)
   elm-test-runner-command-options))

;;;###autoload
(defun elm-test-runner-run-project ()
  "Run elm-test on the whole project."
  (interactive)
  (elm-test-runner--run-target
   (elm-test-runner--test-directory (buffer-file-name))
   elm-test-runner-command-options))

;;;###autoload
(defun elm-test-runner-run-directory ()
  "Prompt for a directory on which to run specs."
  (interactive)
  (let ((selected-dir (elm-test-runner--prompt-directory)))
    (if selected-dir
        (elm-test-runner--run-target
         selected-dir
         elm-test-runner-command-options))))

;;;###autoload
(defun elm-test-runner-watch ()
  "Run elm-test on the current buffer's file in watch mode."
  (interactive)
  (elm-test-runner--run-target
   (buffer-file-name)
   (concat elm-test-runner-command-options " --watch")))

;;;###autoload
(defun elm-test-runner-rerun ()
  "Re-run the last elm-test invocation."
  (interactive)
  (if (not elm-test-runner--last-directory)
      (error "No previous verification")
    (let ((default-directory elm-test-runner--last-directory))
      (apply #'elm-test-runner--compile elm-test-runner--last-arguments))))

;;;###autoload
(defun elm-test-runner-toggle-test-and-target ()
  "Switch to the test or the target file for the current buffer.
If the current buffer is visiting a test file, switches to the
target, otherwise the test."
  (interactive)
  (if (elm-test-runner--buffer-is-test-p)
      (elm-test-runner--go-to-matching-target-file (buffer-file-name))
    (elm-test-runner--go-to-matching-test-file (buffer-file-name))))

(defun elm-test-runner--go-to-matching-target-file (file-name)
  "Opens the implementation module corresponding to the specs in FILE-NAME."
  (let ((matching-target-file (elm-test-runner--target-file-for file-name)))
    (if matching-target-file
        (find-file matching-target-file)
        (message "Could not find matching target file."))))

(defun elm-test-runner--go-to-matching-test-file (target-file-name)
  "Opens the test module corresponding to TARGET-FILE-NAME."
  (let* ((test-directory (elm-test-runner--test-directory target-file-name))
         (test-file-name (elm-test-runner--test-file-for target-file-name test-directory))
         (test-file-exists (or (file-exists-p test-file-name)
                               (elm-test-runner--offer-create-test-file
                                target-file-name
                                test-file-name
                                test-directory))))
    (if test-file-exists
        (find-file test-file-name)
      (message "Could not find matching target file. Maybe the test directory is not readable?"))))

(defun elm-test-runner--offer-create-test-file (target-file-name test-file-name test-directory)
  "Asks the user if they want to create a missing test module.
TARGET-FILE-NAME is the source module for which we're trying to add tests.
TEST-FILE-NAME is the file name of the module that will be created.
TEST-DIRECTORY is where we'll add the specs."
  (when (y-or-n-p (concat "File " test-file-name " does not exist. Create it?"))
    (when (not (file-readable-p (file-name-directory test-file-name)))
      (make-directory (file-name-directory test-file-name) t))
    (write-region
     (apply elm-test-runner-template-for-module
            (list
             (elm-test-runner--module-name (file-relative-name test-file-name test-directory))
             (elm-test-runner--module-name (elm-test-runner--relative-target-file-name target-file-name))))
     nil test-file-name)
    test-file-name))

(defun elm-test-runner--module-name (relative-file-name)
  "Builds the name of the elm module expeted to be defined in RELATIVE-FILE-NAME."
  (replace-regexp-in-string
   "/" "."
   (replace-regexp-in-string
    "\.elm$" ""
    relative-file-name)))

(defun elm-test-runner--default-template-for-module (test-module-name target-module-name)
  "The template we'll use when creating TEST-MODULE-NAME, the test suite for TARGET-MODULE-NAME."
  (let ((entry-point (elm-test-runner--suite-entry-point))
        (sorted-imports (sort (list
                               "import Expect\n"
                               "import Test exposing (..)\n"
                               (concat "import " target-module-name " exposing (..)\n")
                               )
                              'string<)))
    (concat
     "module " test-module-name " exposing (" entry-point ")\n"
     "\n"
     (apply 'concat sorted-imports)
     "\n"
     "\n"
     entry-point " : Test\n"
     entry-point " =\n"
     "    describe \"something\"\n"
     "        [ test \"it works\" <|\n"
     "            \\() ->\n"
     "                Expect.equal 1 1\n"
     "        ]\n")))

(defun elm-test-runner--suite-entry-point ()
  "The name of the exported Test instance we'll expose in the template.
This is computed so that it matches ‘elm-test-runner-preferred-test-suffix’."
  (if (string= elm-test-runner-preferred-test-suffix "Test")
      "tests"
    (let ((first-char (substring elm-test-runner-preferred-test-suffix nil 1))
          (rest-str   (substring elm-test-runner-preferred-test-suffix 1)))
      (concat (downcase first-char) rest-str))))

(defun elm-test-runner--buffer-is-test-p ()
  "Is non-nil if the current buffer is a test."
  (and (buffer-file-name)
       (elm-test-runner--test-file-p (buffer-file-name))))

(defun elm-test-runner--test-file-p (a-file-name)
  "Is non-nil if the specified A-FILE-NAME is a test."
  (numberp (string-match
            (concat elm-test-runner-preferred-test-suffix "\\.elm\\'")
            a-file-name)))

(defun elm-test-runner--test-file-for (a-file-name test-directory)
  "Return the path to the matching test suite for A-FILE-NAME in TEST-DIRECTORY."
  (if (elm-test-runner--test-file-p a-file-name)
      a-file-name
    (let*
        ((relative-file-name (elm-test-runner--relative-target-file-name a-file-name))
         (file-name-in-test-dir (expand-file-name relative-file-name test-directory)))
      (elm-test-runner--testize-file-name file-name-in-test-dir))))

(defun elm-test-runner--relative-target-file-name (a-file-name)
  "Make a A-FILE-NAME's path relative to the source directory.
Examples:
  Bar.elm         -> Bar.elm
  Foo/Bar.elm     -> Foo/Bar.elm
  src/Bar.elm     -> Bar.elm
  src/Foo/Bar.elm -> Foo/Bar.elm"
  (let ((relative-file-name (file-relative-name a-file-name (elm-test-runner--project-root a-file-name))))
    (if (elm-test-runner--target-in-holder-dir-p a-file-name)
        (replace-regexp-in-string "^[^/]+/" "" relative-file-name)
      relative-file-name)))

(defun elm-test-runner--target-in-holder-dir-p (a-file-name)
  "Tell if A-FILE-NAME is contained in one of the 'well known' source directories (that would be only './src' for the moment)."
  (string-match (concat "^" (concat
                             (regexp-quote (elm-test-runner--project-root a-file-name))
                             "src"
                             "/"))
                a-file-name))

(defun elm-test-runner--target-file-for (a-spec-file-name)
  "Find the target for A-SPEC-FILE-NAME."
  (let ((candidate (elm-test-runner--targetize-file-name a-spec-file-name)))
    (cl-loop for dir in (list "." "src")
             for target = (replace-regexp-in-string
                           "/tests/"
                           (concat "/" dir "/")
                           candidate)
             if (file-exists-p target)
             return target)))

(defun elm-test-runner--parent-directory (a-directory)
  "Return the directory of which A-DIRECTORY is a child."
  (file-name-directory (directory-file-name a-directory)))

(defun elm-test-runner--root-directory-p (a-directory)
  "Return t if A-DIRECTORY is the root."
  (equal a-directory (elm-test-runner--parent-directory a-directory)))

(defun elm-test-runner--test-directory (a-file)
  "Return the nearest test directory that could contain specs for A-FILE."
  (if (file-directory-p a-file)
      (or
       (car (directory-files a-file t "^tests$"))
       (if (elm-test-runner--root-directory-p a-file)
           nil
         (elm-test-runner--test-directory (elm-test-runner--parent-directory a-file))))
    (elm-test-runner--test-directory (elm-test-runner--parent-directory a-file))))

(defun elm-test-runner--testize-file-name (a-file-name)
  "Return A-FILE-NAME but converted in to a test file name."
  ;; "tests/Foo/Bar.elm"  ->  "tests/Foo/BarTest.elm"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string
    "\\.elm$"
    (concat elm-test-runner-preferred-test-suffix ".elm")
    (file-name-nondirectory a-file-name))))

(defun elm-test-runner--targetize-file-name (a-file-name)
  "Return A-FILE-NAME but converted into a non-test file name with EXTENSION."
  (concat (file-name-directory a-file-name)
          (elm-test-runner--file-name-with-extension
           (replace-regexp-in-string
            (concat elm-test-runner-preferred-test-suffix "\\.elm")
            ".elm"
            (file-name-nondirectory a-file-name)))))

(defun elm-test-runner--file-name-with-extension (a-file-name)
  "Add .elm file extension to A-FILE-NAME if it does not already have an extension."
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".elm")))

(defun elm-test-runner--run-target (test-file &rest opts)
  "Run elm-test on TEST-FILE with the specified options OPTS."
  (elm-test-runner--compile (shell-quote-argument test-file) opts))

(defun elm-test-runner--compile (target &optional opts)
  "Run a compile for TARGET with the specified options OPTS."

  (setq elm-test-runner--last-directory default-directory
        elm-test-runner--last-arguments (list target opts))

  (let ((default-directory (or (elm-test-runner--run-directory)
                               (elm-test-runner--standard-project-root-for-file (buffer-file-name))
                               default-directory))
        ; unset TERM=dumb to allow colorized output
        (compilation-environment '("TERM=ansi")))
    (compile
     (elm-test-runner--compile-command target opts)
     'elm-test-runner-compilation-mode)))

(defun elm-test-runner--colorize-compilation-buffer ()
  "Interpret ansi colors in the test output buffer."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(defun elm-test-runner--run-directory (&optional current-file-name)
  "The directory from which the test command will be run.
Optional argument CURRENT-FILE-NAME the file name of whose project we'll run tests."
  (let*
      ((starting-point (or current-file-name (buffer-file-name)))
       (root-dir (apply elm-test-runner-run-directory-for-file (list starting-point))))
    (expand-file-name root-dir)))

(defun elm-test-runner--prompt-directory ()
  "Asks the user for a directory on which we'll run specs."
  (let ((selected-dir (read-directory-name "Test directory: " (file-name-directory buffer-file-name) nil t)))
    (when (and selected-dir (not (eq selected-dir "")))
      (expand-file-name selected-dir))))

(defun elm-test-runner--project-root (&optional current-file-name)
  "The root directory of CURRENT-FILE-NAME's elm project.
That is, the one with the elm.json file. The result of this function
depends on the value of ‘elm-test-runner-project-root-for-file’."
  (let*
      ((starting-point (or current-file-name (buffer-file-name)))
       (root-dir (apply elm-test-runner-project-root-for-file (list starting-point))))
    (expand-file-name root-dir)))

(defun elm-test-runner--standard-project-root-for-file (current-file-name)
  "Default implementation for ‘elm-test-runner--project-root’.

This function assumes a standard layout as described in elm-test's
documentation: the root directory should contain a 'tests' directory for
elm-test stuff.
Argument CURRENT-FILE-NAME the file whose Elm project's root we're looking for."
  (or
   (elm-test-runner--standard-elm-19-project-root-for-file current-file-name)
   (elm-test-runner--standard-elm-18-project-root-for-file current-file-name)))

(defun elm-test-runner--standard-elm-19-project-root-for-file (current-file-name)
  "Detect project root for a standard elm 19 project.
Argument CURRENT-FILE-NAME the file whose Elm project's root we're looking for."
  (locate-dominating-file current-file-name "elm.json"))

(defun elm-test-runner--standard-elm-18-project-root-for-file (current-file-name)
  "Detect project root for a standard elm 18 project.
Argument CURRENT-FILE-NAME the file whose Elm project's root we're looking for."
  ;; If we are on a target file, return the first directory we see with an elm-package.json
  ;; If we are on a test file, it's the one above.
  (let*
      ((first-elm-package-dir (locate-dominating-file current-file-name "elm-package.json"))
       (parent-dir (elm-test-runner--parent-directory first-elm-package-dir))
       (parent-dir-elm-package (car (directory-files parent-dir t "^elm-package\.json$"))))

    (if parent-dir-elm-package
        parent-dir
      first-elm-package-dir)))

(defun elm-test-runner--compile-command (target &optional opts)
  "Composes the compilation command to run specs for TARGET.
Optional argument OPTS is a string of command line args that will be passed to the test runner."
  (mapconcat 'identity `(,(elm-test-runner--runner)
                         ,(elm-test-runner--runner-options opts)
                         ,target) " "))

(defun elm-test-runner--runner ()
  "The command we'll use to run specs.

This can be customized using the ‘elm-test-runner-command’ variable."
  (or elm-test-runner-command (elm-test-runner--detect-node-runner)))

(defun elm-test-runner--detect-node-runner ()
  "The default way to locate the elm-test binary.

We first check if there is a local installation of the elm-test node package in
this project. If not, we assume an 'elm-test' binary will be present in the PATH."
  (let ((local-runner-path (concat default-directory "node_modules/.bin/elm-test")))
    (if (file-readable-p local-runner-path)
        local-runner-path
      "elm-test")))

(defun elm-test-runner--runner-options (&optional opts)
  "Return string of options from OPTS for command line."
  (let ((opts (if (listp opts)
                  opts
                (list opts))))
    (mapconcat 'identity opts " ")))

(provide 'elm-test-runner)
;;; elm-test-runner.el ends here
