;;; flycheck-haskell.el --- Flycheck: Automatic Haskell configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Sergey Vinokurov <serg.foo@gmail.com>
;; Copyright (C) 2014-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2016-2018 Danny Navarro <j@dannynavarro.net>
;; Copyright (C) 2015 Mark Karpov <markkarpov@opmbx.org>
;; Copyright (C) 2015 Michael Alan Dorman <mdorman@ironicdesign.com>
;; Copyright (C) 2015 Alex Rozenshteyn <rpglover64@gmail.com>
;; Copyright (C) 2014 Gracjan Polak <gracjanpolak@gmail.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-haskell
;; Keywords: tools, convenience
;; Version: 0.9-cvs
;; Package-Requires: ((emacs "24.3") (flycheck "0.25") (haskell-mode "13.7") (dash "2.4.0") (seq "1.11") (let-alist "1.0.1"))

;; This file is not part of GNU Emacs.

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

;; Automatically configure Flycheck for Haskell.

;;;; Cabal support

;; Try to find a Cabal file for the current Haskell buffer, and configure syntax
;; checking according to the Cabal project settings.

;;;; Cabal sandboxes

;; Try to find a Cabal sandbox configuration for this project, and configure the
;; Haskell syntax checkers in Flycheck to use the package database from the
;; Sandbox.

;;;; Stack support

;; Try to find a stack.yaml file for current project and configure stack projct
;; according to the Stack project settings.

;;;; Setup

;; (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'let-alist))

(require 'seq)
(require 'haskell-cabal)
(require 'flycheck)
(require 'dash)


;;; Customization

(defgroup flycheck-haskell nil
  "Haskell support for Flycheck."
  :prefix "flycheck-haskell-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-haskell"))

(defcustom flycheck-haskell-runghc-command
  (let ((stack-exe (funcall flycheck-executable-find "stack"))
        (runghc-exe (funcall flycheck-executable-find "runghc")))
    (cond
      (stack-exe
       `(,stack-exe "--verbosity" "silent" "runghc" "--no-ghc-package-path" "--" "-i"
                    "-packageCabal"
                    "-packagebase"
                    "-packagebytestring"
                    "-packagecontainers"
                    "-packageprocess"
                    "-packagedirectory"
                    "-packagefilepath"))
      (runghc-exe
       `(,runghc-exe "--" "-i"
                     "-packageCabal"
                     "-packagebase"
                     "-packagebytestring"
                     "-packagecontainers"
                     "-packageprocess"
                     "-packagedirectory"
                     "-packagefilepath"))
      (t
       ;; A reasonable default.
       '("runghc" "-i"))))
  "Command for `runghc'.

This library uses `runghc' to run various Haskell helper scripts
to extract information from Cabal files.  This option provides
the command to invoke `runghc'.  The default is to use `stack'
and otherwise fall back to standard `runghc'."
  :type '(repeat (string :tag "Command"))
  :risky t
  :group 'flycheck-haskell)

(defcustom flycheck-haskell-hpack-executable (funcall flycheck-executable-find "hpack")
  "Path to the `hpack' executable.

This library uses `hpack' to get package configuration if `package.yaml' file
is present.  This option provides the path to the `hpack' executable.  The nil
value will make this library ignore `package.yaml' file, even if it's present."
  :type 'string
  :risky t
  :group 'flycheck-haskell)

(defcustom flycheck-haskell-hpack-preference 'prefer-hpack
  "How to handle projects with both `.cabal' and `package.yaml' files present.

This option controls which configuration file this library will pick for
a project that has both `.cabal' and `package.yaml' files present.
The default, 'prefer-hpack, will make it pick `package.yaml' file as the source
of configuration parameters.  Another possible value, 'prefer-cabal will
make it pick `.cabal' file in such a case."
  :group 'flycheck-haskell
  :type '(set (const :tag "Prefer hpack's \"package.yaml\" file" prefer-hpack)
              (const :tag "Prefer cabal's \".cabal\" file" prefer-cabal))
  :safe #'symbolp)


;;; Cabal support
(defconst flycheck-haskell-directory
  (file-name-directory (if load-in-progress
                           load-file-name
                         (buffer-file-name)))
  "The package directory of flycheck-haskell.")

(defconst flycheck-haskell-helper
  (expand-file-name "get-cabal-configuration.hs" flycheck-haskell-directory)
  "The helper to dump the Cabal configuration.")

(defun flycheck-haskell-runghc-command (args)
  "Create a runghc command with ARGS.

Take the base command from `flycheck-haskell-runghc-command'."
  (append flycheck-haskell-runghc-command args nil))

(defun flycheck-haskell--read-configuration-with-helper (command)
  (with-temp-buffer
    ;; Hack around call-process' limitation handling standard error
    (let ((error-file (make-temp-file "flycheck-haskell-errors")))
      (pcase (apply 'call-process (car command) nil (list t error-file) nil (cdr command))
        (0 (delete-file error-file)
           (goto-char (point-min))
           (read (current-buffer)))
        (retcode (insert-file-contents error-file)
                 (delete-file error-file)
                 (message "Reading Haskell configuration failed with exit code %s and output:\n%s"
                          retcode (buffer-string))
                 nil)))))

(defun flycheck-haskell-read-cabal-configuration (cabal-file)
  "Read the Cabal configuration from CABAL-FILE."
  (let ((args (list flycheck-haskell-helper "--cabal-file" (expand-file-name cabal-file))))
    (flycheck-haskell--read-configuration-with-helper
     (flycheck-haskell-runghc-command args))))

(defun flycheck-haskell-read-hpack-configuration (hpack-file)
  "Read the hpack configuration from HPACK-FILE."
  (cl-assert flycheck-haskell-hpack-executable)
  (let ((args (list flycheck-haskell-helper
                    "--hpack-exe" flycheck-haskell-hpack-executable
                    "--hpack-file" (expand-file-name hpack-file))))
    (flycheck-haskell--read-configuration-with-helper
     (flycheck-haskell-runghc-command args))))

(defun flycheck-haskell--delete-dups (xs)
  "Remove duplicates from a list XS using `equal'. Leaves initial
list unchanged."
  (copy-sequence (delete-dups xs)))


;;; Cabal configuration caching
(defconst flycheck-haskell-config-cache (make-hash-table :test 'equal)
  "Cache of Cabal configuration.

A hash table, mapping the name of a cabal file to a
cons-cell `(MODTIME . CONFIG)', where MODTIME is the modification
time of the cabal file, and CONFIG the extracted configuration.")

(defun flycheck-haskell-clear-config-cache ()
  "Clear the cache of configurations."
  (interactive)
  (clrhash flycheck-haskell-config-cache))

(defun flycheck-haskell-get-cached-configuration (config-file)
  "Get the cached configuration for CABAL-FILE.

Return the cached configuration, or nil, if there is no cache
entry, or if the cache entry is outdated."
  (pcase-let* ((cache-entry (gethash config-file flycheck-haskell-config-cache))
               (`(,modtime . ,config) cache-entry))
    (when (and modtime (file-exists-p config-file))
      (let ((current-modtime (nth 5 (file-attributes config-file))))
        (if (time-less-p modtime current-modtime)
            ;; The entry is outdated, drop it.  `remhash' always
            ;; returns nil, so we are safe to use it here.
            (remhash config-file flycheck-haskell-config-cache)
          ;; The configuration is up to date, use it
          config)))))

(defun flycheck-haskell-read-and-cache-configuration (config-file)
  "Read and cache configuration from CABAL-FILE.

Return the configuration."
  (let ((modtime (nth 5 (file-attributes config-file)))
        (config (if (equal "yaml" (file-name-extension config-file))
                    (flycheck-haskell-read-hpack-configuration config-file)
                  (flycheck-haskell-read-cabal-configuration config-file))))
    (puthash config-file (cons modtime config) flycheck-haskell-config-cache)
    config))

(defun flycheck-haskell-get-configuration (config-file)
  "Get the Cabal configuration from CABAL-FILE.

Get the configuration either from our cache, or by reading the
CABAL-FILE.

Return the configuration."
  (or (flycheck-haskell-get-cached-configuration config-file)
      (flycheck-haskell-read-and-cache-configuration config-file)))


;;; Cabal sandbox support
(defconst flycheck-haskell-cabal-config "cabal.config"
  "The file name of a Cabal configuration.")

(defconst flycheck-haskell-cabal-config-keys '(with-compiler)
  "Keys to parse from a Cabal configuration file.")

(defconst flycheck-haskell-sandbox-config "cabal.sandbox.config"
  "The file name of a Cabal sandbox configuration.")

(defconst flycheck-haskell-sandbox-config-keys '(package-db)
  "Keys to parse from a Cabal sandbox configuration.")

(defmacro flycheck-haskell-with-config-file-buffer (file-name &rest body)
  "Eval BODY in a buffer with the contents of FILE-NAME."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,file-name)
     (goto-char (point-min))
     ,@body))

(defun flycheck-haskell-get-config-value (key)
  "Get the value of a configuration KEY from this buffer.

KEY is a symbol denoting the key whose value to get.  Return
a `(KEY . VALUE)' cons cell."
  (save-excursion
    (goto-char (point-min))
    (let ((setting (haskell-cabal--get-field (symbol-name key))))
      (when setting
        (cons key (substring-no-properties setting))))))

(defun flycheck-haskell-parse-config-file (keys config-file)
  "Parse KEYS from CONFIG-FILE.

KEYS is a list of symbols.  Return an alist with all parsed
KEYS."
  (flycheck-haskell-with-config-file-buffer config-file
    (mapcar #'flycheck-haskell-get-config-value keys)))

(defun flycheck-haskell-find-config (config-file)
  "Find a CONFIG-FILE for the current buffer.

Return the absolute path of CONFIG-FILE as string, or nil if
CONFIG-FILE was not found."
  (let ((root-dir (locate-dominating-file (buffer-file-name) config-file)))
    (when root-dir
      (expand-file-name config-file root-dir))))

(defun flycheck-haskell-get-cabal-config ()
  "Get Cabal configuration for the current buffer.

Return an alist with the Cabal configuration for the current
buffer."
  (let ((file-name (flycheck-haskell-find-config
                    flycheck-haskell-cabal-config)))
    (when file-name
      (flycheck-haskell-parse-config-file flycheck-haskell-cabal-config-keys
                                          file-name))))

(defun flycheck-haskell-get-sandbox-config ()
  "Get sandbox configuration for the current buffer.

Return an alist with the sandbox configuration for the current
buffer."
  (let ((file-name (flycheck-haskell-find-config
                    flycheck-haskell-sandbox-config)))
    (when file-name
      (flycheck-haskell-parse-config-file flycheck-haskell-sandbox-config-keys
                                          file-name))))


;;; Buffer setup
(defun flycheck-haskell-process-configuration (config)
  "Process the a Cabal CONFIG."
  (let-alist config
    (setq-local flycheck-ghc-search-path
                (flycheck-haskell--delete-dups
                 (append .build-directories .source-directories
                         flycheck-ghc-search-path)))
    (setq-local flycheck-ghc-language-extensions
                (flycheck-haskell--delete-dups
                 (append .extensions .languages
                         flycheck-ghc-language-extensions)))
    (setq-local flycheck-ghc-args
                (flycheck-haskell--delete-dups
                 (append .other-options
                         (seq-map (apply-partially #'concat "-I")
                                  .autogen-directories)
                         (when (car .should-include-version-header)
                           '("-optP-include" "-optPcabal_macros.h"))
                         (when (not (car .package-env-exists))
                           (cons "-hide-all-packages"
                               (seq-map (apply-partially #'concat "-package=")
                                        .dependencies)))
                         flycheck-ghc-args)))
    (setq-local flycheck-hlint-args
                (flycheck-haskell--delete-dups
                 (append (seq-map (apply-partially #'concat "--cpp-include=")
                                  .autogen-directories)
                         '("--cpp-file=cabal_macros.h"))))))

(defun flycheck-haskell-configure ()
  "Set paths and package database for the current project."
  (interactive)
  (when (and (buffer-file-name) (file-directory-p default-directory))
    (let ((config-file (flycheck-haskell--find-config-file)))
      (when config-file
        (let ((config (flycheck-haskell-get-configuration config-file)))
          (when config
            (flycheck-haskell-process-configuration config)))))

    (let-alist (flycheck-haskell-get-cabal-config)
      (when .with-compiler
        (setq-local flycheck-haskell-ghc-executable .with-compiler)))

    (let-alist (flycheck-haskell-get-sandbox-config)
      (when .package-db
        (setq-local flycheck-ghc-package-databases
                    (flycheck-haskell--delete-dups
                     (cons .package-db flycheck-ghc-package-databases)))
        (setq-local flycheck-ghc-no-user-package-database t)))))

(defun flycheck-haskell--find-config-file ()
  (let* ((cabal-file (haskell-cabal-find-file))
         (hpack-dir
          (and flycheck-haskell-hpack-executable
               (locate-dominating-file default-directory "package.yaml")))
         (hpack-file
          (when hpack-dir
            (concat hpack-dir "/package.yaml"))))
    (if cabal-file
        (if hpack-file
            (cond
              ((eq 'prefer-hpack flycheck-haskell-hpack-preference)
               hpack-file)
              (t
               cabal-file))
          cabal-file)
      hpack-file)))

;;;###autoload
(defun flycheck-haskell-setup ()
  "Setup Haskell support for Flycheck.

If the current file is part of a Cabal project, configure
Flycheck to take the module paths of the Cabal projects into
account.

Also search for Cabal sandboxes and add them to the module search
path as well."
  (flycheck-haskell-configure)
  (add-hook 'hack-local-variables-hook #'flycheck-haskell-configure))

(provide 'flycheck-haskell)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; flycheck-haskell.el ends here
