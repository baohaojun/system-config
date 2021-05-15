;;; inheritenv.el --- Make temp buffers inherit buffer-local environment variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/inheritenv
;; Package-Version: 20210204.354
;; Package-Commit: 7e4c8b0d0a43b6f1c6c4d6dbd2f3bf5ce7f20067
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1-pre
;; Keywords: unix

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

;; Environment variables in Emacs can be set buffer-locally, like many
;; Emacs preferences, which allows users to have different buffer-local
;; paths for executables in different projects, specified by a
;; ".dir-locals.el" file or via a "direnv" integration like
;; envrc (see https://github.com/purcell/envrc).

;; However, there's a fairly common pitfall when Emacs libraries run
;; background processes on behalf of a user: many such libraries run
;; processes in temporary buffers that do not inherit the calling
;; buffer's environment.  This can result in executables not being found,
;; or the wrong versions of executables being picked up.

;; An example is the Emacs built-in command
;; `shell-command-to-string'.  Whatever buffer-local `process-environment'
;; (or `exec-path') the user has set, that command will always use the
;; Emacs-wide default.  This is *specified* behaviour, but not *expected*
;; or *helpful*.

;; `inheritenv' provides a couple of tools for dealing with this
;; issue:

;; 1. Library authors can wrap code that plans to execute processes in
;;    temporary buffers with the `inheritenv' macro.
;; 2. End users can modify commands like `shell-command-to-string' using
;;    the `inheritenv-add-advice' macro.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun inheritenv-apply (func &rest args)
  "Apply FUNC such that the environment it sees will match the current value.
This is useful if FUNC creates a temp buffer, because that will
not inherit any buffer-local values of variables `exec-path' and
`process-environment'.

This function is designed for convenient use as an \"around\" advice.

ARGS is as for ORIG."
  (cl-letf* (((default-value 'process-environment) process-environment)
             ((default-value 'exec-path) exec-path))
    (apply func args)))


(defmacro inheritenv (&rest body)
  "Wrap BODY so that the environment it sees will match the current value.
This is useful if BODY creates a temp buffer, because that will
not inherit any buffer-local values of variables `exec-path' and
`process-environment'."
  `(inheritenv-apply (lambda () ,@body)))


(defmacro inheritenv-add-advice (func)
  "Advise function FUNC with `inheritenv-apply'.
This will ensure that any buffers (including temporary buffers)
created by FUNC will inherit the caller's environment."
  `(advice-add ,func :around 'inheritenv-apply))


(provide 'inheritenv)
;;; inheritenv.el ends here
