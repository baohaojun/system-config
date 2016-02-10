;;; ghci-completion.el --- Completion for GHCi commands in inferior-haskell buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Version: 20140316.2331
;; X-Original-Version: 0.1.3
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;; To support completion for GHCi commands (like :load, :module, :set
;; etc.) in inferior-haskell buffers.
;;
;; Installation:
;;
;; To turn on GHCi commands completion in inferior-haskell buffers,
;; add this to .emacs:
;;
;;     (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
;;
;; Otherwise, call `turn-on-ghci-completion'.
;;
;; You may also want to set `ghci-completion-ghc-pkg-additional-args'
;; to the list of additional argument to supply to `ghc-pkg'.  For
;; example, this variable can be used to specify which database (user
;; or global) or which package config file to use.
;;
;; Limitations:
;;
;; * This package is developed for Emacs 24 and it probably only works
;;   with Emacs 24.  In particular, we rely on lexical bindings, which
;;   have been introduced in Emacs 24, and on `pcomplete', which is
;;   effectively broken in 23.2.
;;
;; * Only the following commands are supported: :add, :browse[!], :cd,
;;   :edit, :load, :module, :set, :unset, :show.  It would be nice to
;;   have shell completion for :!, for example.
;;
;; * The starred versions of the commands :add, :browse[!], :load, and
;;   :module are not supported, partly because I don't use them, and
;;   partly because, for example, :module offers completion only on
;;   the exposed modules in registered packages in both the global and
;;   user databases, and for these :module *<mod> is meaningless.
;;
;; * :set and :unset support only a subset of all GHC flags: language
;;   extensions, warnings, and debugging options.  Adding completion
;;   for other flags is trivial.  It would be nice, however, to be
;;   able to generate the list of all GHC flags programmatically.

;;; Code:

(eval-when-compile (require 'cl))
(require 'comint)
(require 'pcomplete)

;;; Command completion

(defconst ghci-completion-commands
  '(":!"
    ":?"
    ":add"
    ":browse" ":browse!"
    ":cd"
    ":cmd"
    ":ctags" ":ctags!"
    ":def"
    ":edit"
    ":etags"
    ":help"
    ":info"
    ":kind"
    ":load"
    ":main"
    ":module"
    ":quit"
    ":reload"
    ":run"
    ":type"
    ":undef"
    ":abandon"
    ":back"
    ":break"
    ":continue"
    ":delete"
    ":force"
    ":forward"
    ":history"
    ":list"
    ":print"
    ":sprint"
    ":step"
    ":steplocal"
    ":stepmodule"
    ":trace"
    ":set"
    ":unset"
    ":show")
  "The list of commands available from the GHCi prompt.")

(defun ghci-completion-match-partial-command ()
  "Return the command name at point, or nil if none is found."
  (save-excursion
    (comint-bol nil)
    (when (looking-at " *\\(:[a-z]*\\)$")
      (match-string-no-properties 1))))

(defun ghci-completion-command-completion ()
  "Return the completion data for the command at point, if any."
  (let ((command (ghci-completion-match-partial-command)))
    (when command
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (completions
             (remove-if-not
              (lambda (candidate)
                (string-prefix-p command candidate))
              ghci-completion-commands)))
        (list
         beg end
         (lambda (string pred action)
           (complete-with-action action completions string pred))
         :exit-function
         (lambda (_string finished)
           (when (memq finished '(sole finished))
             (if (looking-at " ")
                 (goto-char (match-end 0))
               (insert " ")))))))))

;;; Command options

(defcustom ghci-completion-ghc-pkg-additional-args nil
  "The list of additional arguments to `ghc-pkg'.

Can be used, for example, to specify which database (user or
global) or which package config file to use."
  :group 'ghci-completion)

(defvar ghci-completion-exposed-modules nil
  "The list of exposed modules.")

(defun ghci-completion-parse-exposed-modules ()
  "Return the list of exposed modules from the registered
packages in both the global and user databases."
  (with-temp-buffer
    (apply #'call-process
           "ghc-pkg" nil (current-buffer) nil "dump"
           ghci-completion-ghc-pkg-additional-args)
    (goto-char (point-min))
    (loop while (re-search-forward
                 (concat "exposed: True\n"
                         "exposed-modules:"
                         "\\(\\(?:.*\n?\\)*?\\)"
                         "hidden-modules")
                 nil t)
          nconc (split-string (match-string 1) "[\s\n]+" t))))

(defvar ghci-completion-language-options nil
  "The list of supported language extensions.")

(defun ghci-completion-parse-language-options ()
  "Return the list of language extensions supported by GHC."
  (mapcar
   (lambda (extension)
     (concat "-X" extension))
   (split-string
    (shell-command-to-string "ghc --supported-languages")
    "\n" t)))

(defconst ghci-completion-warning-options
  '("-w" "-W" "-Wall" "-Wwarn" "-Werror"
    "-fwarn-unrecognised-pragmas"
    "-fno-warn-unrecognised-pragmas"
    "-fwarn-warnings-deprecations"
    "-fno-warn-warnings-deprecations"
    "-fwarn-deprecated-flags"
    "-fno-warn-deprecated-flags"
    "-fwarn-duplicate-exports"
    "-fno-warn-duplicate-exports"
    "-fwarn-hi-shadowing"
    "-fno-warn-hi-shadowing"
    "-fwarn-implicit-prelude"
    "-fno-warn-implicit-prelude"
    "-fwarn-incomplete-patterns"
    "-fno-warn-incomplete-patterns"
    "-fwarn-incomplete-record-updates"
    "-fno-warn-incomplete-record-updates"
    "-fwarn-lazy-unlifted-bindings"
    "-fno-warn-lazy-unlifted-bindings"
    "-fwarn-missing-fields"
    "-fno-warn-missing-fields"
    "-fwarn-missing-import-lists"
    "-fnowarn-missing-import-lists"
    "-fwarn-missing-methods"
    "-fno-warn-missing-methods"
    "-fwarn-missing-signatures"
    "-fno-warn-missing-signatures"
    "-fwarn-name-shadowing"
    "-fno-warn-name-shadowing"
    "-fwarn-orphans"
    "-fno-warn-orphans"
    "-fwarn-overlapping-patterns"
    "-fno-warn-overlapping-patterns"
    "-fwarn-tabs"
    "-fno-warn-tabs"
    "-fwarn-type-defaults"
    "-fno-warn-type-defaults"
    "-fwarn-monomorphism-restriction"
    "-fno-warn-monomorphism-restriction"
    "-fwarn-unused-binds"
    "-fno-warn-unused-binds"
    "-fwarn-unused-imports"
    "-fno-warn-unused-imports"
    "-fwarn-unused-matches"
    "-fno-warn-unused-matches"
    "-fwarn-unused-do-bind"
    "-fno-warn-unused-do-bind"
    "-fwarn-wrong-do-bind"
    "-fno-warn-wrong-do-bind")
  "The list of warning options.")

(defconst ghci-completion-debugging-options
  '("-dcore-lint"
    "-ddump-asm"
    "-ddump-bcos"
    "-ddump-cmm"
    "-ddump-cpranal"
    "-ddump-cse"
    "-ddump-deriv"
    "-ddump-ds"
    "-ddump-flatC"
    "-ddump-foreign"
    "-ddump-hpc"
    "-ddump-inlinings"
    "-ddump-llvm"
    "-ddump-occur-anal"
    "-ddump-opt-cmm"
    "-ddump-parsed"
    "-ddump-prep"
    "-ddump-rn"
    "-ddump-rules"
    "-ddump-simpl"
    "-ddump-simpl-phases"
    "-ddump-simpl-iterations"
    "-ddump-spec"
    "-ddump-splices"
    "-ddump-stg"
    "-ddump-stranal"
    "-ddump-tc"
    "-ddump-types"
    "-ddump-worker-wrapper"
    "-ddump-if-trace"
    "-ddump-tc-trace"
    "-ddump-rn-trace"
    "-ddump-rn-stats"
    "-ddump-simpl-stats"
    "-dno-debug-output"
    "-dppr-debug"
    "-dsuppress-uniques"
    "-dsuppress-coercions"
    "-dsuppress-module-prefixes"
    "-dppr-noprags"
    "-dppr-user-length"
    "-dsource-stats"
    "-dcmm-lint"
    "-dstg-lint"
    "-dstg-stats"
    "-dverbose-core2core"
    "-dverbose-stg2stg"
    "-dshow-passes"
    "-dfaststring-stats")
  "The list of debugging options.")

(defun ghci-completion-set/unset-options ()
  "Return the list of options suitable for :set/:unset commands."
  (append ghci-completion-language-options
          ghci-completion-warning-options
          ghci-completion-debugging-options
          '("+r" "+s" "+t")))

(defconst ghci-completion-show-options
  '("bindings"
    "breaks"
    "context"
    "modules"
    "packages"
    "languages"
    "args"
    "prog"
    "prompt"
    "editor"
    "stop")
  "The list of options for :show command.")

;;; Pcomplete rules

(defun pcomplete/:add ()
  (while (pcomplete-here* (pcomplete-entries))))

(defun pcomplete/:browse ()
  (pcomplete-here* ghci-completion-exposed-modules))

(fset 'pcomplete/:browse! 'pcomplete/:browse)

(defun pcomplete/:cd ()
  (pcomplete-here* (pcomplete-dirs)))

(defun pcomplete/:edit ()
  (pcomplete-here* (pcomplete-entries)))

(fset 'pcomplete/:e 'pcomplete/:edit)

(defun pcomplete/:load ()
  (while (pcomplete-here* (pcomplete-entries))))

(fset 'pcomplete/:l 'pcomplete/:load)

(defun pcomplete/:module ()
  (while (pcomplete-here* ghci-completion-exposed-modules)))

(fset 'pcomplete/:m 'pcomplete/:module)

(defun pcomplete/:set ()
  (while (pcomplete-here*
          (append (ghci-completion-set/unset-options)
                  '("args"
                    "prog"
                    "prompt"
                    "editor"
                    "stop")))))

(fset 'pcomplete/:s 'pcomplete/:set)

(defun pcomplete/:unset ()
  (while (pcomplete-here* (ghci-completion-set/unset-options))))

(defun pcomplete/:show ()
  (pcomplete-here* ghci-completion-show-options))

;;; ghci-completion-mode

(defvar ghci-completion-mode nil
  "Non-nil if GHCi completion mode is in effect.")
(make-variable-buffer-local 'ghci-completion-mode)

(defvar ghci-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'completion-at-point)
    map))

;;;###autoload
(defun turn-on-ghci-completion ()
  "Turn on GHCi completion mode."
  (setq ghci-completion-mode t)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'pcomplete-parse-comint-arguments)
  ;; The following two variables could be set to something more useful
  ;; (identifier completion?).  Meanwhile, we set both to `ignore', so
  ;; that we don't get directory completion by default.
  (set (make-local-variable 'pcomplete-default-completion-function)
       'ignore)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'ignore)
  (add-hook 'comint-dynamic-complete-functions
            'pcomplete-completions-at-point nil 'local)
  (add-hook 'comint-dynamic-complete-functions
            'ghci-completion-command-completion nil 'local)
  (setq ghci-completion-exposed-modules
        (ghci-completion-parse-exposed-modules))
  (setq ghci-completion-language-options
        (ghci-completion-parse-language-options))
  (let ((map (current-local-map)))
    (while (and map (not (eq map ghci-completion-map)))
      (setq map (keymap-parent map)))
    (unless map
      (set-keymap-parent ghci-completion-map (current-local-map))
      (setq map (make-sparse-keymap))
      (set-keymap-parent map ghci-completion-map)
      (use-local-map map))))

(defun turn-off-ghci-completion ()
  "Turn off GHCi completion mode."
  (remove-hook 'comint-dynamic-complete-functions
               'pcomplete-completions-at-point 'local)
  (remove-hook 'comint-dynamic-complete-functions
               'ghci-completion-command-completion 'local)
  (let ((map (current-local-map)))
    (while map
      (let ((parent (keymap-parent map)))
        (if (eq ghci-completion-map parent)
            (set-keymap-parent map (keymap-parent parent))
          (setq map parent)))))
  (setq ghci-completion-mode nil))

(or (assq 'ghci-completion-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((ghci-completion-mode " GHCi-Completion")))))

;;;###autoload
(defun ghci-completion-mode (&optional arg)
  "GHCi completion mode.
Provides basic TAB-completion of GHCi commands."
  (interactive "P")
  (setq ghci-completion-mode
        (if (null arg)
            (not ghci-completion-mode)
          (> (prefix-numeric-value arg) 0)))
  (if ghci-completion-mode
      (turn-on-ghci-completion)
    (turn-off-ghci-completion)))

(provide 'ghci-completion)

;;; ghci-completion.el ends here
