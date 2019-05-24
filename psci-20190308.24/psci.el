;;; psci.el --- Major mode for purescript repl psci

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.6
;; Package-Requires: ((emacs "24.4") (purescript-mode "13.10") (dash "2.9.0"))
;; Keywords: languages purescript psci repl
;; URL: https://github.com/purescript-emacs/emacs-psci

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides a simple interface to evaluate Purescript expression.
;; Input is handled by the comint package.

;; To start psci repl:
;; M-x psci.  Type C-h m in the *psci* buffer for more info.

;; To activate some basic bindings, you can add the following hook
;; to purescript-mode:
;; (add-hook 'purescript-mode-hook 'inferior-psci-mode)

;; To come back and forth between a purescript-mode buffer and
;; repl, you could use repl-toggle (available on melpa):
;; (require 'repl-toggle)
;; (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

;; More informations: https://ardumont/emacs-psci
;; Issue tracker: https://github.com/purescript-emacs/emacs-psci/issues

;;; Code:

(require 'comint)
(require 'dash)
(require 'purescript-font-lock)

;; constants or variables

(defvar psci/buffer-name "psci"
  "Buffer name of the psci buffer.")

(defcustom psci/purs-path "purs"
  "Path to the \"purs\" binary."
  :group 'psci
  :type 'string)

(defcustom psci/psc-package-path "psc-package"
  "Path to the \"psc-package\" binary."
  :group 'psci
  :type 'string)

(defcustom psci/spago-path "spago"
  "Path to the \"spago\" binary."
  :group 'psci
  :type 'string)

(defcustom psci/arguments '("src/**/*.purs" "bower_components/purescript-*/src/**/*.purs")
  "Command-line arguments to pass to `psci' function."
  :group 'psci
  :type '(repeat string))

(defvar psci/prompt "> "
  "The psci prompt.")

;; private functions

(defun psci--project-root! ()
  "Determine the project's root folder.
Beware, can return nil if no .psci file is found."
  (if (and (fboundp 'projectile-project-root) (projectile-project-p))
      (projectile-project-root)
    default-directory))

(defun psci--process-name (buffer-name)
  "Compute the buffer's process name based on BUFFER-NAME."
  (format "*%s*" buffer-name))

(defun psci--file-content (filename)
  "Load FILENAME's content as a string.
When FILENAME is nil or not a real file, returns nil."
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun psci--run-psci-command! (command)
  "Run psci COMMAND as string."
  (-when-let (process (get-buffer-process (psci--process-name psci/buffer-name)))
    (comint-simple-send process command)))

(defun psci--compute-module-name! ()
  "Compute the current file's module name."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "^module\\s-+\\\([a-zA-Z0-9\\\.]+\\\)\\b"))
      (search-forward-regexp regexp)
      (match-string 1))))

(defun psci--get-psc-package-sources! ()
  "Find extra source path globs using purescript package tools,if they appear to be used."
  (cond
   ((file-exists-p "psc-package.json")
    (process-lines (psci--executable-find-relative psci/psc-package-path) "sources"))
   ((file-exists-p "spago.dhall")
    (process-lines (psci--executable-find-relative psci/spago-path) "sources"))))

(defun psci--executable-find-relative (path)
  "If PATH is a relative path to an executable, return its full path.
Otherwise, just return PATH."
  (let ((relative (expand-file-name path)))
    (if (file-executable-p relative)
        relative
      path)))

;; public functions

;;;###autoload
(defun psci (project-root-folder)
  "Run an inferior instance of \"psci\" inside Emacs, in PROJECT-ROOT-FOLDER.
If not supplied, the root folder will be guessed using
`projectile-project-root' (if available), otherwise it will
default to the current buffer's directory."
  (interactive (list (read-directory-name "Project root: "
                                          (psci--project-root!))))
  (let* ((default-directory project-root-folder)
         (psci-program psci/purs-path)
         (extra-sources (psci--get-psc-package-sources!))
         (buffer (comint-check-proc psci/buffer-name)))
    ;; pop to the "*psci*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'psci-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (psci--process-name psci/buffer-name)))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (let ((full-arg-list (append psci/arguments extra-sources)))
        (apply 'make-comint-in-buffer psci/buffer-name buffer
               psci-program nil "repl" full-arg-list))
      (psci-mode))))

(defvar psci-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `psci'.")

;;;###autoload
(define-derived-mode psci-mode comint-mode "psci"
  "Major mode for `run-psci'.

\\<psci-mode-map>"
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote psci/prompt)))
  (setq-local paragraph-separate "\\'") ;; so commands like M-{ and M-} work.
  (setq-local paragraph-start comint-prompt-regexp)
  (setq-local comint-input-sender-no-newline nil)
  (setq-local comint-input-sender 'comint-simple-send)
  (setq-local comint-get-old-input 'comint-get-old-input-default)
  (setq-local comint-process-echoes nil)
  (setq-local comint-prompt-read-only t) ;; read-only prompt
  (setq-local comint-eol-on-send t)
  (setq-local comint-input-filter-functions nil)
  (setq-local font-lock-defaults '(purescript-font-lock-keywords t))
  (setq-local comment-start "-- ")
  (setq-local comment-use-syntax t))

;;;###autoload
(defun psci/load-current-file! ()
  "Load the current file in the psci repl."
  (interactive)
  (save-buffer)
  (call-interactively 'psci/reset!)
  (call-interactively 'psci/load-module!))

;;;###autoload
(defun psci/load-module! ()
  "Load the module inside the repl session."
  (interactive)
  (-when-let (module-name (psci--compute-module-name!))
    (psci--run-psci-command! (format "import %s" module-name))))

;;;###autoload
(defun psci/reset! ()
  "Reset the current status of the repl session."
  (interactive)
  (psci--run-psci-command! ":reset"))

;;;###autoload
(defun psci/quit! ()
  "Quit the psci session."
  (interactive)
  (psci--run-psci-command! ":quit"))

(defvar inferior-psci-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'psci/load-current-file!)
    (define-key map (kbd "C-c M-n") 'psci/load-module!)
    map)
  "Basic mode map for `inferior-psci-mode'.")

(defgroup psci nil "psci customisation group."
  :tag "psci"
  :version "0.0.4"
  :group 'purescript
  :prefix "psci/")

;;;###autoload
(define-minor-mode inferior-psci-mode "psci minor mode to define default bindings."
  :lighter " ip"
  :keymap inferior-psci-mode-map
  :group 'psci)

(provide 'psci)
;;; psci.el ends here
