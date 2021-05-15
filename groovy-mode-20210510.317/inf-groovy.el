;;; inf-groovy.el — inferior Groovy mode – groovy process in a buffer

;;; Inferior Groovy Mode – groovy process in a buffer.
;;;                      adapted from cmuscheme.el and inf-haskell.el

;; Copyright (c) 2006, 2010 Stuart Clayman
;; Copyright © 2015–2017 Russel WInder

;; Author: Stuart Clayman  <sclayman@ee.ucl.ac.uk>
;;; 2006-08-01       v1
;;; 2010-04-07       v2 fixes for new groovy-mode
;; Maintainer: Russel Winder <russel@winder.org.uk>
;; Version: 201705161954
;; Keywords: languages
;; Package-Requires: ((groovy-mode "0"))

;;;; NB Version number is date and time yyyymmddhhMM UTC.

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;  If you install using the packaging system no further set up should be needed. If you install this mode
;;  manually then you will likely need to add some extra lines in your init file:
;;
;; (1) modify .emacs to use groovy-mode
;;     for example :
;;
;;    (autoload 'groovy-mode "groovy-mode" "Mode for editing groovy source files" t)
;;    (setq auto-mode-alist
;;          (append '(("\\.groovy\\'" . groovy-mode)) auto-mode-alist))
;;    (setq interpreter-mode-alist (append '(("groovy" . groovy-mode))
;;                                   interpreter-mode-alist))
;;
;; (2) set to load inf-groovy and set inf-groovy key definition in groovy-mode.
;;
;;    (autoload 'groovy-mode "groovy-mode" "Groovy mode." t)
;;    (autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
;;    (autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")
;;
;;    (add-hook 'groovy-mode-hook
;;          '(lambda ()
;;             (inf-groovy-keys)
;;    ))
;;
;; (3) If groovysh isn't on $PATH and you haven't set $GROOVY_HOME,
;; you need to tell Emacs where groovysh is.
;;
;;    (setq groovysh "/Users/sclayman/Downloads/groovy-1.7.1/bin/groovysh")
;;
;; (4) execute
;;      M-x run-groovy

;;; Commentary

;;; Bugs:
;;  Bug tracking is currently handled using the GitHub issue tracker at
;;  https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues

;;; Versions:
;;  This mode is available on MELPA which tracks the mainline Git repository on GitHub, so there is a rolling release
;;  system based on commits to the mainline.

;;; Notes:

;;; TODO:
;;   Issues with this code are managed via the project issue management
;;   on GitHub: https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues?state=open

;;; History
;;   History is tracked in the Git repository rather than in this file.
;;   See https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/commits/master

;;----------------------------------------------------------------------------
;;; Code:

(require 'comint)
(require 'compile)
(require 'groovy-mode)


(defcustom groovysh
  (or
   ;; If groovysh in on $PATH, use that by default.
   (executable-find "groovysh")
   ;; Fall back to $GROOVY_HOME if the user has set that.
   (executable-find
    (format "%s/bin/groovysh" (or (getenv "GROOVY_HOME") ""))))
  "The path to the groovysh binary, used by `run-groovy'."
  :group 'groovy)

(defcustom groovysh-args
  "--color=false"
  "Arguments passed to groovysh when starting an inferior groovy buffer.")

;;;; for groovy
(defvar inferior-groovy-first-prompt-pattern "^groovy:.*> *"
  "first prompt regex pattern of groovy interpreter.")

(defvar inferior-groovy-prompt-pattern "^groovy:.*> *"
  "prompt regex pattern of groovy interpreter.")

;;
;; mode variables
;;
(defvar inferior-groovy-mode-hook nil
  "*Hook for customising inferior-groovy mode.")

(defvar inferior-groovy-mode-map nil
  "*Mode map for inferior-groovy-mode")

(defconst inferior-groovy-error-regexp-alist
       '(("SyntaxError: compile error\n^\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
         ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

(cond ((not inferior-groovy-mode-map)
       (setq inferior-groovy-mode-map
             (copy-keymap comint-mode-map))
       (define-key inferior-groovy-mode-map "\C-c\C-l" 'groovy-load-file)
       (define-key inferior-groovy-mode-map "\C-c\C-m" 'inferior-groovy-newline)))

;;;###autoload
(defun inf-groovy-keys ()
  "Set local key defs for inf-groovy in groovy-mode"
  (define-key groovy-mode-map "\M-\C-x" 'groovy-send-definition)
  (define-key groovy-mode-map "\C-x\C-e" 'groovy-send-last-sexp)
  (define-key groovy-mode-map "\C-c\M-d" 'groovy-send-definition)
  (define-key groovy-mode-map "\C-c\C-x" 'groovy-send-definition-and-go)
  (define-key groovy-mode-map "\C-c\M-r" 'groovy-send-region)
  (define-key groovy-mode-map "\C-c\C-r" 'groovy-send-region-and-go)
  (define-key groovy-mode-map "\C-c\C-z" 'switch-to-groovy)
  (define-key groovy-mode-map "\C-c\C-l" 'groovy-load-file)
  (define-key groovy-mode-map "\C-c\C-s" 'run-groovy))

(defvar groovy-buffer nil "current groovy (actually groovysh) process buffer.")

;;;###autoload
(defun inferior-groovy-mode ()
  "Major mode for interacting with an inferior groovy (groovysh) process.

The following commands are available:
\\{inferior-groovy-mode-map}

A groovy process can be fired up with M-x run-groovy.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-groovy-mode-hook (in that order).

You can send text to the inferior groovy process from other buffers containing
Groovy source.
    switch-to-groovy switches the current buffer to the groovy process buffer.
    groovy-send-definition sends the current definition to the groovy process.
    groovy-send-region sends the current region to the groovy process.

    groovy-send-definition-and-go, groovy-send-region-and-go,
        switch to the groovy process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable groovy-buffer.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for groovy; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-groovy-mode-hook
  (setq comint-prompt-regexp inferior-groovy-prompt-pattern)
  (setq major-mode 'inferior-groovy-mode)
  (setq mode-name "Inferior Groovy")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-groovy-mode-map)
  (define-key inferior-groovy-mode-map "\C-c\C-m" 'inferior-groovy-newline)
  (setq comint-input-filter (function groovy-input-filter))
  (setq comint-get-old-input (function groovy-get-old-input))
  (setq comint-use-prompt-regexp t)  ;; added v2
  (setq comint-process-echoes t)  ;; added v2
  (setq comint-eol-on-send t)  ;; added v2
  (compilation-shell-minor-mode t)
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist inferior-groovy-error-regexp-alist)
  (run-hooks 'inferior-groovy-mode-hook))

(defvar inferior-groovy-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun inferior-groovy-newline ()
  (interactive)
  (comint-send-input)
  (let ((proc (groovy-proc)))
    (comint-send-string proc "\n")))

(defun groovy-input-filter (str)
  "Don't save anything matching inferior-groovy-filter-regexp"
  (not (string-match inferior-groovy-filter-regexp str)))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun groovy-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inferior-groovy-first-prompt-pattern)
      (remove-in-string (buffer-substring (point) end)
                        inferior-groovy-prompt-pattern))))

(defun groovy-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (groovy-args-to-list (substring string (+ 1 where)
                                                 (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (groovy-args-to-list (substring string pos
                                                 (length string)))))))))

;;;###autoload
(defun run-groovy (cmd)
  "Run an inferior Groovy process, input and output via buffer *groovy*.
If there is a process already running in *groovy*, switch to that buffer.
With a prefix argument, prompt for the groovysh path and arguments
\(see variables `groovysh' and `groovysh-args' for the defaults).

Runs the hook `inferior-groovy-mode-hook' (after the
`comint-mode-hook' is run).  Type \\[describe-mode] in the
process buffer for a list of commands."
  (interactive (list
                (format "%s %s"
                        (if current-prefix-arg
                            (read-file-name "groovysh binary: " groovysh)
                          groovysh)
                        (if current-prefix-arg
                            (read-string "groovysh arguments: " groovysh-args)
                          groovysh-args))))
  (if (not (comint-check-proc "*groovy*"))
      (let ((cmdlist (groovy-args-to-list cmd)))
        (set-buffer (apply 'make-comint "groovy" (car cmdlist)
                           nil (cdr cmdlist)))
        (inferior-groovy-mode)))
  (setq groovy-buffer "*groovy*")
  (pop-to-buffer "*groovy*")
  (get-buffer-process groovy-buffer))


(defun groovy-proc ()
  "Returns the current groovy process. See variable groovy-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-groovy-mode)
                                      (current-buffer)
                                    groovy-buffer))))
    (or proc
        (call-interactively 'run-groovy))))

(defun groovy-send-region (start end)
  "Send the current region to the inferior Groovy process."
  (interactive "r")

  (save-excursion
    (save-restriction
      (let ((str (concat (buffer-substring start end) "\n"))
            (proc (groovy-proc)))

        (with-current-buffer (process-buffer proc)
          (while (and
                  (goto-char comint-last-input-end)
                  (not (re-search-forward comint-prompt-regexp nil t))
                  (accept-process-output proc)))
          (goto-char (process-mark proc))
          (insert-before-markers str)
          (move-marker comint-last-input-end (point))
          (comint-send-string proc str)
          (comint-send-string proc "\n"))))))

(defun groovy-send-string (str)
  "Send a string STR to the inferior Groovy process."
  (interactive "r")

  (save-excursion
    (save-restriction
      (let ((proc (groovy-proc)))

        (with-current-buffer (process-buffer proc)
          (while (and
                  (goto-char comint-last-input-end)
                  (not (re-search-forward comint-prompt-regexp nil t))
                  (accept-process-output proc)))
          (goto-char (process-mark proc))
          (insert-before-markers str)
          (move-marker comint-last-input-end (point))
          (comint-send-string proc str)
          (comint-send-string proc "\n"))))))

(defun groovy-send-definition ()
  "Send the current definition to the inferior Groovy process."
  (interactive)
  (save-excursion
    (c-end-of-defun)
    (let ((end (point)))
      (c-beginning-of-defun)
      (groovy-send-region (point) end))))

(defun groovy-send-last-sexp ()
  "Send the previous sexp to the inferior Groovy process."
  (interactive)
  (groovy-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun switch-to-groovy (eob-p)
  "Switch to the groovy process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer groovy-buffer)
      (pop-to-buffer groovy-buffer)
    (error "No current process buffer. See variable groovy-buffer."))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun groovy-send-string-and-go (str)
  "Send the string STR to the inferior Groovy process.
Then switch to the process buffer."
  (interactive "r")
  (groovy-send-string str)

  (switch-to-groovy t))

(defun groovy-send-region-and-go (start end)
  "Send the current region to the inferior Groovy process.
Then switch to the process buffer."
  (interactive "r")
  (groovy-send-region start end)

  (switch-to-groovy t))

(defun groovy-send-definition-and-go ()
  "Send the current definition to the inferior Groovy.
Then switch to the process buffer."
  (interactive)
  (groovy-send-definition)
  (switch-to-groovy t))

(defvar groovy-source-modes '(groovy-mode)
  "*Used to determine if a buffer contains Groovy source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a groovy source file by groovy-load-file.
Used by these commands to determine defaults.")

(defvar groovy-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last groovy-load-file command.
Used for determining the default in the
next one.")

(defun groovy-load-file (file-name)
  "Load a Groovy file into the inferior Groovy process."
  (interactive (comint-get-source "Load Groovy file: " groovy-prev-l/c-dir/file
                                  groovy-source-modes t)) ; T because LOAD
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq groovy-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (comint-send-string (groovy-proc) (concat ":l "
                                            file-name
                                            "\n")))

;;; Do the user's customisation...

(defvar inf-groovy-load-hook nil
  "This hook is run when inf-groovy is loaded in.
This is a good place to put keybindings.")

(run-hooks 'inf-groovy-load-hook)

;;;###autoload
(eval-after-load 'groovy-mode
  (lambda ()
    (add-hook 'groovy-mode-hook 'inf-groovy-keys)))

;;----------------------------------------------------------------------------

(provide 'inf-groovy)

;;; inf-groovy.el ends here
