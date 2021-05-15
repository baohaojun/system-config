;;; nim-compile.el --- A support package of compilation for Nim -*- lexical-binding: t -*-

;;; Commentary:

;; TODO: compilation command is way too long.  Maybe we should shorten
;; or hide some default options like excessiveStackTrace etc.

;;; Code:
(require 'cl-lib)
(require 'rx)
(require 'nim-vars)

(defvar nim-compile-command-checker-functions
  '(nim-compile--project)
  "Checker functions to decide build command.
Functions (hooks) take one argument as file file string and
return build command like ‘nim c -r FILE.nim’")

(defvar nim-compile-default-command
  '("c" "-r" "--verbosity:0" "--hint[Processing]:off" "--excessiveStackTrace:on"))

(defvar-local nim-compile--current-command nil)

;; MEMO:
;; Implemented based on compiler document:
;;   http://nim-lang.org/docs/nimc.html#compiler-usage-configuration-files

;; (5) for PROJECT/PROJECT.(nim.cfg/.nims)
(defun nim-get-project-file (candidates &optional dir)
  (let* ((projdir (file-name-base
                   (directory-file-name (or dir default-directory))))
         (projfile
          (directory-file-name (mapconcat 'file-name-as-directory
                                          `(,default-directory ,projdir)
                                          ""))))
    (cl-loop for ext in candidates
             for file = (format "%s%s" projfile ext)
             if (file-exists-p file)
             do (cl-return file))))

;; (3,4) (parentDir or projectDir)/nim.cfg
(defconst nim-config-regex
  (rx (group (or (group (or "nimcfg" "nim.cfg"))
                 (group (? (and (0+ any) ".")) "nim.cfg"))
             line-end)))

(defun nim-find-config-file ()
  "Get the config file from current directory hierarchy.
The config file would one of those: config.nims, PROJECT.nim.cfg, or nim.cfg."
  (nim-find-file-in-heirarchy
   (file-name-directory (buffer-file-name))
   nim-config-regex))

(defun nim-find-file-in-heirarchy (current-dir pattern)
  "Search starting from CURRENT-DIR for a file matching PATTERN upwards through the directory hierarchy."
  (catch 'found
    (locate-dominating-file
     current-dir
     (lambda (dir)
       (let ((pfile (nim-get-project-file '(".nims" ".nim.cfg") dir)))
         (when pfile (throw 'found pfile)))
       (let ((file (cl-first (directory-files dir t pattern nil))))
         (when file (throw 'found file)))))))

(defcustom nim-project-root-regex "\\(\.git\\|\.nim\.cfg\\|\.nimble\\)$"
  "Regex to find project root directory."
  :type 'string
  :group 'nim)

(defun nim-get-project-root ()
  "Return project directory."
  (file-name-directory
   (nim-find-file-in-heirarchy
    (file-name-directory (buffer-file-name)) nim-project-root-regex)))

;; Compile command support
(require 'compile)

(defun nim-compile--project (file)
  "Return ‘nim build FILE’ if there is PROJECT.nims."
  (let ((proj (nim-get-project-file '(".nims" ".nim.cfg"))))
    (when (and proj (nim-nims-file-p proj)
               (eq major-mode 'nim-mode))
      (nim--fmt '("build") file))))

(defun nim-nims-file-p (file)
  "Test if FILE is a nim script file."
  (equal "nims" (file-name-extension file)))

(defun nim-nimble-file-p (file)
  "Test if FILE is a nimble file."
  (equal "nimble" (file-name-extension file)))

(defun nim-compile--get-compile-command ()
  "Return Nim's compile command or use previous command if it exists."
  (if nim-compile--current-command
      nim-compile--current-command
    (setq nim-compile--current-command
          (let ((file (when buffer-file-name
                        (shell-quote-argument buffer-file-name))))
            (when file
              (cond
               ;; WIP
               ((eq 'nimscript-mode major-mode)
                (let ((pfile (nim-get-project-file '(".nims" ".nimble"))))
                  (cond
                   ;; as build tool
                   ((nim-nimble-file-p file)
                    (let ((nim-compile-command "nimble"))
                      (nim--fmt '("build") "")))
                   ((and (nim-nims-file-p pfile)
                         (equal pfile buffer-file-name))
                    (nim--fmt '("build") pfile))
                   (t
                    ;; as script file
                    (nim--fmt '("e") file)))))
               (t
                (let ((cmd (run-hook-with-args-until-success
                            'nim-compile-command-checker-functions file)))
                  (or cmd (nim--fmt nim-compile-default-command file))))))))))

(defun nim--fmt (args file)
  "Format ARGS and FILE for the nim command into a shell compatible string."
  (mapconcat
   'shell-quote-argument
   (delq nil `(,nim-compile-command ,@args ,@nim-compile-user-args ,file))
   " "))

(define-compilation-mode nim-compile-mode "nim-compile"
  "major-mode for *nim-compile* buffer."
  ;; keep `nim--colorize-compilation-buffer' for `recompile' function (g key)
  (if (eq major-mode 'nim-compile-mode)
      (add-hook 'compilation-filter-hook  #'nim--colorize-compilation-buffer t)
    (remove-hook 'compilation-filter-hook #'nim--colorize-compilation-buffer t)))

(defun nim-compile--assert (command)
  "Check COMMAND.
Basically copied from `compile-command's document."
  (and (stringp command)
       (or (not (boundp (quote compilation-read-command))) compilation-read-command)))

;;;###autoload
(defun nim-compile (&optional command)
  "Compile and execute the current buffer as a nim file.
All output is written into the *nim-compile* buffer.
If you put COMMAND argument, you can specify the compilation command."
  (interactive)
  (when (derived-mode-p 'nim-mode)
    (setq-local compile-command
                (or command
                    (if (or compilation-read-command current-prefix-arg)
                        (compilation-read-command (nim-compile--get-compile-command))
                      (nim-compile--get-compile-command))))
    ;; keep users' previous command if they changed
    (setq-local nim-compile--current-command compile-command)
    (if (nim-compile--assert compile-command)
        (funcall 'compile compile-command 'nim-compile-mode)
      (error "Something goes wrong"))))


;; enable the regular for nim error messages in compilation buffers
(add-to-list 'compilation-error-regexp-alist 'nim)

;; Define a regex to parse Nim's compilation message to jump over
;; error or warning points.
(add-to-list
 'compilation-error-regexp-alist-alist
 `(nim ,(rx line-start
            (or
             ;; Info
             (group-n 5 (or "Hint: " "template/generic instantiation from here"))
             ;; Warning or Error
             (group-n 7
              ;; File name
              (group-n 1 (1+ (in alnum "\\" "/" "_" "." "-") "") ".nim" (? "s"))
              "("
              ;; Line
              (group-n 2 (1+ digit))
              ;; Column -- this parameter is optional when it
              ;; comes from stacktrace (see #171)
              (? ", " (group-n 3 (1+ digit)))
              ")"
              ;; Type -- also this parameter doesn't show up when it
              ;; come from stacktrace
              " " (? (group-n 4 (or "Warning" "Error") ": "))))
            ;; Capture rest of message
            (0+ any) line-end)
       ;; See `compilation-error-regexp-alist's document for the detail
       1 2 3 (4 . 5)))

(require 'ansi-color)
(defun nim--colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(provide 'nim-compile)
;;; nim-compile.el ends here
