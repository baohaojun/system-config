;;; magit-todos-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-todos" "magit-todos.el" (0 0 0 0))
;;; Generated autoloads from magit-todos.el

(defvar magit-todos-mode nil "\
Non-nil if Magit-Todos mode is enabled.
See the `magit-todos-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-todos-mode'.")

(custom-autoload 'magit-todos-mode "magit-todos" nil)

(autoload 'magit-todos-mode "magit-todos" "\
Show list of to-do items in Magit status buffer for tracked files in repo.

\(fn &optional ARG)" t nil)

(autoload 'magit-todos-list "magit-todos" "\
Show to-do list of the current Git repository in a buffer.
With prefix, prompt for repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'magit-todos-list-internal "magit-todos" "\
Open buffer showing to-do list of repository at DIRECTORY.

\(fn DIRECTORY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-todos" '("magit-todos-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-todos-autoloads.el ends here
