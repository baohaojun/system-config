;;; ivy-todo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-todo" "ivy-todo.el" (0 0 0 0))
;;; Generated autoloads from ivy-todo.el

(autoload 'ivy-todo "ivy-todo" "\
Read and manipulate entries in `ivy-todo-file'.
The default action changes the TODO state of the selected entry.
With a `\\[universal-argument]' ARG, first change the active TODO list.
With a `\\[universal-argument] \\[universal-argument]' ARG, change `ivy-todo-file'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-todo" '("ivy-todo-")))

;;;***

;;;### (autoloads nil nil ("ivy-todo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-todo-autoloads.el ends here
