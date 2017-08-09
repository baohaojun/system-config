;;; ivy-erlang-complete-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ivy-erlang-complete" "ivy-erlang-complete.el"
;;;;;;  (22922 36375 603894 367000))
;;; Generated autoloads from ivy-erlang-complete.el

(autoload 'ivy-erlang-complete-autosetup-project-root "ivy-erlang-complete" "\
Automatically setup erlang project root.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-init "ivy-erlang-complete" "\
Config ivy-erlang-complete by default.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-show-doc-at-point "ivy-erlang-complete" "\
Show doc for function from standart library.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-reparse "ivy-erlang-complete" "\
Reparse macros and recors for completion in current buffer.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-set-project-root "ivy-erlang-complete" "\
Set root for current project.

\(fn)" t nil)

(autoload 'ivy-erlang-complete "ivy-erlang-complete" "\
Erlang completion at point.

\(fn)" t nil)

(autoload 'ivy-erlang-complete--find-definition "ivy-erlang-complete" "\
Search THING definition in DIRECTORY-PATH.

\(fn THING)" nil nil)

(autoload 'ivy-erlang-complete-find-spec "ivy-erlang-complete" "\
Find spec at point.  It also find callback definition.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-find-definition "ivy-erlang-complete" "\
Find erlang definition.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-find-references "ivy-erlang-complete" "\
Find erlang references.

\(fn)" t nil)

(autoload 'ivy-erlang-complete-find-file "ivy-erlang-complete" "\
Find file in current project.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ivy-erlang-complete-pkg.el") (22922 36375
;;;;;;  555894 249000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ivy-erlang-complete-autoloads.el ends here
