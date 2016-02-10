;;; scala-mode2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "scala-mode2" "scala-mode2.el" (21933 47557
;;;;;;  779571 240000))
;;; Generated autoloads from scala-mode2.el

(autoload 'scala-mode:set-scala-syntax-mode "scala-mode2" "\
Sets the syntax-table and other realted variables for the current buffer to those of scala-mode. Can be used to make some other major mode (such as sbt-mode) use scala syntax-table.

\(fn)" nil nil)

(autoload 'scala-mode:goto-start-of-code "scala-mode2" "\
Go to the start of the real code in the file: object, class or trait.

\(fn)" t nil)

(autoload 'scala-mode "scala-mode2" "\
Major mode for editing scala code.

When started, runs `scala-mode-hook'.

\\{scala-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8)

;;;***

;;;### (autoloads nil nil ("scala-mode2-fontlock.el" "scala-mode2-imenu.el"
;;;;;;  "scala-mode2-indent.el" "scala-mode2-lib.el" "scala-mode2-map.el"
;;;;;;  "scala-mode2-paragraph.el" "scala-mode2-pkg.el" "scala-mode2-sbt.el"
;;;;;;  "scala-mode2-syntax.el") (21933 47558 271277 749000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; scala-mode2-autoloads.el ends here
