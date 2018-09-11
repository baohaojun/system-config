;;; qml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "qml-mode" "qml-mode.el" (0 0 0 0))
;;; Generated autoloads from qml-mode.el

(autoload 'qml-mode "qml-mode" "\
Major mode for editing QML.

\\{qml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "qml-mode" '("qml-")))

;;;***

;;;### (autoloads nil nil ("qml-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; qml-mode-autoloads.el ends here
