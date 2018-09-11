;;; smarty-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smarty-mode" "smarty-mode.el" (0 0 0 0))
;;; Generated autoloads from smarty-mode.el

(setq auto-mode-alist (append '(("\\.tpl\\'" . smarty-mode)) auto-mode-alist))

(autoload 'smarty-mode "smarty-mode" "\
Major mode for editing Smarty template files

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smarty-mode" '("smarty-")))

;;;***

;;;### (autoloads nil nil ("smarty-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smarty-mode-autoloads.el ends here
