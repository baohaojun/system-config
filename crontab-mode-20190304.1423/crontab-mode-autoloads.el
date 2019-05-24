;;; crontab-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "crontab-mode" "crontab-mode.el" (0 0 0 0))
;;; Generated autoloads from crontab-mode.el

(autoload 'crontab-mode "crontab-mode" "\
Major mode for editing crontab file.

\\{crontab-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/crontab\\.X*[[:alnum:]]+\\'" . crontab-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "crontab-mode" '("crontab-")))

;;;***

;;;### (autoloads nil nil ("crontab-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; crontab-mode-autoloads.el ends here
