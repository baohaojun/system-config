;;; jq-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jq-mode" "jq-mode.el" (0 0 0 0))
;;; Generated autoloads from jq-mode.el

(autoload 'jq-mode "jq-mode" "\
Major mode for jq scripts.
\\{jq-mode-map}

\(fn)" t nil)

(autoload 'jq-interactively "jq-mode" "\
Runs jq interactively on a json buffer.

\(fn BEG END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jq-mode" '("jq-")))

;;;***

;;;### (autoloads nil "ob-jq" "ob-jq.el" (0 0 0 0))
;;; Generated autoloads from ob-jq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-jq" '("org-babel-")))

;;;***

;;;### (autoloads nil nil ("jq-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jq-mode-autoloads.el ends here
