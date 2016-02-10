;;; wgrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "wgrep" "wgrep.el" (21466 15800 561098 880000))
;;; Generated autoloads from wgrep.el

(autoload 'wgrep-setup "wgrep" "\
Setup wgrep preparation.

\(fn)" nil nil)
(add-hook 'grep-setup-hook 'wgrep-setup)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wgrep-autoloads.el ends here
