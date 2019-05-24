;;; scratch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "scratch" "scratch.el" (0 0 0 0))
;;; Generated autoloads from scratch.el

(defvar scratch-buffer nil "\
Non-nil if the current buffer is a scratch buffer.")

(autoload 'scratch "scratch" "\
Get a scratch buffer for the MODE.

When called interactively with a prefix arg, prompt for the mode.

\(fn MODE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scratch" '("scratch-")))

;;;***

;;;### (autoloads nil nil ("scratch-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scratch-autoloads.el ends here
