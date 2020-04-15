;;; browse-kill-ring-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "browse-kill-ring" "browse-kill-ring.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from browse-kill-ring.el

(autoload 'browse-kill-ring-default-keybindings "browse-kill-ring" "\
Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'." t nil)

(autoload 'browse-kill-ring "browse-kill-ring" "\
Display items in the `kill-ring' in another buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "browse-kill-ring" '("browse-kill-ring-")))

;;;***

;;;### (autoloads nil nil ("browse-kill-ring-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; browse-kill-ring-autoloads.el ends here
