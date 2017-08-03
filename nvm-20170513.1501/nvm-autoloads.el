;;; nvm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "nvm" "nvm.el" (22915 145 577157 731000))
;;; Generated autoloads from nvm.el

(autoload 'nvm-use "nvm" "\
Activate Node VERSION.

If CALLBACK is specified, active in that scope and then reset to
previously used version.

\(fn VERSION &optional CALLBACK)" nil nil)

(autoload 'nvm-use-for "nvm" "\
Activate Node for PATH or `default-directory'.

This function will look for a .nvmrc file in that path and
activate the version specified in that file.

If CALLBACK is specified, active in that scope and then reset to
previously used version.

\(fn &optional PATH CALLBACK)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nvm-autoloads.el ends here
