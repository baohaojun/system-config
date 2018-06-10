;;; httprepl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "httprepl" "httprepl.el" (0 0 0 0))
;;; Generated autoloads from httprepl.el

(autoload 'httprepl "httprepl" "\
Interactively evaluate HTTP requests at a REPL-like
interface. Switches to the buffer specified by
`httprepl-buffer-name', or creates it if it does not exist. See
`httprepl-mode' for a reference on constructing requests.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "httprepl" '("httprepl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; httprepl-autoloads.el ends here
