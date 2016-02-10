;;; simple-httpd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (httpd-stop httpd-start) "simple-httpd" "simple-httpd.el"
;;;;;;  (21286 54535 759776 30000))
;;; Generated autoloads from simple-httpd.el

(autoload 'httpd-start "simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance.

\(fn)" t nil)

(autoload 'httpd-stop "simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("simple-httpd-pkg.el") (21286 54535 831274
;;;;;;  922000))

;;;***

(provide 'simple-httpd-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; simple-httpd-autoloads.el ends here
