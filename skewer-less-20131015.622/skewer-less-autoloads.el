;;; skewer-less-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (skewer-less-mode) "skewer-less" "skewer-less.el"
;;;;;;  (21286 54551 271810 395000))
;;; Generated autoloads from skewer-less.el

(autoload 'skewer-less-mode "skewer-less" "\
Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

Operates by invoking \"less.refresh()\" via skewer whenever the
buffer is saved.

For this to work properly, the less javascript should be included
in the target web page, and less should be configured in
development mode, using:

        var less = {env: \"development\"};

before including \"less.js\".

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("skewer-less-pkg.el") (21286 54551 350820
;;;;;;  603000))

;;;***

(provide 'skewer-less-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skewer-less-autoloads.el ends here
