;;; yari-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yari" "yari.el" (0 0 0 0))
;;; Generated autoloads from yari.el

(autoload 'yari-anything "yari" "\


\(fn &optional REHASH)" t nil)

(autoload 'yari-helm "yari" "\


\(fn)" t nil)

(autoload 'yari "yari" "\
Look up Ruby documentation.

\(fn &optional RI-TOPIC REHASH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yari" '("yari-" "when-ert-loaded")))

;;;***

;;;### (autoloads nil nil ("yari-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yari-autoloads.el ends here
