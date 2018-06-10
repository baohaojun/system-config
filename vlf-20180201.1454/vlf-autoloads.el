;;; vlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vlf" "vlf.el" (0 0 0 0))
;;; Generated autoloads from vlf.el

(autoload 'vlf "vlf" "\
View Large FILE in batches.  When MINIMAL load just a few bytes.
You can customize number of bytes displayed by customizing
`vlf-batch-size'.
Return newly created buffer.

\(fn FILE &optional MINIMAL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-base" "vlf-base.el" (0 0 0 0))
;;; Generated autoloads from vlf-base.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-base" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-ediff" "vlf-ediff.el" (0 0 0 0))
;;; Generated autoloads from vlf-ediff.el

(autoload 'vlf-ediff-files "vlf-ediff" "\
Run batch by batch ediff over FILE-A and FILE-B.
Files are processed with VLF with BATCH-SIZE chunks.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn FILE-A FILE-B BATCH-SIZE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-ediff" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-follow" "vlf-follow.el" (0 0 0 0))
;;; Generated autoloads from vlf-follow.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-follow" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-occur" "vlf-occur.el" (0 0 0 0))
;;; Generated autoloads from vlf-occur.el

(autoload 'vlf-occur-load "vlf-occur" "\
Load serialized `vlf-occur' results from current buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-occur" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-search" "vlf-search.el" (0 0 0 0))
;;; Generated autoloads from vlf-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-search" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-setup" "vlf-setup.el" (0 0 0 0))
;;; Generated autoloads from vlf-setup.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-setup" '("dired-vlf" "vlf-")))

;;;***

;;;### (autoloads nil "vlf-tune" "vlf-tune.el" (0 0 0 0))
;;; Generated autoloads from vlf-tune.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-tune" '("vlf-")))

;;;***

;;;### (autoloads nil "vlf-write" "vlf-write.el" (0 0 0 0))
;;; Generated autoloads from vlf-write.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vlf-write" '("vlf-")))

;;;***

;;;### (autoloads nil nil ("vlf-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vlf-autoloads.el ends here
