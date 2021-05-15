;;; merlin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "merlin" "merlin.el" (0 0 0 0))
;;; Generated autoloads from merlin.el

(autoload 'merlin-mode "merlin" "\
Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

If called interactively, enable Merlin mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Short cuts:
\\{merlin-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin" '("merlin-")))

;;;***

;;;### (autoloads nil "merlin-cap" "merlin-cap.el" (0 0 0 0))
;;; Generated autoloads from merlin-cap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-cap" '("merlin-c")))

;;;***

;;;### (autoloads nil "merlin-imenu" "merlin-imenu.el" (0 0 0 0))
;;; Generated autoloads from merlin-imenu.el

(autoload 'merlin-use-merlin-imenu "merlin-imenu" "\
Merlin: use the custom imenu feature from Merlin" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-imenu" '("merlin-imenu-")))

;;;***

;;;### (autoloads nil "merlin-xref" "merlin-xref.el" (0 0 0 0))
;;; Generated autoloads from merlin-xref.el

(autoload 'merlin-xref-backend "merlin-xref" "\
Merlin backend for Xref." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-xref" '("merlin-xref--line")))

;;;***

;;;### (autoloads nil nil ("merlin-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; merlin-autoloads.el ends here
