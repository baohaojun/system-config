;;; racer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "racer" "racer.el" (0 0 0 0))
;;; Generated autoloads from racer.el

(autoload 'racer-find-definition "racer" "\
Run the racer find-definition command and process the results." t nil)

(autoload 'racer-find-definition-other-window "racer" "\
Run the racer find-definition command and process the results." t nil)

(autoload 'racer-find-definition-other-frame "racer" "\
Run the racer find-definition command and process the results." t nil)

(autoload 'racer-mode "racer" "\
Minor mode for racer.

If called interactively, enable Racer mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "racer" '("racer-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racer-autoloads.el ends here
