;;; merlin-eldoc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "merlin-eldoc" "merlin-eldoc.el" (0 0 0 0))
;;; Generated autoloads from merlin-eldoc.el

(autoload 'merlin-eldoc-setup "merlin-eldoc" "\
Setup eldoc for OCaml/Reasonml based on merlin." t nil)

(autoload 'merlin-eldoc-disable "merlin-eldoc" "\
Cleanup hooks created by merlin-eldoc." t nil)

(autoload 'merlin-eldoc-customize "merlin-eldoc" "\
Open the customize buffer for the group merlin-eldoc." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-eldoc" '("merlin-eldoc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; merlin-eldoc-autoloads.el ends here
