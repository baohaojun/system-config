;;; ibuffer-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ibuffer-projectile" "ibuffer-projectile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ibuffer-projectile.el
 (autoload 'ibuffer-make-column-project-name "ibuffer-projectile")
 (autoload 'ibuffer-do-sort-by-project-name "ibuffer-projectile")
 (autoload 'ibuffer-make-column-project-relative-file "ibuffer-projectile")

(autoload 'ibuffer-projectile-generate-filter-groups "ibuffer-projectile" "\
Create a set of ibuffer filter groups based on the projectile root dirs of buffers." nil nil)

(autoload 'ibuffer-projectile-set-filter-groups "ibuffer-projectile" "\
Set the current filter groups to filter by projectile root dir." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ibuffer-projectile" '("ibuffer-projectile-" "project")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ibuffer-projectile-autoloads.el ends here
