;;; ibuffer-vc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ibuffer-vc" "ibuffer-vc.el" (21927 14997 973833
;;;;;;  816000))
;;; Generated autoloads from ibuffer-vc.el

(autoload 'ibuffer-vc-generate-filter-groups-by-vc-root "ibuffer-vc" "\
Create a set of ibuffer filter groups based on the vc root dirs of buffers.

\(fn)" nil nil)

(autoload 'ibuffer-vc-set-filter-groups-by-vc-root "ibuffer-vc" "\
Set the current filter groups to filter by vc root dir.

\(fn)" t nil)
 (autoload 'ibuffer-make-column-vc-status "ibuffer-vc")
 (autoload 'ibuffer-make-column-vc-status-mini "ibuffer-vc")
 (autoload 'ibuffer-do-sort-by-vc-status "ibuffer-vc")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ibuffer-vc-autoloads.el ends here
