;;; add-node-modules-path-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "add-node-modules-path" "add-node-modules-path.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from add-node-modules-path.el

(defvar add-node-modules-path-debug nil "\
Enable verbose output when non nil.")

(custom-autoload 'add-node-modules-path-debug "add-node-modules-path" t)

(defvar add-node-modules-max-depth 20 "\
Max depth to look for node_modules.")

(custom-autoload 'add-node-modules-max-depth "add-node-modules-path" t)

(autoload 'add-node-modules-path "add-node-modules-path" "\
Search the current buffer's parent directories for `node_modules/.bin`.
Traverse the directory structure up, until reaching the user's home directory,
 or hitting add-node-modules-max-depth.
Any path found is added to the `exec-path'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; add-node-modules-path-autoloads.el ends here
