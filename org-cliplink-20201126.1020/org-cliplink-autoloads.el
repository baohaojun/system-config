;;; org-cliplink-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-cliplink" "org-cliplink.el" (0 0 0 0))
;;; Generated autoloads from org-cliplink.el

(autoload 'org-cliplink-retrieve-title "org-cliplink" "\


\(fn URL TITLE-CALLBACK)" nil nil)

(autoload 'org-cliplink-insert-transformed-title "org-cliplink" "\
Takes the URL, asynchronously retrieves the title and applies
a custom TRANSFORMER which transforms the url and title and insert
the required text to the current buffer.

\(fn URL TRANSFORMER)" nil nil)

(autoload 'org-cliplink-retrieve-title-synchronously "org-cliplink" "\


\(fn URL)" nil nil)

(autoload 'org-cliplink "org-cliplink" "\
Takes a URL from the clipboard and inserts an org-mode link
with the title of a page found by the URL into the current
buffer" t nil)

(autoload 'org-cliplink-capture "org-cliplink" "\
org-cliplink version for org-capture templates.
Makes synchronous request. Returns the link instead of inserting
it to the current buffer. Doesn't support Basic Auth. Doesn't
support cURL transport." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-cliplink" '("org-cliplink-")))

;;;***

;;;### (autoloads nil "org-cliplink-string" "org-cliplink-string.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-cliplink-string.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-cliplink-string" '("org-cliplink-")))

;;;***

;;;### (autoloads nil "org-cliplink-transport" "org-cliplink-transport.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-cliplink-transport.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-cliplink-transport" '("org-cliplink-")))

;;;***

;;;### (autoloads nil nil ("org-cliplink-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-cliplink-autoloads.el ends here
