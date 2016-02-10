;;; hippie-expand-slime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (set-up-slime-hippie-expand try-expand-slime-fuzzy
;;;;;;  try-expand-slime) "hippie-expand-slime" "hippie-expand-slime.el"
;;;;;;  (21286 54625 691963 540000))
;;; Generated autoloads from hippie-expand-slime.el

(autoload 'try-expand-slime "hippie-expand-slime" "\
Simple slime completion function for `hippie-expand'.

\(fn OLD)" nil nil)

(autoload 'try-expand-slime-fuzzy "hippie-expand-slime" "\
Fuzzy slime completion function for `hippie-expand'.

\(fn OLD)" nil nil)

(autoload 'set-up-slime-hippie-expand "hippie-expand-slime" "\
Add an optionally-fuzzy slime completion function to the front of
`hippie-expand-try-functions-list' for the current buffer.

\(fn &optional FUZZY)" t nil)

;;;***

;;;### (autoloads nil nil ("hippie-expand-slime-pkg.el") (21286 54625
;;;;;;  792212 213000))

;;;***

(provide 'hippie-expand-slime-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hippie-expand-slime-autoloads.el ends here
