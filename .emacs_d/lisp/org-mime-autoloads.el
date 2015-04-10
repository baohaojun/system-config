;;; org-mime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-mime" "org-mime.el" (21523 24296 677314
;;;;;;  496000))
;;; Generated autoloads from org-mime.el

(autoload 'org-mime-htmlize "org-mime" "\
Export a portion of an email body composed using `mml-mode' to
html using `org-mode'.  If called with an active region only
export that region, otherwise export the entire body.

\(fn ARG)" t nil)

(autoload 'org-mime-org-buffer-htmlize "org-mime" "\
Create an email buffer containing the current org-mode file
  exported to html and encoded in both html and in org formats as
  mime alternatives.

\(fn)" t nil)

(autoload 'org-mime-subtree "org-mime" "\
Create an email buffer containing the current org-mode subtree
  exported to a org format or to the format specified by the
  MAIL_FMT property of the subtree.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-mime-autoloads.el ends here
