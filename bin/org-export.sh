#!/bin/bash
cat <<EOF
; {%emacs-lisp%}
(let (old-org-html-style-include-default
      old-org-export-with-toc
      org-export-with-timestamps
      org-export-time-stamp-file
      org-html-htmlize-output-type
      org-html-head-extra
      org-html-preamble
      org-html-postamble
      (load-path load-path))
  (global-font-lock-mode)
  ; (toggle-debug-on-error)
  (setq bhj-org-export-file "$ORG_FILE_NAME")
  (setq load-path
      (nconc (list
              "/usr/share/emacs/site-lisp/gnus"
              "/opt/local/share/emacs/site-lisp/gnus"
              (expand-file-name "~/src/gnus")
              (expand-file-name "~/src/bbdb/lisp")
              (expand-file-name (concat "~/system-config/.emacs_d/" (symbol-name system-type)))
              (expand-file-name "~/system-config/.emacs_d/lisp")
              (expand-file-name "~/system-config/.emacs_d/skeleton-complete")
              (expand-file-name "~/system-config/.emacs_d/org-confluence")
              (expand-file-name "~/system-config/.emacs_d/org-jira")
              (expand-file-name "~/system-config/.emacs_d/mo-git-blame")
              (expand-file-name "~/system-config/.emacs_d/lisp/ext")
              (expand-file-name "~/system-config/.emacs_d/weblogger")
              (expand-file-name "~/system-config/.emacs_d/org2blog")
              (expand-file-name "~/src/github/org-mode/lisp")
              (expand-file-name "~/src/github/helm")
              (expand-file-name "~/src/github/org-mode/contrib/lisp"))
             load-path))
  (setq org-html-style-include-default nil
        org-export-with-toc nil
        org-export-with-timestamps nil
        org-export-time-stamp-file nil
        org-html-htmlize-output-type (quote css)
        org-html-head-extra ""
        org-html-preamble ""
        org-html-postamble "")

  (setq org-use-sub-superscripts (quote {})
        org-export-with-sub-superscripts (quote {}))
  (require 'ox-html)

  (find-file "$1")
  (org-html-export-to-html)
  (kill-buffer "$(basename "$1")")
  ())
; {%/emacs-lisp%}
EOF

# Local Variables:
# eval: (mmm-mode 1)
# End:
