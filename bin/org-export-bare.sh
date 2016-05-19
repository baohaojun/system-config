#!/bin/bash
cat <<EOF
; {%emacs-lisp%}

(let ((my-loaded-themes custom-enabled-themes))
  (global-font-lock-mode)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (require 'ox-html)
  (find-file "$1")
  (org-html-export-to-html)
  (kill-buffer "$(basename "$1")")
  (dolist (theme my-loaded-themes)
    (load-theme theme))
  ())
; {%/emacs-lisp%}
EOF

# Local Variables:
# eval: (mmm-mode 1)
# End:
