#!/bin/bash
cat <<EOF
; {%emacs-lisp%}
(progn
  (global-font-lock-mode)
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
