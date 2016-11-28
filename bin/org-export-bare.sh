#!/bin/bash
cat <<EOF
; {%emacs-lisp%}

(let ((my-loaded-themes custom-enabled-themes)
      (interprogram-cut-function nil))
  (advice-remove 'org-html--build-pre/postamble 'ad-Advice-org-html--build-pre/postamble)
  (global-font-lock-mode)
  (when custom-enabled-themes
      (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
  (require 'ox-html)
  (find-file "$1")
  (org-html-export-to-html)
  (when my-loaded-themes
    (dolist (theme my-loaded-themes)
      (load-theme theme)))
  ())
; {%/emacs-lisp%}
EOF

# Local Variables:
# eval: (mmm-mode 1)
# End:
