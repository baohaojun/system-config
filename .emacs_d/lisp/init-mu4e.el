(when (file-exists-p "/usr/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
  (require 'mu4e))
