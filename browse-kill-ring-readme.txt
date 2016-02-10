Ever feel that 'C-y M-y M-y M-y ...' is not a great way of trying
to find that piece of text you know you killed a while back?  Then
browse-kill-ring.el is for you.

This package is simple to install; add (require 'browse-kill-ring)
to your ~/.emacs file, after placing this file somewhere in your
`load-path'.  If you want to use 'M-y' to invoke
`browse-kill-ring', also add (browse-kill-ring-default-keybindings)
to your ~/.emacs file.  Alternatively, you can bind it to another
key such as "C-c k", with:
(global-set-key (kbd "C-c k") 'browse-kill-ring)

Note that the command keeps track of the last window displayed to
handle insertion of chosen text; this might have unexpected
consequences if you do 'M-x browse-kill-ring', then switch your
window configuration, and try to use the same *Kill Ring* buffer
again.
