;;; ivy-purpose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-purpose" "ivy-purpose.el" (0 0 0 0))
;;; Generated autoloads from ivy-purpose.el

(autoload 'ivy-purpose-switch-buffer-with-purpose "ivy-purpose" "\
Switch to buffer, choose from buffers with purpose PURPOSE.
PURPOSE defaults to the purpose of the current buffer.

\(fn &optional PURPOSE)" t nil)

(autoload 'ivy-purpose-switch-buffer-with-some-purpose "ivy-purpose" "\
Choose a purpose, then switch to a buffer with that purpose.

\(fn)" t nil)

(autoload 'ivy-purpose-switch-buffer-without-purpose "ivy-purpose" "\
Same as `ivy-switch-buffer', but disable window-purpose while the
command executes.

\(fn)" t nil)

(autoload 'ivy-purpose-setup "ivy-purpose" "\
Setup Ivy interface for Purpose.
Currently just sets `purpose-preferred-prompt' to 'vanilla.
Doesn't bind any keys.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("ivy-purpose-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-purpose-autoloads.el ends here
