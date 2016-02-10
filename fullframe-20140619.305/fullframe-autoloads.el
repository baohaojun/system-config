;;; fullframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "fullframe" "fullframe.el" (21703 15740 279434
;;;;;;  813000))
;;; Generated autoloads from fullframe.el

(autoload 'fullframe "fullframe" "\
Save window/frame state when executing COMMAND-ON.

Advises COMMAND-ON so that the buffer it displays will appear in
a full-frame window.  The previous window configuration will be
restored when COMMAND-OFF is executed in that buffer.  If
KILL-ON-COFF is non-nil, then the buffer will also be killed
after COMMAND-OFF has completed.

IGNORED is there for backcompatibillitys sake -- ignore it.

\(fn COMMAND-ON COMMAND-OFF &optional KILL-ON-COFF IGNORED)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fullframe-autoloads.el ends here
