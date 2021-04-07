;;; edit-indirect-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edit-indirect" "edit-indirect.el" (0 0 0 0))
;;; Generated autoloads from edit-indirect.el

(autoload 'edit-indirect-region "edit-indirect" "\
Edit the region BEG..END in a separate buffer.
The region is copied, without text properties, to a separate
buffer, called edit-indirect buffer, and
`edit-indirect-guess-mode-function' is called to set the major
mode.
When done, exit with `edit-indirect-commit', which will remove the
original region and replace it with the edited version; or with
`edit-indirect-abort', which will drop the modifications.

This differs from `clone-indirect-buffer' with narrowing in that
the text properties are not shared, so the parent buffer major mode
and the edit-indirect buffer major mode will not be able to tread
on each other's toes by setting up potentially conflicting text
properties, which happens surprisingly often when the font-lock
mode is used.

Edit-indirect buffers use the `edit-indirect-mode-map' keymap.

If there's already an edit-indirect buffer for BEG..END, use that.
If there's already an edit-indirect buffer active overlapping any
portion of BEG..END, an `edit-indirect-overlapping' error is
signaled.

When DISPLAY-BUFFER is non-nil or when called interactively,
display the edit-indirect buffer in some window and select it.

In any case, return the edit-indirect buffer.

\(fn BEG END &optional DISPLAY-BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edit-indirect" '("edit-indirect-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edit-indirect-autoloads.el ends here
