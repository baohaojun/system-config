;;; window-purpose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "window-purpose" "window-purpose.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from window-purpose.el

(defvar purpose-mode nil "\
Non-nil if Purpose mode is enabled.
See the `purpose-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `purpose-mode'.")

(custom-autoload 'purpose-mode "window-purpose" nil)

(autoload 'purpose-mode "window-purpose" "\
Toggle Purpose mode on or off.

If called interactively, enable Purpose mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{purpose-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose" '("find-file-" "purpose-" "switch-buffer-")))

;;;***

;;;### (autoloads nil "window-purpose-configuration" "window-purpose-configuration.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-configuration.el

(eieio-defclass-autoload 'purpose-conf 'nil "window-purpose-configuration" nil)

(autoload 'purpose-set-extension-configuration "window-purpose-configuration" "\
Set an extension's entry in `purpose-extended-configuration'.
EXT-KEYWORD should be a keyword used to identify the extension.
CONFIG is a `purpose-conf' object containing the extension's purpose
configuration.
Example:
 (purpose-set-extension-configuration
     :python
     (purpose-conf \"py\"
                   :mode-purposes
                   '((python-mode . python)
                     (inferior-python-mode . interpreter))))

This function calls `purpose-compile-extended-configuration' when its
done.

\(fn EXT-KEYWORD CONFIG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-configuration" '("define-purpose-list-checker" "purpose-")))

;;;***

;;;### (autoloads nil "window-purpose-core" "window-purpose-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-core.el

(autoload 'purpose-toggle-window-buffer-dedicated "window-purpose-core" "\
Toggle window WINDOW's dedication to its current buffer on or off.
WINDOW defaults to the selected window.

\(fn &optional WINDOW)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-core" '("default-" "purpose-")))

;;;***

;;;### (autoloads nil "window-purpose-fixes" "window-purpose-fixes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-fixes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-fixes" '("purpose-")))

;;;***

;;;### (autoloads nil "window-purpose-layout" "window-purpose-layout.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-layout.el

(autoload 'purpose-save-window-layout-file "window-purpose-layout" "\
Save window layout of current frame to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead.

\(fn &optional FILENAME)" t nil)

(autoload 'purpose-load-window-layout-file "window-purpose-layout" "\
Load window layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead.

\(fn &optional FILENAME)" t nil)

(autoload 'purpose-save-window-layout "window-purpose-layout" "\
Save a window layout.
NAME is the name to give the window layout.
DIRECTORY is the directory in which to save the layout.

\(fn NAME DIRECTORY)" t nil)

(autoload 'purpose-load-window-layout "window-purpose-layout" "\
Load a window layout.
NAME is the name of a window layout.  If NAME is not given, prompt the
user for a name.
LAYOUT-DIRS is a list of directories to search for the layout file, and
defaults to `purpose-layout-dirs'.  If `purpose-use-built-in-layouts',
then `purpose--built-in-layouts-dir' is also searched.  See
`purpose-find-window-layout' for more details.

To load a window layout from a specific file, use
`purpose-load-window-layout-file'.

\(fn &optional NAME LAYOUT-DIRS)" t nil)

(autoload 'purpose-save-frame-layout-file "window-purpose-layout" "\
Save frame layout of Emacs to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead.

\(fn &optional FILENAME)" t nil)

(autoload 'purpose-load-frame-layout-file "window-purpose-layout" "\
Load frame layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead.

\(fn &optional FILENAME)" t nil)

(autoload 'purpose-save-frame-layout "window-purpose-layout" "\
Save a frame layout.
NAME is the name to give the frame layout.
DIRECTORY is the directory in which to save the layout.

\(fn NAME DIRECTORY)" t nil)

(autoload 'purpose-load-frame-layout "window-purpose-layout" "\
Load a frame layout.
NAME is the name of a frame layout.  If NAME is not given, prompt the
user for a name.
LAYOUT-DIRS is a list of directories to search for the layout file, and
defaults to `purpose-layout-dirs'.  If `purpose-use-built-in-layouts',
then `purpose--built-in-layouts-dir' is also searched.  See
`purpose-find-frame-layout' for more details.

To load a frame layout from a specific file, use
`purpose-load-frame-layout-file'.

\(fn &optional NAME LAYOUT-DIRS)" t nil)

(autoload 'purpose-delete-non-dedicated-windows "window-purpose-layout" "\
Delete all windows that aren't dedicated to their purpose or buffer." t nil)

(autoload 'purpose-set-window-purpose "window-purpose-layout" "\
Set window's purpose to PURPOSE, and dedicate it.
With prefix argument (DONT-DEDICATE is non-nil), don't dedicate the
window.  If DONT-DEDICATE is non-nil, and the current window is
dedicated, un-dedicate the window.
Changing the window's purpose is done by displaying a buffer of
the right purpose in it, or creating a dummy buffer.

\(fn PURPOSE &optional DONT-DEDICATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-layout" '("purpose-")))

;;;***

;;;### (autoloads nil "window-purpose-prefix-overload" "window-purpose-prefix-overload.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-prefix-overload.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-prefix-overload" '("define-purpose-prefix-overload" "purpose--")))

;;;***

;;;### (autoloads nil "window-purpose-switch" "window-purpose-switch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-switch.el

(autoload 'purpose-switch-buffer "window-purpose-switch" "\
Select buffer BUFFER-OR-NAME, preferably in the selected window.
If FORCE-SAME-WINDOW is non-nil, don't select a different window if the
currently selected window is not available.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'.

\(fn BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)" t nil)

(autoload 'purpose-switch-buffer-other-window "window-purpose-switch" "\
Select buffer BUFFER-OR-NAME in another window.
Never selects the currently selected window.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'.

\(fn BUFFER-OR-NAME &optional NORECORD)" t nil)

(autoload 'purpose-switch-buffer-other-frame "window-purpose-switch" "\
Select buffer BUFFER-OR-NAME, preferably in another frame.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'.

\(fn BUFFER-OR-NAME &optional NORECORD)" t nil)

(autoload 'purpose-pop-buffer "window-purpose-switch" "\
Select buffer BUFFER-OR-NAME, preferably in another window.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'.

\(fn BUFFER-OR-NAME &optional NORECORD)" t nil)

(autoload 'purpose-pop-buffer-same-window "window-purpose-switch" "\
Select buffer BUFFER-OR-NAME, preferably in the selected window.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'.

\(fn BUFFER-OR-NAME &optional NORECORD)" t nil)

(autoload 'purpose-switch-buffer-with-purpose "window-purpose-switch" "\
Prompt the user and switch to a buffer with purpose PURPOSE.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose.

\(fn &optional PURPOSE)" t nil)

(autoload 'purpose-switch-buffer-with-some-purpose "window-purpose-switch" "\
Like `purpose-switch-buffer-with-purpose', but first choose a PURPOSE.

\(fn PURPOSE)" t nil)

(autoload 'purpose-switch-buffer-with-purpose-other-window "window-purpose-switch" "\
Prompt the user and switch to a buffer with purpose PURPOSE.
The buffer is display in another window.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose.

\(fn &optional PURPOSE)" t nil)

(autoload 'purpose-switch-buffer-with-purpose-other-frame "window-purpose-switch" "\
Prompt the user and switch to a buffer with purpose PURPOSE.
The buffer is display in another frame.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose.

\(fn &optional PURPOSE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-switch" '("purpose-" "without-purpose")))

;;;***

;;;### (autoloads nil "window-purpose-utils" "window-purpose-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from window-purpose-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-utils" '("purpose-")))

;;;***

;;;### (autoloads nil "window-purpose-x" "window-purpose-x.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from window-purpose-x.el

(autoload 'purpose-x-code1-setup "window-purpose-x" "\
Setup purpose-x-code1.
This setup includes 4 windows:
1. dedicated 'edit window
2. dedicated 'dired window.  This window shows the current buffer's
directory in a special window, using `dired' and
`dired-hide-details-mode' (if available).
3. dedicated 'buffers window.  This window shows the currently open
files, using `ibuffer'.
4. dedicated 'ilist window.  This window shows the current buffer's
imenu." t nil)

(autoload 'purpose-x-magit-single-on "window-purpose-x" "\
Turn on magit-single purpose configuration." t nil)

(autoload 'purpose-x-magit-multi-on "window-purpose-x" "\
Turn on magit-multi purpose configuration." t nil)

(autoload 'purpose-x-golden-ratio-setup "window-purpose-x" "\
Make `golden-ratio-mode' aware of `purpose-mode'." t nil)

(autoload 'purpose-x-popwin-setup "window-purpose-x" "\
Activate `popwin' emulation.
This extension treats certain buffers as \"popup\" buffers and displays
them in a special popup window.
The window is closed automatically when selecting another buffer (via
`switch-to-buffer' and the like), or by pressing \\[keyboard-quit].
You can control which buffers are treated as popup buffers by changing
the variables `purpose-x-popwin-major-modes',
`purpose-x-popwin-buffer-names' and
`purpose-x-popwin-buffer-name-regexps'.
Look at `purpose-x-popwin-*' variables and functions to learn more." t nil)

(autoload 'purpose-x-persp-setup "window-purpose-x" "\
Activate purpose-x-persp extension.
This extension automatically activates a purpose configuration for the
current perspective.  The configuration changes automatically when
switching perspectives or when toggling `persp-mode'.
The variable `purpose-x-persp-confs' matches between perspectives and
purpose configurations." t nil)

(autoload 'purpose-x-persp-switch-buffer "window-purpose-x" "\
Switch to BUFFER, limited by purpose and perspective.
BUFFER is chosen from buffers with the same purpose as the current
buffer that are also part of the current perspective.
NORECORD and FORCE-SAME-WINDOW have the same meaning as in
`switch-to-buffer'.

\(fn BUFFER &optional NORECORD FORCE-SAME-WINDOW)" t nil)

(autoload 'purpose-x-persp-switch-buffer-other-window "window-purpose-x" "\
Switch to BUFFER in other window, limited by purpose and perspective.
NORECORD has the same meaning as in `switch-to-buffer-other-window'.
The relation between `purpose-x-persp-switch-buffer-other-window' and
`switch-to-buffer-other-window' is the same as the relation between
`purpose-x-persp-switch-buffer' and `switch-to-buffer'.

\(fn BUFFER &optional NORECORD)" t nil)

(autoload 'purpose-x-persp-switch-buffer-other-frame "window-purpose-x" "\
Switch to BUFFER in other frame, limited by purpose and perspective.
NORECORD has the same meaning as in `switch-to-buffer-other-frame'.
The relation between `purpose-x-persp-switch-buffer-other-frame' and
`switch-to-buffer-other-frame' is the same as the relation between
`purpose-x-persp-switch-buffer' and `switch-to-buffer'.

\(fn BUFFER &optional NORECORD)" t nil)

(autoload 'purpose-x-kill-setup "window-purpose-x" "\
Activate purpose-x-kill extension.
This extension makes `kill-buffer' aware of the purpose-dedicated window
parameter, when killing a visible buffer.  If a buffer that is being
killed is displayed in a window,and that window is purpose-dedicated,
then try to replace the buffer with another buffer with the same purpose.
If that isn't possible, treat the window as if it was buffer-dedicated.

This is implemented by overriding `replace-buffer-in-windows' with
`purpose-x-replace-buffer-in-windows-1'.  See
`purpose-x-replace-buffer-in-windows-1' for more details." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-purpose-x" '("purpose-x-")))

;;;***

;;;### (autoloads nil nil ("window-purpose-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; window-purpose-autoloads.el ends here
