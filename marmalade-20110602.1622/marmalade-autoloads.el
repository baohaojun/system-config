;;; marmalade-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "marmalade" "marmalade.el" (21591 13998 319120
;;;;;;  97000))
;;; Generated autoloads from marmalade.el

(autoload 'marmalade-login "marmalade" "\
Log in to Marmalade and get the username and authentication token.
Prompt interactively for the user's username and password, then
use these to retreive the token.

CALLBACK is called when the login is completed, and passed the
username and the authentication token.

\(fn &optional CALLBACK)" t nil)

(autoload 'marmalade-register "marmalade" "\
Register a user with NAME, EMAIL, and PASSWORD.
The authentication token is passed to CALLBACK.

\(fn NAME EMAIL PASSWORD &optional CALLBACK)" t nil)

(autoload 'marmalade-upload-buffer "marmalade" "\
Upload the package in BUFFER.
BUFFER should be visiting an Elisp file or a tarball.

CALLBACK is called with the package object once the package is
uploaded.

\(fn BUFFER &optional CALLBACK)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; marmalade-autoloads.el ends here
