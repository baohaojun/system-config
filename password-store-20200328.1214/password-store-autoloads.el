;;; password-store-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "password-store" "password-store.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from password-store.el

(autoload 'password-store-edit "password-store" "\
Edit password for ENTRY.

\(fn ENTRY)" t nil)

(autoload 'password-store-get "password-store" "\
Return password for ENTRY.

Returns the first line of the password data.
When CALLBACK is non-`NIL', call CALLBACK with the first line instead.

\(fn ENTRY &optional CALLBACK)" nil nil)

(autoload 'password-store-get-field "password-store" "\
Return FIELD for ENTRY.
FIELD is a string, for instance \"url\". 
When CALLBACK is non-`NIL', call it with the line associated to FIELD instead.
If FIELD equals to symbol secret, then this function reduces to `password-store-get'.

\(fn ENTRY FIELD &optional CALLBACK)" nil nil)

(autoload 'password-store-clear "password-store" "\
Clear secret in the kill ring.

Optional argument FIELD, a symbol or a string, describes
the stored secret to clear; if nil, then set it to 'secret.
Note, FIELD does not affect the function logic; it is only used
to display the message:

\(message \"Field %s cleared.\" field).

\(fn &optional FIELD)" t nil)

(autoload 'password-store-copy "password-store" "\
Add password for ENTRY into the kill ring.

Clear previous password from the kill ring.  Pointer to the kill ring
is stored in `password-store-kill-ring-pointer'.  Password is cleared
after `password-store-time-before-clipboard-restore' seconds.

\(fn ENTRY)" t nil)

(autoload 'password-store-copy-field "password-store" "\
Add FIELD for ENTRY into the kill ring.

Clear previous secret from the kill ring.  Pointer to the kill ring is
stored in `password-store-kill-ring-pointer'.  Secret field is cleared
after `password-store-timeout' seconds.
If FIELD equals to symbol secret, then this function reduces to `password-store-copy'.

\(fn ENTRY FIELD)" t nil)

(autoload 'password-store-init "password-store" "\
Initialize new password store and use GPG-ID for encryption.

Separate multiple IDs with spaces.

\(fn GPG-ID)" t nil)

(autoload 'password-store-insert "password-store" "\
Insert a new ENTRY containing PASSWORD.

\(fn ENTRY PASSWORD)" t nil)

(autoload 'password-store-generate "password-store" "\
Generate a new password for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'.

\(fn ENTRY &optional PASSWORD-LENGTH)" t nil)

(autoload 'password-store-remove "password-store" "\
Remove existing password for ENTRY.

\(fn ENTRY)" t nil)

(autoload 'password-store-rename "password-store" "\
Rename ENTRY to NEW-ENTRY.

\(fn ENTRY NEW-ENTRY)" t nil)

(autoload 'password-store-version "password-store" "\
Show version of pass executable." t nil)

(autoload 'password-store-url "password-store" "\
Browse URL stored in ENTRY.

\(fn ENTRY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "password-store" '("password-store-")))

;;;***

;;;### (autoloads nil nil ("password-store-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; password-store-autoloads.el ends here
