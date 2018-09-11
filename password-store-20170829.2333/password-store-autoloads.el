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

\(fn ENTRY)" nil nil)

(autoload 'password-store-clear "password-store" "\
Clear password in kill ring.

\(fn)" t nil)

(autoload 'password-store-copy "password-store" "\
Add password for ENTRY to kill ring.

Clear previous password from kill ring.  Pointer to kill ring is
stored in `password-store-kill-ring-pointer'.  Password is cleared
after `password-store-timeout' seconds.

\(fn ENTRY)" t nil)

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
Show version of pass executable.

\(fn)" t nil)

(autoload 'password-store-url "password-store" "\
Browse URL stored in ENTRY.

This will only browse URLs that start with http:// or http:// to
avoid sending a password to the browser.

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
