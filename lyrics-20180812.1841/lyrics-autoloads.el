;;; lyrics-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lyrics" "lyrics.el" (0 0 0 0))
;;; Generated autoloads from lyrics.el

(autoload 'lyrics-edit "lyrics" "\
Edit ARTIST SONG LYRICS in `lyrics-directory'.

\(fn ARTIST SONG)" t nil)

(autoload 'lyrics "lyrics" "\
Browse lyrics wiki from ARTIST SONG in BUFFER.

\(fn ARTIST SONG &optional BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lyrics" '("lyrics-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lyrics-autoloads.el ends here
