;;; keydef-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "keydef" "keydef.el" (21480 34574 372981 364000))
;;; Generated autoloads from keydef.el

(autoload 'keydef "keydef" "\
Define the key sequence SEQ, written in kbd form, to run CMD.
CMD is automatically wrapped in an anonymous interactive function if it
is Emacs Lisp code rather than a command name. SEQ may also have the form
\(MODE SEQ) where the car is a mode name; for example

  (keydef (latex \"C-c %\") comment-region)

means to define the given key in latex-mode-map. And this will work even
if latex-mode is not loaded yet, provided that it is possible to deduce
the file that it will be loaded from, either from the autoload info or
by searching for a matching file name in the Emacs load path.

For best results, the \"mode name\" that you use here should yield the
proper foo-mode-map symbol when \"-mode-map\" is appended; although
this will normally match the mode name as given in the mode line,
Shell-script is one example I can think of where it doesn't---the map is
named sh-mode-map. The common cases that I know about, including
shell-script-mode and latex-mode, are handled as exceptions through the
variable mode-map-alist. But for other cases you will need to look up
the name of the mode-map that goes with the given mode.

\(fn SEQ &rest CMD)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; keydef-autoloads.el ends here
