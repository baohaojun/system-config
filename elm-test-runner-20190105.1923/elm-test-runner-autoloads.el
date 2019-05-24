;;; elm-test-runner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elm-test-runner" "elm-test-runner.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from elm-test-runner.el

(autoload 'elm-test-runner-run "elm-test-runner" "\
Run elm-test on the current buffer's file.

\(fn)" t nil)

(autoload 'elm-test-runner-run-project "elm-test-runner" "\
Run elm-test on the whole project.

\(fn)" t nil)

(autoload 'elm-test-runner-run-directory "elm-test-runner" "\
Prompt for a directory on which to run specs.

\(fn)" t nil)

(autoload 'elm-test-runner-watch "elm-test-runner" "\
Run elm-test on the current buffer's file in watch mode.

\(fn)" t nil)

(autoload 'elm-test-runner-rerun "elm-test-runner" "\
Re-run the last elm-test invocation.

\(fn)" t nil)

(autoload 'elm-test-runner-toggle-test-and-target "elm-test-runner" "\
Switch to the test or the target file for the current buffer.
If the current buffer is visiting a test file, switches to the
target, otherwise the test.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-test-runner" '("elm-test-runner-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elm-test-runner-autoloads.el ends here
