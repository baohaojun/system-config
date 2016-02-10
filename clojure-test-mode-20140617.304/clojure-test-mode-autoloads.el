;;; clojure-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "clojure-test-mode" "clojure-test-mode.el"
;;;;;;  (21591 25926 542745 979000))
;;; Generated autoloads from clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-test-mode" "\
A minor mode for running Clojure tests.

\\{clojure-test-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'clojure-find-clojure-test "clojure-test-mode" "\


\(fn)" nil nil)

(defun clojure-test-maybe-enable nil "\
Enable clojure-test-mode if the current buffer contains a \"clojure.test\" bit in it." (when (clojure-find-clojure-test) (save-window-excursion (clojure-test-mode t))))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-test-mode-autoloads.el ends here
