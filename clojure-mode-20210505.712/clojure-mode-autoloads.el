;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clojure-mode" "clojure-mode.el" (0 0 0 0))
;;; Generated autoloads from clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code.

\\{clojure-mode-map}

\(fn)" t nil)

(autoload 'clojure-unwind "clojure-mode" "\
Unwind thread at point or above point by N levels.
With universal argument \\[universal-argument], fully unwind thread.

\(fn &optional N)" t nil)

(autoload 'clojure-unwind-all "clojure-mode" "\
Fully unwind thread at point or above point." t nil)

(autoload 'clojure-thread "clojure-mode" "\
Thread by one more level an existing threading macro." t nil)

(autoload 'clojure-thread-first-all "clojure-mode" "\
Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-thread-last-all "clojure-mode" "\
Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-cycle-privacy "clojure-mode" "\
Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy" t nil)

(autoload 'clojure-convert-collection-to-list "clojure-mode" "\
Convert collection at (point) to list." t nil)

(autoload 'clojure-convert-collection-to-quoted-list "clojure-mode" "\
Convert collection at (point) to quoted list." t nil)

(autoload 'clojure-convert-collection-to-map "clojure-mode" "\
Convert collection at (point) to map." t nil)

(autoload 'clojure-convert-collection-to-vector "clojure-mode" "\
Convert collection at (point) to vector." t nil)

(autoload 'clojure-convert-collection-to-set "clojure-mode" "\
Convert collection at (point) to set." t nil)

(autoload 'clojure-cycle-if "clojure-mode" "\
Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if" t nil)

(autoload 'clojure-cycle-when "clojure-mode" "\
Change a surrounding when to when-not, or vice-versa." t nil)

(autoload 'clojure-let-backward-slurp-sexp "clojure-mode" "\
Slurp the s-expression before the let form into the let form.
With a numeric prefix argument slurp the previous N s-expressions
into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-let-forward-slurp-sexp "clojure-mode" "\
Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions
into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-introduce-let "clojure-mode" "\
Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up.

\(fn &optional N)" t nil)

(autoload 'clojure-move-to-let "clojure-mode" "\
Move the form at point to a binding in the nearest let." t nil)

(autoload 'clojure-rename-ns-alias "clojure-mode" "\
Rename a namespace alias.
If a region is active, only pick up and rename aliases within the region." t nil)

(autoload 'clojure-add-arity "clojure-mode" "\
Add an arity to a function." t nil)

(autoload 'clojurescript-mode "clojure-mode" "\
Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}

\(fn)" t nil)

(autoload 'clojurec-mode "clojure-mode" "\
Major mode for editing ClojureC code.

\\{clojurec-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "clojure-mode" '("add-custom-clojure-indents" "clojure" "define-clojure-indent" "put-clojure-indent")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-mode-autoloads.el ends here
