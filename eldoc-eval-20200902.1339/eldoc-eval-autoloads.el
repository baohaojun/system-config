;;; eldoc-eval-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eldoc-eval" "eldoc-eval.el" (0 0 0 0))
;;; Generated autoloads from eldoc-eval.el

(defvar eldoc-in-minibuffer-mode nil "\
Non-nil if Eldoc-In-Minibuffer mode is enabled.
See the `eldoc-in-minibuffer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eldoc-in-minibuffer-mode'.")

(custom-autoload 'eldoc-in-minibuffer-mode "eldoc-eval" nil)

(autoload 'eldoc-in-minibuffer-mode "eldoc-eval" "\
Show eldoc for current minibuffer input.

If called interactively, enable Eldoc-In-Minibuffer mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'eldoc-eval-expression "eldoc-eval" "\
Eval expression with eldoc support in mode-line." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eldoc-eval" '("eldoc-" "with-eldoc-in-minibuffer")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eldoc-eval-autoloads.el ends here
