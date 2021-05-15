;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nim-capf" "nim-capf.el" (0 0 0 0))
;;; Generated autoloads from nim-capf.el

(autoload 'nim-capf-nimsuggest-completion-at-point "nim-capf" "\
Complete the symbol at point using nimsuggest." nil nil)

(autoload 'nim-builtin-completion-at-point "nim-capf" "\
Complete the symbol at point for .nim files." nil nil)

(autoload 'nimscript-builtin-completion-at-point "nim-capf" "\
Complete the symbol at point for nimscript files." nil nil)

(autoload 'nim-capf-setup "nim-capf" "\
Setup." nil nil)

(add-hook 'nimsuggest-mode-hook 'nim-capf-setup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-capf" '("nim-c")))

;;;***

;;;### (autoloads nil "nim-compile" "nim-compile.el" (0 0 0 0))
;;; Generated autoloads from nim-compile.el

(autoload 'nim-compile "nim-compile" "\
Compile and execute the current buffer as a nim file.
All output is written into the *nim-compile* buffer.
If you put COMMAND argument, you can specify the compilation command.

\(fn &optional COMMAND)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-compile" '("nim-")))

;;;***

;;;### (autoloads nil "nim-fill" "nim-fill.el" (0 0 0 0))
;;; Generated autoloads from nim-fill.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-fill" '("nim-fill-")))

;;;***

;;;### (autoloads nil "nim-helper" "nim-helper.el" (0 0 0 0))
;;; Generated autoloads from nim-helper.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-helper" '("nim-")))

;;;***

;;;### (autoloads nil "nim-mode" "nim-mode.el" (0 0 0 0))
;;; Generated autoloads from nim-mode.el

(autoload 'nim-mode "nim-mode" "\
A major mode for the Nim programming language.

\(fn)" t nil)

(autoload 'nimscript-mode "nim-mode" "\
A major-mode for NimScript files.
This major-mode is activated when you enter *.nims and *.nimble
suffixed files, but if it’s .nimble file, also another logic is
applied. See also ‘nimscript-mode-maybe’.

\(fn)" t nil)

(autoload 'nimscript-mode-maybe "nim-mode" "\
Most likely turn on ‘nimscript-mode’.
In *.nimble files, if the first line sentence matches
‘nim-nimble-ini-format-regex’, this function activates ‘conf-mode’
instead.  The default regex’s matching word is [Package]." t nil)

(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

(add-to-list 'auto-mode-alist '("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe))

(autoload 'nim-eldoc-function "nim-mode" "\
Return a doc string appropriate for the current context, or nil." t nil)

(autoload 'nim-eldoc-on "nim-mode" "\
This may or may not work.  Maybe this configuration has to set on.
Major-mode configuration according to the document." t nil)

(autoload 'nim-eldoc-setup "nim-mode" "\
This function may not work.
I tried to configure this stuff to be user definable, but currently failing.
The eldoc support should be turned on automatically, so please
use `nim-eldoc-off' manually if you don't like it.

\(fn &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-mode" '("nim-")))

;;;***

;;;### (autoloads nil "nim-rx" "nim-rx.el" (0 0 0 0))
;;; Generated autoloads from nim-rx.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-rx" '("nim-rx-constituents")))

;;;***

;;;### (autoloads nil "nim-smie" "nim-smie.el" (0 0 0 0))
;;; Generated autoloads from nim-smie.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-smie" '("nim-")))

;;;***

;;;### (autoloads nil "nim-suggest" "nim-suggest.el" (0 0 0 0))
;;; Generated autoloads from nim-suggest.el

(autoload 'nimsuggest-available-p "nim-suggest" "\
Return non-nil if nimsuggest is available in current buffer." nil nil)

(autoload 'nimsuggest-mode "nim-suggest" "\
Minor mode for nimsuggest.

If called interactively, enable Nimsuggest mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'nimsuggest-flymake-setup "nim-suggest" "\
Kinda experimental function to use flymake on Emacs 26." nil nil)

(autoload 'nimsuggest-eldoc--nimsuggest "nim-suggest" "\
Update `eldoc-last-message' by nimsuggest's information." nil nil)

(add-hook 'nimsuggest-mode-hook 'nimsuggest-xref-setup)

(autoload 'nimsuggest-xref-setup "nim-suggest" "\
Setup xref backend for nimsuggest." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-suggest" '("flymake-nimsuggest" "nim")))

;;;***

;;;### (autoloads nil "nim-syntax" "nim-syntax.el" (0 0 0 0))
;;; Generated autoloads from nim-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-syntax" '("nim")))

;;;***

;;;### (autoloads nil "nim-util" "nim-util.el" (0 0 0 0))
;;; Generated autoloads from nim-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-util" '("nim-")))

;;;***

;;;### (autoloads nil "nim-vars" "nim-vars.el" (0 0 0 0))
;;; Generated autoloads from nim-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-vars" '("nim")))

;;;***

;;;### (autoloads nil nil ("nim-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nim-mode-autoloads.el ends here
