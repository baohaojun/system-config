;;; expand-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cc-mode-expansions" "cc-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cc-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cc-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "clojure-mode-expansions" "clojure-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from clojure-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "clojure-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "cperl-mode-expansions" "cperl-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cperl-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cperl-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "css-mode-expansions" "css-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from css-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "css-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "enh-ruby-mode-expansions" "enh-ruby-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from enh-ruby-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "enh-ruby-mode-expansions" '("er/add-enh-ruby-mode-expansions")))

;;;***

;;;### (autoloads nil "er-basic-expansions" "er-basic-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from er-basic-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "er-basic-expansions" '("er--" "er/mark-")))

;;;***

;;;### (autoloads nil "erlang-mode-expansions" "erlang-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erlang-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-mode-expansions" '("er/add-erlang-mode-expansions")))

;;;***

;;;### (autoloads nil "expand-region" "expand-region.el" (0 0 0 0))
;;; Generated autoloads from expand-region.el

(autoload 'er/expand-region "expand-region" "\
Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "expand-region-core" "expand-region-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from expand-region-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "expand-region-core" '("er--" "er/")))

;;;***

;;;### (autoloads nil "expand-region-custom" "expand-region-custom.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from expand-region-custom.el

(let ((loads (get 'expand-region 'custom-loads))) (if (member '"expand-region-custom" loads) nil (put 'expand-region 'custom-loads (cons '"expand-region-custom" loads))))

(defvar expand-region-preferred-python-mode 'python "\
The name of your preferred python mode")

(custom-autoload 'expand-region-preferred-python-mode "expand-region-custom" t)

(defvar expand-region-guess-python-mode t "\
If expand-region should attempt to guess your preferred python mode")

(custom-autoload 'expand-region-guess-python-mode "expand-region-custom" t)

(defvar expand-region-autocopy-register "" "\
If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character.")

(custom-autoload 'expand-region-autocopy-register "expand-region-custom" t)

(defvar expand-region-skip-whitespace t "\
If expand-region should skip past whitespace on initial expansion")

(custom-autoload 'expand-region-skip-whitespace "expand-region-custom" t)

(defvar expand-region-fast-keys-enabled t "\
If expand-region should bind fast keys after initial expand/contract")

(custom-autoload 'expand-region-fast-keys-enabled "expand-region-custom" t)

(defvar expand-region-contract-fast-key "-" "\
Key to use after an initial expand/contract to contract once more.")

(custom-autoload 'expand-region-contract-fast-key "expand-region-custom" t)

(defvar expand-region-reset-fast-key "0" "\
Key to use after an initial expand/contract to undo.")

(custom-autoload 'expand-region-reset-fast-key "expand-region-custom" t)

(defvar expand-region-exclude-text-mode-expansions '(html-mode nxml-mode) "\
List of modes which derive from `text-mode' for which text mode expansions are not appropriate.")

(custom-autoload 'expand-region-exclude-text-mode-expansions "expand-region-custom" t)

(defvar expand-region-smart-cursor nil "\
Defines whether the cursor should be placed intelligently after expansion.

If set to t, and the cursor is already at the beginning of the new region,
keep it there; otherwise, put it at the end of the region.

If set to nil, always place the cursor at the beginning of the region.")

(custom-autoload 'expand-region-smart-cursor "expand-region-custom" t)

(define-obsolete-variable-alias 'er/enable-subword-mode\? 'expand-region-subword-enabled "2019-03-23")

(defvar expand-region-subword-enabled nil "\
Whether expand-region should use subword expansions.")

(custom-autoload 'expand-region-subword-enabled "expand-region-custom" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "expand-region-custom" '("expand-region-guess-python-mode")))

;;;***

;;;### (autoloads nil "feature-mode-expansions" "feature-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from feature-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "feature-mode-expansions" '("er--block-between-keywords" "er/")))

;;;***

;;;### (autoloads nil "html-mode-expansions" "html-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from html-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "html-mode-expansions" '("er--" "er/")))

;;;***

;;;### (autoloads nil "js-mode-expansions" "js-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from js-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "js2-mode-expansions" "js2-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from js2-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2-mode-expansions" '("er/add-js2-mode-expansions" "js2-mark-parent-statement")))

;;;***

;;;### (autoloads nil "jsp-expansions" "jsp-expansions.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jsp-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jsp-expansions" '("er/")))

;;;***

;;;### (autoloads nil "latex-mode-expansions" "latex-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from latex-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "latex-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "nxml-mode-expansions" "nxml-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from nxml-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nxml-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "octave-expansions" "octave-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from octave-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "octave-expansions" '("er/")))

;;;***

;;;### (autoloads nil "python-el-expansions" "python-el-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from python-el-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-el-expansions" '("er--python-string-delimiter" "er/")))

;;;***

;;;### (autoloads nil "python-el-fgallina-expansions" "python-el-fgallina-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from python-el-fgallina-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-el-fgallina-expansions" '("er--python-" "er/")))

;;;***

;;;### (autoloads nil "python-mode-expansions" "python-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from python-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-mode-expansions" '("er--" "er/" "py-goto-beyond-clause")))

;;;***

;;;### (autoloads nil "ruby-mode-expansions" "ruby-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ruby-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ruby-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "sml-mode-expansions" "sml-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from sml-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sml-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "subword-mode-expansions" "subword-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from subword-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "subword-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "text-mode-expansions" "text-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from text-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "text-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "the-org-mode-expansions" "the-org-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from the-org-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "the-org-mode-expansions" '("er/")))

;;;***

;;;### (autoloads nil "web-mode-expansions" "web-mode-expansions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from web-mode-expansions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "web-mode-expansions" '("er/add-web-mode-expansions")))

;;;***

;;;### (autoloads nil nil ("expand-region-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; expand-region-autoloads.el ends here
