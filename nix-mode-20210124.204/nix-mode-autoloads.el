;;; nix-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nix" "nix.el" (0 0 0 0))
;;; Generated autoloads from nix.el

(autoload 'pcomplete/nix "nix" "\
Completion for the nix command." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix" '("nix-")))

;;;***

;;;### (autoloads nil "nix-build" "nix-build.el" (0 0 0 0))
;;; Generated autoloads from nix-build.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-build" '("nix-build")))

;;;***

;;;### (autoloads nil "nix-drv-mode" "nix-drv-mode.el" (0 0 0 0))
;;; Generated autoloads from nix-drv-mode.el

(autoload 'nix-drv-mode "nix-drv-mode" "\
Pretty print Nix’s .drv files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\`/nix/store/.+\\.drv\\'" . nix-drv-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-drv-mode" '("nix-drv-mode-dejsonify-buffer")))

;;;***

;;;### (autoloads nil "nix-edit" "nix-edit.el" (0 0 0 0))
;;; Generated autoloads from nix-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-edit" '("nix-edit")))

;;;***

;;;### (autoloads nil "nix-format" "nix-format.el" (0 0 0 0))
;;; Generated autoloads from nix-format.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-format" '("nix-")))

;;;***

;;;### (autoloads nil "nix-instantiate" "nix-instantiate.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from nix-instantiate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-instantiate" '("nix-instantiate")))

;;;***

;;;### (autoloads nil "nix-log" "nix-log.el" (0 0 0 0))
;;; Generated autoloads from nix-log.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-log" '("nix-log")))

;;;***

;;;### (autoloads nil "nix-mode" "nix-mode.el" (0 0 0 0))
;;; Generated autoloads from nix-mode.el

(autoload 'nix-mode-format "nix-mode" "\
Format the entire `nix-mode' buffer." t nil)

(autoload 'nix-indent-line "nix-mode" "\
Indent current line in a Nix expression." t nil)

(autoload 'nix-indent-region "nix-mode" "\
Indent on a whole region. Enabled by default.
START where to start in region.
END where to end the region.

\(fn START END)" t nil)

(autoload 'nix-mode-ffap-nixpkgs-path "nix-mode" "\
Support `ffap' for <nixpkgs> declarations.
If STR contains brackets, call `nix-instantiate' to find the
location of STR. If `nix-instantiate' has a nonzero exit code,
don’t do anything

\(fn STR)" nil nil)

(autoload 'nix-mode "nix-mode" "\
Major mode for editing Nix expressions.

The following commands may be useful:

  '\\[newline-and-indent]'
    Insert a newline and move the cursor to align with the previous
    non-empty line.

  '\\[fill-paragraph]'
    Refill a paragraph so that all lines are at most `fill-column'
    lines long.  This should do the right thing for comments beginning
    with `#'.  However, this command doesn't work properly yet if the
    comment is adjacent to code (i.e., no intervening empty lines).
    In that case, select the text to be refilled and use
    `\\[fill-region]' instead.

The hook `nix-mode-hook' is run when Nix mode is started.

\\{nix-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-mode" '("nix-")))

;;;***

;;;### (autoloads nil "nix-prettify-mode" "nix-prettify-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from nix-prettify-mode.el

(autoload 'nix-prettify-mode "nix-prettify-mode" "\
Toggle Nix Prettify mode.

With a prefix argument ARG, enable Nix Prettify mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Nix Prettify mode is enabled, hash-parts of the Nix store
file names (see `nix-prettify-regexp') are prettified,
i.e. displayed as `nix-prettify-char' character.  This mode can
be enabled programmatically using hooks:

  (add-hook 'shell-mode-hook 'nix-prettify-mode)

It is possible to enable the mode in any buffer, however not any
buffer's highlighting may survive after adding new elements to
`font-lock-keywords' (see `nix-prettify-special-modes' for
details).

Also you can use `global-nix-prettify-mode' to enable Nix
Prettify mode for all modes that support font-locking.

\(fn &optional ARG)" t nil)

(put 'nix-prettify-global-mode 'globalized-minor-mode t)

(defvar nix-prettify-global-mode nil "\
Non-nil if Nix-Prettify-Global mode is enabled.
See the `nix-prettify-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nix-prettify-global-mode'.")

(custom-autoload 'nix-prettify-global-mode "nix-prettify-mode" nil)

(autoload 'nix-prettify-global-mode "nix-prettify-mode" "\
Toggle Nix-Prettify mode in all buffers.
With prefix ARG, enable Nix-Prettify-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Nix-Prettify mode is enabled in all buffers where
`nix-prettify-turn-on' would do it.
See `nix-prettify-mode' for more information on Nix-Prettify mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'global-nix-prettify-mode 'nix-prettify-global-mode "v1.2.2")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-prettify-mode" '("nix-prettify-")))

;;;***

;;;### (autoloads nil "nix-repl" "nix-repl.el" (0 0 0 0))
;;; Generated autoloads from nix-repl.el

(autoload 'nix-repl "nix-repl" "\
Load the Nix-REPL." t nil)

(autoload 'nix-repl-completion-at-point "nix-repl" "\
Completion at point function for Nix using \"nix-repl\".
See `completion-at-point-functions'." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-repl" '("nix-")))

;;;***

;;;### (autoloads nil "nix-search" "nix-search.el" (0 0 0 0))
;;; Generated autoloads from nix-search.el

(autoload 'nix-search "nix-search" "\
Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in.

\(fn &optional SEARCH FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-search" '("nix-search-read-attr")))

;;;***

;;;### (autoloads nil "nix-shebang" "nix-shebang.el" (0 0 0 0))
;;; Generated autoloads from nix-shebang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-shebang" '("nix-shebang-")))

;;;***

;;;### (autoloads nil "nix-shell" "nix-shell.el" (0 0 0 0))
;;; Generated autoloads from nix-shell.el

(autoload 'nix-shell-unpack "nix-shell" "\
Run Nix’s unpackPhase.
FILE is the file to unpack from.
ATTR is the attribute to unpack.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-configure "nix-shell" "\
Run Nix’s configurePhase.
FILE is the file to configure from.
ATTR is the attribute to configure.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-build "nix-shell" "\
Run Nix’s buildPhase.
FILE is the file to build from.
ATTR is the attribute to build.

\(fn FILE ATTR)" t nil)

(autoload 'nix-eshell-with-packages "nix-shell" "\
Create an Eshell buffer that has the shell environment in it.
PACKAGES a list of packages to pull in.
PKGS-FILE a file to use to get the packages.

\(fn PACKAGES &optional PKGS-FILE)" nil nil)

(autoload 'nix-eshell "nix-shell" "\
Create an Eshell buffer that has the shell environment in it.
FILE the .nix expression to create a shell for.
ATTR attribute to instantiate in NIX-FILE.

\(fn FILE &optional ATTR)" t nil)

(autoload 'nix-shell-with-string "nix-shell" "\
A nix-shell emulator in Emacs from a string.
STRING the nix expression to use.

\(fn STRING)" nil nil)

(autoload 'nix-shell "nix-shell" "\
A nix-shell emulator in Emacs.
FILE the file to instantiate.
ATTR an attribute of the Nix file to use.

\(fn FILE &optional ATTR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-shell" '("nix-")))

;;;***

;;;### (autoloads nil "nix-store" "nix-store.el" (0 0 0 0))
;;; Generated autoloads from nix-store.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-store" '("nix-store-realise")))

;;;***

;;;### (autoloads nil nil ("nix-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nix-mode-autoloads.el ends here
