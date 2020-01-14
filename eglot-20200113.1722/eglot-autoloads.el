;;; eglot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eglot" "eglot.el" (0 0 0 0))
;;; Generated autoloads from eglot.el

(autoload 'eglot "eglot" "\
Manage a project with a Language Server Protocol (LSP) server.

The LSP server of CLASS started (or contacted) via CONTACT.  If
this operation is successful, current *and future* file buffers
of MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', `completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CLASS and CONTACT from
`eglot-server-programs' and PROJECT from `project-current'.  If
it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

CLASS is a subclass of symbol `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

INTERACTIVE is t if called interactively.

\(fn MANAGED-MAJOR-MODE PROJECT CLASS CONTACT &optional INTERACTIVE)" t nil)

(autoload 'eglot-ensure "eglot" "\
Start Eglot session for current buffer if there isn't one.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eglot" '("eglot-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eglot-autoloads.el ends here
