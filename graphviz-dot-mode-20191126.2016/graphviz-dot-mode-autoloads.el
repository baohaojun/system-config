;;; graphviz-dot-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-graphviz-dot" "company-graphviz-dot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-graphviz-dot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-graphviz-dot" '("company-g")))

;;;***

;;;### (autoloads nil "graphviz-dot-mode" "graphviz-dot-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "\
Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]	- Indentation function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.

Variables specific to this mode:

  `graphviz-dot-dot-program'                   (default `dot')
       Program used to compile the graphs.
  `graphviz-dot-preview-extension'             (default `png')
       File type to use for output.
  `graphviz-dot-view-command'                  (default `doted %s')
       Command to run when `graphviz-dot-view' is executed.
  `graphviz-dot-view-edit-command'             (default nil)
       If the user should be asked to edit the view command.
  `graphviz-dot-save-before-view'              (default t)
       Automatically save current buffer berore `graphviz-dot-view'.

\(fn)" t nil)

(autoload 'graphviz-dot-preview "graphviz-dot-mode" "\
Compile the graph and preview it in an other buffer.

\(fn)" t nil)

(autoload 'graphviz-turn-on-live-preview "graphviz-dot-mode" "\
Turn on live preview.
This will update the preview on every save.

\(fn)" t nil)

(autoload 'graphviz-turn-off-live-preview "graphviz-dot-mode" "\
Turn off live preview.
Saving the file will no longer also update the preview.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "graphviz-dot-mode" '("graphviz-" "dot-menu")))

;;;***

;;;### (autoloads nil nil ("graphviz-dot-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; graphviz-dot-mode-autoloads.el ends here
