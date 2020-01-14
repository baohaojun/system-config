Simply M-x eglot should be enough to get you started, but here's a
little info (see the accompanying README.md or the URL for more).

M-x eglot starts a server via a shell-command guessed from
`eglot-server-programs', using the current major-mode (for whatever
language you're programming in) as a hint.  If it can't guess, it
prompts you in the mini-buffer for these things.  Actually, the
server needen't be locally started: you can connect to a running
server via TCP by entering a <host:port> syntax.

Anyway, if the connection is successful, you should see an `eglot'
indicator pop up in your mode-line.  More importantly, this means
current *and future* file buffers of that major mode *inside your
current project* automatically become \"managed\" by the LSP
server, i.e.  information about their contents is exchanged
periodically to provide enhanced code analysis via
`xref-find-definitions', `flymake-mode', `eldoc-mode',
`completion-at-point', among others.

To "unmanage" these buffers, shutdown the server with M-x
eglot-shutdown.

You can also do:

  (add-hook 'foo-mode-hook 'eglot-ensure)

To attempt to start an eglot session automatically everytime a
foo-mode buffer is visited.
