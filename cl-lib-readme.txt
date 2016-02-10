This is a forward compatibility package, which provides (a subset of) the
features of the cl-lib package introduced in Emacs-24.3, for use on
previous emacsen.

Make sure this is installed *late* in your `load-path`, i.e. after Emacs's
built-in .../lisp/emacs-lisp directory, so that if/when you upgrade to
Emacs-24.3, the built-in version of the file will take precedence, otherwise
you could get into trouble (although we try to hack our way around the
problem in case it happens).

This code is largely copied from Emacs-24.3's cl.el, with the alias bindings
simply reversed.
