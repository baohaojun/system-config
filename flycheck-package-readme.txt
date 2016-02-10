Provides feedback via flycheck about issues with the package metadata
of a file, e.g. the package dependencies it requires.

To enable, use something like this:

   (eval-after-load 'flycheck
     '(flycheck-package-setup))

Checks will currently be enabled only if a "Package-Requires:" or
"Package-Version:" header is present in the file.
