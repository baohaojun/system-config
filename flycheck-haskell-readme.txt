Configure Haskell syntax checking by Flycheck.

Cabal support

Try to find Cabal project files for Haskell buffers, and configure the
Haskell syntax checkers in Flycheck according to the contents of the Cabal
file:

- Add all source directories to the GHC search path
- Add build directories from Cabal to the GHC search path to speed up
  checking and support non-Haskell modules such as hsc files
- Add auto-generated files from Cabal to the GHC search path
- Set the language from Cabal
- Enable language extensions from Cabal

Cabal sandboxes

Try to find a Cabal sandbox configuration for this project, and configure the
Haskell syntax checkers in Flycheck to use the package database from the
Sandbox.

Setup

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
