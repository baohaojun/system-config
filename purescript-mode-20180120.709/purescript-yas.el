;;; purescript-yas.el --- Customization support for Luke Hoersten's yasnippets

;; Copyright (C) 2013  John Wiegley, Luke Hoersten

;; Author: John Wiegley <johnw@newartisans.com>
;;         Luke Hoersten <Luke@Hoersten.org>
;; Keywords: faces files PureScript

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides customization variables for Luke Hoersten's yasnippet collection
;; to depend on.

;;; Code:

(defgroup purescript-yas nil
  "Customizations for Luke Hoersten's yasnippet collection for purescript-mode."
  :group 'purescript
  :prefix "purescript-yas-")

(defcustom purescript-yas-ghc-language-pragmas
  (split-string (shell-command-to-string "ghc --supported-extensions"))
  "List of language pragmas supported by the installed version of GHC."
  :group 'purescript-yas
  :type '(repeat string))

(defcustom purescript-yas-completing-function 'ido-completing-read
  "Function to use for completing among alternatives."
  :group 'purescript-yas
  :type 'function)

;;;###autoload
(defun purescript-yas-complete (&rest args)
  (apply purescript-yas-completing-function args))

;; Provide ourselves:

(provide 'purescript-yas)

;;; purescript-yas.el ends here
