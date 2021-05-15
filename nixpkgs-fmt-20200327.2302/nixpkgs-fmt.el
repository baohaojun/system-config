;;; nixpkgs-fmt.el --- Reformat Nix using nixpkgs-fmt  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages
;; Package-Version: 20200327.2302
;; Package-Commit: 0df268136d14f27770fa5bb3bdb991e987337cf4
;; URL: https://github.com/purcell/emacs-nixpkgs-fmt
;; Package-Requires: ((emacs "24") (reformatter "0.3"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides commands and a minor mode for easily reformatting Nix code
;; using the external "nixpkgs-fmt" program.

;; Call `nixpkgs-fmt-buffer' or `nixpkgs-fmt-region' as convenient.

;; Enable `nixpkgs-fmt-on-save-mode' in Nix buffers like this:

;;     (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)

;; or locally to your project with a form in your .dir-locals.el like
;; this:

;;     ((nix-mode
;;       (mode . nixpkgs-fmt-on-save)))

;; You might like to bind `nixpkgs-fmt-region' or `nixpkgs-fmt-buffer' to a key,
;; e.g. with:

;;     (define-key 'nix-mode-map (kbd "C-c C-f") 'nixpkgs-fmt)

;;; Code:


;; Minor mode and customisation

(require 'reformatter)

(defgroup nixpkgs-fmt nil
  "Reformat Nix using nixpkgs-fmt."
  :group 'languages)

(defcustom nixpkgs-fmt-command "nixpkgs-fmt"
  "Command used for reformatting."
  :type 'string)


;; Commands for reformatting

;;;###autoload (autoload 'nixpkgs-fmt-buffer "nixpkgs-fmt" nil t)
;;;###autoload (autoload 'nixpkgs-fmt-region "nixpkgs-fmt" nil t)
;;;###autoload (autoload 'nixpkgs-fmt-on-save-mode "nixpkgs-fmt" nil t)
(reformatter-define nixpkgs-fmt
  :program nixpkgs-fmt-command
  :lighter " NixPkgFmt"
  :group 'nixpkgs-fmt)


(provide 'nixpkgs-fmt)
;;; nixpkgs-fmt.el ends here
