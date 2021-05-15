;;; lsp-pyre.el --- lsp-mode client for python using pyre

;; Copyright (C) 2017  John Allen <oss@porcnick.com>

;; Author: John Allen <oss@porcnick.com>
;; Version: 1.0
;; Package-Version: 20190406.335
;; Package-Commit: e177b8f5efd1a955b5753aeb5d1894e6d21be35a
;; Package-Requires: ((lsp-mode "6.0"))
;; URL: https://github.com/jra3/lsp-pyre

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A simple LSP client for hooking up pyre to lsp-mode

;;; Code:
(require 'lsp-mode)

(defgroup lsp-pyre nil
  "Pyre for python."
  :group 'lsp-mode
  :tag "Pyre")

(defcustom lsp-pyre-command '("pyre" "persistent")
  "Pyre command."
  :group 'lsp-pyre
  :risky t
  :type 'list)

(defun lsp-pyre-activate-p (file-name _mode)
  "Check if FILE-NAME is part of a Pyre project.
That is, if there is a .pyre_configuration(.local)? file in the folder hierarchy."
  (or (locate-dominating-file file-name ".pyre_configuration.local")
      (locate-dominating-file file-name ".pyre_configuration")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-pyre-command))
                  :major-modes '(python-mode)
                  :priority -1
                  :activation-fn 'lsp-pyre-activate-p
                  :server-id 'pyre))

(provide 'lsp-pyre)
;;; lsp-pyre.el ends here
