;;; dune-format.el --- Reformat OCaml's dune files automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages
;; Package-Commit: eda7a16ae378e7c482c11228c43ef32b893a1520
;; Package-Requires: ((reformatter "0.6") (emacs "24.1"))
;; Package-Version: 20210505.108
;; Package-X-Original-Version: 0.1-pre
;; Homepage: https://github.com/purcell/emacs-dune-format

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

;; Call `dune-format', `dune-format-buffer' or `dune-format-region' as
;; convenient.

;; Enable `dune-format-on-save-mode' in `dune-mode' buffers like this:

;;     (add-hook 'dune-mode-hook 'dune-format-on-save-mode)

;; or locally to your project with a form in your .dir-locals.el like
;; this:

;;     ((dune-mode
;;       (mode . dune-format-on-save)))

;;; Code:

(require 'reformatter)

(defgroup dune-format nil
  "Reformat OCaml's dune files automatically."
  :group 'languages)

;;;###autoload (autoload 'dune-format-buffer "dune-format" nil t)
;;;###autoload (autoload 'dune-format-region "dune-format" nil t)
;;;###autoload (autoload 'dune-format-on-save-mode "dune-format" nil t)
(reformatter-define dune-format
  :program "dune"
  :args '("format-dune-file")
  :lighter " DuneFmt")


(provide 'dune-format)
;;; dune-format.el ends here
