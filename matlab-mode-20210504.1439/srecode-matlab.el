;;; srecode-matlab.el --- Extra SRecode support for MATLAB
;;
;; Copyright (C) 2014, 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@gnu.org>
;; X-RCS: $Id$
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Support some extra dictionary entries for MATLAB code.

(require 'srecode)
(require 'srecode/dictionary)
(require 'srecode/semantic)
(require 'matlab)

;;; Code:

;;; :matlab ARGUMENT HANDLER
;;
;; When a :matlab argument is required, fill the dictionary info
;; with this MATLAB specific stuff.
;;
;; Error if not in MATLAB mode.

;;;###autoload
(defun srecode-semantic-handle-:matlab (dict)
  "Add macros into the dictionary DICT based on the current MATLAB buffer.
Adds the following:
FILE_SYMBOL - The file name as a symbol.
FILE_DOC_SYMBOL - The file name as a symbol for doc strings.
PACKAGE - The package this file is in, or empty if none.
FILE_CLASS - Show section if filename should be a class.
FILE_FUNCTION - Show setion if filename is a function.

On class prediction - when filling in an empty file, if the filename and
directory it is in match, for example @foo/foo.m then foo should be a classdef."
  (when (not (eq major-mode 'matlab-mode))
    (error "Wrong mode for :matlab argument"))
  (let* ((fsym (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
	 (dir (directory-file-name (file-name-directory (buffer-file-name))))
	 (dsym (file-name-nondirectory dir)))
    ;; Convert the file name to a symbol.
    (srecode-dictionary-set-value dict "FILE_SYMBOL" fsym)
    (srecode-dictionary-set-value dict "FILE_DOC_SYMBOL" (upcase fsym))
    ;; Check that dsym and fsym are the same
    (if (string= dsym (concat "@" fsym))
	(srecode-dictionary-show-section dict "FILE_CLASS")
      (srecode-dictionary-show-section dict "FILE_FUNCTION"))
    ;; Now look at the stack of directories to see what the package name
    ;; might be.
    (let ((package "")
	  (next (file-name-nondirectory dir)))
      (setq dir (file-name-directory dir))
      ;; Keep scanning for packages while there is a + in the path.
      (while (and (> (length dir) 0) (string-match "/\\+\\w" dir))
	(let ((first (aref next 0))
	      (rest (substring next 1)))
	(if (= first ?+)
	    (setq package (concat rest "." package))
	  (setq dir ""))
	;; Setup for next iteration.
	(setq dir (directory-file-name (directory-file-name dir)))
	(setq next (file-name-nondirectory dir))
	))
      (srecode-dictionary-set-value dict "PACKAGE" package)
      )))

(provide 'srecode-matlab)

;;; srecode-matlab.el ends here
