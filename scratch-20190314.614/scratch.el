;;; scratch.el --- Mode-specific scratch buffers

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 1.3
;; Package-Version: 20190314.614
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/ieure/scratch-el
;; Keywords: convenience, tools, files

;; Copyright (c) 1999-2017, 2019 Ian Eure <ian.eure@gmail.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:
;;
;; Scratch
;; =======
;;
;; Scratch is an extension to Emacs that enables one to create scratch
;; buffers that are in the same mode as the current buffer.  This is
;; notably useful when working on code in some language; you may grab
;; code into a scratch buffer, and, by virtue of this extension, do so
;; using the Emacs formatting rules for that language.
;;
;; Scratch is available from MELPA.
;;
;;
;; Usage
;; =====
;;
;; - `M-x scratch' Immediately create a scratch buffer with the same
;; major mode as the current buffer’s.  If the region is active, copy
;; it to the scratch buffer.  If a scratch buffer already exists, pop
;; to it (and do nothing with the region).
;;
;; - `C-u M-x scratch' Prompts for a major mode to create a scratch
;; buffer with.
;;
;;
;; Binding
;; =======
;;
;; `C-c s' is a good mnemonic binding for scratch-el:
;;
;;
;; (define-key (current-global-map) "\C-cs" #'scratch)
;;
;;
;; Customization
;; =============
;;
;; If you want to customize the behavior of all scratch buffers, you
;; can place hooks in `scratch-create-buffer-hook'.
;;
;; For per-mode customizations, you can add a hook to the mode, and
;; check `scratch-buffer' inside it.  For example, to set a default
;; title on all `org-mode' scratch buffers, you could do:
;;
;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (when scratch-buffer
;;               (save-excursion
;;                 (goto-char (point-min))
;;                 (insert "#+TITLE: Scratch\n\n")))))
;;

;;; History:

;; 1999 Ian Eure
;;   Initial version.

;; 2010-08-16 Ian Eure
;;   Broke out into its own file.

;; 2012-08-30 Ian Eure
;;   Dump current region into new scratch buffer

;; 2017-05-23 Ian Eure
;;   Set up *sql* buffers so they know about the inferior process they
;;   were created from.
;;   Add mappings for additional inferior modes.
;;   Fix checkdoc & package-lint issues.
;;
;; 2019-03-13 Ian Eure (v1.3)
;;   Add `scratch-create-buffer-hook', which runs any time a new
;;   scratch buffer is created.
;;   Fix an issue where SQL scratch buffers weren't linked to the SQLi
;;   buffers they were created from.
;;   Fix byte-compilation warnings.
;;   Substantially refactor & update code.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'sql)
(require 'subr-x)

(defgroup scratch nil
  "Scratch buffers."
  :prefix "scratch-"
  :group 'editing)

(defcustom scratch-mode-alist
  '((erc-mode . fundamental-mode)
    (sql-interactive-mode . sql-mode)
    (shell-mode . sh-mode)
    (inferior-python-mode . python-mode)
    (inferior-emacs-lisp-mode . emacs-lisp-mode)
    (cider-repl-mode . clojure-mode)
    (inferior-tcl-mode . tcl-mode)
    (inferior-octave-mode . octave-mode))
  "Alist of mappings from major modes to major modes for `scratch'.

Some interactive modes don't lend themselves well to scratch buffers;
this alist is used to change the mode used by `scratch' for those
buffers."

  :type '(alist :key-type symbol :value-type symbol))

(defcustom scratch-create-buffer-hook nil
  "Hooks to run when creating a scratch buffer.")

(defvar scratch--history nil
  "History of scratch buffers.")

(defvar scratch--major-mode-list nil
  "List of major modes `scratch' may use.  See `scratch-list-modes'.")

(defvar scratch-parent nil
  "The parent buffer of this scratch buffer.")
(make-variable-buffer-local 'scratch-parent)

;;;###autoload
(defvar scratch-buffer nil
  "Non-nil if the current buffer is a scratch buffer.")
(make-variable-buffer-local 'scratch-buffer)

(defun scratch--list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-list))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect (substring name 0 -5)))

(defun scratch--link-sql ()
  "Link a scratch buffer to a SQLi buffer.

This sets up the scratch buffer so `sql-send-buffer' etc work as
expected."
  (let ((product (with-current-buffer scratch-parent sql-product)))
    (setq sql-product product
          sql-buffer scratch-parent)))

(defun scratch--link-buffers ()
  "Link a parent and child buffer.

When a scratch buffer is created from a mode for an inferior process,
and has features which rely on knowing the inferior process, link
them."
  (cond
   ((eq (with-current-buffer scratch-parent major-mode) 'sql-interactive-mode)
    (scratch--link-sql))
   (t nil)))

(defun scratch--buffer-querymode ()
  (cond

   ;; Prompt user for mode
   (current-prefix-arg
    (intern (concat (completing-read
                     "Mode: " (scratch--list-modes)
                     nil t nil scratch--history)
                    "-mode")))

   ;; Overriden mode for ex. sql->sqli, shell->sh
   ((assoc major-mode scratch-mode-alist)
    (cdr (assoc major-mode scratch-mode-alist)))

   ;; Default is the current mode
   (t major-mode)))

(defun scratch--create (mode name)
  (let ((scratch-buffer t)
        (parent (current-buffer))
        (contents (when (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))))
    (with-current-buffer (get-buffer-create name)
      (funcall mode)
      (when contents
          (save-excursion (insert contents)))

      (setq-local scratch-buffer t)
      (unless current-prefix-arg
        (setq-local scratch-parent parent)
        (scratch--link-buffers))
      (run-hooks 'scratch-create-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun scratch (mode)
  "Get a scratch buffer for the MODE.

When called interactively with a prefix arg, prompt for the mode."
  (interactive (list (scratch--buffer-querymode)))
  (let* ((name (thread-last (symbol-name mode)
                 (replace-regexp-in-string "-mode$" "")
                 (format "*%s*")))
         (buf (get-buffer name)))

    (pop-to-buffer
     (if (bufferp buf)
         buf ; Existing scratch buffer

       ;; New scratch buffer
       (scratch--create mode name)))))

(provide 'scratch)
;;; scratch.el ends here
