;;; init-csharp.el --- Support for working with CSHARP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'csharp-mode)

(use-package csharp-mode
  :defer t
  :init
  (autoload 'csharp-mode "csharp-mode" nil t)
  :config
  (defun csharp-end-of-defun (&optional arg)
    (interactive)
    (if (eq major-mode 'csharp-mode)
        (beginning-of-defun-raw -1)
      (end-of-defun arg))))
(provide 'init-csharp)
;;; init-csharp.el ends here
