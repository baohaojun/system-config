;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)

  (when (maybe-require-package 'company-php)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package php-mode
  :defer t
  :init
  (autoload 'php-mode "php-mode" nil t)
  :config
  (mapc (lambda (x) (add-hook x (lambda ()
                             (setq beginning-of-defun-function nil)
                             (local-set-key [?\C-\M-a] 'beginning-of-defun)
                             (local-set-key [?\C-\M-e] 'end-of-defun))))
        (list 'php-mode-hook)))
(provide 'init-php)
;;; init-php.el ends here
