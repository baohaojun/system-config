;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'elpy)
  (after-load 'python
    (add-hook 'python-mode-hook 'elpy-mode)))

(provide 'init-python)
;;; init-python.el ends here
