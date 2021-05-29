;;; init-lsp.el --- Lsp editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I use nix + direnv instead of virtualenv/pyenv/pyvenv, and it is an
;; approach which extends to other languages too. I recorded a
;; screencast about this: https://www.youtube.com/watch?v=TbIHRHy7_JM


(require-package 'lsp-mode)
(use-package lsp-mode
  :commands lsp
  :bind-keymap
  ("M-s-l" . lsp-command-map)
  :config
  (mapc (lambda (f)
          (eval `(defadvice ,f (before bhj/push-mark activate)
                   (ajoke--push-marker-ring))))
        (seq-filter (lambda (f)
                      (and (symbolp f)
                           (string-match "^lsp-find-" (symbol-name f))))
                    obarray))
  :hook
  (python-mode . lsp)
  (csharp-mode . lsp)
  (isearch-mode . (lambda () (when (fboundp 'lsp-ui-doc-mode) (lsp-ui-doc-mode -1))))
  (isearch-mode-end . (lambda () (when (fboundp 'lsp-ui-doc-mode) (lsp-ui-doc-mode +1)))))
(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black"))

(provide 'init-lsp)
;;; init-lsp.el ends here
