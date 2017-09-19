(after-load 'company
  (add-hook 'sh-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-shell))))
(provide 'init-bhj)
