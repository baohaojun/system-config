(after-load 'company
  (add-hook 'sh-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-shell)))
  (add-hook 'c-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-clang)))
  (add-hook 'c++-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-clang))))
(provide 'init-bhj)
