(after-load 'company
  (add-hook 'sh-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-shell)))
  (add-hook 'c-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-clang)))
  (add-hook 'c++-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-clang))))

(defun fix-cjk-spaces ()
  "Fix cjk spaces."
  (interactive)
  (let ((my-point (point))
        (my-start (point-min))
        (my-end (point-max)))
    (when (region-active-p)
      (setq my-start (min (mark) (point))
            my-end (max (mark) (point))))
    (shell-command-on-region my-start my-end "fix-cjk-spaces" nil t)
    (goto-char (min my-point (point-max)))))

(add-hook 'before-save-hook #'fix-cjk-spaces)

(define-key global-map (kbd "s-SPC") #'fix-cjk-spaces)
(provide 'init-bhj)
