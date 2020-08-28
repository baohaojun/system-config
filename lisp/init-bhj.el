(defun sanityinc/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (make-local-variable 'company-backends)
  (push backend company-backends))

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
  (catch 'single-char-done
    (let ((my-point (point))
          (current-line-num (line-number-at-pos))
          (is-eol (looking-at "$"))
          (my-start (point-min))
          (my-end (point-max)))
      (when (region-active-p)
        (setq my-start (min (mark) (point))
              my-end (max (mark) (point)))
        (let ((region-text (buffer-substring-no-properties (mark) (point))))
          (cond
           ((string= region-text " ")
            (delete-region (mark) (point))
            (insert " ")
            (throw 'single-char-done nil))
           ((string= region-text " ")
            (delete-region (mark) (point))
            (insert " ")
            (throw 'single-char-done nil)))))
      (save-mark-and-excursion
        (save-restriction
          (shell-command-on-region my-start my-end "fix-cjk-spaces" nil t)))
      (if is-eol
          (progn
            (goto-char (point-min))
            (forward-line (1- current-line-num))
            (goto-char (line-end-position)))
        (goto-char (min my-point (point-max)))))))

(add-hook 'before-save-hook #'fix-cjk-spaces)

(define-key global-map (kbd "s-SPC") #'fix-cjk-spaces)
(provide 'init-bhj)
