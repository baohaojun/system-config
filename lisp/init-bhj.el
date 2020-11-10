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

(defun bhj-point-visible-p ()
  "Return t if current char is visible."
  (eq nil (delete-if ; delete the nils
           #'listp
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'invisible))
                   (overlays-at (point))))))

(defun bhj-next-visible-char ()
  "Return the next visible char starting from (point).
Returns (point) if current-char is visible."
  (save-excursion
    (while (and (not (bhj-point-visible-p))
                (not (= (point) (point-max))))
      (goto-char (next-overlay-change (point))))
    (point)))

(defun bhj-next-invisible-char ()
  "Return the next invisible char starting from (point).
Returns (point) if current-char is visible."
  (save-excursion
    (while (and (bhj-point-visible-p)
                (not (= (point) (point-max))))
      (goto-char (next-overlay-change (point))))
    (point)))
(defun fix-cjk-spaces ()
  "Fix cjk spaces."
  (interactive)
  (catch 'single-char-done
    (let ((my-point (point))
          (current-line-num (line-number-at-pos))
          (is-eol (looking-at "$"))
          (my-start (point-min))
          (my-end (point-max)))
      (when (region-active-p) ; for changing 1 space
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
          (if (buffer-narrowed-p)
              (save-restriction
                (narrow-to-region my-start my-end)
                (setq my-start (point-min)
                      my-end (point-max))
                (while (< my-start my-end)
                  (goto-char my-start)
                  (goto-char (bhj-next-visible-char))
                  (setq my-end (bhj-next-invisible-char))
                  (shell-command-on-region (point) my-end "fix-cjk-spaces" nil t)
                  (goto-char my-end)
                  (goto-char (bhj-next-visible-char))
                  (setq my-start (point)
                        my-end (point-max))))
            (shell-command-on-region my-start my-end "fix-cjk-spaces" nil t))
          ))
      (if is-eol
          (progn
            (goto-char (point-min))
            (forward-line (1- current-line-num))
            (goto-char (line-end-position)))
        (goto-char (min my-point (point-max)))))))

(defun bhj-quote-it ()
  "Quote it."
  (interactive)
  (when (region-active-p)
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (insert "「")
      (goto-char (point-max))
      (search-backward-regexp "[^ \n]")
      (forward-char)
      (insert "」\n--------------------------------\n"))))



(add-hook 'before-save-hook #'fix-cjk-spaces)

(define-key global-map (kbd "s-SPC") #'fix-cjk-spaces)
(provide 'init-bhj)
