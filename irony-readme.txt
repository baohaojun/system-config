This file provides `irony-mode', a minor mode for C, C++ and Objective-C.

Usage:
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's asynchronous function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)

    ;; Only needed on Windows
    (when (eq system-type 'windows-nt)
      (setq w32-pipe-read-delay 0))

See also:
- https://github.com/Sarcasm/company-irony
- https://github.com/Sarcasm/ac-irony
