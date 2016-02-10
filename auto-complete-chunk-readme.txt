Example usage:

   (add-hook
    'python-mode
    (lambda ()
      ;; Make sure `ac-source-chunk-list' comes first.
      (setq ac-sources (append '(ac-source-chunk-list) ac-sources))
      (setq ac-chunk-list
            '("os.path.abspath" "os.path.altsep" "os.path.basename"))))
