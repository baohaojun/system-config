(when *is-a-mac*
  (setq default-input-method "MacOSX")
  (define-key key-translation-map "\e[21~" [f10])
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  (global-set-key (kbd "s-`") 'ns-next-frame)
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )


(provide 'init-osx-keys)
