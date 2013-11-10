(global-set-key [(meta ?/)] 'hippie-expand)
(global-set-key [(control c) ??] 'bhj-c-show-current-func)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(global-set-key [(f3)] (lambda()(interactive)(woman (current-word))))
(global-set-key [(control meta shift g)]
                (lambda()(interactive)
                  (let
                      ((search-string (current-word)))
                    (progn
                      (setq search-string
                            (read-string (format "search google with [%s]: " search-string) nil nil search-string))
                      (call-process "bash" nil nil nil "googleemacs.sh" search-string)))))

(global-set-key [(meta n)] 'next-error)

(global-set-key [(meta p)] 'previous-error)

(global-set-key [(meta s) ?r]
                (lambda ()
                  (interactive)
                  (let ((grep-history grep-rgrep-history)
                        (grep-buffer-name "*grep-rgrep*")
                        (my-grep-command "rgrep -Hn -e pat")
                        (current-prefix-arg 4))
                    (nodup-ring-insert ajoke--marker-ring (point-marker))
                    (call-interactively 'grep-bhj-dir)
                    (setq grep-rgrep-history grep-history))))

(global-set-key [(meta s) ?p] 'grep-find-file)

(global-set-key [(control meta o)] 'bhj-occur)

(global-set-key (kbd "M-g o") 'bhj-occur)

(global-set-key [f3] 'switch-to-devenv)

(global-set-key [f9] 'devenv-toggle-breakpoint)

(global-set-key [(control f5)] 'devenv-debug)

(global-set-key [(control x) (w)] 'where-are-we)
(global-set-key [(control x) (r)] 'random-theme)
(global-set-key [(control h) (h)] 'bhj-help-it)

(global-set-key [(control x) (c)] 'visit-code-reading)

(global-set-key [(meta shift ? )] 'indent-same-space-as-prev-line)

(global-set-key [(ctrl x) ? ] 'switch-buffer-same-filename)

(global-set-key [(ctrl x) ?\S- ] 'switch-buffer-same-filename-rev)

(global-set-key "\C-cl" 'org-store-link)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "C-;") 'query-replace-regexp)

(global-set-key [(control x) (control z)] 'keyboard-quit)

(global-set-key (kbd "s-h") help-map)

(global-set-key (kbd "M-.") 'grep-gtags)

(global-set-key (kbd "M-g f") 'grep-func-call)

(global-set-key (kbd "M-s e") 'bhj-occur-make-errors)
(global-set-key (kbd "M-s m") 'bhj-occur-merge-conflicts)

(global-set-key (kbd "M-s x") 'compilation-minor-mode)

(global-set-key [?\C-'] 'hippie-expand)

(global-set-key (kbd "M-s g") 'bhj-do-code-generation)

(global-set-key (kbd "M-s c") (lambda () (interactive) (call-interactively 'compile)))

(global-set-key (kbd "M-g i") 'imenu)

(global-set-key (kbd "M-g I") 'grep-imenu)

(global-set-key (kbd "M-g m") 'man)

(global-set-key [(meta shift ?d)] 'insert-today)

(global-set-key [(shift return)] 'bhj-find-missing-file)

(global-set-key [(meta s) ?h ?h] 'source-code-help)

(global-set-key [(control |)] 'toggle-input-method)
(global-set-key [(super ?\\)] 'toggle-input-method)
(define-key global-map [remap upcase-region] 'bhj-upcase-symbol-or-region)
(define-key global-map [remap downcase-region] 'bhj-downcase-symbol-or-region)

