;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
(require-package 'browse-kill-ring)
(require-package 'bbdb)
(require-package 'helm)
(require-package 'dts-mode)
(add-hook 'dts-mode-hook (lambda ()  (setq indent-tabs-mode t)))
(require-package 'csharp-mode)
(require 'helm-config)
(helm-mode 1)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)

(require-package 'ac-clang)
(require-package 'keydef)
(require-package 'lua-mode)
(require-package 'mmm-mode)
(require 'mmm-auto)
(require-package 'oauth2)
(require-package 'session)
(require-package 'yasnippet)
(yas-global-mode)
(setq yas-snippet-dirs
      '("~/system-config/.emacs_d/yasnippet/snippets" "~/system-config/.emacs_d/yasnippet-snippets"))
(yas-reload-all)

(browse-kill-ring-default-keybindings)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)

(require 'init-frame-hooks)
(require 'init-xterm)
; (require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-proxies)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
; (require 'init-ido)
; (require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
; (require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)

(require 'init-editing-utils)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(load "org-mime-autoloads")
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
(require 'init-haskell)
(require 'init-ruby-mode)
(require 'init-rails)
(require 'init-sql)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(when (>= emacs-major-version 24)
  (require 'init-clojure-cider))
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-dash)
(require 'init-ledger)
;; Extra packages which don't require any configuration

(require-package 'gnuplot)

(if (file-exists-p "~/.emacs_d/lisp/auto-complete-clang-async.el")
    (progn
      (require 'auto-complete-clang-async)

      (defun ac-cc-mode-setup ()
        (setq ac-clang-complete-executable "~/bin/Linux/clang-complete")
        (setq ac-sources '(ac-source-clang-async))
        (ac-clang-launch-completion-process)
        )

      (defun my-ac-config ()
        (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
        (add-hook 'auto-complete-mode-hook 'ac-common-setup)
        (global-auto-complete-mode t))

      (my-ac-config))
  (require-package 'auto-complete-clang)
  (require 'auto-complete-clang)
  (defun my-ac-cc-mode-setup ()
    (setq ac-sources '(ac-source-clang)))
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup))

(require-package 'ac-helm)
(require-package 'lua-mode)
(require-package 'htmlize)
(require-package 'dsvn)
(when *is-a-mac*
  (require-package 'osx-location))
(require-package 'regex-tool)
;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
