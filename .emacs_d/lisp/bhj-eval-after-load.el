
(eval-after-load "grep"
  '(progn
     (grep-compute-defaults)
     (load "bhj-grep")))

(eval-after-load "bhj-defines"
  '(progn
     (add-hook 'before-save-hook 'cleanup-buffer-safe)
     (add-hook 'before-save-hook 'confirm-risky-remote-edit)))

(eval-after-load 'c-mode '(c-set-style 'k&r))

(eval-after-load 'js '(define-key js-mode-map [(meta .)] 'grep-gtags))

(eval-after-load 'cc-vars '(require 'guess-offset))

(eval-after-load 'org-mode
  '(progn
     (require 'org-jira)
     (condition-case nil
         (load "jira-users")
       (error nil))
     (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
     (org-babel-do-load-languages
      (quote org-babel-load-languages)
      (quote ((emacs-lisp . t)
              (dot . t)
              (ditaa . t)
              (R . t)
              (python . t)
              (ruby . t)
              (gnuplot . t)
              (clojure . t)
              (sh . t)
              (ledger . t)
              (org . t)
              (plantuml . t)
              (latex . t))))
     (require 'org-md)
     (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
     (add-hook 'org-mode-hook (lambda () (org-jira-mode t)))))

(eval-after-load "diff-mode"
  '(define-key diff-mode-map (kbd "M-g") (lookup-key global-map (kbd "M-g"))))

(eval-after-load 'gnus
  '(progn
     (require 'bbdb-gnus)
     (require 'bbdb)
     (require 'bbdb-hooks)
     (load "bbdb-com" t)
     (add-hook 'gnus-summary-mode-hook
               (lambda ()
                 (define-key gnus-summary-mode-map "v" 'bhj-view-mail-external)))
     (add-hook 'message-send-hook 'bhj-set-reply)
     (setq gnus-parameters
           (nconc
            ;; Some charsets are just examples!
            '(("\\bcn\\.bbs\\..*" ;; Chinese
               (mm-coding-system-priorities
                '(iso-8859-1 gbk utf-8))))
            gnus-parameters))
     (bbdb-initialize 'gnus 'message)
     (bbdb-insinuate-message)
     (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)))

(eval-after-load 'browse-kill-ring
  '(setq browse-kill-ring-quit-action
         (lambda ()
            (mapc
             (lambda (w)
               (when (eq browse-kill-ring-original-buffer (window-buffer w))
                 (set-window-point w
                  (with-current-buffer browse-kill-ring-original-buffer
                    (point)))))
             (window-list))
            (bury-buffer)
            (unless (= (count-windows) 1)
              (delete-window)))))

(eval-after-load 'douban-music-mode
  '(progn
     (add-hook 'douban-song-before-info-hook
               (lambda ()
                 (shell-command-to-string "douban start-play-hook >/dev/null 2>&1 &")
                 (let* ((song-info (elt douban-music-song-list douban-music-current-song))
                        (channel (aget douban-music-channels douban-music-current-channel))
                        (album (aget song-info 'albumtitle))
                        (title (aget song-info 'title))
                        (artist (aget song-info 'artist))
                        (url (aget song-info 'url))
                        (mp3 (expand-file-name (format "~/Music/like/%s/%s/%s.mp3"
                                                       (replace-regexp-in-string "/" "%" artist)
                                                       (replace-regexp-in-string "/" "%" album)
                                                       (replace-regexp-in-string "/" "%" title))))
                        (icon (replace-regexp-in-string "mp3$" "png" mp3)))
                    (if (file-exists-p mp3)
                        (if (string= (shell-command-to-string (concat "douban should-play " (shell-quote-argument mp3))) "no")
                            (setq douban-music-local-url "/dev/null")
                          (setq douban-music-local-url mp3))
                      (setq douban-music-local-url nil))
                    (if (file-exists-p icon)
                        (setq douban-music-local-icon icon)
                      (setq douban-music-local-icon nil)))))))

(eval-after-load 'java-mode
  '(define-key java-mode-map (kbd "M-s d") 'bhj-open-android-doc-on-java-buffer))

(eval-after-load "ediff-init" '(add-hook 'ediff-quit-hook (lambda () (shell-command "find-or-exec emacs"))))

(eval-after-load 'yasnippet
  '(progn
    (define-key yas-minor-mode-map [(tab)]     nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-M-i") 'yas-expand)))

(add-hook 'emacs-startup-hook (lambda () (call-interactively 'yas-global-mode)))

;; (defun eliminate-dup-invalid-buffers ()
;;   (let* ((new-buffer (current-buffer))
;;          (new-file-abs-name (buffer-file-name))
;;          (new-file-rela-name (file-name-nondirectory (buffer-file-name)))
;;          (new-buffer-name (buffer-name))
;;          (buffer-list (buffer-list)))
;;     (mapc
;;      (lambda (abuf)
;;        (unless (eq abuf new-buffer)
;;          (let ((abuf-file-abs-name (buffer-file-name abuf)))
;;            (when (or (and (not abuf-file-abs-name)
;;                           (string= (replace-regexp-in-string "<[0-9]+>$" "" (buffer-name abuf))
;;                                    (replace-regexp-in-string "<[0-9]+>$" "" new-buffer-name)))
;;                      (and
;;                       abuf-file-abs-name
;;                       (string= (file-name-nondirectory abuf-file-abs-name) new-file-rela-name)
;;                       (not (file-exists-p abuf-file-abs-name))))
;;              (with-current-buffer abuf
;;                (kill-buffer abuf))))))
;;      buffer-list)))

;; (add-hook 'find-file-hook 'eliminate-dup-invalid-buffers)
