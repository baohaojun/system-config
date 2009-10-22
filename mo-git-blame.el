(defvar mo-git-blame-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "b") 'mo-git-blame-reblame-for-revision-at)
    (define-key map (kbd "c") 'mo-git-blame-content-for-revision-at)
    (define-key map (kbd "l") 'mo-git-blame-log-for-revision-at)
    (define-key map (kbd "L") 'mo-git-blame-log-for-current-revision)
    (define-key map (kbd "o") 'mo-git-blame-overwrite-file-with-revision-at)
    (define-key map (kbd "O") 'mo-git-blame-overwrite-file-with-current-revision)
    (define-key map (kbd "q") 'mo-git-blame-quit)
    (define-key map (kbd "s") 'mo-git-blame-show-revision-at)
    (define-key map (kbd "S") 'mo-git-blame-show-current-revision)
    (define-key map (kbd "RET") 'mo-git-blame-show-revision-at)
    (define-key map (kbd "TAB") 'mo-git-blame-display-content-buffer)
    (define-key map [?\C-x ?k] 'mo-git-blame-quit)
    (define-key map [?\C-x ?\C-l] 'mo-git-blame-goto-line)
    map))

(defvar mo-git-blame-content-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'mo-git-blame-quit)
    (define-key map [?\C-x ?k] 'mo-git-blame-quit)
    (define-key map [?\C-x ?\C-l] 'mo-git-blame-goto-line)
    map))

(defun mo-git-blame-run (&rest args)
  (apply 'call-process "git" nil (current-buffer) nil args))

(defun mo-git-blame-get-output-buffer ()
  (let* ((name "*mo-git-blame-output*")
         (buffer (get-buffer name)))
    (if (null buffer)
        (progn
          (setq buffer (get-buffer-create name))
          (with-current-buffer buffer
            (use-local-map mo-git-blame-mode-map))))
    buffer))

(defun mo-git-blame-parse-blame-line ()
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (cond ((looking-at "^\\([a-f0-9]+\\) +\\(([^)]+)\\) *$")
             (list 'hash (buffer-substring (match-beginning 1) (match-end 1))
                   'file-name mo-git-blame-file-name
                   'timestamp (buffer-substring (match-beginning 2) (match-end 2))))
            ((looking-at "^\\([a-f0-9]+\\) +\\(([^)]+)\\) +\\(.+\\)")
             (list 'hash (buffer-substring (match-beginning 1) (match-end 1))
                   'file-name (buffer-substring (match-beginning 3) (match-end 3))
                   'timestamp (buffer-substring (match-beginning 2) (match-end 2))))
            (t (error "Not a 'git blame' line"))))))

(defun mo-git-blame-revision-at-point ()
  (plist-get (mo-git-blame-parse-blame-line) 'hash))

(defun mo-git-blame-log-for-revision (revision)
  (let ((file-name mo-git-blame-file-name)
        (buffer (mo-git-blame-get-output-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (mo-git-blame-run "log" revision "--" file-name)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun mo-git-blame-log-for-revision-at ()
  "Calls 'git log' for revision in the current line."
  (interactive)
  (mo-git-blame-log-for-revision (mo-git-blame-revision-at-point)))

(defun mo-git-blame-log-for-current-revision ()
  "Calls 'git log' for the buffer's current revision and file."
  (interactive)
  (mo-git-blame-log-for-revision mo-git-blame-current-revision))

(defun mo-git-blame-show-revision (revision)
  (let ((buffer (mo-git-blame-get-output-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (mo-git-blame-run "show" revision)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun mo-git-blame-show-revision-at ()
  "Calls 'git show' for the revision in the current line."
  (interactive)
  (mo-git-blame-show-revision (mo-git-blame-revision-at-point)))

(defun mo-git-blame-show-current-revision ()
  "Calls 'git show' for the current revision."
  (interactive)
  (mo-git-blame-show-revision mo-git-blame-current-revision))

(defun mo-git-blame-content-for-revision-at ()
  "Calls 'git cat-file' for the revision in the current line."
  (interactive)
  (let ((info (mo-git-blame-parse-blame-line))
        (buffer (mo-git-blame-get-output-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (mo-git-blame-run "cat-file" "blob" (concat (plist-get info 'hash) ":" (plist-get info 'file-name)))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun mo-git-blame-overwrite-file-with-revision (revision)
  (let ((file-name mo-git-blame-original-file-name))
    (if (yes-or-no-p (format "Do you really want to overwrite %s with revision %s " file-name revision))
        (progn
          (find-file (concat mo-git-blame-top-dir file-name))
          (erase-buffer)
          (mo-git-blame-run "cat-file" "blob" (concat revision ":" file-name))
          (goto-char (point-min))))))

(defun mo-git-blame-overwrite-file-with-revision-at ()
  "Calls 'git cat-file' for the revision in the current line and overwrites
the original file's content. The file is not saved but left modified in an
open buffer."
  (interactive)
  (mo-git-blame-overwrite-file-with-revision (mo-git-blame-revision-at-point)))

(defun mo-git-blame-overwrite-file-with-current-revision ()
  "Calls 'git cat-file' for the current revision and overwrites
the original file's content. The file is not saved but left modified in an
open buffer."
  (interactive)
  (mo-git-blame-overwrite-file-with-revision mo-git-blame-current-revision))

(defun mo-git-blame-reblame-for-revision-at ()
  "Calls 'git blame' for the revision in the current line."
  (interactive)
  (let* ((info (mo-git-blame-parse-blame-line))
         (revision (plist-get info 'hash)))
    (if (string= revision mo-git-blame-current-revision)
        (error "Already showing this revision"))
    (mo-git-blame-file (concat mo-git-blame-top-dir (plist-get info 'file-name)) revision mo-git-blame-original-file-name)))

(defun mo-git-blame-file (file-name &optional revision original-file-name)
  "Calls 'git blame' for REVISION of FILE-NAME or HEAD if REVISION
is not given."
  (let* ((the-raw-revision (if (null revision) "HEAD" revision))
         (the-revision (if (string= the-raw-revision "HEAD")
                           (magit-git-string "rev-parse" "--short" "HEAD")
                         the-raw-revision))
         (base-name (concat (file-name-nondirectory file-name) "@" the-revision))
         (blame-buffer (get-buffer-create "*mo-git-blame*"))
         (content-buffer-name (concat "*mo-git-blame:" (file-name-nondirectory file-name) ":" the-revision "*"))
         (content-buffer (if (local-variable-p 'mo-git-blame-content-buffer)
                             mo-git-blame-content-buffer
                           (get-buffer-create content-buffer-name)))
         (top-dir (magit-get-top-dir (file-name-directory file-name)))
         (relative-file-name (file-relative-name file-name top-dir))
         (blame-window (selected-window))
         (content-window nil)
         )
    (switch-to-buffer blame-buffer)
    (if (window-full-width-p)
        (split-window-horizontally 45))
    (select-window (setq content-window (next-window)))
    (switch-to-buffer content-buffer)
    (select-window blame-window)
    (with-current-buffer blame-buffer
      (toggle-read-only 0)
      (kill-all-local-variables)
      (buffer-disable-undo)
      (erase-buffer)
      (setq default-directory top-dir)
      (set (make-local-variable 'mo-git-blame-top-dir) top-dir)
      (set (make-local-variable 'mo-git-blame-file-name) relative-file-name)
      (set (make-local-variable 'mo-git-blame-full-file-name) file-name)
      (set (make-local-variable 'mo-git-blame-original-file-name) (if (null original-file-name) file-name original-file-name))
      (set (make-local-variable 'mo-git-blame-current-revision) the-revision)
      (set (make-local-variable 'mo-git-blame-blame-buffer) blame-buffer)
      (set (make-local-variable 'mo-git-blame-blame-window) blame-window)
      (set (make-local-variable 'mo-git-blame-content-buffer) content-buffer)
      (set (make-local-variable 'mo-git-blame-content-window) content-window)
      (set (make-local-variable 'line-move-visual) nil)
      (setq major-mode 'mo-git-blame-mode
            mode-name "MoGitBlame"
            mode-line-process ""
            truncate-lines t)
      (use-local-map mo-git-blame-mode-map)
      (mo-git-blame-run "blame" the-revision "--" mo-git-blame-file-name)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward "^\\([a-f0-9]+\\) +\\(([^)]+)\\) \\(.*\\)" nil t)
          (replace-match "\\1 \\2" nil nil))
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-f0-9]+\\) +\\([^ ]+\\) +\\(([^)]+)\\) \\(.*\\)" nil t)
          (replace-match "\\1 \\3 \\2" nil nil))
        (goto-char (point-min))
        (while (re-search-forward " +[0-9]+)" nil t)
          (replace-match ")" nil nil)))
      (toggle-read-only t)
      (goto-char (point-min))
      (scroll-all-mode 1)
      )
    (with-current-buffer content-buffer
      (toggle-read-only 0)
      (rename-buffer content-buffer-name)
      (kill-all-local-variables)
      (buffer-disable-undo)
      (erase-buffer)
      (setq buffer-file-name (file-name-nondirectory file-name)
            default-directory top-dir)
      (mo-git-blame-run "cat-file" "blob" (concat the-revision ":" relative-file-name))
      (normal-mode)
      (use-local-map mo-git-blame-content-mode-map)
      (set (make-local-variable 'mo-git-blame-blame-buffer) blame-buffer)
      (set (make-local-variable 'mo-git-blame-blame-window) blame-window)
      (set (make-local-variable 'mo-git-blame-content-buffer) content-buffer)
      (set (make-local-variable 'mo-git-blame-content-window) content-window)
      (set (make-local-variable 'line-move-visual) nil)
      (font-lock-fontify-buffer)
      (toggle-read-only t)
      (set-buffer-modified-p nil)
      (scroll-all-mode 1)
      (setq truncate-lines t))))

(defun mo-git-blame-quit ()
  "Kill the mo-git-blame buffers."
  (interactive)
  (delete-other-windows)
  (scroll-all-mode 0)
  (save-match-data
    (dolist (buffer (buffer-list))
      (if (string-match "^\\*mo-git-blame" (buffer-name buffer))
          (kill-buffer buffer)))))

(defun mo-git-blame-display-content-buffer ()
  "Show the content buffer in the content window."
  (interactive)
  (let ((buffer mo-git-blame-content-buffer)
        (line-num (line-number-at-pos)))
    (with-selected-window mo-git-blame-content-window
      (switch-to-buffer buffer))
    (goto-line line-num)
    (recenter)
    (with-selected-window mo-git-blame-content-window
      (goto-line line-num)
      (recenter))))

(defun mo-git-blame-other-buffer ()
  (if (eq (current-buffer) mo-git-blame-blame-buffer)
      mo-git-blame-content-buffer
    mo-git-blame-blame-buffer))

(defun mo-git-blame-goto-line (line)
  "Goto a line in both the blame and the content buffer."
  (interactive "nGoto line: ")
  (with-selected-window mo-git-blame-blame-window
    (goto-line line))
  (with-selected-window mo-git-blame-content-window
    (goto-line line)))

(defun mo-git-blame-current ()
  "Calls mo-git-blame-file for HEAD for the current buffer."
  (interactive)
  (if (null (buffer-file-name))
      (error "The current buffer is not associated with a file."))
  (mo-git-blame-file (buffer-file-name)))

;; (defun mo-git-blame-special ()
;;   ""
;;   (interactive)
;;   (mo-git-blame-file "/home/mosu/prog/video/mkvtoolnix/src/info/mkvinfo.cpp")) ;merge/cluster_helper.cpp"))
;; (global-set-key [?\C-c ?i ?g] 'mo-git-blame-special)

(provide 'mo-git-blame)
