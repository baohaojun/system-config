(defconst weibo-image-buffer-name "*weibo-image*")

(defun weibo-get-image-directory ()
  (let ((image-directory (expand-file-name "cache" weibo-directory)))
    (unless (file-exists-p image-directory)
      (make-directory image-directory t))
    image-directory))

(defun weibo-make-image-file-name (url)
  (expand-file-name (md5 url) (weibo-get-image-directory)))

(defun weibo-get-image-file (url)
  (let ((image-file (weibo-make-image-file-name url)))
    (unless (file-exists-p image-file)
      (with-current-buffer (url-retrieve-synchronously url)
	(goto-char (point-min))
	(let ((end (search-forward "\n\n" nil t)))
	  (when end
	    (delete-region (point-min) end)
	    (write-file image-file)))))
    (if (file-exists-p image-file) image-file nil)))

(defun weibo-insert-image (image-file &optional url)
  (condition-case err
      (progn
	(insert-image (create-image image-file) url)
	t)
    (error
     (when (file-exists-p image-file)
       (delete-file image-file))
     nil)))

(defun weibo-show-image (url)
  (let ((init_t (not (get-buffer weibo-image-buffer-name))))
    (switch-to-buffer weibo-image-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (when init_t
      (weibo-image-mode))
    (unless (weibo-insert-image (weibo-get-image-file url))
      (weibo-bury-close-window)
      (message "无法打开图片！"))
    (setq buffer-read-only t)))

(defvar weibo-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'weibo-bury-close-window)
    map)
  "Keymap for weibo-image-mode")

(define-derived-mode weibo-image-mode fundamental-mode "Weibo-Image"
  "Major mode for displaying weibo image"
  (use-local-map weibo-image-mode-map))

(provide 'weibo-image)