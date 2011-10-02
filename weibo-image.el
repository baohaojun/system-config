(defun weibo-get-image-directory ()
  (let ((image-directory (concat weibo-directory "cache/")))
    (unless (file-exists-p image-directory)
      (make-directory image-directory t))
    image-directory))

(defun weibo-make-image-file-name (url)
  (concat (weibo-get-image-directory) (md5 url)))

(defun weibo-get-image-file (url)
  (let ((image-file (weibo-make-image-file-name url)))
    (unless (file-exists-p image-file)
      (with-current-buffer (url-retrieve-synchronously url)
	(goto-char (point-min))
	(let ((end (search-forward "\n\n" nil t)))
	  (when end
	    (delete-region (point-min) end)))
	(write-file image-file)))
    image-file))

(provide 'weibo-image)