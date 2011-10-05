(defconst weibo-api-status-update "statuses/update")
(defconst weibo-api-status-repost "statuses/repost")

(defconst weibo-update-buffer-name "*weibo-update*")
(defconst weibo-update-mode-name "发表微博")

(define-derived-mode weibo-update-mode fundamental-mode weibo-update-mode-name
  "Major mode for sending weibo status"
  (local-set-key "\C-c\C-c" 'weibo-post-update)
  (local-set-key "\C-c\C-d" 'weibo-close-update)
  (erase-buffer))

(defun weibo-update-status (&optional text reply_to_id)
  (interactive)
  (select-window (split-window-vertically -8))
  (switch-to-buffer (generate-new-buffer weibo-update-buffer-name))
  (weibo-update-mode)
  (set (make-local-variable 'weibo-update-reply-to-id) reply_to_id)
  (when reply_to_id (setq mode-name "转发微博"))
  (insert (concat text))
  (goto-char (point-min)))

(defun weibo-close-update ()
  (interactive)
  (kill-buffer)
  (delete-window))

(defun weibo-post-update ()
  (interactive)
  (let ((text (buffer-string)))
    (if (> (length text) 140)
	(message "不能发表：消息长度须小于140个字符")
      (weibo-post-status-data (buffer-string) weibo-update-reply-to-id)
      (weibo-close-update))))

(defvar weibo-update-reply-to-id nil "The id that this weibo replies to.")

(defun weibo-post-status-data (&optional text reply_to_id)
  (let ((data nil)
	(api weibo-api-status-update)
	(no_text (= (length text) 0)))
    (unless no_text
      (add-to-list 'data `("status" . ,text)))
    (when reply_to_id
      (add-to-list 'data `("id" . ,reply_to_id))
      (setq api weibo-api-status-repost))
    (if (or (not no_text) reply_to_id)
	(weibo-post-data api 'print data nil nil)
      (message "不能发表空微博"))))

(provide 'weibo-update)