(defconst weibo-api-status-update "statuses/update")

(defun weibo-update-status (text &optional replay_to_id)
  (weibo-post-data weibo-api-status-update 'print `(("status" . ,text )) nil nil))

(provide 'weibo-update)