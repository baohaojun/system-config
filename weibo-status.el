(require 'weibo-user)
(defconst weibo-api-status-public-timeline "statuses/public_timeline")
(defconst weibo-api-status-friends-timeline "statuses/friends_timeline")
(defconst weibo-status-buffer-name "*weibo-%s-timeline*")

(defvar weibo-status-data nil "Buffer local variable that holds status data")

;; created_at: 创建时间
;; id: 微博ID
;; text：微博信息内容
;; source: 微博来源
;; favorited: 是否已收藏(正在开发中，暂不支持)
;; truncated: 是否被截断
;; in_reply_to_status_id: 回复ID
;; in_reply_to_user_id: 回复人UID
;; in_reply_to_screen_name: 回复人昵称
;; thumbnail_pic: 缩略图
;; bmiddle_pic: 中型图片
;; original_pic：原始图片
;; user: 作者信息
;; retweeted_status: 转发的博文，内容为status，如果不是转发，则没有此字段 
(defstruct weibo-status created_at id text
  source favorited truncated
  in_reply_to_status_id
  in_reply_to_user_id
  in_reply_to_screen_name
  thumbnail_pic
  bmiddle_pic
  original_pic
  user retweeted_status)

(defun weibo-parse-statuses (node-list type)
  (switch-to-buffer (format weibo-status-buffer-name type))
  (erase-buffer)
  (mapc '(lambda (node)
	   (insert (concat (weibo-get-node-text (weibo-get-node node 'user) 'screen_name) ": " (weibo-get-node-text node 'text) "\n========================\n"))) node-list)
  (goto-char (point-min)))

(provide 'weibo-status)