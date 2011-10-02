(require 'ewoc)

(defconst weibo-api-status-public-timeline "statuses/public_timeline")
(defconst weibo-api-status-friends-timeline "statuses/friends_timeline")

(defconst weibo-status-buffer-name "*weibo-status-%s*")

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

(defun weibo-make-status (node)
  (make-weibo-status
   :id (weibo-get-node-text node 'id)
   :text (weibo-get-node-text node 'text)
   :thumbnail_pic (weibo-get-node-text node 'thumbnail_pic)
   :bmiddle_pic (weibo-get-node-text node 'bmiddle_pic)   
   :created_at (weibo-get-node-text node 'created_at)
   :user (weibo-make-user (weibo-get-node node 'user))))

(defun weibo-parse-statuses (root front_t replace_t)
  (when root
    (when (string= (xml-node-name root) "statuses")
      (weibo-parse-status (xml-node-children root) front_t replace_t))))

(defun weibo-parse-status (node-list front_t replace_t)
  (let ((proc_func (if front_t 'ewoc-enter-first 'ewoc-enter-last)))
    (mapc '(lambda (node)
	     (apply proc_func (list weibo-status-data (weibo-make-status node))))
	  (if front_t (reverse node-list) node-list))))


(defun weibo-status-pretty-printer (status)
  (when status
    (insert-image (create-image (weibo-get-image-file (weibo-user-profile_image_url (weibo-status-user status)))) nil)
    (insert (concat " " (weibo-user-screen_name (weibo-status-user status)) ": \n"
		    (weibo-status-text status) "\n"))
    (let ((thumb_pic (weibo-status-thumbnail_pic status))
	  (mid_pic (weibo-status-bmiddle_pic status)))
      (when thumb_pic
	(insert "\t")
	(insert-image (create-image (weibo-get-image-file thumb_pic)) nil)
	(insert "\n"))
      (when mid_pic
	(insert-text-button mid_pic	 
	 'action (lambda (b) (find-file (weibo-get-image-file (button-label b)))))
	(insert "\n")))
    (insert (weibo-status-created_at status) "\n=================================================\n")))

(defun weibo-test-friends-timeline ()
  (interactive)
  (switch-to-buffer (format weibo-status-buffer-name "friends-timeline"))
  (goto-char (point-min))
  (make-local-variable 'weibo-status-data)
  (unless weibo-status-data
    (setq weibo-status-data (ewoc-create 'weibo-status-pretty-printer)))
  (let* ((node (ewoc-nth weibo-status-data 0))
	 (first (when node (ewoc-data node)))
	 (id (when first (weibo-status-id first)))
	 (param (when id (format "?since_id=%s" id))))
    (weibo-get-raw-result weibo-api-status-friends-timeline
			  'weibo-parse-statuses param
			  t nil)))

(provide 'weibo-status)