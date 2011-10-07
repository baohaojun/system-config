;; Copyright (C) 2011 Austin<austiny.cn@gmail.com>
          
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst weibo-api-status-public-timeline "statuses/public_timeline")
(defconst weibo-api-status-friends-timeline "statuses/friends_timeline")
(defconst weibo-api-status-user-timeline "statuses/user_timeline")
(defconst weibo-api-status-mention-timeline "statuses/mentions")

(defconst weibo-api-status-update "statuses/update")
(defconst weibo-api-status-repost "statuses/repost")

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
   :source (mm-decode-coding-string (nth 2 (nth 2 (weibo-get-node node 'source))) 'utf-8)
   :favorited (weibo-get-node-text node 'favorited)
   :truncated (weibo-get-node-text node 'truncated)
   :in_reply_to_status_id (weibo-get-node-text node 'in_reply_to_status_id)
   :in_reply_to_user_id (weibo-get-node-text node 'in_reply_to_user_id)
   :in_reply_to_screen_name (weibo-get-node-text node 'in_reply_to_screen_name)   
   :thumbnail_pic (weibo-get-node-text node 'thumbnail_pic)
   :bmiddle_pic (weibo-get-node-text node 'bmiddle_pic)
   :original_pic (weibo-get-node-text node 'original_pic)
   :retweeted_status (let ((retweeted (weibo-get-node node 'retweeted_status)))
		       (when retweeted
			 (weibo-make-status retweeted)))
   :created_at (weibo-get-node-text node 'created_at)
   :user (weibo-make-user (weibo-get-node node 'user))))

(defun weibo-pull-status (node parse-func new type)
  (let* ((keyword (if new "since_id" "max_id"))
	 (id (and node-data (weibo-status-id node-data)))
	 (param (and id (format "?%s=%s" keyword id))))
    (with-temp-message (concat "获取微博 " param "...")
      (weibo-get-data type
		      parse-func param
		      "statuses" new))))

(defun weibo-status-pretty-printer (status &optional p)
  (weibo-insert-status status nil))

(defun weibo-insert-status (status retweeted)
  (when status
    (let ((indent (if retweeted "\t" "")))
      (unless retweeted
	(insert weibo-timeline-separator "\n"))
      (when retweeted
	(insert weibo-timeline-sub-separator "\n")
	(insert " 提到：" indent))
      (weibo-insert-user (weibo-status-user status) nil)
      (insert "说道：\n")
      (insert indent)
      (weibo-timeline-insert-text (weibo-status-text status))
      (when (weibo-status-thumbnail_pic status) (insert indent))
      (weibo-timeline-insert-picture (weibo-status-thumbnail_pic status) (weibo-status-bmiddle_pic status))
      (unless retweeted
	(let ((retweeted_status (weibo-status-retweeted_status status)))
	  (weibo-insert-status retweeted_status t)))
      (insert indent "  来自：" (weibo-status-source status) "  发表于：" (weibo-status-created_at status) "\n")
      (when retweeted
	(insert weibo-timeline-sub-separator "\n")))))

(defun weibo-post-status (&rest p)
  (weibo-create-post "" "发表微博" 'weibo-send-status))

;; reply-to-id t weibo-api-status-repost
;; reply-to-id 0 text t weibo-api-status-update
;; reply-to-id 0 text 0 message
(defun weibo-send-status (text &optional reply-to-id)
  (let ((data nil)
	(api weibo-api-status-update))
    (cond
     ((= (length text) 0) (message "不能发表空消息"))
     ((> (length text) 140) (message "消息长度须小于140字"))
     (t
      (add-to-list 'data `("status" . ,text))
      (when reply-to-id
	(add-to-list 'data `("id" . ,reply-to-id))
	(setq api weibo-api-status-repost))
      (weibo-post-data api 'print data nil nil)))))

(defun weibo-retweet-status (data &rest p)
  (interactive)
  (let* ((id (and data (weibo-status-id data)))
	 (retweeted (and data (weibo-status-retweeted_status data)))
	 (user_name (and retweeted (weibo-user-screen_name (weibo-status-user data))))
	 (user_name_text (and user_name (concat "//@" user_name "：")))
	 (text (and retweeted (weibo-status-text data))))
    (weibo-create-post (concat user_name_text text) "转发微博" id)))

(defun weibo-status-timeline-provider (key name data)
  (make-weibo-timeline-provider
   :key key
   :name name
   :make-function 'weibo-make-status
   :pretty-printer-function 'weibo-status-pretty-printer
   :pull-function 'weibo-pull-status
   :post-function 'weibo-post-status
   :retweet-function 'weibo-retweet-status
   :comment-function nil
   :data data))

(defun weibo-friends-timeline-provider ()
  (weibo-status-timeline-provider "a" "我的关注" weibo-api-status-friends-timeline))

(defun weibo-user-timeline-provider ()
  (weibo-status-timeline-provider "i" "我的微博" weibo-api-status-user-timeline))

(defun weibo-mention-timeline-provider ()
  (weibo-status-timeline-provider "@" "提到我的" weibo-api-status-mention-timeline))

(defun weibo-public-timeline-provider ()
  (weibo-status-timeline-provider "w" "谁在说" weibo-api-status-public-timeline))
(provide 'weibo-status)