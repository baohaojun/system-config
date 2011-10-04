(require 'ewoc)

(defconst weibo-api-status-public-timeline "statuses/public_timeline")
(defconst weibo-api-status-friends-timeline "statuses/friends_timeline")
(defconst weibo-api-status-user-timeline "statuses/user_timeline")
(defconst weibo-api-status-mention-timeline "statuses/mentions")

(defconst weibo-status-buffer-name "*weibo-timeline*")
(defconst weibo-status-mode-name "微博时间线")

(defconst weibo-status-headline "微博：谁在说(w) 关注的(a) 提到我的(@) 我的微博(i)\n命令：新消息(g) 刷新(f) 下一条（空格) 帮助(h) 退出(q)")
(defconst weibo-status-footline "提示：获取更多较早前消息(m)")
(defconst weibo-status-separator
  "=====================================================================")
(defconst weibo-retweeted-status-separator
  "---------------------------------------------------------------------")

(defvar weibo-status-data nil "Buffer local variable that holds status data")
(defvar weibo-status-timeline weibo-api-status-friends-timeline)

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
   :source (nth 2 (weibo-get-node node 'source))
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

(defun weibo-parse-statuses (root front_t)
  (and root (string= (xml-node-name root) "statuses")
       (weibo-parse-status (xml-node-children root) front_t)))

(defun weibo-parse-status (node-list front_t)
  (let ((proc_func (if front_t 'ewoc-enter-first 'ewoc-enter-last)))
    (mapc '(lambda (node)
	     (apply proc_func (list weibo-status-data (weibo-make-status node))))
	  (if front_t (reverse node-list) (cdr node-list)))))

(defun weibo-status-pretty-printer (status)
  (weibo-insert-status status nil))

(defun weibo-insert-status (status retweeted)
  (when status
    (let ((indent (if retweeted "\t" "")))
      (unless retweeted
	(insert weibo-status-separator "\n"))
      (when retweeted
	(insert weibo-retweeted-status-separator "\n")
	(insert " 提到：" indent)) 
      (weibo-insert-image (weibo-get-image-file (weibo-user-profile_image_url (weibo-status-user status))))
      (insert " " (weibo-user-screen_name (weibo-status-user status)) "：\n")
      (let ((pos-begin (point)))
	(insert indent " " (weibo-status-text status) "\n")
	(fill-region pos-begin (- (point) 1)))
      (let ((thumb_pic (weibo-status-thumbnail_pic status))
	    (mid_pic (weibo-status-bmiddle_pic status)))
	(when thumb_pic
	  (insert indent "\t")
	  (let ((begin_pos (point)))
	    (weibo-insert-image (weibo-get-image-file thumb_pic) mid_pic)	    
	    (when mid_pic
	      (make-text-button begin_pos (point)
				'face 'default
				'action (lambda (b) (weibo-show-image (button-label b))))))
	  (insert "\n")))
      (unless retweeted
	(let ((retweeted_status (weibo-status-retweeted_status status)))
	  (weibo-insert-status retweeted_status t)))
      (insert indent "  创建于：" (weibo-status-created_at status) "\n")
      (when retweeted
	(insert weibo-retweeted-status-separator "\n")))))

(defun weibo-status-pull (new &optional type)
  (let* ((pos (if new 0 -1))
	 (keyword (if new "since_id" "max_id"))
	 (node (ewoc-nth weibo-status-data pos))
	 (node-data (and node (ewoc-data node)))
	 (id (and node-data (weibo-status-id node-data)))
	 (param (and id (format "?%s=%s" keyword id))))
    (with-temp-message (concat "获取消息 " param "...")
      (when type (setq weibo-status-timeline type))
      (weibo-get-raw-result weibo-status-timeline
			    'weibo-parse-statuses param
			    new))))

(defun weibo-status-pull-new (&optional type)
  (interactive)  
  (weibo-status-pull t type)
  (goto-char (point-min)))

(defun weibo-status-pull-old ()
  (interactive)  
  (let ((p (point)))
    (weibo-status-pull nil)
    (goto-char p)))

(defun weibo-status-inspect ()
  (interactive)
  (let ((node (ewoc-locate weibo-status-data)))
    (when node
      (print (ewoc-data node))
      (ewoc-invalidate weibo-status-data node))))

(defun weibo-status-move-next ()
  (interactive)  
  (let ((node (ewoc-locate weibo-status-data)))
    (when node
      (goto-char (ewoc-location (ewoc-next weibo-status-data node)))
      (recenter-top-bottom 0))))

(defun weibo-refresh-timeline ()
  (interactive)
  (weibo-timeline t))

(defun weibo-friends-timeline ()
  (interactive)
  (setq mode-name (concat weibo-status-mode-name " 关注的"))  
  (weibo-timeline t weibo-api-status-friends-timeline))

(defun weibo-user-timeline ()
  (interactive)
  (setq mode-name (concat weibo-status-mode-name " 我的微博"))  
  (weibo-timeline t weibo-api-status-user-timeline))

(defun weibo-mention-timeline ()
  (interactive)
  (setq mode-name (concat weibo-status-mode-name " 提到我的"))  
  (weibo-timeline t weibo-api-status-mention-timeline))

(defun weibo-public-timeline ()
  (interactive)
  (setq mode-name (concat weibo-status-mode-name " 谁在说"))  
  (weibo-timeline t weibo-api-status-public-timeline))

(defvar weibo-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" 'weibo-public-timeline)    
    (define-key map "a" 'weibo-friends-timeline)
    (define-key map "@" 'weibo-mention-timeline)
    (define-key map "i" 'weibo-user-timeline)
    (define-key map "f" 'weibo-refresh-timeline)
    (define-key map "g" 'weibo-status-pull-new)
    (define-key map "m" 'weibo-status-pull-old)
    (define-key map "t" 'beginning-of-buffer)
    (define-key map "d" 'end-of-buffer)
    (define-key map " " 'weibo-status-move-next)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "f" 'forward-char)
    (define-key map "b" 'backward-char)
    (define-key map "q" 'weibo-bury-close-window)
    (define-key map "s" 'weibo-status-inspect)
    map)
  "Keymap for weibo-status-mode")

(define-derived-mode weibo-status-mode fundamental-mode (concat weibo-status-mode-name " 关注的") 
  "Major mode for displaying weibo status"
  (use-local-map weibo-status-mode-map)
  (setq buffer-read-only t)
  (setq fill-column 70)
  (make-local-variable 'weibo-status-data)
  (make-local-variable 'weibo-status-timeline)
  (unless (ewoc-p weibo-status-data)
    (setq weibo-status-data (ewoc-create 'weibo-status-pretty-printer weibo-status-headline weibo-status-footline))))

(defun weibo-timeline (&optional refresh type)
  (interactive)
  (let* ((buffer-name weibo-status-buffer-name)
	 (init-t (not (get-buffer buffer-name))))
    (switch-to-buffer buffer-name)
    (when init-t
      (weibo-status-mode))
    (when refresh
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq buffer-read-only t)
      (setq weibo-status-data (ewoc-create 'weibo-status-pretty-printer weibo-status-headline weibo-status-footline)))
    (when (or refresh init-t)
      (weibo-status-pull-new type))))

(provide 'weibo-status)