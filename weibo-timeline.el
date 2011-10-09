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

(require 'ewoc)

(defconst weibo-timeline-buffer-name "*weibo-timeline*")
(defconst weibo-timeline-mode-name "微博时间线")

(defconst weibo-timeline-headline "微博：%s\n命令：%s\n操作：新消息(g) 刷新(r) 下一条（空格) 帮助(h) 退出(q)")
(defconst weibo-timeline-post-caption "发表微博(P) ")
(defconst weibo-timeline-look-caption "察看(L) ")
(defconst weibo-timeline-retweet-caption "转发(T) ")
(defconst weibo-timeline-comment-caption "评论(C) ")
(defconst weibo-timeline-reply-caption "回复(R) ")

(defconst weibo-timeline-footline "提示：获取更多较早前消息(m)")

(defconst weibo-timeline-separator (make-string 70 ?=))
(defconst weibo-timeline-sub-separator (make-string 70 ?-))

(defvar weibo-timeline-data nil "Buffer local variable that holds timeline data")
(defvar weibo-timeline-current-provider nil
  "Buffer local variable that holds current timeline provider")

(defvar weibo-timeline-providers nil "Global variable that holds timeline providers")

(defstruct weibo-timeline-provider
  key
  name
  make-function
  pretty-printer-function
  pull-function
  post-function
  look-function
  retweet-function
  comment-function
  reply-function
  header-function
  data)

(defun weibo-timeline-register-provider (reg-data &optional current)
  (when (weibo-timeline-provider-p reg-data)
    (let* ((key (weibo-timeline-provider-key reg-data))
	   (switch-to-key (append '(lambda () (interactive))
				  `((weibo-timeline-switch-to-provider ,key)))))
      (add-to-list 'weibo-timeline-providers
		   `(,key . ,reg-data) t)
      (define-key weibo-timeline-mode-map key
	switch-to-key))))

(defun weibo-timeline-switch-to-provider (key)
  (let ((provider (cdr (assoc key weibo-timeline-providers))))
    (when provider
      (setq weibo-timeline-current-provider provider)
      (weibo-timeline-refresh))))

(defun weibo-timeline-set-provider (provider)
  (setq weibo-timeline-current-provider provider)
  (weibo-timeline-refresh))

(defun weibo-timeline-pretty-printer (item)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (apply (weibo-timeline-provider-pretty-printer-function weibo-timeline-current-provider)
	   (list item
		 (weibo-timeline-provider-data weibo-timeline-current-provider)))))

(defun weibo-timeline-insert-text (text)
  (let ((pos-begin (point)))
    (insert " " text "\n")
    (let ((pos-end (point)))
      (goto-char pos-begin)
      (while (search-forward-regexp "http://[0-9a-zA-Z\.\?&/]+" pos-end t)
	(make-text-button (match-beginning 0) (match-end 0)
			  'action (lambda (b) (browse-url (button-label b)))
			  'follow-link t))
      (goto-char pos-begin)
      (while (search-forward-regexp "@\\(\\w\\|_\\)+" pos-end t)
	(make-text-button (match-beginning 0) (match-end 0)
			  'action (lambda (b) (weibo-show-user (button-label b)))
			  'follow-link t))
      (goto-char pos-end))
    (fill-region pos-begin (- (point) 1))))

(defun weibo-timeline-insert-picture (thumb_pic mid_pic)
  (when thumb_pic
    (insert "\t")
    (let ((begin_pos (point)))
      (weibo-insert-image (weibo-get-image-file thumb_pic) mid_pic)	    
      (when mid_pic
	(make-text-button begin_pos (point)
			  'face 'default
			  'action (lambda (b) (weibo-show-image (button-label b)))
			  'follow-link t)))
    (insert "\n")))

(defun weibo-timeline-parse-root (root root-name front-t &optional clear-t)
  (when clear-t (ewoc-filter weibo-timeline-data (lambda (data) nil)))
  (and root (string= (xml-node-name root) root-name)
       (weibo-timeline-parse-nodes (xml-node-children root) front-t)))

(defun weibo-timeline-parse-nodes (node-list front_t)
  (let ((proc_func (if front_t 'ewoc-enter-first 'ewoc-enter-last)))
    (mapc '(lambda (node)
	     (apply proc_func (list weibo-timeline-data
				    (apply
				     (weibo-timeline-provider-make-function
				      weibo-timeline-current-provider)
				     `(,node)))))
	  (if front_t (reverse node-list) (cdr node-list)))))

(defun weibo-timeline-pull (new)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (let* ((pos (if new 0 -1))
	   (node (ewoc-nth weibo-timeline-data pos))
	   (node-data (and node (ewoc-data node))))
      (apply (weibo-timeline-provider-pull-function weibo-timeline-current-provider)
	     (list
	      node-data
	      'weibo-timeline-parse-root new
	      (weibo-timeline-provider-data weibo-timeline-current-provider))))))

(defun weibo-timeline-pull-new ()
  (interactive)
  (weibo-timeline-pull t)
  (goto-char (point-min)))

(defun weibo-timeline-pull-old ()
  (interactive)
  (let ((p (point)))
    (weibo-timeline-pull nil)
    (goto-char p)))

(defun weibo-timeline-move-next ()
  (interactive)  
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (ewoc_node (and node (ewoc-next weibo-timeline-data node))))
    (if (not ewoc_node)
	(weibo-timeline-pull-old)
      (goto-char (ewoc-location ewoc_node))
      (recenter-top-bottom 0))))

(defun weibo-timeline-retweet ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-retweet-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-comment ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-comment-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-reply ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-reply-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-post ()
  (interactive)
  (when (weibo-timeline-provider-p weibo-timeline-current-provider)
    (let ((func (weibo-timeline-provider-post-function
		   weibo-timeline-current-provider)))
      (and func (apply func
		       (list (weibo-timeline-provider-data weibo-timeline-current-provider)))))))

(defun weibo-timeline-look ()
  (interactive)
  (let* ((node (ewoc-locate weibo-timeline-data))
	 (data (and node (ewoc-data node))))
    (when (weibo-timeline-provider-p weibo-timeline-current-provider)
      (let ((func (weibo-timeline-provider-look-function
		   weibo-timeline-current-provider)))
	(and func (apply func
			 (list data
			       (weibo-timeline-provider-data weibo-timeline-current-provider))))))))

(defun weibo-timeline-inspect ()
  (interactive)
  (let ((node (ewoc-locate weibo-timeline-data)))
    (when node
      (print (ewoc-data node))
      (ewoc-invalidate weibo-timeline-data node))))

(defun weibo-timeline-refresh ()
  (interactive)
  (with-current-buffer (weibo-timeline-buffer)
    (setq mode-name (format "%s-%s" weibo-timeline-mode-name
			    (weibo-timeline-provider-name
			     weibo-timeline-current-provider)))
    (ewoc-set-hf weibo-timeline-data
		 (concat (format weibo-timeline-headline
			 (mapconcat
			  (lambda (item)
			    (let ((provider (cdr item)))
			      (concat
			       (weibo-timeline-provider-name provider) "("
			       (weibo-timeline-provider-key provider) ")")))
			  weibo-timeline-providers " ")
			 (concat
			  (when (weibo-timeline-provider-post-function
				 weibo-timeline-current-provider)
			    weibo-timeline-post-caption)
			  (when (weibo-timeline-provider-look-function
				 weibo-timeline-current-provider)
			    weibo-timeline-look-caption)		       
			  (when (weibo-timeline-provider-retweet-function
				 weibo-timeline-current-provider)
			    weibo-timeline-retweet-caption)
			  (when (weibo-timeline-provider-comment-function
				 weibo-timeline-current-provider)
			    weibo-timeline-comment-caption)
			  (when (weibo-timeline-provider-reply-function
				 weibo-timeline-current-provider)
			    weibo-timeline-reply-caption)))
			 (let ((header-func
				(weibo-timeline-provider-header-function
				 weibo-timeline-current-provider)))
			   (when header-func
			     (apply header-func
				    (list (weibo-timeline-provider-data
					   weibo-timeline-current-provider))))))
		 weibo-timeline-footline)
    (ewoc-filter weibo-timeline-data (lambda (data) nil))
    (weibo-timeline-pull-new)))

(defvar weibo-timeline-mode-map
  nil
  "Keymap for weibo-timeline-mode")

(setq weibo-timeline-mode-map
      (let ((map (make-sparse-keymap)))
	
	(define-key map "g" 'weibo-timeline-pull-new)
	(define-key map "m" 'weibo-timeline-pull-old)
	(define-key map " " 'weibo-timeline-move-next)
	(define-key map "r" 'weibo-timeline-refresh)
	(define-key map "s" 'weibo-timeline-inspect)
	
	(define-key map "P" 'weibo-timeline-post)
	(define-key map "L" 'weibo-timeline-look)
	(define-key map (kbd "RET") 'weibo-timeline-look)
	(define-key map "T" 'weibo-timeline-retweet)
	(define-key map "C" 'weibo-timeline-comment)
	(define-key map "R" 'weibo-timeline-reply)

	(define-key map "t" 'beginning-of-buffer)
	(define-key map "d" 'end-of-buffer)	
	(define-key map "n" 'next-line)
	(define-key map "p" 'previous-line)
	(define-key map "f" 'forward-char)
	(define-key map "b" 'backward-char)
	
	(define-key map "q" 'weibo-bury-close-window)
	(define-key map "Q" 'weibo-kill-close-window)
	map))

(define-derived-mode weibo-timeline-mode fundamental-mode weibo-timeline-mode-name
  "Major mode for displaying weibo timeline"
  (use-local-map weibo-timeline-mode-map)
  (setq buffer-read-only t)
  (setq fill-column 70)
  (make-local-variable 'weibo-timeline-data)
  (make-local-variable 'weibo-timeline-current-provider)
  (unless (ewoc-p weibo-timeline-data)
    (setq weibo-timeline-data
	  (ewoc-create 'weibo-timeline-pretty-printer
		       weibo-timeline-headline weibo-timeline-footline)))
  (unless (weibo-timeline-provider-p weibo-timeline-current-provider)
    (setq weibo-timeline-current-provider (cdar weibo-timeline-providers))))

(defun weibo-timeline-buffer ()
  (with-current-buffer (get-buffer-create weibo-timeline-buffer-name)
    (unless (eq major-mode 'weibo-timeline-mode)
      (weibo-timeline-mode)
      (weibo-timeline-refresh))
    (current-buffer)))

(defun weibo-timeline ()
  (interactive)
  (switch-to-buffer (weibo-timeline-buffer)))

(provide 'weibo-timeline)