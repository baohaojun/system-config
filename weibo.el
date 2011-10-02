(require 'oauth)
(require 'weibo-status)

(defconst weibo-request-url "http://api.t.sina.com.cn/oauth/request_token" "Request the unauthorized token")
(defconst weibo-authorized-url "http://api.t.sina.com.cn/oauth/authorize" "Redirect the user to this url")
(defconst weibo-access-url "http://api.t.sina.com.cn/oauth/access_token" "Request an access token")
(defconst weibo-api-url "http://api.t.sina.com.cn/" "API base url")


(defvar weibo-token-file "~/.t.weibo.emacs")
(defvar weibo-consumer-key "214135744")
(defvar weibo-consumer-secret "1e0487b02bae1e0df794ebb665d12cf6")
(defvar weibo-token nil)

(defun weibo-get-token ()
  (unless weibo-token
    (weibo-authorize))
  weibo-token)

(defun weibo-authorize (&optional reauthorize)
  (if (file-exists-p weibo-token-file)
      (progn
	(save-excursion
	  (find-file weibo-token-file)
	  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
	    (if (string-match "\\([^:]*\\):\\(.*\\)" str)
		(setq weibo-token (make-oauth-access-token
				   :consumer-key weibo-consumer-key
				   :consumer-secret weibo-consumer-secret
				   :auth-t (make-oauth-t
					    :token (match-string 1 str)
					    :token-secret (match-string 2 str))))))
	  (save-buffer)
	  (kill-this-buffer))))
  (if (or reauthorize (not weibo-token))
    (setq weibo-token (oauth-authorize-app weibo-consumer-key weibo-consumer-secret weibo-request-url weibo-access-url weibo-authorized-url))
	  (save-excursion
	    (find-file weibo-token-file)
	    (let ((token (oauth-access-token-auth-t weibo-token)))
	      (insert (format "%s:%s\n"
			      (oauth-t-token token)
			      (oauth-t-token-secret token))))
	    (save-buffer)
	    (kill-this-buffer)))
  weibo-token)

(defun weibo-get-node (pnode tag)
  (car (xml-get-children pnode tag)))

(defun weibo-get-node-text (node tag)
  (car (xml-node-children (weibo-get-node node tag))))

(defun weibo-get-raw-result (item callback &optional param &rest cbdata)
  (let ((root (car (with-current-buffer
		       (oauth-fetch-url (weibo-get-token) (concat (format "%s%s.xml" weibo-api-url item) param))
		     (goto-char (point-min))
		     (let ((start (search-forward "\r\n\r\n" nil t)))
		       (when start
			 (xml-parse-region start (point-max))))))))
      (apply callback (cons root cbdata))))

(provide 'weibo)