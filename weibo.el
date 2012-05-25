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

(require 'hex-util)
(require 'oauth)
(require 'json)
(require 'weibo-timeline)
(require 'weibo-status)
(require 'weibo-user)
(require 'weibo-image)
(require 'weibo-post)
(require 'weibo-comment)
(require 'weibo-status-comment)

(defconst weibo-request-url "http://api.t.sina.com.cn/oauth/request_token" "Request the unauthorized token")
(defconst weibo-authorized-url "http://api.t.sina.com.cn/oauth/authorize" "Redirect the user to this url")
(defconst weibo-access-url "http://api.t.sina.com.cn/oauth/access_token" "Request an access token")
(defconst weibo-api-url "http://api.t.sina.com.cn/" "API base url")

(defvar weibo-directory "~/.t.weibo.emacs.d")
(defvar weibo-consumer-key "214135744")
(defvar weibo-consumer-secret "1e0487b02bae1e0df794ebb665d12cf6")
(defvar weibo-token nil)

(setq oauth-use-curl nil)

(defun weibo-get-token-file ()
  (unless (file-exists-p (expand-file-name weibo-directory))
    (make-directory (expand-file-name weibo-directory) t))
  (expand-file-name "token" weibo-directory))

(defun weibo-get-token ()
  (unless weibo-token
    (weibo-authorize))
  weibo-token)

(defun weibo-authorize (&optional reauthorize)
  (if (file-exists-p (weibo-get-token-file))
      (progn
	(save-excursion
	  (find-file (weibo-get-token-file))
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
  (when (or reauthorize (not weibo-token))
    (setq weibo-token (oauth-authorize-app weibo-consumer-key weibo-consumer-secret weibo-request-url weibo-access-url weibo-authorized-url)))
  (save-excursion
    (find-file (weibo-get-token-file))
    (let ((token (oauth-access-token-auth-t weibo-token)))
      (erase-buffer)
      (insert (format "%s:%s\n"
		      (oauth-t-token token)
		      (oauth-t-token-secret token))))
    (save-buffer)
    (kill-this-buffer))
  weibo-token)

(defun weibo-check-result (root)
  (if (and root (length root)
	   (if (and (listp root) (weibo-get-node root 'error)) nil t))
      t
    (print (weibo-get-node-text root 'error))
    nil))

(defun weibo-get-node (pnode tag)
  (assoc tag pnode))

(defun weibo-get-node-text (node tag)
  (let ((data (cdr (weibo-get-node node tag))))
    (cond ((numberp data) (format "%d" data))
	  (t data))))

(defun weibo-get-body ()
  (goto-char (point-min))
  (let ((start
	 (or (search-forward "\r\n\r\n" nil t)
	     (search-forward "\n\n" nil t)))
	(buffer (current-buffer))
	(max (point-max)))
    (when start
      (with-temp-buffer
	(insert-buffer-substring buffer start max)
	(mm-decode-coding-region (point-min) (point-max) 'utf-8)
	(goto-char (point-min))
	(replace-regexp "\"id\":\\([0-9]+\\)," "\"id\":\"\\1\",")
	(goto-char (point-min))
	(condition-case nil
	    (json-read)
	  ((error nil) `((error . ,(buffer-substring (point-min) (point-max))))))))))

(defun weibo-get-data (item callback &optional param &rest cbdata)
  (let ((root (with-current-buffer
		       (oauth-fetch-url
			(weibo-get-token)
			(concat (format "%s%s.json" weibo-api-url item) param))
		     (weibo-get-body))))
    (apply callback (cons root cbdata))))

(defun weibo-post-data (item callback vars &optional param &rest cbdata)
  (let ((root (car (with-current-buffer
		       (oauth-post-url
			(weibo-get-token)
			(concat (format "%s%s.json" weibo-api-url item) param) vars)
		     (weibo-get-body)))))
    (apply callback (cons root cbdata))))

(defun weibo-parse-data-result (root &rest data)
  (when root
    (print root)))

(defun weibo-bury-close-window ()
  (interactive)
  (bury-buffer)
  (condition-case err
      (delete-window)
    (error nil)))

(defun weibo-kill-close-window ()
  (interactive)
  (kill-buffer)
  (condition-case err
      (delete-window)
    (error nil)))

(weibo-timeline-register-provider (weibo-friends-timeline-provider))
(weibo-timeline-register-provider (weibo-user-timeline-provider))
(weibo-timeline-register-provider (weibo-mention-timeline-provider))
(weibo-timeline-register-provider (weibo-comments-by-me-timeline-provider))
(weibo-timeline-register-provider (weibo-comments-to-me-timeline-provider))
(weibo-timeline-register-provider (weibo-public-timeline-provider))

(provide 'weibo)