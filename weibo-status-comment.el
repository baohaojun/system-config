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

(defconst weibo-api-status-comments-timeline "statuses/comments")

(defun weibo-pull-status-comments (data parse-func new status)
  (let* ((id (and status (weibo-status-id status)))
	 (param (format "?id=%s" id)))
    (with-temp-message "获取微博评论..."
      (weibo-get-data weibo-api-status-comments-timeline
		       parse-func param "comments" t t))))

(defun weibo-status-comments-pretty-printer (comment &optional p)
  (weibo-insert-comment comment nil))

(defun weibo-status-comments-header (status)
  (with-temp-buffer
    (insert "\n")
    (weibo-insert-status status nil)
    (buffer-string)))

(defun weibo-comment-status-comments (comment status)
  (let* ((id (and status (weibo-status-id status))))
    (weibo-create-post "" "评论微博" 'weibo-send-comment id)))

(defun weibo-status-comments-timeline-provider (status)
  (make-weibo-timeline-provider
   :key nil
   :name "察看微博"
   :make-function 'weibo-make-comment
   :pretty-printer-function 'weibo-status-comments-pretty-printer
   :pull-function 'weibo-pull-status-comments
   :post-function 'weibo-post-status
   :retweet-function nil
   :comment-function 'weibo-comment-status-comments
   :reply-function nil
   :header-function 'weibo-status-comments-header
   :data status))

(provide 'weibo-status-comment)