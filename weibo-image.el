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

(defconst weibo-image-buffer-name "*weibo-image*")

(defvar weibo-display-image t
  "When set to true, images are displayed. Set it to nil to disable image display")

(defvar weibo-download-image-queue nil)
(defvar weibo-download-image-queue2 nil)

(defun weibo-get-image-directory ()
  (let ((image-directory (expand-file-name "cache" weibo-directory)))
    (unless (file-exists-p image-directory)
      (make-directory image-directory t))
    image-directory))

(defun weibo-make-image-file-name (url)
  (expand-file-name (md5 url) (weibo-get-image-directory)))

(defun weibo-download-image-file (url)
  (let ((image-file (weibo-make-image-file-name url)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (let ((end (search-forward "\n\n" nil t)))
	(when end
	  (delete-region (point-min) end)
	  (write-file image-file)))
      (kill-buffer))))

(defun weibo-add-to-image-download-queue (url)
  (unless (member url weibo-download-image-queue2)
    (add-to-list 'weibo-download-image-queue url)))

(defun weibo-download-image-in-queue ()
  (let ((buffer (current-buffer))
	(url (pop weibo-download-image-queue)))
    (while url
	(let ((image-file (weibo-make-image-file-name url))
	      (size (length weibo-download-image-queue)))
	  (message (format "剩余%d张图片" size))
	  (unless (file-exists-p image-file)
	    (add-to-list 'weibo-download-image-queue2 url)
	    (url-retrieve url
			(lambda (status image-file weibo-timeline-data buffer url)
			  (goto-char (point-min))
			  (let ((end (search-forward "\n\n" nil t)))
			    (when end
			      (delete-region (point-min) end)
			      (write-file image-file)))
			  (kill-buffer)
			  (setq weibo-download-image-queue2 (remove url weibo-download-image-queue2))
			  (when (= (% (length weibo-download-image-queue2) 5) 0)
			    (with-current-buffer buffer
			      (let ((current-position (point)))
				(ewoc-refresh weibo-timeline-data)
				(goto-char current-position)))))
			`(,image-file ,weibo-timeline-data ,buffer ,url)))
	  (setq url (pop weibo-download-image-queue))
	  (when (= (% size 5) 0)
	    (sleep-for 0.1))))))

(defun weibo-get-image-file (url &optional download-synchronously)
  (if weibo-display-image
      (let ((image-file (weibo-make-image-file-name url)))
	(unless (file-exists-p image-file)
	  (if download-synchronously
	      (weibo-download-image-file url)
	    (weibo-add-to-image-download-queue url)))
	(if (file-exists-p image-file) image-file nil))
    nil))

(defun weibo-insert-image (image-file &optional url)
  (when weibo-display-image
    (when image-file
      (condition-case err
	  (progn
	    (insert-image (create-image image-file) url)
	    t)
	(error
	 (when (file-exists-p image-file)
	   (delete-file image-file))
	 nil)))))

(defun weibo-show-image (url)
  (let ((init_t (not (get-buffer weibo-image-buffer-name))))
    (switch-to-buffer weibo-image-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (weibo-image-mode)
    (if (weibo-insert-image (weibo-get-image-file url t))
	(progn
	  (setq buffer-read-only t)
	  (image-mode))
      (weibo-bury-close-window)
      (message "无法打开图片！"))))

(defvar weibo-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'weibo-bury-close-window)
    map)
  "Keymap for weibo-image-mode")

(define-derived-mode weibo-image-mode fundamental-mode "Weibo-Image"
  "Major mode for displaying weibo image"
  (use-local-map weibo-image-mode-map))

(provide 'weibo-image)