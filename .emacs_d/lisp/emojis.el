;;; Code: utf-8

(defvar emoji-hash-table (make-hash-table :test 'equal) "The hash table for emojis.")
(defvar emoji-names () "The names of the emojis.")
(defvar emoji-history nil "History of emojis")

(defun setup-emoji-hash ()
  (interactive)
  (setq emoji-hash-table (make-hash-table :test 'equal)
        emoji-names nil)
  (load "~/src/github/Wrench/release/emojis/emojis.el")

  (while emojis-string-list
    (let* ((val (caar emojis-string-list))
           (key (concat " " val " " (caddar emojis-string-list)))
           (png (cadar emojis-string-list))
           (emoji-image-size (floor (* bhj-english-font-size 1.8))))
      (unless (string= png "")
        (setq png (create-image png
                                (when (fboundp 'imagemagick-types)
                                  'imagemagick)
                                nil
                                :ascent 'center
                                :heuristic-mask t
                                :height emoji-image-size))
        (add-text-properties 0 (length val) `(display ,png) val)
        (add-text-properties 1 (1+ (length val)) `(display ,png) key))
      (puthash key val emoji-hash-table)
      (setq emoji-names (cons key emoji-names)))
    (setq emojis-string-list (cdr emojis-string-list)))
  (setq emoji-names (reverse emoji-names)))

;;;###autoload
(defun enter-emoji ()
  "Let the user input an emoji interactively"
  (interactive)
  (unless emoji-names
    (setup-emoji-hash))
  (let ((emoji))
    (flet ((bhj-hack-helm-s-return-helper () (interactive) (throw 's-return (buffer-substring-no-properties (point-min) (point-max)))))
      (let ((key (catch 's-return
                   (completing-read "Enter your emoji: " emoji-names nil t nil 'emoji-history))))
        (setq emoji (gethash key emoji-hash-table))
        (if emoji
            (progn
              (setq emoji-names (cons (find-if (lambda (n) (string= key n)) emoji-names) (delete-if (lambda (n) (string= key n)) emoji-names)))
              (with-temp-buffer
                (let ((emoji-names-frecency (subseq emoji-names 0 20)))
                  (recentf-dump-variable 'emoji-names-frecency))
                (write-file "~/.config/system-config/emacs-local-custom.el"))
              (insert emoji))
          (setq key (substring key (length "Enter your emoji: ")))
          (let  ((emoji-names-copy (copy-sequence emoji-names))
                 (stems (split-string key "\\s +")))
            (while stems
              (setq emoji-names-copy (delete-if (lambda (n)
                                                  (let* ((stem (car stems))
                                                         (not? (string-match "^!" stem))
                                                         match)
                                                    (when not? (setq stem (substring stem 1)))
                                                    (setq match
                                                          (let ((case-fold-search t))
                                                            (string-match (regexp-quote stem) n)))
                                                    (if not?
                                                        match
                                                      (not match))))
                                                emoji-names-copy))
              (setq stems (cdr stems)))
            (while emoji-names-copy
              (setq key (car emoji-names-copy))
              (setq emoji (gethash key emoji-hash-table))
              (insert emoji)
              (setq emoji-names-copy (cdr emoji-names-copy)))))))))

(provide 'emojis)
;;; emojis.el ends here
