;; Add the following lines to your init file.
;; (if (file-exists-p user-py-file)
;;     (load-file user-py-file))

(require 'mule)
(require 'quail)

;; 用户词组文件
(defvar user-py-file "~/emacs/USER_PY.el")
;; 用户用来记录人名的文件
(defvar user-people-names-file "~/emacs/USER_PEOPLE_NAMES.el")
;;转换规则的开始的标记，一旦用户词组文件创建之后不要改动，否则会出问题的
(defvar begin-translation-rulers-tag ";;Beginning of translation rulers")

;;定义用户词组文件的头
(defun new-quail-head ()
  (insert (format "(quail-define-rules\n((append . t))\n%s\n)"
		  begin-translation-rulers-tag)))

;;  建议用如下方法给输入法的词库添加规则.
;;  在你的 Emacs 的 init file 中添加一行:
;;
;;      (global-set-key "\C-cd" 'quail-define-new-ruler-from-line)
;;
;;  如果你想给你的输入法添加一个用户规则: sndsd -> 全能的上帝
;;  你可以在一个 buffer (比如: *scratch*) 中输入如下的一行
;;
;;      全能的上帝  qndsd
;;
;;  然后在此行处输入 C-c d
;;  或者 M-x quail-define-new-ruler-from-line
(defun quail-define-new-ruler-from-line ()
  "Add a new translation rule to the quail package named NAME.
The new rule is scaned from current line in the buffer.
A line looks like:
    全能的上帝  qndsd
will define a rule with key \"qndsd\" and translation \"全能的上帝\"."
  (interactive)
  (let (key translation)
    (save-excursion
      (end-of-line)
      (backward-word 1)
      (setq key (current-word))
      (backward-word 1)
      (setq translation (current-word)))
    (quail-define-new-ruler key translation)))


(defun quail-define-new-ruler (key translation &optional name)
  "Add a new translation rule to the quail package named NAME.
if NAME is not specified, add to the current package."
  ;;  (interactive (read-key-translation "Define the key"))
  (interactive (list (read-string "Key: ") (read-string "Translation: ")))
  (let ((vector (cdar (quail-lookup-key key)))
        (index 0)
        len)
    (if vector (setq len (length vector))
      (setq len 0))
    (while 
        (and (< index len)
             (not (string= translation (aref vector index))))
      (setq index (1+ index)))
    (if (= index len) ;; not redefined
        (progn
          ;;增加用户词组
          (quail-defrule key (vector translation) name t)
          ;;保存此词组到用户词组文件中
          (save-excursion
            (let ((buffer (get-file-buffer user-py-file)))
              (or buffer
                  (setq buffer (get-buffer-create user-py-file)))
              (set-buffer buffer)
              (goto-char (point-min))
              (if (file-exists-p user-py-file)
                  (insert-file-contents user-py-file nil nil nil t)
                (new-quail-head))
              (goto-char (point-min))
              (if (search-forward
                   begin-translation-rulers-tag nil t)
                  (progn
                    (if (search-forward-regexp
                         (concat "^" (regexp-quote (concat "(\"" key "\" ")))
                         nil t)
                        (progn
                          (search-forward-regexp "\\])$" nil t)
                          (goto-char (match-beginning 0))
                          (insert " \"" translation "\""))
                      (progn
                        (forward-line)
                        (beginning-of-line)
                        (insert (format "(\"%s\" [\"%s\"])\n" key translation))))
                    (write-file user-py-file)
                    (kill-buffer buffer))
                (message "No tag found!")))))
      (message "Rule: \"%s\" to \"%s\" already defined!"
               key translation))))

(defun quail-define-new-ruler-for-name-of-people-from-line ()
  "记录人名的命令."
  (interactive)
  (let ((user-py-file user-people-names-file))
    (quail-define-new-ruler-from-line)))