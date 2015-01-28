;;; bhj-grep --- My grep setup.

;;; Commentary:


(require 'ajoke)
(require 'grep)

;;; Code:
(defgroup bhj-grep nil
  "My grep setup"
  :group 'grep)


(defun grep-shell-quote-argument (argument)
  "Quote ARGUMENT for passing as argument to an inferior shell."
  (cond
   ((and (boundp 'no-grep-quote)
         no-grep-quote)
    (format "\"%s\"" argument))
   ((equal argument "")
    "\"\"")
   (t
    ;; Quote everything except POSIX filename characters.
    ;; This should be safe enough even for really weird shells.
    (let ((result "") (start 0) end)
      (while (string-match "[].*[^$\"\\]" argument start)
        (setq end (match-beginning 0)
              result (concat result (substring argument start end)
                             (let ((char (aref argument end)))
                               (cond
                                ((eq ?$ char)
                                 "\\\\\\")
                                ((eq ?\\  char)
                                 "\\\\\\")
                                (t
                                 "\\"))) (substring argument end (1+ end)))
              start (1+ end)))
      (concat "\"" result (substring argument start) "\"")))))

;;;###autoload
(defun bhj-grep-tag-default ()
  (let ((tag (grep-tag-default)))
  (cond
   ((region-active-p)
    tag)
   ((string-match "/res/.*\.xml\\|AndroidManifest.xml" (or (buffer-file-name) ""))
    (replace-regexp-in-string "</\\w+>\\|^<\\|^.*?/" "" tag))
   (t
    (replace-regexp-in-string "^<\\|>$" "" tag)))))

(defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (grep-shell-quote-argument (bhj-grep-tag-default)))
        ;; This a regexp to match single shell arguments.
        ;; Could someone please add comments explaining it?
        (sh-arg-re "\\(\\(?:\"\\(?:\\\\\"\\|[^\"]\\)*\"\\|'[^']+'\\|\\(?:\\\\.\\|[^\"' \\|><\t\n]\\)\\)+\\)")

        (grep-default (or (car grep-history) my-grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
               (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)+"
                       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
               grep-default)
              (string-match
               (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*" ; the only difference with the above is + vs. *
                       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
               grep-default)
              ;; If the string is not yet complete.
              (string-match "\\(\\)\\'" grep-default))
      ;; Maybe we will replace the pattern with the default tag.
      ;; But first, maybe replace the file name pattern.

      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t grep-default 1))))

(autoload 'nodup-ring-insert "bhj-defines")
;;;###autoload
(defun bhj-edit-grep-pattern ()
  (interactive)
  (beginning-of-line)
  (let ((min (progn
               (search-forward "\"" nil t)
               (point)))
        (max (progn
               (search-forward "\"" nil t)
               (backward-char)
               (point))))
    (undo-boundary)
    (when (< min max)
      (delete-region min max))))

(defcustom bhj-grep-dir nil "The default directory for grep")

;;;###autoload
(defun grep-bhj-dir ()
  (interactive)
  (let ((default-directory
          (if bhj-grep-dir
              (expand-file-name bhj-grep-dir)
            default-directory))
        (compilation-buffer-name-function (lambda (_ign) (if (boundp 'grep-buffer-name)
                                                             grep-buffer-name
                                                           "*grep*"))))
    (call-interactively 'grep)))

;;;###autoload
(defun grep-imenu ()
  (interactive)
  (let ((grep-gtags-history grep-imenu-history)
        (grep-buffer-name "*grep-imenu*"))
    (grep-gtags 'grep-imenu-history "grep-imenu -i -e pat")))

;;;###autoload
(defun grep-gtags (&optional history-var def-grep-command)
  (interactive)
  (let ((grep-history grep-gtags-history)
        (no-grep-quote t)
        (my-grep-command (or def-grep-command "grep-gtags -e pat"))
        (grep-buffer-name (if (boundp 'grep-buffer-name) grep-buffer-name "*grep-gtags*"))
        (current-prefix-arg 4))
    (nodup-ring-insert ajoke--marker-ring (point-marker))
    (ajoke--setup-env)
    (call-interactively 'grep-bhj-dir)
    (set (or history-var 'grep-gtags-history) grep-history)))

(defun grep-tag-default-path ()
  (or (and transient-mark-mode mark-active
           (/= (point) (mark))
           (buffer-substring-no-properties (point) (mark)))
      (save-excursion
        (let* ((re "[^-a-zA-Z0-9._/]")
               (p1 (progn (search-backward-regexp re)
                          (if (looking-at "(")
                              (progn
                                (search-backward-regexp "\\." (line-beginning-position))
                                (prog1
                                    (1+ (point))
                                  (search-forward-regexp "(")))
                            (1+ (point)))))
               (p2 (progn (forward-char)
                          (search-forward-regexp re)
                          (backward-char)
                          (if (looking-at ":[0-9]+")
                              (progn
                                (forward-char)
                                (search-forward-regexp "[^0-9]")
                                (1- (point)))
                            (point)))))
          (buffer-substring-no-properties p1 p2)))))

;;;###autoload
(defun grep-find-file ()
  (interactive)
  (let ((grep-history grep-find-file-history)
        (my-grep-command "beagrep -f -e pat")
        (grep-buffer-name "*grep-find-file*")
        (current-prefix-arg 4))
    (flet ((grep-tag-default () (grep-tag-default-path)))
      (nodup-ring-insert ajoke--marker-ring (point-marker))
      (call-interactively 'grep-bhj-dir)
      (setq grep-find-file-history grep-history))))

;;;###autoload
(defun grep-func-call ()
  (interactive)
  (let ((grep-history grep-func-call-history)
        (my-grep-command "grep-func-call -e pat")
        (grep-buffer-name (if (boundp 'grep-buffer-name) grep-buffer-name "*grep-func-call*"))
        (current-prefix-arg 4))
    (nodup-ring-insert ajoke--marker-ring (point-marker))
    (let ((file (ajoke--buffer-file-name (current-buffer)))
          (mode-name-minus-mode
           (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
      (if (file-remote-p file)
          (let ((process-environment tramp-remote-process-environment))
            (setenv "GTAGS_START_FILE" (file-remote-p file 'localname))
            (setenv "GTAGS_LANG_FORCE" (or (cdr (assoc mode-name-minus-mode ajoke--emacs-ctags-alist))
                                           mode-name-minus-mode))
            (setq tramp-remote-process-environment process-environment))
        (setenv "GTAGS_START_FILE" file)
        (setenv "GTAGS_LANG_FORCE" (or (cdr (assoc mode-name-minus-mode ajoke--emacs-ctags-alist))
                                       mode-name-minus-mode))))
    (call-interactively 'grep-bhj-dir)
    (setq grep-func-call-history grep-history)))

(defun bhj-goto-error-when-grep-finished (status code msg)
  (if (eq status 'exit)
      ;; This relies on the fact that `compilation-start'
      ;; sets buffer-modified to nil before running the command,
      ;; so the buffer is still unmodified if there is no output.
      (cond ((and (zerop code) (buffer-modified-p))
             (call-interactively #'next-error)
             '("finished (matches found)\n" . "matched"))
            ((not (buffer-modified-p))
             '("finished with no matches found\n" . "no match"))
            (t
             (cons msg code)))
    (cons msg code)))

;; (add-hook 'grep-setup-hook (lambda () (setq compilation-exit-message-function #'bhj-goto-error-when-grep-finished)))

;;;###autoload
(defun bhj-grep ()
  (interactive)
  (let ((current-prefix-arg 4)
        ;; (default-directory (eval bhj-grep-default-directory))
        (grep-use-null-device nil))
    (nodup-ring-insert ajoke--marker-ring (point-marker))
    (call-interactively 'grep-bhj-dir)))

;;;###autoload
(defun bhj-rgrep ()
  (interactive)
  (let ((grep-history grep-rgrep-history)
        (grep-buffer-name "*grep-rgrep*")
        (my-grep-command "rgrep -Hn -e pat")
        (current-prefix-arg 4))
    (nodup-ring-insert ajoke--marker-ring (point-marker))
    (call-interactively 'grep-bhj-dir)
    (setq grep-rgrep-history grep-history)))

(defvar bhj-grep-mode-map (make-sparse-keymap)
  "Bhj-Grep mode map.")
(define-key bhj-grep-mode-map (kbd "M-g r") 'bhj-grep)
(define-key bhj-grep-mode-map (kbd "M-s E") 'bhj-occur-logcat-errors)
(define-key bhj-grep-mode-map (kbd "M-s e") 'bhj-occur-make-errors)
(define-key bhj-grep-mode-map (kbd "M-s g") 'bhj-do-code-generation)
(define-key bhj-grep-mode-map (kbd "M-s m") 'bhj-occur-merge-conflicts)
(define-key bhj-grep-mode-map (kbd "M-g o") 'bhj-occur)
(define-key bhj-grep-mode-map (kbd "M-.") 'grep-gtags)

(define-minor-mode bhj-grep-mode
  "Toggle the `bhj-grep-mode' minor mode."
  :lighter " BG"
  :keymap bhj-grep-mode-map
  :group 'bhj-grep)

(define-globalized-minor-mode bhj-grep-global-mode
  bhj-grep-mode
  turn-on-bhj-grep-mode)

(defun turn-on-bhj-grep-mode ()
  "Turn on `bhj-grep-mode'."
  (interactive)
  (bhj-grep-mode 1))

(provide 'bhj-grep)
