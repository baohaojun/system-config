(require 'grep)
(setq my-grep-command "beagrep -e pat") ;; should not put it into custom, the custom will be read every time and so the `(let ((grep-command ..' scheme will fail
(defcustom bhj-grep-dir nil "The default directory for grep")
(defvar ajoke--marker-ring (make-ring 32)
  "Ring of markers which are locations from which ajoke was invoked.")

(defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (grep-shell-quote-argument (bhj-grep-tag-default)))
        ;; This a regexp to match single shell arguments.
        ;; Could someone please add comments explaining it?
        (sh-arg-re "\\(\\(?:\"\\(?:\\\\\"\\|[^\"]\\)*\"\\|'[^']+'\\|\\(?:\\\\.\\|[^\"' \\|><\t\n]\\)\\)+\\)")

        (grep-default (or (car grep-history) my-grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
               (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*"
                       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
               grep-default)
              ;; If the string is not yet complete.
              (string-match "\\(\\)\\'" grep-default))
      ;; Maybe we will replace the pattern with the default tag.
      ;; But first, maybe replace the file name pattern.

      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t grep-default 1))))


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

(defun bhj-grep-tag-default ()
  (let ((tag (grep-tag-default)))
  (cond
   ((region-active-p)
    tag)
   ((string-match "/res/.*\.xml\\|AndroidManifest.xml" (or (buffer-file-name) ""))
    (replace-regexp-in-string "</\\w+>\\|^<\\|^.*?/" "" tag))
   (t
    (replace-regexp-in-string "^<\\|>$" "" tag)))))

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

(defun bhj-grep ()
  (interactive)
  (let ((current-prefix-arg 4)
        ;; (default-directory (eval bhj-grep-default-directory))
        (grep-use-null-device nil))
    (nodup-ring-insert ajoke--marker-ring (point-marker))
    (call-interactively 'grep-bhj-dir)))

(defun nodup-ring-insert (ring obj)
  (unless (and (not (ring-empty-p ring))
               (equal (ring-ref ring 0) obj))
    (ring-insert ring obj)))


(define-key goto-map "r" 'bhj-grep)
