(require 'anything)

(defvar anything-c-source-buffers2 )

(setq anything-c-source-buffers2
  '((name . "Buffers")
    (init . anything-c-buffer-list-init)
    (candidates-in-buffer)
    ;; workaround ...
    (header-name . (lambda (e) ""))
    (type . buffer)
    (get-line . buffer-substring)
    (migemo)
    (persistent-action . anything-c-buffer-list-persistent-action)))

(defun anything-c-buffer-list-persistent-action (buf)
  ;; force 2 pain mode
  (anything-c-buffer-list-modify-header)
  (anything-c-buffer-list-preview-buffer buf))

(defun anything-c-buffer-list-preview-buffer (buf)
  (when (and buf
             (or (bufferp buf)
                 (bufferp (get-buffer buf))))
    (delete-other-windows)
    (switch-to-buffer buf)
    (if (< (frame-width) 160)
        (split-window)
        (split-window-horizontally))
    (select-window (next-window))
    (switch-to-buffer anything-buffer)))

(defun anything-c-buffer-list-modify-header ()
  (with-current-buffer anything-buffer
    (save-excursion
      (goto-char (previous-single-property-change (point) 'anything-header))
      (overlay-put (car (overlays-at (line-beginning-position)))
                   'display
                   (anything-aif anything-selection-overlay
                       (let* ((display (buffer-substring (overlay-start it)
                                                         (1- (overlay-end it))))
                              (real (get-text-property 0
                                                       'anything-realvalue
                                                       display)))
                         (anything-c-buffer-list-header-formatter display real))
                     "")))))

(defun nlp:misc-build-simple-tree-view (lst)
  (let ((lst (mapcar (lambda (e) (concat "─" e))  lst)))
    `(,(concat "┬" (first lst))
     ,@(mapcar (lambda (e) (concat "├" e))
               (subseq lst 1 -1))
     ,(concat "└" (car (last lst))))))

(defun nlp:join (lst &optional sep to-string)
  (let ((joind (if sep
                   (loop for e in (subseq lst 0 -1)
                         with ret
                         append (list e sep) into ret
                         finally (return (append ret (last lst))))
                   lst)))
    (if to-string
        (apply 'concat joind)
        joind)))

(defun anything-c-buffer-list-header-formatter (display buf)
  (with-temp-buffer
    (insert (nlp:join
             (nlp:misc-build-simple-tree-view
              (remove nil
                      (list (format "buffer : %s" display)
                            (format "size : %d" (buffer-size buf))
                            (anything-aif (buffer-file-name buf)
                                (format "file : %s" (replace-regexp-in-string
                                                     (getenv "HOME")
                                                     "~"
                                                     it))
                              nil))))
             "\n" t))
    (align-regexp (point-min) (point-max) "\\( \\): [^:]" 1 0)
    (buffer-string)))

(defvar anything-c-buffer-ignore-regexp-list
  '(anything-buffer "*Completions*" "^ "))

(defvar anything-c-buffer-diplay-transformer-list nil
  "List, its element is (REGEXP . PROC) or (PRED. PROC).
 First pattern: Buffer its name is matched REGEXP is passed PROC to
get transformed candidate.
 Second pattern: Buffer is passed to PRED. Buffer is passed to PROC to
get transformed candidate, when PRED return non-nil value")

(defun anything-c-buffer-list-init ()
  (let ((buffers
         ;; put current buffer at last in buffer list
         (save-window-excursion
           (bury-buffer)
           ;; remove useless buffers
           (append 
            (remove-if
            (lambda (buffer)
              (let ((name (buffer-name buffer)))
                (some (lambda (regexp) (string-match regexp name))
                      ;; make all elements to REGEXP string.
                      (mapcar (lambda (element)
                                    (cond
                                      ((stringp element) element)
                                      ((and (symbolp element)
                                            (stringp (symbol-value element)))
                                       (symbol-value element))))
                                  anything-c-buffer-ignore-regexp-list))))
            (remove anything-current-buffer (buffer-list)))
            `(,anything-current-buffer)))))

    (with-current-buffer (anything-candidate-buffer 'local)
        (erase-buffer)
        (mapc (lambda (buffer)
                (let ((display
                       ;; real -> display
                       (anything-c-buffer-list-real-to-display buffer) ))
                  ;; write to candidates buffer
                  (insert (propertize display 'anything-realvalue buffer) "\n")))
              buffers))))

(defun anything-c-buffer-list-real-to-display (buffer)
  "BUFFER is a buffer.
Return transformed buffer name used as anything candidate.
`anything-c-buffer-diplay-transformer-list' is used as Transformation rule."
  (or (some (lambda (transformer)
              (when (if (functionp (car transformer))
                        (funcall (car transformer) buffer)
                        (string-match (car transformer) (buffer-name buffer)))
                (funcall (cdr transformer) buffer)))
            anything-c-buffer-diplay-transformer-list)
      (buffer-name buffer)))

(provide 'anything-c-source-buffers2)


