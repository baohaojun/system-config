(require 'anything)
(require 'anything-goodies)
(require 'linkd)

;;; (@* "Search")
(setq anything-c-source-linkd-tag
      '((name . "linkd tags")
        (candidates . anything-c-linkd-tag-candidates)
        (init . (lambda () (mapc 'linkd-mode '(-1 1))))
        (persistent-action . (lambda (overlay)
                               (switch-to-buffer anything-current-buffer)
                               (if (not (listp selection))
                                   (goto-char selection)
                                   (goto-char (car anything-current-position)))
                               (anything-persistent-highlight-point
                                (line-beginning-position)
                                (line-end-position))))
        (action-transformer . (lambda (actions selection)
                                (if (not (listp selection))
                                    (lambda (point)
                                      (goto-char point)
                                      (linkd-next-link))
                                    (lambda (tag)
                                      (goto-char (car anything-current-position))
                                      (call-interactively (key-binding "\M-\;"))
                                      (insert (format "(@%s \"%s\")"
                                                      (case (car tag)
                                                        ('link ">")
                                                        ('star "*"))
                                                      (cdr tag)))))))
        (action . (lambda (overlay) (goto-char (overlay-start overlay))))
        (candidates-in-buffer . anything-c-linkd-tag-candidates)))

(defun anything-c-linkd-tag-get-list ()
  (with-current-buffer anything-current-buffer
    (save-excursion
  (goto-char (point-min))
  (loop with cand = ""
        while (and (not (eobp))
                   (re-search-forward
                    (rx (and "(" (* (any blank)) "@" (or "*" ">")
                             (* (any blank))
                             "\"" (group (* (any alnum blank graph))) "\""
                             (* (any blank)) ")")) (point-max) t))
        when (and (setq cand (match-string-no-properties 1))
                  (goto-char (line-beginning-position))
                  (re-search-forward anything-pattern (line-end-position) t))
        collect (cons cand (point))
        do (forward-line)))))

(defun anything-c-linkd-tag-new ()
  (list (let* ((type (if (and (< 0 (length anything-pattern))
                                  (= ?> (aref anything-pattern 0)))
                             'link
                             'star))
                   (real (if (or (eq 'link type)
                                 (and (< 0 (length anything-pattern))
                                      (= ?* (aref anything-pattern 0))))
                             anything-pattern
                             (concat "*" anything-pattern)))
                   (disp (subseq real 1 (length real))))
              (cons (format "New %s: %s"
                            (symbol-name type)
                            disp) (cons type disp)))))

(defun anything-c-linkd-tag-candidates ()
  (let ((candidates (anything-c-linkd-tag-get-list)))
    (or candidates
        (anything-c-linkd-tag-new))))

;;; (anything '(anything-c-source-linkd-tag))


;;; (@* "Insert")
(setq anything-c-source-insert-linkd-tag
      '((name . "Insert linkd tag")
        (dummy)
        (action . anything-c-insert-linkd-action)
        (persistent-action . (lambda (c)
                           (goto-char (car anything-current-position))
                           (anything-persistent-highlight-point
                            (line-beginning-position))))))


(defun anything-c-insert-linkd-action
    (c)
  (with-current-buffer anything-current-buffer
                  (goto-char (car anything-current-position))
                  ;; insert comment by bounded function to keyevent M-;
                  (call-interactively (key-binding "\M-\;"))
                  (insert (format "(@%s)"
                                  ;; *foo => * "foo", >foo => > "foo"
                                  (progn
                                    (string-match (rx bol (? (group (any "*" ">")))
                                                      (group (* anything)) eol)
                                                  c)
                                    (format "%s \"%s\""
                                            (or (match-string 1 c) "*")
                                            (match-string 2 c)))))))

;; (with-current-buffer "linkd.el<setting>" (let* ((anything-current-buffer (get-buffer "linkd.el")) (anything-sources '(anything-c-source-linkd-tag))) (anything)))

(provide 'anything-c-linkd-tags)
