(require 'anything)
(require 'imenu)

(unless (boundp 'anything-c-source-imenu)
  (defvar anything-c-source-imenu))

;;; (@* "setup source")
(setq anything-c-source-imenu
      `((name . "Imenu")
        (init . anything-c-imenu-init)
        (candidates-in-buffer)
        (get-line . buffer-substring)
        ;; experimental
        ,(if (fboundp 'anything-persistent-highlight-point)
             '(persistent-action . anything-c-imenu-preview)
             '(--unimplemented-attribute))
        ;; (cached-candidates . #hash( buffer -> candidates))
        (cached-candidates . ,(make-hash-table :test 'eq))
        (action
         . (("Goto point" . (lambda (m)
                              (goto-char (marker-position m))))
            ("Re-eval" . (lambda (m)
                           (save-excursion
                             (let ((beg (goto-char (marker-position m))))
                               (forward-sexp)
                               (eval-region beg (point))))))))))

;;; (@* "details")
(defun anything-c-imenu-preview (c)
  "highlight line that selected item is located"
  (anything-aif
      (case (type-of c)
        ('marker (marker-position c))
        ('number c)
        (otherwise (error (format "%c is not point or merker"
                                  (prin1-to-string c)))))
      (progn
        (goto-char it)
        (anything-persistent-highlight-point
         (line-beginning-position)))))

(defun anything-c-imenu-init ()
  (with-current-buffer (anything-candidate-buffer 'local)
    (loop for c
          in (anything-c-imenu-get-candidates)
          do (insert (propertize (car c)
                                 'anything-realvalue
                                 (cdr c)) "\n"))))

(defun anything-c-imenu-get-candidates ()
  "If anything-buffer is modified, updates candidates cache and return candidates.
Otherwise try to get cached candidates. If cant't get cached candidates in unmodified buffer,
force updates candidates."
  (if (anything-current-buffer-is-modified)
        (anything-c-imenu-update-cache)
        (or (anything-c-get-cached-candidates anything-current-buffer)
            (anything-c-imenu-update-cache))))

(defmacro anything-c-get-cached-candidates (key &optional source)
  `(gethash ,key
            (assoc-default 'cached-candidates
                          (or ,source
                              ;; anything-get-current-source works only in built-in-attribute procedure...
                              (anything-get-current-source)))))

(defun anything-imenu-update-assoc (sym key value)
  (set sym
       (acons key value
              (delete* key (symbol-value sym) :key 'car))))

(defun anything-c-imenu-update-cache ()
  (let ((candidates (anything-c-imenu-make-candidates-alist anything-current-buffer))
        (hash (assoc-default 'cached-candidates
                             (anything-get-current-source))))
    (puthash anything-current-buffer
             candidates
             hash)
    candidates))

(defun anything-c-imenu-make-candidates-alist (&optional buffer)
  "make (symbol . marker) by using imenu"
  (with-current-buffer (or buffer (current-buffer))
    (let* ((imenu-index (save-excursion
                          ;; buffer has no imenu-create-index-function
                          (condition-case err
                              (funcall imenu-create-index-function)
                            (error (message "imenu couldn't create index: %s" (cdr err))
                                   nil))))
           not-funcs
           funcs)
      ;; indexs -> not-funcs, funcs
      (loop for elem in imenu-index
            if (and (consp elem)
                    (stringp (car elem))
                    (listp (cdr elem)))
              do (push elem not-funcs)
            else
              do (push elem funcs))
      ;; not-funcs + funcs
      (append (apply 'append
                     ;; (("Variables" . (...)) ("Types" . (...)) ...)
                     (mapcar
                      (lambda (lst)
                        ;; ("Variables" . (...)) -> ("Variables / name" . #<merker>)
                        (mapcar (lambda (elem)
                                  (cons (format "%s / %s" (car lst) (car elem))
                                        (cdr elem)))
                                (cdr lst)))
                      not-funcs))
              funcs))))

;;; (@* "tests")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
  (expect (non-nil)
    (anything-c-imenu-make-candidates-alist))
  (expect (non-nil)
    (anything-test-candidates '(anything-c-source-imenu))))))

(provide 'anything-c-imenu)
