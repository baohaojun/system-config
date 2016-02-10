;;; hippie-expand-slime.el --- Hook slime's completion into hippie-expand

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/hippie-expand-slime
;; Version: 20130907.132
;; X-Original-Version: DEV

;;; Commentary:

;; Usage:

;; (require 'hippie-expand-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand))

;;; Code:

(require 'slime)
(require 'hippie-exp)

(defun try-expand-slime-with-fn (old complete-fn)
  "Completion function for `hippie-expand' which uses one of
slime's completion functions."
  (if (not old)
      (progn
        (he-init-string (slime-symbol-start-pos) (slime-symbol-end-pos))
        (if  (not (equal he-search-string ""))
            (setq he-expand-list
                  (sort (car (funcall complete-fn he-search-string))
                        'string-lessp))
          (setq he-expand-list ()))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))


;;;###autoload
(defun try-expand-slime (old)
  "Simple slime completion function for `hippie-expand'."
  (try-expand-slime-with-fn old 'slime-simple-completions))

;;;###autoload
(defun try-expand-slime-fuzzy (old)
  "Fuzzy slime completion function for `hippie-expand'."
  (try-expand-slime-with-fn old 'slime-fuzzy-completions))


;;;###autoload
(defun set-up-slime-hippie-expand (&optional fuzzy)
  "Add an optionally-fuzzy slime completion function to the front of
`hippie-expand-try-functions-list' for the current buffer."
  (interactive)
  (set (make-local-variable 'hippie-expand-try-functions-list) hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               (if fuzzy
                   'try-expand-slime-fuzzy
                 'try-expand-slime)))


(provide 'hippie-expand-slime)

;;; hippie-expand-slime.el ends here
