;; -- POINT STACK -----------------------------------------------------------------------
;; matt harrison (matthewharrison@gmail.com)
;; dmitry gutov  (dgutov@yandex.ru)
;;
;; Provides forward/back stack for point.  I use load it like so:
;;
;; (add-to-list 'load-path "/home/matt/work/emacs/point-stack")
;; (require 'point-stack)
;; (global-set-key '[(f5)] 'point-stack-push)
;; (global-set-key '[(f6)] 'point-stack-pop)
;; (global-set-key '[(f7)] 'point-stack-forward-stack-pop)
;;
;; Then when I know I'm going to want to come back to where I am I hit
;; f5.  This stores the location of of the point.  When I want to come
;; back to that point hit f6.  I can go forward by hitting f7.
;;
;; based on http://www.emacswiki.org/emacs/JohnConnors
;; enhanced with forward-stack

(defvar point-stack-stack nil)
;; after you pop put it on the forward stack
(defvar point-stack-forward-stack nil)

(defun point-stack-push ()
  "Push current buffer, point, and scroll position onto stack."
  (interactive)
  (point-stack-store 'point-stack-stack)
  (setq point-stack-forward-stack nil) ; new step resets forward history
  (message "Location marked."))

(defun point-stack-pop ()
  "Push current location onto forward stack, move to previous location."
  (interactive)
  (if (null point-stack-stack)
      (message "Stack is empty.")
    (point-stack-store 'point-stack-forward-stack)
    (point-stack-go (car point-stack-stack))
    (setq point-stack-stack (cdr point-stack-stack))))

(defun point-stack-forward-stack-pop ()
  "Push current location onto stack, pop and move to location from forward stack."
  (interactive)
  (if (null point-stack-forward-stack)
      (message "forward Stack is empty.")
    (point-stack-store 'point-stack-stack)
    (point-stack-go (car point-stack-forward-stack))
    (setq point-stack-forward-stack (cdr point-stack-forward-stack))))

(defun point-stack-store (stack)
  (let ((loc (car (symbol-value stack))))
    ;; don't push the same location twice
    (unless (and (eq (current-buffer) (car loc))
                 (eq (point) (cadr loc)))
      (add-to-list stack (list (current-buffer) (point) (window-start))))))

(defun point-stack-go (loc)
  (switch-to-buffer (car loc))
  (set-window-start nil (caddr loc))
  (goto-char (cadr loc)))

(provide 'point-stack)
