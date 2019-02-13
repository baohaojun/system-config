;;; move-by-indentation.el --- moving around by indentaton

;;; (copied from https://emacs.stackexchange.com/questions/20900/navigate-by-indentation)

(defun indentation-get-next-good-line (direction skip good)
  "Moving in direction `direction', and skipping over blank lines and lines that
satisfy relation `skip' between their indentation and the original indentation,
finds the first line whose indentation satisfies predicate `good'."
  (let ((starting-indentation (current-indentation))
        (lines-moved direction))
    (save-excursion
      (while (and (zerop (forward-line direction))
                  (or (eolp) ; Skip past blank lines and other skip lines
                      (funcall skip (current-indentation) starting-indentation)))
        (setq lines-moved (+ lines-moved direction)))
      ;; Now we can't go further. Which case is it?
      (if (and
           (not (eobp))
           (not (bobp))
           (funcall good (current-indentation) starting-indentation))
          lines-moved
        nil))))

(defun indentation-get-next-sibling-line ()
  "The line number of the next sibling, if any."
  (indentation-get-next-good-line 1 '> '=))

(defun indentation-get-previous-sibling-line ()
  "The line number of the previous sibling, if any"
  (indentation-get-next-good-line -1 '> '=))

(defun indentation-get-parent-line ()
  "The line number of the parent, if any."
  (indentation-get-next-good-line -1 '>= '<))

(defun indentation-get-child-line ()
  "The line number of the first child, if any."
  (indentation-get-next-good-line +1 'ignore '>))


(defun indentation-move-to-line (func preserve-column name)
  "Move the number of lines given by func. If not possible, use `name' to say so."
  (let ((saved-column (current-column))
        (lines-to-move-by (funcall func)))
    (if lines-to-move-by
        (progn
          (forward-line lines-to-move-by)
          (move-to-column (if preserve-column
                              saved-column
                            (current-indentation))))
      (message "No %s to move to." name))))

(defun indentation-forward-to-next-sibling ()
  "Move to the next sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-next-sibling-line t "next sibling"))

(defun indentation-backward-to-previous-sibling ()
  "Move to the previous sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-previous-sibling-line t "previous sibling"))

(defun indentation-up-to-parent ()
  "Move to the parent line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-parent-line nil "parent"))

(defun indentation-down-to-child ()
  "Move to the first child line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-child-line nil "child"))

(global-set-key (kbd "C-c i C-f") 'indentation-forward-to-next-sibling)
(global-set-key (kbd "C-c i C-b") 'indentation-backward-to-previous-sibling)
(global-set-key (kbd "C-c i C-u") 'indentation-up-to-parent)
(global-set-key (kbd "C-c i C-d") 'indentation-down-to-child)
