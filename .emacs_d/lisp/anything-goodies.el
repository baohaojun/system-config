(require 'anything)
(provide 'anything-goodies)

(defmacro anything-candidate-in-buffer-scraper (buf search-proc)
  `(with-current-buffer ,buf
    (goto-char (point-min))
    (let ((anything-pattern (substring-no-properties anything-pattern)))
      (cond
      ;; list all candidates
        ((zerop (length anything-pattern))
         (loop with cnt = anything-candidate-number-limit
               until (or (zerop cnt) (eobp))
               collect (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position))
               do (decf cnt) (forward-line)))
      ;; if pattern may cause infinit loop, return nothing
      ((or (string-match (rx bol (+ space) eol) anything-pattern)
           (string-equal "" anything-pattern)) nil)
      ;; search
      (t (loop with cnt = anything-candidate-number-limit
               while (and (not (eobp))
                          (> cnt 0)
                          ,search-proc)
               collect (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position))
               do (decf cnt)
                     (forward-line)))))))
