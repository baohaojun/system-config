(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(defun bhj-mu4e-view-with-thunderbird ()
  (interactive)
  (let ((msg-file (mu4e-field-at-point :path)))
    (shell-command-to-string (format "of %s >/dev/null 2>&1&" (shell-quote-argument msg-file)))))

(defun ajoke--mu4e-pop-mark ()
  "Pop mark in mu4e mode."
  (interactive)
  (ajoke-pop-mark)
  (delete-other-windows))

(define-key mu4e-headers-mode-map [(control meta ?,)] 'ajoke--mu4e-pop-mark)
(define-key mu4e-view-mode-map [(control meta ?,)] 'ajoke--mu4e-pop-mark)

(define-key mu4e-headers-mode-map [(control c) (v)] 'bhj-mu4e-view-with-thunderbird)
(define-key mu4e-view-mode-map [(control c) (v)] 'bhj-mu4e-view-with-thunderbird)
