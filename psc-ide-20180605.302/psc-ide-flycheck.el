;;; psc-ide-flycheck.el --- Flycheck support for the purescript language -*- lexical-binding: t -*-

;; Copyright (c) 2015 The psc-ide-emacs authors
;; License: GNU General Public License version 3, or (at your option) any later version

;; Author: Brian Sermons
;; URL: https://github.com/epost/psc-ide-emacs

;;; Commentary:

;; Usage:
;;
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'psc-ide-flycheck-setup))

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))

(require 'seq)
(require 'json)
(require 'dash)
(require 'flycheck)
(require 'psc-ide-protocol)


(flycheck-def-option-var psc-ide-flycheck-ignored-error-codes nil psc-ide
  "List of errors codes to ignore."
  :tag "Flycheck PscIde Ignored Error Codes"
  :type '(repeat string))


(defun psc-ide-flycheck-parse-errors (data checker)
  "Decode purescript json output errors from DATA with CHECKER."
  (let-alist data
    (let ((errors)
          (resultType (pcase .resultType
                        (`"success" 'warning)
                        (_ 'error))))
      (seq-do (lambda (err)
                (let-alist err
                  (unless (member .errorCode psc-ide-flycheck-ignored-error-codes)
                    (let ((replacePos (if .suggestion.replaceRange
                                          .suggestion.replaceRange
                                        .position)))

                      (put-text-property 0 1 :suggestion .suggestion .errorCode)
                      (put-text-property 0 1 :startLine (cdr (assoc 'startLine replacePos)) .errorCode)
                      (put-text-property 0 1 :startColumn (cdr (assoc 'startColumn replacePos)) .errorCode)
                      (put-text-property 0 1 :endLine (cdr (assoc 'endLine replacePos)) .errorCode)
                      (put-text-property 0 1 :endColumn (cdr (assoc 'endColumn replacePos)) .errorCode)

                      (push
                       (flycheck-error-new-at
                        .position.startLine
                        .position.startColumn
                        resultType
                        .message
                        :id .errorCode
                        :checker checker
                        :filename .filename)
                       errors)))))
              .result)
      errors)))

;;;###autoload
(defun psc-ide-flycheck-insert-suggestion ()
  "Replace error with suggestion from psc compiler."
  (interactive)
  (-if-let* ((flycheck-err (car (flycheck-overlay-errors-at (point))))
             (suggestion (get-text-property 0 :suggestion (flycheck-error-id flycheck-err)))
             (startLine (get-text-property 0 :startLine (flycheck-error-id flycheck-err)))
             (startColumn (get-text-property 0 :startColumn (flycheck-error-id flycheck-err)))
             (endLine (get-text-property 0 :endLine (flycheck-error-id flycheck-err)))
             (endColumn (get-text-property 0 :endColumn (flycheck-error-id flycheck-err))))
      (let* ((start (save-excursion
                      (goto-char (point-min))
                      (forward-line (- startLine 1))
                      (move-to-column (- startColumn 1))
                      (point)))
             (end (save-excursion
                    (goto-char (point-min))
                    (forward-line (- endLine 1))
                    (move-to-column (- endColumn 1))
                    (point))))
        (progn
          (kill-region start end)
          (goto-char start)
          (let ((new-end
                 (save-excursion
                   (let-alist suggestion
                     (insert (replace-regexp-in-string "\n\\'" "" .replacement)))
                   (point))))
            (set-mark start)
            (goto-char new-end)
            (deactivate-mark))
          (save-buffer)
          (flycheck-buffer)))
    (message "No suggestion available")))

(define-key psc-ide-mode-map (kbd "C-c M-s")
  'psc-ide-flycheck-insert-suggestion)

(defun psc-ide-flycheck-copy-related-files (original temp-file)
  (let ((source-js (concat (file-name-directory original)
                           (file-name-base original)
                           ".js"))
        (target-js (concat (file-name-directory temp-file)
                           (file-name-base temp-file)
                           ".js")))
    (when (file-exists-p source-js)
      (copy-file source-js target-js t)
      (push target-js flycheck-temporaries))))

(defun psc-ide-flycheck-start (checker callback)
  "Start a psc-ide syntax check with CHECKER.

CALLBACK is the status callback passed by flycheck."

  (let ((temp-file (flycheck-save-buffer-to-temp #'flycheck-temp-file-system)))
    (psc-ide-flycheck-copy-related-files (buffer-file-name) temp-file)
    (psc-ide-send (psc-ide-command-rebuild temp-file (buffer-file-name))
                  (lambda (result)
                    (condition-case err
                        (progn
                          (let ((errors (psc-ide-flycheck-parse-errors result checker)))
                            (funcall callback 'finished errors)))
                      (`(error debug)
                       (flycheck-safe-delete-temporaries)
                       (funcall callback 'errored (error-message-string err))))))))

(flycheck-define-generic-checker 'psc-ide
  "A purescript syntax checker using the `psc-ide' interface."
  :start #'psc-ide-flycheck-start
  :modes '(purescript-mode))


;;;###autoload
(defun psc-ide-flycheck-setup ()
  "Setup Flycheck purescript."
  (interactive)
  (add-to-list 'flycheck-checkers 'psc-ide))

(provide 'psc-ide-flycheck)
;;; psc-ide-flycheck.el ends here
