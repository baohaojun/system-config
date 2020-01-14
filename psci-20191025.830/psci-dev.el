;;; psci-dev.el --- Utilities for helping in dev psci

;;; Commentary:

;;; Code:

(require 'psci)

;; ELISP> (with-current-buffer "*psci*"
;;          (psci/--debug-setup!))
;; ("comint-prompt-regexp" "^> "
;;  "paragraph-separate" "\\'"
;;  "paragraph-start" "^> "
;;  "comint-input-sender" comint-simple-send
;;  "comint-get-old-input" comint-get-old-input-default
;;  "comint-process-echoes" nil
;;  "comint-prompt-read-only" t
;;  "comint-eol-on-send" t
;;  "comint-input-filter-functions" nil
;;  "font-lock-defaults" (purescript-font-lock-keywords t)
;;  "comment-start" "-- "
;;  "comment-use-syntax" t)

(defun psci/--debug-setup! ()
  "A function to debug the current major mode's runtime setup."
  (list "comint-prompt-regexp" comint-prompt-regexp
        "paragraph-separate" paragraph-separate
        "paragraph-start" paragraph-start
        "comint-input-sender" comint-input-sender
        "comint-get-old-input" comint-get-old-input
        "comint-process-echoes" comint-process-echoes
        "comint-prompt-read-only" comint-prompt-read-only
        "comint-eol-on-send" comint-eol-on-send
        "comint-input-filter-functions" comint-input-filter-functions
        "font-lock-defaults" font-lock-defaults
        "comment-start" comment-start
        "comment-use-syntax" comment-use-syntax))

(provide 'psci-dev)
;;; psci-dev.el ends here
