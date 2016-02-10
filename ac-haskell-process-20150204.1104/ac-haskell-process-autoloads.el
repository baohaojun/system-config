;;; ac-haskell-process-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ac-haskell-process" "ac-haskell-process.el"
;;;;;;  (21806 20665 663060 833000))
;;; Generated autoloads from ac-haskell-process.el

(defconst ac-source-haskell-process '((available . ac-haskell-process-available-p) (candidates . ac-haskell-process-candidates) (document . ac-haskell-process-doc) (symbol . "h")) "\
Haskell auto-complete source which uses the current haskell process.")

(autoload 'ac-haskell-process-setup "ac-haskell-process" "\
Add the haskell process completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

(autoload 'ac-haskell-process-popup-doc "ac-haskell-process" "\
Show documentation for the symbol at point in a popup.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ac-haskell-process-autoloads.el ends here
