;;; matlab-topic.el --- Help browsing via Emacs buffers.
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Browsing topics via the MATLAB shell in an Emacs buffer is a nice
;; feature.  Moving as optional as this is rarely used.

;;; Code:
(require 'matlab)
(require 'matlab-shell)
(require 'view)

(defvar matlab-shell-topic-mouse-face-keywords
  '(;; These are subtopic fields...
    ("^\\(\\w+/\\w+\\)[ \t]+-" 1 font-lock-reference-face)
    ;; These are functions...
    ("^[ \t]+\\(\\w+\\)[ \t]+-" 1 font-lock-function-name-face)
    ;; Here is a See Also line...
    ("[ \t]+See also "
     ("\\(\\w+\\)\\([,.]\\| and\\|$\\) *" nil  nil (1 font-lock-reference-face))))
  "These are keywords we also want to put mouse-faces on.")

(defvar matlab-shell-topic-font-lock-keywords
  (append matlab-shell-topic-mouse-face-keywords
	  '(("^[^:\n]+:$" 0 font-lock-keyword-face)
	    ;; These are subheadings...
	    ("^[ \t]+\\([^.\n]+[a-zA-Z.]\\)$" 1 'underline)
	    ))
  "Keywords useful for highlighting a MATLAB TOPIC buffer.")

(defvar matlab-shell-help-font-lock-keywords
  (append matlab-shell-topic-mouse-face-keywords
	  '(;; Function call examples
	    ("[ \t]\\([A-Z]+\\)\\s-*=\\s-*\\([A-Z]+[0-9]*\\)("
	     (1 font-lock-variable-name-face)
	     (2 font-lock-function-name-face))
	    ("[ \t]\\([A-Z]+[0-9]*\\)("
	     (1 font-lock-function-name-face))
	    ;; Parameters: Not very accurate, unfortunately.
	    ("[ \t]\\([A-Z]+[0-9]*\\)("
	     ("'?\\(\\w+\\)'?\\([,)]\\) *" nil  nil
	      (1 font-lock-variable-name-face))
	     )
	    ;; Reference uppercase words
	    ("\\<\\([A-Z]+[0-9]*\\)\\>" 1 font-lock-reference-face)))
  "Keywords for regular help buffers.")

;; View-major-mode is an emacs20 thing.  This gives us a small compatibility
;; layer.
(eval-and-compile
  (if (not (fboundp 'view-major-mode)) (defalias 'view-major-mode 'view-mode)))

(defvar matlab-shell-help-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'matlab-shell-help-choose)
    (define-key km "q" 'bury-buffer)
    (define-key km [(control h) (control m)] matlab-help-map)
    (if (string-match "XEmacs" emacs-version)
	(define-key km [button2] 'matlab-shell-help-click)
      (define-key km [mouse-2] 'matlab-shell-help-click)
      (define-key km [mouse-1] 'matlab-shell-help-click)
      )
    (set-keymap-parent km view-mode-map)
    km)
  "Keymap used in MATLAB help mode.")

(easy-menu-define
 matlab-shell-help-mode-menu matlab-shell-help-mode-map
 "MATLAB shell Help menu"
 '("MATLAB Help"
   ["Describe This Command" matlab-shell-help-choose t]
   "----"
   ["Describe Command" matlab-shell-describe-command t]
   ["Describe Variable" matlab-shell-describe-variable t]
   ["Command Apropos" matlab-shell-apropos t]
   "----"
   ["Exit" bury-buffer t]))

;; Need this to fix wierd problem in define-derived-mode
(defvar matlab-shell-help-mode-syntax-table (make-syntax-table)
  "Syntax table used in `matlab-shell-help-mode'.")

;;;###autoload
(define-derived-mode matlab-shell-help-mode
  view-major-mode "M-Help"
  "Major mode for viewing MATLAB help text.
Entry to this mode runs the normal hook `matlab-shell-help-mode-hook'.

Commands:
\\{matlab-shell-help-mode-map}"
  :syntax-table matlab-shell-help-mode-syntax-table
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-shell-help-font-lock-keywords)
			     t nil ((?_ . "w"))))
  ;; This makes sure that we really enter font lock since
  ;; kill-all-local-variables is not used by old view-mode.
  (and (boundp 'global-font-lock-mode) global-font-lock-mode
       (not font-lock-mode) (font-lock-mode 1))
  (matlab-shell-help-mouse-highlight-subtopics)
  (font-lock-ensure)
  )


(defun matlab-shell-help-click (e)
  "Click on an item in a MATLAB help buffer we want more information on.
Must be bound to event E."
  (interactive "e")
  (mouse-set-point e)
  (matlab-shell-help-choose))

(defun matlab-shell-help-choose ()
  "Choose the help to expand on that is under the cursor.
This can fill the help buffer with new information.  If the help is a
command, use `matlab-shell-describe-command' instead of changing the help
buffer."
  (interactive)
  (let ((topic nil) (fun nil) (p (point)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\w+/\\(\\w+\\)[ \t]+-")
	  (setq topic (match-string 1))
	(if (looking-at "^[ \t]+\\(\\(\\w\\|_\\)+\\)[ \t]+-")
	    (setq fun (match-string 1))
	  (if (and (not (looking-at "^[ \t]+See also"))
		   (not (save-excursion (forward-char -2)
					(looking-at ",$"))))
	      ;;(error "You did not click on a subtopic, function or reference")
	      nil
	    (goto-char p)
	    (forward-word -1)
	    (if (not (looking-at "\\(\\(\\w\\|_\\)+\\)\\([.,]\\| and\\|\n\\)"))
		;;(error "You must click on a reference")
		nil
	      (setq topic (match-string 1)))))))
    ;;(message "Opening item %s..." (or topic fun))
    (when (or fun topic)
	(matlab-shell-describe-command (downcase (or fun topic))))
    ))

(defun matlab-shell-help-mouse-highlight-subtopics ()
  "Put a `mouse-face' on all clickable targets in this buffer."
  (save-excursion
    (let ((el matlab-shell-topic-mouse-face-keywords)
	  (inhibit-read-only t))
      (while el
	(goto-char (point-min))
	(while (re-search-forward (car (car el)) nil t)
	  (let ((cd (car (cdr (car el)))))
	    (if (numberp cd)
		(put-text-property (match-beginning cd) (match-end cd)
				   'mouse-face 'highlight)
	      (while (re-search-forward (car cd) nil t)
		(put-text-property (match-beginning (car (nth 3 cd)))
				   (match-end (car (nth 3 cd)))
				   'mouse-face 'highlight)))))
	(setq el (cdr el))))))

(defvar mouse-grabbed-buffer) ;; Suppress compilation warning in Emacs (an XEmacs only variable)
(defvar mode-motion-extent)   ;; Suppress compilation warning in Emacs (an XEmacs only variable)

(defun matlab-shell-topic-highlight-line (event)
  "A value of `mode-motion-hook' which will highlight topics under the mouse.
EVENT is the user mouse event."
  ;; XEMACS only function
  (let* ((buffer (when (fboundp 'event-buffer) (event-buffer event)))
	 (point (and buffer (when (fboundp 'event-point) (event-point event)))))
    (if (and buffer (not (eq buffer mouse-grabbed-buffer)))
	(save-excursion
	  (save-window-excursion
	    (set-buffer buffer)
	    (when (fboundp 'mode-motion-ensure-extent-ok) (mode-motion-ensure-extent-ok event))
	    (if (not point)
		(when (fboundp 'detach-extent) (detach-extent mode-motion-extent))
	      (goto-char point)
	      (end-of-line)
	      (setq point (point))
	      (beginning-of-line)
	      (if (or (looking-at "^\\w+/\\(\\w+\\)[ \t]+-")
		      (looking-at "^[ \t]+\\(\\(\\w\\|_\\)+\\)[ \t]+-"))
		  (when (fboundp 'set-extent-endpoints)
                    (set-extent-endpoints mode-motion-extent (point) point))
		(when (fboundp 'detach-extent) (detach-extent mode-motion-extent)))))))))

(provide 'matlab-topic)

;;; matlab-topic.el ends here
