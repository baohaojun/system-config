
;;; j-mode.el --- Major mode for editing J programs

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL: http://github.com/zellio/j-mode
;; Version: 1.1.1
;; Keywords: J, Languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:


(require 'comint)


;; (defconst j-console-version "1.1.1"
;;   "`j-console' version")

(defgroup j-console nil
  "REPL integration extention for `j-mode'"
  :group 'applications
  :group 'j
  :prefix "j-console-")

(defcustom j-console-cmd "ijconsole"
  "Name of the executable used for the J REPL session"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-args '()
  "Arguments to be passed to the j-console-cmd on start"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-init-file nil
  "Full path to the file who's contents are sent to the
  j-console-cmd on start

Should be NIL if there is no file not the empty string"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-buffer-name "J"
  "Name of the buffer which contains the j-console-cmd session"
  :type 'string
  :group 'j-console)

(defvar j-console-comint-input-filter-function nil
  "J mode specific mask for comint input filter function")

(defvar j-console-comint-output-filter-function nil
  "J mode specific mask for comint output filter function")

(defvar j-console-comint-preoutput-filter-function nil
  "J mode specific mask for comint preoutput filter function")

;; 'comint-preoutput-filter-functions
;; (lambda ( output )
;;   (if (string-match "^[ \r\n\t]+" output)
;;       (concat "  " (replace-match "" nil t output))
;;     output))))

(defun j-console-create-session ()
  "Starts a comint session wrapped around the j-console-cmd"
  (setq comint-process-echoes t)
  (apply 'make-comint j-console-cmd-buffer-name
         j-console-cmd j-console-cmd-init-file j-console-cmd-args)
  (mapc
   (lambda ( comint-hook-sym )
     (let ((local-comint-hook-fn-sym
            (intern
             (replace-regexp-in-string
              "s$" "" (concat "j-console-" (symbol-name comint-hook-sym))))))
       (when (symbol-value local-comint-hook-fn-sym)
         (add-hook comint-hook-sym (symbol-value local-comint-hook-fn-sym)))))
   '(comint-input-filter-functions
     comint-output-filter-functions
     comint-preoutput-filter-functions)))

(defun j-console-ensure-session ()
  "Checks for a running j-console-cmd comint session and either
  returns it or starts a new session and returns that"
  (or (get-process j-console-cmd-buffer-name)
      (progn
        (j-console-create-session)
        (get-process j-console-cmd-buffer-name))))

(define-derived-mode inferior-j-mode comint-mode "Inferior J"
  "Major mode for J inferior process.")

;;;###autoload
(defun j-console ()
  "Ensures a running j-console-cmd session and switches focus to
the containing buffer"
  (interactive)
  (switch-to-buffer-other-window (process-buffer (j-console-ensure-session)))
  (inferior-j-mode))

(defun j-console-execute-region ( start end )
  "Sends current region to the j-console-cmd session and exectues it"
  (interactive "r")
  (when (= start end)
    (error "Region is empty"))
  (let ((region (buffer-substring-no-properties start end))
        (session (j-console-ensure-session)))
    (pop-to-buffer (process-buffer session))
    (goto-char (point-max))
    (insert (format "\n%s\n" region))
    (comint-send-input)))

(defun j-console-execute-line ()
  "Sends current line to the j-console-cmd session and exectues it"
  (interactive)
  (j-console-execute-region (point-at-bol) (point-at-eol)))

(defun j-console-execute-buffer ()
  "Sends current buffer to the j-console-cmd session and exectues it"
  (interactive)
  (j-console-execute-region (point-min) (point-max)))

(provide 'j-console)

;;; j-console.el ends here
