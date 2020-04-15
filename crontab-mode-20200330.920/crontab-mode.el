;;; crontab-mode.el --- Major mode for crontab(5)     -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/crontab-mode
;; Package-Version: 20200330.920
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for crontab(5) files

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'sh-script)

(defgroup crontab nil
  "Major mode for editing crontab(5) files."
  :prefix "crontab-"
  :group 'languages)

(defface crontab-minute
  '((t :inherit outline-1))
  "Face to use for highlighting crontab minute field."
  :group 'crontab)

(defface crontab-hour
  '((t :inherit outline-2))
  "Face to use for highlighting crontab hour field."
  :group 'crontab)

(defface crontab-month-day
  '((t :inherit outline-3))
  "Face to use for highlighting crontab day of month field."
  :group 'crontab)

(defface crontab-month
  '((t :inherit outline-4))
  "Face to use for highlighting crontab month field."
  :group 'crontab)

(defface crontab-week-day
  '((t :inherit outline-5))
  "Face to use for highlighting crontab day of week field."
  :group 'crontab)

(defface crontab-predefined
  '((t :inherit outline-1))
  "Face to use for crontab predefined definitions."
  :group 'crontab)

(defvar crontab-fields '("minute (0-59)" "hour (0-23)" "day (1-31)" "month (1-12)" "day-of-week (0-6)" "command")
  "Fields used by `crontab-eldoc-function' to show the crontab information.")

(eval-and-compile
  (defconst crontab-rx-constituents
    ;; https://en.wikipedia.org/wiki/Cron#CRON_expression
    `((unit    . ,(rx (+ (in "-,*" num))))
      (step    . ,(rx (? "/" (+ num))))
      (month   . ,(rx (or "jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")))
      (weekday . ,(rx (or "sun" "mon" "tue" "wed" "thu" "fri" "sat")))
      )
    "Additional specific sexps for `crontab-rx'")

  (defmacro crontab-rx (&rest regexps)
    "Crontab specialized rx macro."
    (let ((rx-constituents (append crontab-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

(defvar crontab-font-lock-keywords
  `(
    ;;  ┌───────────────────────── min (0 - 59)
    ;;  │ ┌─────────────────────── hour (0 - 23)
    ;;  │ │ ┌───────────────────── day of month (1 - 31)
    ;;  │ │ │ ┌─────────────────── month (1 - 12)
    ;;  │ │ │ │ ┌───────────────── day of week (0 - 6) (Sunday to Saturday; 7 is also Sunday)
    ;;  │ │ │ │ │
    ;;  │ │ │ │ │
    ;;  │ │ │ │ │
    ;;  * * * * *  command to execute
    (,(crontab-rx line-start (* space)
                  (group unit (? step)) (+ space) ; minutes
                  (group unit (? step)) (+ space) ; hours
                  (group (or (seq unit (? step)) "?" "L" "W")) (+ space) ; day of month
                  (group (or unit month) (? step)) (+ space)             ; month
                  (group (or unit weekday) (? step)) (+ space) ; day of week
                  (group (+ not-newline))                      ; command
                  line-end)
     (1 'crontab-minute)
     (2 'crontab-hour)
     (3 'crontab-month-day)
     (4 'crontab-month)
     (5 'crontab-week-day))

    ;; Nonstandard predefined scheduling definitions
    (,(rx line-start (* space)
          (group (or "@reboot" "@yearly" "@annually"
                     "@monthly" "@weekly" "@daily" "@hourly"))
          (+ space)
          (group (+ not-newline))       ; Command
          line-end)
     (1 'crontab-predefined))

    ;; Variables
    ("^\\([^#=]+\\)=\\(.*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face)))
  "Info for function `font-lock-mode'.")

(defun crontab-indent-line ()
  "Indent current line as crontab mode."
  (interactive)
  (indent-line-to 0))

(defun crontab-eldoc-function ()
  "`eldoc-documentation-function' for Crontab."
  (let* ((point (point))
         (end-of-line (point-at-eol))
         (fields (copy-sequence crontab-fields))
         (n (save-excursion
              (beginning-of-line)
              (cl-loop while (re-search-forward "[^[:space:]]+" end-of-line t)
                       with field = -1
                       do (cl-incf field)
                       if (or (>= (point) point) (>= field 5))
                       return field))))
    (when n
      (setcar (nthcdr n fields) (propertize (elt fields n) 'face 'font-lock-constant-face)))
    (mapconcat 'identity fields "  ")))

;;;###autoload
(define-derived-mode crontab-mode text-mode "Crontab"
  "Major mode for editing crontab file.

\\{crontab-mode-map}"
  :syntax-table sh-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (if (null eldoc-documentation-function) ; Emacs<25
      (set (make-local-variable 'eldoc-documentation-function)
           #'crontab-eldoc-function)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'crontab-eldoc-function))

  (set (make-local-variable 'font-lock-defaults)
       '(crontab-font-lock-keywords nil t))

  (set (make-local-variable 'indent-line-function)
       'crontab-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("/crontab\\(\\.X*[[:alnum:]]+\\)?\\'" . crontab-mode))

(provide 'crontab-mode)
;;; crontab-mode.el ends here
