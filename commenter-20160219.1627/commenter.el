;;; commenter.el --- multiline-comment support package -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/commenter
;; Package-Version: 20160219.1627
;; Package-Commit: 6d1885419434ba779270c6fda0e30d390bb074bd
;; Version: 0.5.2
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.4"))
;; Keywords: comment

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package allows you to set both single and multi line comment
;; variables like ‘comment-start’ or ‘comment-end’ etc.
;;
;; Configuration example for go-mode:
;;
;;   After this configuration your ‘comment-dwim’ will be multi line
;;   comment.
;;
;;   (setq comment-style 'extra-line)
;;
;;   (add-hook 'go-mode-hook
;;     (lambda ()
;;       (setq-local commenter-config
;;           '((single
;;              . ((comment-start      . "//")
;;                 (comment-end        . "")
;;                 (comment-start-skip . "\\(//+\\|/\\*+\\)\\s *")))
;;             (multi
;;              . ((comment-start      . "/* ")
;;                 (comment-end        . " */")
;;                 (comment-start-skip . "/\\*")
;;                 (comment-end-skip   . "\\*/")
;;                 (comment-continue   . " * ")
;;                 (comment-padding    . " ")
;;                 (comment-multi-line . t)))))
;;
;;       (commenter-setup)))
;;
;;; Code:
(require 'let-alist)

(defvar-local commenter-config nil
  "Buffer local variable for comment.

Available variables:
(values of inside parenthesis are default value)

  comment-start      (itself)
  comment-end        (itself)
  comment-start-skip (nil)
  comment-end-skip   (nil)
  comment-multi-line (nil)
  comment-continue   (nil)
  comment-padding    (1)
  comment-use-syntax (itself)

You can set comment-variable for single and multi line separately.

An example for go-mode:

(add-hook 'go-mode-hook
  (lambda ()
    (setq-local commenter-config
        '((single
           . ((comment-start      . \"//\")
              (comment-end        . \"\")
              (comment-start-skip . \"\\(//+\\|/\\*+\\)\\s *\")))
          (multi
           . ((comment-start      . \"/* \")
              (comment-end        . \" */\")
              (comment-start-skip . \"/\\*\")
              (comment-end-skip   . \"\\*/\")
              (comment-continue   . \" * \")
              (comment-padding    . \" \")
              (comment-multi-line . t)))))

    (commenter-setup)))")

(defvar commenter--already-called nil)

(defun commenter-vars-setup (&rest _args)
  "Dynamically change comment-xxx variables.
This function is used as advice to ‘comment-normalize-vars’."
  (let ((config commenter-config))
    (if (or (not config) commenter--already-called)
        nil ; don’t bother other modes
      (let* ((ppss (save-excursion ; <- not sure why this is needed
                     (syntax-ppss  ; but without this, multi line comment
                                   ; doesn’t work correctly
                      (cond ((and (or (use-region-p) (region-active-p)))
                             (1+ (min (region-beginning) (region-end)
                                      (1- (point-max)))))
                            (t (point))))))
             (single-or-multi
              (cond
               ;; if it’s called at non comment place,
               ;; check user’s setting (‘comment-style’)
               ((eq nil (nth 4 ppss))
                (if (commenter-multiline-style-p) 'multi 'single))
               ;; inside comment and it’s multi comment
               ((or (and (eq t (nth 4 ppss))
                         (commenter-multiline-comment-p ppss config))
                    ;; nested comment
                    (numberp (nth 4 ppss)))
                'multi)
               ;; inside comment and it’s single comment
               ((eq t (nth 4 ppss))
                'single))))
        (let-alist (assoc-default single-or-multi config)
          ;; Almost major-modes have default value
          (setq-local comment-start
                      (or .comment-start comment-start))
          (setq-local comment-end
                      (or .comment-end   comment-end))
          ;; Note that those comment-xxx-skip variables will be
          ;; checked by ‘comment-normalize-vars’, so keep in mind
          ;; that your regex matches comment-start or comment-end
          ;; (correspond to the comment-xxx-skip variable)
          (setq-local comment-start-skip .comment-start-skip)
          (setq-local comment-end-skip   .comment-end-skip)
          ;; This value affects M-j’s behavior
          (setq-local comment-multi-line .comment-multi-line)
          ;; This variable is for multiline comment.
          ;; Please make sure this variable is including non space
          ;; character, otherwise ‘uncomment-region’ doesn’t work
          ;; correctly. (probably)
          (setq-local comment-continue
                      (when comment-multi-line
                        .comment-continue))
          ;; 1 is default
          (setq-local comment-padding (or .comment-padding 1))
          ;; Default variable is ’undecided, so use default
          ;; value instead of nil if user doesn’t set.
          (setq-local comment-use-syntax
                      (or .comment-use-syntax comment-use-syntax)))))
    (setq commenter--already-called t)))

(defun commenter-multiline-comment-p (ppss config)
  (let-alist config
    (save-excursion
      (goto-char (nth 8 ppss))
      (looking-at .multi.comment-start-skip))))

(defun commenter-multiline-style-p ()
  "Return nil-nil if user setting is multi line comment style."
  (nth 0 (assoc-default comment-style comment-styles)))

;;;###autoload
(defun commenter-setup ()
  "Add advises to ‘comment-normalize-vars’."
  (advice-add 'comment-normalize-vars :before 'commenter-vars-setup)
  (advice-add 'fill-paragraph :before 'commenter-vars-setup))

(defun commenter-reset ()
  (setq commenter--already-called nil))

(add-hook 'post-command-hook 'commenter-reset)

(provide 'commenter)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; commenter.el ends here
