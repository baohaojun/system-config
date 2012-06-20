;;;  -*- coding: utf-8; mode: emacs-lisp; -*-
;;; anything-c-moccur.el

;; Author: Kenji.Imakado <ken.imakaado -at- gmail.com>
;; Keywords: occur
;; Prefix: anything-c-moccur-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; Tested on Emacs 22

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-c-moccur-from-isearch'
;;    Run `anything-c-moccur-occur-by-moccur' with isearch string.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-c-moccur-anything-idle-delay'
;;    anything-c-moccurが提供するコマンドでanythingが起動された際の`anything-idle-delay'の値
;;    default = nil
;;  `anything-c-moccur-push-mark-flag'
;;    non-nilならコマンド起動時に現在のポイントにマークをセットする
;;    default = nil
;;  `anything-c-moccur-widen-when-goto-line-flag'
;;    non-nilなら必要に応じてナローイングを解除する
;;    default = nil
;;  `anything-c-moccur-show-all-when-goto-line-flag'
;;    non-nilなら必要に応じてoutlineの折畳み表示を解除する
;;    default = nil
;;  `anything-c-moccur-higligt-info-line-flag'
;;    non-nilならdmoccur, dired-do-moccurの候補を表示する際にバッファ名などの情報をハイライト表示する
;;    default = nil
;;  `anything-c-moccur-enable-auto-look-flag'
;;    non-nilなら選択中の候補を他のバッファにリアルタイムに表示する
;;    default = nil
;;  `anything-c-moccur-enable-initial-pattern'
;;    non-nilなら`anything-c-moccur-occur-by-moccur'を起動する際に、ポイントの位置の単語をpatternの初期値として起動する。
;;    default = nil
;;  `anything-c-moccur-use-moccur-anything-map-flag'
;;    non-nilならanything-c-moccurのデフォルトのキーバインドを使用する
;;    default = t
;;  `anything-c-moccur-recenter-count'
;;    これは選択した候補の位置にポイントを移動した後に呼ばれる 関数`recenter'に引数として渡される値である
;;    default = 10
;;  `anything-c-moccur-preselect-current-line'
;;    *Preselect current line in *anything moccur* buffer.
;;    default = t

;; sample config
;; (require 'anything-c-moccur)
;; (global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
;; (global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur)
;; (add-hook 'dired-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))
;; (global-set-key (kbd "C-M-s") 'anything-c-moccur-isearch-forward)
;; (global-set-key (kbd "C-M-r") 'anything-c-moccur-isearch-backward)

;;; Todo:
;; resume

;;;code:

(require 'anything)
(require 'cl)
(require 'color-moccur)
(require 'rx)

(defgroup anything-c-moccur nil
  "anything config moccur"
  :group 'anything-c-moccur)


(defcustom anything-c-moccur-anything-idle-delay nil
  "anything-c-moccurが提供するコマンドでanythingが起動された際の`anything-idle-delay'の値
nilなら`anything-idle-delay'の値を使う"
  :type '(choice (number)
                 (boolean))
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-push-mark-flag nil
  "non-nilならコマンド起動時に現在のポイントにマークをセットする"
  :type 'boolean
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-widen-when-goto-line-flag nil
  "non-nilなら必要に応じてナローイングを解除する"
  :type 'boolean
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-show-all-when-goto-line-flag nil ;outline
  "non-nilなら必要に応じてoutlineの折畳み表示を解除する"
  :type 'boolean
  :group 'anything-c-moccur
  )

(defcustom anything-c-moccur-higligt-info-line-flag nil
  "non-nilならdmoccur, dired-do-moccurの候補を表示する際にバッファ名などの情報をハイライト表示する"
  :type 'boolean
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-enable-auto-look-flag nil
  "non-nilなら選択中の候補を他のバッファにリアルタイムに表示する"
  :type 'boolean
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-enable-initial-pattern nil
  "non-nilなら`anything-c-moccur-occur-by-moccur'を起動する際に、ポイントの位置の単語をpatternの初期値として起動する。"
  :type 'boolean
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-use-moccur-anything-map-flag t
  "non-nilならanything-c-moccurのデフォルトのキーバインドを使用する
nilなら使用しない"
  :type 'boolean
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-recenter-count 10
  "これは選択した候補の位置にポイントを移動した後に呼ばれる 関数`recenter'に引数として渡される値である"
  :type '(choice (integer)
                 (boolean))
  :group 'anything-c-moccur)

(defcustom anything-c-moccur-preselect-current-line t
  "*Preselect current line in *anything moccur* buffer."
  :type 'boolean  
  :group 'anything-c-moccur)

;;; variables
(defvar anything-c-moccur-version 0.33)
(defvar anything-c-moccur-anything-invoking-flag nil)
(defvar anything-c-moccur-anything-initial-pattern "")
(defvar anything-c-moccur-anything-current-buffer nil)
(defvar anything-c-moccur-saved-info nil)
(defvar anything-c-moccur-buffer "*anything moccur*")
(defvar anything-c-moccur-anything-map
  (let ((map (copy-keymap anything-map)))
    (when anything-c-moccur-use-moccur-anything-map-flag
      (define-key map (kbd "D")  'anything-c-moccur-wrap-symbol)
      (define-key map (kbd "W")  'anything-c-moccur-wrap-word)
      (define-key map (kbd "F")  'anything-c-moccur-match-only-function)
      (define-key map (kbd "C")  'anything-c-moccur-match-only-comment)
      (define-key map (kbd "S")  'anything-c-moccur-match-only-string)

      (define-key map (kbd "U")  'anything-c-moccur-start-symbol)
      (define-key map (kbd "I")  'anything-c-moccur-end-symbol)
      (define-key map (kbd "O")  'anything-c-moccur-start-word)
      (define-key map (kbd "P")  'anything-c-moccur-end-word)

      (define-key map (kbd "J")  'scroll-other-window)
      (define-key map (kbd "K")  'scroll-other-window-down)

      ;; anything
      (define-key map (kbd "C-n")  'anything-c-moccur-next-line)
      (define-key map (kbd "C-p")  'anything-c-moccur-previous-line)

      (define-key map (kbd "C-M-f")  'anything-c-moccur-anything-next-file-matches)
      (define-key map (kbd "C-M-b")  'anything-c-moccur-anything-previous-file-matches)

      (define-key map (kbd "C-M-%")  'anything-c-moccur-query-replace-regexp)
      )
    map))

;;overlay
(defvar anything-c-moccur-current-line-overlay
  (make-overlay (point) (point)))

;;; utilities
(defun anything-c-moccur-widen-if-need ()
  (when anything-c-moccur-widen-when-goto-line-flag
    (widen))
  (when anything-c-moccur-show-all-when-goto-line-flag
    (require 'outline)
    (show-all)))

;; regexp from `moccur-get-info'
(defvar anything-c-moccur-info-line-re "^[-+ ]*Buffer:[ ]*\\([^\r\n]*\\) File\\([^:/\r\n]*\\):[ ]*\\([^\r\n]+\\)$")

(defun anything-c-moccur-anything-move-selection-if-info-line (direction)
  (unless (= (buffer-size (get-buffer anything-buffer)) 0)
    (with-current-buffer anything-buffer
      (let ((re anything-c-moccur-info-line-re))
        (when (save-excursion
                (beginning-of-line)
                (looking-at re))
          (case direction
            (next (anything-next-line))
            (previous (anything-previous-line)))))
      (anything-mark-current-line))))

(defun anything-c-moccur-next-line-if-info-line ()
  (anything-c-moccur-anything-move-selection-if-info-line 'next))

(defun anything-c-moccur-previous-line-if-info-line ()
  (anything-c-moccur-anything-move-selection-if-info-line 'previous))

(defun anything-c-moccur-get-info ()
  "return (values buffer file)"
  (cond
   (anything-c-moccur-saved-info
    anything-c-moccur-saved-info)
   (t
    (unless (or (= (buffer-size (get-buffer anything-buffer)) 0))
      (with-current-buffer anything-buffer
        (save-excursion
          (let ((re anything-c-moccur-info-line-re))
            (when (re-search-backward re nil t)
              (values (match-string-no-properties 1) ;buffer
                      (match-string-no-properties 3))))))))))

(defun anything-c-moccur-anything-move-selection (unit direction)
  (unless (or (= (buffer-size (get-buffer anything-buffer)) 0)
              (not (get-buffer-window anything-buffer 'visible)))
    (save-selected-window
      (select-window (get-buffer-window anything-buffer 'visible))

      (case unit
        (file (let ((search-fn (case direction
                                 (next 're-search-forward)
                                 (previous (prog1 're-search-backward
                                             (re-search-backward anything-c-moccur-info-line-re nil t)))
                                 (t (error "Invalid direction.")))))
                ;;(funcall search-fn (rx bol "Buffer:" (* not-newline) "File:") nil t)))
                (funcall search-fn anything-c-moccur-info-line-re nil t)))

        (t (error "Invalid unit.")))

      (while (anything-pos-header-line-p)
        (forward-line (if (and (eq direction 'previous)
                               (not (eq (line-beginning-position)
                                        (point-min))))
                          -1
                        1)))

      (if (eobp)
          (forward-line -1))
      (anything-mark-current-line)

      ;; top
      (recenter 0))))

(defun anything-c-moccur-anything-next-file-matches ()
  (interactive)
  (anything-c-moccur-anything-move-selection 'file 'next)
  (anything-c-moccur-next-line-if-info-line)
  (anything-c-moccur-anything-try-execute-persistent-action))

(defun anything-c-moccur-anything-previous-file-matches ()
  (interactive)
  (anything-c-moccur-anything-move-selection 'file 'previous)
  (anything-c-moccur-next-line-if-info-line)
  (anything-c-moccur-anything-try-execute-persistent-action))

(defun anything-c-moccur-initialize ()
  (setq anything-c-moccur-saved-info nil
        anything-c-moccur-anything-invoking-flag t))

(defun anything-c-moccur-anything-try-execute-persistent-action ()
  (when (and anything-c-moccur-enable-auto-look-flag
             anything-c-moccur-anything-invoking-flag)
    (unless (zerop (buffer-size (get-buffer (anything-buffer-get))))
      (anything-execute-persistent-action))))

(defun anything-c-moccur-preselect-current-line-maybe ()
  (and anything-c-moccur-preselect-current-line
       (anything-preselect (format "^ *%d "
                                   (with-current-buffer anything-current-buffer
                                     (line-number-at-pos))))))


(defvar anything-c-moccur-last-buffer nil)
(defmacro anything-c-moccur-with-anything-env (sources &rest body)
  (declare (indent 1))
  `(let ((anything-buffer anything-c-moccur-buffer)
         (anything-sources ,sources)
         (anything-map anything-c-moccur-anything-map)
         (anything-idle-delay (cond
                               ((integerp anything-c-moccur-anything-idle-delay)
                                anything-c-moccur-anything-idle-delay)
                               (t anything-idle-delay))))
     (add-hook 'anything-update-hook 'anything-c-moccur-preselect-current-line-maybe)
     (add-hook 'anything-c-moccur-anything-after-update-hook 'anything-c-moccur-anything-try-execute-persistent-action)
     (unwind-protect
         (progn
           ,@body)
       (remove-hook 'anything-c-moccur-anything-after-update-hook 'anything-c-moccur-anything-try-execute-persistent-action)
       (remove-hook 'anything-update-hook 'anything-c-moccur-preselect-current-line-maybe)
       (setq anything-c-moccur-last-buffer anything-current-buffer))))


(defun anything-c-moccur-clean-up ()
  (setq anything-c-moccur-anything-invoking-flag nil)
  (when (overlayp anything-c-moccur-current-line-overlay)
    (delete-overlay anything-c-moccur-current-line-overlay)))

;; (anything-next-line) 後のanything-update-hook
;; persistent-actionを動作させるために実装
(defvar anything-c-moccur-anything-after-update-hook nil)
(defadvice anything-process-delayed-sources (after anything-c-moccur-anything-after-update-hook activate protect)
  (when (and (boundp 'anything-c-moccur-anything-invoking-flag)
             anything-c-moccur-anything-invoking-flag)
    (ignore-errors
      (run-hooks 'anything-c-moccur-anything-after-update-hook))))

(defadvice anything-select-action (before anything-c-moccur-saved-info activate)
  (when (and (boundp 'anything-c-moccur-anything-invoking-flag)
             anything-c-moccur-anything-invoking-flag)
    (ignore-errors
      (unless anything-c-moccur-saved-info
        (setq anything-c-moccur-saved-info (anything-c-moccur-get-info))))))

(defadvice moccur-search (around anything-c-moccur-no-window-change)
  (cond
   ((and (boundp 'anything-c-moccur-anything-invoking-flag)
         anything-c-moccur-anything-invoking-flag)
    (let ((regexp (ad-get-arg 0))
          (arg (ad-get-arg 1))
          (buffers (ad-get-arg 2)))
      (when (or (not regexp)
                (string= regexp ""))
        (error "No search word specified!"))
      ;; initialize
      (let ((lst (list regexp arg buffers)))
        (if (equal lst (car moccur-searched-list))
            ()
          (setq moccur-searched-list (cons (list regexp arg buffers) moccur-searched-list))))
      (setq moccur-special-word nil)
      (moccur-set-regexp)
      (moccur-set-regexp-for-color)
      ;; variable reset
      (setq dmoccur-project-name nil)
      (setq moccur-matches 0)
      (setq moccur-match-buffers nil)
      (setq moccur-regexp-input regexp)
      (if (string= (car regexp-history) moccur-regexp-input)
          ()
        (setq regexp-history (cons moccur-regexp-input regexp-history)))
      (save-excursion
        (setq moccur-mocur-buffer (generate-new-buffer "*Moccur*"))
        (set-buffer moccur-mocur-buffer)
        (insert "Lines matching " moccur-regexp-input "\n")
        (setq moccur-buffers buffers)
        ;; search all buffers
        (while buffers
          (if (and (car buffers)
                   (buffer-live-p (car buffers))
                   ;; if b:regexp exists,
                   (if (and moccur-file-name-regexp
                            moccur-split-word)
                       (string-match moccur-file-name-regexp (buffer-name (car buffers)))
                     t))
              (if (and (not arg)
                       (not (buffer-file-name (car buffers))))
                  (setq buffers (cdr buffers))
                (if (moccur-search-buffer (car moccur-regexp-list) (car buffers))
                    (setq moccur-match-buffers (cons (car buffers) moccur-match-buffers)))
                (setq buffers (cdr buffers)))
            ;; illegal buffer
            (setq buffers (cdr buffers)))))))
   (t
    ad-do-it)))

(defun anything-c-moccur-bad-regexp-p (re)
  (or (string-match (rx bol (+ space) eol) re)
      (string-equal "" re)
      (string-match (rx (or bol (+ space)) (+ (any "<" ">" "\\" "_" "`")) (or eol (+ space ))) re)))

(defun anything-c-moccur-moccur-search (regexp arg buffers)
  (ignore-errors
    (unwind-protect
        (progn
          ;; active advice
          (ad-enable-advice 'moccur-search 'around 'anything-c-moccur-no-window-change)
          (ad-activate 'moccur-search)
          ;; 空白のみで呼ばれると固まることがあったので追加
          (when (anything-c-moccur-bad-regexp-p anything-pattern)
            (error ""))

          (save-window-excursion
            (moccur-setup)
            (moccur-search regexp arg buffers)))
      ;; disable advance
      (ad-disable-advice 'moccur-search 'around 'anything-c-moccur-no-window-change)
      (ad-activate 'moccur-search))))

(defun anything-c-moccur-occur-by-moccur-scraper ()
  (when (buffer-live-p moccur-mocur-buffer)
    (with-current-buffer moccur-mocur-buffer
      (let* ((buf (buffer-substring (point-min) (point-max)))
             (lines (delete "" (subseq (split-string buf "\n") 3))))
        lines))))

(defun anything-c-moccur-occur-by-moccur-get-candidates ()
  (anything-c-moccur-moccur-search anything-pattern t (list anything-current-buffer))
  (anything-c-moccur-occur-by-moccur-scraper))

(defun anything-c-moccur-occur-by-moccur-persistent-action (candidate)
  (anything-c-moccur-widen-if-need)
  (goto-line (string-to-number candidate))
  (recenter anything-c-moccur-recenter-count)
  (when (overlayp anything-c-moccur-current-line-overlay)
    (move-overlay anything-c-moccur-current-line-overlay
                  (line-beginning-position)
                  (line-end-position)
                  (current-buffer))
    (overlay-put anything-c-moccur-current-line-overlay 'face 'highlight)))

(defun anything-c-moccur-occur-by-moccur-goto-line (candidate)
  (anything-c-moccur-widen-if-need)     ;utility
  (goto-line (string-to-number candidate))
  (recenter anything-c-moccur-recenter-count))

(defvar anything-c-source-occur-by-moccur
  `((name . "Occur by Moccur")
    (candidates . anything-c-moccur-occur-by-moccur-get-candidates)
    (action . (("Goto line" . anything-c-moccur-occur-by-moccur-goto-line)))
    (persistent-action . anything-c-moccur-occur-by-moccur-persistent-action)
    (init . anything-c-moccur-initialize)
    (cleanup . anything-c-moccur-clean-up)
    (match . (identity))
    (requires-pattern . 3)
    (delayed)
    (volatile)))

(defun anything-c-moccur-occur-by-moccur-base (initial-pattern)
  (anything-c-moccur-with-anything-env (list anything-c-source-occur-by-moccur)
    (and anything-c-moccur-push-mark-flag (push-mark))
    (anything nil initial-pattern)))

(defun anything-c-moccur-occur-by-moccur (&optional prefix)
  (interactive "P")
  (if prefix
      (anything-c-moccur-resume)
    (anything-c-moccur-occur-by-moccur-base
     (if anything-c-moccur-enable-initial-pattern
         (regexp-quote (or (thing-at-point 'symbol) ""))
       ""))))

(defun anything-c-moccur-occur-by-moccur-only-function ()
  (interactive)
  (anything-c-moccur-occur-by-moccur-base "! "))

(defun anything-c-moccur-occur-by-moccur-only-comment ()
  (interactive)
  (anything-c-moccur-occur-by-moccur-base ";;; "))

(defun anything-c-moccur-query-replace-regexp ()
  (interactive)
  (lexical-let ((input-re (minibuffer-contents))
                (cur-point (first anything-current-position)))
    (setq anything-saved-action (lambda (dummy)
                                  (let ((to-string (read-from-minibuffer "to: " input-re)))
                                    (unwind-protect
                                        (perform-replace input-re to-string t t nil nil nil (point-min) (point-max))
                                      (goto-char cur-point)))))
    (anything-exit-minibuffer)))

;; e.x, (global-set-key (kbd "C-c f") (anything-c-moccur-define-occur-command "defun "))
;; rubikitch: This is replaced by headline plug-in in anything-config.el.
(defun anything-c-moccur-define-occur-command (initial)
  (lexical-let ((initial initial))
    (lambda () (interactive) (anything 'anything-c-source-occur-by-moccur initial))))


;;; moccur buffers
(defvar anything-c-source-moccur-buffer-list
  '((name . "Moccur To Buffers")
    (candidates . (lambda ()
                    (anything-c-moccur-moccur-search anything-pattern nil (buffer-list))
                    (anything-c-moccur-dmoccur-scraper)))
    (action . (("Goto line" . anything-c-moccur-dmoccur-goto-line)))
    (persistent-action . anything-c-moccur-dmoccur-persistent-action)
    (match . (identity))
    (requires-pattern . 5)
    (init . anything-c-moccur-initialize)
    (cleanup . anything-c-moccur-clean-up)
    (delayed)
    (volatile)))

(defun anything-c-moccur-buffer-list ()
  (interactive)
  (anything-c-moccur-with-anything-env (list anything-c-source-moccur-buffer-list)
    (anything)))

;;; dmoccur
(defvar anything-c-moccur-dmoccur-buffers nil)

(defun anything-c-moccur-dmoccur-higligt-info-line ()
  (let ((re anything-c-moccur-info-line-re))
    (loop initially (goto-char (point-min))
          while (re-search-forward re nil t)
          do (put-text-property (line-beginning-position)
                                (line-end-position)
                                'face
                                anything-header-face))))

(defun anything-c-moccur-dmoccur-scraper ()
  (when (buffer-live-p moccur-mocur-buffer)
    (with-current-buffer moccur-mocur-buffer
      (let ((lines nil)
            (re (rx bol (group (+ not-newline)) eol)))

        ;; put face [Buffer:...] line
        (when anything-c-moccur-higligt-info-line-flag
          (anything-c-moccur-dmoccur-higligt-info-line))
        
        (loop initially (progn (goto-char (point-min))
                               (forward-line 1))
              while (re-search-forward re nil t)
              do (push (match-string 0) lines))
        (nreverse lines)))))

(defun anything-c-moccur-dmoccur-get-candidates ()
  (anything-c-moccur-moccur-search anything-pattern nil anything-c-moccur-dmoccur-buffers)
  (anything-c-moccur-dmoccur-scraper))

(defun anything-c-moccur-dmoccur-persistent-action (candidate)
  (anything-c-moccur-next-line-if-info-line)

  (let ((real-candidate (anything-get-selection)))
  
    (multiple-value-bind (buffer file-path)
        (anything-c-moccur-get-info)    ;return (values buffer file)
      (when (and (stringp buffer)
                 (bufferp (get-buffer buffer))
                 (stringp file-path)
                 (file-readable-p file-path))
        
        (find-file file-path)
      
        (anything-c-moccur-widen-if-need)

        (let ((line-number (string-to-number real-candidate)))
          (when (and (numberp line-number)
                     (not (= line-number 0)))
            (goto-line line-number)
      
            (recenter anything-c-moccur-recenter-count)
            (when (overlayp anything-c-moccur-current-line-overlay)
              (move-overlay anything-c-moccur-current-line-overlay
                            (line-beginning-position)
                            (line-end-position)
                            (current-buffer))
              (overlay-put anything-c-moccur-current-line-overlay 'face 'highlight))))))))

(defun anything-c-moccur-dmoccur-goto-line (candidate)
  (multiple-value-bind (buffer file-path)
                       (anything-c-moccur-get-info)
    (let ((line-number (string-to-number candidate)))
      (when (and (stringp buffer)
                 (bufferp (get-buffer buffer))
                 (stringp file-path)
                 (file-readable-p file-path))
        (find-file file-path)
        (goto-line line-number)))))

(defvar anything-c-source-dmoccur
  '((name . "DMoccur")
    (candidates . anything-c-moccur-dmoccur-get-candidates)
    (action . (("Goto line" . anything-c-moccur-dmoccur-goto-line)))
    (persistent-action . anything-c-moccur-dmoccur-persistent-action)
    (match . (identity))
    (requires-pattern . 5)
    (init . anything-c-moccur-initialize)
    (cleanup . anything-c-moccur-clean-up)    
    (delayed)
    (volatile)))

(defun anything-c-moccur-dmoccur (dir)
  (interactive (list (dmoccur-read-from-minibuf current-prefix-arg)))
  (let ((buffers (sort
                   (moccur-add-directory-to-search-list dir)
                   moccur-buffer-sort-method)))

  (setq anything-c-moccur-dmoccur-buffers buffers)

  (anything-c-moccur-with-anything-env (list anything-c-source-dmoccur)
    (anything))))

;;; dired-do-moccur
(defvar anything-c-moccur-dired-do-moccur-buffers nil)

(defun anything-c-moccur-dired-get-buffers ()
  (moccur-add-files-to-search-list
   (funcall (cond ((fboundp 'dired-get-marked-files) ; GNU Emacs
                   'dired-get-marked-files)
                  ((fboundp 'dired-mark-get-files) ; XEmacs
                   'dired-mark-get-files))
            t nil) default-directory t 'dired))

(defun anything-c-moccur-dired-do-moccur-by-moccur-get-candidates ()
  (anything-c-moccur-moccur-search anything-pattern nil anything-c-moccur-dired-do-moccur-buffers)
  (anything-c-moccur-dmoccur-scraper))

(defvar anything-c-source-dired-do-moccur
  '((name . "Dired do Moccur")
    (candidates . anything-c-moccur-dired-do-moccur-by-moccur-get-candidates)
    (action . (("Goto line" . anything-c-moccur-dmoccur-goto-line)))
    (persistent-action . anything-c-moccur-dmoccur-persistent-action)
    (match . (identity))
    (requires-pattern . 3)
    (init . anything-c-moccur-initialize)
    (cleanup . anything-c-moccur-clean-up)    
    (delayed)
    (volatile)))

(defun anything-c-moccur-dired-do-moccur-by-moccur ()
  (interactive)
  (let ((buffers (anything-c-moccur-dired-get-buffers)))
    (setq anything-c-moccur-dired-do-moccur-buffers buffers)

    (anything-c-moccur-with-anything-env (list anything-c-source-dired-do-moccur)
      (anything))))

(defun anything-c-moccur-isearch-get-regexp ()
  (if isearch-regexp
      isearch-string
    (regexp-quote isearch-string)))

;;; Commands

(defun anything-c-moccur-last-sources-is-moccur-p ()
  (and (equal anything-c-moccur-last-buffer (current-buffer))
       (every (lambda (source)
                (let ((source (if (listp source) source (symbol-value source))))
                  (string-match "moccur" (assoc-default 'name source))))
              anything-last-sources)))

(defun anything-c-moccur-resume ()
  (interactive)
  (let (current-prefix-arg)
    (anything-resume anything-c-moccur-buffer)))

(defun anything-c-moccur-isearch-forward ()
  (interactive)
  (let ((anything-c-moccur-widen-when-goto-line-flag nil))
    (save-window-excursion
      (save-restriction
        (narrow-to-region (point-at-bol) (point-max))
        (anything-c-moccur-occur-by-moccur)))))

(defun anything-c-moccur-isearch-backward ()
  (interactive)
  (let* ((anything-c-moccur-widen-when-goto-line-flag nil)
         (copied-source (copy-alist anything-c-source-occur-by-moccur)) ;anything-c-source-occur-by-moccur is list. not symbol
         (anything-c-source-occur-by-moccur (cons '(candidate-transformer . (lambda (-candidates)
                                                                              (reverse -candidates)))
                                                  copied-source)))
    (save-window-excursion
      (save-restriction
        (narrow-to-region (point-min) (point-at-eol))
        (anything-c-moccur-occur-by-moccur)))))

(defun anything-c-moccur-from-isearch ()
  "Run `anything-c-moccur-occur-by-moccur' with isearch string."
  (interactive)
  (isearch-exit)
  (anything-c-moccur-occur-by-moccur-base (anything-c-moccur-isearch-get-regexp)))
;; (define-key isearch-mode-map "\M-o" 'anything-c-moccur-from-isearch)

;;; Commands for `anything-c-moccur-anything-map'
(defun anything-c-moccur-next-line ()
  (interactive)
  (anything-next-line)
  (anything-c-moccur-next-line-if-info-line)
  (anything-c-moccur-anything-try-execute-persistent-action))

(defun anything-c-moccur-previous-line ()
  (interactive)
  (anything-previous-line)
  (anything-c-moccur-previous-line-if-info-line)
  (anything-c-moccur-anything-try-execute-persistent-action))


(defun anything-c-moccur-wrap-word-internal (s1 s2)
  (ignore-errors
    (let ((cur-syntax-table
           (with-current-buffer anything-current-buffer
             (syntax-table))))
      (when (syntax-table-p cur-syntax-table)
        (with-syntax-table cur-syntax-table
          (save-excursion
            (backward-sexp)
            (insert s1))
          (insert s2))))))

(defun anything-c-moccur-start-symbol ()
  (interactive)
  (anything-c-moccur-wrap-word-internal "\\_<" ""))

(defun anything-c-moccur-end-symbol ()
  (interactive)
  (anything-c-moccur-wrap-word-internal "" "\\_>"))

(defun anything-c-moccur-wrap-symbol ()
  (interactive)
  (anything-c-moccur-wrap-word-internal "\\_<" "\\_>"))

(defun anything-c-moccur-start-word ()
  (interactive)
  (anything-c-moccur-wrap-word-internal "\\<" ""))

(defun anything-c-moccur-end-word ()
  (interactive)
  (anything-c-moccur-wrap-word-internal "" "\\>"))

(defun anything-c-moccur-wrap-word ()
  (interactive)
  (anything-c-moccur-wrap-word-internal "\\<" "\\>"))



;; minibuf: hoge
;; => minibuf: ! hoge
(defun anything-c-moccur-delete-special-word ()
  (let ((re (rx (or "!" ";" "\"")
                (* space))))
    (ignore-errors
      (save-excursion
        (beginning-of-line)
        (when (looking-at re)
          (replace-match ""))))))

(defun anything-c-moccur-match-only-internal (str)
  (anything-c-moccur-delete-special-word)
  (save-excursion
    (beginning-of-line)
    (insert-before-markers str)))

(defun anything-c-moccur-match-only-function ()
  (interactive)
  (anything-c-moccur-match-only-internal "! "))

(defun anything-c-moccur-match-only-comment ()
  (interactive)
  (anything-c-moccur-match-only-internal "; "))

(defun anything-c-moccur-match-only-string ()
  (interactive)
  (anything-c-moccur-match-only-internal "\" "))


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "initialize test")
      (expect t
        (let (v)
          (anything-test-candidates
           '(((name . "TEST")
            (candidates "foo")
            (init . (lambda ()
                      (anything-c-moccur-initialize)
                      (setq v anything-c-moccur-anything-invoking-flag)))
            (cleanup . anything-c-moccur-clean-up))))
            v))
      (desc "cleanup test")
      (expect nil
        (let ((anything-c-moccur-anything-invoking-flag t))
          (anything-test-candidates
           '(anything-c-source-occur-by-moccur))
          anything-c-moccur-anything-invoking-flag))
      (desc "anything-c-source-occur-by-moccur")
      (expect '(("Occur by Moccur" ("    2 bbb")))
        (let ((buf (get-buffer-create "*test anything-c-moccur*")))
        (with-current-buffer buf
          (insert "aaa\nbbb\nccc")
          (prin1
           (anything-test-candidates
            '(anything-c-source-occur-by-moccur) "bbb")
           (kill-buffer buf)))))
      (desc "anything-c-moccur-bad-regexp-p")
      (expect t
        (when (anything-c-moccur-bad-regexp-p "\\_>") t))
      (expect t
        (when (anything-c-moccur-bad-regexp-p "\\_> ") t))
      (expect t
        (when (anything-c-moccur-bad-regexp-p " \\_>") t))
      (expect t
        (when (anything-c-moccur-bad-regexp-p " \\_> ") t))
      (expect t
        (when (anything-c-moccur-bad-regexp-p "g \\_> ") t))
      (expect t
        (when (anything-c-moccur-bad-regexp-p "g \\_>") t))
      (expect t
        (when (anything-c-moccur-bad-regexp-p " \\_> g") t))
      (expect nil
        (when (anything-c-moccur-bad-regexp-p "g\\_> ") t))
      (expect nil
        (when (anything-c-moccur-bad-regexp-p " g\\_>") t))
        )))


(provide 'anything-c-moccur)

;;; anything-c-moccur.el ends here
