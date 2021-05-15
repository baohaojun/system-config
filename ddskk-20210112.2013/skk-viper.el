;;; skk-viper.el --- SKK related code for Viper -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;;   Murata Shuuichirou <mrt@notwork.org>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@notwork.org>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'skk)

(eval-when-compile
  (if (boundp 'viper-mode)
      (when (eq viper-mode 'ask)
        (setq viper-mode nil))
    (defvar viper-mode nil)))
(require 'viper)

(eval-when-compile
  (defvar viper-insert-state-cursor-color))

;;; macros and inline functions.
(defmacro skk-viper-advice-select (viper vip arg body)
  `(if skk-viper-use-vip-prefix
       (defadvice ,vip ,arg ,@body)
     (defadvice ,viper ,arg ,@body)))

(setq skk-kana-cleanup-command-list
      (cons
       (if skk-viper-use-vip-prefix
           'vip-delete-backward-char
         'viper-del-backward-char-in-insert)
       skk-kana-cleanup-command-list))

(setq skk-use-viper t)
(save-match-data
  (unless (string-match (sentence-end) "。？！．")
    (setq sentence-end (concat "[。？！．]\\|" sentence-end))))

;;; cursor color support.
;; what should we do if older Viper that doesn't have
;; `viper-insert-state-cursor-color'?
(when (boundp 'viper-insert-state-cursor-color)
  (defadvice skk-cursor-current-color (around skk-viper-cursor-ad activate)
    "vi-state 以外で且つ SKK モードのときのみ SKK 由来のカーソル色を返す。"
    (cond
     ((not skk-use-color-cursor)
      ad-do-it)
     ((or (and (boundp 'viper-current-state)
               (eq viper-current-state 'vi-state))
          (and (boundp 'vip-current-mode)
               (eq vip-current-mode 'vi-mode)))
      (setq ad-return-value skk-cursor-default-color))
     ((not skk-mode)
      (setq viper-insert-state-cursor-color
            skk-viper-saved-cursor-color)
      (setq ad-return-value
            (cond
             ((eq viper-current-state 'insert-state)
              viper-insert-state-cursor-color)
             ((eq viper-current-state 'replace-state)
              viper-replace-overlay-cursor-color)
             ((eq viper-current-state 'emacs-state)
              viper-emacs-state-cursor-color))))
     (t
      ad-do-it
      (setq viper-insert-state-cursor-color ad-return-value))))

  (let ((funcs
         ;; cover to VIP/Viper functions.
         (if skk-viper-use-vip-prefix
             '(vip-Append
               vip-Insert
               vip-insert
               vip-escape-to-emacs
               vip-open-line)
           '(viper-change-state-to-insert
             viper-change-state-to-replace
             viper-change-state-to-vi
             viper-change-state-to-emacs
             viper-insert-state-post-command-sentinel))))
    (dolist (func funcs)
      (eval
       `(defadvice ,(intern (symbol-name func))
            (after skk-viper-cursor-ad activate)
          "Set cursor color which represents skk mode."
          (when skk-use-color-cursor
            (skk-cursor-set))))))

  (let ((funcs '(skk-abbrev-mode
                 skk-jisx0208-latin-mode
                 skk-latin-mode
                 skk-toggle-characters)))
    (dolist (func funcs)
      (eval
       `(defadvice ,(intern (symbol-name func))
            (after skk-viper-cursor-ad activate)
          "\
`viper-insert-state-cursor-color' を SKK の入力モードのカーソル色と合わせる。"
          (when skk-use-color-cursor
            (setq viper-insert-state-cursor-color
                  (skk-cursor-current-color)))))))

  (defadvice skk-mode (after skk-viper-cursor-ad activate)
    "Set cursor color which represents skk mode."
    (when skk-use-color-cursor
      (skk-cursor-set)))

  (defadvice skk-kakutei (after skk-viper-cursor-ad activate)
    (setq viper-insert-state-cursor-color skk-cursor-hiragana-color)))

(when (boundp 'viper-insert-state-cursor-color)
  (skk-defadvice read-from-minibuffer (before skk-viper-ad activate)
    "`minibuffer-setup-hook' に `update-buffer-local-frame-params' をフックする。
`viper-read-string-with-history' は `minibuffer-setup-hook' を関数ローカル
にしてしまうので、予め `minibuffer-setup-hook' にかけておいたフックが無効
となる。"
    (when skk-use-color-cursor
      ;; non-command subr.
      (add-hook 'minibuffer-setup-hook 'ccc-update-buffer-local-frame-params
                'append))))

;;; advices.
;; vip-4 の同種の関数名は vip-read-string-with-history？
(defadvice viper-read-string-with-history (after skk-viper-ad activate)
  "次回ミニバッファに入ったときに SKK モードにならないようにする。"
  (skk-remove-skk-pre-command)
  (skk-remove-minibuffer-setup-hook 'skk-j-mode-on
                                    'skk-setup-minibuffer
                                    'skk-add-skk-pre-command))

(skk-viper-advice-select
 viper-forward-word-kernel vip-forward-word
 (around skk-ad activate)
 ("SKK モードがオンで、ポイントの直後の文字が JISX0208/JISX0213 だったら\
 `forward-word' する。"
  (if (and skk-mode
           (or (skk-jisx0208-p (following-char))
               (skk-jisx0213-p (following-char))))
      (forward-word (ad-get-arg 0))
    ad-do-it)))

(skk-viper-advice-select
 viper-backward-word-kernel vip-backward-word
 (around skk-ad activate)
 ("SKK モードがオンで、ポイントの直前の文字が JISX0208/JISX0213 だったら\
 `backward-word' する。"
  (if (and skk-mode (or (skk-jisx0208-p (preceding-char))
                        (skk-jisx0213-p (preceding-char))))
      (backward-word (ad-get-arg 0))
    ad-do-it)))

;; please sync with `skk-delete-backward-char'
(skk-viper-advice-select
 viper-del-backward-char-in-insert vip-delete-backward-char
 (around skk-ad activate)
 ("▼モードで `skk-delete-implies-kakutei' なら直前の文字を消して確定する。
▼モードで `skk-delete-implies-kakutei' が nil だったら前候補を表示する。
▽モードで`▽'よりも前のポイントで実行すると確定する。
確定入力モードで、かなプレフィックスの入力中ならば、かなプレフィックスを消す。"
  (skk-with-point-move
   (let ((count (or (prefix-numeric-value (ad-get-arg 0)) 1)))
     (cond
      ((eq skk-henkan-mode 'active)
       (if (and (not skk-delete-implies-kakutei)
                (= (+ skk-henkan-end-point (length skk-henkan-okurigana))
                   (point)))
           (skk-previous-candidate)
         ;; overwrite-mode で、ポイントが全角文字に囲まれていると
         ;; きに delete-backward-char を使うと、全角文字は消すが半
         ;; 角文字分しか backward 方向にポイントが戻らない (Emacs
         ;; 19.31 にて確認)。変換中の候補に対しては
         ;; delete-backward-char で必ず全角文字 1 文字分 backward
         ;; 方向に戻った方が良い。
         (if overwrite-mode
             (progn
               (backward-char count)
               (delete-char count))
           ad-do-it)
         ;; XXX assume skk-prefix has no multibyte chars.
         (if (> (length skk-prefix) count)
             (setq skk-prefix (substring skk-prefix
                                         0 (- (length skk-prefix) count)))
           (setq skk-prefix ""))
         (when (>= skk-henkan-end-point (point))
           (if (eq skk-delete-implies-kakutei 'dont-update)
               (let ((skk-update-jisyo-function #'ignore))
                 (skk-kakutei))
             (skk-kakutei)))))
      ((and skk-henkan-mode
            (>= skk-henkan-start-point (point))
            (not (skk-get-prefix skk-current-rule-tree)))
       (setq skk-henkan-count 0)
       (skk-kakutei))
      ;; 入力中の見出し語に対しては delete-backward-char で
      ;; 必ず全角文字 1文字分 backward 方向に戻った方が良い。
      ((and skk-henkan-mode
            overwrite-mode)
       (backward-char count)
       (delete-char count))
      (t
       (skk-delete-okuri-mark)
       (if (skk-get-prefix skk-current-rule-tree)
           (skk-erase-prefix 'clean)
         (skk-set-marker skk-kana-start-point nil)
         ad-do-it)))))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-escape-to-emacs
 (before skk-add activate)
 ("▽モード、▼モードだったら確定する。"
  (when (and skk-mode
             skk-henkan-mode)
    (skk-kakutei))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-escape-to-emacs
 (after skk-kana-cleanup-ad activate)
 ("vi-state 移行の際に確定入力モードで入力されたローマ字プレフィックスを消す。"
  (skk-kana-cleanup t)))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("スペースの両側の文字セットが JISX0208/JISX0213 だったらスペースを取り除く。"
  (save-match-data
    (let ((char-after (char-after (progn
                                    (skip-chars-forward " ")
                                    (point))))
          (char-before (char-before (progn
                                      (skip-chars-backward " ")
                                      (point)))))
      (when (and (or (skk-jisx0208-p char-after)
                     (skk-jisx0213-p char-after))
                 (or (skk-jisx0208-p char-before)
                     (skk-jisx0213-p char-before)))
        (while (looking-at " ")
          (delete-char 1)))))))

;;; functions.
;;;###autoload
(defun skk-viper-normalize-map ()
  (let ((other-buffer (local-variable-if-set-p 'minor-mode-map-alist)))
    ;; for current buffer and buffers to be created in the future.
    ;; substantially the same job as viper-harness-minor-mode does.
    (funcall skk-viper-normalize-map-function)
    (setq-default minor-mode-map-alist minor-mode-map-alist)
    (when other-buffer
      ;; for buffers which are already created and have
      ;; the minor-mode-map-alist localized by Viper.
      (skk-loop-for-buffers (buffer-list)
        (unless (assq 'skk-j-mode minor-mode-map-alist)
          (skk-update-minor-mode-map-alist 'skk-latin-mode skk-latin-mode-map)
          (skk-update-minor-mode-map-alist 'skk-abbrev-mode skk-abbrev-mode-map)
          (skk-update-minor-mode-map-alist 'skk-j-mode skk-j-mode-map)
          (skk-update-minor-mode-map-alist 'skk-jisx0208-mode skk-jisx0208-latin-mode-map))
        (funcall skk-viper-normalize-map-function)))))

(eval-after-load "viper-cmd"
  '(defun viper-toggle-case (arg)
     "Toggle character case.
Convert hirakana to katakana and vice versa."
     (interactive "P")
     (let ((val (viper-p-val arg)) (c))
       (viper-set-destructive-command
        (list 'viper-toggle-case val nil nil nil nil))
       (while (> val 0)
         (setq c (following-char))
         (delete-char 1 nil)
         (cond ((skk-ascii-char-p c)
                (if (eq c (upcase c))
                    (insert-char (downcase c) 1)
                  (insert-char (upcase c) 1)))
               ((and (<= ?ぁ c) (>= ?ん c))
                (insert (skk-hiragana-to-katakana (char-to-string c))))
               ((and (<= ?ァ c) (>= ?ン c))
                (insert (skk-katakana-to-hiragana (char-to-string c))))
               (t
                (insert-char c 1)))
         (when (eolp)
           (backward-char 1))
         (setq val (1- val))))))

(defun skk-viper-init-function ()
  (when (and (boundp 'viper-insert-state-cursor-color)
             (featurep 'skk-cursor))
    (setq viper-insert-state-cursor-color (skk-cursor-current-color)))
  ;; viper-toggle-key-action と連動させる？
  (skk-viper-normalize-map)
  (remove-hook 'skk-mode-hook 'skk-viper-init-function))

(add-hook 'skk-mode-hook 'skk-viper-init-function)

(provide 'skk-viper)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-viper.el ends here
