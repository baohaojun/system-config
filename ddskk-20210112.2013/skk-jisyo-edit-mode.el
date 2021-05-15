;;; skk-jisyo-edit-mode.el --- major mode for editing SKK dictionaries -*- coding: iso-2022-jp -*-

;; Copyright (C) 2001-2010 SKK Development Team

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

;; This is a major mode for editing SKK dictionaries.

;;; Code:

(require 'skk)
(require 'skk-cus)

(eval-when-compile
  (defvar font-lock-defaults))

(defvar skk-jisyo-edit-map nil
  "Keymap for SKK JISYO Edit mode.")

(defvar skk-jisyo-edit-mode-hook nil
  "Hook run on entry in `skk-jisyo-edit-mode'.")

(defvar skk-jisyo-edit-syntax-table nil)

(unless skk-jisyo-edit-map
  (setq skk-jisyo-edit-map (make-sparse-keymap 'skk-jisyo-edit-map)))

(defvar skk-jisyo-edit-font-lock-keywords
  '(("\\(\\[[^]]*/\\]\\)" 1 font-lock-constant-face)
    ("^\\([^; ]+ \\)/" 1 font-lock-function-name-face)
    ("/[^;\n]+\\(;[^/\n]*\\)" 1 font-lock-type-face t)
    ("^\\(;.+\\)$" 1 font-lock-comment-face t)
    ("^\\(;; okuri-ari entries\\.\\)$" 1 font-lock-keyword-face t)
    ("^\\(;; okuri-nasi entries\\.\\)$" 1 font-lock-keyword-face t)
    ("/\\([^/\n]+\\)$" 1 highlight)
    ("\\(/\\)" 1 font-lock-warning-face))
  "Additional expressions to highlight in SKK JISYO edit mode.")

(put 'skk-jisyo-edit-mode
     'font-lock-defaults
     '(skk-jisyo-edit-font-lock-keywords))

(defvar skk-jisyo-edit-original-window-configuration nil)

;;;###autoload
(defun skk-jisyo-edit-mode ()
  "Major mode for editing SKK JISYO."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "SKK JISYO Edit")
  (setq major-mode #'skk-jisyo-edit-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(skk-jisyo-edit-font-lock-keywords))
  (make-local-variable 'skk-jisyo-edit-syntax-table)
  (setq skk-jisyo-edit-syntax-table (make-syntax-table))
  (set-syntax-table skk-jisyo-edit-syntax-table)
  (let ((map (make-sparse-keymap)))
    (use-local-map (nconc map skk-jisyo-edit-map)))
  (modify-syntax-entry ?\" "w" skk-jisyo-edit-syntax-table)
  (modify-syntax-entry ?/ "w" skk-jisyo-edit-syntax-table)
  (run-hooks 'skk-jisyo-edit-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("SKK-JISYO" . skk-jisyo-edit-mode) t)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.skk-jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$"
                                . skk-jisyo-edit-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\..*skk/jisyo\\(\\.BAK\\|\\.bak\\|~\\)?$"
                                . skk-jisyo-edit-mode))

;;;###autoload
(defun skk-edit-private-jisyo (&optional coding-system)
  "個人辞書ファイル `skk-jisyo' を編集する。
任意での個人辞書保存のあと、`skk-jisyo' を開き、`skk-jisyo-edit-mode' に入る。
ローカルに 以下のキー定義が追加される。

key       binding
---       -------
C-c C-c   Save & Exit
C-c C-k   Abort

SKK 使用中の場合は SKK による個人辞書バッファの更新が禁止される。

オプショナル引数 CODING-SYSTEM にて個人辞書のコード系を指定可能。

この機能は従来の手動での個人辞書編集より配慮されているが、SKK 辞書の構文を
チェックすることはできず、自己責任での編集であることは変わりない。"
  (interactive "P")
  (let (answer)
    (unless skk-jisyo-edit-user-accepts-editing
      (setq answer (skk-yes-or-no-p "\
個人辞書の編集は辞書を壊す可能性があります。自己責任での実行に同意しますか？ "
                                    "\
You must edit your private dictionary at your own risk.  Do you accept it? "))
      (when answer
        (skk-cus-set '((skk-jisyo-edit-user-accepts-editing . t))))))
  (when skk-jisyo-edit-user-accepts-editing
    (when coding-system
      (setq coding-system (read-coding-system
                           "個人辞書のコーディングシステムを指定: "
                           (skk-find-coding-system (skk-jisyo t)))))
    (unless coding-system
      (setq coding-system (skk-find-coding-system (skk-jisyo t))))
    ;;
    (when (skk-y-or-n-p "個人辞書を保存しますか？ "
                        "Save private jisyo? ")
      (skk-save-jisyo))
    (skk-edit-private-jisyo-1 coding-system)))

(defun skk-edit-private-jisyo-1 (coding-system)
  (setq skk-jisyo-edit-original-window-configuration
        (current-window-configuration))
  ;; SKK 辞書の文字コードは誤判定がありうるため、注意する
  (let ((coding-system-for-read coding-system))
    (find-file (skk-jisyo)))
  (unless (eq major-mode 'skk-jisyo-edit-mode)
    (skk-jisyo-edit-mode))
  ;; 編集中に再度実行しても、
  ;; ↓ のようになるから skk-update-jisyo-function は復元される。
  ;; '((lambda nil
  ;;     (setq skk-update-jisyo-function #'ignore))
  ;;   (lambda nil
  ;;     (setq skk-update-jisyo-function #'skk-update-jisyo-original))
  ;;   t)
  (add-hook 'kill-buffer-hook
            `(lambda ()
               (setq skk-update-jisyo-function
                     #',skk-update-jisyo-function)
               (ad-disable-advice 'skk-henkan-in-minibuff 'before 'notify-no-effect)
               (ad-disable-advice 'skk-purge-from-jisyo 'around 'notify-no-effect)
               (ad-activate 'skk-henkan-in-minibuff)
               (ad-activate 'skk-purge-from-jisyo))
            nil t)
  (setq skk-update-jisyo-function #'ignore)
  (ad-enable-advice 'skk-henkan-in-minibuff 'before 'notify-no-effect)
  (ad-enable-advice 'skk-purge-from-jisyo 'around 'notify-no-effect)
  (ad-activate 'skk-henkan-in-minibuff)
  (ad-activate 'skk-purge-from-jisyo)
  (local-set-key "\C-c\C-c"
                 (lambda ()
                   (interactive)
                   (when (skk-y-or-n-p "編集を終了しますか？ "
                                       "Finish editing jisyo? ")
                     (save-buffer)
                     (kill-buffer (current-buffer))
                     (skk-reread-private-jisyo t)
                     (set-window-configuration
                      skk-jisyo-edit-original-window-configuration))
                   (message nil)))
  (local-set-key "\C-c\C-k"
                 (lambda ()
                   (interactive)
                   (when (skk-y-or-n-p "編集を中止しますか？ "
                                       "Abort editing jisyo? ")
                     (set-buffer-modified-p nil)
                     (kill-buffer (current-buffer))
                     (set-window-configuration
                      skk-jisyo-edit-original-window-configuration))
                   (message nil)))
  (skk-message "保存終了: C-c C-c, 編集中止: C-c C-k"
               "Save & Exit: C-c C-c, Abort: C-c C-k"))

(defadvice skk-henkan-in-minibuff (before notify-no-effect disable)
  (ding)
  (skk-message "個人辞書の編集中です。登録は反映されません。"
               "You are editing private jisyo.  This registration has no effect.")
  (sit-for 1.5))

(defadvice skk-purge-from-jisyo (around notify-no-effect disable)
  (if (eq skk-henkan-mode 'active)
      (progn
        (ding)
        (skk-message "個人辞書の編集中です。削除できません。"
                     "You are editing private jisyo.  Can't purge."))
    ad-do-it))

(provide 'skk-jisyo-edit)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-jisyo-edit-mode.el ends here
