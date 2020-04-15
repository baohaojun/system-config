;;; skk-sticky.el --- Improved sticky-shift on SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 2008  IRIE Tetsuya

;; Author: IRIE Tetsuya <irie@t.email.ne.jp>
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

;; [概要]
;; SKK では通常、変換開始および送り開始の位置を大文字で指定しますが、こ
;; れを任意のキーで指定できるようにし、sticky-shift ライクな操作を可能
;; にします。

;; ~/.skk.el に

;;   (setq skk-sticky-key ";")

;; と書いておくと ";" キーで変換位置が指定できるようになります。例えば
;; 「有る」が

;;   "; a ; r u"

;; でも入力でき、シフトキーを押す必要がなくなります。操作上は

;;   (info "(skk) Q.3-4 左手の小指を SHIFT で酷使したくありません。")

;; などにある通常の sticky-shift と変わりませんが、画面表示は

;;   打鍵  通常のsticky  skk-sticky
;;    ;      変化なし      ▽
;;    a      ▽あ          ▽あ
;;    ;      ▽あ          ▽あ*
;;    r      ▽あ*r        ▽あ*r

;; と遷移します。skk-sticky は ";" を押した時点で表示が変化するので若干
;; 分かり易くなるかと思います。

;; [設定方法]
;; キーの設定方法は割当てるキーの種類によって異なります。

;; 1. 表示可能なキー

;;   ";" などの表示が可能なキーの場合は

;;     (setq skk-sticky-key ";")

;;   のように string を設定して下さい。`skk-sticky-key' に設定した文字
;;   そのものを入力したい場合は2回続けて打つと入力できます。

;; 2. 表示されないキー

;;   "無変換" のような表示を伴わないキーの場合は

;;     (setq skk-sticky-key [muhenkan]) ; Windows 環境だと [noconvert]

;;   のようにそのキーを表わす vector を設定して下さい。

;; 3. 同時打鍵

;;   2つのキーを同時に打鍵することでも変換位置を指定できます。例えば
;;   "f" と "j" の同時打鍵で指定する場合は

;;     (setq skk-sticky-key '(?f ?j))

;;   のように character のリストを設定して下さい。
;;   Dvorak 配列のような、押しやすい場所に適当なキーがない環境でもこの
;;   機能を使いたい場合に便利かもしれません。

;; [注意事項]
;; o これはあくまでも「任意のキーで変換位置を指定する」ものであり、
;;   sticky-shift そのものではありません。したがって、latin モードや
;;   abbrev モード、また SKK 以外でも sticky-shift を使いたい場合は前述
;;   の info のような設定を併用する必要があります。

;; o skk-hint.el を併用する場合、`skk-hint-start-char' のデフォルトが
;;   ";" なので設定によっては割当てキーが重複する可能性があります。

;;; TODO

;; * SPC に割当てたいという人も居たりする?

;;; Code:

(require 'skk)

(defvar skk-sticky-key-orig-output nil)
(skk-deflocalvar skk-sticky-okuri-flag nil)

;;; 初期設定
(cond ((stringp skk-sticky-key)
       (let ((orig-rule (assoc skk-sticky-key
                               (append skk-rom-kana-rule-list
                                       skk-rom-kana-base-rule-list))))
         (setq skk-sticky-key-orig-output (car (cddr orig-rule)))
         (setq skk-rom-kana-rule-list
               (cons `(,skk-sticky-key nil skk-sticky-set-henkan-point)
                     (delete orig-rule skk-rom-kana-rule-list)))))
      ((vectorp skk-sticky-key)
       (define-key skk-j-mode-map skk-sticky-key 'skk-sticky-set-henkan-point)))

;;; Functions
(defun skk-sticky-looking-back-okuri-mark ()
  "`point' の直前の文字が `*' ならば t を返す。"
  (and (not (bobp))
       (string= "*" (buffer-substring-no-properties
                     (1- (point)) (point)))))

(defun skk-sticky-set-okuri-mark ()
  "送り開始点を `*' を挿入することで標識し、送りあり変換の待ち状態に入る。"
  (when (eq skk-henkan-mode 'on)
    (unless (and skk-sticky-okuri-flag
                 (skk-sticky-looking-back-okuri-mark))
      (when (and skk-dcomp-activate
                 (skk-dcomp-marked-p))
        (skk-dcomp-before-kakutei))
      (insert-and-inherit "*")
      (setq skk-sticky-okuri-flag t)
      nil)))

(defun skk-sticky-set-henkan-point (&optional arg)
  "変換開始位置もしくは送り開始位置を指定する。"
  (interactive "*P")
  (cond ((not (eq skk-henkan-mode 'on))
         (skk-set-henkan-point-subr))
        ((eq (point) (marker-position skk-henkan-start-point))
         (let ((data skk-sticky-key-orig-output))
           (when data
             (skk-kakutei)
             (when (functionp data)
               (setq data (funcall data (skk-make-raw-arg arg))))
             (when (stringp (if (consp data)
                                (car data)
                              data))
               (if (consp data)
                   (if skk-katakana
                       (car data)
                     (cdr data))
                 data)))))
        (t
         (skk-sticky-set-okuri-mark))))

(defadvice skk-kakutei (after skk-sticky-ad activate)
  "`skk-sticky-okuri-flag' をクリアする。"
  (setq skk-sticky-okuri-flag nil))

(defadvice keyboard-quit (after skk-sticky-ad activate)
  "`skk-sticky-okuri-flag' をクリアする。"
  (setq skk-sticky-okuri-flag nil))

(defadvice skk-insert (before skk-sticky-ad activate)
  "`*' の直後であれば入力を大文字に変換する。"
  (when (and skk-sticky-okuri-flag
             (skk-sticky-looking-back-okuri-mark)
             (string= "" skk-prefix))
    (let ((pair (rassq last-command-event skk-downcase-alist)))
      (set 'last-command-event (if pair
                                   (car pair)
                                 (upcase last-command-event))))))

(defadvice skk-set-henkan-point (before skk-sticky-ad activate)
  "`point' 直前の `*' を消す。"
  (when (and skk-sticky-okuri-flag
             (skk-sticky-looking-back-okuri-mark))
    (delete-backward-char 1)))

;; `skk-kana-input' は通常 `cancel-undo-boundary' を呼ぶが、
;; `skk-sticky-key' の2度打ちの際に本来あるべき boundary (C-k の後など)
;; も消してしまうため、advice にて呼ばないように対処する。
;; これにより、`skk-undo-kakutei-word-only' が non-nil でも2度打ちの時
;; に boundary が入ってしまう副作用があるが、先の問題よりはマシだと考え
;; る。
;;; (defadvice skk-kana-input (around skk-sticky-ad activate)
;;;   "▽直後の `skk-sticky-key' の入力の際 `cancel-undo-boundary' を呼ばないように。"
;;;   (if (and (stringp skk-sticky-key)
;;;        (eq (skk-last-command-char) (string-to-char skk-sticky-key))
;;;        (eq skk-henkan-mode 'on)
;;;        (eq (point) (marker-position skk-henkan-start-point)))
;;;       (progn
;;;     (let ((skk-self-insert-non-undo-count 20))
;;;       ad-do-it)
;;;     (setq skk-self-insert-non-undo-count (1+ skk-self-insert-non-undo-count)))
;;;     ad-do-it))

;;; 同時打鍵関連
(defun skk-sticky-double-p (first next)
  "FIRST と NEXT が同時打鍵であれば non-nil を返す。"
  (let ((char (if (characterp first)
                  first
                last-command-event)))
    (and (not (eq char next))
         (memq char skk-sticky-key)
         (memq next skk-sticky-key))))

(defadvice skk-insert (around skk-sticky-ad-double activate)
  "同時打鍵を検出して処理する。"
  (cond ((not (consp skk-sticky-key))
         ad-do-it)
        ((not (memq last-command-event skk-sticky-key))
         ad-do-it)
        ((sit-for skk-sticky-double-interval t)
         ;; No input in the interval.
         ad-do-it)
        (t
         ;; Some key's pressed.
         (let ((next-event (read-event)))
           (if (skk-sticky-double-p this-command
                                    (aref (skk-event-key next-event) 0))
               (skk-sticky-set-henkan-point)
             ad-do-it
             (skk-unread-event next-event))))))

(provide 'skk-sticky)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-sticky.el ends here
