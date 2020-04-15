;;; skk-dcomp.el --- SKK dynamic completion -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999, 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Copyright (C) 2008, 2009 IRIE Tetsuya <irie@t.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Author: IRIE Tetsuya <irie@t.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
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

;; これは▽モードにおける見出し語の入力を、自動的にダイナミックにコンプ
;; リーションするプログラムです。

;; MS Excel のセル入力の自動補完 (同じ列に既に入力している文字列があっ
;; たときにそれを参照して補完しようとする機能) を見ていて、これ便利だなぁ
;; と思ったのが、開発のきっかけです。

;; その後、増井俊之 さんが開発している POBox を見て、MS Excel を見た際に
;; 思ったことを思い出し、SKK の skk-comp.el で提供されているコンプリーシ
;; ョンの機能を自動的に提供する方向で実装してみたのが skk-dcomp.el のコー
;; ディング始まりです。

;; POBox は沢山候補を出しますが、少し動作が遅いのが難点です。skk-dcomp.el
;; は一つしか候補を出しませんが、ユーザの見出し語の入力に追従しダイナミッ
;; クにコンプリーションする機能は POBox 同様持っていますし、また動作はかな
;; り高速で、skk-dcomp.el を使うことによるオーバーヘッドを体感することはな
;; いと思います。


;; <INSTALL>

;; SKK を普通に make して下さい。特に作業は不要です。

;; <HOW TO USE>

;; ~/.emacs.d/init.el もしくは ~/.skk に (setq skk-dcomp-activate t) と書き
;; ましょう。
;; SKK 起動後にダイナミックコンプリーションの機能を止めたかったら、
;; (setq skk-dcomp-activate nil) を評価しましょう。


;; <HOW TO WORK>

;; ▽モードに入り見出し語を入力すると、個人辞書を自動的に検索し、見出
;; し語を コンプリーションします。下記のように動作します (カッコ内はキー
;; 入力を、-!- はポイント位置を表します)。

;;   (Ho) ▽ほ -> ▽ほ-!-んとう

;;   * SKK のコンプリーションは、元来個人辞書のみを参照する仕様になってい
;;     るので、個人辞書にない見出し語はコンプリーションされません。
;;   * コンプリーションは、送りなし変換の場合しか行われません。
;;   * Ho の入力に対し、「ほんとう」がコンプリーションされるかどうかは個
;;     人辞書のエントリの順番次第 (変換順に降順に並んでいる) ですので、人
;;     それぞれ違うはずです。

;; 自動的にコンプリーションされた見出し語が、自分の意図したものであれば TAB
;; を押すことでポイント位置を動かし、コンプリーションされた見出し語を選択す
;; ることができます。そのまま SPC を押して変換するなり、q を押してカタカナ
;; にするなり SKK 本来の動作を何でも行うことができます。

;;   (Ho) ▽ほ -> ▽ほ-!-んとう (TAB) -> ▽ほんとう-!- (TAB)

;; コンプリーションされた見出し語が自分の意図したものでない場合は、かま
;; わず次の入力をして下さい。コンプリーションされた部分を無視したかのように
;; 動作します。

;;   (Ho) ▽ほ -> ▽ほ-!-んとう (ka) -> ▽ほか-!-ん

;; コンプリーションされない状態が自分の意図したものである場合も、コンプリー
;; ションされた部分を単に無視するだけで OK です。

;;   (Ho) ▽ほ -> ▽ほ-!-んとう (C-j) -> ほ
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (SPC) -> ▼保 (「ほ」を見出し語とした変換が行われる)
;;   (Ho) ▽ほ -> ▽ほ-!-んとう (q) -> ホ

;; コンプリーションされた状態から BS を押すと、消されたコンプリーション前の
;; 見出し語から再度コンプリーションを行います。

;;   (Ho) ▽ほ -> ▽ほ-!-んとう (ka) -> ▽ほか-!-ん (BS) -> ▽ほ-!-んとう

;;; Code:

(require 'skk)

(require 'skk-comp)
(eval-when-compile (require 'cl-lib))

(defsubst skk-dcomp-face-on (start end)
  (skk-face-on skk-dcomp-extent start end skk-dcomp-face
               skk-dcomp-face-priority))

(defsubst skk-dcomp-face-off ()
  (skk-detach-extent skk-dcomp-extent))

(defsubst skk-dcomp-delete-completion ()
  (ignore-errors
    (delete-region skk-dcomp-start-point skk-dcomp-end-point)))

;;;###autoload
(defun skk-dcomp-marked-p ()
  (and (eq skk-henkan-mode 'on)
       (markerp skk-dcomp-start-point)
       (markerp skk-dcomp-end-point)
       (marker-position skk-dcomp-start-point)
       (marker-position skk-dcomp-end-point)
       (< skk-dcomp-start-point skk-dcomp-end-point)))

(defun skk-dcomp-cleanup-buffer ()
  (skk-delete-overlay skk-dcomp-multiple-overlays)
  (when (and (or skk-dcomp-activate
                 skk-dcomp-multiple-activate)
             (skk-dcomp-marked-p))
    (skk-dcomp-face-off)
    (delete-region skk-dcomp-end-point (point))
    (skk-set-marker skk-dcomp-end-point (point))
    (setq skk-dcomp-multiple-select-index -1)))

(defun skk-dcomp-activate-p ()
  (and skk-dcomp-activate
       (cond ((functionp skk-dcomp-activate)
              (save-match-data
                (funcall skk-dcomp-activate)))
             ((listp skk-dcomp-activate)
              (save-match-data
                (eval skk-dcomp-activate)))
             (skk-hint-inhibit-dcomp
              nil)
             (t
              t))))

(defun skk-dcomp-multiple-activate-p ()
  (and skk-dcomp-multiple-activate
       (cond ((functionp skk-dcomp-multiple-activate)
              (save-match-data
                (funcall skk-dcomp-multiple-activate)))
             ((listp skk-dcomp-multiple-activate)
              (save-match-data
                (eval skk-dcomp-multiple-activate)))
             (skk-hint-inhibit-dcomp
              nil)
             (t
              t))))

(defun skk-dcomp-do-completion (pos)
  (when (and (eq skk-henkan-mode 'on)
             (not skk-okurigana))
    (when (and (not (eq (marker-position skk-henkan-start-point) (point)))
               (skk-dcomp-activate-p))
      (condition-case nil
          (progn
            (skk-comp-do 'first 'silent)
            (skk-set-marker skk-dcomp-start-point pos)
            (skk-set-marker skk-dcomp-end-point (point))
            (skk-dcomp-face-on skk-dcomp-start-point skk-dcomp-end-point)
            (goto-char skk-dcomp-start-point))
        (error
         (setq skk-comp-stack nil)
         (message nil))))
    (when (and (skk-dcomp-multiple-activate-p)
               (skk-dcomp-multiple-available-p))
      (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates
                                (and (string= skk-dcomp-multiple-key
                                              (buffer-substring-no-properties
                                               skk-henkan-start-point (point)))
                                     (string= skk-dcomp-multiple-prefix skk-prefix)))))))

;; 複数表示のために検索して辞書バッファの point を動かすと、skk-comp の
;; 補完候補が狂ってしまうので一旦保存しておき最後に元に戻す
(defmacro skk-dcomp-save-point-in-jisyo-buffer (form)
  `(let (alist)
     (dolist (buf skk-dcomp-multiple-keep-point-buffer-list)
       (when (get-buffer buf)
         (with-current-buffer buf
           (setq alist (cons (cons buf (point)) alist)))))
     ,form
     (dolist (pair alist)
       (with-current-buffer (car pair)
         (goto-char (cdr pair))))))

(defun skk-dcomp-multiple-available-p ()
  (< (1+ skk-dcomp-multiple-rows)
     (window-body-height)))

(defun skk-dcomp-multiple-increase-index (index &optional ignore-search-done)
  (cond ((and skk-comp-circulate
              (or ignore-search-done
                  skk-dcomp-multiple-search-done
                  skk-comp-search-done)
              (<= (1- (length skk-dcomp-multiple-candidates))
                  index))
         -1)
        ((and skk-dcomp-multiple-search-done
              (<= (1- (length skk-dcomp-multiple-candidates))
                  index))
         index)
        (t (1+ index))))

(defun skk-dcomp-multiple-extract-candidates (candidates index)
  (let ((i 0)
        head extract)
    (setq head (cond ((< index skk-dcomp-multiple-rows)
                      candidates)
                     (t
                      (nthcdr (* (/ index skk-dcomp-multiple-rows)
                                 skk-dcomp-multiple-rows)
                              candidates))))
    (while (and (< i skk-dcomp-multiple-rows) head)
      (push (pop head) extract)
      (cl-incf i))
    (nreverse extract)))

(defun skk-dcomp-multiple-get-candidates (&optional same-key)
  (let (candidates)
    (cond
     ;; (1) 新規検索
     ((not same-key)
      (setq skk-dcomp-multiple-select-index
            ;; skk-comp の C-u TAB を考慮する
            (if (and current-prefix-arg (listp current-prefix-arg)) 0 -1))
      (setq skk-dcomp-multiple-key
            ;; skk-comp の C-u TAB を考慮する
            (if (and current-prefix-arg (listp current-prefix-arg))
                skk-comp-key
              (let ((key (buffer-substring-no-properties
                          skk-henkan-start-point (point))))
                (if skk-katakana
                    (skk-katakana-to-hiragana key)
                  key))))
      (setq skk-dcomp-multiple-prefix skk-prefix)
      (setq skk-dcomp-multiple-search-done nil)
      (let ( ;; `skk-comp-get-candidate' に必要なデータを束縛
            (skk-comp-key skk-dcomp-multiple-key)
            (skk-comp-prefix skk-dcomp-multiple-prefix)
            ;; `skk-comp-get-candidate' で値が変わってしまうため束縛
            (skk-current-completion-prog-list
             skk-current-completion-prog-list)
            (skk-server-completion-words skk-server-completion-words)
            (skk-look-completion-words skk-look-completion-words)
            (i 0)
            cand)
        (when (or skk-comp-use-prefix
                  ;; skk-comp-use-prefix が nil の場合、▽n などは
                  ;; 補完候補を検索しない
                  (not (skk-get-kana skk-current-rule-tree)))
          (skk-dcomp-save-point-in-jisyo-buffer
           (while (and (< i skk-dcomp-multiple-rows)
                       (setq cand (skk-comp-get-candidate (zerop i))))
             (unless (member cand candidates)
               (push cand candidates)
               (cl-incf i)))))
        (setq candidates (nreverse candidates))
        (when (< i skk-dcomp-multiple-rows)
          (setq skk-dcomp-multiple-search-done t))
        (setq skk-dcomp-multiple-candidates candidates)))

     ;; (2) 全て検索済
     (skk-dcomp-multiple-search-done
      (setq candidates (skk-dcomp-multiple-extract-candidates
                        skk-dcomp-multiple-candidates
                        skk-dcomp-multiple-select-index)))

     ;; (3) 全検索する (TAB 連打で繰り越したとき)
     ((and same-key
           (< (1- (length skk-dcomp-multiple-candidates))
              skk-dcomp-multiple-select-index))
      (skk-dcomp-save-point-in-jisyo-buffer
       (let ( ;; `skk-comp-get-all-candidates' で空になってしまうため束縛
             (skk-comp-kakutei-midasi-list skk-comp-kakutei-midasi-list)
             (skk-server-completion-words skk-server-completion-words)
             (skk-look-completion-words skk-look-completion-words))
         (setq skk-dcomp-multiple-candidates
               (skk-comp-get-all-candidates skk-dcomp-multiple-key
                                            skk-dcomp-multiple-prefix
                                            skk-completion-prog-list))))
      (setq skk-dcomp-multiple-search-done t)
      (setq skk-dcomp-multiple-select-index
            (min skk-dcomp-multiple-select-index
                 (1- (length skk-dcomp-multiple-candidates))))
      (setq candidates (skk-dcomp-multiple-extract-candidates
                        skk-dcomp-multiple-candidates
                        skk-dcomp-multiple-select-index)))

     ;; (4) 単なる TAB 打鍵
     (t
      (setq candidates skk-dcomp-multiple-candidates)))
    (when candidates
      (append candidates
              (list (format " [ %s / %s ]"
                            (if (< skk-dcomp-multiple-select-index 0)
                                "-"
                              (1+ skk-dcomp-multiple-select-index))
                            (if skk-dcomp-multiple-search-done
                                (length skk-dcomp-multiple-candidates)
                              "??")))))))

(defun skk-dcomp-multiple-show (candidates)
  (skk-delete-overlay skk-dcomp-multiple-overlays)
  (unless (skk-in-minibuffer-p)
    (let* ((margin 1)
           (beg-col (save-excursion
                      (goto-char skk-henkan-start-point)
                      (max 0 (- (skk-screen-column) margin))))
           (i 0)
           max-width bottom col ol invisible)
      (when candidates
        (when (zerop beg-col)
          (setq margin 0))
        (setq max-width (skk-max-string-width candidates))
        (dolist (str candidates)
          (setq str (concat (make-string margin ? )
                            str
                            (make-string (+ (- max-width (string-width str))
                                            margin)
                                         ? )))
          (cond
           ((and (< -1 skk-dcomp-multiple-select-index)
                 (= i (mod skk-dcomp-multiple-select-index
                           skk-dcomp-multiple-rows)))
            (setq str (propertize
                       str 'face 'skk-dcomp-multiple-selected-face)))
           (t
            (setq str (propertize str 'face 'skk-dcomp-multiple-face))
            (unless (string= "" skk-dcomp-multiple-key)
              (let ((beg (string-match skk-dcomp-multiple-key str))
                    (end (progn (string-match "[^ ]+" str) (match-end 0))))
                (when beg
                  (put-text-property (+ beg (length skk-dcomp-multiple-key))
                                     end
                                     'face 'skk-dcomp-multiple-trailing-face
                                     str))))))
          (save-excursion
            (scroll-left (max 0
                              (- (+ beg-col margin max-width margin 1)
                                 (window-width) (window-hscroll))))
            (unless (zerop (window-hscroll))
              (setq beg-col
                    (save-excursion (goto-char skk-henkan-start-point)
                                    (- (current-column) margin))))
            (setq bottom (> (1+ i) (vertical-motion (1+ i))))
            (cond (bottom
                   ;; バッファ最終行では普通に overlay を追加していく方
                   ;; 法だと overlay の表示される順番が狂うことがあって
                   ;; うまくない。したがって前回の overlay の
                   ;; after-string に追加する。ただし、EOB の場合は
                   ;; prefix の overlay と衝突するため
                   ;; `skk-prefix-overlay' に追加する
                   (setq ol (if (zerop i)
                                (cond ((or (not skk-echo)
                                           (string= "" skk-prefix)
                                           (< (overlay-end skk-prefix-overlay)
                                              (point)))
                                       (make-overlay (point) (point)))
                                      (t skk-prefix-overlay))
                              (pop skk-dcomp-multiple-overlays)))
                   (setq str (concat (overlay-get ol 'after-string)
                                     "\n" (make-string beg-col ? ) str)))
                  (t
                   (setq col (skk-move-to-screen-column beg-col))
                   (cond ((> beg-col col)
                          ;; 桁合わせの空白を追加
                          (setq str (concat (make-string (- beg-col col) ? )
                                            str)))
                         ;; overlay の左端がマルチ幅文字と重なったときの微調整
                         ((< beg-col col)
                          (backward-char)
                          (setq col (skk-screen-column))
                          (setq str (concat (make-string (- beg-col col) ? )
                                            str))))))
            ;; この時点で overlay の開始位置に point がある
            (unless bottom
              (let ((ol-beg (point))
                    (ol-end-col (+ col (string-width str)))
                    base-ol)
                (setq col (skk-move-to-screen-column ol-end-col))
                ;; overlay の右端がマルチ幅文字と重なったときの微調整
                (when (< ol-end-col col)
                  (setq str (concat str
                                    (make-string (- col ol-end-col) ? ))))
                (setq ol (make-overlay ol-beg (point)))
                ;; 元テキストの face を継承しないように1つ後ろに
                ;; overlay を作って、その face を 'default に指定しておく
                (setq base-ol (make-overlay (point) (1+ (point))))
                (overlay-put base-ol 'face 'default)
                (push base-ol skk-dcomp-multiple-overlays)
                ;; 候補が可視かどうかチェック
                (unless (pos-visible-in-window-p (point))
                  (setq invisible t)))))
          (overlay-put ol 'invisible t)
          (overlay-put ol 'after-string str)
          (push ol skk-dcomp-multiple-overlays)
          (cl-incf i))
        (when (or invisible
                  (and bottom
                       (> (+ 2 skk-dcomp-multiple-rows)
                          (- (window-body-height)
                             (count-screen-lines (window-start) (point))))))
          (recenter (- (+ 2 skk-dcomp-multiple-rows))))))))

;;;###autoload
(defun skk-dcomp-before-kakutei ()
  (skk-delete-overlay skk-dcomp-multiple-overlays)
  (when (and (or skk-dcomp-activate
                 skk-dcomp-multiple-activate)
             (eq skk-henkan-mode 'on)
             (skk-dcomp-marked-p))
    (skk-dcomp-face-off)
    (skk-dcomp-delete-completion)))

(defun skk-dcomp-after-kakutei ()
  (when (or skk-dcomp-activate
            skk-dcomp-multiple-activate)
    (skk-set-marker skk-dcomp-start-point nil)
    (skk-set-marker skk-dcomp-end-point nil)
    (setq skk-comp-stack nil)
    (setq skk-dcomp-multiple-select-index -1)))

;;;###autoload
(defun skk-dcomp-after-delete-backward-char ()
  (when (and (or skk-dcomp-activate
                 skk-dcomp-multiple-activate)
             skk-mode
             (eq skk-henkan-mode 'on)
             (not skk-hint-inhibit-dcomp))
    (skk-delete-overlay skk-dcomp-multiple-overlays)
    (setq skk-dcomp-multiple-select-index -1)
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    (when (and skk-abbrev-mode
               skk-use-look)
      (setq skk-look-completion-words nil))
    (skk-dcomp-do-completion (point)))
  ;; dcomp との順番制御のため、ここで呼ぶ
  (skk-henkan-on-message))

;;; advices.
;; main dynamic completion engine.
(defadvice skk-kana-input (around skk-dcomp-ad activate)
  (cond
   ((or skk-hint-inhibit-dcomp
        (not (and (or skk-dcomp-activate
                      skk-dcomp-multiple-activate)
                  skk-henkan-mode)))
    ad-do-it)
   (t
    (cond
     ((or (eq skk-henkan-mode 'active) ; ▼モード
          (skk-get-prefix skk-current-rule-tree)
          (not skk-comp-stack))
      (skk-set-marker skk-dcomp-start-point nil)
      (skk-set-marker skk-dcomp-end-point nil))

     ;; experimental/skk-pre-henkan.el
     ((and (featurep 'skk-pre-henkan)
           (eq last-command 'skk-comp-do))
      (skk-kakutei))

     ((skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (unless (member (this-command-keys)
                      skk-dcomp-keep-completion-keys)
        (skk-dcomp-delete-completion))))
    ad-do-it
    (when (and skk-j-mode
               (or skk-use-kana-keyboard
                   ;; 送りあり変換が始まったら補完しない
                   (not (memq last-command-event skk-set-henkan-point-key))))
      (if (skk-get-prefix skk-current-rule-tree)
          (when (and (skk-dcomp-multiple-activate-p)
                     (skk-dcomp-multiple-available-p))
            (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates)))
        (skk-dcomp-do-completion (point)))))))

(defadvice skk-set-henkan-point-subr (around skk-dcomp-ad activate)
  (cond
   ((or skk-dcomp-activate
        skk-dcomp-multiple-activate)
    (let ((henkan-mode skk-henkan-mode))
      ad-do-it
      (unless (or henkan-mode
                  (char-after (point)))
        (skk-dcomp-do-completion (point)))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-insert (around skk-dcomp-ad activate)
  (cond
   ((or skk-dcomp-activate
        skk-dcomp-multiple-activate)
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-event '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-comma (around skk-dcomp-ad activate)
  (cond
   ((and (or skk-dcomp-activate
             skk-dcomp-multiple-activate)
         (not (eq last-command 'skk-comp-do)))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-event '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-abbrev-period (around skk-dcomp-ad activate)
  (cond
   ((and (or skk-dcomp-activate
             skk-dcomp-multiple-activate)
         (not (eq last-command 'skk-comp-do)))
    (when (skk-dcomp-marked-p)
      (skk-dcomp-face-off)
      (skk-dcomp-delete-completion))
    ad-do-it
    (when skk-use-look
      (setq skk-look-completion-words nil))
    (unless (memq last-command-event '(?*))
      (skk-dcomp-do-completion (point))))
   (t
    ad-do-it)))

(defadvice skk-comp-previous (after skk-dcomp-ad activate)
  (when (and (skk-dcomp-multiple-activate-p)
             (skk-dcomp-multiple-available-p)
             (or skk-comp-circulate
                 (< 0 skk-dcomp-multiple-select-index)))
    (skk-kana-cleanup 'force)
    (setq skk-dcomp-multiple-select-index
          (cond ((and skk-comp-circulate
                      (< skk-dcomp-multiple-select-index 0))
                 (1- (length skk-comp-stack)))
                (t (1- skk-dcomp-multiple-select-index))))
    (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates t))))

(defadvice skk-kakutei (around skk-dcomp-ad activate)
  (skk-dcomp-before-kakutei)
  ad-do-it
  (skk-dcomp-after-kakutei))

(defadvice keyboard-quit (around skk-dcomp-ad activate)
  (skk-dcomp-before-kakutei)
  ad-do-it
  (skk-dcomp-after-delete-backward-char))

;; (defadvice skk-henkan (before skk-dcomp-ad activate)
(defadvice skk-start-henkan (before skk-dcomp-ad activate)
  (skk-dcomp-cleanup-buffer))

(defadvice skk-process-prefix-or-suffix (before skk-dcomp-ad activate)
  (when skk-henkan-mode
    (skk-dcomp-cleanup-buffer)))

(defadvice skk-comp (around skk-dcomp-ad activate)
  (cond ((and (or skk-dcomp-activate
                  skk-dcomp-multiple-activate)
              (skk-dcomp-marked-p))
         (cond ((integerp (ad-get-arg 0))
                (skk-dcomp-cleanup-buffer)
                ad-do-it)
               (t
                (goto-char skk-dcomp-end-point)
                (setq this-command 'skk-comp-do)
                (skk-dcomp-face-off)
                (skk-set-marker skk-dcomp-start-point nil)
                (skk-set-marker skk-dcomp-end-point nil)
                (when (and (skk-dcomp-multiple-activate-p)
                           (skk-dcomp-multiple-available-p)
                           (or skk-comp-circulate
                               (< skk-dcomp-multiple-select-index
                                  (1- (length skk-dcomp-multiple-candidates)))))
                  (setq skk-dcomp-multiple-select-index
                        (skk-dcomp-multiple-increase-index
                         skk-dcomp-multiple-select-index t))
                  (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates t))))))
        (t
         ad-do-it
         (when (and (skk-dcomp-multiple-activate-p)
                    (skk-dcomp-multiple-available-p))
           (setq skk-dcomp-multiple-select-index
                 (skk-dcomp-multiple-increase-index
                  skk-dcomp-multiple-select-index))
           (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates
                                     ;; skk-comp の C-u TAB を考慮する
                                     (not (and current-prefix-arg
                                               (listp current-prefix-arg)))))))))

(defadvice skk-comp-do (before skk-dcomp-ad activate)
  (when (and skk-comp-use-prefix
             (not (string= "" skk-prefix))
             (eq last-command-event skk-next-completion-char))
    (ad-set-arg 0 t)))

(defadvice skk-comp-do (after skk-dcomp-ad activate)
  (when (and (skk-dcomp-multiple-activate-p)
             (skk-dcomp-multiple-available-p)
             ;;(not (ad-get-arg 0))
             (eq last-command-event skk-next-completion-char))
    (skk-kana-cleanup 'force)
    (setq skk-dcomp-multiple-select-index
          (skk-dcomp-multiple-increase-index skk-dcomp-multiple-select-index))
    (skk-dcomp-multiple-show (skk-dcomp-multiple-get-candidates t))))

(defadvice skk-comp-start-henkan (around skk-dcomp-ad activate)
  (cond ((and (eq skk-henkan-mode 'on)
              (or skk-dcomp-activate
                  skk-dcomp-multiple-activate)
              (skk-dcomp-marked-p))
         (goto-char skk-dcomp-end-point)
         (setq this-command 'skk-comp-do)
         (skk-dcomp-face-off)
         (skk-set-marker skk-dcomp-start-point nil)
         (skk-set-marker skk-dcomp-end-point nil)
         (skk-start-henkan (ad-get-arg 0)))
        (t
         ad-do-it)))

(defadvice skk-delete-backward-char (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice skk-undo (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice viper-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice vip-del-backward-char-in-insert (after skk-dcomp-ad activate)
  (skk-dcomp-after-delete-backward-char))

(defadvice skk-previous-candidate (around skk-dcomp-ad activate)
  (let ((active (eq skk-henkan-mode 'active)))
    ad-do-it
    (when active
      (skk-dcomp-after-delete-backward-char))))

(provide 'skk-dcomp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-dcomp.el ends here
