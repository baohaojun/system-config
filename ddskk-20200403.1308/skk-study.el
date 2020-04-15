;;; skk-study.el --- SKK 学習効果提供プログラム -*- coding: iso-2022-jp -*-
;; Copyright (C) 1999, 2000, 2002, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese
;; Created: Apr. 11, 1999

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

;; ある語 A' を確定した場合に、A' 及びその見出し語 A に対して、直前に
;; 変換した語 B' とその見出し語 B を関連語として登録しておき、再度 A
;; の変換を行ったときに、B 及び B' のペアが直前の何回かに確定した語の
;; 中に見つかれば、A' を優先して出力する単純な学習効果を提供するプログ
;; ラムです。

;; 昔 SKK ML で話題になった単語の属性の保存のために、skk-attr.el を作
;; りましたが、機能を欲張りすぎてものになりませんでした。直前の変換と
;; の関連性を保存するためだけに機能を絞って再構成したのがこのプログラ
;; ムです。

;; <How to install>

;; ~/.skk に

;;   (require 'skk-study)

;; と書いて下さい。

;; <DATA STRUCTURE (SKK-STUDY-ALIST)>

;; ((okuri-ari .  ((A . (((B . B') . (A' ...))
;;                                       ...))))
;;  (okuri-nasi . ((A . (((B . B') . (A' ...))
;;                                       ...)))))

;;  o examples

;; ((okuri-ari .
;;           (("きr" . ((("ふく" . "服") . ("着"))
;;                      (("き" . "木") . ("切"))
;;                      (("えん" . "縁") . ("切"))))
;;            ("なk" . ((("こども" . "子供") . ("泣"))
;;                      (("ことり" . "小鳥") . ("鳴"))))
;;            ("かk" . ((("かみ" . "紙") . ("書")) (("ひんかく" . "品格") . ("欠")))))
;;           ...)
;;  (okuri-nasi .
;;            (("かみ" . ((("きr" . "切") . ("紙"))))
;;             ...)))
;; <TODO>


;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (defvar jka-compr-compression-info-list)
  (defvar print-quoted))

(require 'skk)
(require 'ring)

(defconst skk-study-file-format-version "0.3")
(skk-deflocalvar skk-study-current-buffer-theme nil)

;;;; inline functions.
(defsubst skk-study-get-last-henkan-data (index)
  (and (> (ring-length skk-study-data-ring) index)
       (ring-ref skk-study-data-ring index)))

(defsubst skk-study-get-current-alist (&optional theme)
  (let ((base-alist (cdr (if theme
                             (assoc theme skk-study-alist)
                           (or (assoc skk-study-current-buffer-theme
                                      skk-study-alist)
                               (assoc "general" skk-study-alist))))))
    (assq (cond ((or skk-okuri-char skk-henkan-okurigana)
                 'okuri-ari)
                (t 'okuri-nasi))
          base-alist)))

(add-to-list 'skk-search-end-function 'skk-study-search)
(add-to-list 'skk-update-end-function 'skk-study-update)

;;;###autoload
(defun skk-study-search (henkan-buffer midasi okurigana entry)
  "学習データを参照して ENTRY を加工し、関連性のある語の優先順位を上げて返す。"
  (or skk-study-data-ring
      (setq skk-study-data-ring (make-ring skk-study-search-times)))
  (when (and entry (cdr entry))
    (or skk-study-alist (skk-study-read))
    (with-current-buffer henkan-buffer
      ;; (("きr" . ((("ふく" . "服") . ("着")) (("き" . "木") . ("切"))))
      ;;  ("なk" . ((("こども" . "子供") . ("泣")))))
      (let ((alist (cdr (assoc midasi (cdr (skk-study-get-current-alist))))))
        (when alist
          (setq entry (skk-study-search-1 alist midasi okurigana entry))))))
  entry)

(defun skk-study-search-1 (target-alist midasi okurigana entry)
  (cl-do ((index 0 (1+ index))
          (times skk-study-search-times (1- times))
          last-data associates e exit)
      ((or exit (zerop times)) entry)
    (and
     (setq last-data (skk-study-get-last-henkan-data index))
     ;; ((("ふく" . "服") . ("着")) (("き" . "木") . ("切")))
     ;; ("着")
     (setq associates (cdr (assoc last-data target-alist)))
     (setq associates (reverse associates))
     (setq exit t)
     (while (setq e (car associates))
       ;;uniq
       (setq entry (cons e (delete e entry))
             associates (cdr associates))))))

;;;###autoload
(defun skk-study-update (henkan-buffer midasi okurigana word purge)
  "MIDASI と WORD について `skk-study-data-ring' の最初の関連語を関連付けて学習する。"
  (or skk-study-data-ring
      (setq skk-study-data-ring (make-ring skk-study-search-times)))
  (let ((inhibit-quit t)
        last-data diff grandpa papa baby)
    (with-current-buffer henkan-buffer
      (when (and
             ;; 第一候補で確定したかどうか
             (or skk-study-first-candidate
                 (not (string= word (car skk-henkan-list))))
             ;; 変換バッファが変わっていないかどうか
             (eq (skk-get-last-henkan-datum 'henkan-buffer) henkan-buffer)
             (or (not skk-study-max-distance)
                 (and (setq diff
                            (- (point)
                               (skk-get-last-henkan-datum 'henkan-point)))
                      ;; 直前の変換よりポイントが前へ移動していないかどうか
                      (> diff 0)
                      ;; skk-study-max-distance を超えて直前の変換とポイン
                      ;; トが離れていないかどうか。
                      (> skk-study-max-distance diff)))
             midasi word
             (setq last-data (if (not (ring-empty-p skk-study-data-ring))
                                 (ring-ref skk-study-data-ring 0)))
             (not (or (string= midasi "") (string= word "")
                      (and (string= midasi (car last-data))
                           (string= word (cdr last-data))))))
        (or skk-study-alist (skk-study-read))
        (setq grandpa (skk-study-get-current-alist)
              ;; ((("ふく" . "服") . ("着")) (("き" . "木") . ("切")))
              papa (assoc midasi (cdr grandpa)))
        (cond (
               ;; car に見出し語を持つ cell がない
               (not (or papa purge))
               (setcdr grandpa
                       (nconc
                        (list (cons midasi (list (cons last-data (list word)))))
                        (cdr grandpa))))
              ;; 見出し語から始まる cell はあるが、cdr に (last-key . last-word) を
              ;; キーにした cell がない。
              ((not (or
                     ;; (("ふく" . "服") . ("着"))
                     (setq baby (assoc last-data (cdr papa)))
                     purge))
               (setcdr papa (cons (cons last-data (list word)) (cdr papa))))
              ;; 見出し語をキーとした既存の cell 構造ができあがっているので、関連語だけ
              ;; アップデートする。
              ((not purge)
               ;; ring データの方がもっと効率的か？  でもここの部分のデータのアップデート
               ;; が効率良くできない。
               (setcdr baby (cons word (delete word (cdr baby))))
               (if (> (1- (length (cdr baby))) skk-study-associates-number)
                   (skk-study-chomp (cdr baby) (1- skk-study-associates-number))))
              (t (setcdr grandpa (delq baby (cdr grandpa)))))))))

;;;###autoload
(defun skk-study-save (&optional nomsg)
  "学習結果を `skk-study-file' へ保存する。
オプショナル引数の NOMSG が non-nil であれば、保存メッセージを表示しない。"
  (interactive "P")
  (if (or (and (null skk-study-alist) (not nomsg))
          (not skk-study-last-read)
          (and skk-study-last-save
               (skk-study-time-lessp
                skk-study-last-save skk-study-last-read)))
      (progn
        (skk-message "SKK の学習結果をセーブする必要はありません"
                     "No SKK study need saving")
        (sit-for 1))
    (skk-study-save-1 nomsg)))

(defun skk-study-save-1 (nomsg)
  (let ((inhibit-quit t)
        e)
    (when (not nomsg)
      (skk-message "SKK の学習結果を %s にセーブしています..."
                   "Saving SKK study to %s..." skk-study-file))
    (and skk-study-backup-file
         (file-exists-p (expand-file-name skk-study-file))
         (cond ((eq system-type 'ms-dos)
                (with-temp-file skk-study-backup-file
                  (erase-buffer)
                  (insert-file-contents skk-study-file)))
               (t
                (copy-file (expand-file-name skk-study-file)
                           (expand-file-name skk-study-backup-file)
                           'ok-if-already-exists 'keep-date))))
    (with-temp-buffer
      (insert (format ";;; skk-study-file format version %s\n"
                      skk-study-file-format-version))
      (when skk-study-sort-saving
        ;; sort is not necessary, but make an alist rather readable.
        (setq e (assq 'okuri-ari skk-study-alist))
        (setcdr e (sort (cdr e)
                        (lambda (a b)
                          (skk-string< (car a) (car b)))))
        (setq e (assq 'okuri-nasi skk-study-alist))
        (setcdr e (sort (cdr e)
                        (lambda (a b)
                          (skk-string< (car a) (car b))))))
      (skk-study-prin1 skk-study-alist (current-buffer))
      (let ((coding-system-for-write (skk-find-coding-system (skk-jisyo t)))
            jka-compr-compression-info-list)
        (write-region (point-min) (point-max) skk-study-file)))
    (setq skk-study-last-save (current-time))
    (when (not nomsg)
      (skk-message "SKK の学習結果を %s にセーブしています...完了！"
                   "Saving SKK study to %s...done" skk-study-file)
      (sit-for 1)
      (message ""))))

;;;###autoload
(defun skk-study-switch-current-theme (theme)
  "カレントバッファに対して skk-study の学習テーマ THEME を設定する。
学習テーマ名 THEME には任意の文字列を指定できる。
カレントバッファに学習テーマが設定されないときは、学習テーマ
\"general\" に対して学習が行われる。"
  (interactive
   (list (completing-read
          "Theme of current buffer: (default: general) "
          (when (or skk-study-alist (skk-study-read))
            (let ((n 0))
              (mapcar (lambda (e)
                        (setq n (1+ n))
                        (cons e n))
                      (mapcar 'car skk-study-alist)))))))
  (setq skk-study-current-buffer-theme theme)
  (let ((alist (assoc theme skk-study-alist)))
    (unless alist
      (setq skk-study-alist
            (cons
             (cons theme '((okuri-ari) (okuri-nasi)))
             skk-study-alist)))))

;;;###autoload
(defun skk-study-remove-theme (theme)
  "skk-study の学習テーマ THEME を削除する。"
  (interactive
   (list (completing-read
          "Remove skk-study theme: "
          (when (or skk-study-alist (skk-study-read))
            (let ((n 0))
              (mapcar (lambda (e)
                        (setq n (1+ n))
                        (cons e n))
                      (mapcar 'car skk-study-alist))))
          nil 'require-match)))
  (if (string= theme "general")
      (skk-message "学習テーマ `general' は削除できません"
                   "Cannot remove skk-study theme `general'")
    (setq skk-study-alist (delq (assoc theme skk-study-alist)
                                skk-study-alist))
    (when (and skk-study-current-buffer-theme
               (string= skk-study-current-buffer-theme theme))
      (setq skk-study-current-buffer-theme nil))))

;;;###autoload
(defun skk-study-copy-theme (from to)
  "skk-study の学習テーマ FROM を TO にコピーする。
TO の既存データは破壊される。"
  (interactive
   (list (completing-read "Copy skk-study theme from: "
                          (when (or skk-study-alist (skk-study-read))
                            (let ((n 0))
                              (mapcar (lambda (e)
                                        (setq n (1+ n))
                                        (cons e n))
                                      (mapcar 'car skk-study-alist))))
                          nil 'require-match)
         (completing-read "Copy skk-study theme to: "
                          (let ((n 0))
                            (mapcar (lambda (e)
                                      (setq n (1+ n))
                                      (cons e n))
                                    (mapcar 'car skk-study-alist))))))
  (when (string= from to)
    (skk-error "コピー元とコピー先のテーマが同一です"
               "FROM and TO is the same theme"))
  (let ((fromalist (copy-tree (cdr (assoc from skk-study-alist))))
        (toalist (assoc to skk-study-alist)))
    (unless fromalist
      (skk-error "コピー元の学習データがありません"
                 "FROM study data is null"))
    (if toalist
        (setcdr toalist fromalist)
      (setq skk-study-alist (cons (cons to fromalist) skk-study-alist)))))

;;;###autoload
(defun skk-study-read (&optional nomsg force)
  "`skk-study-file' から学習結果を読み込む。
オプショナル引数の FORCE が non-nil であれば、破棄の確認をしない。"
  (interactive "P")
  (skk-create-file skk-study-file
                   (if (not nomsg)
                       (if skk-japanese-message-and-error
                           "SKK の学習結果ファイルを作りました"
                         "I have created an SKK study file for you")))
  (when (or (null skk-study-alist)
            force
            (skk-yes-or-no-p
             (format "%s を再読み込みしますか？ " skk-study-file)
             (format "Reread %s? " skk-study-file)))
    (unless nomsg
      (skk-message "%s の SKK 学習結果を展開しています..."
                   "Expanding SKK study of %s ..."
                   (file-name-nondirectory skk-study-file)))
    (when skk-study-check-alist-format
      (skk-study-check-alist-format skk-study-file))
    (setq skk-study-alist (skk-study-read-1 skk-study-file))
    (setq skk-study-last-read (current-time))
    (when (and skk-study-alist (not nomsg))
      (skk-message "%s の SKK 学習結果を展開しています...完了！"
                   "Expanding SKK study of %s ...done"
                   (file-name-nondirectory skk-study-file))
      (sit-for 1)
      (message ""))))

(defun skk-study-read-1 (file)
  ;; read FILE and return alist.
  (with-temp-buffer
    (let ((version-string
           (format ";;; skk-study-file format version %s\n"
                   skk-study-file-format-version))
          version)
      (let ((coding-system-for-read (skk-find-coding-system (skk-jisyo t)))
            format-alist)
        (insert-file-contents file))
      (when (zerop (buffer-size))
        ;; bare alist
        (insert version-string
                "((\"general\" . ((okuri-ari) (okuri-nasi))))"))
      (goto-char (point-min))
      (when (looking-at "^;;; skk-study-file format version \\([.0-9]+\\)\n")
        (setq version (match-string 1)))
      (cond ((not version)
             (skk-error "skk-study-file が壊れています"
                        "Broken skk-study-file"))
            ((string= version skk-study-file-format-version)
             (read (current-buffer)))
            (t
             ;; convert the format to new one
             (list (cons "general" (read (current-buffer)))))))))

(defun skk-study-check-alist-format (file)
  "skk-study の学習データファイル FILE のフォーマットをチェックする。"
  (interactive
   (list (read-file-name
          (format "File to check: (default: %s) " skk-study-file)
          default-directory skk-study-file)))
  (skk-message "%s のフォーマットをチェックしています..."
               "Checking format of %s..." file)
  (or (skk-study-check-alist-format-1 (skk-study-read-1 file))
      (skk-error "%s のフォーマットは壊れています"
                 "%s format is broken" file))
  (skk-message "%s のフォーマットをチェックしています...完了!"
               "Checking format of %s...done" file)
  (sit-for 1)
  (message ""))

(defun skk-study-check-alist-format-1 (alist)
  (let (a)
    (dolist (elm alist)
      (when (and (= (length elm) 3)
                 (stringp (car elm))
                 (setq a (cdr elm))
                 (assq 'okuri-ari a)
                 (assq 'okuri-nasi a))
        (catch 'exit
          (let ((index '(okuri-ari okuri-nasi))
                (func (lambda (str)
                        (let ((len (length str)))
                          (and
                           (> len 1)
                           (skk-ascii-char-p (aref str (1- len)))))))
                a2 e f)
            (while index
              (and (eq (car index) 'okuri-nasi)
                   (setq func
                         (lambda (str)
                           (let ((len (length str)))
                             (cond ((= len 1))
                                   ((not (skk-ascii-char-p (aref str (1- len)))))
                                   ((skk-ascii-char-p (aref str (- len 2)))))))))
              (setq a2 (cdr (assq (car index) a)))
              (while a2
                (setq e (car a2))
                (or (funcall func (car e))
                    ;; 見出し語のチェック
                    (throw 'exit nil))
                (setq f (cdr e))
                (while f
                  (if (not (and
                            ;; 直前の変換の情報
                            (consp (caar f))
                            ;; 関連語リスト
                            (listp (cdar f))))
                      (throw 'exit nil))
                  (setq f (cdr f)))
                (setq a2 (cdr a2)))
              (setq index (cdr index)))
            t))))))

(defun skk-study-prin1 (form &optional stream)
  (let ((print-readably t)
        print-level print-length print-quoted)
    (prin1 form stream)))

(defun skk-study-chomp (nth list)
  ;; LIST := '(A B C D), NTH := 1
  ;; -> '(A B)
  (and (> nth -1) (setcdr (nthcdr nth list) nil))
  list)

(defadvice skk-kakutei-initialize (before skk-study-ad activate)
  (let ((kakutei-word (ad-get-arg 0)))
    (when kakutei-word
      (ring-insert
       skk-study-data-ring (cons skk-henkan-key kakutei-word)))))

(defadvice skk-undo-kakutei (after skk-study-ad activate)
  (let ((last (ring-ref skk-study-data-ring 0))
        (last2 (ring-ref skk-study-data-ring 1))
        target)
    (when (and last last2)
      (setq target (assoc (car last)
                          ;; skk-undo-kakutei is called in henkan buffer
                          (skk-study-get-current-alist))
            target (delq (assoc last2 (cdr target)) target)))))

;; time utilities...
;;  from ls-lisp.el.  Welcome!
(defun skk-study-time-lessp (time0 time1)
  (let ((hi0 (car time0))
        (hi1 (car time1))
        (lo0 (nth 1 time0))
        (lo1 (nth 1 time1)))
    (or (< hi0 hi1) (and (= hi0 hi1) (< lo0 lo1)))))

(add-hook 'kill-emacs-hook 'skk-study-save)

(provide 'skk-study)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-study.el ends here
