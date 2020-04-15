;;; skk-comp.el --- 補完のためのプログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;;               1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
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

;; ▽さ (TAB) -> ▽さとう (.) -> ▽さいとう (,) -> ▽さとう(.) -> ▽さいとう

;;; Code:

(require 'skk)

(eval-when-compile
  (defvar smart-find-file-path)
  (defvar dont-bind-my-keys)
  (condition-case nil
      (let ((dont-bind-my-keys t))
        (load "smart-find"))
    (error
     (defalias 'smart-find-file-all 'ignore))))

;;;###autoload
(defun skk-comp-start-henkan (arg)
  "▽モードで読みを補完した後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。"
  (interactive "*P")
  (cond
   ((eq skk-henkan-mode 'on)
    (skk-comp-do (not (eq last-command 'skk-comp-do)))
    (skk-start-henkan arg))
   (t
    (skk-emulate-original-map arg))))

;;;###autoload
(defun skk-comp (first &optional silent)
  (setq this-command 'skk-comp-do)
  (skk-comp-do first silent))

;;;###autoload
(defun skk-comp-do (first &optional silent set-this-command)
  ;; main completion engine.
  (when set-this-command
    (setq this-command 'skk-comp-do))
  (let ((inhibit-quit t)
        ;; skk-num が require されてないと
        ;; buffer-local 値を壊す恐れあり。
        skk-num-list
        tmp-key data
        orig-key
        c-word)
    (when first
      (setq skk-comp-search-done nil
            skk-comp-stack nil
            skk-comp-depth 0
            skk-comp-prefix skk-prefix)
      ;;  key  \ use-prefix    nil    kakutei-first   non-nil    # data
      ;; "かk"           "か"  , ""    "か"  , "k"    "か", "k"  #    t
      ;; "かn"           "かん", ""    "かん", ""     "か", "n"  # non-t
      (setq tmp-key (buffer-substring-no-properties
                     skk-henkan-start-point (point)))
      ;; skk-kana-cleanup() を呼ぶ前の key を取得
      (unless (or skk-abbrev-mode
                  (memq skk-comp-use-prefix '(nil kakutei-first)))
        (save-match-data
          (if (string-match "^\\([^a-z]*\\)[a-z]*$" tmp-key)
              (setq skk-comp-key (match-string 1 tmp-key))
            ;; 送り無しで見出しにアルファベットを含むような変則的な候補は、
            ;; skk-echo も考えるとまともな対処が面倒なので、
            ;; 害が無い範囲で適当に処理。 nil か kakutei-first を使ってもらう。
            (setq skk-comp-key tmp-key))))
      ;; prefix に対応する「かな」etc. があれば non-t
      ;; 副作用を伴なうルールが指定されているかもしれないので、
      ;; データがあるかどうかのチェックのみに使う。
      (setq data (skk-kana-cleanup 'force))
      (when (or skk-abbrev-mode
                (memq skk-comp-use-prefix '(nil kakutei-first)))
        (setq skk-comp-key (buffer-substring-no-properties
                            skk-henkan-start-point (point)))
        (unless (and skk-comp-use-prefix
                     (eq data t))
          (setq skk-comp-prefix ""))))
    (setq orig-key (if skk-katakana
                       (skk-hiragana-to-katakana skk-comp-key)
                     skk-comp-key))
    (when skk-katakana
      (setq skk-comp-key (skk-katakana-to-hiragana skk-comp-key)))
    (cond
     ;; (全候補探索済み)
     (skk-comp-search-done
      (if (zerop skk-comp-depth)
          ;; circulate ならば c-word = skk-comp-key なので c-word = nil
          ;; non-circulate ならば これ以上候補がないので c-word = nil
          (if skk-comp-circulate
              (setq skk-comp-depth (length skk-comp-stack)))
        (setq skk-comp-depth (1- skk-comp-depth))
        (setq c-word (nth skk-comp-depth skk-comp-stack))))
     ;; (未探索候補が残っている可能性有り)
     (t
      (cond
       ;; 最後に得られた候補を表示している
       ((zerop skk-comp-depth)
        (setq c-word
              (let ((word (skk-comp-get-candidate first)))
                (while (member word skk-comp-stack)
                  (setq word (skk-comp-get-candidate)))
                word))
        (if c-word
            ;; 新規に見つけたときだけ push する。
            (push c-word skk-comp-stack)
          (setq skk-comp-search-done t)
          (if skk-comp-circulate
              (setq skk-comp-depth (length skk-comp-stack)))))
       (t
        ;; "," などで前候補に戻っている
        (setq skk-comp-depth (1- skk-comp-depth)
              c-word (nth skk-comp-depth skk-comp-stack))))))
    (cond
     (c-word
      (delete-region skk-henkan-start-point (point))
      (insert c-word))
     (t
      ;; When skk-comp-circulate, return to the keyword.
      (when skk-comp-circulate
        (delete-region skk-henkan-start-point (point))
        (insert orig-key))
      (unless silent
        (ding)
        (cond
         ((and (string= skk-comp-key "")
               (assq 'skk-comp-by-history skk-completion-prog-list)
               (or (not skk-comp-use-prefix)
                   (string= skk-comp-prefix "")))
          (skk-message "これ以上の履歴はありません"
                       "No more words in history"))
         (t
          (if skk-japanese-message-and-error
              (message "\"%s\" で補完すべき見出し語は%sありません"
                       (if skk-comp-use-prefix
                           (concat orig-key skk-comp-prefix)
                         orig-key)
                       (if first "" "他に"))
            (message "No %scompletions for \"%s\""
                     (if first "" "more ")
                     (if skk-comp-use-prefix
                         (concat orig-key skk-comp-prefix)
                       orig-key))))))))))

;;;###autoload
(defun skk-comp-get-candidate (&optional first)
  (when first
    (setq skk-comp-first t
          skk-current-completion-prog-list
          (cond
           ((and (integerp first)
                 (<= 0 first)
                 (<= first 9))
            (let ((list (symbol-value
                         (intern
                          (format "skk-completion-prog-list-%d" first)))))
              (or list skk-completion-prog-list)))
           (t
            skk-completion-prog-list))))
  (let (cand prog)
    (while (and (null cand)
                skk-current-completion-prog-list)
      (setq prog (car skk-current-completion-prog-list))
      (setq cand (eval prog)        ; `skk-comp-key' をキーとして、文字列ひとつが戻る
            skk-comp-first nil)
      (unless cand
        (setq skk-current-completion-prog-list
              (cdr skk-current-completion-prog-list))
        (setq skk-comp-first t)))
    (if (and skk-katakana cand)
        (skk-hiragana-to-katakana cand)
      cand)))

;; for test or backend use
;;;###autoload
(defun skk-comp-get-all-candidates (key prefix prog-list)
  (let ((skk-comp-key key)
        (skk-comp-prefix prefix)
        (skk-completion-prog-list prog-list)
        skk-current-completion-prog-list
        skk-comp-first
        cand ret)
    (setq cand (skk-comp-get-candidate 'first))
    (when cand
      (setq ret (list cand))
      (while (setq cand (skk-comp-get-candidate))
        (unless (member cand ret)
          (setq ret (cons cand ret)))))
    (nreverse (if skk-katakana
                  (mapcar 'skk-hiragana-to-katakana ret)
                ret))))

;;;###autoload
(defun skk-comp-get-regexp (prefix)
  ;; プレフィックスに対応する正規表現を返す。
  ;; 一度生成した正規表現は skk-comp-prefix-regexp-alist に保存しておく。
  (or (cdr (assoc prefix skk-comp-prefix-regexp-alist))
      (let ((regexp
             (if (string= prefix "")
                 ""
               (let ((tree skk-rule-tree)
                     kana-list)
                 (dolist (c (string-to-list prefix))
                   (setq tree (skk-select-branch tree c)))
                 (setq kana-list
                       (skk-comp-arrange-kana-list
                        (skk-comp-collect-kana tree)
                        prefix))
                 (condition-case nil
                     (regexp-opt kana-list 'with-paren)
                   (error
                    (if kana-list
                        (concat "\\("
                                (mapconcat #'regexp-quote kana-list "\\|")
                                "\\)")
                      "")))))))
        (add-to-list 'skk-comp-prefix-regexp-alist (cons prefix regexp))
        regexp)))

;;;###autoload
(defun skk-comp-collect-kana (tree)
  ;; skk-rule-tree の部分木に属する "かな" を集める
  (let ((data (skk-get-kana tree))
        (branches (skk-get-branch-list tree))
        kana kana-list)
    (when data
      (setq kana (if (consp data)
                     (cdr data)
                   data))
      (when (stringp kana)
        (setq kana-list (list kana))))
    (nconc kana-list (apply #'nconc
                            (mapcar #'skk-comp-collect-kana
                                    branches)))))

;;;###autoload
(defun skk-comp-arrange-kana-list (kana-list prefix)
  ;; skk-comp-collect-kana から得た "かな" のリストを元に
  ;; プレフィックスに対応した調整をする
  (let (short-list long-list tmp)
    (dolist (kana kana-list)
      (if (= (length kana) 1)
          (add-to-list 'short-list kana)
        (add-to-list 'long-list kana)))
    ;; "に" がある時に "にゃ" とかはいらない
    (dolist (s-kana short-list)
      (dolist (l-kana long-list)
        (when (string= s-kana
                       (substring l-kana 0 1))
          (setq long-list (delete l-kana long-list)))))
    (setq tmp (nconc short-list long-list))
    (if skk-comp-kana-list-filter-function
        (funcall skk-comp-kana-list-filter-function tmp prefix)
      tmp)))

;;;###autoload
(defun skk-comp-from-jisyo (file)
  ;; skk-comp-prefix を使える
  "SKK 辞書フォーマットの FILE から補完候補を得る。"
  (let ((buffer (skk-get-jisyo-buffer file 'nomsg))
        (abbrev skk-abbrev-mode)
        (key skk-comp-key)
        (prefix skk-comp-prefix)
        (first skk-comp-first)
        (use-prefix skk-comp-use-prefix))
    (with-current-buffer buffer
      (when first
        (goto-char skk-okuri-nasi-min))
      (if use-prefix
          (unless (and (string= key "")
                       (string= prefix ""))
            (skk-comp-re-search-current-buffer key prefix abbrev))
        (unless (string= key "")
          (skk-comp-search-current-buffer key abbrev))))))

;;;###autoload
(defun skk-comp-search-current-buffer (key &optional abbrev)
  (let (c-word)
    (save-match-data
      ;; `case-fold-search' は、辞書バッファでは常に nil。
      (while (and (not c-word)
                  (search-forward
                   (concat "\n"
                           (if skk-use-numeric-conversion
                               (skk-num-compute-henkan-key key)
                             key))
                   nil t))
        (unless (eq (following-char)
                    ?\040) ;SPC
          (setq c-word (concat key
                               (buffer-substring-no-properties
                                ;; 見出し語に空白は含まれない。
                                ;; " /" をサーチする必要はない。
                                (point)
                                (1- (search-forward " ")))))
          (when (and abbrev
                     (string-match "\\Ca" c-word))
            ;; abbrev モードで「3ねん」などの補完はしない
            (setq c-word nil))))
      c-word)))

;;;###autoload
(defun skk-comp-re-search-current-buffer (key prefix &optional abbrev)
  ;; 問題のあるケースがあるかもしれないので
  ;; skk-comp-search-current-buffer との一本化はとりあえず保留
  (let (c-word regexp-key)
    (setq regexp-key (concat (regexp-quote
                              (if skk-use-numeric-conversion
                                  (skk-num-compute-henkan-key key)
                                key))
                             (skk-comp-get-regexp prefix)))
    (save-match-data
      ;; `case-fold-search' は、辞書バッファでは常に nil。
      (while (and (not c-word)
                  (re-search-forward (concat "\n" regexp-key) nil t))
        (beginning-of-line)
        (search-forward (if skk-use-numeric-conversion
                            (skk-num-compute-henkan-key key)
                          key))
        (unless (eq (following-char)
                    ?\040)      ;SPC
          (setq c-word (concat key
                               (buffer-substring-no-properties
                                (point)
                                (1- (search-forward " ")))))
          (when (and abbrev
                     (string-match "\\Ca" c-word))
            ;; abbrev モードで「3ねん」などの補完はしない
            (setq c-word nil))))
      c-word)))

;;;###autoload
(defun skk-comp-previous (&optional set-this-command)
  ;; skk-abbrev-comma, skk-insert-comma のサブルーチン。
  ;; 直前に補完を行った見出しを挿入する。
  (when set-this-command
    (setq this-command 'skk-comp-do))
  (let ((inhibit-quit t)
        (stack-length (length skk-comp-stack))
        c-word)
    (if (and skk-comp-circulate (= skk-comp-depth stack-length))
        (setq skk-comp-depth 0)
      (setq skk-comp-depth (1+ skk-comp-depth)))
    (setq c-word (nth skk-comp-depth skk-comp-stack))
    (cond
     (c-word
      (delete-region skk-henkan-start-point (point))
      (insert c-word))
     (t
      (if (null skk-comp-circulate)
          ;; non-circulate ならば skk-comp-depth が範囲外なので 1 戻す
          (setq skk-comp-depth (1- skk-comp-depth))
        (delete-region skk-henkan-start-point (point))
        (insert skk-comp-key))
      ;;(setq skk-comp-depth (1- skk-comp-depth))
      (ding)
      (skk-message "\"%s\"で補完すべき見出し語は他にありません"
                   "No more previous completions for \"%s\""
                   (if skk-comp-use-prefix
                       (concat skk-comp-key skk-comp-prefix)
                     skk-comp-key))))))

;;;###autoload
(defun skk-comp-previous/next (ch)
  (cond
   ((eq ch skk-next-completion-char)
    (skk-comp-do nil nil t))
   ((eq ch skk-previous-completion-char)
    (skk-comp-previous t))))

;;;###autoload
(defun skk-try-completion (arg)
  "▽モードで見出し語を補完する。
それ以外のモードでは、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "P")
  (skk-with-point-move
   (if (eq skk-henkan-mode 'on)
       (skk-comp (or arg
                     (not (eq last-command 'skk-comp-do))))
     (skk-emulate-original-map arg))))

;;;###autoload
(defun skk-comp-wrapper (&optional arg)
  "Character でないキーに補完を割り当てるためのコマンド。"
  (interactive "p")
  (skk-bind-last-command-char skk-try-completion-char
    (call-interactively #'skk-insert)))

;;;###autoload
(defun skk-previous-comp-maybe (&optional arg)
  "Character でないキーに補完前候補を割り当てるためのコマンド。
▽モードでは補完前候補、さもなければオリジナルのキー定義を実行する。"
  (interactive "P")
  (if (eq skk-henkan-mode 'on) ;▽モード
      (skk-comp-previous t)
    (skk-emulate-original-map arg)))

;;;###autoload
(defun skk-comp-by-history ()
  ;; skk-comp-prefix を考慮
  "入力が空の時に履歴から補完する。
対象は現在の Emacs のセッションにおいて行った送り無し変換のうち、
`skk-kakutei-history-limit' で指定される最近のものである。"
  (when (and (string= skk-comp-key "")
             (or (not skk-comp-use-prefix)
                 (string= skk-comp-prefix "")))
    (when skk-comp-first
      (setq skk-comp-kakutei-midasi-list
            (skk-remove-duplicates (mapcar #'car skk-kakutei-history))))
    (pop skk-comp-kakutei-midasi-list)))

;;;###autoload
(defun skk-comp-smart-find (&optional path)
  "`smart-find' が見つけたファイル名で補完する"
  (cond (skk-abbrev-mode
         (when skk-comp-first
           (setq skk-comp-smart-find-files
                 (skk-smart-find skk-comp-key path)))
         (if skk-comp-smart-find-files
             (pop skk-comp-smart-find-files)
           nil))
        (t
         (setq skk-comp-smart-find-files nil))))

;;;###autoload
(defun skk-search-smart-find (&optional path not-abbrev-only
                                        without-char-maybe)
  "`smart-find'を利用した変換を行う。
SKK abbrev モードにて、英文字 + `skk-completion-search-char' (~)で
未完スペルを指定して変換すると、補完候補が変換候補として出現する。
デフォルトでは SKK abbrev モードのみで有効な機能だが、
NOT-ABBREV-ONLY を指定する事で常に有効となる。"
  (when (and (or not-abbrev-only
                 skk-abbrev-mode))
    (skk-completion-search `((skk-comp-smart-find ',path))
                           '((skk-search-identity))
                           'without-midasi
                           without-char-maybe)))

;;;###autoload
(defun skk-smart-find (key &optional path)
  ;; smart-find は provide されていない
  (unless (fboundp 'smart-find-file)
    (let ((dont-bind-my-keys t))
      (load-library "smart-find"))
    (message ""))
  ;;
  (unless path
    (setq path skk-smart-find-file-path))
  (setq skk-comp-smart-find-files nil)
  ;;
  (let ((smart-find-file-path (or path smart-find-file-path))
        results files)
    (unless (string= key "")
      (setq results (if (fboundp 'smart-find-file-all)
                        (smart-find-file-all key)))
      (while results
        (if (string-match skk-smart-find-ignored-file-regexp
                          (car results))
            (setq results (cdr results))
          (setq files (cons (pop results) files)))))
    (when files
      (setq files
            (sort files #'string-lessp)))))

;;;###autoload
(defun skk-comp-lisp-symbol (&optional predicate)
  "Lisp symbol 名で補完する。
PREDICATE に引数 1 個の関数を指定すれば、PREDICATE を満たすシンボル
に限って補完する。PREDICATE には `fboundp', `boundp', `commandp'
などが指定できる。指定しなければ関数または変数に限って補完する。

`skk-completion-prog-list' へ追加すると有効となる。
\(add-to-list 'skk-completion-prog-list
         '\(skk-comp-lisp-symbol\) t\)"
  (cond (skk-abbrev-mode
         (when skk-comp-first
           (let (temp)
             (unless predicate
               (setq predicate (lambda (symbol)
                                 (or (fboundp symbol)
                                     (boundp symbol)))))
             (setq temp
                   (sort (let ((completion-ignore-case nil))
                           (all-completions skk-comp-key obarray predicate))
                         #'string-lessp))
             (when temp
               ;; read-only な object などもあるのでそのまま使わない
               (setq skk-comp-lisp-symbols (mapcar #'copy-sequence temp)))))
         (if skk-comp-lisp-symbols
             (pop skk-comp-lisp-symbols)
           nil))
        (t
         (setq skk-comp-lisp-symbols nil))))

;;;###autoload
(defun skk-search-lisp-symbol (&optional predicate not-abbrev-only
                                         without-char-maybe)
  "Lisp symbol 名で補完した結果を検索結果として返す。
PREDICATE に引数 1 個の関数を指定すれば、PREDICATE を満たすシンボル
に限って補完する。PREDICATE には `fboundp', `boundp', `commandp'
などが指定できる。指定しなければ関数または変数に限って補完する。
SKK abbrev モードにて、英文字 + `skk-completion-search-char' (~)で
未完スペルを指定して変換すると、補完候補が変換候補として出現する。
デフォルトでは SKK abbrev モードのみで有効な機能だが、
NOT-ABBREV-ONLY を指定する事で常に有効となる。


設定例
\(add-to-list 'skk-search-prog-list
         '\(skk-search-lisp-symbol\) t\)"
  (when (and (or not-abbrev-only
                 skk-abbrev-mode))
    (skk-completion-search `((skk-comp-lisp-symbol ',predicate))
                           '((skk-search-identity))
                           'without-midasi
                           without-char-maybe)))

(defun skk-comp-restrict-by-prefix (comp-prog)
  "補完プログラムにより得られた候補を `skk-comp-prefix' で絞り込む。
`skk-comp-prefix' に対応していない補完プログラムを手軽に対応させる際に使う。

  (skk-comp-restrict-by-prefix '(your-completion-program))
のようなものを `skk-completion-prog-list' の要素に指定する。"
  (save-match-data
    (let ((regexp-key (concat "^"
                              (regexp-quote skk-comp-key)
                              (skk-comp-get-regexp skk-comp-prefix)))
          cand)
      (setq cand (eval comp-prog))
      (when skk-comp-use-prefix
        (while (and cand
                    (not (string-match regexp-key cand)))
          (let (skk-comp-first)
            (setq cand (eval comp-prog)))))
      cand)))

;;;###autoload
(defun skk-completion-search (comp-prog-list &optional search-prog-list
                                             without-midasi without-char-maybe)
  "変換キーで補完を行い、得られた各見出しでさらに検索する。
COMP-PROG-LIST は `skk-completion-prog-list' と同じ形式で、
これに含まれる補完関数によって、まず変換キーから見出しのリストを得る。
SEARCH-PROG-LIST は `skk-search-prog-list' と同じ形式で、
補完関数によって得た見出しをこれに含まれる検索関数により変換候補を得る。
デフォルトでは、補完によって得られた見出しと対応する候補はセットであるが、
WITHOUT-MIDASI を指定すると見出しは省かれる。"
  (let (search-char)
    (when (or (setq search-char
                    (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
                        skk-completion-search-char))
              without-char-maybe)
      (let* ((key (if search-char
                      (substring skk-henkan-key
                                 0 (1- (length skk-henkan-key)))
                    skk-henkan-key))
             (skk-comp-use-prefix nil)
             (midasi-list (skk-comp-get-all-candidates key "" comp-prog-list))
             tmp words)
        (dolist (midasi midasi-list)
          (setq tmp (skk-search-progs midasi
                                      (or search-prog-list
                                          skk-search-prog-list)))
          (when tmp ; 補完対象と検索対象は独立なので存在しない事も
            (unless without-midasi
              (setq words (nconc words (list midasi))))
            ;; SKK 本体で skk-nunion してるのでここでは高速性重視
            (setq words (nconc words tmp))))
        words))))

(run-hooks 'skk-comp-load-hook)

(provide 'skk-comp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-comp.el ends here
