;;; skk-server.el --- 辞書サーバのためのプログラム -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
;;               1997, 1998, 1999, 2000, 2001
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

;;; Code:

(require 'skk)

(defun skk-server-live-p (&optional process)
  "Return t if PROCESS is alive.
When PROCESS is nil, check `skkserv-process' instead."
  (unless process
    (setq process skkserv-process))
  (and process
       (eq (process-status process) 'open)))

;;;###autoload
(defun skk-server-version ()
  "Return version information of SKK server.
When called interactively, print version information."
  (interactive)
  (cond
   ((called-interactively-p 'interactive)
    (message "%s" (skk-server-version)))
   ((not (or skk-server-host
             skk-servers-list))
    (skk-error "辞書サーバのホスト情報がありません"
               "Lack of host information of SKK server"))
   ((skk-server-live-p (skk-open-server))
    (let (v)
      (save-match-data
        (with-current-buffer skkserv-working-buffer
          (erase-buffer)
          ;; 辞書サーバのバージョンを得る。
          (process-send-string skkserv-process "2")
          (while (eq (buffer-size) 0)
            (accept-process-output))
          (setq v (buffer-string))
          (erase-buffer)
          ;; ホスト名を得る。
          (process-send-string skkserv-process "3")
          (while (eq (buffer-size) 0)
            (accept-process-output))
          (goto-char (point-min))
          (format (concat "SKK SERVER version %s"
                          (if skk-japanese-message-and-error
                              "(ホスト名 %s)"
                            "running on HOST %s"))
                  v
                  (prog1
                      (buffer-string)
                    (erase-buffer)))))))))

;;;###autoload
(defun skk-search-server-1 (file limit)
  "`skk-search-server' のサブルーチン。"
  (let ((key
         (if skk-use-numeric-conversion
             (skk-num-compute-henkan-key skk-henkan-key)
           skk-henkan-key))
        ;; バッファローカル値の受け渡しのため、別名の一時変数に取る。
        (okurigana (or skk-henkan-okurigana
                       skk-okuri-char)))
    (cond
     ((skk-server-live-p (skk-open-server))
      (with-current-buffer skkserv-working-buffer
        (let ((cont t)
              (count 0)
              l)
          (erase-buffer)
          (process-send-string skkserv-process (concat "1" key " "))
          (while (and cont (skk-server-live-p))
            (accept-process-output)
            ;; XXX workaround
            ;; dbskkd-cdb や skksearch などの辞書サーバを使って変換する
            ;; 際に、2 秒ほど待たされることがある。どうやら応答待ちのループ
            ;; が負担になっているようで、多いときには数百〜最大で数十万回の
            ;; 応答待ちになることがある。skk-server-report-response を t に
            ;; しておくとこの現象が確認できる。
            ;; 以下の対策でほぼ回避できる模様。
            (sit-for 0.01)
            ;;
            (setq count (1+ count))
            (when (> (buffer-size) 0)
              (if (eq (char-after 1) ?1) ;?1
                  ;; found key successfully, so check if a whole line
                  ;; is received.
                  (when (eq (char-after (1- (point-max)))
                            ?\n) ;?\n
                    (setq cont nil))
                ;; not found or error, so exit
                (setq cont nil))))
          (goto-char (point-min))
          (when skk-server-report-response
            (skk-message "辞書サーバの応答を %d 回待ちました"
                         "Waited for server response %d times"
                         count))
          (when (eq (following-char) ?1) ;?1
            (forward-char 2)
            (setq l (skk-compute-henkan-lists okurigana))
            (when l
              (cond ((and okurigana
                          skk-henkan-okuri-strictly)
                     ;; 送り仮名が同一のエントリのみを返す。
                     (nth 2 l))
                    ((and okurigana
                          skk-henkan-strict-okuri-precedence)
                     (skk-nunion (nth 2 l) (car l)))
                    (t
                     (car l))))))))
     (t
      ;; server is not active, so search file instead
      (when (and (stringp file)
                 (file-readable-p file))
        (skk-search-jisyo-file file limit))))))

(defun skk-open-server ()
  "辞書サーバと接続する。サーバープロセスを返す。"
  (unless (skk-server-live-p)
    (setq skkserv-process (skk-open-server-1))
    (when (skk-server-live-p)
      (let ((code (cdr (assoc "euc" skk-coding-system-alist))))
        (set-process-coding-system skkserv-process code code))))
  skkserv-process)

(defun skk-open-server-1 ()
  "`skk-open-server' のサブルーチン。
skkserv サービスをオープンできたら process を返す。"
  (let* ((host (or skk-server-host (getenv "SKKSERVER")))
         (prog (or skk-server-prog (getenv "SKKSERV")))
         (jisyo (or skk-server-jisyo (getenv "SKK_JISYO")))
         (port skk-server-portnum)
         ;; skk-server-host の情報を skk-servers-list に反映する
         (list (cond ((null skk-servers-list)
                      (if host
                          (list (list host prog jisyo port))
                        (setq skk-server-prog nil)))
                     ((and host
                           (not (assoc host skk-servers-list)))
                      ;; skk-servers-list に host が入っていない場合
                      (cons (list host prog jisyo port) skk-servers-list))
                     (t
                      skk-servers-list)))
         elt
         process)
    ;;
    (while (and (not (skk-server-live-p process))
                list)
      ;; サービスが利用可能でないホストの情報は skk-servers-list から消す
      (setq skk-servers-list list
            elt (car list)
            process (or (skk-open-network-stream (car elt) (nth 3 elt))
                        ;; サービスが利用可能でない場合はサービスを起動
                        ;; できるかどうか試みる
                        (if (not skk-server-inhibit-startup-server)
                            (apply #'skk-startup-server elt)
                          nil))
            list (cdr list)))
    ;;
    (unless (skk-server-live-p process)
      ;; clear skk-server-host to disable server search in this session
      (setq skk-server-host nil
            skk-server-prog nil
            skk-servers-list nil))
    ;; 環境変数 SKKSERVER を clear して再度の問い合わせを防ぐ。
    (setenv "SKKSERVER" nil t)
    ;;
    process))

(defun skk-open-network-stream (&optional host port)
  "HOST に指定されたホストにおける skkserv サービスの TCP 接続をオープンする。
HOST が nil ならば `skk-server-host' を参照する。
プロセスを返す。"
  (ignore-errors
    (let ((process
           (open-network-stream "skkservd"
                                skkserv-working-buffer
                                (or host skk-server-host)
                                (or port "skkserv"))))
      (set-process-query-on-exit-flag process nil)
      process)))

(defun skk-startup-server (host prog jisyo port)
  "HOST の skkserv を直接起動する。
起動後そのサービスに接続を試みる。接続できた場合はプロセスを返す。"
  (when (and host prog)
    (let (;;(msgbuff (get-buffer-create " *skkserv-msg*"))
          (count 7)
          arg
          process)
      ;; skkserv に与える引数を設定する。

      ;; skkserv の起動オプションは下記の通り。
      ;;     skkserv [-d] [-p NNNN] [JISHO]
      ;;     `-d'     ディバッグ・モード
      ;;     `-p NNNN'     通信用のポート番号としてNNNNを使う.
      ;;     `~/JISYO'     ~/JISYOを辞書として利用.
      (when jisyo
        ;; skkserv は引数に辞書が指定されていなければ、DEFAULT_JISYO を
        ;; 参照する。
        (setq arg (list jisyo)))
      ;;(if skk-server-debug
      ;;    (setq arg (cons "-d" arg)))
      (when port
        (setq arg (nconc (list "-p" (number-to-string port)) arg)))

      ;; skkserv の起動トライアルを繰り返す...?
      (while (> count 0)
        (skk-message
         "%s の辞書サーバが起動していません。起動します%s"
         "SKK SERVER on %s is not active, I will activate it%s"
         host (make-string count ?.))
        (if (or (string= host (system-name))
                (string= host "localhost"))
            ;; server host is local machine
            (apply 'call-process prog nil
                   ;;msgbuff
                   0 nil arg)
          (apply 'call-process
                 skk-server-remote-shell-program nil
                 ;; 0 にしてサブプロセスの終了を待ってはいけない理由がある？
                 ;; なければ msgbuf にエラー出力を取った方が建設的では？  また
                 ;; その場合はこの while ループ自身がいらない？
                 ;; msgbuff
                 0 nil host prog arg))
        (sleep-for 3)
        (if (and (setq process (skk-open-network-stream host port))
                 (skk-server-live-p process))
            (setq count 0)
          (setq count (1- count))))

      ;;
      (cond
       ((skk-server-live-p process)
        (skk-message "ホスト %s の辞書サーバが起動しました"
                     "SKK SERVER on %s is active now"
                     skk-server-host)
        (sit-for 1)
        ;; process を返り値とする
        process)
       (t
        (skk-message "%s の辞書サーバを起動することができませんでした"
                     "Could not activate SKK SERVER on %s"
                     skk-server-host)
        (sit-for 1)
        (ding)
        nil)))))

;;;###autoload
(defun skk-adjust-search-prog-list-for-server-search (&optional non-del)
  "変数 `skk-search-prog-list' を調整する。
`skk-server-host' もしくは `skk-servers-list' が nil であれば、
`skk-search-prog-list' から `skk-search-server' を car に持つリストを消す。
non-nil であれば、加える。"
  (when (and (or skk-server-host
                 skk-servers-list)
             (not (assq 'skk-search-server
                        (default-value 'skk-search-prog-list))))
    ;; skk-search-prog-list が nil ということはまずないだろうが、念のた
    ;; め、setq しておく。
    (setq-default
     skk-search-prog-list
     ;; 末尾に付ける。末尾には (skk-okuri-search) を持ってきたい人
     ;; もいるかも。オプションで付ける場所を変更するようにした方が
     ;; 良い？
     (nconc (default-value 'skk-search-prog-list)
            (list
             '(skk-search-server skk-aux-large-jisyo 10000))))))

;;;###autoload
(defun skk-disconnect-server ()
  "辞書サーバを切り離す。"
  (when (and skk-server-host
             (skk-server-live-p))
    ;; disconnect server
    (process-send-string skkserv-process "0")
    ;; Workaround is needed for NTEmacs. It cannot receive output from
    ;; a server at least in noninteractive mode.
    (unless (and (eq system-type 'windows-nt)
                 (not (featurep 'meadow))
                 noninteractive)
      (accept-process-output skkserv-process))))

;; (add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-server-search)
(add-hook 'kill-emacs-hook 'skk-disconnect-server)

(run-hooks 'skk-server-load-hook)

(provide 'skk-server)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-server.el ends here
