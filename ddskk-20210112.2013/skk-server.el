;;; skk-server.el --- $B<-=q%5!<%P$N$?$a$N%W%m%0%i%`(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
;;               1997, 1998, 1999, 2000, 2001
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
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
    (skk-error "$B<-=q%5!<%P$N%[%9%H>pJs$,$"$j$^$;$s(B"
               "Lack of host information of SKK server"))
   ((skk-server-live-p (skk-open-server))
    (let (v)
      (save-match-data
        (with-current-buffer skkserv-working-buffer
          (erase-buffer)
          ;; $B<-=q%5!<%P$N%P!<%8%g%s$rF@$k!#(B
          (process-send-string skkserv-process "2")
          (while (eq (buffer-size) 0)
            (accept-process-output))
          (setq v (buffer-string))
          (erase-buffer)
          ;; $B%[%9%HL>$rF@$k!#(B
          (process-send-string skkserv-process "3")
          (while (eq (buffer-size) 0)
            (accept-process-output))
          (goto-char (point-min))
          (format (concat "SKK SERVER version %s"
                          (if skk-japanese-message-and-error
                              "($B%[%9%HL>(B %s)"
                            "running on HOST %s"))
                  v
                  (prog1
                      (buffer-string)
                    (erase-buffer)))))))))

;;;###autoload
(defun skk-search-server-1 (file limit)
  "`skk-search-server' $B$N%5%V%k!<%A%s!#(B"
  (let ((key
         (if skk-use-numeric-conversion
             (skk-num-compute-henkan-key skk-henkan-key)
           skk-henkan-key))
        ;; $B%P%C%U%!%m!<%+%kCM$N<u$1EO$7$N$?$a!"JLL>$N0l;~JQ?t$K<h$k!#(B
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
            ;; dbskkd-cdb $B$d(B skksearch $B$J$I$N<-=q%5!<%P$r;H$C$FJQ49$9$k(B
            ;; $B:]$K!"(B2 $BIC$[$IBT$?$5$l$k$3$H$,$"$k!#$I$&$d$i1~EzBT$A$N%k!<%W(B
            ;; $B$,IiC4$K$J$C$F$$$k$h$&$G!"B?$$$H$-$K$O?tI4!A:GBg$G?t==K|2s$N(B
            ;; $B1~EzBT$A$K$J$k$3$H$,$"$k!#(Bskk-server-report-response $B$r(B t $B$K(B
            ;; $B$7$F$*$/$H$3$N8=>]$,3NG'$G$-$k!#(B
            ;; $B0J2<$NBP:v$G$[$\2sHr$G$-$kLOMM!#(B
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
            (skk-message "$B<-=q%5!<%P$N1~Ez$r(B %d $B2sBT$A$^$7$?(B"
                         "Waited for server response %d times"
                         count))
          (when (eq (following-char) ?1) ;?1
            (forward-char 2)
            (setq l (skk-compute-henkan-lists okurigana))
            (when l
              (cond ((and okurigana
                          skk-henkan-okuri-strictly)
                     ;; $BAw$j2>L>$,F10l$N%(%s%H%j$N$_$rJV$9!#(B
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
  "$B<-=q%5!<%P$H@\B3$9$k!#%5!<%P!<%W%m%;%9$rJV$9!#(B"
  (unless (skk-server-live-p)
    (setq skkserv-process (skk-open-server-1))
    (when (skk-server-live-p)
      (let ((code (cdr (assoc "euc" skk-coding-system-alist))))
        (set-process-coding-system skkserv-process code code))))
  skkserv-process)

(defun skk-open-server-1 ()
  "`skk-open-server' $B$N%5%V%k!<%A%s!#(B
skkserv $B%5!<%S%9$r%*!<%W%s$G$-$?$i(B process $B$rJV$9!#(B"
  (let* ((host (or skk-server-host (getenv "SKKSERVER")))
         (prog (or skk-server-prog (getenv "SKKSERV")))
         (jisyo (or skk-server-jisyo (getenv "SKK_JISYO")))
         (port skk-server-portnum)
         ;; skk-server-host $B$N>pJs$r(B skk-servers-list $B$KH?1G$9$k(B
         (list (cond ((null skk-servers-list)
                      (if host
                          (list (list host prog jisyo port))
                        (setq skk-server-prog nil)))
                     ((and host
                           (not (assoc host skk-servers-list)))
                      ;; skk-servers-list $B$K(B host $B$,F~$C$F$$$J$$>l9g(B
                      (cons (list host prog jisyo port) skk-servers-list))
                     (t
                      skk-servers-list)))
         elt
         process)
    ;;
    (while (and (not (skk-server-live-p process))
                list)
      ;; $B%5!<%S%9$,MxMQ2DG=$G$J$$%[%9%H$N>pJs$O(B skk-servers-list $B$+$i>C$9(B
      (setq skk-servers-list list
            elt (car list)
            process (or (skk-open-network-stream (car elt) (nth 3 elt))
                        ;; $B%5!<%S%9$,MxMQ2DG=$G$J$$>l9g$O%5!<%S%9$r5/F0(B
                        ;; $B$G$-$k$+$I$&$+;n$_$k(B
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
    ;; $B4D6-JQ?t(B SKKSERVER $B$r(B clear $B$7$F:FEY$NLd$$9g$o$;$rKI$0!#(B
    (setenv "SKKSERVER" nil t)
    ;;
    process))

(defun skk-open-network-stream (&optional host port)
  "HOST $B$K;XDj$5$l$?%[%9%H$K$*$1$k(B skkserv $B%5!<%S%9$N(B TCP $B@\B3$r%*!<%W%s$9$k!#(B
HOST $B$,(B nil $B$J$i$P(B `skk-server-host' $B$r;2>H$9$k!#(B
$B%W%m%;%9$rJV$9!#(B"
  (ignore-errors
    (let ((process
           (open-network-stream "skkservd"
                                skkserv-working-buffer
                                (or host skk-server-host)
                                (or port "skkserv"))))
      (set-process-query-on-exit-flag process nil)
      process)))

(defun skk-startup-server (host prog jisyo port)
  "HOST $B$N(B skkserv $B$rD>@\5/F0$9$k!#(B
$B5/F08e$=$N%5!<%S%9$K@\B3$r;n$_$k!#@\B3$G$-$?>l9g$O%W%m%;%9$rJV$9!#(B"
  (when (and host prog)
    (let (;;(msgbuff (get-buffer-create " *skkserv-msg*"))
          (count 7)
          arg
          process)
      ;; skkserv $B$KM?$($k0z?t$r@_Dj$9$k!#(B

      ;; skkserv $B$N5/F0%*%W%7%g%s$O2<5-$NDL$j!#(B
      ;;     skkserv [-d] [-p NNNN] [JISHO]
      ;;     `-d'     $B%G%#%P%C%0!&%b!<%I(B
      ;;     `-p NNNN'     $BDL?.MQ$N%]!<%HHV9f$H$7$F(BNNNN$B$r;H$&(B.
      ;;     `~/JISYO'     ~/JISYO$B$r<-=q$H$7$FMxMQ(B.
      (when jisyo
        ;; skkserv $B$O0z?t$K<-=q$,;XDj$5$l$F$$$J$1$l$P!"(BDEFAULT_JISYO $B$r(B
        ;; $B;2>H$9$k!#(B
        (setq arg (list jisyo)))
      ;;(if skk-server-debug
      ;;    (setq arg (cons "-d" arg)))
      (when port
        (setq arg (nconc (list "-p" (number-to-string port)) arg)))

      ;; skkserv $B$N5/F0%H%i%$%"%k$r7+$jJV$9(B...?
      (while (> count 0)
        (skk-message
         "%s $B$N<-=q%5!<%P$,5/F0$7$F$$$^$;$s!#5/F0$7$^$9(B%s"
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
                 ;; 0 $B$K$7$F%5%V%W%m%;%9$N=*N;$rBT$C$F$O$$$1$J$$M}M3$,$"$k!)(B
                 ;; $B$J$1$l$P(B msgbuf $B$K%(%i!<=PNO$r<h$C$?J}$,7z@_E*$G$O!)(B  $B$^$?(B
                 ;; $B$=$N>l9g$O$3$N(B while $B%k!<%W<+?H$,$$$i$J$$!)(B
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
        (skk-message "$B%[%9%H(B %s $B$N<-=q%5!<%P$,5/F0$7$^$7$?(B"
                     "SKK SERVER on %s is active now"
                     skk-server-host)
        (sit-for 1)
        ;; process $B$rJV$jCM$H$9$k(B
        process)
       (t
        (skk-message "%s $B$N<-=q%5!<%P$r5/F0$9$k$3$H$,$G$-$^$;$s$G$7$?(B"
                     "Could not activate SKK SERVER on %s"
                     skk-server-host)
        (sit-for 1)
        (ding)
        nil)))))

;;;###autoload
(defun skk-adjust-search-prog-list-for-server-search (&optional non-del)
  "$BJQ?t(B `skk-search-prog-list' $B$rD4@0$9$k!#(B
`skk-server-host' $B$b$7$/$O(B `skk-servers-list' $B$,(B nil $B$G$"$l$P!"(B
`skk-search-prog-list' $B$+$i(B `skk-search-server' $B$r(B car $B$K;}$D%j%9%H$r>C$9!#(B
non-nil $B$G$"$l$P!"2C$($k!#(B"
  (when (and (or skk-server-host
                 skk-servers-list)
             (not (assq 'skk-search-server
                        (default-value 'skk-search-prog-list))))
    ;; skk-search-prog-list $B$,(B nil $B$H$$$&$3$H$O$^$:$J$$$@$m$&$,!"G0$N$?(B
    ;; $B$a!"(Bsetq $B$7$F$*$/!#(B
    (setq-default
     skk-search-prog-list
     ;; $BKvHx$KIU$1$k!#KvHx$K$O(B (skk-okuri-search) $B$r;}$C$F$-$?$$?M(B
     ;; $B$b$$$k$+$b!#%*%W%7%g%s$GIU$1$k>l=j$rJQ99$9$k$h$&$K$7$?J}$,(B
     ;; $BNI$$!)(B
     (nconc (default-value 'skk-search-prog-list)
            (list
             '(skk-search-server skk-aux-large-jisyo 10000))))))

;;;###autoload
(defun skk-disconnect-server ()
  "$B<-=q%5!<%P$r@Z$jN%$9!#(B"
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
