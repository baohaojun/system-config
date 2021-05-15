;;; skk-annotation.el --- SKK annotation $B4XO"%W%m%0%i%`(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Copyright (C) 2000-2010  SKK Development Team

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; Keywords: japanese, mule, input method
;; Created: Oct. 27, 2000.

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

;; $B$3$l$O!"(BSKK $B8D?M<-=q$KIU$1$?%"%N%F!<%7%g%s(B ($BCp<a(B) $B$r3hMQ$9$k%W%m%0(B
;; $B%i%`$G$9!#(B

;; <HOW TO USE>

;;   (setq skk-show-annotation t)

;; $B$H(B ~/.skk $B$K=q$-$^$7$g$&!#<-=q$N8uJd$K(B `;' $B$+$i;O$^$kJ8;zNs$,$"$l$P!"(B
;; $B$=$N3:Ev$N8uJd$,JQ49$5$l$F%P%C%U%!$K=PNO$5$l$?:]$K(B `;' $B0J9_$r$=$N8u(B
;; $BJd$N%"%N%F!<%7%g%s$H$7$F%(%3!<%(%j%"$KI=<($7$^$9!#(B

;;   (setq skk-annotation-show-as-message nil)

;; $B$H(B ~/.skk $B$K=q$$$?>l9g$O!"(Bother-window $B$r0l;~E*$K3+$$$F%"%N%F!<%7%g(B
;; $B%s$rI=<($7$^$9!#(Bother-window $B$O$=$N8uJd$K$D$$$F3NDj$9$k$+!"$=$N8uJd(B
;; $B$NA*Br$r;_$a$k(B ($B<!$N8uJd$rA*Br$7$?$j!"(Bquit $B$7$?$j(B) $B$9$k$H<+F0E*$KJD(B
;; $B$8$i$l$^$9!#(B

;; SKK $B$G$O(B 5 $BHVL\$N8uJd0J9_$O8uJd0lMw(B ($B%j%9%H(B) $B$H$7$FI=<($5$l$k$?$a!"%"(B
;; $B%N%F!<%7%g%s$O8uJd$H0l=o$KE;$a$GI=<($5$l$^$9!#(B

;; $B$"$kC18l$K%"%N%F!<%7%g%s$rIU$1$?$$$H$-$O!"3NDj$7$?D>8e$KF1$8%P%C%U%!(B
;; $B$G(B

;;   M-x skk-annotation-add

;; $B$H<B9T$7$^$7$g$&!#%"%N%F!<%7%g%s$rJT=8$9$k%P%C%U%!$,3+$$$F!"%+%l%s%H(B
;; $B%P%C%U%!$K$J$j$^$9$N$G!"$=$3$X%"%N%F!<%7%g%s$rIU$1$^$7$g$&!#(B
;; 1 $B9T$G$"$kI,MW$O$"$j$^$;$s$,!"J#?t9T$N%"%N%F!<%7%g%s$rIU$1$k$H%(%3!<(B
;; $B%(%j%"$KI=<($7$?$H$-$KA4BN$,8+$($J$/$J$j$^$9!#(B
;; $B$^$?!"(B`;' $B$NJ8;z<+BN$OF~$l$kI,MW$O$"$j$^$;$s!#(B
;; $B:#$^$G$K4{$KIU$1$F$$$?%"%N%F!<%7%g%s$,$"$l$PJT=8%P%C%U%!$,I=<($5$l(B
;; $B$?$H$-$K$=$N%"%N%F!<%7%g%s$,(B prefix $BE*$K=PNO$5$l$^$9!#4{B8$N%"%N%F!<(B
;; $B%7%g%s$b4^$a$FJT=8$7$F2<$5$$!#%P%C%U%!$N@hF,9T$r=|$$$FA4$F$N9T$,?7(B
;; $B$7$$%"%N%F!<%7%g%s$H$7$F>e=q$-$5$l$^$9!#(B
;; $BJT=8$,=*$o$C$?$i(B C-c C-c $B$7$^$7$g$&!#(B

;; $B>e5-$NF0:n$G%f!<%6$,IU$1$?%"%N%F!<%7%g%s$r!V%f!<%6%"%N%F!<%7%g%s!W(B
;; $B$H8F$S$^$9!#%f!<%6%"%N%F!<%7%g%s$O!"(B

;;   $B!V$-$+$s(B /$B4|4V(B/$B5!4X(B;*$B5!4XEj;q2H(B/$B4p44(B;*$B4p446HL3(B/$B!W(B

;; $B$N$h$&$K(B `;' $B$ND>8e$K(B `*' $B$NJ8;z$,<+F0E*$K?6$i$l$^$9!#$3$l$O%f!<%6(B
;; $B$,FH<+$KIU$1$?%"%N%F!<%7%g%s$G$"$k$3$H$r<($7$^$9(B (`*' $B$NJ8;z$OJQ49(B
;; $B;~$K$OI=<($5$l$^$;$s(B)$B!#(B

;; $B0lJ}!"6&M-<-=q$K85!9IU$1$i$l$F$$$k%"%N%F!<%7%g%s$r!V%7%9%F%`%"%N%F!<(B
;; $B%7%g%s!W$H8F$S!"$3$l$O(B `;' $B$ND>8e$K(B `*' $B$NJ8;z$rH<$J$$$^$;$s!#(B
;; <$BNc(B>
;;    $B!V$$$<$s(B /$B0JA0(B;previous/$B0MA3(B;still/$B!W(B

;; $B%f!<%6%"%N%F!<%7%g%s$H%7%9%F%`%"%N%F!<%7%g%s$r6hJL$9$k$3$H$G!"%f!<(B
;; $B%6%"%N%F!<%7%g%s$@$1$rI=<($7$?$j!"$"$k$$$O$=$N5U$r9T$&$3$H$,2DG=(B
;; $B$G$9!#(B`skk-annotation-function' $B$KI=<($7$?$$%"%N%F!<%7%g%s$r(B
;; non-nil $B$HH=Dj$9$k4X?t$r=q$-$^$7$g$&!#$3$s$J46$8$G$9!#(B

;;   (setq skk-annotation-function
;;         (lambda (annotation) (eq (aref annotation 0) ?*)))

;; $B>e5-$NNc$G$O!"%"%N%F!<%7%g%s$N@hF,$,(B `*' $B$G;O$^$k!V%f!<%6%"%N%F!<%7%g(B
;; $B%s!W$N>l9g$K(B t $B$rJV$7$^$9$N$G!"%f!<%6%"%N%F!<%7%g%s$@$1$rI=<($7$^$9!#(B

;; M-x skk-annotation-add $B$7$?$b$N$N!"7k6I%"%N%F!<%7%g%s$rIU$1$:$KCV$-(B
;; $B$?$$$H$-$O!"(B

;;   M-x skk-annotation-kill

;; $B$7$F2<$5$$!#(B

;; $B$^$?!":G8e$K3NDj$7$?8uJd$K$D$$$F$N%"%N%F!<%7%g%s$r<h$j5n$j$?$$$H$-(B
;; $B$O!"(B

;;   M-x skk-annotation-remove

;; $B$7$F2<$5$$!#(B

;; Viper $BBP:v$O$^$@9T$C$F$$$^$;$s!#(B~/.viper $B$K<!$N$h$&$K=q$$$F2<$5$$!#(B
;; (viper-harness-minor-mode "skk-annotation")


;; <lookup.el $B$+$i$N%"%N%F!<%7%g%s(B>

;; $B0J2<$N@_Dj$r(B ~/.skk $B$K$9$k$H4X?t(B skk-lookup-get-content $B$+$i%"%N%F!<(B
;; $B%7%g%s$,<hF@$5$l$^$9!#(B

;;   (setq skk-annotation-lookup-lookup t)


;; <Apple OS X $B!V<-=q!W%5!<%S%9$+$i$N%"%N%F!<%7%g%s(B>

;; Mac $B$N<-=q%"%W%j(B (Dictionary.app) $B$G$OI8=`$G9q8l<-E5$J$IMxMQ$G$-$^$9!#(B
;; $B$3$N$&$AM%@h=g0L$N9b$$<-=q$+$i%"%N%F!<%7%g%s$r<hF@$9$k5!G=$,MxMQ$G$-$^(B
;; $B$9!#8=>u$G$O<-=q$N8!:w=g$O(B Dictionary.app $B$N4D6-@_Dj$G@_Dj$9$kI,MW$,$"(B
;; $B$j$^$9!#(B

;; $B$3$N5!G=$rMxMQ$9$k>l9g$O0J2<$N@_Dj$r(B ~/.skk $B$K5-=R$7$F$/$@$5$$!#(B

;;   (setq skk-annotation-lookup-DictionaryServices t)

;; $B$3$N5!G=$O(B Carbon Emacs 22 $B$^$?$O(B Cocoa Emacs 23 $B0J9_$G%F%9%H$5$l$F$$$^(B
;; $B$9!#$?$@$7(B Carbon Emacs 22 $B$G$OJQ49A`:n$,B.$9$.$k>l9g$K<-=q%5!<%S%9$+$i<u$1(B
;; $B$H$C$?J8;zNs$N%G%3!<%I$K<:GT$9$k$3$H$,$"$k$h$&$G$9!#(B


;; <Wikipedia $B%"%N%F!<%7%g%s(B>

;; $B"'%b!<%I$K$F(B C-i $B$r%?%$%W$9$k$H!"I=<(Cf$N8uJd$r(B Wikipedia/Wiktionary
;; $B$N9`L\$+$iC5$7!$8+$D$+$C$?>l9g$O!"FbMF$NH4?h$r%"%N%F!<%7%g%s$H$7$FI=<((B
;; $B$7$^$9!#$3$N5!G=$O(B Emacs 22 $B0J>e$G%F%9%H$5$l$F$$$^$9!#(B


;; <$B5l$$(B SKK $B$+$i$N0\9T(B>

;; $B$3$N9`$O%"%N%F!<%7%g%s5!G=$,$J$$5l$$(B SKK (DDSKK 11.2 $B0JA0$^$?$O(B SKK
;; 10.62 $B0JA0(B) $B$+$i:G?7$N$b$N$K0\9T$9$k>l9g$NCm0U;v9`$G$9!#(B

;; $B%"%N%F!<%7%g%s$O%;%Q%l!<%?$H$7$F(B `;' $B$r;HMQ$7$F$$$k$?$a!"(B`;' $B$NJ8;z(B
;; $B$r4^$s$@8uJd$O!"(Beval $B$9$k$H(B `;' $B$K$J$k(B Lisp $B<0$H$7$F(B quote $B$7<-=q8u(B
;; $BJd$K<}$a$kI,MW$,$"$j$^$9!#(B

;; $B$^$@%"%N%F!<%7%g%s5!G=$r0lEY$b;HMQ$7$F$$$J$$8D?M<-=q$K$D$$$F$O!"0J2<(B
;; $B$N(B S $B<0$rI>2A$7$?8e!"(B

;;   (defun skk-annotation-update-jisyo-format ()
;;     (interactive)
;;     (skk-setup-jisyo-buffer)
;;     (let ((min skk-okuri-ari-min) (max skk-okuri-ari-max))
;;       (skk-annotation-update-jisyo-format-1 min max)
;;       (setq min skk-okuri-nasi-min
;;       max (point-max))
;;       (skk-annotation-update-jisyo-format-1 min max)))

;;   (defun skk-annotation-update-jisyo-format-1 (min max)
;;     (let (candidate)
;;       (goto-char min)
;;       (while (re-search-forward "\\/\\([^\n/]*;[^\n/]*\\)\\/" max t nil)
;;   (setq candidate (buffer-substring-no-properties
;;            (match-beginning 1) (match-end 1)))
;;   (delete-region (match-beginning 1) (match-end 1))
;;   (goto-char (match-beginning 1))
;;   (insert
;;    (concat "(concat \""
;;        (mapconcat
;;         (function
;;          (lambda (c)
;;            (if (eq c ?\;)
;;            "\\073"
;;          (char-to-string c))))
;;         (append candidate nil) "")
;;        "\")")))))

;; $B8D?M<-=q$rFI$_$3$_!"<-=q$rFI$_9~$s$@%P%C%U%!$G(B

;;   M-x skk-annotation-update-jisyo-format

;; $B$9$k$3$H$G$3$N:n6H$r9T$&$3$H$,$G$-$^$9!#(B

;; $BC"$7!"4{$K%"%N%F!<%7%g%s$,IU$1$i$l$F$$$k>l9g$O!"$3$N%"%N%F!<%7%g%s(B
;; $B<+BN$b8uJd$H6hJL$G$-$:$K(B quote $B$5$l$F$7$^$$$^$9$N$G!"$4Cm0U2<$5$$(B
;; ($B:#$N$H$3$m<j:n6H$G(B quote $B$5$l$J$$$h$&$KB`Hr$9$k$J$I$7$+J}K!$O$"$j(B
;; $B$^$;$s(B)$B!#(B

;;; Code:

(require 'skk)

(eval-and-compile
  (require 'compile)
  (require 'comint)

  (autoload 'skk-lookup-get-content "skk-lookup")
  (autoload 'run-python "python")
  (autoload 'python-check-comint-prompt "python")
  (autoload 'python-shell-internal-get-or-create-process "python")
  (autoload 'python-shell-get-process-name "python")
  (autoload 'html2text "html2text")
  (autoload 'html2text-delete-tags "html2text")
  (autoload 'url-hexify-string "url-util")
  (autoload 'url-retrieve "url"))

(eval-when-compile
  (defvar python-buffer)
  (defvar python-shell-prompt-regexp)
  (defvar python-shell-buffer-name)
  (defvar mule-version)
  (defvar html2text-remove-tag-list)
  (defvar html2text-format-tag-list))

(unless skk-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'skk-annotation-save-and-quit)
    (define-key map "\C-c\C-k" 'skk-annotation-kill)
    (setq skk-annotation-mode-map map)))

(unless (assq 'skk-annotation-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(skk-annotation-mode " annotation")
                               minor-mode-alist)))

(when (and (boundp 'minor-mode-map-alist)
           (not (assq 'skk-annotation-mode-map minor-mode-map-alist)))
  (setq minor-mode-map-alist
        (cons (cons 'skk-annotation-mode skk-annotation-mode-map)
              minor-mode-map-alist)))

;; functions.
(defsubst skkannot-in-normal-buffer-p ()
  (not (or skk-isearch-switch (skk-in-minibuffer-p))))

(defsubst skkannot-clear-msg ()
  (unless (skkannot-in-normal-buffer-p)
    (message nil)))

(defsubst skkannot-dict-exec-find ()
  (ignore-errors (executable-find skk-annotation-dict-program)))

(defun skkannot-check-lookup ()
  (unless (and (boundp 'lookup-search-agents)
               (symbol-value 'lookup-search-agents)
               (locate-library "lookup")
               (locate-library "skk-lookup"))
    (setq skk-annotation-other-sources
          (delq 'lookup.el skk-annotation-other-sources))))

(defun skk-annotation-erase-buffer ()
  (let ((inhibit-read-only t)
        buffer-read-only)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)))

(defun skk-annotation-insert (annotation)
  "`skk-annotation-buffer' $B$K(B ANNOTATION $B$rA^F~$9$k(B"
  (with-current-buffer (get-buffer-create skk-annotation-buffer)
    (skk-annotation-erase-buffer)
    (setq buffer-read-only nil)
    (insert annotation)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

;;;###autoload
(defun skk-annotation-get (annotation)
  (cond ((string= annotation "")
         "")
        ((eq (aref annotation 0) ?*)
         (substring annotation 1))
        (t
         annotation)))

(defun skkannot-sit-for (seconds &optional listing-p)
  (condition-case nil
      (sit-for seconds)
    (quit
     (with-current-buffer skkannot-buffer-origin
       (cond
        (listing-p
         (skk-escape-from-show-candidates 0))
        (t
         (keyboard-quit)))))))

;;; $B%"%N%F!<%7%g%sI=<(5!G=(B
;;;###autoload
(defun skk-annotation-find-and-show (pair)
  "$B3F<o%j%=!<%9$+$i%"%N%F!<%7%g%s$r<hF@$7I=<($9$k!#(B"
  (skkannot-check-lookup)
  (skkannot-clear-msg)
  ;;
  (let ((word (car-safe pair))
        (note (cdr-safe pair))
        (srcs skk-annotation-other-sources)
        list)
    (when skk-annotation-first-candidate
      (setq skkannot-remaining-delay skk-annotation-delay
            skk-annotation-first-candidate nil))
    (when (and (skkannot-in-normal-buffer-p)
               word
               (or skk-annotation-lookup-lookup
                   skk-annotation-lookup-DictionaryServices
                   (and skk-annotation-lookup-dict
                        (skkannot-dict-exec-find))))
      ;; Mac $B$N!V<-=q!W$"$k$$$O(B dict ($B30It%W%m%0%i%`(B) $B$N@_Dj$,$"$l$P(B
      ;; SKK $B<-=q$N%"%N%F!<%7%g%s$h$jM%@h$5$;$k(B
      (setq note nil)
      (when skk-annotation-lookup-lookup
        (setq note (skk-lookup-get-content word)))
      (when (and (null note)
                 (eq system-type 'darwin)
                 skk-annotation-lookup-DictionaryServices)
        (catch '$B<-=q(B
          (setq note (skk-annotation-lookup-DictionaryServices word))))
      (when (and (null note)
                 skk-annotation-lookup-dict
                 (skkannot-dict-exec-find))
        (catch 'dict
          (setq note (skk-annotation-lookup-dict word))
          ;; $BM>M5$,$"$l$P<!8uJd$N0UL#$r@hFI$_(B
          (dotimes (i (min (length skk-henkan-list) 4))
            (add-to-list 'list (nth i skk-henkan-list) t))
          (when list
            (dolist (el list)
              (setq el (car (skk-treat-strip-note-from-word el)))
              (unless (equal word el)
                (skk-annotation-preread-dict el))))))
      (unless note
        (setq note (cdr-safe pair))))
    (when (and word (not note))
      ;; Wikipedia $B$J$I$=$NB>$N%j%=!<%9$+$i$N%-%c%C%7%e$,$"$l$P(B
      ;; $B$=$l$rI=<($9$k!#(B
      (unless skkannot-cached-srcs
        (while srcs
          (unless (memq (car srcs) '(lookup.el))
            (add-to-list 'skkannot-cached-srcs (car srcs) t))
          (setq srcs (cdr srcs))))
      (setq note (car (skkannot-cache word skkannot-cached-srcs))))
    ;;
    (setq skkannot-buffer-origin (current-buffer))
    (cond
     ((or (<= skkannot-remaining-delay 0)
          (skkannot-sit-for skkannot-remaining-delay))
      (setq skkannot-remaining-delay 0)
      (skk-annotation-show (or note "") word
                           (unless skk-annotation-lookup-lookup
                             skkannot-cached-srcs)))
     (t
      (setq skkannot-remaining-delay skk-annotation-delay)
      (skk-annotation-show "" word (unless skk-annotation-lookup-lookup
                                     skkannot-cached-srcs))))))

;;;###autoload
(defun skk-annotation-show (annotation &optional word sources)
  (when (and (not skk-kakutei-flag)
             (or (not skk-annotation-function)
                 (funcall skk-annotation-function annotation)))
    (setq annotation (skk-annotation-get annotation))
    (let ((notes (mapcar #'skk-eval-string (split-string annotation ";")))
          (inhibit-wait skk-isearch-switch))
      (setq annotation (skk-eval-string annotation))
      (unless (string= annotation "")
        (setq inhibit-wait (skk-annotation-show-2 annotation)))
      ;; $BCm<a$NI=<($O$3$3$^$G$@$,!"$3$3$G%f!<%6$,Cm<a$NFbMF$r%3%T!<$7$?$j(B
      ;; $B$7$FMxMQ$G$-$k$h$&$K$9$k!#(B
      (unless inhibit-wait
        (skk-annotation-wait-for-input annotation notes word sources)))))

(defun skk-annotation-show-2 (annotation)
  (let (inhibit-wait)
    (cond (skk-isearch-switch
           ;; do nothing
           (setq inhibit-wait t))
          ((and (not (skk-annotation-display-p 'minibuf))
                (skk-in-minibuffer-p))
           ;; do nothing
           (setq inhibit-wait t))
          ((and window-system skk-show-tooltip)
           (skk-tooltip-show-at-point annotation 'annotation))
          ((and skk-annotation-show-as-message
                (skkannot-in-normal-buffer-p))
           (skk-annotation-show-as-message annotation))
          (t
           (skk-annotation-show-buffer annotation)))
    inhibit-wait))

(defun skk-annotation-show-buffer (annotation)
  (condition-case nil
      (save-window-excursion
        (let ((minibuf-p (skk-in-minibuffer-p))
              event window)
          (skk-annotation-insert annotation)
          (when minibuf-p
            (if (setq window (get-buffer-window (skk-minibuffer-origin)))
                (select-window window)
              (other-window 1))
            (unless (eq (next-window) (selected-window))
              (delete-other-windows)))
          ;;
          (fit-window-to-buffer (display-buffer skk-annotation-buffer))
          (when minibuf-p
            (select-window (minibuffer-window)))
          ;;
          (skk-annotation-message 'annotation)
          ;;
          (setq event (read-event))
          (when (skk-key-binding-member (skk-event-key event)
                                        '(key-board-quit
                                          skk-kanagaki-bs
                                          skk-kanagaki-esc)
                                        skk-j-mode-map)
            (signal 'quit nil))
          (skk-unread-event event)))
    (quit
     ;; skk-previous-candidate $B$X(B
     (skk-reset-henkan-count 0))))

(defun skk-annotation-show-as-message (annotation)
  (message "%s" annotation))

;;;###autoload
(defun skk-annotation-display-p (test)
  ;; TEST $B$O(B 'list $BKt$O(B 'minibuf
  (skkannot-clear-msg)
  ;;
  (cond ((null skk-show-annotation)
         nil)
        ((and (listp skk-show-annotation)
              (eq (car skk-show-annotation) 'not)
              ;; (not ...)
              (memq test skk-show-annotation))
         ;; (not list), (not minibuf) or (not list minibuf)
         nil)
        (t
         ;; non-nil
         t)))

;;;###autoload
(defun skk-annotation-toggle-display-p ()
  (interactive)
  (cond ((null skk-show-annotation)
         ;; do nothing
         nil)
        ((and (listp skk-show-annotation)
              (eq (car skk-show-annotation) 'not))
         ;; (not ...)
         (cond ((memq 'list skk-show-annotation)
                (if (eq (length skk-show-annotation) 2)
                    ;; (not list) -> t  i.e. turn on
                    (setq skk-show-annotation t)
                  ;; (not list minibuf) -> (not minibuf)
                  (setq skk-show-annotation '(not minibuf))))
               (t
                ;; (not minibuf) -> (not list minibuf)  i.e. turn off
                (setq skk-show-annotation '(not list minibuf)))))
        (t
         ;; non-nil -> (not list)  i.e. turn off
         (setq skk-show-annotation '(not list)))))

;;; $B%"%N%F!<%7%g%sA`:n5!G=(B
(defun skk-annotation-setup ()
  (let ((skk-henkan-key (skk-get-last-henkan-datum 'henkan-key))
        (skk-okuri-char (skk-get-last-henkan-datum 'okuri-char))
        (cand (car (skk-get-last-henkan-datum 'henkan-list)))
        word)
    (unless cand
      (setq skk-henkan-key
            (read-from-minibuffer "Midasi: "))
      (when (string= skk-henkan-key "")
        (skk-error "$B%"%N%F!<%7%g%s$9$kC18l$,$"$j$^$;$s(B"
                   "No word to be annotated"))
      (when (string-match "\\cj\\([a-z]+\\)$"
                          skk-henkan-key)
        (setq skk-okuri-char (match-string 1 skk-henkan-key)
              ;; $BAw$j$"$jJQ49$r;XDj$9$k$H(B
              ;; skk-henkan-okurigana $B$N;XDj$K:$$k!#(B
              skk-henkan-okurigana ""))
      (setq cand
            (prog1
                (skk-henkan-in-minibuff)
              (setq skk-kakutei-flag nil))))
    ;; $B$3$N;~E@$G$O(B skk-num-list $B$O4{$K(B nil
    ;; $B%_%K%P%C%U%!$+$iBP>]$r;XDj$7$?>l9g$K$O(B consp $B$K$J$i$J$$(B
    (when (consp cand)
      (setq cand (car cand)))
    (setq word (car (skk-treat-strip-note-from-word cand)))
    (when (and (string-match "[0-9]" skk-henkan-key)
               (or (string-match "#[0-9]" word)
                   (skk-lisp-prog-p word)))
      (setq skk-henkan-key
            (skk-num-compute-henkan-key skk-henkan-key)))
    (setq skk-annotation-target-data
          (list skk-henkan-key
                skk-okuri-char
                cand))
    ;; $B0U?^$rM}2r$7$F$J$$$,!"(Bskk-kakutei-initialize $B$N$[$&$,E,@Z$J5$$b(B
    (skk-kakutei)))

;;;###autoload
(defun skk-annotation-add (&optional no-previous-annotation)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$k!#(B
$B4{$K(B annotation $B$,IU$1$i$l$F$$$l$P!"$=$l$rJT=8%P%C%U%!$K=PNO$9$k!#(B
NO-PREVIOUS-ANNOTATION $B$r;XDj(B (\\[Universal-Argument] \\[skk-annotation-add])
$B$9$k$H!"4{$KIU$1$i$l$F$$$k(B annotation $B$rJT=8%P%C%U%!$K=PNO$7$J$$!#(B"
  (interactive "P")
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (let* ((plist (append
                   '(intangible t read-only t)
                   '(front-sticky t rear-nonsticky t)))
           (wholestring (nth 2 skk-annotation-target-data))
           (realword (if (and wholestring
                              (string-match ";\\*?" wholestring))
                         (substring wholestring 0 (match-beginning 0))
                       wholestring))
           (annotation (if (and realword
                                (string-match ";\\*?" wholestring))
                           (substring wholestring (match-end 0))
                         nil)))
      (setq skk-annotation-original-window-configuration
            (current-window-configuration))
      (pop-to-buffer skk-annotation-buffer)

      (setq buffer-read-only nil
            skk-annotation-mode t)
      (skk-annotation-erase-buffer)
      (insert
       (format "\
;; Add a note to word `%s' (this line will not be added to the note.)
"
               realword))
      (add-text-properties (point-min) (1- (point)) plist)
      (when (and (not no-previous-annotation)
                 annotation)
        (insert annotation))
      (run-hooks 'skk-annotation-mode-hook)
      (message "%s to save edits, %s to just kill this buffer"
               (mapconcat 'key-description
                          (where-is-internal 'skk-annotation-save-and-quit
                                             skk-annotation-mode-map)
                          ", ")

               (mapconcat 'key-description
                          (where-is-internal 'skk-annotation-kill
                                             skk-annotation-mode-map)
                          ", ")))))

(defun skk-annotation-save-and-quit (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K(B annotation $B$rIU$1$F(B annotation $B%P%C%U%!$rJD$8$k!#(B"
  ;; called in the annotation buffer.
  (interactive "P")
  (let (annotation)
    (save-match-data
      (with-current-buffer (get-buffer-create skk-annotation-buffer)
        (goto-char (point-min))
        (when (looking-at ";; Add a note to word") ; $BCfESH>C<(B
          (forward-line 1)
          (beginning-of-line))
        (setq annotation (buffer-substring-no-properties
                          (point) (point-max)))
        (when (string-match "^[\t\n $B!!(B]+" annotation)
          (setq annotation (substring annotation (match-end 0))))
        (when (string-match "[\t\n $B!!(B]+$" annotation)
          (setq annotation (substring annotation 0 (match-beginning 0))))
        (when (string= annotation "")
          (setq annotation nil))
        (setq annotation (skk-quote-char annotation))))
    (if annotation
        (skk-annotation-last-word-1
         (lambda (beg end)
           (goto-char beg)
           (when (re-search-forward ";[^/]*" end t)
             (delete-region (match-beginning 0) (match-end 0)))
           (goto-char end)
           (insert ";*" annotation)))
      ;; $B:o=|$7$?;~(B
      (let ((old-annotation
             (cdr (skk-treat-strip-note-from-word
                   (nth 2 skk-annotation-target-data)))))
        (when (and old-annotation
                   (yes-or-no-p
                    (format (if skk-japanese-message-and-error
                                "$B4{B8$N%"%N%F!<%7%g%s(B `%s' $B$r:o=|$7$^$9$+!)(B "
                              "Delete old annotation `%s' ? ")
                            (skk-annotation-get old-annotation))))
          (skk-annotation-last-word-1
           (lambda (beg end)
             (goto-char beg)
             (when (re-search-forward ";[^/]*" end t)
               (delete-region (match-beginning 0) (match-end 0))))))))
    (skk-annotation-erase-buffer)
    (kill-buffer (current-buffer))
    (set-window-configuration
     skk-annotation-original-window-configuration)
    (when annotation
      (unless quiet
        (message "%s" "Added annotation")))))

(defun skk-annotation-kill ()
  "annotation $B$rIU$1$:$K(B annotation $B%P%C%U%!$r(B kill $B$9$k!#(B"
  ;; called in the annotation buffer.
  (interactive)
  (skk-annotation-erase-buffer)
  (kill-buffer (current-buffer))
  (set-window-configuration
   skk-annotation-original-window-configuration))

;;;###autoload
(defun skk-annotation-remove ()
  "$B:G8e$K3NDj$7$?8l$+$i(B annotation $B$r<h$j5n$k!#(B"
  (interactive)
  (save-match-data
    (skk-kakutei)
    (skk-annotation-setup)
    (when (yes-or-no-p
           (format (if skk-japanese-message-and-error
                       "%s $B$K$D$$$F$N%"%N%F!<%7%g%s$r:o=|$7$^$9$+!)(B "
                     "Really delete annotation for %s? ")
                   (nth 2 skk-annotation-target-data)))
      (skk-annotation-last-word-1
       (lambda (beg end)
         (goto-char beg)
         (when (re-search-forward ";[^/]*" end t)
           (delete-region (match-beginning 0) (match-end 0))))))))

(defun skk-annotation-last-word-1 (function)
  ;; funcall FUNCTION with BEG and END where BEG and END are markers.
  (let ((inhibit-quit t)
        (jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
        (word (nth 2 skk-annotation-target-data))
        (beg (make-marker))
        (end (make-marker))
        (eol (make-marker))
        pattern)
    (when (buffer-live-p jisyo-buffer)
      (save-match-data
        (with-current-buffer jisyo-buffer
          (goto-char (if (nth 1 skk-annotation-target-data)
                         skk-okuri-ari-min
                       skk-okuri-nasi-min))
          (when (re-search-forward
                 (concat "^\\("
                         (regexp-quote (car skk-annotation-target-data))
                         "\\) /")
                 (if (nth 1 skk-annotation-target-data)
                     skk-okuri-ari-max nil)
                 t nil)
            (goto-char (match-beginning 1))
            (set-marker eol (line-end-position))
            (when (string-match ";" word)
              (setq word (substring word 0 (match-beginning 0))))
            (when (re-search-forward
                   (concat "/\\(" word "\\)\\(;[^/]*\\)*/")
                   eol t nil)
              (set-marker beg (match-beginning 1))
              (set-marker end (or (match-end 2) (match-end 1)))
              (funcall function beg end)
              (when (nth 1 skk-annotation-target-data)
                (goto-char end)
                ;; skip other candidates that has not a okuirigana.
                (search-forward "/[" eol t nil)
                (setq pattern (concat "/\\(" word "\\)\\(;[^/]*\\)*/"))
                (while (re-search-forward pattern eol t nil)
                  (set-marker beg (match-beginning 1))
                  (set-marker end (or (match-end 2)
                                      (match-end 1)))
                  (funcall function beg end)))
              (set-marker beg nil)
              (set-marker end nil)
              (set-marker eol nil))))))))

;;;###autoload
(defun skk-annotation-quote (&optional quiet)
  "$B:G8e$K3NDj$7$?8l$K4^$^$l$k(B `;' $B$r8uJd$N0lIt$H$7$F(B quote $B$9$k!#(B"
  (interactive "P")
  (skk-kakutei)
  (skk-annotation-setup)
  (let (candidate)
    (skk-annotation-last-word-1
     (lambda (beg end)
       (goto-char beg)
       (setq candidate (buffer-substring-no-properties beg end))
       (when (string-match ";" candidate)
         (delete-region beg end)
         (insert (skk-quote-semicolon candidate))
         (unless quiet
           (message "%s" "Quoted")))))))

;;; $B%"%N%F!<%7%g%s(B UI $B3HD%5!G=(B
(defun skk-annotation-wait-for-input (annotation notes &optional word sources)
  "$B%"%N%F!<%7%g%sI=<(;~$K%-!<F~NO$rJaB*$9$k!#(B
$B%-!<F~NO$NFbMF$K$h$C$F%"%N%F!<%7%g%s$N%3%T!<!">pJs8;(B URL $B$N%V%i%&%:!"$^$?$O(B
$BJL$N>pJs8;$+$i$N0UL#<hF@$r9T$&!#(B"
  (let* ((copy-command (key-binding skk-annotation-copy-key))
;;;  (browse-command (key-binding skk-annotation-browse-key))
         ;; * skk-kakutei-key $B$,(B $BI8=`(B C-j $B$G$"$l$P!"(Bbrowse-command $B$O(B C-o $B$N(B open-line() $B$H$J$k!#(B
         ;; * skk-kakutei-key $B$,(B skk-annotation-browse-key $B$H>WFM$9$k(B C-o $B$G$"$l$P!"(B
         ;;   browse-command $B$O(B skk-insert() $B$H$J$k!#(B SPC $B$b(B skk-insert() $B$G$"$k$?$a!"(B
         ;;   $B7k2L$H$7$F(B SPC $B$NBG80$G(B browse-command $B$H$J$C$F$7$^$&!#(B
         ;; * skk-kakutei-key $B$,(B skk-insert() $B$J$N$O(B skk-compile-rule-list() $B;2>H$N$3$H!#(B
         (it (key-binding skk-annotation-browse-key))     ; Fix #58
         (browse-command (if (eq 'skk-insert it) nil it)) ; Fix #58
         (list (list copy-command browse-command))
         event key command urls note cache char digit exit)
    (while (and (not exit)
                list
                (or (memq this-command '(skk-annotation-wikipedia-region-or-at-point
                                         skk-annotation-lookup-region-or-at-point))
                    (eq skk-henkan-mode 'active))
                (if digit
                    t
                  (skk-annotation-message (if (and annotation
                                                   (> (length annotation) 0))
                                              'annotation
                                            nil)))
                (condition-case nil
                    (progn
                      (setq event (read-event)
                            key (skk-event-key event)
                            command (key-binding key))
                      ;; Return value of the following expression is important.
                      (or (memq command list)
                          (eq command 'digit-argument)
                          (memq command '(skk-annotation-wikipedia-region-or-at-point
                                          skk-annotation-lookup-region-or-at-point))
                          (equal (key-description key)
                                 (key-description skk-annotation-wikipedia-key))))
                  (quit
                   nil)))
      (cond ((eq command copy-command)
             (setq list (delq copy-command list))
             (unless (equal annotation "")
               (kill-new (substring-no-properties annotation))
               (skk-message "$B8=:_$NCm<a$r%3%T!<$7$^$7$?(B"
                            "Copying the current note...done")
               (setq event nil
                     digit nil
                     char  nil)
               (skk-annotation-show-2 annotation)))
            ((eq command browse-command)
             (setq list (delq browse-command list))
             (setq urls nil)
             (when (and word (setq cache (skkannot-cache word sources)))
               (let ((url (cond
                           ((string= (cdr cache) "dict")
                            (format "dict:///%s" word))
                           ((string= (cdr cache) "lookup.el")
                            (list 'lookup-word word))
                           (t
                            (apply #'skkannot-generate-url
                                   "http://%s.org/wiki/%s"
                                   (cdr (split-string (cdr cache) " " t)))))))
                 (when url
                   (setq urls (cons url urls)))))
             (unless (equal annotation "")
               (cond (urls
                      (dolist (url urls)
                        (cond ((consp url)
                               (setq exit t)
                               (apply (car url) (cdr url)))
                              (t
                               (browse-url url))))
                      (skk-message "$BCm<a$N%=!<%9$r%V%i%&%:$7$F$$$^$9(B..."
                                   "Browsing originals for the current notes..."))
                     (t
                      (skk-message "$BCm<a$N%=!<%9$,8+$D$+$j$^$;$s(B"
                                   "No originals found for the current notes")))
               (setq event nil
                     digit nil
                     char  nil)
               (unless exit
                 (skk-annotation-show-2 annotation))))
            ((eq command 'digit-argument)
             (setq char (cond ((integerp event) event)
                              (t (get event 'ascii-character)))
                   digit (- (logand char ?\177) ?0)
                   event nil))
            ((or (equal (key-description key)
                        (key-description skk-annotation-wikipedia-key))
                 (memq command '(skk-annotation-wikipedia-region-or-at-point
                                 skk-annotation-lookup-region-or-at-point)))
             (setq sources (if (and digit
                                    (> digit 0)
                                    (<= digit
                                        (length skk-annotation-other-sources)))
                               (list (nth (1- digit)
                                          skk-annotation-other-sources))
                             skk-annotation-other-sources))
             (setq event nil
                   digit nil
                   char  nil)
             (when word
               (setq note (skk-annotation-wikipedia word sources)))
             (cond ((null note)
                    (setq note annotation))
                   (t
                    (setq annotation note)))
             (unless (equal note "")
               (add-to-list 'list browse-command)
               (add-to-list 'list copy-command)
               (skk-annotation-show-2 (or note annotation))))
            (t
             (setq list nil))))
    (when event
      (skk-unread-event event))))

;;;###autoload
(defun skk-annotation-message (&optional situation)
  (when (and skk-verbose (skkannot-in-normal-buffer-p))
    (unless skk-annotation-wikipedia-message
      (let ((key (key-description skk-annotation-wikipedia-key))
            (string "")
            list new)
        (when (equal key "TAB")
          (setq key "C-i"))
        (setq list
              (delete ""
                      (split-string
                       (dotimes (i (length skk-annotation-other-sources) string)
                         (setq string
                               (format "%s[C-%d %s]%s  "
                                       string
                                       (1+ i)
                                       key
                                       (nth i skk-annotation-other-sources))))
                       "  ")))
        (dolist (x list)
          (let* ((y (split-string x "]"))
                 (s1 (car y))
                 (s2 (nth 1 y)))
            (setq new (concat new
                              (propertize (concat s1 "]") 'face
                                          'skk-verbose-kbd-face)
                              s2 " "))))
        (setq skk-annotation-wikipedia-message
              (concat (propertize "{$B$I$l$r;2>H(B?}" 'face
                                  'skk-verbose-intention-face)
                      new))))
    ;;
    (unless skk-annotation-message
      (let ((key-copy (or (key-description skk-annotation-copy-key)
                          "$BL$Dj5A(B"))
            (key-wiki (or (key-description skk-annotation-wikipedia-key)
                          "$BL$Dj5A(B"))
            (key-browse (or (key-description skk-annotation-browse-key)
                            "$BL$Dj5A(B"))
            list new)
        (when (equal key-wiki "TAB")
          (setq key-wiki "C-i"))
        (setq list
              (split-string
               (format "[%s]$B%3%T!<(B  [%s]$B%V%i%&%:(B  [%s]$B%G%U%)%k%H$N%=!<%9$r;2>H(B"
                       key-copy key-browse key-wiki) "  "))
        (dolist (x list)
          (let* ((y (split-string x "]"))
                 (s1 (car y))
                 (s2 (nth 1 y)))
            (setq new (concat new
                              (propertize (concat s1 "]") 'face
                                          'skk-verbose-kbd-face)
                              s2 " "))))
        (setq skk-annotation-message
              (concat (propertize "{$B%"%N%F!<%7%g%s(B}" 'face
                                  'skk-verbose-intention-face)
                      new))))
    ;;
    (condition-case nil
        (cond ((eq situation 'annotation)
               (if (sit-for skk-verbose-wait)
                   (let ((i 0))
                     (catch 'loop
                       (while (< i 20)
                         (message "%s" skk-annotation-message)
                         (unless (sit-for skk-verbose-message-interval)
                           (throw 'loop nil))
                         (message "%s" skk-annotation-wikipedia-message)
                         (unless (sit-for skk-verbose-message-interval)
                           (throw 'loop nil))
                         (setq i (1+ i))))
                     (message nil))
                 nil))
              ;;
              (t
               (when (sit-for skk-verbose-wait)
                 (message "%s" skk-annotation-wikipedia-message))))
      (quit
       (when (eq skk-henkan-mode 'active)
         (skk-reset-henkan-count 0)))))
  ;; $B>o$K(B t $B$rJV$9(B
  t)

;;;###autoload
(defun skk-annotation-lookup-region-or-at-point (&optional prefix-arg
                                                           start end)
  "$BA*BrNN0h$^$?$O%]%$%s%H0LCV$NC18l$r<-=q$GD4$Y$k!#(B
$B<-=q$H$7$F$O(B lookup.el$B!"(BApple OS X $B$N<-=q%5!<%S%9!"(BWikipedia/Wikitionary $B$J$I$,(B
$BMxMQ$5$l$k!#(B

$BNN0h$,A*Br$5$l$F$$$J$1$l$PC18l$N;O$a$H=*$o$j$r?dB,$7$FD4$Y$k!#(B

$BD4$Y$?7k2L$r(B `skk-annotation-show-as-message' $B$,(B Non-nil $B$G$"$l$P%(%3!<%(%j%"(B
$B$K!"(Bnil $B$G$"$l$PJL(B window $B$KI=<($9$k!#(B"
  (interactive (cons (prefix-numeric-value current-prefix-arg)
                     (cond
                      ((use-region-p)
                       (list (region-beginning) (region-end)))
                      ((eq skk-henkan-mode 'on)
                       (list (marker-position skk-henkan-start-point)
                             (point)))
                      (t
                       ;; dummy
                       (list 1 1)))))
  (skkannot-check-lookup)
  (skkannot-clear-msg)
  ;;
  (let ((word (if (and (= start 1) (= end 1))
                  ;; region $B$,(B active $B$G$J$$$H$-$O!$%]%$%s%H$K$"$k(B
                  ;; $BC18l$r?dB,$9$k(B
                  (thing-at-point 'word)
                (buffer-substring-no-properties start end)))
        (sources
         (if (and current-prefix-arg
                  (> prefix-arg 0)
                  (<= prefix-arg (length skk-annotation-other-sources)))
             (list (nth (1- prefix-arg) skk-annotation-other-sources))
           skk-annotation-other-sources))
        note)
    (when (and word
               (> (length word) 0))
      (setq note (or (car (skkannot-cache word sources))
                     (skk-annotation-wikipedia word sources)))
      (skk-annotation-show (or note "") word sources))))


;;; Apple OS X $B<-=q%5!<%S%94XO"5!G=(B
(defsubst skkannot-DictServ-command (word)
  (format skkannot-DictServ-cmd-format-str word "%" "%"))

(defsubst skkannot-dict-buffer-format (word)
  "dict $B$NFbMF$r3JG<$9$k%P%C%U%!$N%U%)!<%^%C%H!#(B"
  (format "  *skk dict %s" word))

(defun skkannot-py-send-string (string)
  "Evaluate STRING in inferior Python process."
  (require 'comint)
  (let ((proc (get-buffer-process skkannot-py-buffer)))
    (comint-send-string proc string)
    (unless (string-match "\n\\'" string)
      ;; Make sure the text is properly LF-terminated.
      (comint-send-string proc "\n"))
    (when (string-match "\n[ \t].*\n?\\'" string)
      ;; If the string contains a final indented line, add a second newline so
      ;; as to make sure we terminate the multiline instruction.
      (comint-send-string proc "\n"))))

(eval-and-compile
  (defsubst skkannot-emacs-24_3-or-later ()
    (or (> emacs-major-version 24)
        (and (= emacs-major-version 24)
             (>= emacs-minor-version 3)))))

(defun skkannot-py-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (cond
   ((eval-when-compile (skkannot-emacs-24_3-or-later))
    (with-current-buffer (process-buffer (or proc (python-shell-internal-get-or-create-process)))
      (save-excursion
        (save-match-data
          (re-search-backward (concat python-shell-prompt-regexp " *\\=")
                              nil t)))))
   (t
    (python-check-comint-prompt))))

(defun skkannot-py-send-command (command)
  "Like `skkannot-py-send-string' but resets `compilation-shell-minor-mode'."
  (when (or (skkannot-py-check-comint-prompt (get-buffer-process
                                              skkannot-py-buffer))
            t)
    (with-current-buffer skkannot-py-buffer
      (goto-char (point-max))
      (compilation-forget-errors)
      (skkannot-py-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

;;;###autoload
(defun skk-annotation-start-python (&optional wait)
  "OS X $B$N!V<-=q!W$rMxMQ$9$k$?$a$K(B python $B$r5/F0$9$k!#(B"
  (require 'python)
  (cond
   ((buffer-live-p skkannot-py-buffer)
    skkannot-py-buffer)
   (t
    ;; python + readline $B$G(B UTF-8 $B$NF~NO$r$9$k$?$a$K(B LANG $B$N@_Dj$,I,MW!#(B
    (let* ((env (getenv "LANG"))
           orig-py-buffer)
      (unless (eval-when-compile (skkannot-emacs-24_3-or-later))
        ;; Emacs 24.2 or earlier
        (setq orig-py-buffer (default-value 'python-buffer)))
      (unless (equal env "Ja_JP.UTF-8")
        (setenv "LANG" "ja_JP.UTF-8"))
      (save-window-excursion
        (run-python skk-annotation-python-program t t))
      (when (eval-when-compile (skkannot-emacs-24_3-or-later))
        ;; Emacs 24.3 or later
        (setq python-buffer (get-buffer (format "*%s*" (python-shell-get-process-name t)))
              orig-py-buffer (default-value 'python-buffer)))
      (setenv "LANG" env)
      (with-current-buffer python-buffer
        (rename-buffer "  *skk python")
        (setq-default python-buffer orig-py-buffer)
        (setq python-buffer orig-py-buffer)
        (setq skkannot-py-buffer (current-buffer))

        (font-lock-mode 0)
        (set-buffer-multibyte t)
        (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
        (set-buffer-file-coding-system 'utf-8-emacs)

        ;; Jan 15 2018, lisp/international/mule.el
        ;; *  (set-buffer-process-coding-system): Mark as interactive-only.
        ;; *  in Lisp code use 'set-process-coding-system' instead.
        (set-process-coding-system (get-buffer-process (current-buffer))
                                   'utf-8-unix 'utf-8-unix)

        (skkannot-py-send-command "import DictionaryServices")
        (cond ((and wait (skkannot-sit-for 1.0))
               (setq skkannot-remaining-delay
                     (- skkannot-remaining-delay 1.0)))
              (wait
               (throw '$B<-=q(B nil))
              (t
               nil))
        skkannot-py-buffer)))))

(defun skkannot-DictServ-cache (word truncate)
  "OS X $B$N!V<-=q!W$+$i$3$l$^$G$K<hF@:Q$N%"%N%F!<%7%g%s$rC5$9!#(B"
  (let (success pt)
    (skk-save-point
     (goto-char (point-max))
     (cond
      ((re-search-backward (regexp-quote
                            (format " %s in DictionaryServices" word))
                           nil t)
       (forward-line 1)
       (beginning-of-line)
       (setq pt (point))
       (setq success (re-search-forward "^>>> " nil t))
       (unless success
         (unless (skkannot-sit-for skk-annotation-loop-interval truncate)
           (unless truncate
             (throw '$B<-=q(B nil)))
         (setq success (re-search-forward "^>>> " nil t)))
       (cond
        (success
         (forward-line  -1)
         (end-of-line)
         (when (>= pt (point))
           (forward-line 1)
           (end-of-line))
         (when (eq (point-at-bol) (point-at-eol))
           (forward-line -1)
           (end-of-line))
         (buffer-substring-no-properties pt (point)))
        (t
         nil)))
      (t
       nil)))))

;;;###autoload
(defun skk-annotation-lookup-DictionaryServices (word &optional truncate force)
  "python $B$r2p$7$F(B DictionaryServices $B$rMxMQ$7%"%N%F!<%7%g%s$r<hF@$9$k!#(B
$B%*%W%7%g%s0z?t(B TRUNCATE $B$,(B non-nil $B$N>l9g$O8uJd0lMwMQ$KC;$$%"%N%F!<%7%g%s(B
$B$K9J$j$3$`!#(B"
  (when (or skk-annotation-lookup-DictionaryServices force)
    (skk-annotation-start-python (not truncate))
    (let ((command (skkannot-DictServ-command word))
          (process (get-buffer-process skkannot-py-buffer))
          (loopint skk-annotation-loop-interval)
          output)
      (with-current-buffer skkannot-py-buffer
        (unless (setq output (skkannot-DictServ-cache word truncate))
          (goto-char (point-max))
          (skkannot-py-send-command command)
          (cond ((and (skkannot-sit-for loopint truncate)
                      (not truncate))
                 (setq skkannot-remaining-delay
                       (- skkannot-remaining-delay loopint)))
                (t
                 (unless truncate
                   (throw '$B<-=q(B nil))))
          (accept-process-output process loopint)
          (goto-char (point-max))
          (setq output (or (skkannot-DictServ-cache word truncate)
                           "None")))
        (unless (string-match skkannot-py-none-regexp output)
          (cond
           (truncate
            (with-temp-buffer
              (set-buffer-multibyte t)
              (insert output)
              (goto-char (point-min))
              (when (re-search-forward "[;,.$B!"!#!(!C(B]" nil t)
                (beginning-of-line)
                (buffer-substring (point) (match-beginning 0)))))
           (t
            output)))))))

;;; $B30It%W%m%0%i%`$+$i$N%"%N%F!<%7%g%s<hF@5!G=(B
(defun skkannot-start-dict-process (buffer word)
  "dict $B$N%W%m%;%9$r5/F0$9$k!#(B"
  (let ((process-connection-type nil)
        (word (encode-coding-string word skk-annotation-dict-coding-system))
        process)
    (make-local-variable 'coding-system-for-read)
    (setq coding-system-for-read 'undecided)
    (prog1
        (setq process (apply #'start-process (buffer-name buffer) buffer
                             skk-annotation-dict-program
                             (append skk-annotation-dict-program-arguments
                                     (list word))))
      (set-process-query-on-exit-flag process nil))))

;;;###autoload
(defun skk-annotation-preread-dict (word &optional nowait)
  "dict $B$N%W%m%;%9$r5/F0$9$k!#@hFI$_$N$?$a$KMQ$$$k!#(B"
  (let ((buffer (get-buffer-create (skkannot-dict-buffer-format word)))
        (text "")
        (loopint 0.01))
    (setq skkannot-buffer-origin (current-buffer))
    (with-current-buffer buffer
      (setq text (buffer-string))
      (when (string= text "")
        (unless (get-process (buffer-name buffer))
          (skkannot-start-dict-process buffer word)
          (unless nowait
            (cond ((skkannot-sit-for loopint)
                   (setq skkannot-remaining-delay
                         (- skkannot-remaining-delay loopint)))
                  (t
                   (throw 'dict nil)))))))))

;;;###autoload
(defun skk-annotation-lookup-dict (word &optional truncate)
  "dict $B$N%W%m%;%9$rI,MW$J$i5/F0$7!"7k2L$rD4$Y$k!#(B
$B0UL#$,<hF@$G$-$?>l9g$K$O7k2L$rJ8;zNs$H$7$FJV$9!#(B"
  (let ((buffer (get-buffer-create (skkannot-dict-buffer-format word)))
        (text "")
        (no-user-input t)
        (loopint skk-annotation-loop-interval)
        process)
    (setq skkannot-buffer-origin (current-buffer))
    (with-current-buffer buffer
      (setq text (buffer-string))
      (when (string= text "")
        (unless (setq process (get-process (buffer-name buffer)))
          (setq process (skkannot-start-dict-process buffer word)))
        (cond
         (truncate
          (while (eq (process-status process) 'run)
            (sleep-for loopint))
          (sleep-for loopint))
         (t
          (while (and no-user-input
                      (eq (process-status process) 'run))
            (when (setq no-user-input (skkannot-sit-for loopint truncate))
              (setq skkannot-remaining-delay
                    (- skkannot-remaining-delay loopint))))
          (if no-user-input
              (sleep-for 0.01)
            (throw 'dict nil)))))
      (goto-char (point-max))
      (forward-line -1)
      (cond
       ((looking-at "^Process .+ finished$")
        (cond
         (truncate
          (goto-char (point-min))
          (cond
           ((re-search-forward "[$B!"!#(B]" nil t)
            (beginning-of-line)
            (setq text (buffer-substring (point) (match-beginning 0))))
           (t
            (erase-buffer)
            (insert " ")
            (setq text ""))))
         (t
          (forward-line -1)
          (setq text (buffer-substring (point-min) (point))))))
       (t
        (erase-buffer)
        (insert " ")
        (setq text ""))))
    (unless (string= text "")
      text)))

;;; Wiktionary/Wikipedia $B%"%N%F!<%7%g%s(B
;;;###autoload
(defun skk-annotation-wikipedia (word &optional sources)
  "Wiktionary/Wikipedia $B$N(B WORD $B$KAjEv$9$k5-;v$+$i%"%N%F!<%7%g%s$r<hF@$9$k!#(B"
  (let ((sources (or sources skk-annotation-other-sources))
        source
        words
        (string "")
        (note nil))
    ;; sources $B$K;XDj$5$l$?=gHV$K;2>H$9$k(B
    (if (catch 'skkannot-wikipedia-suspended
          (save-match-data
            (while (and (or (not note) (equal note ""))
                        sources)
              (setq source (car sources))
              (cond
               ((memq source '(dict DictionaryServices
                                    en.wiktionary ja.wiktionary))
                (setq words (list word))
                (when (skk-ascii-char-p (aref word 0))
                  (cond
                   ((skk-lower-case-p (aref word 0))
                    ;; foo -> Foo, FOO
                    (setq words (append words (list (upcase-initials word))))
                    (if (>= (length word) 2)
                        (setq words (append words (list (upcase word))))))
                   ((and (>= (length word) 2)
                         (skk-lower-case-p (aref word 1)))
                    ;; Foo -> foo, FOO
                    (setq words (append words (list (downcase word) (upcase word)))))
                   (t
                    ;; FOO -> foo, Foo
                    (setq words (append words (list (downcase word))))
                    (if (>= (length word) 2)
                        (setq words (append words
                                            (list (upcase-initials word)))))))))
               (t
                (setq words (list (upcase-initials word)))
                (if (>= (length word) 2)
                    (setq words (append words (list (upcase word)))))))
              (while (and (not note) words)
                (setq note (skk-annotation-wikipedia-1 (car words) source t))
                (sleep-for 0.01) ; $B$3$l$,$J$$$H;_$^$k$3$H$"$j(B
                (setq words (cdr words))
                (when (equal note "")
                  (setq note nil)))
              ;;
              (setq string (format (if (string= "" string)
                                       "%s%s"
                                     "%s/%s")
                                   string source))
              (setq sources (cdr sources)))
            (unless note
              (message "%s $B$K9`L\$,$"$j$^$;$s(B" string)))
          nil)
        ;; $B%@%&%s%m!<%I$,CfCG$5$l$?$H$-(B
        (progn
          (message "%s $B$NE>Aw$,CfCG$5$l$^$7$?(B" source)
          nil)
      ;;
      note)))

(defun skkannot-wikipedia-clean-sup (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'underline)
  (save-excursion
    (goto-char p2)
    (insert "^"))
  (html2text-delete-tags p1 p2 (1+ p3) (1+ p4)))

(defun skkannot-wikipedia-clean-sub (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'underline)
  (save-excursion
    (goto-char p2)
    (insert "_"))
  (html2text-delete-tags p1 p2 (1+ p3) (1+ p4)))

(defun skk-annotation-wikipedia-1 (word source &optional preserve-case)
  "Wiktionary/Wikipedia $B$N(B WORD $B$KAjEv$9$k5-;v$r<B:]$K%@%&%s%m!<%I$7$FD4$Y$k!#(B
$B3:Ev%Z!<%8(B (html) $B$r%@%&%s%m!<%I$9$k5!G=$O(B Emacs $B$KIUB0$N(B URL $B%Q%C%1!<%8$K0M(B
$B$k!#(B"
  (cond
   ((eq source 'lookup.el)
    (skk-lookup-get-content word nil))
   ((eq source '$B<-=q(B)
    (catch '$B<-=q(B
      (skk-annotation-lookup-DictionaryServices word nil t)))
   ((eq source 'dict)
    (catch 'dict (skk-annotation-lookup-dict word)))
   (t
    (require 'html2text)
    (require 'url)
    ;;
    (setq word (skkannot-wikipedia-normalize-word word source preserve-case))
    ;;
    (let ((cache-buffer (format "  *skk %s %s" source word))
          ;; html2text $B$,@5$7$/07$($J$$(B tag $B$O0J2<$N%j%9%H$K;XDj$9$k(B
          (html2text-remove-tag-list
           (append '("a" "span" "table" "tr" "td" "h2" "h3" "h4" "h5" "small"
                     "code")
                   html2text-remove-tag-list))
          (html2text-format-tag-list
           (append '(("sup" . skkannot-wikipedia-clean-sup)
                     ("sub" . skkannot-wikipedia-clean-sub))
                   html2text-format-tag-list))
          (url-retrieve-func #'url-retrieve)
          buf buffer)
      (if (get-buffer cache-buffer)
          (with-current-buffer cache-buffer
            (buffer-string))
        ;; $B%-%c%C%7%e$,$J$$>l9g(B
        (setq buffer (funcall url-retrieve-func
                              (skkannot-generate-url
                               "http://%s.org/wiki/%s"
                               source word)
                              #'skkannot-wikipedia-retrieved
                              (list (list source))))
        (while (not buf)
          (setq buf (catch 'skkannot-wikipedia-retrieved
                      (condition-case nil
                          (sleep-for 0.01)
                        ((error quit)
                         (kill-buffer buffer)
                         (throw 'skkannot-wikipedia-suspended
                                source))))))
        (when (and (setq buffer buf)
                   (buffer-live-p buffer))
          (skkannot-wikipedia-format-buffer source buffer
                                            cache-buffer)))))))

(defun skkannot-wikt-preferred-lang-regexp (lang)
  (let ((head "<h2>.*<span class=\"mw-headline\".+>\\(<a href=.+>\\)?\\(")
        (tail "\\)\\(</a>\\)?</span></h2>"))
    (concat head lang tail)))

(defun skkannot-wikt-find-preferred-langs (wiktlang)
  (let ((langs (cdr (assoc wiktlang
                           skk-annotation-wiktionary-preferred-lang-alist)))
        (pt (point))
        regexp)
    (catch 'found
      (dolist (lang langs)
        (setq regexp (skkannot-wikt-preferred-lang-regexp lang))
        (if (re-search-forward regexp nil t)
            (throw 'found t)
          (goto-char pt))))))

(defun skkannot-wikipedia-format-buffer (source buffer cache-buffer)
  "html $B$NM>7W$JMWAG$r=|5n$7!"(Bhtml2text $B$N5!G=$rMQ$$$F@07A$9$k!#(B"
  (let ((html2text-remove-tag-list
         (append '("a" "span" "table" "tr" "td" "h2" "h3" "h4" "h5" "small"
                   "code")
                 html2text-remove-tag-list))
        (html2text-format-tag-list
         (append '(("sup" . skkannot-wikipedia-clean-sup)
                   ("sub" . skkannot-wikipedia-clean-sub))
                 html2text-format-tag-list))
        note aimai continue nop point top pt1 pt2 btag etag end)
    (with-current-buffer buffer
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (when (> (buffer-size) 0)
        (when (get-buffer cache-buffer)
          (kill-buffer cache-buffer))
        (rename-buffer cache-buffer)
        ;; $BMW$i$J$$ItJ,$r>C$9(B
        (cond
         ;; ja.wiktionary
         ((eq source 'ja.wiktionary)
          (goto-char (point-min))
          (if (save-excursion
                (re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
                                   nil t))
              ;; $B9`L\$,$J$$>l9g(B
              (erase-buffer)
            (search-forward "<!-- start content -->" nil t)
            (delete-region (point-min) (point))
            ;;
            (goto-char (point-min))
            (when (or (skkannot-wikt-find-preferred-langs "ja")
                      (re-search-forward
                       skkannot-ja-wiktionary-lang-regexp
                       nil t))
              (save-excursion
                (goto-char (match-end 2))
                (insert ", "))
              (delete-region (point-min) (match-beginning 0))
              (setq top (point))
              (when (re-search-forward skkannot-ja-wiktionary-lang-regexp
                                       nil t)
                (delete-region (setq pt1 (match-beginning 0)) (point-max))))
            ;;
            (setq point top)
            (goto-char (point-min))
            ;; ja.wiktionary $B$N=q<0$,(B en.wiktionary $B$[$I@0$C$F$$$J$$$N$G(B
            ;; workaround
            (unless
                (save-excursion
                  (re-search-forward
                   skkannot-ja-wiktionary-part-of-speech-regexp
                   nil t))
              (setq point pt1))
            ;;
            (while (re-search-forward
                    skkannot-ja-wiktionary-part-of-speech-regexp
                    nil t)
              (setq nop t)
              (save-match-data
                (when (looking-at "</h3>")
                  (delete-region (match-beginning 0) (match-end 0))))
              (goto-char (match-beginning 0))
              (delete-region (or point (point-min)) (point))
              (cond
               ((re-search-forward "<\\(ol\\|dl\\)>" nil t)
                (setq btag (match-string 0)
                      etag (if (string= btag "<ol>")
                               "</ol>"
                             "</dl>")
                      point nil
                      pt1 (point)
                      pt2 nil)
                (while (and (not point)
                            (search-forward etag nil t))
                  (setq pt2 (point))
                  (goto-char pt1)
                  (if (and (search-forward btag nil t)
                           (< (point) pt2))
                      (progn
                        (goto-char pt2)
                        (setq pt1 (point)))
                    (setq point pt2)
                    (goto-char point))))
               (t
                (goto-char (match-end 0))
                (when (search-forward "</p>" nil t)
                  (setq point (point))))))
            ;;
            (when point
              (delete-region point (point-max)))
            ;; ($BMQNc$J$I$r=|$/(B -- $B=|$+$J$$$[$&$,$$$$!)(B)
            ;; ja.wiktionary $B$O(B en.wiktionary $B$HA4$/E}0l$5$l$?=q$-J}$K$O(B
            ;; $B$J$C$F$$$J$$$N$G!"(Bul $B$r=|$/$H>pJs$,$[$H$s$I;D$i$J$$>l9g$,(B
            ;; $B$"$k(B
            (skkannot-wikipedia-remove-nested "<ul>" "</ul>")
            (skkannot-wikipedia-remove-nested "<dl>" "</dl>")
            (skkannot-wikipedia-remove-nested "<table[^<]*>"
                                              "</table>")
            (skkannot-wikipedia-remove-nested "\
<div class=\"\\(infl-table\\|thumb.+\\)\"[^<]*>" "</div>" "<div[^<]*>")
            ;;
            (goto-char (point-min))
            (while (re-search-forward
                    "<span.*>\\[<a.+>$BJT=8(B</a>\\]</span>"
                    nil t)
              (replace-match ""))))
         ;; en.wiktionary
         ((eq source 'en.wiktionary)
          (goto-char (point-min))
          (if (save-excursion
                (re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
                                   nil t))
              ;; $B9`L\$,$J$$>l9g(B
              (erase-buffer)
            (search-forward "<!-- start content -->" nil t)
            (delete-region (point-min) (point))
            ;;
            (goto-char (point-min))
            (when (or (skkannot-wikt-find-preferred-langs "en")
                      (re-search-forward
                       skkannot-en-wiktionary-lang-regexp
                       nil t))
              (save-excursion
                (goto-char (match-end 2))
                (insert ", "))
              (delete-region (point-min) (match-beginning 0))
              (setq top (point))
              (when (re-search-forward
                     skkannot-en-wiktionary-lang-regexp
                     nil t)
                (delete-region (match-beginning 0) (point-max))))
            ;;
            (setq point top)
            (goto-char (point-min))
            (while (re-search-forward
                    skkannot-en-wiktionary-part-of-speech-regexp
                    nil t)
              (setq nop t)
              (save-match-data
                (when (looking-at "</h3>")
                  (delete-region (match-beginning 0) (match-end 0))))
              (goto-char (match-beginning 0))
              (delete-region (or point (point-min)) (point))
              (cond
               ((re-search-forward "<\\(ol\\|dl\\)>" nil t)
                (setq btag (match-string 0)
                      etag (if (string= btag "<ol>")
                               "</ol>"
                             "</dl>")
                      point nil
                      pt1 (point)
                      pt2 nil)
                (while (and (not point)
                            (search-forward etag nil t))
                  (setq pt2 (point))
                  (goto-char pt1)
                  (if (and (search-forward btag nil t)
                           (< (point) pt2))
                      (progn
                        (goto-char pt2)
                        (setq pt1 (point)))
                    (setq point pt2)
                    (goto-char point))))
               (t
                (goto-char (match-end 0))
                (when (search-forward "</p>" nil t)
                  (setq point (point))))))
            ;;
            (when point
              (delete-region point (point-max)))
            ;; ($BMQNc$J$I$r=|$/(B -- $B=|$+$J$$$[$&$,$$$$!)(B)
            (skkannot-wikipedia-remove-nested "<ul>" "</ul>")
            (skkannot-wikipedia-remove-nested "<dl>" "</dl>")
            (skkannot-wikipedia-remove-nested "<table[^<]*>"
                                              "</table>")
            (skkannot-wikipedia-remove-nested "\
<div class=\"\\(infl-table\\|thumb.+\\)\"[^<]*>" "</div>" "<div[^<]*>")
            (skkannot-wikipedia-remove-nested "\
<span class=\"interProject\">" "</span>")
            ;; Wikipedia $B$X$N0FFb$r=|$/(B
            (goto-char (point-min))
            (while (re-search-forward "\
\\(<a href=\"/wiki/Wikipedia\" title=\"Wikipedia\">\\)?\
Wikipedia\\(</a>\\)? has an article on:$" nil t)
              (save-excursion
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (setq point (point)))
              (forward-line 2)
              (end-of-line)
              (delete-region point (point)))
            ;;
            (goto-char (point-min))
            (while (re-search-forward "\
<span.*>\\(\\[<a.+>edit</a>\\]\\|Inflection\\)</span>"
                                      nil t)
              (replace-match ""))))
         ;; wikipedia
         ((memq source '(ja.wikipedia simple.wikipedia en.wikipedia))
          (goto-char (point-min))
          (if (save-excursion
                (re-search-forward "\
\\(^HTTP/1\\.0 301 Moved Permanently\\|<div class=\"noarticletext\">\
\\|:Badtitle\\)"
                                   nil t))
              ;; $B9`L\$,$J$$>l9g(B
              (erase-buffer)
            (setq aimai
                  (save-excursion
                    (re-search-forward "\
wgCategories.+\\($B[#Kf$52sHr(B\\|[Dd]isambiguation\\).+$" nil t)))
            ;; <span> $B$r=|5n$9$k(B
            (setq point nil)
            (goto-char (point-min))
            (while (re-search-forward "\
<span class=\"\\(.+audiolink.+\\|editsection\\)\".*>" nil t)
              (setq point (match-beginning 0))
              (goto-char point)
              (search-forward "</span>" nil t)
              (delete-region point (point))
              (goto-char point))
            ;; <big> $B$r=|5n$9$k(B
            (goto-char (point-min))
            (while (re-search-forward "<p><big>.+</big></p>" nil t)
              (replace-match ""))
            ;; &#160; $B$r=hM}(B
            (goto-char (point-min))
            (while (re-search-forward "&#160;" nil t)
              (replace-match " "))
            ;; <br /> $B$r=|5n$9$k(B
            (goto-char (point-min))
            (while (re-search-forward "<p>.+\\(<br />\\)$" nil t)
              (replace-match "" nil nil nil 1))
            ;; xxx > xxx > xxx ... $B$r=|5n$9$k(B
            (goto-char (point-min))
            (while (re-search-forward
                    "<p>.+</a> &gt; \\(<a.+>\\|<b>\\).+</p>" nil t)
              (replace-match ""))
            ;; <script> $B$r=|5n(B
            (skkannot-wikipedia-remove-nested "<script.*>" "</script>")
            ;; <table> $B$r=|5n(B
            (skkannot-wikipedia-remove-nested "<table.*>" "</table>")
            ;;
            (goto-char (point-min))
            (when (or (when (re-search-forward
                             "<p>\\(<br />\n\\|[^\n]*\\)?<b>[^\n]+</b>[^\n]+"
                             nil t)
                        (goto-char (match-beginning 0))
                        (if (and (save-excursion
                                   (re-search-forward "</p>" nil t))
                                 (string-match
                                  (cond
                                   ((eq source 'ja.wikipedia)
                                    "$B!#(B\\|$B!%(B")
                                   (t
                                    "\\."))
                                  (buffer-substring (point)
                                                    (match-beginning 0))))
                            t
                          (setq point (point)
                                continue t)
                          nil))
                      (when (progn
                              (goto-char (point-min))
                              (re-search-forward "<\\(u\\|o\\)l>" nil t))
                        (goto-char (if continue
                                       point
                                     (match-beginning 0)))
                        (setq nop t)))
              (delete-region (point-min) (point))
              (goto-char (point-min))
              ;;
              (cond
               ((or aimai nop)
                (setq pt1 (if (re-search-forward "<\\(u\\|o\\)l>" nil t)
                              (match-end 0)
                            nil)
                      pt2 nil)
                (while (and (not end)
                            (re-search-forward "</\\(u\\|o\\)l>"
                                               nil t))
                  (setq pt2 (match-end 0))
                  (save-excursion
                    (goto-char (or pt1 (1+ (point-min))))
                    (when (re-search-forward "<\\(u\\|o\\)l>"
                                             nil t)
                      (setq pt1 (match-end 0))))
                  (when (or (null pt1)
                            (> pt1 pt2))
                    (setq end t))))
               (t
                (re-search-forward "</p>" nil t)))
              (delete-region (point) (point-max))))))
        ;;
        (setq point nil)
        (when (> (buffer-size) 0)
          (html2text)
          (goto-char (point-min))
          (cond
           ((memq source '(ja.wiktionary en.wiktionary))
            ;; wiktionary $B$N@07A7k2L$O6u9T$@$i$1$K$J$k(B...
            (goto-char (point-min))
            (while (re-search-forward "\n[\n]+" nil t)
              (replace-match "\n"))
            (goto-char (point-min))
            (while (not (eobp))
              (beginning-of-line)
              (setq point (point))
              (forward-line 1)
              (fill-region point (point))))
           (t
            (while (looking-at "^[ \t]*$")
              (kill-line 1))
            (cond ((or aimai nop)
                   (while (not (eobp))
                     (beginning-of-line)
                     (setq point (point))
                     (forward-line 1)
                     (fill-region point (point))))
                  (t
                   (fill-paragraph nil)))))
          ;;
          (when aimai
            (insert (if (eq source 'ja.wikipedia)
                        "\n($B[#Kf$52sHr$N%Z!<%8(B)"
                      "\n(Disambiguation page)")))
          ;;
          (goto-char (point-max))
          (while (and (looking-at "^$")
                      (not (string= "" (buffer-string))))
            (delete-char -1))
          ;;
          (when (and (not (equal (buffer-string) ""))
                     (not (get-text-property 1 'face)))
            (put-text-property 1 2 'face 'default))
          (setq note (buffer-string)))))
    ;;
    (cond ((stringp note)
           (if (equal note "")
               nil
             note))
          (t
           nil))))

(defun skkannot-wikipedia-remove-nested (btag etag &optional ibtag)
  "<dl> <ul> <table> $B$J$I$NF~$l;R9=B$$r=|5n$9$k!#(B"
  (unless ibtag
    (setq ibtag btag))
  (let (point pt1 pt2 orig-btag)
    (setq point nil)
    (goto-char (point-min))
    (while (re-search-forward btag nil t)
      (setq point (match-beginning 0))
      (goto-char point)
      (cond
       ((not (search-forward etag nil t))
        (delete-region point (match-end 0))
        (goto-char (point-min)))
       (t
        (setq pt2 (match-end 0))
        (goto-char (1+ point))
        (cond
         ((not (re-search-forward ibtag nil t))
          (delete-region point pt2)
          (when orig-btag
            (setq btag      orig-btag
                  orig-btag nil))
          (goto-char (point-min)))
         (t
          (setq pt1 (match-beginning 0))
          (cond
           ((< pt2 pt1)
            (delete-region point pt2)
            (setq point nil)
            (when orig-btag
              (setq btag      orig-btag
                    orig-btag nil))
            (goto-char (point-min)))
           (t
            (unless orig-btag
              (setq orig-btag btag
                    btag      ibtag))
            (goto-char pt1))))))))))

(defun skkannot-wikipedia-retrieved (&rest args)
  (cond ((or (member "deleted\n" (assq 'error (memq :error (car args))))
             (< (buffer-size) 7)
             (not (skkannot-wikipedia-test-html-tag)))
         ;; $BIT40A4$J(B retrieval $B$K$*$$$F$b(B STATUS $B$,(B nil $B$H$J$k$3$H$,$"$k$N$G(B
         ;; $B$3$3$GD4@0$9$k!#(B
         (kill-buffer (current-buffer))
         (ignore-errors
           (throw 'skkannot-wikipedia-suspended (cadr args))))
        (t
         (throw 'skkannot-wikipedia-retrieved (current-buffer)))))

(defun skkannot-wikipedia-test-html-tag ()
  ;; html $B%G!<%?$,:G8e$N(B </html> $B%?%0$r;}$D$3$H$r3NG'$9$k(B
  (goto-char (point-min))
  (when (re-search-forward "^Content-Encoding: gzip$" nil t)
    ;; html $B$,(B gzip $B05=L$GAw$i$l$FMh$?>l9g(B
    (unless (fboundp 'url-handle-content-transfer-encoding)
      ;; Emacs 24.3 or earlier
      (let ((gzip (executable-find "gzip")))
        (unless gzip
          (error "$B$3$NFbMF$rI=<($9$k$K$O(B %s $B$,I,MW$G$9(B" "gzip"))
        (while (and (not (looking-at "^\n"))
                    (not (eobp)))
          (forward-line 1))
        (forward-line 1)
        (when (< (point) (point-max))
          (let ((coding-system-for-write 'binary))
            (call-process-region (point) (point-max) gzip t t t "-cd"))))))
  (goto-char (point-max))
  (search-backward "</html>" nil t))

;;;###autoload
(defalias 'skk-annotation-wikipedia-region-or-at-point
  'skk-annotation-lookup-region-or-at-point)

(defun skkannot-generate-url (format-string &rest args)
  (condition-case nil
      (require 'url-util)
    (error
     (error "%s" "$B?7$7$$(B URL $B%Q%C%1!<%8$,I,MW$G$9(B")))
  (apply #'format format-string
         (mapcar (lambda (element)
                   (if (stringp element)
                       (url-hexify-string element)
                     element))
                 args)))

(defun skkannot-wikipedia-normalize-word (word &optional method preserve-case)
  ;; $B%9%Z!<%9$O(B %20 $B$G$O$J$/!"%"%s%@!<%9%3%"$KJQ49$9$k(B
  (replace-regexp-in-string
   " " "_"
   (cond
    ((memq method '(ja.wiktionary en.wiktionary))
     (if (and (not preserve-case)
              (> (length word) 1)
              (skk-ascii-char-p (aref word 0))
              (skk-lower-case-p (aref word 1)))
         ;; $BFsJ8;z$a$,(B lower case $B$J$i(B downcase
         (downcase word)
       ;; $B0lJ8;z$@$C$?$i85$N(B case
       ;; $BFsJ8;z$a$,(B upper case $B$J$i85$N(B case
       ;; $B1Q8l0J30$OL$BP1~(B
       word))
    ((eq method 'upcase-initials)
     (upcase-initials word))
    (t
     (if (> (length word) 1)
         (concat (vector (upcase (aref word 0)))
                 (substring word 1))
       word)))))

;;; $B3F<o%"%N%F!<%7%g%s!&%=!<%9$N%-%c%C%7%e4IM}(B
;;;###autoload
(defun skkannot-cache (word &optional sources)
  (let ((sources (or sources skk-annotation-other-sources))
        (word (skkannot-wikipedia-normalize-word word 'en.wiktionary))
        (cword (skkannot-wikipedia-normalize-word word))
        (ccword (skkannot-wikipedia-normalize-word word 'upcase-initials)))
    (catch 'found
      (while sources
        (let* ((source (pop sources))
               (ccache-buffer (if (equal word cword)
                                  nil
                                (format "  *skk %s %s" source cword)))
               (cccache-buffer (if (or (equal word ccword)
                                       (equal cword ccword))
                                   nil
                                 (format "  *skk %s %s" source ccword)))
               (cache-buffer (format "  *skk %s %s" source word))
               string)
          (cond
           ((eq source 'lookup.el)
            (setq string (skk-lookup-get-content word))
            (if (or (null string)
                    (string= string ""))
                nil
              (throw 'found
                     (cons string "lookup.el"))))
           ((eq source '$B<-=q(B)
            (setq string
                  (catch '$B<-=q(B
                    (skk-annotation-lookup-DictionaryServices word)))
            (if (or (null string)
                    (string= string ""))
                nil
              (throw 'found
                     (cons string "dict"))))
           ((eq source 'dict)
            (setq string (catch 'dict (skk-annotation-lookup-dict word)))
            (if (or (null string)
                    (string= string ""))
                nil
              (throw 'found
                     (cons string "dict"))))
           (t
            (setq string
                  (if (and ccache-buffer
                           (get-buffer ccache-buffer))
                      ;; Word word
                      (with-current-buffer (get-buffer ccache-buffer)
                        (buffer-string))
                    ""))
            (if (> (length string) 0)
                (throw 'found (cons string ccache-buffer))
              (setq string
                    (if (and cccache-buffer
                             (get-buffer cccache-buffer))
                        ;; Word Word
                        (with-current-buffer (get-buffer cccache-buffer)
                          (buffer-string))
                      ""))
              (if (> (length string) 0)
                  (throw 'found (cons string cccache-buffer))
                (setq string
                      (if (get-buffer cache-buffer)
                          ;; word word
                          (with-current-buffer (get-buffer cache-buffer)
                            (buffer-string))
                        ""))
                (if (string= string "")
                    nil
                  (throw 'found (cons string cache-buffer))))))))))))

(provide 'skk-annotation)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-annotation.el ends here
