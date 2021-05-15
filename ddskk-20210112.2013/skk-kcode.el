;;; skk-kcode.el --- $BJ8;z%3!<%I$r;H$C$?JQ49$N$?$a$N%W%m%0%i%`(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
;;               1998, 1999, 2000
;;   Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1998-2010 SKK Development Team

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
(require 'skk-tankan)

(eval-when-compile
  (require 'cl-lib)
  (defvar enable-recursive-minibuffers)
  (defvar message-log-max))

;;;###autoload
(defun skk-input-by-code-or-menu (&optional arg)
  "$BJQ?t(B `skk-kcode-method' $B$G;XDj$5$l$?5!G=$rMQ$$$FJ8;z$rA^F~$9$k!#(B"
  ;; `skk-rom-kana-base-rule-list' $B$,;X$7$F$$$k$N$O$3$N4X?t!#(B
  (interactive "*P")
  (let (list)
    (cl-case skk-kcode-method
      (char-list
       (skk-list-chars arg))
      (code-or-char-list
       (and (setq list (skk-input-by-code arg))
            (skk-list-chars arg)))
      (this-key
       (insert (format "%s" (this-command-keys))))
      (t
       (and (setq list (skk-input-by-code arg))
            (insert (apply #'skk-input-by-code-or-menu-0 list))))))
  (when (eq skk-henkan-mode 'active)
    (skk-kakutei)))

(defun skk-input-by-code (&optional arg)
  "7/8 bit JIS $B%3!<%I(B $B$b$7$/$O(B $B6hE@HV9f$KBP1~$9$kJ8;z$rA^F~$9$k!#(B"
  (interactive "*P")
  (when arg
    (setq skk-kcode-charset (intern (completing-read
                                     "Character set: "
                                     '(("japanese-jisx0213-1")
                                       ("japanese-jisx0213-2")
                                       ("japanese-jisx0208"))
                                     nil t
                                     (symbol-name skk-kcode-charset)))))
  (let* ((enable-recursive-minibuffers t)
         (codestr (skk-kcode-read-code-string))
         (list (skk-kcode-parse-code-string codestr))
         (flag (nth 0 list))
         (n1 (nth 1 list))
         (n2 (nth 2 list))
         (char (nth 3 list)))
    (cond
     ((> n1 160)
      (insert (skk-kcode-find-char-string flag n1 n2 char))
      nil)
     (t
      (list n1 n2)))))

(defun skk-kcode-read-code-string ()
  (read-string (format (if skk-japanese-message-and-error
                           "\
`%s' $B$NJ8;z$r;XDj$7$^$9!#(B7/8 $B%S%C%H(B JIS $B%3!<%I(B (00nn), $B6hE@%3!<%I(B (00-00),\
 UNICODE (U+00nn), $B$^$?$O(B [RET] (%s): "
                         "\
To find a character in `%s', type 7/8 bits JIS code (00nn),\
 KUTEN code (00-00), UNICODE (U+00nn), or [RET] for %s: ")
                       skk-kcode-charset
                       (if (eq skk-kcode-method 'code-or-char-list)
                           "char-list"
                         "menu"))))

(defun skk-kcode-parse-code-string (str)
  (let* ((list (split-string str "-"))
         (len (length list))
         n1 n2 flag char)
    (cond
     ((eq len 2)
      ;; $B%O%$%U%s(B `-' $B$G6h@Z$i$l$?!V6h(B-$BE@!W(B
      (setq n1 (+ (string-to-number (nth 0 list))
                  32 128)
            n2 (+ (string-to-number (nth 1 list))
                  32 128)))
     ((eq len 3)
      ;; $B%O%$%U%s(B `-' $B$G6h@Z$i$l$?!VLL(B-$B6h(B-$BE@!W(B
      (setq flag (if (equal "2" (nth 0 list))
                     'x0213-2
                   'x0213-1)
            n1 (+ (string-to-number (nth 1 list))
                  32 128)
            n2 (+ (string-to-number (nth 2 list))
                  32 128)))
     ((string-match "^[uU]\\+\\(.*\\)$" str)
      ;; `U+' $B$G;O$^$l$P%f%K%3!<%I(B
      (setq flag 'unicode
            n1 161
            n2 0
            char (string-to-number (match-string-no-properties 1 str) 16)))
     (t
      ;; $B>e5-0J30$O(B JIS $B%3!<%I$H$_$J$9(B
      (setq n1 (if (string= str "")
                   128
                 (+ (* 16 (skk-char-to-hex (aref str 0) 'jis))
                    (skk-char-to-hex (aref str 1))))
            n2 (if (string= str "")
                   128
                 (+ (* 16 (skk-char-to-hex (aref str 2) 'jis))
                    (skk-char-to-hex (aref str 3)))))))
    ;;
    (when (or (> n1 256)
              (> n2 256))
      (skk-error "$BL58z$J%3!<%I$G$9(B"
                 "Invalid code"))
    (list flag n1 n2 char)))

(defun skk-kcode-find-char-string (flag n1 n2 char)
  (cl-case flag
    (x0213-1
     (char-to-string (make-char 'japanese-jisx0213-1 n1 n2)))
    (x0213-2
     (char-to-string (make-char 'japanese-jisx0213-2 n1 n2)))
    (unicode
     (char-to-string
      (if (fboundp 'ucs-representation-decoding-backend)
          (funcall #'ucs-representation-decoding-backend 'ucs char nil)
        char)))
    (t
     (skk-make-string n1 n2))))

(defun skk-char-to-hex (char &optional jischar)
  "CHAR $B$r(B 16 $B?J?t$H$_$J$7$F!"BP1~$9$k?tCM$r(B 10 $B?J?t$GJV$9!#(B"
  (cond
   ;; a(97) -- f(102)
   ((and (<= 97 char) (<= char 102))
    (- char 87))            ; a $B$J$i(B 10 $B$,!"(Bf $B$J$i(B 15 $B$,JV$k!#(B
   ;; A(65) -- F(70)
   ((and (<= 65 char) (<= char 70))
    (- char 55))            ; A $B$J$i(B 10 $B$,!"(BF $B$J$i(B 15 $B$,JV$k!#(B
   ;; 0(48) -- 9(57)
   ((and (<= 48 char) (<= char 57))
    (if jischar
        (- char 40)         ; 0 $B$J$i(B 8 $B$,!"(B9 $B$J$i(B 17 $B$,JV$k!#(B
      (- char 48)))         ; 0 $B$J$i(B 0 $B$,!"(B9 $B$J$i(B 9 $B$,JV$k!#(B
   (t
    (skk-error "`%c' $B$r(B 16 $B?J?t$KJQ49$G$-$^$;$s(B"
               "Cannot convert `%c' to hexadecimal number"
               char))))

(defun skk-make-string (n1 n2)
  "`skk-kcode-charset' $B$,<($9J8;z=89g$K=>$C$F(B n1 n2 $B$KBP1~$9$k(B STRING $B$rJV$9(B"
  (char-to-string (make-char skk-kcode-charset n1 n2)))

(defun skk-next-n2-code (n)
  (if (<= (setq n (1+ n)) skk-code-n2-max)
      n
    skk-code-n2-min))

(defun skk-previous-n2-code (n)
  (if (<= skk-code-n2-min (setq n (1- n)))
      n
    skk-code-n2-max))

(defun skk-next-n1-code (n)
  (if (<= (setq n (1+ n)) skk-code-n1-max)
      n
    skk-code-n1-min))

(defun skk-previous-n1-code (n)
  (if (<= skk-code-n1-min (setq n (1- n)))
      n
    skk-code-n1-max))

(defun skk-input-by-code-or-menu-0 (n1 n2)
  (if (= n1 skk-code-null)
      (skk-input-by-code-or-menu-jump n2)
    (skk-input-by-code-or-menu-1 n1 n2)))

(defun skk-input-by-code-or-menu-jump (n)
  (let ((menu-keys1 (mapcar (lambda (char) ; $BI=<(MQ$N%-!<%j%9%H(B
                              (skk-char-to-unibyte-string (upcase char)))
                            skk-input-by-code-menu-keys1))
        kanji-char)
    (when (< n skk-code-n1-min)
      (setq n skk-input-by-code-or-menu-jump-default))
    (while (not kanji-char)
      (let ((n-org n)
            (chars (list
                    (list (skk-make-string n skk-code-n1-min)
                          n skk-code-n1-min)
                    (list (skk-make-string n 177) n 177)
                    (list (skk-make-string n 193) n 193)
                    (list (skk-make-string n 209) n 209)
                    (list (skk-make-string n 225) n 225)
                    (list (skk-make-string n 241) n 241)
                    (progn
                      (setq n (skk-next-n1-code n))
                      (list (skk-make-string n skk-code-n1-min)
                            n skk-code-n1-min))
                    (list (skk-make-string n 177) n 177)
                    (list (skk-make-string n 193) n 193)
                    (list (skk-make-string n 209) n 209)
                    (list (skk-make-string n 225) n 225)
                    (list (skk-make-string n 241) n 241))))
        (skk-save-point
         (let ((i 0)
               message-log-max str)
           (while (< i 12)
             (setq str (concat
                        str
                        (propertize (nth i menu-keys1) 'face
                                    'skk-henkan-show-candidates-keys-face)
                        ":"
                        (car (nth i chars))
                        (if (and skk-show-tooltip (= i 5))
                            "\n"
                          "  "))
                   i (1+ i)))
           (if skk-show-tooltip
               (funcall skk-tooltip-function str)
             (message "%s" str)))
         ;;
         (let* ((event (read-event))
                (char (event-to-character event))
                (key (skk-event-key event))
                rest ch)
           (cond
            ((skk-key-binding-member key skk-quit-commands skk-j-mode-map)
             (signal 'quit nil))
            ;;
            ((not (characterp char))
             (skk-message "`%s' $B$OL58z$J%-!<$G$9!*(B"
                          "`%s' is not valid here!"
                          (or (key-description key)
                              (key-description char)))
             (sit-for 1)
             (message "")
             (setq n n-org))
            ;;
            ((setq rest (or (memq char skk-input-by-code-menu-keys1)
                            (if (skk-lower-case-p char)
                                (memq (upcase char)
                                      skk-input-by-code-menu-keys1)
                              (memq (downcase char)
                                    skk-input-by-code-menu-keys1)))
                   ch (if rest
                          ;; 12 == (length skk-input-by-code-menu-keys1)
                          (nth (- 12 (length rest)) chars)
                        nil))
             (setq kanji-char ch))
            ;;
            ((or (skk-key-binding-member (skk-char-to-unibyte-string char)
                                         '(skk-previous-candidate))
                 (and (not skk-delete-implies-kakutei)
                      (eq 'skk-delete-backward-char
                          (lookup-key skk-j-mode-map (vector char)))))
             (when (< (setq n (- n-org 2)) skk-code-n1-min)
               (setq n skk-code-n1-max)))
            ;;
            ((eq char skk-start-henkan-char) ; space
             (setq n (skk-next-n1-code n)))
            ;;
            ((eq char ?\?)
             (skk-message
              "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [$B2?$+%-!<$r2!$7$F$/$@$5$$(B]"
              "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [Hit any key to continue]"
              (caar chars)
              n-org skk-code-n1-min
              n-org skk-code-n1-min
              (- n-org 128) (- skk-code-n1-min 128)
              (- n-org 128) (- skk-code-n1-min 128))
             (read-event)
             (setq n n-org))
            ;;
            (t
             (skk-message "`%c' $B$OL58z$J%-!<$G$9!*(B"
                          "`%c' is not valid here!"
                          char)
             (sit-for 1)
             (message "")
             (setq n n-org)
             nil))))))
    (setq skk-input-by-code-or-menu-jump-default
          (cadr kanji-char))
    (skk-input-by-code-or-menu-1 (cadr kanji-char)
                                 (car (cddr kanji-char)))))

(defun skk-input-by-code-or-menu-1 (n1 n2)
  (let ((menu-keys2 (mapcar (lambda (char) ; $BI=<(MQ$N%-!<%j%9%H(B
                              (skk-char-to-unibyte-string (upcase char)))
                            skk-input-by-code-menu-keys2))
        kanji-char)
    (while (not kanji-char)
      (let ((n1-org n1)
            (n2-org n2)
            (i 0)
            (chars (list (skk-make-string n1 n2))))
        ;; 16 == (length skk-input-by-code-menu-keys2)
        (while (< i (1- 16))
          (nconc chars (list (progn
                               (setq n2 (skk-next-n2-code n2))
                               (if (= n2 skk-code-n2-min)
                                   (setq n1 (skk-next-n1-code n1)))
                               (skk-make-string n1 n2))))
          (setq i (1+ i)))
        (skk-save-point
         (let ((i 0)
               message-log-max str)
           (while (< i 16)
             (setq str (concat
                        str
                        (propertize (nth i menu-keys2) 'face
                                    'skk-henkan-show-candidates-keys-face)
                        ":"
                        (nth i chars)
                        (if (and skk-show-tooltip (= i 8))
                            "\n"
                          " "))
                   i (1+ i)))
           (if skk-show-tooltip
               (funcall skk-tooltip-function str)
             (message "%s" str)))
         (let* ((event (read-event))
                (char (event-to-character event))
                (key (skk-event-key event))
                rest ch)
           (cond
            ((skk-key-binding-member key skk-quit-commands skk-j-mode-map)
             (signal 'quit nil))
            ((not (characterp char))
             (skk-message "`%s' $B$OL58z$J%-!<$G$9!*(B" "`%s' is not valid here!"
                          (or (key-description key) (key-description char)))
             (sit-for 1)
             (message "")
             (setq n1 n1-org n2 n2-org))
            ((setq rest (or (memq char skk-input-by-code-menu-keys2)
                            (if (skk-lower-case-p char)
                                (memq (upcase char)
                                      skk-input-by-code-menu-keys2)
                              (memq (downcase char)
                                    skk-input-by-code-menu-keys2)))
                   ch (when rest
                        ;; 16 == (length skk-input-by-code-menu-keys2)
                        (nth (- 16 (length rest)) chars)))
             (setq kanji-char ch))
            ((or (skk-key-binding-member (skk-char-to-unibyte-string char)
                                         '(skk-previous-candidate))
                 (and (not skk-delete-implies-kakutei)
                      (eq 'skk-delete-backward-char
                          (lookup-key skk-j-mode-map(vector char)))))
             (when (< (setq n2 (- n2 31)) skk-code-n2-min)
               (setq n2 (+ n2 94)
                     n1 (skk-previous-n1-code n1))))
            ;;
            ((eq char skk-start-henkan-char) ; space
             (if (= (setq n2 (skk-next-n2-code n2))
                    skk-code-n2-min)
                 (setq n1 (skk-next-n1-code n1))))
            ;;
            ((eq char ?\?)
             (skk-message
              "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [$B2?$+%-!<$r2!$7$F$/$@$5$$(B]"
              "\
`%s' EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d) [Hit any key to continue]"
              (car chars)
              n1-org n2-org
              n1-org n2-org
              (- n1-org 128) (- n2-org 128)
              (- n1-org 128) (- n2-org 128))
             (read-event)
             (setq n1 n1-org n2 n2-org))
            ;;
            ((eq char ?>)
             (if (= (setq n2 (skk-next-n2-code n2-org))
                    skk-code-n2-min)
                 (setq n1 (skk-next-n1-code n1-org))
               (setq n1 n1-org)))
            ;;
            ((eq char ?<)
             (if (= (setq n2 (skk-previous-n2-code n2-org))
                    skk-code-n2-max)
                 (setq n1 (skk-previous-n1-code n1-org))
               (setq n1 n1-org)))
            ;;
            (t
             (skk-message "`%c' $B$OL58z$J%-!<$G$9!*(B"
                          "`%c' is not valid here!"
                          char)
             (sit-for 1)
             (message "")
             (setq n1 n1-org n2 n2-org)))))))
    kanji-char))

;;;###autoload
(defun skk-display-code-for-char-at-point (&optional arg)
  "$B%]%$%s%H$K$"$kJ8;z$N6hE@HV9f!"(BJIS $B%3!<%I!"(BEUC $B%3!<%I!"%7%U%H(B JIS $B%3!<%I(B\
$B5Z$S%f%K%3!<%I$rI=<($9$k!#(B"
  (interactive)
  (cond ((not skk-display-code-method)
         (insert (format "%s" (this-command-keys))))
        ((eobp)
         (skk-message "$B%+!<%=%k$,%P%C%U%!$N=*C<$K$"$j$^$9(B"
                      "Cursor is at the end of the buffer"))
        (t
         (skk-display-code (following-char) (point))))
  t) ; $B%(%3!<$7$?J8;zNs$r%+%l%s%H%P%C%U%!$KA^F~$7$J$$$h$&$K!#(B

(defun skk-display-code (char p)
  (let ((charset (char-charset char skk-charset-list))
        mesg)
    (cond
     ((memq charset '(japanese-jisx0213-1
                      japanese-jisx0213-2
                      japanese-jisx0208
                      japanese-jisx0208-1978))
      (let* ((char1-j (skk-char-octet char 0))
             (char1-k (- char1-j 32))
             (char1-e (+ char1-j 128))
             (char2-j (skk-char-octet char 1))
             (char2-k (- char2-j 32))
             (char2-e (+ char2-j 128))
             (sjis (if (eq charset 'japanese-jisx0213-2)
                       (skk-jis2sjis2 char1-j char2-j)
                     (skk-jis2sjis char1-j char2-j)))
             (char1-s (car sjis))
             (char2-s (cadr sjis))
             (char-data (skk-tankan-get-char-data char))
             (anno (skk-tankan-get-char-annotation char))
             (unicode (concat ", "
                              (propertize "UNICODE:" 'face
                                          'skk-display-code-prompt-face)
                              (format "U+%04x" char)))
             (composition (find-composition p nil nil t)))

        (setq mesg
              (concat (propertize (char-to-string char)
                                  'face 'skk-display-code-char-face)
                      "\t"
                      (propertize "KUTEN:" 'face 'skk-display-code-prompt-face)
                      (if (eq charset 'japanese-jisx0213-2)
                          "(plane2)" "")
                      (format "%02d-%02d, " char1-k char2-k)

                      (propertize "JIS:" 'face 'skk-display-code-prompt-face)
                      (format "#x%2x%2x, " char1-j char2-j)

                      (propertize "EUC:" 'face 'skk-display-code-prompt-face)
                      (format "#x%2x%2x, " char1-e char2-e)

                      (propertize "SJIS:" 'face 'skk-display-code-prompt-face)
                      (format "#x%2x%2x" char1-s char2-s)

                      unicode
                      (if composition
                          (format " (Composed with U+%x)"
                                  (string-to-char
                                   (buffer-substring (1+ p) (nth 1 composition)))))
                      (unless (zerop (nth 2 char-data))
                        (concat ", "
                                (propertize
                                 (format "$BAm(B%d$B2h!J(B%s$BIt(B %d$B2h!K(B"
                                         (nth 2 char-data)
                                         (aref skk-tankan-radical-vector
                                               (nth 0 char-data))
                                         (nth 1 char-data))
                                 'face 'skk-display-code-tankan-radical-face)))
                      (if anno
                          (concat ", "
                                  (propertize
                                   anno
                                   'face
                                   'skk-display-code-tankan-annotation-face)))))))
     ;;
     ((memq charset '(ascii latin-jisx0201))
      (setq mesg
            (concat (format "`%c', " char)
                    (propertize "HEX:" 'face 'skk-display-code-prompt-face)
                    (format "#x%2x, " (skk-char-octet char 0))
                    (propertize "DECIMAL:" 'face 'skk-display-code-prompt-face)
                    (format "%3d" (skk-char-octet char 0)))))
     ;;
     ((eq (char-charset char) 'unicode)
      (setq mesg (concat (propertize (char-to-string char)
                                     'face 'skk-display-code-char-face)
                         "\t"
                         (format "UNICODE: U+%04x" char))))
     ;;
     (t
      (setq mesg (format (if skk-japanese-message-and-error
                             "$BJ8;z=89g(B %s $B$O%5%]!<%H$7$F$$$^$;$s(B"
                           "%s character set is not supported")
                         (char-charset char)))))
    ;;
    (cond
     ((and window-system
           skk-show-tooltip
           (not (eq (symbol-function 'skk-tooltip-show-at-point) 'ignore)))
      (skk-tooltip-show-at-point (replace-regexp-in-string ", " "\n\t" mesg)
                                 'annotation))
     (skk-show-candidates-always-pop-to-buffer
      (skk-annotation-show (replace-regexp-in-string ", " "\n\t" mesg)))
     (t
      (message "%s" mesg)))))

(defun skk-jis2sjis (char1 char2)
  (let* ((ch2 (if (eq (* (/ char1 2) 2) char1)
                  (+ char2 125) (+ char2 31)))
         (c2 (if (>= ch2 127)
                 (+ ch2 1) ch2))
         (ch1 (+ (/ (- char1 33) 2) 129))
         (c1 (if (> ch1 159)
                 (+ ch1 64) ch1)))
    (list c1 c2)))

(defun skk-jis2sjis2 (char1 char2)
  (let* ((ch2 (if (eq (* (/ char1 2) 2) char1)
                  (+ char2 125) (+ char2 31)))
         (c2 (if (>= ch2 127)
                 (+ ch2 1) ch2))
         (ku (- char1 32))
         (c1 (if (<= ku 15)
                 (- (/ (+ ku ?\x1df) 2) (* (/ ku 8) 3))
               (/ (+ ku ?\x19b) 2))))
    (list c1 c2)))

(defun skk-sjis2jis (char1 char2)
  (let* ((ch1 (if (<= char1 159) (+ (* (- char1 113) 2) 1)
                (+ (* (- char1 177) 2) 1)))
         (ch2 (if (> char2 127) (- char2 1) char2))
         (c2 (if (>= ch2 158) (- ch2 125) (- ch2 31)))
         (c1 (if (> ch2 127) (+ ch1 1) ch1)))
    (list c1 c2)))

;;;; skk-list-chars
;; TODO
;;   o mode-line $B$K8=:_(B charset $B$rI=<($7$?$$(B
;;   o $BA^F~@h$N%P%C%U%!(B skk-list-chars-destination-buffer $B$,B8:_$7$J$$>l9g$NBP=h(B
;;   o $B%-!<A`:n(B a, e $B$G6h$N@hF,(B/$BKvHx$X(B
;;   o $B%-!<A`:n(B <, > $B$G@hF,(B/$BKvHx$X(B

(defun skk-list-chars-mode ()
  "Major mode for skk-list-chars.

\\{skk-list-chars-mode-map}"
  (kill-all-local-variables)
  (use-local-map skk-list-chars-mode-map)
  (setq mode-name "skk-list-chars"
        major-mode 'skk-list-chars-mode)
  (setq tab-width 4))

(defun skk-list-chars-sub (high charset)
  "Docstring."
  (let ((min 33)            ;?\x21
        (max 126)           ;?\x7e
        i ch)
    (insert "\n"
            (propertize
             (format "%02d-#x--- 0-- 1-- 2-- 3-- 4-- 5-- 6-- 7-- 8-- 9-- A-- B-- C-- D-- E-- F" (- high 32)) 'face 'skk-list-chars-table-header-face))
    (setq i (* (/ min 16) 16))      ; i $B$O(B $B2<0L%P%$%H(B
    (while (<= i max)           ; 0x21 .. 0x7e
      (when (zerop (% i 16))
        (insert (propertize (format "\n %5X0" (/ (+ (* high 256)
                                                    i)
                                                 16))
                            'face 'skk-list-chars-table-header-face)))
      (setq ch (if (< i min)
                   32
                 (or (make-char charset (/ (* high 256) 256) i)
                     32)))
      (insert "\t" (propertize (char-to-string ch)
                               'mouse-face
                               'highlight))
      (setq i (1+ i)))))

;;;###autoload
(defun skk-list-chars (arg)
  "$BJQ?t(B `skk-kcode-charset' $B$K=>$C$FJ8;z0lMw$rI=<($9$k(B.
\\[universal-argument] $BIU$-$G<B9T$9$k$H!"(Bfollowing-char() $B$rM%@hI=<($9$k(B."
  (interactive "P")
  (setq skk-list-chars-original-window-configuration
        (current-window-configuration))
  (let* ((buf (progn (and (get-buffer skk-list-chars-buffer-name)
                          (kill-buffer skk-list-chars-buffer-name))
                     (get-buffer-create skk-list-chars-buffer-name)))
         (char (if arg
                   (following-char)
                 (make-char skk-kcode-charset 33 33)))
         (charset (if arg
                      (car (split-char char))
                    skk-kcode-charset)))
    (if (eq charset 'ascii)
        (setq charset 'japanese-jisx0208
              char (make-char 'japanese-jisx0208 33 33)))
    (when skk-henkan-mode       ; $B"&(B or $B"'(B $B$G8F$P$l$?>l9g(B
      (skk-kakutei))
    (setq skk-list-chars-destination-buffer (current-buffer))
    (set-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert (format "Characters in the coded character set `%s'.\n"
                    charset))
    (dotimes (high 94)          ; from ?\x21 to ?\x7e
      (skk-list-chars-sub (+ high 33) charset))
    (pop-to-buffer buf)
    (search-backward (char-to-string char))
    (setq skk-list-chars-point (point))
    (put-text-property skk-list-chars-point (progn (forward-char) (point))
                       'face 'skk-list-chars-face)
    (goto-char skk-list-chars-point)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (skk-list-chars-mode)
    ;;
    (when skk-list-chars-default-charstr
      (skk-list-chars-move-to-charstr skk-list-chars-default-charstr))))

(defun skk-list-chars-quit ()
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration skk-list-chars-original-window-configuration))

(defun skk-list-chars-display-code ()
  (interactive)
  (let ((c (following-char)))
    (if (eq 'ascii (car (split-char c)))
        ;; $B6h@Z$j9T$J$I$G(B $ $B$5$l$?>l9g(B
        (next-completion 1)
      (skk-display-code c (point)))))

(defun skk-list-chars-copy ()
  (interactive)
  (unless (eobp)
    (message "`%s' copied."
             (kill-new (char-to-string (following-char))))))

(defun skk-list-chars-next-line ()
  (interactive)
  (let ((col (current-column)))
    (when (< col 8)
      (setq col 8))
    (unless (zerop (mod col 4))
      (setq col (- col 2)))
    (forward-line)
    (move-to-column col)
    (when (eq 'ascii (car (split-char (following-char))))
      (forward-line)
      (move-to-column col)
      (when (eq 'ascii (car (split-char (following-char))))
        (forward-line)
        (move-to-column col)))))

(defun skk-list-chars-previous-line ()
  (interactive)
  (let ((col (current-column)))
    (when (< col 8)
      (setq col 8))
    (unless (zerop (mod col 4))
      (setq col (- col 2)))
    (if (< (count-lines (point-min) (point)) 5)
        (progn
          (goto-char (point-min))
          (search-forward (char-to-string (make-char skk-kcode-charset 33 33)))
          (move-to-column col))
      (forward-line -1)
      (move-to-column col)
      (when (eq 'ascii (car (split-char (following-char))))
        (forward-line -1)
        (move-to-column col)
        (when (eq 'ascii (car (split-char (following-char))))
          (forward-line -1)
          (move-to-column col))))))

(defun skk-list-chars-goto-point ()
  (interactive)
  (goto-char skk-list-chars-point))

(defun skk-list-chars-insert ()
  (interactive)
  (when (buffer-live-p skk-list-chars-destination-buffer)
    (if (eobp)
        (forward-char -1)
      (if (eq 'ascii (car (split-char (following-char))))
          ;; $B6h@Z$j9T$J$I$G(B RET $B$5$l$?>l9g(B
          (next-completion 1)
        (let ((c (following-char)))
          (set-buffer skk-list-chars-destination-buffer)
          (insert c)
          (setq skk-list-chars-default-charstr (char-to-string c)))))))

(defun skk-list-chars-other-charset ()
  (interactive)
  (setq skk-kcode-charset
        (intern-soft (completing-read "Character set: "
                                      '(("japanese-jisx0213-1")
                                        ("japanese-jisx0213-2")
                                        ("japanese-jisx0208"))
                                      nil t
                                      (symbol-name skk-kcode-charset))))
  (skk-list-chars-quit)
  (skk-list-chars nil))

(defun skk-list-chars-code-input ()
  (interactive)
  (skk-list-chars-jump 'insert))

(defun skk-list-chars-jump (&optional insert)
  (interactive)
  (let ((code (skk-kcode-read-code-string))
        str)
    (unless (string= code "")
      (setq str (skk-list-chars-find-char-string-for-code code))
      (when str
        (when insert
          (save-current-buffer
            (set-buffer skk-list-chars-destination-buffer)
            (insert str))
          (setq skk-list-chars-default-charstr str))
        (skk-list-chars-move-to-charstr str)))))

(defun skk-list-chars-move-to-charstr (charstr)
  (when (memq (char-charset (string-to-char charstr) skk-charset-list)
              (list 'japanese-jisx0208 skk-kcode-charset))
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward charstr nil t))
    (forward-char -1)))

(defun skk-list-chars-find-char-string-for-code (code)
  (let ((list (skk-kcode-parse-code-string code)))
    (if (> (nth 1 list) 160)
        (apply #'skk-kcode-find-char-string list)
      nil)))

(run-hooks 'skk-kcode-load-hook)

(provide 'skk-kcode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-kcode.el ends here
