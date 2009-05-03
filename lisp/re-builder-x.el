;;; re-builder-x.el --- Extension to re-builder

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 21 Jan 2008
;; Version: 0.01
;; Keywords: matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Bug:
;; 1. In perl syntax, can't not input multiple characters at one time.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 're-builder-x)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 're-builder)

(defvar reb-builder-alist
  '((emacs-lisp
     :syntax (lisp-re sregex rx)
     :mode reb-lisp-mode)
    (emacs-read
     :syntax (read)
     :mode reb-lisp-mode)
    (emacs-string
     :syntax (string)
     :mode reb-mode)
    (perl
     :syntax (perl)
     :matcher reb-perl-build-matches
     :cleaner reb-perl-kill-process
     :changer reb-perl-send-buffer
     :switcher reb-perl-toggle-case
     :mode reb-perl-mode))
  "Regexp builder used by re-builder.
Available properties are as following:
  :syntax   - A list of symbols to distinct regexp syntax
  :derived  - the builder are derived from another builder,
              that is to say all property can inherit from parent
              except :syntax
  :mode     - Major mode used for the re-builder buffer
  :matcher  - A function to build match-data. Call with one argument:
              whether use subexp
  :cleaner  - A function when quit re-builder.
  :changer  - A function to change target buffer
  :switcher - A function to toggle case-sensitive.
  :reader   - A function to get raw regexp string.
  :cooker   - A function to transform raw regexp to appliable regexp.
              Call with one argument, the raw regexp
  :inserter - A function to insert raw regexp into buffer when initialized.
              Call with one argument, the raw regexp
")

(defvar reb-re-builder nil
  "builder for `reb-re-syntax'.")

;;; New Functions
(defun reb-builder-get (builder prop)
  (or (plist-get (cdr builder) prop)
      (and (not (memq prop '(:derived :syntax)))
           (let ((derived (assoc (reb-builder-get builder :derived)
                                 reb-builder-alist)))
             (and derived
                  (reb-builder-get derived prop))))))

(defun reb-re-builder ()
  (let ((alist reb-builder-alist)
        builder
        found)
    (while (and (not found) alist)
      (setq builder (car alist)
            alist (cdr alist)
            found (memq reb-re-syntax (reb-builder-get builder :syntax))))
    (and found builder)))

(defsubst reb-all-in-builder (prop)
  (mapcar (lambda (el) (reb-builder-get el prop)) reb-builder-alist))

(defun reb-string-read-regexp ()
  (goto-char (point-min))
  (re-search-forward "\"")
  (let ((beg (point)))
    (goto-char (point-max))
    (re-search-backward "\"")
    (buffer-substring-no-properties beg (point))))

(defun reb-string-insert-regexp (default)
  (insert "\n\"" default "\""))

(defun reb-lisp-build-matches (subexp)
  (let ((re reb-regexp)
        (len 0)
        matches)
    (goto-char (point-min))
    (while (and (not (eobp))
                (re-search-forward re (point-max) t)
                (or (not reb-auto-match-limit)
                    (< len reb-auto-match-limit)))
      (if (= 0 (length (match-string 0)))
          (unless (eobp)
            (forward-char 1)))
      (setq len (1+ len))
      (push (nbutlast (match-data t)) matches))
    (nreverse matches)))

;;; Changed function in re-builder

;; FIX FOR: not use hard coded data in function
(defsubst reb-lisp-syntax-p ()
  (eq (car reb-re-builder) 'emacs-lisp))

;; FIX FOR: not use hard coded data in function
(defun reb-change-syntax (&optional syntax)
  "Change the syntax used by the RE Builder.
Optional argument SYNTAX must be specified if called non-interactively."
  (interactive
   (list (intern
          (completing-read "Select syntax: "
                           (mapcar (lambda (el) (cons (symbol-name el) 1))
                                   (apply 'append (reb-all-in-builder :syntax)))
                           nil t (symbol-name reb-re-syntax)))))
  (if (memq syntax (apply 'append (reb-all-in-builder :syntax)))
      (let ((buffer (get-buffer reb-buffer)))
        (setq reb-re-syntax syntax)
        (when buffer
          (with-current-buffer buffer
            (reb-initialize-buffer))))
    (error "Invalid syntax: %s" syntax)))

;; FIX FOR: make kill-buffer-hook local for re-builder buffer
(defun reb-mode-common ()
  "Setup functions common to functions `reb-mode' and `reb-mode-lisp'."
  (setq	reb-mode-string  ""
	reb-valid-string ""
	mode-line-buffer-identification
	                 '(25 . ("%b" reb-mode-string reb-valid-string)))
  (reb-update-modestring)
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions
	    'reb-auto-update)
  ;; At least make the overlays go away if the buffer is killed
  (add-hook 'kill-buffer-hook 'reb-kill-buffer nil t)
  (reb-auto-update nil nil nil))

;; FIX FOR: call :cleaner function
(defun reb-kill-buffer ()
  "When the RE Builder buffer is killed make sure no overlays stay around."
  (when (memq major-mode (reb-all-in-builder :mode))
    (reb-delete-overlays)
    (funcall (or (reb-builder-get reb-re-builder :cleaner) 'ignore))))

;; FIX FOR: call :cleaner function
(defun reb-quit ()
  "Quit the RE Builder mode."
  (interactive)
  (setq reb-subexp-mode nil
        reb-subexp-displayed nil)
  (reb-delete-overlays)
  (funcall (or (reb-builder-get reb-re-builder :cleaner) 'ignore))
  (bury-buffer)
  (set-window-configuration reb-window-config))

;; FIX FOR: call :reader function
(defun reb-read-regexp ()
  "Read current RE."
  (save-excursion
    (cond ((eq reb-re-syntax 'read)
           (progn
             (goto-char (point-min))
             (read (current-buffer))))
          ((reb-lisp-syntax-p)
           (buffer-string))
          (t (funcall (or (reb-builder-get reb-re-builder :reader)
                          'reb-string-read-regexp))))))

;; FIX FOR: call :cooker function
(defun reb-cook-regexp (re)
  "Return RE after processing it according to `reb-re-syntax'."
  (cond ((eq reb-re-syntax 'lisp-re)
         (if (fboundp 'lre-compile-string)
             (lre-compile-string (eval (car (read-from-string re))))))
        ((eq reb-re-syntax 'sregex)
         (apply 'sregex (eval (car (read-from-string re)))))
        ((eq reb-re-syntax 'rx)
         (rx-to-string (eval (car (read-from-string re)))))
        (t (funcall (or (reb-builder-get reb-re-builder :cooker)
                        'identity) re))))

;; FIX FOR: call :inserter function
(defun reb-insert-regexp ()
  "Insert current RE."
  (let ((re (or (reb-target-binding reb-regexp)
                (reb-empty-regexp))))
    (cond ((eq reb-re-syntax 'read)
           (print re (current-buffer)))
          ;; For the Lisp syntax we need the "source" of the regexp
          ((reb-lisp-syntax-p)
           (insert (or (reb-target-binding reb-regexp-src)
                       (reb-empty-regexp))))
          (t (funcall (or (reb-builder-get reb-re-builder :inserter)
                          'reb-string-insert-regexp) re)))))

;;; FIX FOR: call :switcher function
(defun reb-toggle-case ()
  "Toggle case sensitivity of searches for RE Builder target buffer."
  (interactive)
  (with-current-buffer reb-target-buffer
    (setq case-fold-search (not case-fold-search))
    (funcall (or (reb-builder-get reb-re-builder :switcher) 'ignore)))
  (reb-update-modestring)
  (reb-auto-update nil nil nil t))

;; FIX FOR: call :mode function
(defun reb-initialize-buffer ()
  "Initialize the current buffer as a RE Builder buffer."
  (erase-buffer)
  (reb-insert-regexp)
  (setq reb-re-builder (reb-re-builder))
  (goto-char (+ 2 (point-min)))
  (funcall (or (reb-builder-get reb-re-builder :mode) 'reb-mode)))

;; FIX FOR: call :changer function
(defun reb-change-target-buffer (buf)
  "Change the target buffer and display it in the target window."
  (interactive "bSet target buffer to: ")
  (let ((buffer (get-buffer buf)))
    (if (not buffer)
        (error "No such buffer")
      (reb-delete-overlays)
      (setq reb-target-buffer buffer)
      (funcall (or (reb-builder-get reb-re-builder :changer) 'ignore))
      (reb-do-update
       (if reb-subexp-mode reb-subexp-displayed nil))
      (reb-update-modestring))))

;; FIX FOR: call :matcher function
;; Note that `reb-count-subexps' is not needed in this function
(defun reb-update-overlays (&optional subexp)
  "Switch to `reb-target-buffer' and mark all matches of `reb-regexp'.
If SUBEXP is non-nil mark only the corresponding sub-expressions."
  (let ((submatches 0)
        matches firstmatch i max-suffix suffix )
    (save-excursion
      (set-buffer reb-target-buffer)
      (goto-char (point-min))
      (setq matches (funcall (or (reb-builder-get reb-re-builder :matcher)
                                 'reb-lisp-build-matches) subexp))
      (reb-delete-overlays)
      (dolist (match matches)
        (setq i (or subexp 0))
        (while match
          (let ((overlay (make-overlay (car match) (cadr match)))
                ;; When we have exceeded the number of provided faces,
                ;; cycle thru them where `max-suffix' denotes the maximum
                ;; suffix for `reb-match-*' that has been defined and
                ;; `suffix' the suffix calculated for the current match.
                (face
                 (cond
                  (max-suffix
                   (if (= suffix max-suffix)
                       (setq suffix 1)
                     (setq suffix (1+ suffix)))
                   (intern-soft (format "reb-match-%d" suffix)))
                  ((intern-soft (format "reb-match-%d" i)))
                  ((setq max-suffix (1- i))
                   (setq suffix 1)
                   ;; `reb-match-1' must exist.
                   'reb-match-1))))
            (setq reb-overlays (cons overlay reb-overlays)
                  submatches (1+ submatches))
            (overlay-put overlay 'face face)
            (overlay-put overlay 'priority i))
          (setq i (1+ i)
                match (cddr match)))))
    (let ((count (if subexp submatches (length matches))))
      (message "%s %smatch%s%s"
               (if (= 0 count) "No" (int-to-string count))
               (if subexp "subexpression " "")
               (if (= 1 count) "" "es")
               (if (and reb-auto-match-limit
                        (= reb-auto-match-limit count))
                   " (limit reached)" "")))
    (if matches
        (progn (store-match-data
                (append (car matches) (list reb-target-buffer)))
               (reb-show-subexp (or subexp 0))))))

;; FIX FOR: make this command generic
(defun reb-next-match ()
  "Go to next match in the RE Builder target window."
  (interactive)
  (reb-assert-buffer-in-window)
  (with-selected-window reb-target-window
    (let ((face-re (if reb-subexp-mode
                       "^reb-match-[0-9]+$"
                     "^reb-match-0$"))
          (oldpos (point))
          face found)
      (while (and (not found) (not (eobp)))
        (goto-char (next-overlay-change (point)))
        (mapc (lambda (ov)
                (and (not found)
                     (setq face (overlay-get ov 'face))
                     (string-match face-re (symbol-name face))
                     (setq found ov)))
              (overlays-at (point))))
      (if (not found)
          (progn
            (goto-char oldpos)
            (message "No more matches."))
        ;; FIXME: save match data in overlay or just a little hack
        (store-match-data (list (overlay-start found)
                                (overlay-end found)
                                (current-buffer)))
        (reb-show-subexp 0 t)))))

(defun reb-prev-match ()
  "Go to previous match in the RE Builder target window."
  (interactive)
  (reb-assert-buffer-in-window)
  (with-selected-window reb-target-window
    (let ((face-re (if reb-subexp-mode
                       "^reb-match-[0-9]+$"
                     "^reb-match-0$"))
          (oldpos (point))
          face found)
      (while (and (not found) (not (bobp)))
        (goto-char (previous-overlay-change (point)))
        (mapc (lambda (ov)
                (and (not found)
                     (setq face (overlay-get ov 'face))
                     (string-match face-re (symbol-name face))
                     (< (overlay-end ov) oldpos)
                     (setq found ov)))
              (overlays-at (point))))
      (if (not found)
          (progn
            (goto-char oldpos)
            (message "No more matches."))
        ;; FIXME: save match data in overlay or just a little hack
        (store-match-data (list (overlay-start found)
                                (overlay-end found)
                                (current-buffer)))
        (reb-show-subexp 0 t)))))

;;; Regexp Builder for Perl
(defvar reb-perl-coding-system-alist
  '((utf-8 . "utf8")
    (chinese-gbk . "gbk"))
  "Coding system conversion between emacs and perl")
(defvar reb-perl-process nil
  "Process of perl re-builder")
(defvar reb-perl-buffer " reb-perl"
  "Name of the buffer store output of `reb-perl-process'.")
(defvar reb-perl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map reb-mode-map)
    (define-key map "\C-c\C-a" 'reb-perl-send-buffer)
    map)
  "Keymap used for reb-perl-mode.")
(defvar reb-perl-script
"use Encode qw/decode/;
our ( $buffer, $pattern, $str, %config );
while (<>) {
    chomp( my $cmd = $_ );
    if ( $cmd =~ '^[.] ' ) {
        $str .= substr( $_, 2 );
    }
    elsif ( $cmd eq \"output\" ) {
        display_matches();
    }
    elsif ( $cmd =~ /set\\s+(buffer|pattern)/ ) {
        chomp($str);
        if ( $1 eq \"buffer\" ) {
            $buffer = decode($config{encoding}, $str);
        }
        else {
            $pattern = decode($config{encoding}, $str);
        }
        $str = \"\";
    }
    elsif ( $cmd =~ /set\\s+(limit|subexp|case|encoding)\\s+(\\w+)/ ) {
        if ( $2 eq \"undef\" ) {
            delete $config{$1};
        }
        else {
            $config{$1} = $2;
        }
    }
    elsif ( $cmd eq 'debug' ) {
        print \"String:\\n\", $buffer, \"\\n\",
              \"Pattern: \", $pattern, \"\\n\",
              \"Limit: \", $config{limit} || \"no limit\", \"\\n\",
              \"Subexp: \", (exists $config{limit} ? $config{limit} : \"All\" ), \"\\n\";
    }
}

sub read_until_eof {
    my $s;
    while (<>) {
        $s .= $_;
    }
    chomp($s);
    return $s;
}

sub display_matches {
    my $p;
    if ( !$pattern ) {
        print \"()\\n\";
        return;
    }
    if ( $config{case} ) {
        $pattern = \"(?i)\" . $pattern;
    }
    eval { $p = qr($pattern); };
    if ($@) {
        print \"Error: $@\";
        return;
    }
    my ( $limit, $subexp ) = ( $config{limit}, $config{subexp} );
    my $len = length($buffer);
    $limit ||= 1 << 31;
    my $i = 1;
    print \"(\";
    pos($buffer) = 0;
    while ( $buffer =~ /$p/gm ) {
        if ( length($&) == 0 ) {
            my $p = pos($buffer)++;
            last if $p >= $len;
        }
        last if $i > $limit;
        my @pairs;
        if ( defined $subexp ) {
            @pairs = ($subexp);
        }
        else {
            @pairs = 0 .. $#-;
        }
        print \"(\", join( \" \", map { $-[$_] + 1, $+[$_] + 1 } @pairs ), \")\";
        $i++;
    }
    print \")\\n\";
}
"
  "*Script for perl re-builder")

(defsubst reb-perl-kill-process ()
  (ignore-errors
    (and reb-perl-process
         (kill-process reb-perl-process))))

(defun reb-perl-start-process ()
  (setq reb-perl-process
        (start-process "perl-rebuilder"
                       reb-perl-buffer
                       "perl"))
  (process-send-string reb-perl-process reb-perl-script)
  (process-send-eof reb-perl-process)
  (process-send-string reb-perl-process
                       (format "set encoding %s\n"
                               (assoc-default (coding-system-base (car (process-coding-system reb-perl-process))) reb-perl-coding-system-alist))))

(defsubst reb-perl-clear-output ()
  (with-current-buffer (get-buffer-create reb-perl-buffer)
    (erase-buffer)))

(defsubst reb-perl-literal (text)
  (concat ". " (replace-regexp-in-string "\n" "\n. " (or text "")) "\n"))

(define-derived-mode reb-perl-mode reb-mode "RE Builder Perl"
  "Major mode for interactively building symbolic Regular Expressions."
  (reb-perl-kill-process)
  (reb-perl-start-process)
  (reb-perl-clear-output)
  (reb-perl-send-buffer))

(defun reb-perl-toggle-case ()
  (process-send-string reb-perl-process
                       (concat "set case "
                               (if case-fold-search "1" "undef")
                               "\n")))

(defun reb-perl-send-buffer ()
  (interactive)
  (save-excursion
    (set-buffer reb-target-buffer)
    (process-send-string reb-perl-process
                         (reb-perl-literal (buffer-string)))
    (process-send-string reb-perl-process "set buffer\n"))
  (reb-force-update))

(defun reb-perl-build-matches (subexp)
  (let ((re reb-regexp)
        (proc reb-perl-process)
        done)
    (reb-perl-clear-output)
    (process-send-string proc (reb-perl-literal re))
    (process-send-string proc "set pattern\n")
    (process-send-string proc (format "set subexp %S\n" (or subexp 'undef)))
    (process-send-string proc (format "set limit %S\n"
                                      (or reb-auto-match-limit 'undef)))
    (process-send-string proc "output\n")
    (with-current-buffer (process-buffer proc)
      (while (not done)
        (goto-char (point-min))
        (sit-for 0.1)
        (if (looking-at "Error")
            (error (buffer-string))
          (when (save-excursion (re-search-forward "(" nil t))
            (condition-case nil
                (progn
                  (scan-sexps (point-min) 1)
                  (setq done t))
              (error nil)))))
      (read (current-buffer)))))

(provide 're-builder-x)
;;; re-builder-x.el ends here
