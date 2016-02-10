;;; skk-macs.el --- macros and inline functions commonly used in SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 1999-2010 SKK Development Team <skk@ring.gr.jp>
;; Copyright (C) 1993-2000 Free Software Foundation, Inc.

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-macs.el,v 1.199 2013/03/28 11:55:19 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2013/03/28 11:55:19 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (defvar mule-version)
  (defvar skk-emacs-modeline-property))

(eval-when-compile
  (require 'advice))

(eval-and-compile
  (require 'skk-vars))

;;;; macros

(when (eval-when-compile (and (featurep 'emacs)
			      (= emacs-major-version 22)))
  (defmacro ignore-errors (&rest body)
    "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last form in BODY."
    `(condition-case nil
	 (progn
	   ,@body)
       (error nil))))

(defmacro skk-defadvice (function &rest everything-else)
  "Defines a piece of advice for FUNCTION (a symbol).
This is like `defadvice', but warns if FUNCTION is a subr command and advice
doesn't give arguments of `interactive'. See `interactive' for details."
  (let ((origfunc (and (fboundp function)
		       (if (ad-is-advised function)
			   (ad-get-orig-definition function)
			 (symbol-function function))))
	interactive)
    (unless
	(or (not origfunc)
	    (not (subrp origfunc))
	    (memq function ; XXX possibilly Emacs version dependent
		  ;; built-in commands which do not have interactive specs.
		  '(abort-recursive-edit
		    bury-buffer
		    delete-frame
		    delete-window
		    exit-minibuffer)))
      ;; check if advice definition has a interactive call or not.
      (setq interactive
	    (cond
	     ((and (stringp (nth 1 everything-else)) ; have document
		   (eq 'interactive (car-safe (nth 2 everything-else))))
	      (nth 2 everything-else))
	     ((eq 'interactive (car-safe (nth 1 everything-else)))
	      (nth 1 everything-else))))
      (cond
       ((and (commandp origfunc)
	     (not interactive))
	(message
	 "\
*** WARNING: Adding advice to subr %s\
 without mirroring its interactive spec ***"
		 function))
       ((and (not (commandp origfunc))
	     interactive)
	(setq everything-else (delq interactive everything-else))
	(message
	 "\
*** WARNING: Deleted interactive call from %s advice\
 as %s is not a subr command ***"
	 function function))))
    `(defadvice ,function ,@everything-else)))

;;;###autoload
(put 'skk-defadvice 'lisp-indent-function 'defun)
(def-edebug-spec skk-defadvice defadvice)

(defmacro skk-save-point (&rest body)
  `(let ((skk-save-point (point-marker)))
     (unwind-protect
	 (progn
	   ,@body)
       (goto-char skk-save-point)
       (skk-set-marker skk-save-point nil))))

(def-edebug-spec skk-save-point t)

(defmacro skk-message (japanese english &rest arg)
  "メッセージを表示する。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil で
あれば ENGLISH をエコーエリアに表示する。
 ARG は `message' 関数の第２引数以降の引数として渡される。"
  (append
   (if arg
       `(message (if skk-japanese-message-and-error
		     ,japanese
		   ,english))
     `(message "%s" (if skk-japanese-message-and-error
			,japanese
		      ,english)))
   arg))

(defmacro skk-error (japanese english &rest arg)
  "メッセージを表示して、エラーを発生させる。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil で
あれば ENGLISH をエコーエリアに表示し、エラーを発生させる。
 ARG は `error' 関数の第２引数以降の引数として渡される。"
  (append
   (if arg
       `(error (if skk-japanese-message-and-error
		   ,japanese
		 ,english))
     `(error "%s" (if skk-japanese-message-and-error
		      ,japanese
		    ,english)))
   arg))

(defmacro skk-yes-or-no-p (japanese english)
  "ユーザに yes-or-no を質問し、答えが yes だったら t を返す。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil であれ
ば ENGLISH を PROMPT として `yes-or-no-p' を実行する。
`yes-or-no-p' の引数 PROMPT が複雑に入れ込んでいる場合は `skk-yes-or-no-p' を
使うよりもオリジナルの `yes-or-no-p' を使用した方がコードが複雑にならない場合
がある。"
  `(yes-or-no-p (if skk-japanese-message-and-error
		    ,japanese ,english)))

(defmacro skk-y-or-n-p (japanese english)
  "ユーザに \"y or n\" を質問し、答えが \"y\" だったら t を返す。
`skk-japanese-message-and-error' が non-nil であれば JAPANESE を、 nil であれ
ば ENGLISH を PROMPT として `y-or-n-p' を実行する。"
  `(y-or-n-p (if skk-japanese-message-and-error
		 ,japanese ,english)))

(defmacro skk-set-marker (marker position &optional buffer)
  "マーカ MARKER を BUFFER の POSITION に移動する。
BUFFER のディフォルト値はカレントバッファである。
MARKER が nil だったら、新規マーカーを作って代入する。"
  `(progn
     (if (not ,marker)
	 (setq ,marker (make-marker)))
     (set-marker ,marker ,position ,buffer)))

(defmacro skk-with-point-move (&rest form)
  "ポイントを移動するがフックを実行してほしくない場合に使う。"
  `(unwind-protect
       (progn
	 ,@form)
     (setq skk-previous-point (point))))

(def-edebug-spec skk-with-point-move t)

(defmacro skk-face-on (object start end face &optional priority)
  (cond
   ((eval-when-compile (featurep 'xemacs))
    `(let ((inhibit-quit t))
       (if (not (extentp ,object))
	   (progn
	     (setq ,object (make-extent ,start ,end))
	     (if (not ,priority)
		 (set-extent-face ,object ,face)
	       (set-extent-properties
		,object (list 'face ,face 'priority ,priority))))
	 (set-extent-endpoints ,object ,start ,end))))
   (t
    `(let ((inhibit-quit t))
       (if (not (overlayp ,object))
	   (progn
	     (setq ,object (make-overlay ,start ,end))
	     (when ,priority
	       (overlay-put ,object 'priority ,priority))
	       (overlay-put ,object 'face ,face)
	       ;;(overlay-put (, object) 'evaporate t)
	       )
	 (move-overlay ,object ,start ,end))))))

(defmacro skk-cannot-be-undone (&rest body)
  `(let ((buffer-undo-list t)
	 ;;buffer-read-only
	 (modified (buffer-modified-p)))
     (unwind-protect
	 (progn
	   ,@body)
       (set-buffer-modified-p modified))))

;;;###autoload
(put 'skk-loop-for-buffers 'lisp-indent-function 1)
(defmacro skk-loop-for-buffers (buffers &rest body)
  "BUFFERS が指定する各バッファに移動して BODY を実行する。"
  `(save-current-buffer
     (dolist (buf ,buffers)
       (when (buffer-live-p buf)
	 (set-buffer buf)
	 ,@body))))

(defmacro skk-called-interactively-p (&optional kind)
  (cond ((featurep 'xemacs)
	 ;; XEmacs
	 ;; `called-interactively-p' is not defined.
	 '(interactive-p))

	((or (>= emacs-major-version 24)
	     (and (= emacs-major-version 23)
		  (>= emacs-minor-version 2)))
	 ;; GNU Emacs 23.2 or later
	 ;; `called-interactively-p' takes one argument.
	 `(called-interactively-p ,kind))

	(t
	 ;; GNU Emacs 22 and 23.1
	 ;; `called-interactively-p' takes no argument and is equivalent
	 ;; to (called-interactively-p 'any) in later Emacs versions.
	 `(if (eq ,kind 'interactive)
	      (interactive-p)
	    (called-interactively-p)))))

(defmacro skk-delete-overlay (list)
  ;; skk-dcomp-multiple-hide と skk-inline-hide を統合した。
  `(when ,list
     (dolist (o ,list)
       (delete-overlay o))
     (setq ,list nil)))

(defmacro skk-set-deactivate-im-func (func)
  (cond ((boundp 'deactivate-current-input-method-function)
	 ;; GNU Emacs 24.2 から
	 `(setq deactivate-current-input-method-function ,func))

	(t
	 ;; GNU Emacs 24.1 まで
	 `(setq inactivate-current-input-method-function ,func))
	))

(defmacro skk-deactivate-input-method ()
  (cond ((fboundp 'deactivate-input-method)
	 ;; GNU Emacs 24.2 から
	 '(deactivate-input-method))
	(t
	 ;; GNU Emacs 24.1 まで
	 '(inactivate-input-method))
	))

(defmacro skk-facep (face)
  (cond ((featurep 'xemacs)
	 `(find-face ,face))
	(t
	 `(facep ,face))))

(defmacro skk-help-make-usage (symbol arglist)
  (cond ((fboundp 'help--make-usage)
	 ;; GNU Emacs 25.1 から
	 `(help--make-usage ,symbol ,arglist))

	(t
	 ;; GNU Emacs 24.1 まで
	 `(help-make-usage ,symbol ,arglist))))

;;; functions.
;; version dependent
;; Many functions are derived from emu (APEL).

(when (eval-when-compile (and (featurep 'emacs)
			      (= emacs-major-version 22)))
  ;; GNU Emacs 22 まで
  ;; . char-valid-p is a built-in function in `C source code'.
  ;;     (char-valid-p OBJECT &optional GENERICP)
  ;;   Return t if OBJECT is a valid normal character.
  ;;   If optional arg GENERICP is non-nil, also return t if OBJECT is
  ;;   a valid generic character.
  (defalias 'characterp 'char-valid-p)

  ;; GNU Emacs 23 から
  ;; . char-valid-p is an alias for `characterp' in `mule.el'.
  ;;     (char-valid-p OBJECT &optional IGNORE)
  ;;   This function is obsolete since 23.1;
  ;;   use `characterp' instead.

  ;; . characterp is a built-in function in `C source code'.
  ;;     (characterp OBJECT &optional IGNORE)
  ;;   Return non-nil if OBJECT is a character.
  )

(when (eval-when-compile (featurep 'emacs))
  ;; int-char() が出現するのは skk-compute-henkan-lists() のみ
  ;; XEmacs では int-char() は標準？
  (defalias 'int-char 'identity))

;;; string-to-char-list が出現するのは 9 行下の defalias のみ
;;; よって string-to-int-list の定義を変更してしまう
;; (when (eval-when-compile (featurep 'emacs))
;;   (defun string-to-char-list (string)
;;     "Return a list of which elements are characters in the STRING."
;;     (mapcar #'identity string)))

;;; string-to-int-list() の定義を macro へ変更
;; (when (eval-when-compile (featurep 'emacs))
;;   ;; (defalias 'string-to-int-list 'string-to-char-list))
;;   (defun string-to-int-list (string)
;;     "Return a list of which elements are characters in the STRING."
;;     (mapcar #'identity string)))
;;
;; (when (eval-when-compile (featurep 'xemacs))
;;   (defun string-to-int-list (string)
;;     (mapcar #'char-int string)))

(defmacro string-to-int-list (string)
  (cond ((featurep 'xemacs)
	 `(mapcar #'char-int ,string))	; XEamcs
	(t
	 `(mapcar #'identity ,string)))) ; GNU Emacs

(when (eval-when-compile (featurep 'emacs))
  (defun character-to-event (ch)
    "Convert keystroke CH into an event structure, replete with bucky bits.
Note that CH (the keystroke specifier) can be an integer, a character
or a symbol such as 'clear."
    ch))

(when (eval-when-compile (featurep 'emacs))
  (defun event-to-character (event)
    "Return the character approximation to the given event object.
If the event isn't a keypress, this returns nil."
    (cond
     ((symbolp event)
      ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
      (let ((mask (get event 'event-symbol-element-mask)))
	(if mask
	    (let ((base (get (car mask) 'ascii-character)))
	      (if base
		  (logior base (cadr mask)))))))
     ((integerp event)
      event))))

(when (eval-when-compile (featurep 'emacs))
  (defun cancel-undo-boundary ()
    "Cancel undo boundary."
    (if (and (consp buffer-undo-list)
	     (null (car buffer-undo-list)))
	(setq buffer-undo-list (cdr buffer-undo-list)))))

(when (eval-when-compile (featurep 'xemacs))
  (defun substring-no-properties (string &optional from to)
    "Return a substring of string, without text properties.
It starts at index from and ending before to.
to may be nil or omitted; then the substring runs to the end of string.
If from is nil or omitted, the substring starts at the beginning of string.
If from or to is negative, it counts from the end.

With one argument, just copy string without its properties."
    (let ((substr (copy-sequence (substring string (or from 0) to))))
      (set-text-properties 0 (length substr) nil substr)
      substr)))

;; From GNU Emacs 22.1.
(when (eval-when-compile (and (featurep 'xemacs)
			      (= emacs-major-version 21)
			      (= emacs-minor-version 4)))
  (defun replace-regexp-in-string (regexp rep string &optional
						fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function, it is called with the actual text of each
match, and its value is used as the replacement text.  When REP is called,
the match-data are the result of matching REGEXP against a substring
of STRING.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

    ;; To avoid excessive consing from multiple matches in long strings,
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacements it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l (length string))
	  (start (or start 0))
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0 str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb)      ; unmatched prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ; leftover
	(apply #'concat (nreverse matches))))))

;; For GNU Emacs.
(when (eval-when-compile (featurep 'emacs))
  (defun next-command-event (&optional event prompt)
    "Read an event object from the input stream.
If EVENT is non-nil, it should be an event object and will be filled
in and returned; otherwise a new event object will be created and
returned.
If PROMPT is non-nil, it should be a string and will be displayed in
the echo area while this function is waiting for an event."
    (read-event prompt)))

(when (eval-when-compile (featurep 'xemacs))
  (defun set-buffer-multibyte (flag)
    "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  flag))

(defun skk-ding (&optional arg sound device)
  "`ding' の Emacsen による違いを吸収する。"
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (ding arg sound device))
   (t
    (ding arg))))

(defun skk-color-cursor-display-p ()
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (eq (device-class (selected-device)) 'color))
   ((eval-when-compile (featurep 'emacs))
    (and (skk-find-window-system)
	 (fboundp 'x-display-color-p)
	 (x-display-color-p)))))

(defun skk-char-to-unibyte-string (char)
  (ignore-errors
    (cond
     ((eval-when-compile (and (featurep 'emacs)
			      (>= emacs-major-version 23)))
      ;; GNU Emacs 23.1 or later
      (string-make-unibyte (char-to-string char)))
     (t
      (char-to-string char)))))

(defun skk-ascii-char-p (char)
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; GNU Emacs 23.1 or later
    (eq (char-charset char skk-charset-list) 'ascii))
   (t
    (eq (char-charset char) 'ascii))))

(defun skk-jisx0208-p (char)
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; GNU Emacs 23.1 or later
    (eq (char-charset char skk-charset-list) 'japanese-jisx0208))
   (t
    (eq (char-charset char) 'japanese-jisx0208))))

(defun skk-jisx0213-p (char)
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; GNU Emacs 23.1 or later
    (memq (char-charset char skk-charset-list)
	  '(japanese-jisx0213-1
	    japanese-jisx0213.2004-1
	    japanese-jisx0213-2)))
   (t
    (and (featurep 'jisx0213)		; Mule-UCS
	 (memq (char-charset char)
	       '(japanese-jisx0213-1 japanese-jisx0213-2))))))

(defun skk-split-char (ch)
  ;; http://mail.ring.gr.jp/skk/200908/msg00006.html
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; C の split-char() と同様の機能だが、char-charset() の呼出しにおいて
    ;; 文字集合の選択肢を skk-charset-list に含まれるものに制限する。
    ;; これは例えば、japanese-jisx0208 の文字が unicode-bmp に属する、
    ;; と判定されるような状況を回避する。
    (let* ((charset (char-charset ch skk-charset-list))
	   (code (encode-char ch charset))
	   (dimension (charset-dimension charset))
	   val)
      (while (> dimension 0)
	(setq val (cons (logand code 255) ;; 0xFF
			val))
	(setq code (lsh code -8))
	(setq dimension (1- dimension)))
      (cons charset val)))
   (t
    ;; Emacs 22 および XEmacs
    (split-char ch))))

(defun skk-char-charset (ch &optional restriction)
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; GNU Emacs 23.1 or later
    (char-charset ch restriction))
   (t
    (char-charset ch))))

;; this one is called once in skk-kcode.el, too.
(defun skk-charsetp (object)
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (find-charset object))
   (t
    (charsetp object))))

(defun skk-indicator-to-string (indicator &optional no-properties)
  "SKK インジケータ型オブジェクト INDICATOR を文字列に変換する。"
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (if (stringp indicator)
	indicator
      (cdr indicator)))
   (t
    (if no-properties
	(with-temp-buffer
	  (insert indicator)
	  (buffer-substring-no-properties (point-min) (point-max)))
      indicator))))

(defun skk-mode-string-to-indicator (mode string)
  "文字列 STRING を SKK インジケータ型オブジェクトに変換する。"
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (cons (cdr (assq mode skk-xemacs-extent-alist))
	  string))
   (t
    (if (and window-system
	     (not (eq mode 'default)))
	(apply 'propertize string
	       (cdr (assq mode skk-emacs-property-alist)))
      string))))

(defun skk-local-variable-p (variable &optional buffer afterset)
  "Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer."
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (local-variable-p variable (or buffer (current-buffer)) afterset))
   (t
    (local-variable-p variable (or buffer (current-buffer))))))

(defun skk-face-proportional-p (face)
  (cond ((eval-when-compile (featurep 'xemacs))
	 (face-proportional-p face))
	(t
	 (or (face-equal face 'variable-pitch)
	     (eq (face-attribute face :inherit) 'variable-pitch)))))

(defun skk-event-key (event)
  "イベント EVENT を発生した入力の情報を取得する。"
  (cond
   ((eval-when-compile (featurep 'xemacs))
    (let ((char (event-to-character event t t t)))
      (if (characterp char)
	  char
	(event-key event))))
   (t
    (let ((char (event-to-character event))
	  keys)
      (if char
	  (vector char)
	(setq keys (recent-keys))
	(vector (aref keys (1- (length keys)))))))))

;; Emacs 23 or later shows warning for the use of last-command-char,
;; which is an obsolete variable alias for last-command-event.
;; What is annoying is incompatibility between FSF Emacs and XEmacs.
;; In XEmacs, last-command-event is an event, not a character, whereas
;; last-command-char is a character equivalent to last-command-event.
(defun skk-last-command-char ()
  (cond
   ((eval-when-compile (featurep 'xemacs))
    last-command-char)
   (t
    last-command-event)))

(defun skk-set-last-command-char (char)
  (let ((variable (if (featurep 'xemacs)
		      'last-command-char
		    'last-command-event)))
    (set variable char)))

(defun skk-region-active-p ()
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    (use-region-p))
   ((eval-when-compile (featurep 'xemacs))
    (region-active-p))
   (t
    ;; GNU Emacs 22.
    (and transient-mark-mode mark-active))))

(put 'skk-bind-last-command-char 'lisp-indent-function 1)
(defmacro skk-bind-last-command-char (char &rest body)
  (let ((variable (cond ((featurep 'xemacs)
			 'last-command-char)
			(t
			 'last-command-event))))
    `(let ((,variable ,char))
       (progn
	 ,@body))))

(defun skk-process-kill-without-query (process)
  (cond
   ((eval-when-compile (featurep 'emacs))
    (set-process-query-on-exit-flag process nil))
   (t
    (process-kill-without-query process))))

;;; version independent

(defsubst skk-char-octet (ch &optional n)
  (or (nth (if n
	       (1+ n)
	     1)
	   (skk-split-char ch))
      0))

(defun skk-cursor-set (&optional color force)
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  (when (or skk-use-color-cursor
	    force)
    (skk-cursor-set-1 color)))

(defun skk-cursor-off ()
  (unless (skk-color-cursor-display-p)
    (setq skk-use-color-cursor nil))
  (when skk-use-color-cursor
    (skk-cursor-off-1)))

(defun skk-modify-indicator-alist (mode string)
  (setcdr (assq mode skk-indicator-alist)
	  (cons string (skk-mode-string-to-indicator mode string))))

(defun skk-update-modeline (&optional mode string)
  (unless mode
    (setq mode 'default))
  ;;
  (when string
    (skk-modify-indicator-alist mode string))
  ;;
  (let ((indicator (cdr (assq mode skk-indicator-alist))))
    (setq skk-modeline-input-mode
	  (if (eq skk-status-indicator 'left)
	      (cdr indicator)
	    (car indicator)))
    (force-mode-line-update)))

;; ツリーにアクセスするためのインターフェース
(defun skk-make-rule-tree (char prefix nextstate kana branch-list)
  (list char
	prefix
	(if (string= nextstate "")
	    nil
	  nextstate)
	kana
	branch-list))

;;(defsubst skk-get-char (tree)
;;  (car tree))
;;
;; skk-current-rule-tree に対して破壊的な操作は行えない。skk-rule-tree の
;; 内容まで変わってしまい、skk-current-rule-tree の initialize が手軽に行え
;; なくなる。ここが解決できれば skk-prefix を全滅できるのに...。
;;(defsubst skk-set-char (tree char)
;;  (setcar tree char))
;;
;;(defsubst skk-set-prefix (tree prefix)
;;  (setcar (cdr tree) prefix))

(defsubst skk-get-prefix (tree)
  (nth 1 tree))

(defsubst skk-get-nextstate (tree)
  (nth 2 tree))

(defsubst skk-set-nextstate (tree nextstate)
  (when (string= nextstate "")
    (setq nextstate nil))
  (setcar (nthcdr 2 tree) nextstate))

(defsubst skk-get-kana (tree)
  (nth 3 tree))

(defsubst skk-set-kana (tree kana)
  (setcar (nthcdr 3 tree) kana))

(defsubst skk-get-branch-list (tree)
  (nth 4 tree))

(defsubst skk-set-branch-list (tree branch-list)
  (setcar (nthcdr 4 tree) branch-list))

;; tree procedure for skk-kana-input.
(defsubst skk-add-branch (tree branch)
  (skk-set-branch-list tree (cons branch (skk-get-branch-list tree))))

(defsubst skk-select-branch (tree char)
  (assq char (skk-get-branch-list tree)))

(defun skk-erase-prefix (&optional clean)
  "`skk-echo' が non-nil であれば現在表示されている `skk-prefix' を消す。
オプショナル引数の CLEAN が指定されると、変数としての `skk-prefix' を空文字に、
`skk-current-rule-tree' を nil に初期化する。"
  ;; かな文字の入力がまだ完成していない場合にこの関数が呼ばれたときなどは
  ;; 表示されている skk-prefix は削除したいが、変数としての skk-prefix は
  ;; null 文字にしたくない。
  (when (overlayp skk-prefix-overlay)
	(condition-case nil
	    (delete-overlay skk-prefix-overlay)
	  (error
	   (skk-set-marker skk-kana-start-point nil)
	   (setq skk-prefix ""
		 skk-current-rule-tree nil))))
  (when clean
    (setq skk-prefix ""
	  skk-current-rule-tree nil))) ; fail safe

(defun skk-kana-cleanup (&optional force)
  (let ((data (cond
	       ((and skk-current-rule-tree
		     (null (skk-get-nextstate skk-current-rule-tree)))
		(skk-get-kana skk-current-rule-tree))
	       (skk-kana-input-search-function
		(car (funcall skk-kana-input-search-function)))))
	kana)
    (when (or force data)
      (skk-erase-prefix 'clean)
      (setq kana (if (functionp data)
		     (funcall data nil)
		   data))
      (when (consp kana)
	(setq kana (if skk-katakana
		       (car kana)
		     (cdr kana))))
      (when (stringp kana)
	(skk-insert-str kana))
      (skk-set-marker skk-kana-start-point nil)
      (or data t)))) ; skk-prefix に対応するデータがあったならそれを返す

(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion
       (require 'skk-num)
       skk-num-list))

(defsubst skk-file-exists-and-writable-p (file)
  (and (setq file (expand-file-name file))
       (file-exists-p file)
       (file-writable-p file)))

(defsubst skk-share-private-jisyo-p ()
  (vectorp skk-jisyo-update-vector))

(defsubst skk-lower-case-p (char)
  "CHAR が小文字のアルファベットであれば、t を返す。"
  (and (<= ?a char)
       (>= ?z char)))

(defsubst skk-downcase (char)
  "連想リスト `skk-downcase-alist' に該当あれば (assq)、要素(子リスト)の cdr を返す。
該当なければ 関数 `downcase'の結果を返す。
関数 `skk-set-henkan-point' から呼ばれている。"
  (or (cdr (assq char skk-downcase-alist))
      (downcase char)))

(defun skk-mode-off ()
  (setq skk-mode nil
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana nil)
  ;; initialize
  (skk-update-modeline)
  (skk-cursor-off)
  (skk-remove-skk-pre-command))

(defun skk-j-mode-on (&optional katakana)
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode t
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana katakana)
  (skk-setup-keymap)
  (skk-update-modeline (if skk-katakana
			   'katakana
			 'hiragana))
  (skk-cursor-set))

(defun skk-latin-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode t
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'latin)
  (skk-cursor-set))

(defun skk-jisx0208-latin-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode nil
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode t
	skk-jisx0201-mode nil
	;; sub mode of skk-j-mode.
	skk-katakana nil)
  (skk-setup-keymap)
  (skk-update-modeline 'jisx0208-latin)
  (skk-cursor-set))

(defun skk-abbrev-mode-on ()
  (setq skk-mode t
	skk-abbrev-mode t
	skk-latin-mode nil
	skk-j-mode nil
	skk-jisx0208-latin-mode nil
	skk-jisx0201-mode nil
	;; skk-abbrev-mode は一時的な ascii 文字による変換なので、変換後は元の
	;; 入力モード (かなモードかカナモード) に戻ることが期待される。
	;; skk-katakana は minor-mode フラグではなく、skk-j-mode マイナーモード
	;; の中でこのフラグにより入力文字を決定するポインタを変更するだけなので
	;; skk-abbrev-mode マイナーモード化するのに skk-katakana フラグを初期化
	;; しなければならない必然性はない。
	;; sub mode of skk-j-mode.
	;;skk-katakana nil
	)
  (skk-setup-keymap)
  (skk-update-modeline 'abbrev)
  (skk-cursor-set))

(defsubst skk-in-minibuffer-p ()
  "カレントバッファがミニバッファであれば t を返す。"
  (eq (current-buffer) (window-buffer (minibuffer-window))))

(defun skk-window-body-height ()
  (cond
   ((eval-when-compile (featurep 'xemacs))
    nil) ; XEmacs でサポートされない機能
   (t
    (window-body-height)))) ; emacs21 にはない

(defun skk-screen-column ()
  "スクリーン行から得たカーソル位置の桁数を返す。
テキスト行（改行文字で区切られたテキスト）がウィンドウ幅を越えて折り返して表示
されている場合にも対応する。"
  (- (current-column)
     (save-excursion
       (vertical-motion 0)		;スクリーン行の行頭に移動する
       (current-column))))		;↑この結果、スクリーン行の行頭なのか
					;テキスト行の行頭なのか

(defun skk-move-to-screen-column (col)
  "スクリーン行から見た COL 桁位置にポイントを移動する。
テキスト行（改行文字で区切られたテキスト）がウィンドウ幅を越えて折り返して表示
されている場合にも対応するが、改行文字を越える移動は行わない。"
  (move-to-column (+ (current-column)	;テキスト行から見た桁数
		     (- col (skk-screen-column))))
  (skk-screen-column))

(defun skk-max-string-width (list)
  "LIST の各要素の中から、最長のコラム幅 (string-width) を返す。"
  (apply 'max (mapcar 'string-width list)))

(defun skk-insert-prefix (&optional char)
  "`skk-echo' が non-nil であればカレントバッファに `skk-prefix' を挿入する。"
  (when skk-echo
    ;; skk-prefix はアンドゥの対象とならないので
    ;; バッファに挿入される必要がない。
    (setq skk-prefix-overlay (make-overlay (point) (point)))
    (let ((prefix (or char skk-prefix)))
      (when (and skk-use-face (not skk-henkan-mode))
	(setq prefix
	      (propertize prefix 'face
			  (cond ((and skk-j-mode (not skk-katakana))
				 'skk-prefix-hiragana-face)
				(skk-katakana
				 'skk-prefix-katakana-face)
				(skk-jisx0201-mode
				 'skk-prefix-jisx0201-face)))))
      (overlay-put skk-prefix-overlay 'after-string prefix))))

(defsubst skk-string-lessp-in-coding-system (str1 str2 coding-system)
  (string< (encode-coding-string str1 coding-system)
	   (encode-coding-string str2 coding-system)))

(defun skk-string< (str1 str2)
  "STR1 と STR2 とを比較する。
内部コードが emacs-mule でないなど `stringp' の返り値が異なる Emacs に
対して emacs-mule の encoded string に変換して比較する。
比較の結果 str1 < str2 ならば t を返す。"
  (cond
   ((eval-when-compile (and (featurep 'emacs)
			    (>= emacs-major-version 23)))
    ;; Emacs with coding system utf-8-emacs
    (skk-string-lessp-in-coding-system str1 str2 'emacs-mule))
   (t
    (string< str1 str2))))

(defsubst skk-string<= (str1 str2)
  "STR1 と STR2 とを比較して、`string<' か `string=' であれば、t を返す。"
  (or (skk-string< str1 str2)
      (string= str1 str2)))

(defsubst skk-do-auto-fill ()
  "`auto-fill-function' に値が代入されていれば、それをコールする。"
  (when auto-fill-function
    (funcall auto-fill-function)))

(defsubst skk-current-input-mode ()
  (cond (skk-abbrev-mode 'abbrev)
	(skk-latin-mode 'latin)
	(skk-jisx0208-latin-mode 'jisx0208-latin)
	(skk-katakana 'katakana)
	(skk-j-mode 'hiragana)))

;;(defsubst skk-substring-head-character (string)
;;  (char-to-string (string-to-char string)))

(defsubst skk-get-current-candidate-1 (&optional count)
  (setq count (or count (skk-henkan-count)))
  (when (> 0 count)
    (skk-error "候補を取り出すことができません"
	       "Cannot get current candidate"))
  ;; (nth -1 '(A B C)) は、A を返すので、負でないかどうかチェックする。
  (nth count skk-henkan-list))

;; convert skk-rom-kana-rule-list to skk-rule-tree.
;; The rule tree follows the following syntax:
;; <branch-list>    ::= nil | (<tree> . <branch-list>)
;; <tree>         ::= (<char> <prefix> <nextstate> <kana> <branch-list>)
;; <kana>         ::= (<ひらがな文字列> . <カタカナ文字列>) | nil
;; <char>         ::= <英小文字>
;; <nextstate>    ::= <英小文字文字列> | nil

(defsubst skk-make-raw-arg (arg)
  (case arg
    (1 nil)
    (-1 '-)
    (t (if (numberp arg) (list arg) nil))))

(defsubst skk-unread-event (event)
  "Unread single EVENT."
  (setq unread-command-events
	(nconc unread-command-events (list event))))

(defsubst skk-get-last-henkan-datum (key)
  (cdr (assq key skk-last-henkan-data)))

;;; 2013-3-18 どこからも参照されておらず
;; (defsubst skk-put-last-henkan-datum (key val)
;;   (let ((e (assq key skk-last-henkan-data)))
;;     (if e
;; 	(setcdr e val)
;;       (push (cons key val) skk-last-henkan-data))))

(defun skk-put-last-henkan-data (alist)
  (let (e)
    (dolist (kv alist)
      (if (setq e (assq (car kv) skk-last-henkan-data))
	  (setcdr e (cdr kv))
	(push (cons (car kv) (cdr kv))
	      skk-last-henkan-data)))))

(defun skk-find-coding-system (code)
  "CODE が、Emacs が解釈する coding-system シンボル表現であればそのまま返し、
文字列であれば連想リスト `skk-coding-system-alist' を用いてシンボル表現へ変換する。
これら以外（nil を含む）であればシンボル euc-jis-2004 (GNU Emacs 22 まではシンボル euc-jisx0213) を返す。"
  (cond ((and code
	      (or (and (fboundp 'coding-system-p) ; GNU Emacs
		       (coding-system-p code))
		  (and (fboundp 'find-coding-system) ; XEmacs
		       (symbolp code)
		       (find-coding-system code))))
	 code)
	;;
	((and code
	      (stringp code))
	 (cdr (assoc code skk-coding-system-alist)))
	;;
	(t
	 (cdr (assoc "euc" skk-coding-system-alist)))))

(defsubst skk-lisp-prog-p (string)
  "STRING が Lisp プログラムであれば、t を返す。"
  (let ((l (length string)))
    (and (> l 2)
	 (eq (aref string 0) ?\()
	 ;; second character is ascii or not.
	 (skk-ascii-char-p (aref string 1))
	 (eq (aref string (1- l)) ?\))      ; この行、もう不要かも
	 (ignore-errors
	   (= l (cdr (read-from-string string)))))))

(defun skk-eval-string (string)
  "Eval STRING as a lisp program and return the result."
  (cond ((string-match ";" string)
	 (concat (skk-eval-string (substring string 0 (match-beginning 0)))
		 (substring string (match-beginning 0) (match-end 0))
		 (skk-eval-string (substring string (match-end 0)))))
	((skk-lisp-prog-p string)
	 (let (func face)
	   ;; (^_^;) のような文字列に対し、read-from-string を呼ぶと
	   ;; エラーになるので、ignore-errors で囲む。
	   (ignore-errors
	     (setq func (car (read-from-string string)))
	     (when (and (listp func)
			(functionp (car func)))
	       (setq face (get-text-property 0 'face string))
	       (setq string (if face
				(propertize (format "%s" (eval func))
					    'face face)
			      (format "%s" (eval func))))))
	   string))
	(t
	 string)))

;;;; from dabbrev.el.  Welcome!
;; 判定間違いを犯す場合あり。要改良。
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)))

(defun skk-quote-char-1 (word alist)
  (mapconcat (lambda (char)
	       (or (cdr (assq char alist))
		   (char-to-string char)))
   ;; 文字列を対応する char のリストに分解する。
	     (append word nil) ""))

(defun skk-key-binding-member (key commands &optional map)
  "入力 KEY が発動するコマンドが、COMMANDS に含まれれば non-nil を返す。
MAP は入力が書かれているキーマップを指定するが、指定されなければ
`skk-j-mode-map' を参照する。
この関数は、入力 KEY が `lookup-key' で探せない形式でありうる場合に用いる。"
  (unless map
    (setq map skk-j-mode-map))
  (let (keys)
    (dolist (command commands)
      (setq keys (nconc keys
			(where-is-internal command map))))
    (member (key-description key)
	    (mapcar #'key-description
		    keys))))

(defun skk-update-minor-mode-map-alist (mode map)
  (let ((element (assq mode minor-mode-map-alist)))
    (if element
	(setcdr element map)
      (add-to-list 'minor-mode-map-alist (cons mode map)))))

;; Functions from alist.el (APEL)
(defun skk-put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
	(progn
	  (setcdr elm value)
	  alist)
      (cons (cons key value) alist))))

(defun skk-del-alist (key alist)
  "Delete an element whose car equals KEY from ALIST.
Return the modified ALIST."
  (let ((pair (assoc key alist)))
    (if pair
	(delq pair alist)
      alist)))

(defun skk-remove-alist (symbol key)
  "Delete an element whose car equals KEY from the alist bound to SYMBOL."
  (and (boundp symbol)
       (set symbol (skk-del-alist key (symbol-value symbol)))))

(defun skk-fit-window (&optional window)
  "カレントウィンドウ (又は WINDOW) を、その表示内容に応じた高さに調節する。"
  (unless (eval-when-compile (and (featurep 'xemacs)
				  (= emacs-major-version 21)
				  (<= emacs-minor-version 4)))
    ;; XEmacs 21.4 にはない関数
    (fit-window-to-buffer window)))

(defun skk-reset-henkan-count (count)
  ;; ▽モードに戻るときは 0
  ;; ▼モードのまま候補一覧の手前に戻るときは 4
  (skk-set-henkan-count count)
  (skk-unread-event (character-to-event
		     (aref (car (where-is-internal
				 'skk-previous-candidate
				 skk-j-mode-map))
			   0))))

(defun skk-escape-from-show-candidates (count)
  ;; skk-henkan まで一気に throw する。
  (skk-reset-henkan-count count)
  (throw 'unread nil))

(provide 'skk-macs)

;;; skk-macs.el ends here
