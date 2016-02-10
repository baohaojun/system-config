;;; skk-isearch.el --- isearch mode for SKK -*- coding: iso-2022-jp -*-

;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999
;;   Enami Tsugutomo <enami@ba2.so-net.or.jp>

;; Author: Enami Tsugutomo <enami@ba2.so-net.or.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-isearch.el,v 1.80 2013/11/02 05:29:53 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2013/11/02 05:29:53 $

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

;; Hook functions are defined in skk-setup.el.
;;
;; 1. always invoke skk isearch.
;;
;; (setq skk-isearch-mode-enable 'always)
;;
;; 2. invoke only if skk-mode is on.
;;
;; (setq skk-isearch-mode-enable t)
;;
;; 3. invoke if current buffer has japanese characters.
;; ...
;;
;; skk-isearch-initial-mode examine the variable of skk before calling
;; skk-mode.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-macs))
(require 'skk)
(require 'skk-vars)

;; interface to skk.el
;;
(defsubst skk-isearch-turn-off-skk-mode ()
  "Turn off skk mode."
  (let ((skk-use-color-cursor nil))
    (skk-mode 0)))

(defsubst skk-isearch-turn-on-skk-mode ()
  "Turn on skk mode."
  (let ((skk-use-color-cursor nil))
    (skk-mode 1)))

(defsubst skk-isearch-conversion-active-p ()
  "Non-nil if skk conversion is active."
  skk-henkan-mode)

(defsubst skk-isearch-conversion-start ()
  "Point where conversion is start.  Includes skk marker."
  (1- skk-henkan-start-point))

(defsubst skk-isearch-skk-kakutei ()
  "Perform kakutei."
  (let ((skk-use-color-cursor nil))
    (skk-kakutei)))

(defsubst skk-isearch-skk-hiragana-mode-p ()
  "Non-nil if skk is hiragana input mode."
  (and (not skk-katakana) skk-j-mode))

(defsubst skk-isearch-skk-turn-on-hiragana-mode ()
  "Set current skk mode to hiragana input mode."
  (let ((skk-use-color-cursor nil))
    (skk-j-mode-on)))

(defsubst skk-isearch-skk-katakana-mode-p ()
  "Non-nil if skk is katakana input mode."
  (and skk-j-mode skk-katakana))

(defsubst skk-isearch-skk-turn-on-katakana-mode ()
  "Set current skk mode to katakana input mode."
  (let ((skk-use-color-cursor nil))
    (skk-j-mode-on 'katakana)))

(defsubst skk-isearch-skk-jisx0208-latin-mode-p ()
  "Non-nil if skk is jisx0208 latin (zenkaku) input mode."
  skk-jisx0208-latin-mode)

(defsubst skk-isearch-skk-abbrev-mode-p ()
  "Non-nil if skk is Abbrev mode."
  skk-abbrev-mode)

(defsubst skk-isearch-skk-turn-on-jix0208-latin-mode ()
  "Set current skk mode to jisx0208 latin (zenkaku) input mode."
  (let ((skk-use-color-cursor nil))
    (skk-jisx0208-latin-mode-on)))

(defsubst skk-isearch-skk-turn-on-latin-mode ()
  "Set current skk mode to normal latin input mode."
  (let ((skk-use-color-cursor nil))
    (skk-latin-mode-on)))

(defun skk-isearch-buffer-string ()
  (if (and skk-echo
	   skk-prefix)
      (concat (buffer-string) skk-prefix)
    (buffer-string)))

;;;###autoload
(defun skk-isearch-message ()
  "Show isearch message."
  (skk-isearch-incomplete-message
   (if (string= skk-prefix "")
       (skk-char-to-unibyte-string (skk-last-command-char))
     skk-prefix)))

(defun skk-isearch-current-mode ()
  "Return the symbolic current mode of skk for skk-isearch."
  (cond ((not skk-mode) nil)
	((skk-isearch-skk-katakana-mode-p) 'katakana)
	((skk-isearch-skk-hiragana-mode-p) 'hiragana)
	((skk-isearch-skk-jisx0208-latin-mode-p) 'jisx0208-latin)
	((skk-isearch-skk-abbrev-mode-p) 'abbrev)
	(t 'latin)))

(defun skk-isearch-set-initial-mode (mode)
  "Set up the initial condition according to given symbolic MODE.
The MODE should be canonical."
  ;; following code is highly depends on internal of skk.
  ;; (skk-isearch-turn-on-skk-mode)
  ;; (skk-isearch-skk-kakutei)
  (case mode
    (hiragana
     (skk-isearch-skk-turn-on-hiragana-mode))
    (katakana
     (skk-isearch-skk-turn-on-katakana-mode))
    (jisx0208-latin
     (skk-isearch-skk-turn-on-jix0208-latin-mode))
    (latin
     (skk-isearch-skk-turn-on-latin-mode))
    (t
     (skk-isearch-turn-off-skk-mode))))


(defun skk-isearch-symbolic-mode (mode)
  "Return symbolic skk isearch mode for given numerical MODE."
  (car (rassq mode skk-isearch-mode-canonical-alist)))

(defun skk-isearch-numerical-mode (mode)
  "Return numerical skk isearch mode for given symbolic MODE."
  (cdr (assq mode skk-isearch-mode-canonical-alist)))

(defun skk-isearch-mode-string ()
  "Return the current skk mode string for prompting."
  (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
    (cdr (assq (skk-isearch-current-mode) skk-isearch-mode-string-alist))))

(defun skk-isearch-current-numerical-mode ()
  "Return the symbolic skk isearch mode according to the current skk
internal condition."
  (skk-isearch-numerical-mode (or (skk-isearch-current-mode) 'latin)))

(defun skk-isearch-canonical-start-mode (mode)
  "Canonicalize the symbolic skk isearch MODE."
  ;; alias, canonical, or error.
  (cond ((cdr (rassq mode skk-isearch-mode-alias-alist)))
	((cdr (assq mode skk-isearch-mode-alias-alist)))
	((skk-isearch-numerical-mode mode) mode)
	(t (error "Unknown skk-isearch-start-mode: %s" mode))))

(defun skk-isearch-initial-mode ()
  "Return a symbol that represents a skk-isearch mode name.
It is used to initialize the working buffer."
  (cond ((and skk-isearch-use-previous-mode skk-isearch-mode)
	 ;; use the mode when last isearch is done.  note that the
	 ;; `skk-isearch-mode' is numerical, so convert it to symbolic
	 ;; mode.
	 (skk-isearch-symbolic-mode skk-isearch-mode))
	(skk-isearch-start-mode
	 ;; always start with the specified mode.
	 ;; `skk-isearch-start-mode' is symbolic.
	 (skk-isearch-canonical-start-mode skk-isearch-start-mode))
	(skk-isearch-state
	 ;; after `isearch-edit-string'.
	 skk-isearch-state)
	;; guess the current buffer.  note that if skk-mode is off,
	;; skk-isearch-current-mode returns symbol `nil' and control
	;; falls through to next cond clause.
	((skk-isearch-current-mode))
	;; skk-mode is off in this buffer.
	(t skk-isearch-initial-mode-when-skk-mode-disabled)))

(defun skk-isearch-initialize-working-buffer ()
  "Initialize the current buffer as working buffer for skk isearch.
More precicely, turn on skk-mode, put into kana mode, make sure
kakutei'ed and erase the buffer contents."
  (skk-isearch-turn-on-skk-mode)
  (skk-isearch-skk-kakutei)
  (make-local-variable 'skk-dcomp-activate)
  (setq skk-dcomp-activate nil)
  (erase-buffer))

;;;###autoload
(defun skk-isearch-mode-setup ()
  "hook function called when skk isearch begin."
  ;; setup working buffer.  initial skk mode for isearch should be
  ;; determined in the original buffer and set in working buffer.
  (let ((initial (skk-isearch-initial-mode)))
    (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
      (skk-erase-prefix 'clean)
      (skk-isearch-initialize-working-buffer)
      (skk-isearch-set-initial-mode initial)))
  ;; setup variables and keymap
  (unless (keymapp skk-isearch-mode-map)
    (cond
     ((eval-when-compile (featurep 'xemacs))
      (setq skk-isearch-mode-map (skk-isearch-setup-keymap
				  (make-keymap)))
      (set-keymap-parents skk-isearch-mode-map
			  isearch-mode-map))
     (t
      (setq skk-isearch-mode-map
	    (skk-isearch-setup-keymap (cons 'keymap
					    isearch-mode-map))))))
  (set skk-isearch-overriding-local-map skk-isearch-mode-map)
  ;; Input Method として SKK を使っている場合の対策
  (when (eval-when-compile (featurep 'emacs))
    (when (and current-input-method
	       (string-match "^japanese-skk" current-input-method))
      (let* ((method current-input-method)
	     (func (if (string= "japanese-skk" method)
		       'skk-inactivate
		     'skk-auto-fill-inactivate)))
	(with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	  (unless current-input-method
	    (skk-set-deactivate-im-func func)
	    (setq current-input-method method))))))
  ;; skk-isearch の状態を表す内部変数の設定
  (setq skk-isearch-switch t)
  (setq skk-isearch-in-editing nil)
  (setq skk-isearch-current-buffer (current-buffer))
  ;;
  (setq skk-isearch-incomplete-message ""
	;; set skk-isearch-message non-nil to call skk-isearch-message.
	skk-isearch-message "")
  (skk-isearch-mode-message)
  (skk-isearch-incomplete-message))

;;;###autoload
(defun skk-isearch-mode-cleanup ()
  "Hook function called when skk isearch is done."
  ;; remember the current skk mode for next use.
  (let ((mode (skk-current-input-mode)))
    (when skk-isearch-use-previous-mode
      (setq skk-isearch-mode
	    (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	      (skk-isearch-current-numerical-mode))))
    ;; reset the overrinding-local-map.
    (set skk-isearch-overriding-local-map nil)
    (setq skk-isearch-message nil
	  skk-isearch-last-mode-string ""
	  skk-isearch-last-mode-regexp "")
    ;; サーチ中に入力モードを変更したら、モードラインの表示もそれに従い
    ;; 変更されるので、カレントバッファの入力モードとモードラインの表示
    ;; とが sync しなくなる。従い、サーチが終了した際、モードラインをカ
    ;; レントバッファの入力モードと sync させる。
    (case mode
      (hiragana
       (skk-j-mode-on))
      (katakana
       (skk-j-mode-on t))
      (abbrev
       (skk-abbrev-mode-on))
      (latin
       (skk-latin-mode-on))
      (jisx0208-latin
       (skk-jisx0208-latin-mode-on))))
  ;; Input Method として SKK を使っている場合の対策
  (when (eval-when-compile (featurep 'emacs))
    (when (string-match "^japanese-skk" (format "%s" default-input-method))
      (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	(skk-deactivate-input-method))))
  ;; skk-isearch の状態を表す内部変数の設定
  (setq skk-isearch-switch nil)
  (unless skk-isearch-in-editing
    (setq skk-isearch-state nil))
  (setq skk-isearch-current-buffer nil)
  ;;
  (skk-remove-minibuffer-setup-hook 'skk-j-mode-on
				    'skk-setup-minibuffer
				    'skk-add-skk-pre-command))

(defun skk-isearch-incomplete-message (&optional prefix)
  "Show message when kana kanji conversion is in progress.
Optional argument PREFIX is appended if given."
  (let ((isearch-message (concat isearch-message
				 skk-isearch-incomplete-message
				 prefix)))
    (isearch-message)))

;;
;; define keymap
;;

(defun skk-isearch-find-keys-define (map commands command)
  ;; COMMANDS のいずれかにバインドされているキーを全て調べる。
  ;; skk-isearh の中でそれらのキーを COMMAND にバインドする。
  (let (prefs)
    (dolist (c commands)
      (dolist (key (where-is-internal c (current-global-map)))
	(when (and (= (length key) 2)
		   (not (member (aref key 0) prefs)))
	  (define-key map (vector (aref key 0)) (make-sparse-keymap))
	  (push (aref key 0) prefs))
	(when (<= (length key) 2)
	  (define-key map key command))))))

;; XXX should be more generic
(defun skk-isearch-setup-keymap (map)
  ;; printable chars.
  (do ((c ?\040 (1+ c)))
      ((>= c ?\177))
    (define-key map (skk-char-to-unibyte-string c) 'skk-isearch-wrapper))

  (when skk-j-mode-function-key-usage
    (define-key map [f1] 'skk-isearch-wrapper)
    (define-key map [f2] 'skk-isearch-wrapper)
    (define-key map [f3] 'skk-isearch-wrapper)
    (define-key map [f4] 'skk-isearch-wrapper)
    (define-key map [f5] 'skk-isearch-wrapper)
    (define-key map [f6] 'skk-isearch-wrapper)
    (define-key map [f7] 'skk-isearch-wrapper)
    (define-key map [f8] 'skk-isearch-wrapper)
    (define-key map [f9] 'skk-isearch-wrapper)
    (define-key map [f10] 'skk-isearch-wrapper))

  ;; control chars for skk.
  (define-key map "\C-g" 'skk-isearch-keyboard-quit)
  (define-key map skk-kakutei-key 'skk-isearch-newline)
  (define-key map "\C-m" 'skk-isearch-exit)
  (dolist (key skk-previous-candidate-keys)
    (define-key map key 'skk-isearch-wrapper))

  ;; C-x map for skk.
  (define-key map "\C-x" (make-sparse-keymap))

  ;; Keys for `skk-isearch-skk-mode'.
  (let ((commands '(skk-mode skk-auto-fill-mode)))
    (unless (eval-when-compile (featurep 'xemacs))
      (when (string-match "^japanese-skk" (format "%s" default-input-method))
	(push 'toggle-input-method commands)))
    (skk-isearch-find-keys-define map commands 'skk-isearch-skk-mode))

  (unless (eval-when-compile (featurep 'xemacs))
    ;; XEmacs にはないコマンド
    (if (fboundp 'isearch-other-control-char)			;2013-10-08 Remove functions
	(define-key map [?\C-x t] 'isearch-other-control-char)) ; GNU Emacs 24.4 から廃止

    (define-key map [?\C-0] 'skk-isearch-start-henkan)
    (define-key map [?\C-1] 'skk-isearch-start-henkan)
    (define-key map [?\C-2] 'skk-isearch-start-henkan)
    (define-key map [?\C-3] 'skk-isearch-start-henkan)
    (define-key map [?\C-4] 'skk-isearch-start-henkan)
    (define-key map [?\C-5] 'skk-isearch-start-henkan)
    (define-key map [?\C-6] 'skk-isearch-start-henkan)
    (define-key map [?\C-7] 'skk-isearch-start-henkan)
    (define-key map [?\C-8] 'skk-isearch-start-henkan)
    (define-key map [?\C-9] 'skk-isearch-start-henkan)
    (define-key map [?\M-0] 'skk-isearch-start-henkan)
    (define-key map [?\M-1] 'skk-isearch-start-henkan)
    (define-key map [?\M-2] 'skk-isearch-start-henkan)
    (define-key map [?\M-3] 'skk-isearch-start-henkan)
    (define-key map [?\M-4] 'skk-isearch-start-henkan)
    (define-key map [?\M-5] 'skk-isearch-start-henkan)
    (define-key map [?\M-6] 'skk-isearch-start-henkan)
    (define-key map [?\M-7] 'skk-isearch-start-henkan)
    (define-key map [?\M-8] 'skk-isearch-start-henkan)
    (define-key map [?\M-9] 'skk-isearch-start-henkan))

  ;; Keys for `skk-isearch-delete-char'.
  (let ((commands '(backward-delete-char-untabify
		    backward-delete-char
		    backward-or-forward-delete-char
		    delete-backward-char)))
    (skk-isearch-find-keys-define map commands 'skk-isearch-delete-char))
  ;;
  map)


;;
;; wrapper functions
;;

(defun skk-isearch-redo-function ()
  "Execute the command of given key sequence in skk environment."
  ;; with saving value of old binding.
  (let ((local-map (symbol-value skk-isearch-overriding-local-map)))
    (unwind-protect
	(progn
	  ;; temporarily disable the overriding-local-map.  this
	  ;; should be done in ther buffer isearch is performed, i.e.,
	  ;; before entering skk-isearch-working-buffer.
	  (set skk-isearch-overriding-local-map nil)
	  ;; don't change the current buffer during save/restore the
	  ;; overriding-local-map, because it is buffer local in some
	  ;; version of emacs.
	  (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	    ;; listify this-command-keys.  this works only if it is
	    ;; string.
	    (setq unread-command-events
		  (append (if (zerop (length (this-command-keys)))
			      (list last-command-event)
			    (this-command-keys))
			  nil))
	    (condition-case error
		;; setup last-command-event and this-command because
		;; some command refers them.
		(let* ((keys (read-key-sequence nil))
		       (this-command (key-binding keys))
		       ;; 直後の command-execute() にて、skk-insert() 経由で
		       ;; skk-dcomp-multiple-show() が実行されるとエラーとなってしまう
		       skk-dcomp-multiple-activate)
		  (setq last-command-event (aref keys (1- (length keys))))
		  (command-execute this-command))
	      ((quit error)
	       (signal (car error) (cdr error)))))
	  (skk-isearch-mode-message))
      (set skk-isearch-overriding-local-map local-map))))

(defun skk-isearch-search-string ()
  "Return the string to be searched.
If the conversion is in progress and no string is fixed, just return nil."
    (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
      (prog1
	  (cond ((skk-isearch-conversion-active-p)
		 (let ((start (skk-isearch-conversion-start)))
		   ;; is there fixed string?
		   (if (> start 1)
		       (prog1
			   (buffer-substring 1 start)
			 (delete-region 1 start)))))
		;; a conversion is in progress
		((not (string= skk-prefix ""))
		 nil)
		;;(skk-current-rule-tree nil)
		;; whole string in the buffer is fixed.
		((not (zerop (buffer-size)))
		 (prog1
		     (skk-isearch-buffer-string)
		   (erase-buffer))))
	;; update incomplete-message with contents of working buffer.
	(setq skk-isearch-incomplete-message (skk-isearch-buffer-string))
	;; update echo area.
	(skk-isearch-incomplete-message))))


;;
;; regexp search supports.
;;
(defun skk-isearch-last-char (string)
  (when (string-match ".\\'" string)
    (string-to-char (substring string (match-beginning 0)))))

(defun skk-isearch-breakable-p (char)
  (and char
       (funcall skk-isearch-breakable-character-p-function char)))

(defun skk-isearch-search-string-regexp (string)
  (if isearch-regexp
      (do ((prev (skk-isearch-last-char isearch-string) (car chars))
	   (result "" (concat result (char-to-string (car chars))))
	   (chars (string-to-list string) (cdr chars)))
	  ((null chars) result)
	(when (and (skk-isearch-breakable-p prev)
		   (skk-isearch-breakable-p (car chars)))
	  (setq result (concat result skk-isearch-whitespace-regexp))))
    ;; else
    string))

(defun skk-isearch-mode-message ()
  "Prepend the skk isearch mode string to `isearch-message'.
If the current mode is different from previous, remove it first."
  (let ((mode-string (skk-isearch-mode-string)))
    (unless (string= mode-string skk-isearch-last-mode-string)
      (if (string-match skk-isearch-last-mode-regexp isearch-message)
	  (setq isearch-message (substring isearch-message
					   (match-end 0))))
      (setq skk-isearch-last-mode-string mode-string
	    skk-isearch-last-mode-regexp (concat "^" (regexp-quote
						      mode-string)))
      (setq isearch-message (concat mode-string isearch-message)))))

(defun skk-isearch-process-search-string (string)
  (isearch-process-search-string
   (skk-isearch-search-string-regexp string)
   string))


;;
;; interactive functions.
;;
(defun skk-isearch-delete-char (&rest args)
  (interactive "P")
  (unless (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	    ;; following code is highly depends on internal of skk.
	    (when (skk-isearch-conversion-active-p)
	      (prog1
		  t
		(cond
		 ((save-excursion
		    (ignore-errors
		      (goto-char (point-max)))
		    (>= skk-henkan-start-point (point)))
		  (setq skk-henkan-count 0)
		  (skk-kakutei)
		  (isearch-message))
		 ((and (eq skk-henkan-mode 'active)
		       skk-delete-implies-kakutei)
		  (if (eq skk-delete-implies-kakutei 'dont-update)
		      (let ((skk-update-jisyo-function #'ignore))
			(skk-kakutei))
		    (skk-kakutei))
		  (delete-char -1)
		  (setq isearch-string (concat isearch-string
					       (skk-isearch-buffer-string))
			isearch-message (concat
					 (skk-isearch-mode-string)
					 (mapconcat
					  #'isearch-text-char-description
					  isearch-string "")))
		  (isearch-push-state)
		  (isearch-update)
		  (erase-buffer))
		 (t
		  (cond
		   ((eq skk-henkan-mode 'active)
		    ;; In this case, `skk-delete-implies-kakutei' is nil.
		    (skk-previous-candidate))
		   ((string= skk-prefix "")
		    ;; now, we can't pass the universal argument within
		    ;; the isearch-mode.  so hard code the value `1'.
		    (delete-char -1))
		   (t
		    (skk-erase-prefix 'clean)))
		  (setq skk-isearch-incomplete-message
			(skk-isearch-buffer-string))
		  (skk-isearch-incomplete-message))))))

    ;; isearch-cmds ... Stack of search status sets.
    ;;   '(["test" "test" 198 198 t 194 nil nil nil 192 t nil]
    ;;     ["tes" "tes" 197 197 t 194 nil nil nil 192 t nil]
    ;;     ["te" "te" 196 196 t 194 nil nil nil 192 t nil]
    ;;     ["t" "t" 195 195 t 194 nil nil nil 192 t nil]
    ;;     ["" "" 192 t t nil nil nil nil 192 t nil])
    ;;   or
    ;;   '([cl-struct-isearch--state "test" "[aa] test" 196 196 t ..]
    ;;     [cl-struct-isearch--state "tes" "[aa] tes" 195 195 t ..]
    ;;     [cl-struct-isearch--state "te" "[aa] te" 102 102 t ..]
    ;;     [cl-struct-isearch--state "t" "[aa] t" 92 92 t ..]
    ;;     [cl-struct-isearch--state "" "[か] " 78 t t ..]
    (let* ((cmd (nth 1 isearch-cmds))
	   (oldmsg (cond ((null cmd)
			  "")
			 ((stringp (aref cmd 0))
			  (aref cmd 1))	  ; GNU Emacs 24.2 まで
			 (t
			  (aref cmd 2)))) ; GNU Emacs 24.3 から
	   (prompt (skk-isearch-mode-string))
	   newmsg)
      (unless (or (null cmd)
		  (string-match (concat "^" (regexp-quote prompt))
				oldmsg))
	;; `skk-isearch-delete-char' が呼ばれる前に `skk-isearch-working-buffer'
	;; 内のモードが切り替えられていた場合、 isearch-cmds の第 2 要素につい
	;; て、 messege の内容を update しないと [DEL] したときのモードの表示が
	;; おかしくなる。
	(do ((alist skk-isearch-mode-string-alist (cdr alist))
	     (msg nil (when (string-match
			     (concat "^" (regexp-quote (cdar alist)))
			     oldmsg)
			(substring oldmsg (match-end 0)))))
	    ((or msg (null alist))
	     (setq newmsg (concat prompt (or msg oldmsg)))
	     (if (stringp (aref cmd 0))
		 (aset cmd 1 newmsg)	;GNU Emacs 24.2 まで
	       (aset cmd 2 newmsg))	;GNU Emacs 24.3 から
	     ))))
    (isearch-delete-char)))

(defun skk-isearch-kakutei (isearch-function)
  "Special wrapper for skk-kakutei or newline."
  (if (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	;; following code is highly depends on internal of skk.
	(when (skk-isearch-conversion-active-p)
	  (prog1
	      t
	    (skk-isearch-skk-kakutei))))
      (skk-isearch-process-search-string (skk-isearch-search-string))
    (funcall isearch-function)))

(defun skk-isearch-exit (&rest args)
  (interactive "P")
  (skk-isearch-kakutei #'isearch-exit))

(defun skk-isearch-newline (&rest args)
  (interactive "P")
  ;; following code is highly depends on internal of skk.
  (cond
   ((with-current-buffer (get-buffer-create skk-isearch-working-buffer)
      (when (memq (skk-isearch-current-mode)
		  '(latin jisx0208-latin nil))
	(prog1
	    t
	  ;; if the working buffer is latin or jisx0208-latin
	  ;; mode, default behaviour of C-j is set current mode
	  ;; to kana mode.
	  (skk-isearch-turn-on-skk-mode)
	  (skk-isearch-mode-message))))
    (isearch-message))
   ((event-to-character last-command-event)
    (skk-isearch-kakutei #'isearch-printing-char))
   (t
    (skk-isearch-mode-message)
    (isearch-message))))

;;;###autoload
(defun skk-isearch-skk-mode (&rest args)
  (interactive "P")
  (skk-isearch-redo-function)
  (isearch-message))

(defun skk-isearch-keyboard-quit (&rest args)
  (interactive "P")
  (condition-case ()
      (progn
	(skk-isearch-redo-function)
	;; update echo area message.
	(skk-isearch-search-string))
    (quit
     (isearch-abort))))

(defun skk-isearch-wrapper (&rest args)
  (interactive "P")
  (skk-isearch-redo-function)
  (skk-isearch-wrapper-1))

(defun skk-isearch-wrapper-1 ()
  (let ((string (skk-isearch-search-string)))
    (when string ; nil means on the way to converting to kanji.
      ;; with saving value of old binding...
      (let ((local-map (symbol-value skk-isearch-overriding-local-map))
	    (current-buffer (current-buffer)))
	;; because the overrinding local map may be buffer local, keep the
	;; current buffer, but we can't use save-excursion. ...
	(unwind-protect
	    (progn
	      (set skk-isearch-overriding-local-map isearch-mode-map)
	      (let ((command (key-binding string)))
		(cond
		 ((not (commandp command))
		  ;; just search literally.
		  (skk-isearch-process-search-string string))
		 ;; internationalized isearch.el
		 ((fboundp 'isearch-process-search-multibyte-characters)
		  ;; internationalized isearch.el binds all
		  ;; multibyte characters to `isearch-printing-char'.
		  (skk-isearch-process-search-string string))
		 ;; non internationalized isearch.el
		 (t
		  (command-execute command)))))
	  ;; restore the overriding local map.
	  (set-buffer current-buffer)
	  (set skk-isearch-overriding-local-map local-map))))))

(defun skk-isearch-start-henkan (&optional digit last-event)
  "skk-isearch の▽モードで変換を開始する。
このコマンドは digit-argument の 0-9 に対応するキーに割り当てられる。
変換には skk-search-prog-list の代わりに skk-search-prog-list-{0-9}
が参照される。"
  (interactive)
  (let ((digit (or digit
		   (- (logand (skk-last-command-char) ?\177) ?0)))
	(event (next-command-event nil (skk-isearch-incomplete-message))))
    (cond
     ((equal event (character-to-event ?\ ))
      ;; XEmacs では eq にはならない
      (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
	(when (eq skk-henkan-mode 'on)
	  (skk-bind-last-command-char skk-start-henkan-char
	    (skk-start-henkan 1 digit))))
      (skk-isearch-mode-message)
      (skk-isearch-wrapper-1))

     (t
      (cond
       ((eval-when-compile (featurep 'xemacs))
	(let ((search-nonincremental-instead nil))
	  (isearch-exit))
	(skk-unread-event last-event)
	;; XXX なぜ 2 回 unread する...?
	(skk-unread-event event)
	(skk-unread-event event))

       (t
	(skk-unread-event event)
	(if (fboundp 'isearch-other-control-char) ; 2013-10-08 Remove functions.
	    (isearch-other-control-char)))	  ; GNU Emacs 24.4 から廃止
	)))))


;;
;; advices.
;;

(defadvice isearch-repeat (after skk-isearch-ad activate compile)
  "`isearch-message' を適切に設定する。"
  (when skk-isearch-switch
    (unless (string-match (concat "^" (regexp-quote (skk-isearch-mode-string)))
			  isearch-message)
      (setq isearch-message
	    (concat
	     (skk-isearch-mode-string)
	     (mapconcat #'isearch-text-char-description isearch-string "")))
      (setq isearch-cmds (cdr isearch-cmds))
      (isearch-push-state)
      (isearch-update))
    ;;
    (when isearch-regexp
      (let ((regexp
	     (regexp-quote
	      (mapconcat 'isearch-text-char-description
			 skk-isearch-whitespace-regexp
			 ""))))
	(when (string-match regexp isearch-message)
	  (setq isearch-message (replace-regexp-in-string regexp ""
							  isearch-message))
	  (setq isearch-cmds (cdr isearch-cmds))
	  (isearch-push-state)
	  (isearch-update))))))

(defadvice isearch-edit-string (before skk-isearch-ad activate compile)
  "`isearch-message' を適切に設定する。"
  (when skk-isearch-switch
    (with-current-buffer (get-buffer-create skk-isearch-working-buffer)
      (setq skk-isearch-state (skk-isearch-current-mode))
      (setq skk-isearch-in-editing t))
    (when (string-match (concat "^" (regexp-quote (skk-isearch-mode-string)))
			isearch-message)
      (setq isearch-message (substring isearch-message (match-end 0))))))

(defadvice isearch-search (before skk-isearch-ad activate compile)
  "`isearch-message' を適切に設定する。"
  (when skk-isearch-switch
    (unless (or isearch-nonincremental
		(string-match (concat "^" (regexp-quote
					   (skk-isearch-mode-string)))
			      isearch-message))
      (setq isearch-message
	    (concat
	     (skk-isearch-mode-string)
	     (mapconcat 'isearch-text-char-description isearch-string ""))))))

(when (eval-when-compile (featurep 'xemacs))
  (defadvice digit-argument (around skk-isearch activate)
    "isearch 内で digit-argument を活用できるよう調整する。"
    (if (and skk-isearch-switch
	     (with-current-buffer skk-isearch-working-buffer
	       (eq skk-henkan-mode 'on)))
	(let* ((event last-command-event)
	       (key (and (key-press-event-p event)
			 (event-key event)))
	       (digit (and key (characterp key) (>= key ?0) (<= key ?9)
			   (- key ?0))))
	  (skk-isearch-start-henkan digit event))
      ad-do-it)))

;;; This advice will be enabled before skk-isearch is loaded.
;;;###autoload
(defconst skk-isearch-really-early-advice
  (lambda ()
    (defadvice isearch-message-prefix (around skk-isearch-ad activate)
      (let ((current-input-method
	     (unless (and (boundp 'skk-isearch-switch)
			  skk-isearch-switch)
	       current-input-method)))
	ad-do-it))
    (defadvice isearch-toggle-input-method (around skk-isearch-ad activate)
      ;; Needed for calling skk-isearch via isearch-x.
      (cond ((string-match "^japanese-skk"
			   (format "%s" default-input-method))
	     (let ((skk-isearch-initial-mode-when-skk-mode-disabled
		    'latin))
	       (skk-isearch-mode-setup)
	       (skk-isearch-skk-mode)))
	    ((null default-input-method)
	     ad-do-it
	     (when (string-match "^japanese-skk"
				 (format "%s" default-input-method))
	       (let ((skk-isearch-initial-mode-when-skk-mode-disabled
		      'latin))
		 (skk-isearch-mode-setup))
	       (skk-deactivate-input-method)))
	    (t
	     ad-do-it)))))

;;;###autoload
(unless (featurep 'xemacs)
  (define-key isearch-mode-map [(control \\)] 'isearch-toggle-input-method)
  (cond
   ((and (featurep 'advice)
	 (assq 'skk-isearch-ad
	       (assq 'around
		     (ad-get-advice-info 'isearch-toggle-input-method))))
    ;; Already advised.
    nil)
   ((locate-library "advice")
    ;; Advise now.
    (funcall skk-isearch-really-early-advice))
   (t
    ;; Emacs 21 loads "leim-list" files before `load-path' is prepared.
    (add-hook 'before-init-hook skk-isearch-really-early-advice))))

(put 'digit-argument 'isearch-command t)
(if (fboundp 'isearch-other-control-char)		  ; 2013-10-08 Remove functions
    (put 'isearch-other-control-char 'isearch-command t)) ; GNU Emacs 24.4 から廃止
(put 'skk-isearch-delete-char 'isearch-command t)
(put 'skk-isearch-exit 'isearch-command t)
(put 'skk-isearch-keyboard-quit 'isearch-command t)
(put 'skk-isearch-newline 'isearch-command t)
(put 'skk-isearch-skk-mode 'isearch-command t)
(put 'skk-isearch-start-henkan 'isearch-command t)
(put 'skk-isearch-wrapper 'isearch-command t)

(provide 'skk-isearch)

;;; skk-isearch.el ends here
