;;; screen-lines.el --- a minor mode for screen-line-based point motion

;; Copyright (C) 2000, 2001, 2002, 2003, 2004
;; Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Filename:      screen-lines.el
;; Author:        Yuji Minejima <ggb01164@nifty.ne.jp>
;; Keywords:      convenience
;; Description:   a minor mode for screen-line-based point motion
;; Compatibility: GNU Emacs 20.7.2, XEmacs 21.1.12
;; URL: http://homepage1.nifty.com/bmonkey/emacs/elisp/screen-lines.el
;; $Revision: 0.55 $

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This package contains part of the code and the documentation of the
;; following packages:
;;
;;   simple.el --- basic editing commands for Emacs
;;   Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 1999
;;   Free Software Foundation, Inc.
;;
;;   window-lines.el --- basic cursor moving commands based on window lines
;;   Copyright (C) 1990 enami tsugutomo (enami@ptgd.sony.co.jp)
;;
;; All of these are distributed under GPL.
;; Thanks to the authors for writing these excellent packages.


;;; Commentary:

;; This package provides "Screen Lines" minor mode.
;; In this minor mode, the following standard commands work in terms of
;; screen lines, as opposed to text lines when they are called interactively.
;;
;;     `beginning-of-line' (C-a)
;;     `end-of-line'       (C-e)
;;     `next-line'         (C-n, down)
;;     `previous-line'     (C-p, up)
;;     `kill-line'         (C-k)
;;
;; Screen Lines minor mode should be handy when you edit a file with
;; long lines like this: <DT><A HREF="http://tiny-tools.sourceforge.net/emacs-keys.html" ADD_DATE="977149523" LAST_VISIT="977488726" LAST_MODIFIED="977149517">Emacs keybinding help page</A>
;;
;; In order to test this as you see this file, proceed as follows:
;;   1: Load this package:  M-x eval-buffer
;;   2: Toggle the mode for the current buffer: M-x screen-lines-mode
;;   3: Move around the above long line.
;;
;; You might want to check screenline.el written by Joshua E. Buhl
;; <josh@math.uni-bonn.de> which does text property business properly and
;; doesn't use `defadvice' facility.
;; Or window-lines.el by enami tsugutomo <enami@ptgd.sony.co.jp> which is
;; lightweight.


;; Text lines vs Screen lines:
;;                                             See "(elisp)Text Lines"
;;                                                 "(elisp)Screen Lines"
;;
;;   Screen lines are defined by the way the text appears horizontally
;;   on the screen, whereas text lines are defined in terms of newline
;;   characters as line delimiters.
;;   
;;   Emacs standard line oriented point moving commands such as
;;   `next-line' and `previous-line' are based on text lines.
;;
;;   A text line is a single screen line:
;;     * if it is short enough to fit the width of the selected window, or
;;     * if it is truncated on the screen as follows:
;;
;;          |                                       |
;;          |The quick brown fox jumped over the la$|
;;          |                                       |
;;
;;   A text line is divided into several screen lines:
;;     * if it is folded on the screen as follows:
;;
;;          |                                       |
;;          |The quick brown fox jumped over the la\|
;;          |zy dogs.<RET>                          |
;;          |                                       |
;;
;;       The above text line consists of the two screen lines,
;;       "The quick brown fox jumped over the la" and
;;       "zy dogs.<RET>".
;;


;;; Requirements:

;; Tested with FSF Emacs 20.7.2 and XEmacs 21.1.12 on Debian GNU/Linux woody.


;;; Install:

;; 1: Put this file in one of the directories listed in `load-path'.
;;    You can see the contents of `load-path' by entering
;;    `M-x customize-option <RET> load-path'.
;;
;; 2: Enter `M-x byte-compile-file <RET>
;;           <DIR-YOU-PUT-THIS-FILE-IN>/screen-lines.el<RET>'.
;;    This should speed up the execution of this package.
;;
;; 3: Put the following lines in your .emacs file.
;;
;;    (autoload 'screen-lines-mode "screen-lines"
;;              "Toggle Screen Lines minor mode for the current buffer." t)
;;    (autoload 'turn-on-screen-lines-mode "screen-lines"
;;              "Turn on Screen Lines minor mode for the current buffer." t)
;;    (autoload 'turn-off-screen-lines-mode "screen-lines"
;;              "Turn off Screen Lines minor mode for the current buffer." t)
;;
;;    ;; comment out the following lines if you want the original `kill-line'
;;    ;; in Screen Lines minor mode.
;;    ;;(add-hook 'screen-lines-load-hook
;;    ;;          #'(lambda ()
;;    ;;               (ad-disable-advice 'kill-line 'around 'screen-lines)
;;    ;;               (ad-activate 'kill-line)))
;;
;; 4: Restart Emacs.


;;; Activation:

;; * Enter `M-x screen-lines-mode' to toggle Screen Lines minor
;;   mode for the current buffer.
;;   You can also do `M-x turn-on-screen-lines-mode' or
;;   `M-x turn-off-screen-lines-mode'.
;;
;; * When the mode is enabled in a buffer, "SLines" should appear in
;;   the mode line (which sits at the bottom of the buffer's window).


;; Customization:

;; * Enter `M-x customize-group <RET> screen-lines' to customize
;;   this package. You might need to enter
;;   `M-x load-library <RET> screen-lines' in advance.
;;
;; * The `screen-lines-mode' option determines whether Screen Lines mode
;;   is enabled or disabled by default for all buffers.
;;
;; * This package respects the following standard options:
;;     `next-line-add-newlines'
;;     `track-eol'
;;     `goal-column'
;;     `kill-whole-line'
;;   You can customize these options by
;;   `M-x customize-group <RET> editing-basics' for the first three, and
;;   `M-x customize-group <RET> killing' for the last.
;;
;; * "(elisp)Activation of Advice" might be useful to fine-tune this package.


;;; Implementation notes:

;; All commands are based on the behavior of the standard `vertical-motion'
;; function. (But some commands partially use forward-line for outline-mode)
;;
;; The idea of using vertical-motion comes from window-lines.el by
;; enami tsugutomo (enami@ptgd.sony.co.jp).
;;
;; See tscreen-lines.txt (This is a private file, not distributed).
;; See "(elisp)Text Lines" and "(elisp)Screen Lines"
;; See "(elisp)Special Properties" for `Intangible' text property.
;; See "(elisp)Invisible Text" for `Invisible' text property.


;;; Change Log:

;; Version 0.55 (9 Jan 2004)
;;  * Modify Screen-lines-kill-line to work properly on GNU Emacs 21 when point
;;    is at the beginning of a screen line.

;; Version 0.52 (25 Dec 2000)
;;  * Advised `kill-line' to use `Screen-lines-kill-line'.
;;  * Changed the implementation of Screen-lines-beginning-of-line,
;;    Screen-lines-end-of-line, screen-lines-current-column and
;;    screen-lines-move-to-column to work more properly in outline-mode.
;;  * Added turn-on-screen-lines-mode and turn-off-screen-lines-mode
;;  * All the standard commands advised by this package work in terms of
;;    screen lines only if they are called interactively irrespective of
;;    the state of Screen Lines minor mode. (See `screen-lines-mode-p'.)
;;
;;    Hopefully, this should solve compatibility problems with other
;;    packages which call these commands, expecting their original behavior
;;    (e.g. fontlock-lock.el).
;;
;;    On the other hand, there are other packages which work more naturally
;;    if they are under the influence of screen lines mode
;;    (e.g. pc-select.el).
;;    So I introduced `screen-lines-mode-commands' variable for them.
;;  * Fixed a bug in screen-lines-move-to-column.
;;    When a line ended with a tab, and the tab stretched over to
;;    the next line by line folding, `Screen-lines-next-line' didn't
;;    move point.
;;  * Added enabled, disabled messages

;; Version 0.38 (13 Aug 2000):
;;  * Added the URL of the latest version.

;; Version 0.35 (5 Aug 2000):
;;  * Added some comments.
;;  * Added `screen-lines-load-hook'.

;; Version 0.32 (10 Jul 2000):
;;  * Changed to provide a minor mode.
;;  * Should work with pc-select.el now because of the change to a minor mode.

;; Version 0.21 (29 Jun 2000):
;;  * First public release of this package.


;;; Bugs:
;;  * inhibit-point-motion-hooks business is totally ignored.
;;  * pc-select.el's end-of-line-mark and end-of-line-nomark fail to work
;;    in terms of screen lines if they are byte-compiled.
;;    Maybe this is because `end-of-line' is a primitive byte-code?
;;    A workaround is included and enabled. (see the bottom of this file.)


;;; TODO:


;;; Acknowledgements:

;;  Knut <wannhedenk@post.ch> gave a solution to compatibility problems with
;;  other packages and suggested adding advice to kill-line.
;;  Pedja Raspopovic <pedja@lsil.com> reported a problem with pc-select.el
;;  Gregory A. Shimansky <gregory@mcst.ru> reported a workaround for
;;  pc-select.el problem.
;;

;;; Code:

;; Global variable declarations
(eval-when-compile
  (defvar goal-column)
  (defvar kill-whole-line)
  (defvar last-command)
  (defvar minor-mode-alist)
  (defvar next-line-add-newlines)
  (defvar temporary-goal-column)
  (defvar this-command)
  (defvar track-eol)
  (defvar zmacs-region-stays)           ; for XEmacs
  )


;;; Mode
(defgroup screen-lines nil
  "This package provides \"Screen Lines\" minor mode.
In this minor mode, the following standard commands work in terms of
screen lines, as opposed to text lines when they are called interactively.

    `beginning-of-line' (C-a)
    `end-of-line'       (C-e)
    `next-line'         (C-n, down)
    `previous-line'     (C-p, up)
    `kill-line'         (C-k)

Screen Lines minor mode should be handy when you edit a file with
long lines like this: <DT><A HREF=\"http://tiny-tools.sourceforge.net/emacs-keys.html\" ADD_DATE=\"977149523\" LAST_VISIT=\"977488726\" LAST_MODIFIED=\"977149517\">Emacs keybinding help page</A>

`M-x screen-lines-mode' toggles the mode for the current buffer."
  :group 'editing-basics
  :group 'convenience)

(defcustom screen-lines-mode nil
  "*Toggle Screen Lines minor mode for the current buffer.
This variable is buffer-local."
  :type 'boolean
  :require 'screen-lines
  :group 'screen-lines)
(make-variable-buffer-local 'screen-lines-mode)


(defcustom screen-lines-minor-mode-string " SLines"
  "*String to display in mode line when Screen Lines mode is enabled."
  :type 'string
  :group 'screen-lines)

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'screen-lines-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((screen-lines-mode
                             screen-lines-minor-mode-string)))))

;;;###autoload
(defun screen-lines-mode (&optional arg)
  "Toggle Screen Lines minor mode for the current buffer.
With ARG, turn the mode on if ARG is positive, off otherwise."
  (interactive "P")
  (setq screen-lines-mode
        (if (null arg) (not screen-lines-mode)
          (> (prefix-numeric-value arg) 0)))
  (when (interactive-p)
    (message "Screen Lines minor mode %s"
             (if screen-lines-mode "enabled" "disabled"))))

;;;###autoload
(defun turn-on-screen-lines-mode ()
  "Turn on Screen Lines minor mode for the current buffer."
  (interactive)
  (screen-lines-mode 1))


;;;###autoload
(defun turn-off-screen-lines-mode ()
  "Turn off Screen Lines minor mode for the current buffer."
  (interactive)
  (screen-lines-mode -1))


;; Turn on/off automatically for specific buffers and major modes.
(defvar screen-lines-mode-auto-alist
  '((dired-mode-hook . nil)             ; dired-next-line needs this
    (minibuffer-setup-hook . nil))      ; not necessarily needed
  "Alist of hook variable and the value of Screen Lines mode for it.")
(mapcar #'(lambda (assoc)
            (add-hook (car assoc)
                      `(lambda () (setq screen-lines-mode ,(cdr assoc)))))
        screen-lines-mode-auto-alist)


;;; Commands
(defsubst screen-lines-keep-region-active ()
  "Only for XEmacs."
  (when (boundp 'zmacs-region-stays)
    (setq zmacs-region-stays t)))

(defun Screen-lines-kill-line (&optional arg)
  "Screen Lines minor mode version of kill-line.
Mimic the original behavior with screen-line-oriented twist."
  (interactive "P")
  (when (eobp) (signal 'end-of-buffer nil))
  (let* ((pos (point))
         (beg (progn (Screen-lines-beginning-of-line) (point)))
         (beg-bolp (bolp))
         (beg-before-c (char-before))
         (end (progn (vertical-motion (if arg (prefix-numeric-value arg) 1))
                     (point)))
         (end-bolp (bolp))
         (end-eobp (eobp))
         (within-line (prog2 (forward-line 0)
                          (>= beg (point)) (goto-char end)))
         (pos-end (buffer-substring pos end))
         (add-nl (and (not arg)
                      (not (string-match "\\`[ \t]*$" pos-end))
                      (not (and kill-whole-line (= pos beg)))
                      (not (and (not end-bolp) end-eobp)))))
    (when (featurep 'xemacs) (goto-char pos))
    (kill-region pos end)
    (when (or (featurep 'xemacs) (<= emacs-major-version 20))
      (when (and (= pos beg) (not beg-bolp) (not (eq beg-before-c ?\t))
                 (or (not within-line)
                     end-eobp
                     (and kill-whole-line end-bolp)
                     (and (string-match "\\`[ \t]*$" pos-end) end-bolp)
                     add-nl))
        (insert "\n")))
    (when add-nl
      (insert "\n")
      (backward-char)
      )))

(defun Screen-lines-beginning-of-line (&optional count)
  "Move point to the beginning of the current screen line.
With an argument COUNT not nil or 1, move forward COUNT-1 screen lines and
then to the beginning of the line."
  (interactive "p")
  (screen-lines-keep-region-active)
  (unless count (setq count 1))
  ;; In outline-mode, forward-line behaves more natural than vertical-motion.
  ;; might fail miserably when count > 1 and text property is involved.
  (let* ((pos (point))
         (vpos (progn (vertical-motion (1- count)) (point)))
         (fpos (progn (goto-char pos) (forward-line (1- count)) (point))))
    (when (> vpos fpos)
      (goto-char vpos))))

(defun Screen-lines-end-of-line (&optional count)
  "Move point to the end of the current screen line.
With an argument COUNT not nil or 1, move forward COUNT-1 screen lines and
then to the end of the line."
  (interactive "p")
  (screen-lines-keep-region-active)
  (unless count (setq count 1))
  ;; In outline-mode, forward-line behaves more natural than vertical-motion
  ;; might fail miserably when count > 1 and text property is involved.
  (let* ((pos (point))
         (vpos (progn (when (= count (vertical-motion count))
                        (backward-char))
                      (point)))
         (fpos (progn (goto-char pos)
                      (when (and (zerop (forward-line count)) (not (eobp)))
                        (backward-char))
                      (point))))
    (when (< vpos fpos)
      (goto-char vpos))))

(defun screen-lines-current-column ()
  "Return the horizontal position of point.  Beginning of screen line is 0."
  (let* ((pos (point))
         (cur-col  (current-column))
         (sbol-col (prog2 (Screen-lines-beginning-of-line)
                       (current-column) (goto-char pos))))
    (- cur-col sbol-col)))

(defun screen-lines-move-to-column (column)
  "Move point to COLUMN in the current screen line."
  (let* ((pos (point))
         (beg (progn (Screen-lines-beginning-of-line) (current-column)))
         (end (progn (Screen-lines-end-of-line) (current-column))))
    (goto-char pos)
    (if (< (+ beg column) end)
        (move-to-column (+ beg column))
      (Screen-lines-end-of-line))))

(defun screen-lines-move (arg)
  "Move ARG lines and go to the appropreate column position."
  (unless (memq last-command '(Screen-lines-next-line      next-line
                               Screen-lines-previous-line  previous-line))
    (setq temporary-goal-column
          (if (and track-eol
                   (eolp)
                   ;; Don't count beg of empty line as end of line
                   ;; unless we just did explicit end-of-line.
                   (or (not (bolp))
                       (memq last-command '(Screen-lines-end-of-line
                                            end-of-line))))
              9999
            (screen-lines-current-column))))

  (if (= arg (vertical-motion arg))
      (screen-lines-move-to-column (or goal-column temporary-goal-column))
    (signal (if (< arg 0)
                'beginning-of-buffer
              'end-of-buffer)
            nil)))

(defun Screen-lines-next-line (arg)
  "Move cursor vertically down ARG screen lines."
  (interactive "p")
  (screen-lines-keep-region-active)
  (if (and next-line-add-newlines (= arg 1))
      (let ((pos (point)))
        (Screen-lines-end-of-line)
        (if (eobp)
            (newline 1)
          (goto-char pos)
          (screen-lines-move arg)))
    (if (interactive-p)
        (condition-case nil
            (screen-lines-move arg)
          ((beginning-of-buffer end-of-buffer) (ding)))
      (screen-lines-move arg)))
  nil)

(defun Screen-lines-previous-line (arg)
  "Move cursor vertically up ARG screen lines."
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (screen-lines-move (- arg))
        ((beginning-of-buffer end-of-buffer) (ding)))
    (screen-lines-move (- arg)))
  nil)


;;; Advice
(defvar screen-lines-mode-commands
  '()
  "List of commands which should behave in terms of screen lines.")

(defmacro screen-lines-mode-p ()
  "Return non-nil when Screen Lines minor mode should be effective."
  '(and screen-lines-mode
        (or (interactive-p)
            (memq this-command screen-lines-mode-commands))))

(defadvice next-line
  (around screen-lines last activate compile preactivate)
  "Screen lines minor mode version.  Work in terms of screen lines."
  (if (screen-lines-mode-p)
      (condition-case err
          (Screen-lines-next-line (ad-get-arg 0))
        ((beginning-of-buffer end-of-buffer)
         (if (interactive-p)            ; emulate the original error handling
             (ding)
           (signal (car err) (cdr err)))))
    ad-do-it))

(defadvice previous-line
  (around screen-lines last activate compile preactivate)
  "Screen lines minor mode version.  Work in terms of screen lines."
  (if (screen-lines-mode-p)
      (condition-case err
          (Screen-lines-previous-line (ad-get-arg 0))
        ((beginning-of-buffer end-of-buffer)
         (if (interactive-p)            ; emulate the original error handling
             (ding)
           (signal (car err) (cdr err)))))
    ad-do-it))

(defadvice beginning-of-line
  (around screen-lines last activate compile preactivate)
  "Screen lines minor mode version.  Work in terms of screen lines."
  (interactive "p")
  (if (screen-lines-mode-p)
      (Screen-lines-beginning-of-line (ad-get-arg 0))
    ad-do-it))

(defadvice end-of-line
  (around screen-lines last activate compile preactivate)
  "Screen lines minor mode version.  Work in terms of screen lines."
  (interactive "p")
  (if (screen-lines-mode-p)
      (Screen-lines-end-of-line (ad-get-arg 0))
    ad-do-it))

(defadvice kill-line
  (around screen-lines last activate compile preactivate)
  "Screen lines minor mode version.  Work in terms of screen lines."
  (if (screen-lines-mode-p)
      (Screen-lines-kill-line (ad-get-arg 0))
    ad-do-it))


;;; pc-select support
(eval-after-load "pc-select"
  '(progn
     (setq screen-lines-mode-commands
           (append '(;; GNU Emacs pc-select.el
                     next-line-mark             next-line-nomark
                     previous-line-mark         previous-line-nomark
                     beginning-of-line-mark     beginning-of-line-nomark
                     end-of-line-mark           end-of-line-nomark
                     ;; XEmacs pc-select.el
                     pc-select-move-line-up     pc-select-move-line-down
                     pc-select-move-bol         pc-select-move-eol
                     pc-select-mark-line-up     pc-select-mark-line-down
                     pc-select-mark-to-bol      pc-select-mark-to-eol)
                   screen-lines-mode-commands))

     (cond
      ((featurep 'xemacs)
       ;; swapped the position of
       ;; (setq zmacs-region-stays pc-select-keep-regions)
       ;; (setq this-command ADVISED-FUNC)
       ;; and
       ;; (ADVISED-FUN arg)
       ;; ADVISED-FUN is one of the advised functions.
       ;; Also avoid byte-compiled `end-of-line' call
       (defun pc-select-move-line-up (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (next-line (- arg))
         (setq zmacs-region-stays pc-select-keep-regions)
         (setq this-command 'previous-line))
       (defun pc-select-move-line-down (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (next-line arg)
         (setq zmacs-region-stays pc-select-keep-regions)
         (setq this-command 'next-line))
       (defun pc-select-move-bol (&optional arg)
         (interactive "p")
         "Redefined by screen-lines.el"
         (beginning-of-line arg)
         (setq zmacs-region-stays pc-select-keep-regions)
         (setq this-command 'beginning-of-line))
       (defun pc-select-move-eol (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (end-of-line arg)
         (setq zmacs-region-stays pc-select-keep-regions)
         (setq this-command 'end-of-line))

       (defun pc-select-mark-line-up (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (pc-select-ensure-mark)
         (next-line (- arg))
         (setq this-command 'previous-line))
       (defun pc-select-mark-line-down (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (pc-select-ensure-mark)
         (next-line arg)
         (setq this-command 'next-line))
       (defun pc-select-mark-to-bol (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (pc-select-ensure-mark)
         (beginning-of-line arg)
         (setq this-command 'beginning-of-line))
       (defun pc-select-mark-to-eol (&optional arg)
         "Redefined by screen-lines.el"
         (interactive "p")
         (pc-select-ensure-mark)
         (end-of-line arg)
         (setq this-command 'end-of-line))
       )

      (t
       ;; avoid byte-compiled `end-of-line' call. see Bugs section.
       (defun end-of-line-mark (&optional arg)
         "Redefined by screen-lines.el to avoid byte-compiled definition."
         (interactive "p")
         (ensure-mark)
         (end-of-line arg)
         (setq this-command 'end-of-line))

       (defun end-of-line-nomark (&optional arg)
         "Redefined by screen-lines.el to avoid byte-compiled definition."
         (interactive "p")
         (setq mark-active nil)
         (end-of-line arg)
         (setq this-command 'end-of-line))))))


(defvar screen-lines-load-hook nil
  "Hook to run at the end of loading screen-lines package.")

(provide 'screen-lines)
(run-hooks 'screen-lines-load-hook)
;;; screen-lines.el ends here
