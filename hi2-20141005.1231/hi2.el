;;; hi2.el --- indentation module for Haskell Mode

;; Copyright (C) 2013  Gergely Risko
;; Copyright (C) 2009  Kristof Bastiaensen
;; This file is heavily based on haskell-indentation.el by Kristof.

;; Author: Gergely Risko <gergely@risko.hu>
;; Version: 20141005.1231
;; X-Original-Version: 20141005.0
;; Keywords: indentation haskell
;; URL: https://github.com/errge/hi2

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a modified version of haskell-indentation-mode.  Currently
;; the semantic parser is not changed, but the UI of tab completion is
;; reworked.  Most notably:

;;  - DEL and C-d is not mapped: if you want to indent backwards, you can
;;    use S-TAB.  This means no random jumping on backspace anymore.
;;
;;  - TAB steps to the right as before, but when the end is reached, it
;;    starts going to the left instead of wrapping around.
;;
;;  - TAB stays inside the code, instead of going to the beginning of the
;;    line.  As in pyhton-mode and perl-mode.
;;
;;  - Region indentation common case is supported: TAB and S-TAB is simply
;;    moving the whole region to the left/right by 1-column.  Can be
;;    pressed repeatedly.
;;
;;  - The current indentations are shown as underscores in the current
;;    line.  So you have some visual indication on what's gonna happen.
;;    Also useful while hacking on the parser and want to see the results.
;;    Can be turned off by setting hi2-show-indentations to nil in your
;;    init file or calling hi2-disable-show-indentations from the buffer.
;;    If there are collisions with other overlay hacking modes
;;    (e.g. fill-column-indicator), try to turn off
;;    hi2-show-indentations-after-eol.
;;
;;  - The buffer is not changed when indentation is not changed (so there
;;    are no undo points created and no dirty flag in the buffer if
;;    pressing TAB had no effect).
;;
;;  - The code for all this is somewhat commented and cleaned.

;; There is ongoing development on making this better and I invite
;; everyone to participate (report bugs, fix issues, etc.):
;;   https://github.com/errge/hi2
;;
;; Once this is stable and better in every way than the default
;; haskell-indentation-mode, I'm happy to merge it with haskell-mode,
;; but until this is moving fast, I want to keep it separate.

;;; Installation:

;; To turn indentation on for all Haskell buffers under Haskell mode
;; <http://www.haskell.org/haskell-mode/> add this to .emacs:
;;
;;    (add-hook haskell-mode-hook 'turn-on-hi2)
;;
;; Otherwise, call `hi2-mode'.

;;; TODOs:

;;   - hi2-find-indentations should always go to the beginning of the
;;     line and compute the indentations there; for this we need to
;;     adapt auto-fill and newline-and-indent,
;;   - rename every def* to start with hi2-,
;;   - research emacs parser writing, maybe we can have something more
;;     maintainable than dinamically scoped variables; or write
;;     comments at least.

;;; Code:

(require 'hl-line)
(require 'syntax)
(with-no-warnings (require 'cl))

(defvar hi2-dyn-first-position)
(defvar hi2-dyn-last-direction)
(defvar hi2-dyn-last-indentations)

(defgroup hi2 nil
  "Haskell hi2 indentation."
  :group 'haskell
  :prefix "hi2-")

(defcustom hi2-show-indentations t
  "If t the current line's indentation points will be showed as
underscore overlays in new haskell-mode buffers.  Use
`hi2-enable-show-indentations' and `hi2-disable-show-indentations'
to switch the behavior for already existing buffers."
  :type 'boolean
  :group 'hi2)

(defcustom hi2-show-indentations-after-eol t
  "If t, try to show indentation points after the end of line.
This requires strange overlay hacks and can collide with other
modes (e.g. fill-column-indicator)."
  :type 'boolean
  :group 'hi2)

(defcustom hi2-show-indentations-delay 0.1
  "Number of seconds to wait before showing the indentations.
Since showing the indentations can be expensive on complicated
files, you should set this value higher than your key repeat
speed, so we'll never start computing indentations while you're
moving your cursor around.")

(defface hi2-show-normal-face
  '((t :underline (:style wave)))
  "Default face for indentations overlay."
  :group 'hl2)

(defface hi2-show-hl-line-face
  '((t :underline (:style wave) :inherit hl-line))
  "Face used for indentations overlay after EOL if hl-line mode is enabled."
  :group 'hl2)


(defcustom hi2-layout-offset 2
  "Extra indentation to add before expressions in a haskell layout list."
  :type 'integer
  :group 'hi2)

(defcustom hi2-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'hi2)

(defcustom hi2-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'hi2)

(defcustom  hi2-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'hi2)

(defcustom hi2-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'hi2)

(defcustom hi2-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'hi2)

(defconst hi2-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\r] 'hi2-newline-and-indent)
    (define-key keymap [backtab] 'hi2-indent-backwards)
    keymap))

;;;###autoload
(define-minor-mode hi2-mode
  "Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode."
  :lighter " HI2"
  :keymap hi2-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'indent-region-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when hi2-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function) 'hi2-indent-line)
    (set (make-local-variable 'indent-region-function) 'hi2-indent-region)
    (set (make-local-variable 'normal-auto-fill-function) 'hi2-auto-fill-function)
    (when hi2-show-indentations (hi2-enable-show-indentations))))

;;;###autoload
(defun turn-on-hi2 ()
  "Turn on the hi2 minor mode."
  (interactive)
  (hi2-mode t))

(put 'parse-error
     'error-conditions
     '(error parse-error))
(put 'parse-error 'error-message "Parse error")

(defun parse-error (&rest args)
  (signal 'parse-error (apply 'format args)))

(defmacro on-parse-error (except &rest body)
  `(condition-case parse-error-string
       (progn ,@body)
     (parse-error
      ,except
      (message "%s" (cdr parse-error-string)))))

(defvar haskell-literate)
(defun hi2-birdp (&optional force)
  "Return t if this is a literate haskell buffer in bird style, nil otherwise.
If FORCE is non-nil, throw an error if this is a literate haskell
buffer, but the point is on a non-literate (e.g. comment) line."
  (and (eq haskell-literate 'bird)
       (if (not force)
           t
         (save-excursion
           (progn
             (beginning-of-line)
             (when (/= (char-after) ?>) (error "literate bird command used on non-literate line"))
             t)))))

;;---------------------------------------- UI starts here

(defun hi2-auto-fill-function ()
  (when (> (current-column) fill-column)
    (while (> (current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((indent (car (last (hi2-find-indentations-safe)))))
      (delete-horizontal-space)
      (newline)
      (when (hi2-birdp) (insert ">"))
      (indent-to indent)
      (end-of-line))))

(defun hi2-reindent-to (col &optional move)
  "Reindent current line to COL, also move the point there if MOVE"
  (let* ((cc (current-column))
         (ci (hi2-current-indentation)))
    ;; only untabify if will do something, so we don't pollute undo
    ;; have to untabify outside of save-excursion, to get good point moving
    (when (/= ci col)
      (untabify (line-beginning-position) (line-end-position)))
    (save-excursion
      (move-to-column ci)
      (if (<= ci col)
          (insert-before-markers (make-string (- col ci) ? ))
        (delete-char (- col ci))))
    (when move
      (move-to-column col))))

(defun hi2-indent-rigidly (start end arg)
  "Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large negative ARG.
Handles bird style literate haskell too."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (let ((end-marker (point-marker)))
      (goto-char start)
      (or (bolp) (forward-line 0))
      (untabify (point) end-marker)
      (while (< (point) end-marker)
        (let ((ci (hi2-current-indentation)))
          (when (and t
                     (= (char-after) ?>))
            (forward-char 1))
          (skip-syntax-forward "-")
          (unless (eolp)
            (hi2-reindent-to (max 0 (+ ci arg))))
          (forward-line 1)))
      (move-marker end-marker nil))))

(defun hi2-current-indentation ()
  "Column position of first non whitespace character in current line"
  (save-excursion
    (beginning-of-line)
    (when (hi2-birdp) (forward-char))
    (skip-syntax-forward "-")
    (current-column)))

(defun hi2-bird-outside-codep ()
  "True iff we are in bird literate mode, but outside of code"
  (and (hi2-birdp)
       (or (< (current-column) 2)
           (save-excursion
             (beginning-of-line)
             (/= (char-after) ?>)))))

(defun hi2-delete-horizontal-space-and-newline ()
  (delete-horizontal-space)
  (newline))

(defun hi2-newline-and-indent ()
  "Ran on C-j or RET"
  (interactive)
  ;; On RET (or C-j), we:
  ;;   - just jump to the next line if literate haskell, but outside code
  (if (hi2-bird-outside-codep)
      (hi2-delete-horizontal-space-and-newline)
    ;; - just jump to the next line if parse-error
    (on-parse-error
     (hi2-delete-horizontal-space-and-newline)
     (let* ((cc (current-column))
            (ci (hi2-current-indentation))
            (indentations (hi2-find-indentations-safe)))
       ;; - jump to the next line and reindent to at the least same level
       ;;   if parsing was OK
       (skip-syntax-forward "-")
       (hi2-delete-horizontal-space-and-newline)
       (when (hi2-birdp) (insert "> "))
       (hi2-reindent-to
        (hi2-next-indentation (- ci 1) indentations 'nofail)
        'move)))))

(defun hi2-next-indentation (col indentations &optional nofail)
  "Find the leftmost indentation which is greater than COL.
Or returns the last indentation if there are no bigger ones and
NOFAIL is non-nil."
  (when (null indentations) (error "hi2-next-indentation called with empty list"))
  (or (find-if #'(lambda (i) (> i col)) indentations)
      (when nofail (car (last indentations)))))

(defun hi2-previous-indentation (col indentations &optional nofail)
  "Find the rightmost indentation which is less than COL."
  (when (null indentations) (error "hi2-previous-indentation called with empty list"))
  (let ((rev (reverse indentations)))
    (or (find-if #'(lambda (i) (< i col)) rev)
        (when nofail (car rev)))))

(defun hi2-indent-line ()
  (interactive)
  "Auto indentation on TAB.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
  ;; try to repeat
  (when (not (hi2-indent-line-repeat))
    (setq hi2-dyn-last-direction nil)
    ;; do nothing if we're inside a string or comment
    (unless (save-excursion
              (beginning-of-line)
              (nth 8 (syntax-ppss)))
      ;; parse error is intentionally not catched here, it may come from
      ;; hi2-find-indentations-safe, but escapes the scope and aborts the
      ;; opertaion before any moving happens
      (let* ((cc (current-column))
             (ci (hi2-current-indentation))
             (inds (save-excursion
                     (move-to-column ci)
                     (hi2-find-indentations-safe)))
             (valid (memq ci inds))
             (cursor-in-whitespace (< cc ci)))
        ;; can't happen right now, because of -safe, but we may want to have this in the future
        ;; (when (null inds)
        ;;   (error "returned indentations empty, but no parse error"))
        (if (and valid cursor-in-whitespace)
            (move-to-column ci)
          (hi2-reindent-to (hi2-next-indentation ci inds 'nofail) cursor-in-whitespace))
        (setq hi2-dyn-last-direction 'right)
        (setq hi2-dyn-first-position (hi2-current-indentation))
        (setq hi2-dyn-last-indentations inds)))))

(defun hi2-indent-line-repeat ()
  "Ran if the user repeatedly presses the TAB key"
  (cond
   ((and (memq last-command '(indent-for-tab-command hi2-indent-backwards))
         (eq hi2-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (hi2-indent-rigidly (region-beginning) (region-end) 1))
    t)
   ((and (eq last-command 'indent-for-tab-command)
         (memq hi2-dyn-last-direction '(left right))
         hi2-dyn-last-indentations)
    (let* ((cc (current-column))
           (ci (hi2-current-indentation)))
      (if (eq hi2-dyn-last-direction 'left)
          (hi2-reindent-to (hi2-previous-indentation ci hi2-dyn-last-indentations 'nofail))
        ;; right
        (if (hi2-next-indentation ci hi2-dyn-last-indentations)
            (hi2-reindent-to (hi2-next-indentation ci hi2-dyn-last-indentations 'nofail))
          ;; but failed, switch to left
          (setq hi2-dyn-last-direction 'left)
          ;; and skip to the point where the user started pressing TABs.
          ;; except if there are <= 2 indentation points, because this
          ;; behavior is very confusing in that case
          (when (< 2 (length hi2-dyn-last-indentations))
            (hi2-reindent-to hi2-dyn-first-position))
          (hi2-indent-line-repeat))))
    t)
   (t nil)))

(defun hi2-indent-region (start end)
  (setq hi2-dyn-last-direction 'region)
  (hi2-indent-rigidly start end 1)
  (message "Press TAB or S-TAB again to indent the region more"))

(defun hi2-indent-backwards ()
  "Indent the current line to the previous indentation point"
  (interactive)
  (cond
   ((and (memq last-command '(indent-for-tab-command hi2-indent-backwards))
         (eq hi2-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (hi2-indent-rigidly (region-beginning) (region-end) -1)))
   ((use-region-p)
    (setq hi2-dyn-last-direction 'region)
    (hi2-indent-rigidly (region-beginning) (region-end) -1)
    (message "Press TAB or S-TAB again to indent the region more"))
   (t
    (setq hi2-dyn-last-direction nil)
    (let* ((cc (current-column))
           (ci (hi2-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (hi2-find-indentations-safe)))
           (cursor-in-whitespace (< cc ci))
           (pi (hi2-previous-indentation ci inds)))
      (if (null pi)
          ;; if there are no more indentations to the left, just go to column 0
          (hi2-reindent-to (car (hi2-first-indentation)) cursor-in-whitespace)
        (hi2-reindent-to pi cursor-in-whitespace))))))

;;---------------------------------------- hi2 show indentations UI starts here
(defvar hi2-dyn-show-indentations
  "Whether showing of indentation points is enabled in this buffer.")
(make-variable-buffer-local 'hi2-dyn-show-indentations)

(defvar hi2-dyn-overlays nil
  "Overlays used by hi2-enable-show-indentations.")
(make-variable-buffer-local 'hi2-dyn-overlays)

(defun hi2-init-overlays (n)
  "Makes sure that hi2-dyn-overlays contains at least N overlays."
  (let* ((clen (length hi2-dyn-overlays))
         (needed (- n clen)))
    (dotimes (n needed hi2-dyn-overlays)
      (setq hi2-dyn-overlays
            (cons (make-overlay 1 1) hi2-dyn-overlays)))))

(defvar hi2-dyn-show-overlays-timer (timer-create)
  "Timer used to schedule hi2-show-overlays.  Scheduled when we
go to a new line and canceled in pre-command-hook if the user
very quickly starts a new command.")
(make-variable-buffer-local 'hi2-dyn-show-overlays-timer)
(defun hi2-show-overlays-cancel-timer ()
  "Cancels hi2-dyn-show-overlays-timer."
  (when hi2-dyn-show-overlays-timer
    (cancel-timer hi2-dyn-show-overlays-timer)))
(defun hi2-show-overlays-schedule-timer ()
  "Schedule a hi2-show-overlays run."
  (setq hi2-dyn-show-overlays-timer
        (run-with-timer hi2-show-indentations-delay nil #'hi2-show-overlays)))

(defvar hi2-dyn-show-overlays-cache '(-1 . nil)
  "(line-beginning-position . indentations) at the previous
hi2-find-indentations-safe call inside hi2-show-overlays")
(make-variable-buffer-local 'hi2-dyn-show-overlays-cache)

(defun hi2-unshow-overlays ()
  "Unshows all the overlays."
  (mapc #'delete-overlay hi2-dyn-overlays))

(defun hi2-show-overlays-change-major-mode ()
  (hi2-unshow-overlays)
  (hi2-show-overlays-cancel-timer))

(defun hi2-show-overlays-pre-command ()
  (hi2-unshow-overlays)
  (hi2-show-overlays-cancel-timer))

(defun hi2-show-overlays-post-command ()
  "Schedules a hi2-show-overlay run and deletes the overlays if
the line is different than previously, so the cache is not
usable.  If the cache is usable, just uses the cache to show the
overlays."
  (if (= (car hi2-dyn-show-overlays-cache)
         (line-beginning-position))
      ; cache is usable
      (progn
        (hi2-show-overlays (cdr hi2-dyn-show-overlays-cache)))
    ; cache is not usable
    (hi2-show-overlays-schedule-timer)))

(defun hi2-show-overlays (&optional cached-indentations)
  "Put an underscore overlay at all the indentations points in
the current buffer."
  (when (and (memq major-mode '(haskell-mode literate-haskell-mode))
             (memq 'hi2-mode minor-mode-list)
             hi2-dyn-show-indentations)
    (save-excursion
      (let* ((columns (progn
                        (end-of-line)
                        (current-column)))
             (ci (hi2-current-indentation))
             (allinds (or cached-indentations
                          (save-excursion
                            (move-to-column ci); XXX: remove when hi2-find-indentations is fixed
                            ;; don't freak out on parse-error
                            (condition-case e
                                (hi2-find-indentations-safe)
                              (parse-error nil)))))
             ;; indentations that are easy to show
             (inds (remove-if (lambda (i) (>= i columns)) allinds))
             ;; tricky indentations, that are after the current EOL
             (overinds (member-if (lambda (i) (>= i columns)) allinds))
             ;; +1: leave space for an extra overlay to show overinds
             (overlays (hi2-init-overlays (+ 1 (length inds)))))
        (while inds
          (move-to-column (car inds))
          (and
           ;; if we jump in the middle of a tab, we will be misaligned
           (= (current-column) (car inds))
           ;; if we jump to the beginning, we will look at the tab
           (/= (char-after) '?\t)
           ;; and we only want to display indentations for spaces,
           ;; because otherwise they'll look messy
           (progn
             (overlay-put (car overlays) 'face 'hi2-show-normal-face)
             (overlay-put (car overlays) 'after-string nil)
             (move-overlay (car overlays) (point) (+ 1 (point)))
             (setq overlays (cdr overlays))))
          (setq inds (cdr inds)))
        (when (and overinds
                   hi2-show-indentations-after-eol)
          (let ((o (car overlays))
                (s (make-string (+ 1 (- (car (last overinds)) columns)) ? )))
            ;; needed for the cursor to be in the good position, see:
            ;;   http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-03/msg00079.html
            (put-text-property 0 1 'cursor t s)
            ;; color the whole line ending overlay with hl-line face if needed
            (when (or hl-line-mode global-hl-line-mode)
              (put-text-property 0 (length s) 'face 'hl-line s))
            ;; put in the underlines at the correct positions
            (dolist (i overinds)
              (put-text-property
               (- i columns) (+ 1 (- i columns))
               'face (if (or hl-line-mode global-hl-line-mode)
                         'hi2-show-hl-line-face
                       'hi2-show-normal-face)
               s))
            (overlay-put o 'face nil)
            (overlay-put o 'after-string s)
            (end-of-line)
            (move-overlay o (point) (point))))
        (setq hi2-dyn-show-overlays-cache
              (cons (line-beginning-position) allinds))))))

(defun hi2-enable-show-indentations ()
  "Enable showing of indentation points in the current buffer."
  (interactive)
  (setq hi2-dyn-show-indentations t)
  (add-hook 'change-major-mode-hook #'hi2-show-overlays-change-major-mode nil t)
  (add-hook 'pre-command-hook #'hi2-show-overlays-pre-command nil t)
  (add-hook 'post-command-hook #'hi2-show-overlays-post-command nil t))

(defun hi2-disable-show-indentations ()
  "Disable showing of indentation points in the current buffer."
  (interactive)
  (setq hi2-dyn-show-indentations nil)
  (remove-hook 'post-command-hook #'hi2-show-overlays-post-command t)
  (hi2-unshow-overlays)
  (remove-hook 'change-major-mode-hook #'hi2-show-overlays-change-major-mode t)
  (remove-hook 'pre-command-hook #'hi2-show-overlays-pre-command t))

;;---------------------------------------- parser starts here

;; PARSER TODOS:
;; - why is there an indentation point at 2 after an import?
;;   because you can write:
;;
;;   import A
;;     (a, b, c)
;;
;;   But at least we should fix this:
;;
;;   import A (a)
;;     (b)    <- why there is an indentation point at 2 here?
;;
;;   and maybe have a setq for the first case

;; - why there are no indentation points at all before data in this file:
;;     import A
;;     data Foo

;; - there should be an indentation point at 2 in the second line:
;;
;;   data Person = Person
;;                 { firstName :: !String  -- ^ First name
;;                 , lastName  :: !String  -- ^ Last name
;;                 , age       :: !Int     -- ^ Age
;;                 } deriving (Eq, Show)
;;
;;   see: https://github.com/errge/hi2/issues/4

;; - module export list indentation is inconsistent, see:
;;   https://github.com/errge/hi2/issues/3

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)

(defun hi2-goto-least-indentation ()
  (beginning-of-line)
  (if (hi2-birdp)
      (catch 'return
        (while t
          (when (/= (char-after) ?>)
            (forward-line)
            (forward-char 2)
            (throw 'return nil))
          (let ((ps (nth 8 (syntax-ppss))))
            (when ps ;; inside comment or string
              (goto-char ps)
              (beginning-of-line)))
          (when (and (>= 2 (hi2-current-indentation))
                     (not (looking-at ">\\s-*$")))
            (forward-char 2)
            (throw 'return nil))
          (when (bobp)
            (forward-char 2)
            (throw 'return nil))
          (forward-line -1)))
    ;; not bird style
    (catch 'return
      (while (not (bobp))
        (forward-comment (- (buffer-size)))
        (beginning-of-line)
        (let ((ps (nth 8 (syntax-ppss))))
          (when ps ;; inside comment or string
            (goto-char ps)))
        (when (= 0 (hi2-current-indentation))
          (throw 'return nil))))
    (beginning-of-line)
    (when (bobp)
      (forward-comment (buffer-size)))))

(defun hi2-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (parse-line-number 0)
          (current-indent hi2-layout-offset)
          (starter-indent hi2-layout-offset)
          (left-indent hi2-layout-offset)
          (case-fold-search nil)
          current-token
          following-token
          possible-indentations)
      (hi2-goto-least-indentation)
      (if (<= indentation-point (point))
          (hi2-first-indentation)
        (setq current-token (hi2-peek-token))
        (catch 'parse-end
          (hi2-toplevel)
          (unless (eq current-token 'end-tokens)
            (parse-error "Illegal token: %s" current-token)))
        possible-indentations))))

(defun hi2-first-indentation ()
  (if (hi2-birdp) '(2) '(0)))

(defun hi2-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (hi2-first-indentation))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (hi2-parse-to-indentations)
        (hi2-first-indentation)))
     (t
      (hi2-parse-to-indentations)))))

;; XXX: this is a hack, the parser shouldn't return nil without parse-error
(defun hi2-find-indentations-safe ()
  (let ((ret (hi2-find-indentations)))
    (if ret
        ret
      (hi2-first-indentation))))

(defconst hi2-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("↢" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("↣" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation dictionary from UnicodeSyntax tokens to their ASCII representation.")

(defconst hi2-toplevel-list
  '(("module" . hi2-module)
    ("data" . (lambda () (hi2-statement-right #'hi2-data)))
    ("type" . (lambda () (hi2-statement-right #'hi2-data)))
    ("newtype" . (lambda () (hi2-statement-right #'hi2-data)))
    ("class" . hi2-class-declaration)
    ("instance" . hi2-class-declaration )))

(defconst hi2-type-list
  '(("::"    . (lambda () (hi2-with-starter
                           (lambda () (hi2-separated #'hi2-type "->" nil)) nil)))
    ("("     . (lambda () (hi2-list #'hi2-type
                                                    ")" "," nil)))
    ("["     . (lambda () (hi2-list #'hi2-type
                                                    "]" "," nil)))
    ("{"     . (lambda () (hi2-list #'hi2-type
                                                    "}" "," nil)))))

(defconst hi2-expression-list
  '(("data" . hi2-data)
    ("type" . hi2-data)
    ("newtype" . hi2-data)
    ("if"    . (lambda () (hi2-with-starter #'hi2-if-maybe-multiwayif nil)))
    ("let"   . (lambda () (hi2-phrase
                           '(hi2-declaration-layout
                             "in" hi2-expression))))
    ("do"    . (lambda () (hi2-with-starter
                           #'hi2-expression-layout nil)))
    ("mdo"   . (lambda () (hi2-with-starter
                           #'hi2-expression-layout nil)))
    ("rec"   . (lambda () (hi2-with-starter
                           #'hi2-expression-layout nil)))
    ("case"  . (lambda () (hi2-phrase
                           '(hi2-expression
                             "of" hi2-case-layout))))
    ("\\"    . (lambda () (hi2-with-starter
                           #'hi2-lambda-maybe-lambdacase nil)))
    ("proc"  . (lambda () (hi2-phrase
                           '(hi2-expression
                             "->" hi2-expression))))
    ("where" . (lambda () (hi2-with-starter
                           #'hi2-declaration-layout nil t)))
    ("::"    . (lambda () (hi2-with-starter
                           (lambda () (hi2-separated #'hi2-type "->" nil)) nil)))
    ("="     . (lambda () (hi2-statement-right #'hi2-expression)))
    ("<-"    . (lambda () (hi2-statement-right #'hi2-expression)))
    ("("     . (lambda () (hi2-list #'hi2-expression
                                                    ")" '(list "," "->") nil)))
    ("["     . (lambda () (hi2-list #'hi2-expression
                                                    "]" "," "|")))
    ("{"     . (lambda () (hi2-list #'hi2-expression
                                                    "}" "," nil)))))

(defun hi2-expression-layout ()
  (hi2-layout #'hi2-expression))

(defun hi2-declaration-layout ()
  (hi2-layout #'hi2-declaration))

(defun hi2-case-layout ()
  (hi2-layout #'hi2-case))

;; After a lambda (backslash) there are two possible cases:
;;   - the new lambdacase expression, that can be recognized by the
;;     next token being "case",
;;   - or simply an anonymous function definition in the form of
;;     "expression -> expression".
(defun hi2-lambda-maybe-lambdacase ()
  (if (string= current-token "case")
      (hi2-with-starter
       #'hi2-case-layout nil)
    (hi2-phrase-rest
     '(hi2-expression "->" hi2-expression))))

(defun hi2-if-maybe-multiwayif ()
  (cond ((string= current-token "|")
         ;; Multi-way if
         (hi2-implicit-multiwayif-layout #'hi2-multiwayif-phrase))
        ((string= current-token "{")
         ;; Multi-way if with explicit layout
         (hi2-layout #'hi2-multiwayif-phrase))
        (t
         ;; Regular if
         (hi2-phrase-rest '(hi2-expression "then" hi2-expression "else" hi2-expression)))
        ))

(defun hi2-multiwayif-phrase ()
  (cond ((string= current-token "|")
         (hi2-phrase '(hi2-expression "->" hi2-expression)))
        ))

;; TODO(klao): this is an almost verbatim copy of
;; hi2-implicit-layout-list, I was just too afraid to touch that one.
(defun hi2-implicit-multiwayif-layout (parser)
  (let* ((layout-indent (current-column))
         (current-indent (current-column))
         (left-indent (current-column)))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-next ";"))
               (hi2-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (hi2-expression-token following-token)
                         (member following-token '(";" "|")))
                 (hi2-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put hi2-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (hi2-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun hi2-fundep ()
  (hi2-with-starter
   (lambda () (hi2-separated
               #'hi2-fundep1 "," nil))
   nil))

(defun hi2-fundep1 ()
  (let ((current-indent (current-column)))
    (while (member current-token '(value "->"))
      (hi2-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (hi2-add-indentation current-indent))))

(defun hi2-toplevel ()
  (hi2-layout
   (lambda ()
     (let ((parser (assoc current-token hi2-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (hi2-declaration))))))

(defun hi2-type ()
  (let ((current-indent (current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->"))
          (hi2-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "->" "(" "[" "{" "::"))
            (hi2-add-indentation current-indent))
          (throw 'return nil))

         (t (let ((parser (assoc current-token hi2-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

(defun hi2-data ()
  (hi2-with-starter
   (lambda ()
     (when (string= current-token "instance")
       (hi2-read-next-token))
     (hi2-type)
     (cond ((string= current-token "=")
            (hi2-with-starter
             (lambda () (hi2-separated #'hi2-type "|" "deriving"))
             nil))
           ((string= current-token "where")
            (hi2-with-starter
             #'hi2-expression-layout nil))))
   nil))

(defun hi2-class-declaration ()
  (hi2-with-starter
   (lambda ()
     (hi2-type)
     (when (string= current-token "|")
       (hi2-fundep))
     (when (string= current-token "where")
       (hi2-with-starter
        #'hi2-expression-layout nil)))
   nil))

(defun hi2-module ()
  (hi2-with-starter
   (lambda ()
     (let ((current-indent (current-column)))
       (hi2-read-next-token)
       (when (string= current-token "(")
         (hi2-list
          #'hi2-module-export
          ")" "," nil))
       (when (eq current-token 'end-tokens)
         (hi2-add-indentation current-indent)
         (throw 'parse-end nil))
       (when (string= current-token "where")
         (hi2-read-next-token)
         (when (eq current-token 'end-tokens)
           (hi2-add-layout-indent)
           (throw 'parse-end nil))
         (hi2-layout #'hi2-toplevel))))
   nil))

(defun hi2-module-export ()
  (cond ((string= current-token "module")
         (let ((current-indent (current-column)))
           (hi2-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (hi2-add-indentation current-indent))
                 ((eq current-token 'value)
                  (hi2-read-next-token)))))
        (t (hi2-type))))

(defun hi2-list (parser end sep stmt-sep)
  (hi2-with-starter
   `(lambda () (hi2-separated #',parser
                                              ,sep
                                              ,stmt-sep))
   end))

(defun hi2-with-starter (parser end &optional where-expr?)
  (let ((starter-column (current-column))
        (current-indent current-indent)
        (left-indent (if (= (current-column) (hi2-current-indentation))
                         (current-column) left-indent)))
    (hi2-read-next-token)
    (when (eq current-token 'end-tokens)
      (if (equal following-token end)
          (hi2-add-indentation starter-column)
        (if where-expr?
            (hi2-add-where-post-indent left-indent)
          (hi2-add-indentation
           (+ left-indent hi2-left-offset))))
      (throw 'parse-end nil))
    (let* ((current-indent (current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent (if end (+ current-indent hi2-starter-offset)
                          left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               (hi2-add-indentation starter-indent))
             (when end (throw 'parse-end nil))) ;; add no indentations
            ((equal current-token end)
             (hi2-read-next-token)) ;; continue
            (end (parse-error "Illegal token: %s" current-token))))))

(defun hi2-case ()
  (hi2-expression)
  (cond ((eq current-token 'end-tokens)
         (hi2-add-indentation current-indent))
        ((string= current-token "|")
         (hi2-with-starter
          (lambda () (hi2-separated #'hi2-case "|" nil))
          nil))
        ((string= current-token "->")
         (hi2-statement-right #'hi2-expression))
        ;; otherwise fallthrough
        ))

(defun hi2-statement-right (parser)
    (hi2-read-next-token)
    (when (eq current-token 'end-tokens)
      (hi2-add-indentation
       (+ left-indent hi2-left-offset))
      (throw 'parse-end nil))
    (let ((current-indent (current-column)))
          (funcall parser)))

(defun hi2-simple-declaration ()
  (hi2-expression)
  (cond ((string= current-token "=")
         (hi2-statement-right #'hi2-expression))
        ((string= current-token "::")
         (hi2-statement-right #'hi2-type))
        ((and (eq current-token 'end-tokens)
              (string= following-token "="))
         (hi2-add-indentation current-indent)
         (throw 'parse-end nil))))

(defun hi2-declaration ()
  (hi2-expression)
  (cond ((string= current-token "|")
         (hi2-with-starter
          (lambda () (hi2-separated #'hi2-expression "," "|"))
          nil))
        ((eq current-token 'end-tokens)
         (when (member following-token '("|" "=" "::" ","))
           (hi2-add-indentation current-indent)
           (throw 'parse-end nil)))))

(defun hi2-layout (parser)
  (if (string= current-token "{")
      (hi2-list parser "}" ";" nil)
    (hi2-implicit-layout-list parser)))

(defun hi2-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" "::"
                  value operator no-following-token)))

(defun hi2-expression ()
  (let ((current-indent (current-column)))
    (catch 'return
      (while t
        (cond
         ((memq current-token '(value operator))
          (hi2-read-next-token))

         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (hi2-add-where-pre-indent))
                ((hi2-expression-token following-token)
                 (hi2-add-indentation
                  current-indent)))
          (throw 'return nil))

         (t (let ((parser (assoc current-token hi2-expression-list)))
              (when (null parser)
                (throw 'return nil))
              (funcall (cdr parser))
              (when (and (eq current-token 'end-tokens)
                         (string= (car parser) "let")
                         (= hi2-layout-offset current-indent)
                         (hi2-expression-token following-token))
                ;; inside a layout, after a let construct
                (hi2-add-layout-indent)
                (throw 'parse-end nil))
              (unless (member (car parser) '("(" "[" "{" "do" "case"))
                (throw 'return nil)))))))))

(defun hi2-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (hi2-find-indentations-safe)))
        (str "")
        (pos 0))
    (while indentations
      (when (>= (car indentations) pos)
        (setq str (concat str (make-string (- (car indentations) pos) ?\ )
                          "|"))
        (setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun hi2-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
             (hi2-at-separator))

            ((equal current-token stmt-separator)
             (setq starter-indent (current-column))
             (hi2-at-separator))

            ((eq current-token 'end-tokens)
             (cond ((or (equal following-token separator)
                        (equal following-token stmt-separator))
                    (hi2-add-indentation starter-indent)
                    (throw 'parse-end nil)))
             (throw 'return nil))

            (t (throw 'return nil))))))

(defun hi2-at-separator ()
  (let ((separator-column
         (and (= (current-column) (hi2-current-indentation))
              (current-column))))
    (hi2-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (hi2-add-indentation current-indent)
           (throw 'return nil))
          (separator-column ;; on the beginning of the line
           (setq current-indent (current-column))
           (setq starter-indent separator-column)))))

(defun hi2-implicit-layout-list (parser)
  (let* ((layout-indent (current-column))
         (current-indent (current-column))
         (left-indent (current-column)))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-next ";"))
               (hi2-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (hi2-expression-token following-token)
                                         (string= following-token ";"))
                         (hi2-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put hi2-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (hi2-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun hi2-phrase (phrase)
  (hi2-with-starter
   `(lambda () (hi2-phrase-rest ',phrase))
   nil))

(defun hi2-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (current-column)))
      (funcall (car phrase)))
    (cond
     ((eq current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
            ((equal following-token (cadr phrase))
             (hi2-add-indentation starter-indent)
             (throw 'parse-end nil))
            ((string= (cadr phrase) "in")
             (when (= left-indent layout-indent)
               (hi2-add-layout-indent)
               (throw 'parse-end nil)))
            (t (throw 'parse-end nil))))

     ((null (cdr phrase)))

     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (current-column) (hi2-current-indentation)))
             (lines-between (- parse-line-number starter-line))
             (left-indent (if (<= lines-between 0)
                              left-indent
                            starter-indent)))
        (hi2-read-next-token)
        (when (eq current-token 'end-tokens)
          (hi2-add-indentation
           (cond ((member (cadr phrase) '("then" "else"))
                  (+ starter-indent hi2-ifte-offset))
                 ((member (cadr phrase) '("in" "->"))
                  ;; expression ending in another expression
                  (if on-new-line
                      (+ left-indent hi2-starter-offset)
                    left-indent))
                 (t (+ left-indent hi2-left-offset))))
          (throw 'parse-end nil))
        (hi2-phrase-rest (cddr phrase))))

     ((string= (cadr phrase) "in")) ;; fallthrough
     (t (parse-error "Expecting %s" (cadr phrase))))))

(defun hi2-add-indentation (indent)
  (hi2-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent hi2-layout-offset)
     indent)))

(defun hi2-add-layout-indent ()
  (hi2-push-indentation layout-indent))

(defun hi2-add-where-pre-indent ()
  (hi2-push-indentation
   (+ layout-indent hi2-where-pre-offset))
  (if (= layout-indent hi2-layout-offset)
      (hi2-push-indentation
       hi2-where-pre-offset)))

(defun hi2-add-where-post-indent (indent)
  (hi2-push-indentation
   (+ indent hi2-where-post-offset)))

(defun hi2-push-indentation (indent)
  (when (or (null possible-indentations)
            (< indent (car possible-indentations)))
    (setq possible-indentations
          (cons indent possible-indentations))))

(defun hi2-token-test ()
  (let ((current-token nil)
        (following-token nil)
        (layout-indent 0)
        (parse-line-number 0)
        (indentation-point (mark)))
    (hi2-read-next-token)))

(defun hi2-read-next-token ()
  (cond ((eq current-token 'end-tokens)
         'end-tokens)
        ((eq current-token 'layout-end)
         (cond ((> layout-indent (current-column))
                'layout-end)
               ((= layout-indent (current-column))
                (setq current-token 'layout-next))
               ((< layout-indent (current-column))
                (setq current-token (hi2-peek-token)))))
        ((eq current-token 'layout-next)
         (setq current-token (hi2-peek-token)))
        ((> layout-indent (current-column))
         (setq current-token 'layout-end))
        (t
         (hi2-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (= (point) indentation-point)
                         (hi2-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (current-column) (hi2-current-indentation))
             ;; on a new line
             (setq current-indent (current-column))
             (setq left-indent (current-column))
             (setq parse-line-number (+ parse-line-number 1)))
           (cond ((> layout-indent (current-column))
                  (setq current-token 'layout-end))
                 ((= layout-indent (current-column))
                  (setq current-token 'layout-next))
                 (t (setq current-token (hi2-peek-token))))))))

(defun hi2-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok hi2-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun hi2-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))

    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at         ; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
      ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))
    (while (and (hi2-birdp)
                (bolp)
                (eq (char-after) ?>))
      (forward-char)
      (forward-comment (buffer-size)))))

(provide 'hi2)

;;; hi2.el ends here
