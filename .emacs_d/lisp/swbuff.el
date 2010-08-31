;;; swbuff.el --- Quick switch between Emacs buffers.

;; Copyright (C) 1998, 2000, 2001, 2003 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 12 Nov 1998
;; Keywords: extensions convenience
;; Revision: $Id: swbuff.el,v 1.19 2003/04/25 11:31:22 ponced Exp $

(defconst swbuff-version "3.2")

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package provides the commands `swbuff-switch-to-next-buffer'
;; and `swbuff-switch-to-previous-buffer' to respectively switch to
;; the next or previous buffer in the buffer list.

;; The `swbuff-exclude-buffer-regexps' defines a list of regular
;; expressions for excluded buffers. The default setting excludes
;; buffers whose name begin with a blank character. To exclude all the
;; internal buffers (that is *scratch*, *Message*, etc...) you could
;; use the following regexps '("^ .*" "^\\*.*\\*").

;; Switching buffers pops-up a status window at the bottom of the
;; selected window. The status window shows the list of switchable
;; buffers where the switched one is hilighted using
;; `swbuff-current-buffer-face'. This window is automatically
;; discarded after any command is executed or after the delay
;; specified by `swbuff-clear-delay'.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (require 'swbuff)
;;

;;; History:
;;

;;; Code:
(require 'timer)

;;; Options
;;
(defgroup swbuff nil
  "Quick switch between Emacs buffers."
  :group 'extensions
  :group 'convenience
  :prefix "swbuff-")

(defcustom swbuff-status-window-layout nil
  "*Method used to ensure the switched buffer is always visible.
This occurs when the buffer list is larger than the status window
width. The possible choices are:

- - 'Default' If there is only one window in the frame (ignoring the
              minibuffer one and the status window itself) the status
              window height is adjusted.
              Otherwise horizontal scrolling is used.
- - 'Scroll'  Horizontal scrolling is always used.
- - 'Adjust'  Only adjust the window height."
  :group 'swbuff
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Scroll"  scroll)
                 (const :tag "Adjust"  adjust)))

(defcustom swbuff-clear-delay 3
  "*Time in seconds to delay before discarding the status window."
  :group 'swbuff
  :type '(number :tag "seconds")) 

(defcustom swbuff-separator ", "
  "*String used to separate buffer names in the status line."
  :group 'swbuff
  :type 'string)

(defcustom swbuff-header ""
  "*Status line header string."
  :group 'swbuff
  :type 'string)

(defcustom swbuff-trailer ""
  "*Status line trailer string."
  :group 'swbuff
  :type 'string)

(defcustom swbuff-window-min-text-height 1
  "*Minimum text height of the status window."
  :group 'swbuff
  :type 'integer)

(defface swbuff-default-face '((t nil))
  "*Default face used for buffer names."
  :group 'swbuff)

(defface swbuff-current-buffer-face '((t (:foreground "red" :bold t :underline t)))
  "*Face used to highlight the current buffer name."
  :group 'swbuff)

(defface swbuff-separator-face '((t (:foreground "blue")))
  "*Face used for separators."
  :group 'swbuff)

(defcustom swbuff-exclude-buffer-regexps '("^ ")
  "*List of regular expressions for excluded buffers.
The default setting excludes buffers whose name begin with a blank
character.  To exclude all the internal buffers (that is *scratch*,
*Message*, etc...) you could use the following regexps:
  (\"^ \" \"^\\*.*\\*\")."
  :group 'swbuff
  :type '(repeat (regexp :format "%v")))

(defcustom swbuff-load-hook '(swbuff-default-load-hook)
  "Hook run when package has been loaded.
See also `swbuff-default-load-hook'."
  :group 'swbuff
  :type 'hook)

;;; Internals
;;
(defconst swbuff-status-buffer-name "*swbuff*"
  "Name of the working buffer used to display the buffer list.")

(defun swbuff-include-p (name)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `swbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote swbuff-status-buffer-name)
                  swbuff-exclude-buffer-regexps)))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun swbuff-buffer-list ()
  "Return the list of switchable buffers.
That is without the ones whose name matches
`swbuff-exclude-buffer-regexps'."
  (let ((blist (delq nil
                     (mapcar (function
                              (lambda (buf)
                                (and (swbuff-include-p (buffer-name buf))
                                     buf)))
                             (buffer-list)))))
    (or (memq (current-buffer) blist)
        (setq blist (cons (current-buffer) blist)))
    blist))

(if (fboundp 'count-lines)
    (defalias 'swbuff-count-lines 'count-lines)
  (defun swbuff-count-lines (start end)
    "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (if (eq selective-display t)
            (save-match-data
              (let ((done 0))
                (while (re-search-forward "[\n\C-m]" nil t 40)
                  (setq done (+ 40 done)))
                (while (re-search-forward "[\n\C-m]" nil t 1)
                  (setq done (+ 1 done)))
                (goto-char (point-max))
                (if (and (/= start end)
                         (not (bolp)))
                    (1+ done)
                  done)))
          (- (buffer-size) (forward-line (buffer-size))))))))

(defun swbuff-window-lines ()
  "Return the number of lines in current buffer.
This number may be greater than the number of actual lines in the
buffer if any wrap on the display due to their length."
  (swbuff-count-lines (point-min) (point-max)))

(defun swbuff-adjust-window (&optional text-height)
  "Adjust window height to fit its buffer contents.
If optional TEXT-HEIGHT is non-nil adjust window height to this
value."
  (setq text-height (max swbuff-window-min-text-height
                         (or text-height
                             (swbuff-window-lines))))
  (if (fboundp 'set-window-text-height)
      (set-window-text-height nil text-height)
    (let ((height (window-height))
          (lines  (+ 2 text-height)))
      (enlarge-window (- lines height))))
  (goto-char (point-min)))

;; Used to prevent discarding the status window on some mouse event.
(defalias 'swbuff-ignore 'ignore)

;;; Compatibility
(cond
 ;; GNU Emacs 21
 ((and (not (featurep 'xemacs))
       (> emacs-major-version 20))
  
  (defun swbuff-scroll-window (position)
    "Adjust horizontal scrolling to ensure that POSITION is visible."
    (setq truncate-lines t)
    (let ((automatic-hscrolling t))
      (goto-char position)))

  ;; Use mouse-1, mouse-3 on mode line buffer identification to
  ;; respectively switch to previous or next buffer.  And mouse-2 to
  ;; kill the current buffer.
  (let ((map mode-line-buffer-identification-keymap))
    (define-key map [mode-line mouse-1]
      'swbuff-switch-to-previous-buffer)
    (define-key map [mode-line drag-mouse-1]
      'swbuff-ignore)
    (define-key map [mode-line down-mouse-1]
      'swbuff-ignore)
    (define-key map [mode-line mouse-2]
      'swbuff-kill-this-buffer)
    (define-key map [mode-line mouse-3]
      'swbuff-switch-to-next-buffer))

  )
 
 ;; GNU Emacs 20 or XEmacs
 (t

  (defconst swbuff-extra-space 3
    "Extra space left in a line of the status window.
The default value correspond to the truncated glyphs + one space.")
  
  (defun swbuff-scroll-window (position)
    "Adjust horizontal scrolling to ensure that POSITION is visible."
    (setq truncate-lines t)
    ;; Don't display the XEmacs horizontal scrollbar
    (let* ((window (selected-window))
           (wdth (window-width window))
           (hscr (window-hscroll window)))
      (if (featurep 'xemacs)
          (set-specifier horizontal-scrollbar-visible-p nil window))
      (if (>= position (+ wdth hscr))
          (set-window-hscroll window (- (+ position swbuff-extra-space) wdth))
        (if (< position hscr)
            (set-window-hscroll window (- position swbuff-extra-space))))))
  
  ))

(defun swbuff-one-window-p (window)
  "Return non-nil if there is only one window in this frame.
Ignore WINDOW and the minibuffer window."
  (let ((count 0))
    (walk-windows #'(lambda (w)
                      (or (eq w window) (setq count (1+ count)))))
    (= count 1)))

(defvar swbuff-buffer-list-holder nil
  "Hold the current displayed buffer list.")

(defun swbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let ((blist  swbuff-buffer-list-holder)
        (head   (or swbuff-header    "" ))
        (separ  (or swbuff-separator " "))
        (trail  (or swbuff-trailer   "" ))
        (width  (window-width window))
        (lines  0)
        (adjust (or (eq swbuff-status-window-layout 'adjust)
                    (swbuff-one-window-p window)))
        start end buffer bname fillr)
    (save-selected-window
      (select-window window)
      (setq header-line-format nil) ;; Hide Emacs 21 header line.
      (erase-buffer)
      (setq start (point))
      (insert head)
      (if (> (point) start)
          (set-text-properties
           start (point) '(face swbuff-separator-face)))
      (while blist
        (setq buffer (car blist)
              blist  (cdr blist))
        (when (buffer-live-p buffer)
          (setq bname (buffer-name buffer)
                start (point)
                fillr (if blist separ trail))
          (when (and adjust
                     (> (- (+ start (length bname) (length fillr))
                           (* lines width))
                        width))
            (newline)
            (setq start (point)
                  lines (1+ lines)))
          (insert bname)
          (cond
           ((string-equal bname bcurr)
            (setq end (point))
            (set-text-properties
             start end '(face swbuff-current-buffer-face)))
           (t
            (set-text-properties
             start (point) '(face swbuff-default-face))))
          (setq start (point))
          (insert fillr)
          (if (> (point) start)
              (set-text-properties
               start (point) '(face swbuff-separator-face)))))
      (if adjust
          (swbuff-adjust-window)
        (swbuff-adjust-window 1)
        (swbuff-scroll-window end)))))

(defvar swbuff-timer nil
  "Timer used to discard the status window.")

(defun swbuff-show-status-window ()
  "Pop-up a status window at the bottom of the selected window.
The status window shows the list of switchable buffers where the
switched one is hilighted using `swbuff-current-buffer-face'. It is
automatically discarded after any command is executed or after the
delay specified by `swbuff-clear-delay'."
  (if (or swbuff-buffer-list-holder
          (setq swbuff-buffer-list-holder (swbuff-buffer-list)))
      (let ((bcurr (buffer-name))
            (window-min-height 1)
            cursor-in-non-selected-windows)
        (with-current-buffer (get-buffer-create swbuff-status-buffer-name)
          (let ((w (or (get-buffer-window swbuff-status-buffer-name)
                       (split-window-vertically -2))))
            (set-window-buffer w (current-buffer))
            (swbuff-layout-status-line w bcurr)
            (add-hook 'pre-command-hook 'swbuff-pre-command-hook)
            (if (timerp swbuff-timer)
                (cancel-timer swbuff-timer))
            (setq swbuff-timer (run-with-timer
                                swbuff-clear-delay nil
                                #'swbuff-discard-status-window)))))
    (message "No buffers eligible for switching.")))

(defun swbuff-discard-status-window ()
  "Discard the status window."
  (let ((w (get-buffer-window swbuff-status-buffer-name))
        (b (get-buffer swbuff-status-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))

(defun swbuff-pre-command-hook ()
  "Track successive calls to switch commands.
Run as a `pre-command-hook'."
  (if (memq this-command '(swbuff-switch-to-previous-buffer
                           swbuff-switch-to-next-buffer
                           swbuff-kill-this-buffer
                           swbuff-ignore))
      nil
    (swbuff-discard-status-window)
    (setq swbuff-buffer-list-holder nil))
  (if (timerp swbuff-timer)
      (cancel-timer swbuff-timer))
  (setq swbuff-timer nil)
  (remove-hook 'pre-command-hook 'swbuff-pre-command-hook))

(defun swbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((l (swbuff-buffer-list)))
    (and l (switch-to-buffer (nth (1- (length l)) l)))))

(defun swbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((l (nreverse (swbuff-buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l)))))

;;; Commands
;;

;;;###autoload
(defun swbuff-switch-to-previous-buffer ()
  "Switch to the previous buffer in the buffer list."
  (interactive)
  (swbuff-previous-buffer)
  (swbuff-show-status-window))

;;;###autoload
(defun swbuff-switch-to-next-buffer ()
  "Switch to the next buffer in the buffer list."
  (interactive)
  (swbuff-next-buffer)
  (swbuff-show-status-window))

;;;###autoload
(defun swbuff-kill-this-buffer ()
  "Kill the current buffer.
And update the status window if showing."
  (interactive)
  (kill-buffer (current-buffer))
  (and (get-buffer-window swbuff-status-buffer-name)
       (swbuff-show-status-window)))

(defun swbuff-default-load-hook ()
  "Default hook run when package has been loaded.
Define keyboard shortcut [C-f6] for `swbuff-switch-to-next-buffer' and
\[C-S-f6] for `swbuff-switch-to-previous-buffer'."
  (global-set-key [(control f6)]       'swbuff-switch-to-next-buffer)
  (global-set-key [(control shift f6)] 'swbuff-switch-to-previous-buffer))

(provide 'swbuff)

(run-hooks 'swbuff-load-hook)

;;; swbuff.el ends here
