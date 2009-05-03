;;; tabbar-x.el --- Tabbar for PDE

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 23 Dec 2007
;; Version: 0.01
;; Keywords: tools

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

;; I want some buffer display tabbar, and most buffer don't.
;; They can groups together. That some kind like tabbar-minor-mode
;; this extension implement a local tabbar without affect global
;; tabbar-mode.
;; 
;; perl: Imenu Pods Dirs
;; perldb-ui: breakpoints 
;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'tabbar)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tabbar)

(defvar tabbar-x-buffer-group nil)
(defvar tabbar-x-tabsets (make-vector 31 0))

(defun tabbar-x-buffer-list ()
  "Return buffers for tabbar"
  (let (buffers)
    ;; remove killed buffers
    (setq tabbar-x-buffer-group
          (mapcar (lambda (group)
                    (let (bufs)
                      (mapc (lambda (buf)
                              (when (buffer-live-p buf)
                                (push buf bufs)
                                (push buf buffers)))
                            (cdr group))
                      (cons (car group) bufs)))
                  tabbar-x-buffer-group))
    buffers))

(defun tabbar-x-buffer-groups (name)
  "Return group name for buffer"
  (let ((buffer (get-buffer name))
        groups)
    (mapc (lambda (group)
            (if (memq buffer (cdr group))
                (push (car group) groups)))
          tabbar-x-buffer-group)
    (nreverse groups)))

(defun tabbar-x-register (group buffer)
  "Register the buffer to the group."
  (let ((buffers (assoc group tabbar-x-buffer-group)))
    (if buffers
        (progn
          (setq tabbar-x-buffer-group (delq buffers tabbar-x-buffer-group))
          (setq buffers (cdr buffers))
          (add-to-list 'buffers buffer)
          (push (cons group buffers) tabbar-x-buffer-group))
      (setq tabbar-x-buffer-group
            (cons (list group buffer)
                  tabbar-x-buffer-group)))
    (with-current-buffer buffer
      (set (make-local-variable 'tabbar-tabsets) tabbar-x-tabsets)
      (set (make-local-variable 'tabbar-buffer-list-function)
           'tabbar-x-buffer-list)
      (set (make-local-variable 'tabbar-buffer-groups-function)
           'tabbar-x-buffer-groups)
      (add-hook 'kill-buffer-hook 'tabbar-x-kill-buffer nil t)
      (setq header-line-format tabbar-header-line-format)
      (force-mode-line-update))))

(defun tabbar-x-kill-buffer ()
  (and
   (eq tabbar-current-tabset-function 'tabbar-buffer-tabs)
   (eq (current-buffer) (window-buffer (selected-window)))
   (let ((bl (tabbar-tab-values (tabbar-current-tabset)))
         (bn (buffer-name))
         found sibling)
     (while (and bl (not found))
       (if (equal bn (car bl))
           (setq found t)
         (setq sibling (car bl)))
       (setq bl (cdr bl)))
     (when (setq sibling (or (car bl) sibling))
       ;; Move sibling buffer in front of the buffer list.
       (save-current-buffer
         (switch-to-buffer sibling))))))

(provide 'tabbar-x)
;;; tabbar-x.el ends here
