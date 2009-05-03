;;; pde-load.el --- Configuration for PDE

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 22 Dec 2007
;; Version: 0.01
;; Keywords: tools, convenience, languages

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'pde-load)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'pde-vars)
(require 'template-simple)
(add-to-list 'load-path
             (concat (file-name-as-directory pde-load-path) "contrib"))
(add-to-list 'template-directory-list
             (expand-file-name "templates" pde-load-path))

(defcustom pde-extra-setting t
  "*Non-nil means more settings."
  :type 'boolean
  :group 'pde)

;; autoloads
(load "pde-loaddefs")
(autoload 'pde-perl-mode-hook "pde" "Hooks run when enter perl-mode")

(require 'help-dwim)
(help-dwim-register
 '(perldoc . [ "0-9a-zA-Z_:." perldoc-obarray nil perldoc ])
 nil
 '((require 'perldoc)
   (perldoc-recache-everyday)))

(help-dwim-register
 '(perlapi . [ "a-zA-Z0-9_" perlapi-obarray nil perlapi ])
 nil
 '((require 'perlapi)))

(defalias 'perl-mode 'cperl-mode)
(setq-default indent-tabs-mode nil)
(setq ffap-url-regexp
      (concat
       "\\`\\("
       "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
       "\\|"
       "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
       "\\)[^:]"             ; require one more character that not ":"
       ))
(eval-after-load "ffap"
  '(add-to-list 'ffap-alist '("." . pde-ffap-locate)))
(add-hook 'cperl-mode-hook 'pde-perl-mode-hook)

;; Extra setting that run only once
(when pde-extra-setting
  (require 'pde-patch)
  (require 'pde-abbv)
  ;; set it before load cperl-mode
  (setq cperl-invalid-face nil)
  (setq cperl-lazy-help-time 2)

  (setq completion-ignore-case t)
  ;; many DWIM commands work only in transient-mark-mode
  (transient-mark-mode t)
  ;; don't use ido-imenu unless turn on ido-mode
  (ido-mode t)
  (eval-after-load "imenu"
    '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))
  ;; complete reading module name when calling find-file
  (require 'complete)           ; need for define PC-include-file-path
  (partial-completion-mode t)
  (setq PC-include-file-path
        (append PC-include-file-path pde-perl-inc))
  (setq imenu-tree-auto-update t)
  (setq compilation-buffer-name-function 'pde-compilation-buffer-name)
  (add-hook 'perldoc-mode-hook 'pde-tabbar-register)
  (autoload 'comint-dynamic-complete "comint" "Complete for file name" t)
  (autoload 're-builder "re-builder-x" "Construct a regexp interactively." t)
  (setq comint-completion-addsuffix '("/" . ""))
  (setq tags-table-list '("./TAGS" "../TAGS" "../../TAGS"))
  (setq hippie-expand-try-functions-list
        '(try-expand-line
          try-expand-dabbrev
          try-expand-line-all-buffers
          try-expand-list
          try-expand-list-all-buffers
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name
          try-complete-file-name-partially
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-whole-kill))

  (global-set-key "\C-m" 'newline-and-indent)
  (global-set-key "\C-j" 'newline)
  (global-set-key (kbd "M-'") 'just-one-space)
  (global-set-key (kbd "C-M-=") 'pde-indent-dwim)
  ;; nearest key to dabbrev-expand
  (global-set-key (kbd "M-;") 'hippie-expand)
  (global-set-key (kbd "C-;") 'comment-dwim)
  (global-set-key "\C-cf" 'comint-dynamic-complete)
  (global-set-key "\C-cs" 'compile-dwim-compile)
  (global-set-key "\C-cr" 'compile-dwim-run)
  (global-set-key "\C-ci" 'imenu)
  (global-set-key "\C-cv" 'imenu-tree)
  (global-set-key "\C-cj" 'ffap)
  (global-set-key "\C-ch" 'help-dwim)
  (global-set-key "\C-xan" 'tempo-forward-mark)
  (global-set-key "\C-xap" 'tempo-backward-mark)
  (global-set-key "\C-xam" 'tempo-complete-tag)
  (global-set-key " " 'tempo-x-space)
  )

(provide 'pde-load)
;;; pde-load.el ends here
