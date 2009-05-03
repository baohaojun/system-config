;;; pde-patch.el --- Patches for other extensions

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 25 Dec 2007
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

;; 

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-after-load "cperl-mode"
  '(progn
     ;; Fix error when define tags-table-list
     (defun cperl-tags-hier-init (&optional update)
       "Show hierarchical menu of classes and methods.
Finds info about classes by a scan of loaded TAGS files.
Supposes that the TAGS files contain fully qualified function names.
One may build such TAGS files from CPerl mode menu."
       (interactive "P")
       (require 'etags)
       (require 'imenu)
       (if (or update (null (nth 2 cperl-hierarchy)))
           (let ((remover (function (lambda (elt) ; (name (file1...) (file2..))
                                      (or (nthcdr 2 elt)
                                          ;; Only in one file
                                          (setcdr elt (cdr (nth 1 elt)))))))
                 pack name cons1 to l1 l2 l3 l4 b)
             ;; (setq cperl-hierarchy '(() () ())) ; Would write into '() later!
             (setq cperl-hierarchy (list l1 l2 l3))
             (if cperl-xemacs-p         ; Not checked
                 (progn
                   (or tags-file-name
                       ;; Does this work in XEmacs?
                       (call-interactively 'visit-tags-table))
                   (message "Updating list of classes...")
                   (set-buffer (get-file-buffer tags-file-name))
                   (cperl-tags-hier-fill))
               (or tags-table-list
                   (call-interactively 'visit-tags-table))
               (mapc
                (function
                 (lambda (tagsfile)
                   (message "Updating list of classes... %s" tagsfile)
                   (let ((buf (get-file-buffer tagsfile)))
                     (when buf
                       (set-buffer buf)
                       (cperl-tags-hier-fill)))))
                tags-table-list)
               (message "Updating list of classes... postprocessing..."))
             (mapcar remover (car cperl-hierarchy))
             (mapcar remover (nth 1 cperl-hierarchy))
             (setq to (list nil (cons "Packages: " (nth 1 cperl-hierarchy))
                            (cons "Methods: " (car cperl-hierarchy))))
             (cperl-tags-treeify to 1)
             (setcar (nthcdr 2 cperl-hierarchy)
                     (cperl-menu-to-keymap (cons '("+++UPDATE+++" . -999) (cdr to))))
             (message "Updating list of classes: done, requesting display...")
             ;;(cperl-imenu-addback (nth 2 cperl-hierarchy))
             ))
       (or (nth 2 cperl-hierarchy)
           (error "No items found"))
       (setq update
;;;	(imenu-choose-buffer-index "Packages: " (nth 2 cperl-hierarchy))
             (if (if (fboundp 'display-popup-menus-p)
                     (let ((f 'display-popup-menus-p))
                       (funcall f))
                   window-system)
                 (x-popup-menu t (nth 2 cperl-hierarchy))
               (require 'tmm)
               (tmm-prompt (nth 2 cperl-hierarchy))))
       (if (and update (listp update))
           (progn (while (cdr update) (setq update (cdr update)))
                  (setq update (car update)))) ; Get the last from the list
       (if (vectorp update)
           (progn
             (find-file (elt update 0))
             (cperl-etags-goto-tag-location (elt update 1))))
       (if (eq update -999) (cperl-tags-hier-init t)))

     ;; Fix for outline-level 0 is not valid level
     (defun cperl-outline-level ()
       (looking-at outline-regexp)
       (cond ((not (match-beginning 1)) 1) ; beginning-of-file
;;;; 2=package-group, 5=package-name 8=sub-name 16=head-level
             ((match-beginning 2) 1)    ; package
             ((match-beginning 8) 2)    ; sub
             ((match-beginning 16)
              (- (char-after (match-beginning 16)) ?0)) ; headN ==> N
             (t 5)))

     ;; Fix indentation for `map' and `grep'
     (defun cperl-after-block-p (lim &optional pre-block)
       "Return true if the preceeding } ends a block or a following { starts one.
Would not look before LIM.  If PRE-BLOCK is nil checks preceeding }.
otherwise following {."
       ;; We suppose that the preceding char is }.
       (save-excursion
         (condition-case nil
             (progn
               (or pre-block (forward-sexp -1))
               (cperl-backward-to-noncomment lim)
               (or (eq (point) lim)
                   (eq (preceding-char) ?\) ) ; if () {}    sub f () {}
                   (if (eq (char-syntax (preceding-char)) ?w) ; else {}
                       (save-excursion
                         (forward-sexp -1)
                         (or (looking-at "\\(else\\|continue\\|BEGIN\\|END\\|CHECK\\|INIT\\)\\>")
                             ;; sub f {}
                             (progn
                               (cperl-backward-to-noncomment lim)
                               (and (eq (char-syntax (preceding-char)) ?w)
                                    (progn
                                      (forward-sexp -1)
                                      (looking-at "sub\\>"))))))
                     (cperl-after-expr-p lim))))
           (error nil))))
     ))

(provide 'pde-patch)
;;; pde-patchs.el ends here
