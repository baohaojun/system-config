;;; pde-vars.el --- Variables for PDE

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 23 Dec 2007
;; Version: 0.01
;; Keywords: languages, tools

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

(defvar pde-version "0.2.6"
  "Version of PDE")

(defgroup pde nil
  "Perl Development Environment"
  :group 'languages
  :prefix "pde-")

(defcustom pde-perl-program "perl"
  "*Name of perl"
  :type 'string
  :group 'pde)

(defcustom pde-perl-version "5.8.8"
  "*Version of perl used"
  :type 'string
  :group 'pde)

(defcustom pde-perldoc-program "perldoc"
  "*Name of perldoc"
  :type 'string
  :group 'pde)

(defcustom pde-find-program "find"
  "Name of GNU find program"
  :type 'string
  :group 'pde)

(defcustom pde-completing-read-function 'ido-completing-read
  "Function to call in minibuffer completion."
  :type 'function
  :group 'pde)

(defcustom pde-load-path (file-name-directory load-file-name)
  "*Directory name of pde"
  :type 'directory
  :group 'pde)

(defcustom pde-perl-inc
  (when pde-perl-program
    (let ((cmd (format "%s -e \"print join(';', grep { -d && /^[^.]/} @INC);\""
                       pde-perl-program)))
      (mapcar 'file-name-as-directory
              (split-string (shell-command-to-string cmd) ";"))))
  "*Include path of perl"
  :type '(repeat directory)
  :group 'pde)

(provide 'pde-vars)
;;; pde-vars.el ends here
