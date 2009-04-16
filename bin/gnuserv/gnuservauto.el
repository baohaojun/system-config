;;; gnuservauto.el --- Loading gnuserv automatically

;; Copyright (C) 2004 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Maintainer: Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2004-12-27
;; Version: 1.0
;; Keywords: installation setup

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

;; Load gnuserv automatically. Should be used together with the
;; version of gnuserv/gnuclient that you should have received together
;; with this file.
;;
;; Usage: called when you use -s option with gnuclient(w). Note that
;; the following files must be in the same directory:
;;
;; - gnuserv.exe, gnuclient(w).exe, gnuservauto.el, gnuserv.el


;; Load gnuserv
(let ((load-path load-path))
  (add-to-list 'load-path (file-name-directory load-file-name))
  (require 'gnuserv))
(unless gnuserv-process
  (setq gnuserv-program 
	(expand-file-name (concat (file-name-directory load-file-name) "gnuserv.exe")))
  (gnuserv-start))

(provide 'gnuservauto)
