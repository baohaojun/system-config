;;; ccc.el --- buffer local cursor color control library

;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Copyright (C) 2001, 2002, 2004, 2005,
;;   2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
;;   2015, SKK Development Team <skk@ring.gr.jp>

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; URL: https://github.com/skk-dev/ddskk/blob/master/READMEs/README.ccc.org
;; Package-Version: 20200314.1557
;; Keywords: cursor

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Buffer local frame parameters
;; --- cursor, foreground, background
;; --- TODO: support other frame parameters
;;           should use uni prefix for functions and variables?

;;; Code:

(eval-when-compile
  (require 'advice))

;; Internal variables.
(defvar ccc-buffer-local-cursor-color nil)
(make-variable-buffer-local 'ccc-buffer-local-cursor-color)

(defvar ccc-buffer-local-foreground-color nil)
(make-variable-buffer-local 'ccc-buffer-local-foreground-color)

(defvar ccc-buffer-local-background-color nil)
(make-variable-buffer-local 'ccc-buffer-local-background-color)

(defvar ccc-default-cursor-color nil)
(defvar ccc-default-foreground-color nil)
(defvar ccc-default-background-color nil)

;; Frame parameters.
(defsubst ccc-current-cursor-color ()
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
(defsubst ccc-initial-cursor-color ()
  (cdr (assq 'cursor-color initial-frame-alist)))
(defsubst ccc-default-cursor-color ()
  (or ccc-default-cursor-color
      (cdr (assq 'cursor-color default-frame-alist))))
(defsubst ccc-fallback-cursor-color ()
  (if (eq frame-background-mode 'dark)
      "white"
    "black"))

(defsubst ccc-current-foreground-color ()
  (cdr (assq 'foreground-color (frame-parameters (selected-frame)))))
(defsubst ccc-initial-foreground-color ()
  (cdr (assq 'foreground-color initial-frame-alist)))
(defsubst ccc-default-foreground-color ()
  (or ccc-default-foreground-color
      (cdr (assq 'foreground-color default-frame-alist))))
(defsubst ccc-fallback-foreground-color ()
  (if (eq frame-background-mode 'dark)
      "white"
    "black"))

(defsubst ccc-current-background-color ()
  (cdr (assq 'background-color (frame-parameters (selected-frame)))))
(defsubst ccc-initial-background-color ()
  (cdr (assq 'background-color initial-frame-alist)))
(defsubst ccc-default-background-color ()
  (or ccc-default-background-color
      (cdr (assq 'background-color default-frame-alist))))
(defsubst ccc-fallback-background-color ()
  (if (eq frame-background-mode 'dark)
      "black"
    "white"))

(defsubst ccc-frame-cursor-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'ccc-frame-cursor-color))
(defsubst ccc-set-frame-cursor-color (frame color)
  (modify-frame-parameters frame (list (cons 'ccc-frame-cursor-color color))))

(defsubst ccc-frame-foreground-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'ccc-frame-foreground-color))
(defsubst ccc-set-frame-foreground-color (frame color)
  (when (eval-when-compile (>= emacs-major-version 23))
    (unless (window-system frame)
      (setq color "unspecified-fg")))
  (modify-frame-parameters frame (list (cons 'ccc-frame-foreground-color color))))

(defsubst ccc-frame-background-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'ccc-frame-background-color))
(defsubst ccc-set-frame-background-color (frame color)
  (when (eval-when-compile (>= emacs-major-version 23))
    (unless (window-system frame)
      (setq color "unspecified-bg")))
  (modify-frame-parameters frame (list (cons 'ccc-frame-background-color color))))

;; Functions.
(defsubst ccc-read-color (prompt)
  (list (facemenu-read-color prompt)))

(defsubst ccc-color-equal (a b)
  (facemenu-color-equal a b))

(defun ccc-setup-new-frame (frame)
  (ccc-set-frame-cursor-color frame (or (ccc-default-cursor-color)
                                        (ccc-fallback-cursor-color)))
  (ccc-set-frame-foreground-color frame (or (ccc-default-foreground-color)
                                            (ccc-fallback-foreground-color)))
  (ccc-set-frame-background-color frame (or (ccc-default-background-color)
                                            (ccc-fallback-background-color))))

;;;###autoload
(defun ccc-setup ()
  (add-hook 'post-command-hook 'ccc-update-buffer-local-frame-params)
  (add-hook 'after-make-frame-functions 'ccc-setup-new-frame)
  ;; Determine default colors for frames other than the initial frame.
  (setq ccc-default-cursor-color (or (ccc-default-cursor-color)
                                     (ccc-current-cursor-color))
        ccc-default-foreground-color (or (ccc-default-foreground-color)
                                         (ccc-current-foreground-color))
        ccc-default-background-color (or (ccc-default-background-color)
                                         (ccc-current-background-color)))
  ;; Set up colors for the initial frame.
  (let ((frame (selected-frame)))
    (ccc-set-frame-cursor-color frame (or (ccc-initial-cursor-color)
                                          (ccc-default-cursor-color)
                                          (ccc-fallback-cursor-color)))
    (ccc-set-frame-foreground-color frame (or (ccc-initial-foreground-color)
                                              (ccc-default-foreground-color)
                                              (ccc-fallback-background-color)))
    (ccc-set-frame-background-color frame (or (ccc-initial-background-color)
                                              (ccc-default-background-color)
                                              (ccc-fallback-background-color)))))

;;;###autoload
(defun ccc-update-buffer-local-frame-params (&optional buffer)
  (with-current-buffer (if (buffer-live-p buffer)
                           buffer
                         (window-buffer (selected-window)))
    (ccc-update-buffer-local-cursor-color)
    (ccc-update-buffer-local-foreground-color)
    (ccc-update-buffer-local-background-color)))

;;
;; buffer-local-cursor
;;
(defun ccc-set-buffer-local-cursor-color (color-name)
  (interactive (ccc-read-color "Cursor color: "))
  (let ((local ccc-buffer-local-cursor-color))
    (setq ccc-buffer-local-cursor-color
          (or color-name
              (ccc-frame-cursor-color)))
    (condition-case nil
        (ccc-update-buffer-local-cursor-color)
      (error
       (setq ccc-buffer-local-cursor-color local)))))

(defun ccc-update-buffer-local-cursor-color ()
  (let ((color (if (stringp ccc-buffer-local-cursor-color)
                   ccc-buffer-local-cursor-color
                 (ccc-frame-cursor-color))))
    (when (and (stringp color)
               (x-color-defined-p color)
               (not (ccc-color-equal color (ccc-current-cursor-color))))
      (set-cursor-color color))))

(defun ccc-set-cursor-color-buffer-local (arg)
  (if arg
      (setq ccc-buffer-local-cursor-color (ccc-current-cursor-color))
    (set-cursor-color (ccc-frame-cursor-color))
    (setq ccc-buffer-local-cursor-color nil)))

;;
;; ccc-buffer-local-foreground-color
;;
(defun ccc-set-buffer-local-foreground-color (color-name)
  (interactive (ccc-read-color "Foreground color: "))
  (unless window-system
    (setq color-name nil))
  (let ((local ccc-buffer-local-foreground-color))
    (setq ccc-buffer-local-foreground-color
          (or color-name
              (ccc-frame-foreground-color)))
    (condition-case nil
        (ccc-update-buffer-local-foreground-color)
      (error
       (setq ccc-buffer-local-foreground-color local)))))

(defun ccc-update-buffer-local-foreground-color ()
  (let ((color (if (stringp ccc-buffer-local-foreground-color)
                   ccc-buffer-local-foreground-color
                 (ccc-frame-foreground-color))))
    (when (and window-system
               (stringp color)
               (x-color-defined-p color)
               (not (ccc-color-equal color (ccc-current-foreground-color))))
      (set-foreground-color color))))

(defun ccc-set-foreground-color-buffer-local (arg)
  (if arg
      (setq ccc-buffer-local-foreground-color (ccc-current-foreground-color))
    (set-foreground-color (ccc-frame-foreground-color))
    (setq ccc-buffer-local-foreground-color nil)))

;;
;; ccc-buffer-local-background-color
;;
(defun ccc-set-buffer-local-background-color (color-name)
  (interactive (ccc-read-color "Background color: "))
  (unless window-system
    (setq color-name nil))
  (let ((local ccc-buffer-local-background-color))
    (setq ccc-buffer-local-background-color
          (or color-name
              (ccc-frame-background-color)))
    (condition-case nil
        (ccc-update-buffer-local-background-color)
      (error
       (setq ccc-buffer-local-background-color local)))))

(defun ccc-update-buffer-local-background-color ()
  (let ((color (if (stringp ccc-buffer-local-background-color)
                   ccc-buffer-local-background-color
                 (ccc-frame-background-color))))
    (when (and window-system
               (stringp color)
               (x-color-defined-p color)
               (not (ccc-color-equal color (ccc-current-background-color))))
      (set-background-color color))))

(defun ccc-set-background-color-buffer-local (arg)
  (if arg
      (setq ccc-buffer-local-background-color (ccc-current-background-color))
    (set-background-color (ccc-frame-background-color))
    (setq ccc-buffer-local-background-color nil)))

(defun ccc-setup-current-colors ()
  (setq ccc-default-cursor-color (ccc-current-cursor-color)
        ccc-default-foreground-color (ccc-current-foreground-color)
        ccc-default-background-color (ccc-current-background-color))
  (ccc-set-frame-cursor-color (selected-frame) (ccc-current-cursor-color))
  (ccc-set-frame-foreground-color (selected-frame) (ccc-current-foreground-color))
  (ccc-set-frame-background-color (selected-frame) (ccc-current-background-color)))

;; Advices.
(defadvice modify-frame-parameters (after ccc-ad activate)
  (when (and (assq 'cursor-color (ad-get-arg 1))
             (null ccc-buffer-local-cursor-color))
    (ccc-set-frame-cursor-color (ad-get-arg 0)
                                (cdr (assq 'cursor-color (ad-get-arg 1)))))
  (when (and (assq 'foreground-color (ad-get-arg 1))
             (null ccc-buffer-local-foreground-color))
    (ccc-set-frame-foreground-color (ad-get-arg 0)
                                    (cdr (assq 'foreground-color (ad-get-arg 1)))))
  (when (and (assq 'background-color (ad-get-arg 1))
             (null ccc-buffer-local-background-color))
    (ccc-set-frame-background-color (ad-get-arg 0)
                                    (cdr (assq 'background-color
                                               (ad-get-arg 1))))))

(defadvice custom-theme-checkbox-toggle (after ccc-ad activate)
  (setq ccc-default-cursor-color (ccc-current-cursor-color)
        ccc-default-foreground-color (ccc-current-foreground-color)
        ccc-default-background-color (ccc-current-background-color))
  (ccc-set-frame-cursor-color (selected-frame) (ccc-current-cursor-color))
  (ccc-set-frame-foreground-color (selected-frame) (ccc-current-foreground-color))
  (ccc-set-frame-background-color (selected-frame) (ccc-current-background-color)))

(defadvice enable-theme (after ccc-ad activate)
  (ccc-setup-current-colors))

(defadvice disable-theme (after ccc-ad activate)
  (ccc-setup-current-colors))

(provide 'ccc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ccc.el ends here
