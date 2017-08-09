;;; gitlab-ui.el --- Gitlab UI tools

;; Copyright (C) 2014, 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'ansi-color)
(require 'tabulated-list)


;; Faces

(defface helm-gitlab--title
  '((((class color) (background light)) :foreground "red" :weight semi-bold)
    (((class color) (background dark)) :foreground "green" :weight semi-bold))
  "face of Gitlab information"
  :group 'helm-gitlab)

(defface helm-gitlab--red-face
  '((((class color)) :foreground "#cd4d40"))
  "Red color.."
  :group 'helm-gitlab)

(defface helm-gitlab--green-face
  '((((class color)) :foreground "#61b361"))
  "Green color."
  :group 'helm-gitlab)

(defface helm-gitlab--yellow-face
  '((((class color)) :foreground "#e7e24c"))
  "Yellow color."
  :group 'helm-gitlab)

(defun colorize-dot (color)
  (cond
   ((string= color  "red")
    (propertize "●" 'face 'helm-gitlab--red-face))
   ((string= color "yellow")
    (propertize "●" 'face 'helm-gitlab--yellow-face))
   ((string= color  "green")
    (propertize "●" 'face 'helm-gitlab--green-face))
   (t (concat "Unknown: " "'" color "' "))))


;; Buffer

(defconst helm-gitlab--buffer-name "*helm-gitlab*"
  "Name of helm-gitlab mode buffer.")


;; Entries

(defconst gitlab-list-format
  [("Key" 15 t :right-align t)
   ("Value" 40 t)]
  "List format.")

(defconst gitlab-list-sort-key
  '("Name" . nil)
  "Sort table on this key.")

;; TODO
;; (defun gitlab-list-entries ()
;;   "Create the entries for the service list."
;;   (-map
;;    (lambda (service)
;;      (list
;;       (prodigy-service-id service)
;;       (apply 'vector
;;              (--map
;;               (funcall it service)
;;               '(prodigy-marked-col
;;                 prodigy-name-col
;;                 prodigy-status-col
;;                 prodigy-tags-col)))))
;;    (prodigy-services)))

;; Mode

(defvar gitlab-mode-hook nil
  "Mode hook for `gitlab-mode'.")

;;;###autoload
(define-derived-mode gitlab-mode tabulated-list-mode "Gitlab"
  "Special mode for Gitlab buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Gitlab")
  (setq major-mode 'gitlab-mode)
  (use-local-map gitlab-mode-map)
  (setq tabulated-list-format gitlab-list-format)
  (setq tabulated-list-entries 'gitlab-list-entries)
  (setq tabulated-list-sort-key gitlab-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (run-mode-hooks 'gitlab-mode-hook))



(provide 'gitlab-ui)
;;; gitlab-ui.el ends here
