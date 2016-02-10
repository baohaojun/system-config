;;; skewer-less.el --- Skewer support for live LESS stylesheet updates

;; Copyright (C) 2013  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages, tools
;; Version: 20131015.622
;; X-Original-Version: DEV
;; Package-Requires: ((skewer-mode "1.5.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

;; Note that this is intended for use in place of `skewer-css-mode',
;; which does not work with lesscss.

;; Enable `skewer-less-mode' in a ".less" buffer. Save the buffer to
;; trigger an update, or hit "C-c C-k" just like in
;; `skewer-css-mode'.

;; Operates by invoking "less.refresh()" via skewer on demand, or
;; whenever the buffer is saved.

;; For this to work properly, the less javascript should be included
;; in the target web page, and less should be configured in
;; development mode, e.g.

;;    <script>
;;      var less = {env: "development"};
;;    </script>
;;    <link href="/stylesheets/application.less" rel="stylesheet/less">
;;    <script src="/path/to/less.js" type="text/javascript"></script>

;; I may consider providing an option to instead run "lessc" from
;; Emacs, then send the output via skewer-css. Let me know if you want this.

;;; Code:

(require 'skewer-mode)

(defvar skewer-less-mode-map
  (let ((m (make-sparse-keymap)))
    ;; for consistency with skewer-css
    (define-key m (kbd "C-c C-k") 'skewer-less-save-and-reload)
    m)
  "Keymap for `skewer-less-mode'.")

;;;###autoload
(define-minor-mode skewer-less-mode
  "Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

Operates by invoking \"less.refresh()\" via skewer whenever the
buffer is saved.

For this to work properly, the less javascript should be included
in the target web page, and less should be configured in
development mode, using:

        var less = {env: \"development\"};

before including \"less.js\"."
  nil
  " skewer-less"
  skewer-less-mode-map
  (if skewer-less-mode
      (add-hook 'after-save-hook 'skewer-less-reload nil t)
    (remove-hook 'after-save-hook 'skewer-less-reload t)))

(defun skewer-less-save-and-reload ()
  "When skewer appears to be active, ask for a reload."
  (interactive)
  (save-buffer)
  (skewer-less-reload))

(defun skewer-less-reload ()
  "When skewer appears to be active, ask for a reload."
  (interactive)
  (skewer-eval "less.refresh();"))


(provide 'skewer-less)
;;; skewer-less.el ends here
