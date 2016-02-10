;;; hayoo.el --- Query hayoo and show results in a tabulated buffer.

;; Copyright (C) 2014 Marko Bencun

;; Author: Marko Bencun <mbencun@gmail.com>
;; URL: https://github.com/benma/hayoo.el/
;; Version: 20140831.521
;; X-Original-Version: 0.1
;; Package-requires: ((emacs "24") (json "1.3"))
;; Keywords: hayoo, haskell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; What's this?
;;
;; This package provides a tabulated interface to hayoo.


(require 'tabulated-list)
(require 'json)

(defvar hayoo--results)

(defvar hayoo/hayoo-query-url "http://hayoo.fh-wedel.de/json/?query=%s"
  "Query url for json hayoo results.")

(defvar hayoo/hackage-package-url "http://hackage.haskell.org/package/%s"
  "Url for a package on hackage.")

(defun hayoo--query-and-list-results (query)
  (url-retrieve
   (format hayoo/hayoo-query-url query)
   (lambda (status)
     (re-search-forward "\r?\n\r?\n")
     (let* ((res (json-read-object))
            (results (assoc-default 'result res)))
       (setq hayoo--results results)
       (kill-buffer)
       (switch-to-buffer-other-window (hayoo--list-results))
       (message
        "Commands: [v] (visit package), [RET] (visit result); q to quit.")))
   nil t t))

(defun hayoo--current-result ()
  (elt hayoo--results (tabulated-list-get-id)))

(defun hayoo--visit-package ()
  (interactive)
  (browse-url (format hayoo/hackage-package-url (assoc-default 'resultPackage (hayoo--current-result)))))

(defun hayoo--visit-thing ()
  (interactive)
  (browse-url (assoc-default 'resultUri (hayoo--current-result))))

(defvar hayoo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "v" 'hayoo--visit-package)
    (define-key map "\C-m" 'hayoo--visit-thing)
    map)
  "Local keymap for `Hayoo' buffers.")

(define-derived-mode hayoo-mode tabulated-list-mode "Hayoo")

(defun hayoo--list-results ()
  (let ((buffer (get-buffer-create "*Hayoo*")))
    (with-current-buffer buffer
      (hayoo-mode)
      (setq tabulated-list-format
            (vector '("type" 10 t)
                    '("package" 13 t)
                    '("modules" 30 t)
                    '("name" 1 t)))
      ;; the entries below are populated sorted by score.
      ;; do not let tabulated-list-mode sort for us, s.t.
      ;; the orignal sort is preserved
      (setq tabulated-list-sort-key nil)
      (let ((count (length hayoo--results))
            entries)

        (dotimes (i count)
          (let* ((result (elt hayoo--results i))
                 (result-type (assoc-default 'resultType result))
                 (result-name (assoc-default 'resultName result))
                 (result-package (assoc-default 'resultPackage result))
                 (result-modules (assoc-default 'resultModules result))
                 (result-signature (assoc-default 'resultSignature result)))
            (push (list i (vector
                           result-type

                           (cond ((equal result-type "package")
                                  result-name)
                                 (t result-package))

                           (if result-modules
                               (elt result-modules 0)
                             "")

                           (if (and result-signature (not (string= "" result-signature)))
                               (format "%s :: %s" result-name result-signature)
                             result-name)))
                  entries)))
        (setq tabulated-list-entries (nreverse entries)))
      (tabulated-list-init-header)
      (tabulated-list-print))
    buffer))

;;;###autoload
(defun hayoo-query (query)
  "Do a Hayoo search for QUERY."
  (interactive
   (let ((def))
     (if (region-active-p)
         (setq def (buffer-substring-no-properties (region-beginning) (region-end)))
       (when (fboundp 'haskell-ident-at-point)
         (setq def (haskell-ident-at-point)))
       (if (and def (symbolp def)) (setq def (symbol-name def))))
       
     (list (read-string (if def
                            (format "Hayoo query (default %s): " def)
                          "Hayoo query: ")
                        nil nil def))))
  (hayoo--query-and-list-results query))

(provide 'hayoo)
;;; hayoo.el ends here
