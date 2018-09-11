;;; osx-location.el --- Watch and respond to changes in geographical location on OS X

;; Copyright (C) 2012-2015 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/osx-location
;; Keywords: convenience, calendar

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

;; This library uses OS X CoreLocation services to put useful
;; information into variables `osx-location-latitude' and
;; `osx-location-longitude'.

;; To use, run `osx-location-watch', which starts monitoring the
;; location asynchronously.

;; To add code which responds to location changes, use
;; `osx-location-changed-hook'.  For example, you might add a hook
;; function which updates `calendar-latitude' and `calendar-longitude'
;; (defined in the built-in library `solar').

;; Hook functions take no arguments; when your hook function runs, it
;; can use the freshly-updated values of `osx-location-latitude' and
;; `osx-location-longitude'.

;; Here's an example:

;; (eval-after-load 'osx-location
;;   '(when (eq system-type 'darwin)
;;      (add-hook 'osx-location-changed-hook
;;                (lambda ()
;;                  (setq calendar-latitude osx-location-latitude
;;                        calendar-longitude osx-location-longitude
;;                        calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude))))))

;; Comes bundled with a required executable called
;; EmacsLocationHelper, which does the communication with
;; CoreServices; if you don't trust me, you can build it yourself from
;; the instructions at https://gist.github.com/1416248 or using the
;; bundled Makefile.

;;; Code:

(require 'find-func)

(defgroup osx-location nil
  "Watch and respond to changes in geographical location on OS X"
  :group 'calendar)

(defcustom osx-location-helper "EmacsLocationHelper"
  "Name of the helper executable.
When not a full path, the executable will be assumed to reside in
the same directory as this library."
  :type 'string
  :group 'osx-location)

(defcustom osx-location-changed-hook nil
  "Hook functions to be run when the location has changed."
  :type '(repeat symbol)
  :group 'osx-location)

(defvar osx-location-latitude nil
  "Current latitude (if known) as a floating point number of degrees.")
(defvar osx-location-longitude nil
  "Current longitude (if known) as a floating point number of degrees.")



(defvar osx-location-last-match-end-pos 0
  "A position before which the last match was found.")

(make-variable-buffer-local 'osx-location-last-match-end-pos)

(defun osx-location-helper-path ()
  "Return the full path of the location helper."
  (let* ((libdir (file-name-as-directory
                  (file-name-directory
                   (find-library-name "osx-location"))))
         (bundled-path (expand-file-name osx-location-helper libdir)))
    (if (file-exists-p bundled-path)
        bundled-path
      (or (executable-find osx-location-helper)
          (error "Program named in osx-location-helper not found: %s"
                 osx-location-helper)))))

(defun osx-location-scan-for-updates ()
  "Look for and respond to process output which indicates a location change."
  (save-excursion
    (goto-char osx-location-last-match-end-pos)
    (while (re-search-forward
           "latitude,lon?gitude : \\(-?[0-9]+\\.[0-9]+\\), \\(-?[0-9]+\\.[0-9]+\\)"
           nil t)
      (setq osx-location-latitude (string-to-number (match-string 1))
            osx-location-longitude (string-to-number (match-string 2))
            osx-location-last-match-end-pos (match-end 0))
      (dolist (match '(1 2))
        (put-text-property (match-beginning match) (match-end match)
                           'face 'font-lock-keyword-face))
      (run-hooks 'osx-location-changed-hook))))

(defun osx-location-process-filter (proc string)
  "Handle when helper PROC outputs STRING.
This mostly just copies the output to the corresponding buffer,
but also triggers `osx-location-scan-for-updates'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point))
          (osx-location-scan-for-updates))
        (if moving (goto-char (process-mark proc)))))))


;; Public interface

;;;###autoload
(defun osx-location-watch ()
  "Run the helper program in a buffer and respond to its output."
  (interactive)
  (with-current-buffer (get-buffer-create "*osx-location*")
    (buffer-disable-undo)
    (unless (get-buffer-process (current-buffer))
      (let* ((helper-path (osx-location-helper-path))
             (proc (start-process (file-name-nondirectory helper-path)
                                  (current-buffer)
                                  helper-path)))
        (set-process-filter proc 'osx-location-process-filter)
        (set-process-query-on-exit-flag proc nil)
        (set (make-local-variable 'kill-buffer-hook)
             (lambda ()
               (let ((proc (get-buffer-process (current-buffer))))
                 (when proc
                   (kill-process proc)))))))))



(provide 'osx-location)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; osx-location.el ends here
