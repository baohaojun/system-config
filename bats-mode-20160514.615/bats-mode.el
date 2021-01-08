;;; bats-mode.el --- Emacs mode for editing and running Bats tests

;; Author: Doug MacEachern
;; URL: https://github.com/dougm/bats-mode
;; Package-Version: 20160514.615
;; Package-Commit: d519f7c89f5ae17dfc33400596df4564b478315f
;; Version: 0.1.0
;; Keywords: bats, tests

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

(require 'compile)
(require 'sh-script)

(defvar bats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'bats-run-all)
    (define-key map (kbd "C-c C-,") 'bats-run-current-file)
    (define-key map (kbd "C-c M-,") 'bats-run-current-test)
    map)
  "Keymap used in Bats mode.")

(defvar bats-check-program (executable-find (concat (file-name-directory
                                                     (or load-file-name
                                                         buffer-file-name))
                                                    "bin/batscheck"))
  "Default batscheck program.")

;;;###autoload
(define-derived-mode bats-mode sh-mode "Bats"
  "Major mode for editing and running Bats tests.

See URL `https://github.com/sstephenson/bats'.

\\{bats-mode-map}"

  (set (make-local-variable 'flycheck-sh-shellcheck-executable) bats-check-program)
  (set (make-local-variable 'sh-shell) 'bash)

  ;; bash font-lock + a few bats keywords
  (add-to-list 'sh-font-lock-keywords-var
               '(bats sh-append bash
                      ("\\(@test\\)" 1 font-lock-keyword-face)
                      ("\\(load\\|run\\|skip\\)" 1 font-lock-function-name-face)))

  ;; Match file/line from bats errors in compile-mode.
  (add-to-list 'compilation-error-regexp-alist 'bats t)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(bats . ("file \\([^ \t\r\n(]+\\), line \\([0-9]+\\)" 1 2)) t))

(defvar bats-program "bats"
  "Default bats program.")

(defun bats-current-test ()
  "Find current bats test."
  (let (test-name)
    (save-excursion
      (end-of-line)
      (unless (search-backward-regexp "^@test \"\\(.*?\\)\" {" nil t)
        (error "Unable to find a @test"))
      (setq test-name (match-string 1)))
    test-name))

(defun bats-run (file &optional name)
  "Run bats -t FILE.
NAME if given is used as the bats test pattern."
  (let ((cmd (concat bats-program " -t " file)))
    (compile (if name (concat (format "BATS_TEST_PATTERN='^%s$' " name) cmd)
               cmd))))

(defun bats-run-current-test ()
  "Run bats with the current test at point."
  (interactive)
  (bats-run-current-file (bats-current-test)))

(defun bats-run-current-file (&optional name)
  "Run bats with the current buffer file.
NAME if given is used as the bats test pattern."
  (interactive)
  (bats-run buffer-file-name name))

(defun bats-run-all ()
  "Run bats in the current directory."
  (interactive)
  (bats-run "."))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.bats\\'" . bats-mode)))

(eval-after-load 'flycheck
  '(progn
     (flycheck-add-mode 'sh-shellcheck 'bats-mode)))

(provide 'bats-mode)

;;; bats-mode.el ends here
