;;; csv-mode-tests.el --- Tests for CSV mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'csv-mode)
(eval-when-compile (require 'subr-x))

(ert-deftest csv-mode-tests-end-of-field ()
  (with-temp-buffer
    (csv-mode)
    (insert "aaa,bbb")
    (goto-char (point-min))
    (csv-end-of-field)
    (should (equal (buffer-substring (point-min) (point))
                   "aaa"))
    (forward-char)
    (csv-end-of-field)
    (should (equal (buffer-substring (point-min) (point))
                   "aaa,bbb"))))

(ert-deftest csv-mode-tests-end-of-field-with-quotes ()
  (with-temp-buffer
    (csv-mode)
    (insert "aaa,\"b,b\"")
    (goto-char (point-min))
    (csv-end-of-field)
    (should (equal (buffer-substring (point-min) (point))
                   "aaa"))
    (forward-char)
    (csv-end-of-field)
    (should (equal (buffer-substring (point-min) (point))
                   "aaa,\"b,b\""))))

(ert-deftest csv-mode-tests-beginning-of-field ()
  (with-temp-buffer
    (csv-mode)
    (insert "aaa,bbb")
    (csv-beginning-of-field)
    (should (equal (buffer-substring (point) (point-max))
                   "bbb"))
    (backward-char)
    (csv-beginning-of-field)
    (should (equal (buffer-substring (point) (point-max))
                   "aaa,bbb"))))

(ert-deftest csv-mode-tests-beginning-of-field-with-quotes ()
  (with-temp-buffer
    (csv-mode)
    (insert "aaa,\"b,b\"")
    (csv-beginning-of-field)
    (should (equal (buffer-substring (point) (point-max))
                   "\"b,b\""))
    (backward-char)
    (csv-beginning-of-field)
    (should (equal (buffer-substring (point) (point-max))
                   "aaa,\"b,b\""))))

(defun csv-mode-tests--align-fields (before after)
  (with-temp-buffer
    (insert (string-join before "\n"))
    (csv-align-fields t (point-min) (point-max))
    (should (equal (buffer-string) (string-join after "\n")))))

(ert-deftest csv-mode-tests-align-fields ()
  (csv-mode-tests--align-fields
   '("aaa,bbb,ccc"
     "1,2,3")
   '("aaa, bbb, ccc"
     "1  , 2  , 3")))

(ert-deftest csv-mode-tests-align-fields-with-quotes ()
  (csv-mode-tests--align-fields
   '("aaa,\"b,b\",ccc"
     "1,2,3")
   '("aaa, \"b,b\", ccc"
     "1  , 2    , 3")))

(provide 'csv-mode-tests)
;;; csv-mode-tests.el ends here
