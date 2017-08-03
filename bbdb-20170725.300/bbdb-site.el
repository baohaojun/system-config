;;; bbdb-site.el.in --- site-specific variables for BBDB -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017 Roland Winkler <winkler@gnu.org>

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst bbdb-version "@PACKAGE_VERSION@" "Version of BBDB.")

(if (< emacs-major-version 24)
  (error "BBDB %s requires GNU Emacs 24 or later" bbdb-version))

(defconst bbdb-version-date "@PACKAGE_DATE@"
  "Version date of BBDB.")

(defcustom bbdb-tex-path '("@pkgdatadir@")
  "List of directories with the BBDB TeX files.
If this is t assume that these files reside in directories
that are part of the regular TeX search path"
  :group 'bbdb-utilities-print
  :type '(choice (const :tag "Files in TeX path" t)
                 (repeat (directory :tag "Directory"))))

(provide 'bbdb-site)

;;; bbdb-site.el.in ends here
