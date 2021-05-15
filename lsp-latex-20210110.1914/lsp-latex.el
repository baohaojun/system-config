;;; lsp-latex.el --- lsp-mode client for LaTeX, on texlab     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: languages, tex
;; Package-Version: 20210110.1914
;; Package-Commit: 5fc536f24dc659f998bc673129d9e7c4b20d297c

;; Version: 1.3.2

;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0"))
;; URL: https://github.com/ROCKTAKEY/lsp-latex

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
;;; lsp-mode client for LaTeX.
;;; How to Use?
;;   - First, you have to install ~texlab~.
;;     Please install this from https://github.com/latex-lsp/texlab/releases .
;;   - Next, you should make ~lsp-mode~ available.
;;     See https://github.com/emacs-lsp/lsp-mode.
;;   - Now, you can use Language Server Protocol (LSP) on (la)tex-mode or
;;     yatex-mode just to evaluate this:

;;   (add-to-list 'load-path "/path/to/lsp-latex")
;;   (require 'lsp-latex)
;;   ;; "texlab" must be located at a directory contained in `exec-path'.
;;   ;; If you want to put "texlab" somewhere else,
;;   ;; you can specify the path to "texlab" as follows:
;;   ;; (setq lsp-latex-texlab-executable "/path/to/texlab")

;;   (with-eval-after-load "tex-mode"
;;    (add-hook 'tex-mode-hook 'lsp)
;;    (add-hook 'latex-mode-hook 'lsp))

;;   ;; For YaTeX
;;   (with-eval-after-load "yatex"
;;    (add-hook 'yatex-mode-hook 'lsp))

;;; Functions
;;;; ~lsp-latex-build~
;;    Build .tex files with texlab.
;;    It use latexmk internally, so add .latexmkrc if you want to customize
;;    build commands or options.

;;    This command build asynchronously by default, while it build synchronously
;;    with prefix argument(C-u).
;;; Note
;;   In this package, you can use even texlab v0.4.2 or older, written with Java,
;;   though it is not recommended.  If you want to use them, you can write like:

;;   ;; Path to Java executable.  If it is added to environmental PATH,
;;   ;; you don't have to write this.
;;   (setq lsp-latex-java-executable "/path/to/java")

;;   ;; "texlab.jar" must be located at a directory contained in `exec-path'
;;   ;; "texlab" must be located at a directory contained in `exec-path'.
;;   (setq lsp-latex-texlab-jar-file 'search-from-exec-path)
;;   ;; If you want to put "texlab.jar" somewhere else,
;;   ;; you can specify the path to "texlab.jar" as follows:
;;   ;; (setq lsp-latex-texlab-jar-file "/path/to/texlab.jar")

;;; License
;;   This package is licensed by GPLv3. See the file "LICENSE".

;;; Code:
(require 'lsp-mode)
(require 'cl-lib)

(defgroup lsp-latex nil
  "Language Server Protocol client for LaTeX."
  :group 'lsp-mode)


;;; For texlab v0.4.2 or older.
(defcustom lsp-latex-java-executable "java"
  "Executable command to run Java.
This is used with `lsp-latex-java-argument-list'.

This variable is only for texlab v0.4.2 or older.  If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :type 'string)

(defcustom lsp-latex-java-argument-list '("-jar")
  "List of arguments passed to `lsp-latex-java-executable'.

This variable is only for texlab v0.4.2 or older.  If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :risky t
  :type '(repeat string))

(defcustom lsp-latex-texlab-jar-file 'search-from-exec-path
  "File named \"texlab.jar\".
You can install it from https://github.com/latex-lsp/texlab/releases/ .

The value can be a string (path to \"texlab.jar\") or the symbol
search-from-exec-path. See the docstring of `lsp-latex-get-texlab-jar-file'.

This variable is only for texlab v0.4.2 or older. If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :type '(choice string (const search-from-exec-path)))

(defcustom lsp-latex-texlab-jar-argument-list '()
  "List of arguments passed to `lsp-latex-texlab-jar-file'.

This variable is only for texlab v0.4.2 or older.  If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :type '(repeat string))

(defun lsp-latex-get-texlab-jar-file ()
  "Return the path to \"texlab.jar\".

If `lsp-latex-texlab-jar-file' is a string, return it.
If `lsp-latex-texlab-jar-file' is the symbol search-from-exec-path,
then search a file named \"texlab.jar\" from variable `exec-path'.

This function is only for texlab v0.4.2 or older. If you use newer,
You don't have to set or care about this variable."
  (cond
   ((stringp lsp-latex-texlab-jar-file)
    lsp-latex-texlab-jar-file)
   ((eq lsp-latex-texlab-jar-file 'search-from-exec-path)
    (locate-file "texlab.jar" exec-path))
   (t (error "Invalid value of `lsp-latex-texlab-jar-file'"))))


;;; For texlab v1.0.0 or newer.
(defcustom lsp-latex-texlab-executable
  (cond ((eq system-type 'windows-nt)
         "texlab.exe")
        (t "texlab"))
  "Executable command to run texlab.
Called with the arguments in `lsp-latex-texlab-executable-argument-list'."
  :group 'lsp-latex
  :type 'string)

(defcustom lsp-latex-texlab-executable-argument-list '()
  "List of Arguments passed to `lsp-latex-texlab-executable'."
  :group 'lsp-latex
  :type '(repeat string))



(defcustom lsp-latex-root-directory "."
  "Root directory of each buffer."
  :group 'lsp-latex
  :risky t
  :type 'string)

(defcustom lsp-latex-build-executable "latexmk"
  "Build command used on `lsp-latex-build'."
  :group 'lsp-latex
  :risky t
  :type 'string)

(defcustom lsp-latex-build-args
  '("-pdf" "-interaction=nonstopmode" "-synctex=1" "%f")
  "Argument list passed to `lsp-latex-build-executable'.
Value is used on `lsp-latex-build'.
\"%f\" can be used as the path of the TeX file to compile."
  :group 'lsp-latex
  :risky t
  :type '(repeat string))

(defcustom lsp-latex-build-on-save nil
  "Build after saving a file or not."
  :group 'lsp-latex
  :type 'boolean)

(defcustom lsp-latex-build-output-directory "."
  "Directory to which built file is put.
Note that you should change `lsp-latex-build-args' to change output directory.
If you use latexmk, use \"-outdir\" flag."
  :group 'lsp-latex
  :type 'string
  :risky t)

(defcustom lsp-latex-forward-search-after nil
  "Execute forward-research after building."
  :group 'lsp-latex
  :type 'boolean)

(defcustom lsp-latex-forward-search-executable nil
  "Executable command used to search in preview.
It is passed server as \"latex.forwardSearch.executable\"."
  :group 'lsp-latex
  :type 'string
  :risky t)

(defcustom lsp-latex-forward-search-args nil
  "Argument list passed to `lsp-latex-forward-search-executable'.
It is passed server as \"latex.forwardSearch.executable\".

Placeholders
    %f: The path of the current TeX file.
    %p: The path of the current PDF file.
    %l: The current line number."
  :group 'lsp-latex
  :type '(repeat string)
  :risky t)

(defcustom lsp-latex-lint-on-change nil
  "Lint using chktex after changing a file."
  :group 'lsp-latex
  :type 'boolean)

(defcustom lsp-latex-lint-on-save nil
  "Lint using chktex after saving a file."
  :group 'lsp-latex
  :type 'boolean)

(defcustom lsp-latex-bibtex-formatting-line-length 120
  "Maximum amount of line on formatting BibTeX files.
0 means disable."
  :group 'lsp-latex
  :type 'integerp)

(defcustom lsp-latex-bibtex-formatting-formatter "texlab"
  "Formatter used to format BibTeX file.
You can choose \"texlab\" or \"latexindent\"."
  :group 'lsp-latex
  :type '(choice (const "texlab") (const "latexindent")))

(defun lsp-latex--build-args-getter ()
  "Get `lsp-latex-build-args' with changing to vector.
Because `json-serialize' cannot recognize normal list as array of json,
should be vector."
  (vconcat lsp-latex-build-args))

(defun lsp-latex--forward-search-args-getter ()
  "Get `lsp-latex-build-args' with changing to vector.
Because `json-serialize' cannot recognize normal list as array of json,
should be vector."
  (vconcat lsp-latex-forward-search-args))

(lsp-register-custom-settings
 `(("latex.rootDirectory"            lsp-latex-root-directory)
   ("latex.build.executable"         lsp-latex-build-executable)
   ("latex.build.args"               lsp-latex--build-args-getter)
   ("latex.build.onSave"             lsp-latex-build-on-save t)
   ("latex.build.outputDirectory"    lsp-latex-build-output-directory)
   ("latex.build.forwardSearchAfter" lsp-latex-forward-search-after t)
   ("latex.forwardSearch.executable" lsp-latex-forward-search-executable)
   ("latex.forwardSearch.args"       lsp-latex--forward-search-args-getter)
   ("latex.lint.onChange"            lsp-latex-lint-on-change t)
   ("latex.lint.onSave"              lsp-latex-lint-on-save t)
   ("bibtex.formatting.lineLength"   lsp-latex-bibtex-formatting-line-length)
   ("bibtex.formatting.formatter"    lsp-latex-bibtex-formatting-formatter)))

(add-to-list 'lsp-language-id-configuration '(".*\\.tex$" . "latex"))
(add-to-list 'lsp-language-id-configuration '(".*\\.bib$" . "bibtex"))
(defun lsp-latex-new-connection ()
  "Create new connection of lsp-latex."
  (let (jar-file)
    (cond
     ((locate-file lsp-latex-texlab-executable exec-path)
      (cons lsp-latex-texlab-executable
            lsp-latex-texlab-executable-argument-list))
     ((setq jar-file (lsp-latex-get-texlab-jar-file))
      (append
       (cons
        lsp-latex-java-executable
        lsp-latex-java-argument-list)
       (cons
        jar-file
        lsp-latex-texlab-jar-argument-list)))
     (t
      (error "No executable \"texlab\" file")))))

;; Copied from `lsp-clients--rust-window-progress' in `lsp-rust'.
(defun lsp-latex-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  ;; Minimal implementation - we could show the progress as well.
  (lsp-log (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   #'lsp-latex-new-connection)
                  :major-modes '(tex-mode
                                 yatex-mode
                                 latex-mode
                                 bibtex-mode)
                  :server-id 'texlab2
                  :priority 2
                  :initialized-fn
                  (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "latex"))
                      (lsp--set-configuration
                       (lsp-configuration-section "bibtex"))))
                  :notification-handlers
                  (lsp-ht
                   ("window/progress"
                    'lsp-latex-window-progress))))



(defun lsp-latex--message-result-build (result)
  "Message RESULT means success or not."
  (message
   (cl-case (gethash "status" result)
     ((0)                             ;Success
      "Build was succeeded.")
     ((1)                             ;Error
      "Build do not succeeded.")
     ((2)                             ;Failure
      "Build failed.")
     ((3)                             ;Cancelled
      "Build cancelled."))))

(defun lsp-latex-build (&optional sync)
  "Build current tex file with latexmk, through texlab.
Build synchronously if SYNC is non-nil."
  (interactive "P")
  (if sync
      (lsp-latex--message-result-build
       (lsp-request
       "textDocument/build"
       (list :textDocument (lsp--text-document-identifier))))
    (lsp-request-async
     "textDocument/build"
     (list :textDocument (lsp--text-document-identifier))
     #'lsp-latex--message-result-build)))



;; To suppress warning.
(defvar pdf-sync-forward-display-action)
(declare-function pdf-info-synctex-forward-search "ext:pdf-info")
(declare-function pdf-sync-synctex-file-name "ext:pdf-sync")
(declare-function pdf-util-assert-pdf-window "ext:pdf-util")
(declare-function pdf-util-tooltip-arrow "ext:pdf-util")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-view-image-size "ext:pdf-view")

;;;###autoload
(defun lsp-latex-forward-search-with-pdf-tools (tex-file pdf-file line)
  "Forward search with pdf-tools, from TEX-FILE line LINE to PDF-FILE.
This function is partially copied from
`pdf-sync-forward-search' and `pdf-sync-forward-correlate'."
  (unless (fboundp 'pdf-tools-install)
    (error "Please install pdf-tools"))
  (require 'pdf-tools)
  (require 'pdf-sync)

  (with-current-buffer (get-file-buffer tex-file)
   (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
       (let* ((column 1)
              (pdf (expand-file-name (with-no-warnings pdf-file)))
              (sfilename (pdf-sync-synctex-file-name
                          (buffer-file-name) pdf)))
         (cons pdf
               (condition-case error
                   (let-alist (pdf-info-synctex-forward-search
                               (or sfilename
                                   (buffer-file-name))
                               line column pdf)
                     (cons .page .edges))
                 (error
                  (message "%s" (error-message-string error))
                  (list nil nil nil nil nil)))))
     (let ((buffer (or (find-buffer-visiting pdf)
                       (find-file-noselect pdf))))
       (with-selected-window (display-buffer
                              buffer pdf-sync-forward-display-action)
         (pdf-util-assert-pdf-window)
         (when page
           (pdf-view-goto-page page)
           (when y1
             (let ((top (* y1 (cdr (pdf-view-image-size)))))
               (pdf-util-tooltip-arrow (round top))))))
       (with-current-buffer buffer
         (run-hooks 'pdf-sync-forward-hook))))))

(defun lsp-latex--message-forward-search (result)
  "Message unless RESULT means success."
  (message
   (cl-case (plist-get result :status)
     ((1)                             ;Error
      "Forward search do not succeeded.")
     ((2)                             ;Failure
      "Forward search failed.")
     ((3)                             ;Unconfigured
      "Forward search has not been configured."))))

(defun lsp-latex-forward-search ()
  "Forward search on preview."
  (interactive)
  (lsp-request-async
   "textDocument/forwardSearch"
   (lsp--text-document-position-params)
   #'lsp-latex--message-forward-search))


(provide 'lsp-latex)
;;; lsp-latex.el ends here
