;;; ajoke.el --- Ambitious Java On Emacs, K is silent.

;; Copyright (C) 2013 Bao Haojun

;; Author: Bao Haojun <baohaojun@gmail.com>
;; Maintainer: Bao Haojun <baohaojun@gmail.com>
;; Created: 2013-07-25
;; Keywords: java
;; Version: 0.0.20130725
;; URL: https://github.com/baohaojun/ajoke

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For more information see the readme at:
;; https://github.com/baohaojun/ajoke

;;; Code:

(require 'cl)

(defsubst ajoke--current-line (&optional to-here)
  (buffer-substring-no-properties (point-at-bol) (if to-here (point) (point-at-eol))))

(defcustom ajoke--emacs-ctags-alist
  '(("emacs-lisp" . "lisp")
    ("sawfish" . "lisp")
    ("js" . "javascript")
    ("c" . "c++")
    ("objc" . "ObjectiveC")
    ("makefile-gmake" . "make")
    ("csharp" . "C#"))
  "Map from Emacs major modes to ctags languages")

(defcustom ajoke--emacs-filter-alist
  '(("c" . "| perl -ne '@f = split; print unless $f[1] =~ m/^member|macro$/'")
    ("php" . "| perl -ne '@f = split; print unless $f[1] =~ m/^variable$/'"))
  "Map from Emacs major modes to ctags output filter")

(defvar ajoke--marker-ring (make-ring 32)
  "Ring of markers which are locations from which ajoke was invoked.")

(defvar ajoke--marker-ring-poped (make-ring 32)
  "Ring of markers which are locations poped from ajoke--marker-ring.")

(defvar ajoke--last-tagged-buffer nil
  "The last buffer tagged, use for optimization.")

(defvar ajoke--last-tagged-tick 0
  "The modification tick of the last tagged buffer, for optimization.")

(defvar ajoke--tagged-lines nil
  "A vector of the tagged lines for the current buffer.

Each element of it is the line number on the source code buffer,
where a tag is defined.")

(defun ajoke--buffer-file-name (&optional buf)
  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (if (eq major-mode 'dired-mode)
        (directory-file-name default-directory)
      (or (buffer-file-name buf) ""))))

(defun ajoke--buffer-file-name-local (&optional buf)
  (let ((name (ajoke--buffer-file-name buf)))
    (or (file-remote-p name 'localname)
        name)))

(defun ajoke--delete-empty-strings (l)
  (delete-if
   (lambda (s) (string-equal s ""))
   l))

(defun ajoke--setup-env ()
  "Set environment variable for the current file.

So that the scripts know which source code file you're editing,
and treat it specialy, because in most cases the gtags database
record about this file is outdated since you're editing it right
now, and thus need rebuild tags for this file."
  (let ((file (ajoke--buffer-file-name)))
    (if (file-remote-p file)
        (let ((process-environment tramp-remote-process-environment))
          (setenv "GTAGS_START_FILE" (file-remote-p file 'localname))
          (setq tramp-remote-process-environment process-environment))
      (setenv "GTAGS_START_FILE" file))))

(defun ajoke--tag-current-buffer (output-buf)
  "Tag the current buffer using ctags."
  (interactive)
  (let ((current-buffer (current-buffer))
        (current-buffer-tick (buffer-chars-modified-tick))
        last-code-line)
    (unless (and
             (eq current-buffer ajoke--last-tagged-buffer)
             (= current-buffer-tick ajoke--last-tagged-tick))
      ;; tag is out-dated, retag
      (message "Ajoke: re-tag the buffer")
      (let (deactivate-mark) ;;see the help of save-excursion
        (save-excursion
          (save-window-excursion
            (save-restriction
              (widen)
              (setq last-code-line (line-number-at-pos (buffer-end 1)))
              (shell-command-on-region
               (point-min)
               (point-max)
               (let ((mode-name-minus-mode
                      (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
                 (concat "ctags-stdin --extra=+q --language-force="
                         (shell-quote-argument
                          (or (cdr (assoc mode-name-minus-mode ajoke--emacs-ctags-alist))
                              mode-name-minus-mode))
                         " -xu "
                         (cdr (assoc mode-name-minus-mode ajoke--emacs-filter-alist))))
               output-buf))))
        (with-current-buffer output-buf
          (goto-char (point-max))
          (insert (concat "hello function "
                          (number-to-string last-code-line)
                          " hello world"))
          (let* ((number-of-tags (line-number-at-pos))
                 (it 1)
                 (vec (make-vector number-of-tags 0)))
            (setq ajoke--tagged-lines nil)
            (while (<= it number-of-tags)
              (aset vec (1- it) (list (ajoke--extract-line-number it)
                                      (ajoke--extract-tag it)))
              (setq it (1+ it)))
            (setq ajoke--tagged-lines vec))))
      (setq ajoke--last-tagged-buffer current-buffer
            ajoke--last-tagged-tick current-buffer-tick))))

(defun ajoke--extract-line-number (nth-tag-line)
  "Extract line number for `ajoke--thing-at-tag'."
  (if ajoke--tagged-lines
      (nth 0 (aref ajoke--tagged-lines (max 0 (1- nth-tag-line))))
    (goto-line nth-tag-line)
    (let ((subs (split-string (ajoke--current-line))))
      (string-to-number
       (if (string-equal (car subs) "operator")
           (cadddr subs) ;operator +=      function    183 /home...
         (caddr subs)))))) ;region_iterator  struct      189 /home...

(defun ajoke--extract-tag (nth-tag-line)
  "Extract tag for `ajoke--thing-at-tag'."
  (if ajoke--tagged-lines
      (nth 1 (aref ajoke--tagged-lines (1- nth-tag-line)))
    (goto-line nth-tag-line)
    (car (split-string (ajoke--current-line)))))

(defun ajoke--extract-class (nth-tag-line)
  "Extract classes for `ajoke--thing-at-tag'.

If there are more than one classes/interfaces before
NTH-TAG-LINE, ask user to pick."
  (with-current-buffer "*ajoke--tags*"
    (goto-line nth-tag-line)
    (let ((limit (line-end-position))
          classes)
      (goto-char (point-min))
      (while (search-forward-regexp "class\\|interface" limit t)
        (let* ((tagstr (ajoke--current-line))
               (fields (split-string tagstr))
               (name (car fields))
               (type (cadr fields)))
          (cond
           ((or (string-equal type "class")
                (string-equal type "interface"))
            (setq classes (cons tagstr classes))))))
      (car (split-string (ajoke--pick-one "Which class/interface? " (delete-dups (nreverse classes)) nil t))))))

;;;###autoload
(defun ajoke--pick-one (prompt collection &rest args)
  "Pick an item from COLLECTION, which is a list.
ARGS is passed to the supporting function completing-read (or
HELM's or Anything's version of completing-read: you are strongly
advised to use one of these elisp tools)."
  (if (= (length (delete-dups collection)) 1)
      (car collection)
    (apply 'completing-read prompt collection args)))

(defun ajoke--thing-at-tag (thing-func nth-tag-cur)
  "Like `thing-at-point', this function finds something for the current tag.

THING-FUNC is a function to specify which thing of the tag to
extract, for e.g., the line number the tag is on, or the name of
the tag.

NTH-TAG-CUR means the NTH-TAG-CUR'th tag around the current code
line. If it is positive, it means the NTH-TAG-CUR-th tag whose
code line is smaller than the current code line. If it is
negative, it means larger. If it is 0, it means equal or
smaller. In most cases NTH-TAG-CUR should be 0, because we are
most interested in the current tag."
  (interactive)
  (ajoke--tag-current-buffer (get-buffer-create "*ajoke--tags*"))
  (let ((old-code-line (line-number-at-pos))
        (last-def-line 1))
    (let* ((min 1)
           (max (length ajoke--tagged-lines))
           (mid (/ (+ min max) 2))
           (mid-code-line (ajoke--extract-line-number mid))
           (mid+1-codeline (ajoke--extract-line-number (1+ mid))))
      (while (and
              (not (and
                    (< mid-code-line old-code-line)
                    (>= mid+1-codeline old-code-line)))
              (< min max))
        (if (>= mid-code-line old-code-line)
            (setq max (1- mid))
          (setq min (1+ mid)))
        (setq mid (/ (+ min max) 2)
              mid-code-line (ajoke--extract-line-number mid)
              mid+1-codeline (ajoke--extract-line-number (1+ mid))))
      (funcall thing-func
               (cond
                ((= 0 nth-tag-cur)
                 (if (= mid+1-codeline old-code-line)
                     (1+ mid)
                   mid))
                ((< nth-tag-cur 0)
                 (if (= mid+1-codeline old-code-line)
                     (+ mid 1 (- nth-tag-cur))
                   (+ mid (- nth-tag-cur))))
                (t ; (> nth-tag-cur 0)
                 (+ mid -1 nth-tag-cur)))))))

(defun ajoke--current-regexp (re &optional func)
  "Look for regular expression RE around the current point.

When matched, return the matched string by default. But if FUNC
is set, call FUNC with the start and end of the matched region."
  (save-excursion
    (let (start end)
      (while (not (looking-at re))
        (backward-char))
      (while (looking-at re)
        (backward-char))
      (forward-char)
      (setq start (point))
      (search-forward-regexp re)
      (setq end (point))
      (funcall (or func 'buffer-substring-no-properties) start end))))

(defun ajoke--beginning-of-defun-function (&optional arg)
  "Ajoke's version of `beginning-of-defun-function'."
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let ((target-line
         (ajoke--thing-at-tag
          'ajoke--extract-line-number
          (if (and (not (bolp)) (> arg 0))
              (1- arg)
            arg))))
  (goto-line target-line)))

(defun ajoke--create-index-function ()
  "Ajoke's version of `imenu-default-create-index-function'."
  (let ((source-buffer (current-buffer))
        (temp-buffer (get-buffer-create "* imenu-ctags *"))
        result-alist)
    (save-excursion
      (save-restriction
        (widen)
        (save-window-excursion
          (shell-command-on-region
           (point-min)
           (point-max)
           (concat "imenu-ctags "
                   (file-name-nondirectory (buffer-file-name source-buffer)))
           temp-buffer))
        (with-current-buffer temp-buffer
          (goto-char (point-min))
          (while (search-forward-regexp "^\\([0-9]+\\) : \\(.*\\)" nil t)
            (setq result-alist
                  (cons (cons (match-string 2)
                              (let ((marker (make-marker)))
                                (set-marker marker (string-to-number (match-string 1)) source-buffer)))
                        result-alist))))))
    (nreverse result-alist)))

;;;###autoload
(defun ajoke-get-imports ()
  "Write the java import statements automatically."
  (interactive)
  (save-excursion
    (let ((old-buffer (current-buffer))
          import-list)
      (with-temp-buffer
        (shell-command (format "ajoke-get-imports.pl %s -v" (buffer-file-name old-buffer)) (current-buffer))
        (goto-char (point-min))
        (while (search-forward-regexp "^import" nil t)
          (save-excursion
            (if (looking-at "-multi")
                (setq
                 import-list
                 (cons
                  (format
                   "import %s;\n"
                   (ajoke--pick-one
                    "Import which? "
                    (cdr
                     (ajoke--delete-empty-strings
                      (split-string (ajoke--current-line) "\\s +")))
                    nil
                    t))
                  import-list))
              (setq import-list (cons (format "%s;\n" (ajoke--current-line)) import-list))))
          (forward-line)
          (beginning-of-line)))
      (goto-char (point-max))
      (or (search-backward-regexp "^import\\s +" nil t)
          (search-backward-regexp "^package\\s +" nil t))
      (forward-line)
      (beginning-of-line)
      (while import-list
        (insert (car import-list))
        (setq import-list (cdr import-list)))
      (let ((end-imports (point))
            (start-imports
             (save-excursion
               (previous-line)
               (beginning-of-line)
               (while (looking-at "^import\\s +")
                 (previous-line)
                 (beginning-of-line))
               (next-line)
               (beginning-of-line)
               (point))))
        (shell-command-on-region start-imports end-imports "sort -u" nil t)))))

;;;###autoload
(defun ajoke-get-hierarchy ()
  "Print the class/interface inheritance hierarchy for the
current class. Output is in compilation-mode for ease of cross
referencing."
  (interactive)
  (ajoke--setup-env)
  (let ((class-name (ajoke--thing-at-tag 'ajoke--extract-class 0))
        (method-name
         (replace-regexp-in-string
          ".*\\." ""
          (or (and transient-mark-mode mark-active
                   (/= (point) (mark))
                   (buffer-substring-no-properties (point) (mark)))
              (ajoke--thing-at-tag 'ajoke--extract-tag 0))))
        (compilation-buffer-name-function (lambda (_ign) "*ajoke-get-hierarchy*")))
    (compile (format "ajoke-get-hierarchy.pl %s %s"
                     class-name
                     (if current-prefix-arg
                         "-v"
                       (concat "-m " method-name))))))

;;;###autoload
(defun ajoke-get-override ()
  "Overide a method defined in super classes/interfaces."
  (interactive)
  (ajoke--setup-env)
  (let (method)
    (save-excursion
      (let* ((class-name (ajoke--thing-at-tag 'ajoke--extract-class 0))
             (hierarchy (shell-command-to-string (format "ajoke-get-hierarchy.pl %s -v|grep '('|perl -npe 's/^\\s+//'|sort -u" class-name)))
             (methods (split-string hierarchy "\n")))
        (setq method (completing-read "Which method to override? " methods nil t))))
    (insert "@Override\n")
    (insert (replace-regexp-in-string  "\\(,\\|)\\)" "\\1 " method))))

;;;###autoload
(defun ajoke-resolve (id)
  "Resolve the type (class/interface) of ID."
  (interactive
   (list (or (and transient-mark-mode mark-active
                  (/= (point) (mark))
                  (buffer-substring-no-properties (point) (mark)))
             (ajoke--current-regexp "[.a-z0-9]+"))))
  (shell-command (format "ajoke-get-imports.pl %s -r %s"
                         (shell-quote-argument (ajoke--buffer-file-name-local))
                         (shell-quote-argument id))))

;;;###autoload
(defun ajoke-complete-method (id)
  "Complete a method given an ID. First will resolve the
type (class/interface) of ID, then complete using the type's
methods."
  (interactive
   (list (or (and transient-mark-mode mark-active
                  (/= (point) (mark))
                  (buffer-substring-no-properties (point) (mark)))
             (ajoke--current-regexp "[.a-z0-9_]+"))))
  (let (method (remove ""))
    (save-excursion
      (let* ((resolve (shell-command-to-string (format "ajoke-get-imports.pl %s -r %s|tr -d '\\n'"
                                                       (shell-quote-argument (ajoke--buffer-file-name-local))
                                                       (shell-quote-argument id))))
             (comp (split-string resolve "\\."))
             (comp-last (car (last comp)))
             (class (cond
                     ((string= comp-last "")
                      (setq remove ".")
                      (mapconcat 'identity (butlast comp) "."))
                     ((let ((case-fold-search nil))
                              (string-match "^[a-z]" comp-last))
                      (setq remove (concat "." comp-last))
                      (mapconcat 'identity (butlast comp) "."))
                     (t resolve)))
             (hierarchy (shell-command-to-string (format "ajoke-get-hierarchy.pl %s -v|grep '('|perl -npe 's/^\\s+//'|sort -u" class)))
             (methods (split-string hierarchy "\n")))
        (setq method (completing-read "Which method to call? " methods nil t))))
    (goto-char (ajoke--current-regexp "[.a-z0-9_]+" (lambda (start end) end)))
    (when (not (string-equal remove ""))
      (delete-region (- (point) (length remove)) (point)))
    (insert ".")
    (insert (replace-regexp-in-string ".*\\s \\(\\S *(.*)\\).*" "\\1" method))))

;;;###autoload
(defun ajoke-search-local-id ()
  "Search an identifier such as a local variable from the
beginning of current defun."
  (interactive)
  (with-syntax-table (let ((new-table (make-syntax-table (syntax-table))))
                       (modify-syntax-entry ?_ "w" new-table)
                       new-table)
    (let ((word (current-word)))
      (nodup-ring-insert ajoke--marker-ring (point-marker))
      (ajoke--beginning-of-defun-function)
      (unless (string-equal (car regexp-search-ring) (concat "\\b" word "\\b"))
        (add-to-history
         'regexp-search-ring
         (concat "\\b" word "\\b")
         regexp-search-ring-max))
      (let ((not-match t))
        (while not-match
          (search-forward-regexp (concat "\\b" word "\\b"))
          (when (string-equal word (current-word))
            (setq not-match nil)))))))

(setq-default imenu-create-index-function #'ajoke--create-index-function)
; (setq-default beginning-of-defun-function #'ajoke--beginning-of-defun-function)
(defadvice beginning-of-defun (around ajoke--beginning-of-defun first activate)
  (if (eq this-command 'beginning-of-defun)
    (let ((beginning-of-defun-function #'ajoke--beginning-of-defun-function))
      ad-do-it)
    ad-do-it))
(defadvice end-of-defun (around ajoke--end-of-defun first activate)
  (if (eq this-command 'end-of-defun)
      (let ((beginning-of-defun-function #'ajoke--beginning-of-defun-function))
        ad-do-it)
    ad-do-it))

(global-set-key [(meta g)(j)(i)] 'ajoke-get-imports)
(global-set-key [(meta g)(j)(h)] 'ajoke-get-hierarchy)
(global-set-key [(meta g)(j)(o)] 'ajoke-get-override)
(global-set-key [(meta g)(j)(r)] 'ajoke-resolve)
(global-set-key [(meta g)(j)(m)] 'ajoke-complete-method)
(global-set-key [(shift meta s)] 'ajoke-search-local-id)

(provide 'ajoke)
