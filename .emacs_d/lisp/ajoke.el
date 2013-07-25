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

(defmacro ajoke--current-line ()
 `(buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defcustom ajoke--emacs-ctags-alist
  '(("emacs-lisp" . "lisp")
    ("sawfish" . "lisp")
    ("c" . "c++")
    ("objc" . "ObjectiveC")
    ("makefile-gmake" . "make")
    ("csharp" . "C#"))
  "Map from Emacs major modes to ctags languages")

(defcustom ajoke--emacs-filter-alist
  '(("c" . "| perl -ne '@f = split; print unless $f[1] =~ m/^member|macro$/'")
    ("php" . "| perl -ne '@f = split; print unless $f[1] =~ m/^variable$/'"))
  "Map from Emacs major modes to ctags output filter")

(defun set-gtags-start-file ()
  (let ((file (my-buffer-file-name)))
    (if (file-remote-p file)
        (let ((process-environment tramp-remote-process-environment))
          (setenv "GTAGS_START_FILE" (file-remote-p file 'localname))
          (setq tramp-remote-process-environment process-environment))
      (setenv "GTAGS_START_FILE" file))))

;;;###autoload
(defun java-get-hierarchy ()
  (interactive)
  (set-gtags-start-file)
  (let ((class-name (get-the-tag-around-me 'class-name-from-tag-line 0))
        (method-name
         (replace-regexp-in-string
          ".*\\." ""
          (or (and transient-mark-mode mark-active
                   (/= (point) (mark))
                   (buffer-substring-no-properties (point) (mark)))
              (get-the-tag-around-me 'tag-name-from-tag-line 0))))
        (compilation-buffer-name-function (lambda (_ign) "*java-get-hierarchy*")))
    (compile (format "java-get-hierarchy.pl %s %s"
                     class-name
                     (if current-prefix-arg
                         "-v"
                       (concat "-m " method-name))))))

;;;###autoload
(defun java-get-override ()
  (interactive)
  (set-gtags-start-file)
  (let (method)
    (save-excursion
      (let* ((class-name (get-the-tag-around-me 'class-name-from-tag-line 0))
             (hierarchy (shell-command-to-string (format "java-get-hierarchy.pl %s -v|grep '('|perl -npe 's/^\\s+//'|sort -u" class-name)))
             (methods (split-string hierarchy "\n")))
        (setq method (completing-read "Which method to override? " methods nil t))))
    (insert "@Override\n")
    (insert (replace-regexp-in-string  "\\(,\\|)\\)" "\\1 " method))))

;;;###autoload
(defun bhj-isearch-from-bod (&optional col-indent)
  (interactive "p")
  (with-syntax-table (let ((new-table (make-syntax-table (syntax-table))))
                       (modify-syntax-entry ?_ "w" new-table)
                       new-table)
    (let ((word (current-word)))
      (nodup-ring-insert cscope-marker-ring (point-marker))
      (bhj-c-beginning-of-defun)

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

;;;###autoload
(defun bhj-jdk-help (jdk-word)
  "start jdk help"
  (interactive
   (progn
     (let ((default (current-word)))
       (list (read-string "Search JDK help on: "
                          default
                          'jdk-help-history)))))

  ;; Setting process-setup-function makes exit-message-function work
  (call-process "/bin/bash" nil nil nil "jdkhelp.sh" jdk-word)
  (w3m-goto-url "file:///d/knowledge/jdk-6u18-docs/1.html"))

;;;###autoload
(defun cscope-pop-mark ()
  "Pop back to where cscope was last invoked."
  (interactive)

  ;; This function is based on pop-tag-mark, which can be found in
  ;; lisp/progmodes/etags.el.

  (if (ring-empty-p cscope-marker-ring)
      (error "There are no marked buffers in the cscope-marker-ring yet"))
  (let* ( (marker (ring-remove cscope-marker-ring 0))
          (old-buffer (current-buffer))
          (marker-buffer (marker-buffer marker))
          marker-window
          (marker-point (marker-position marker))
          (cscope-buffer (get-buffer cscope-output-buffer-name)) )
    (when (and (not (ring-empty-p cscope-marker-ring))
               (equal marker (ring-ref cscope-marker-ring 0)))
      (ring-remove cscope-marker-ring 0))
    (nodup-ring-insert cscope-marker-ring-poped (point-marker))

    ;; After the following both cscope-marker-ring and cscope-marker will be
    ;; in the state they were immediately after the last search.  This way if
    ;; the user now makes a selection in the previously generated *cscope*
    ;; buffer things will behave the same way as if that selection had been
    ;; made immediately after the last search.
    (setq cscope-marker marker)

    (if marker-buffer
        (if (eq old-buffer cscope-buffer)
            (progn ;; In the *cscope* buffer.
              (set-buffer marker-buffer)
              (setq marker-window (display-buffer marker-buffer))
              (set-window-point marker-window marker-point)
              (select-window marker-window))
          (switch-to-buffer marker-buffer))
      (error "The marked buffer has been deleted"))
    (goto-char marker-point)
    (set-buffer old-buffer)))

;;;###autoload
(defun cscope-pop-mark-back ()
  "Pop back to where cscope was last invoked."
  (interactive)

  ;; This function is based on pop-tag-mark, which can be found in
  ;; lisp/progmodes/etags.el.

  (if (ring-empty-p cscope-marker-ring-poped)
      (error "There are no marked buffers in the cscope-marker-ring-poped yet"))
  (let* ( (marker (ring-remove cscope-marker-ring-poped 0))
          (old-buffer (current-buffer))
          (marker-buffer (marker-buffer marker))
          marker-window
          (marker-point (marker-position marker))
          (cscope-buffer (get-buffer cscope-output-buffer-name)) )
    (nodup-ring-insert cscope-marker-ring (point-marker))

    ;; After the following both cscope-marker-ring-poped and cscope-marker will be
    ;; in the state they were immediately after the last search.  This way if
    ;; the user now makes a selection in the previously generated *cscope*
    ;; buffer things will behave the same way as if that selection had been
    ;; made immediately after the last search.
    (setq cscope-marker marker)

    (if marker-buffer
        (if (eq old-buffer cscope-buffer)
            (progn ;; In the *cscope* buffer.
              (set-buffer marker-buffer)
              (setq marker-window (display-buffer marker-buffer))
              (set-window-point marker-window marker-point)
              (select-window marker-window))
          (switch-to-buffer marker-buffer))
      (error "The marked buffer has been deleted"))
    (goto-char marker-point)
    (set-buffer old-buffer)))

;;;###autoload
(defun tag-this-file (&optional output-buf)
  (interactive)
  (save-excursion
    (save-window-excursion
      (save-restriction
        (widen)
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
         output-buf)))))

(defun code-line-number-from-tag-line (line)
  (goto-line line)
  (let ((subs (split-string (ajoke--current-line))))
    (string-to-number
     (if (string-equal (car subs) "operator")
         (cadddr subs) ;operator +=      function    183 /home...
       (caddr subs))))) ;region_iterator  struct      189 /home...

(defun code-def-from-tag-line (line)
  (goto-line line)
  (let ((subs (split-string (ajoke--current-line))))
    (car subs)))

(defun code-indentation-from-tag-line (line)
  (let ((code-line (code-line-number-from-tag-line line)))
    (with-current-buffer old-buffer
      (save-excursion
        (goto-line code-line)
        (back-to-indentation)
        (current-column)))))

;;;###autoload
(defun ctags-get-fully-qualified-name ()
  (interactive)
  (save-excursion
    (let (deactivate-mark) ;;see the help of save-excursion
      (tag-this-file (get-buffer-create "*ctags-beginning-of-defun*"))
      (let ((old-buffer (current-buffer))
            (old-code-line (line-number-at-pos))
            (last-code-line (line-number-at-pos (buffer-end 1)))
            (last-def-line 1)
            (fully-qualified-name "")
            current-code-line-indent)
        (with-current-buffer "*ctags-beginning-of-defun*"
          (goto-char (point-max))
          (insert ( concat "hello function "
                           (number-to-string last-code-line)
                           "hello world"))
          (while (< old-code-line (code-line-number-from-tag-line (line-number-at-pos)))
            (previous-line))
          (setq fully-qualified-name (code-def-from-tag-line (line-number-at-pos))
                current-code-line-indent (code-indentation-from-tag-line (line-number-at-pos)))
          (while (and (> current-code-line-indent 0)
                      (> (line-number-at-pos) 1))
            (previous-line)
            (if (< (code-indentation-from-tag-line (line-number-at-pos)) current-code-line-indent)
                (setq fully-qualified-name (concat (code-def-from-tag-line (line-number-at-pos))
                                                   "."
                                                   fully-qualified-name)
                      current-code-line-indent (code-indentation-from-tag-line (line-number-at-pos)))))
          (message "%s" fully-qualified-name)
          (with-temp-buffer
            (insert-string fully-qualified-name)
            (copy-region-as-kill (point-min) (point-max))))))))

;;;###autoload
(defun ctags-beginning-of-defun (&optional arg)
  (interactive "^p")
  (goto-line
   (get-the-tag-around-me 'code-line-number-from-tag-line arg)))

(defun tag-name-from-tag-line (line)
  (goto-line line)
  (car (split-string (ajoke--current-line))))

(defun completing-read-one? (prompt collection &rest args)
  (if (= (length (delete-dups collection)) 1)
      (car collection)
    (apply 'completing-read prompt collection args)))

(defun class-name-from-tag-line (line)
  (goto-line line)
  (let ((limit (line-end-position))
        classes)
    (goto-char (point-min))
    (while (search-forward-regexp "class\\|interface" limit t)
      (let* ((line (ajoke--current-line))
             (fields (split-string line))
             (name (car fields))
             (type (cadr fields)))
        (cond
         ((or (string-equal type "class")
              (string-equal type "interface"))
          (setq classes (cons line classes))))))
    (car (split-string (completing-read-one? "Which class/interface to hierarchy? " (delete-dups (nreverse classes)) nil t)))))

;;;###autoload
(defun get-the-tag-around-me (get-attr-func &optional arg)
  (interactive)
  "GET-ATTR-FUNC is a function to specify what attribute of the tag to return,
for e.g., the line number the tag is on, or the name of the tag.

ARG means found the (ARG - 1)th tag to find."
  (save-excursion
     (let (deactivate-mark) ;;see the help of save-excursion
       (tag-this-file (get-buffer-create "*ctags-beginning-of-defun*"))
       (let ((old-buffer (current-buffer))
             (old-code-line (line-number-at-pos))
             (last-code-line (line-number-at-pos (buffer-end 1)))
             (last-def-line 1))
         (with-current-buffer "*ctags-beginning-of-defun*"
           (goto-char (point-max))
           (insert (concat "hello function "
                           (number-to-string last-code-line)
                           " hello world"))
           (let* ((min 1)
                  (max (line-number-at-pos (buffer-end 1)))
                  (mid (/ (+ min max) 2))
                  (mid-code-line (code-line-number-from-tag-line mid))
                  (mid+1-codeline (code-line-number-from-tag-line (1+ mid))))
             (while (and
                     (not (and
                           (< mid-code-line old-code-line)
                           (>= mid+1-codeline old-code-line)))
                     (< min max))
               (if (>= mid-code-line old-code-line)
                   (setq max (1- mid))
                 (setq min (1+ mid)))
               (setq mid (/ (+ min max) 2)
                     mid-code-line (code-line-number-from-tag-line mid)
                     mid+1-codeline (code-line-number-from-tag-line (1+ mid))))
             (funcall get-attr-func (- mid -1
                                       (if (and (numberp arg)
                                                (= arg 0)
                                                (not (= (code-line-number-from-tag-line (1+ mid)) old-code-line)))
                                           1
                                         (or arg 1))))))))))

(defun my-buffer-file-name (&optional buf)
  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (if (eq major-mode 'dired-mode)
        (replace-regexp-in-string "/*$" "" default-directory)
      (or (buffer-file-name buf)
          ""))))

(defun my-buffer-file-name-local (&optional buf)
  (let ((name (my-buffer-file-name buf)))
    (or (file-remote-p name 'localname)
        name)))

;;;###autoload
(defun bhj-choose (args)
  (interactive)
  (insert
   (completing-read "Please enter which one to use: " args)))

;;;###autoload
(defun bhj-choose-from-output ()
  (interactive)
  (bhj-choose (split-string (shell-command-to-string (read-from-minibuffer "command to run: ")) nil t)))

;;;###autoload
(defun replace-double-quotes ()
  (interactive)
  (query-replace "``" "“" nil (point-min) (point-max))
  (query-replace "''" "”" nil (point-min) (point-max)))

(defun imenu-create-index-using-ctags ()
  "Create the index like the imenu-default-create-index-function,
using ctags-exuberant"

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
(defun java-get-imports ()
  (interactive)
  (save-excursion
    (let ((old-buffer (current-buffer))
          import-list)
      (with-temp-buffer
        (shell-command (format "java-get-imports.pl %s -v" (buffer-file-name old-buffer)) (current-buffer))
        (goto-char (point-min))
        (while (search-forward-regexp "^import" nil t)
          (save-excursion
            (if (looking-at "-multi")

                (setq import-list (cons
                                   (format "import %s;\n"
                                           (completing-read-one? "Import which? "
                                                                 (cdr
                                                                  (delete-if-empty
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
(defun java-resolve (id)
  (interactive
   (list (or (and transient-mark-mode mark-active
                  (/= (point) (mark))
                  (buffer-substring-no-properties (point) (mark)))
             (current-regexp "[.a-z0-9]+"))))
  (shell-command (format "java-get-imports.pl %s -r %s"
                         (shell-quote-argument (my-buffer-file-name-local))
                         (shell-quote-argument id))))

;;;###autoload
(defun java-complete-method (id)
  (interactive
   (list (or (and transient-mark-mode mark-active
                  (/= (point) (mark))
                  (buffer-substring-no-properties (point) (mark)))
             (current-regexp "[.a-z0-9_]+"))))

  (let (method (remove ""))
    (save-excursion
      (let* ((resolve (shell-command-to-string (format "java-get-imports.pl %s -r %s|tr -d '\\n'"
                                                       (shell-quote-argument (my-buffer-file-name-local))
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
             (hierarchy (shell-command-to-string (format "java-get-hierarchy.pl %s -v|grep '('|perl -npe 's/^\\s+//'|sort -u" class)))
             (methods (split-string hierarchy "\n")))
        (setq method (completing-read "Which method to call? " methods nil t))))
    (goto-char (current-regexp "[.a-z0-9_]+" (lambda (start end) end)))
    (when (not (string-equal remove ""))
      (delete-region (- (point) (length remove)) (point)))
    (insert ".")
    (insert (replace-regexp-in-string ".*\\s \\(\\S *(.*)\\).*" "\\1" method))))

(defun delete-if-empty (l)
  (delete-if
   (lambda (s) (string-equal s ""))
   l))

(defun current-regexp (re &optional func)
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
