;;; nim-suggest.el --- a plugin to use nimsuggest from Emacs -*- lexical-binding: t -*-

;; Description: A minor mode for Nim language tooling using nimsuggest
;; Author: Simon Hafner
;; Maintainer: Yuta Yamada <cokesboy"at"gmail.com>

;; Package-Requires: ((emacs "24.4") (epc "0.1.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; -- document is still work in progress --
;; Supporting features:
;; - Xref package (cross reference package for Emacs) -- from Emacs 25.1
;; - flycheck/flymake (on the fly linter)
;;   - flycheck-nimsugget is other repo
;;   - flymake-nimsuggest is only available if you use Emacs 26 or higher
;; - eldoc (help on the hover)
;; - company-mode (auto-completion)

;; TODO: write manual configuration, but basically you can find
;; `nimsuggest-mode-hook' what this package does though.

;;; Code:

(require 'nim-mode)
(require 'epc)
(require 'cl-lib)

(defconst nim--epc-keywords
  ;; Those names come from suggest.nim
  '(:section ; sug, con, def, use, dus, highlight, outline
    :symkind ; symKind
    :qpath   ; qualifiedPath
    :file    ; filePath
    :forth   ; type
    :line    ; min is 1 in suggest.nim
    :column  ; min is 0 in suggest.nim
    :doc     ; document
    :quality ; rank of completion
    :prefix) ; matching state. See also prefixmatches.nim
  "Keywords for SexpNode type on nimsuggest.nim.")

(defvar-local nimsuggest--state 'not-started)

(cl-defstruct nim--epc
  section symkind qpath file forth line column doc quality prefix)

(defun nimsuggest--parse-epc (epc-result method)
  "Parse EPC-RESULT according to METHOD."
  (cl-case method
    ((chk highlight outline) epc-result)
    ((sug con def use dus)
     (cl-mapcar
      (lambda (sublist)
        (apply #'make-nim--epc
               (cl-mapcan #'list nim--epc-keywords sublist)))
      epc-result))))

(defvar nimsuggest--epc-processes-alist nil)

(defvar nimsuggest-get-option-function nil
  "Function to get options for nimsuggest.")

;; TODO: Is there something needed in this function?
;; https://irclogs.nim-lang.org/12-07-2017.html
(defun nimsuggest-get-options (project-path)
  "Get prerequisite options for EPC mode.

PROJECT-PATH is added as the last option."
  (delq nil
        (append nimsuggest-options nimsuggest-local-options
                ;; FIXME:
                ;; In recent nim’s update, this configuration no
                ;; longer can use.
                ;; (when (eq 'nimscript-mode major-mode)
                ;;   '("--define:nimscript" "--define:nimconfig"))
                (list "--epc" project-path))))

(defun nimsuggest--get-epc-process (file)
  "Get active epc process instance for FILE."
  (let ((old-epc (cdr (assoc file nimsuggest--epc-processes-alist))))
    (if (eq 'run (epc:manager-status-server-process old-epc))
        (prog1 old-epc
          (nim-log "nimsuggest: use old EPC process\n - %s" old-epc))
      (prog1 () (nimsuggest--kill-zombie-processes file)))))

;;;###autoload
(defun nimsuggest-available-p ()
  "Return non-nil if nimsuggest is available in current buffer."
  (and nimsuggest-path
       (not nim--inside-compiler-dir-p)
       ;; Prevent turn on nimsuggest related feature on org-src block
       ;; and nimscript-mode (nimsuggest doesn't support yet).
       ;; https://github.com/nim-lang/nimsuggest/issues/29
       (not (memq major-mode '(org-mode nimscript-mode)))
       (not (and (fboundp 'org-in-src-block-p)
                 (or (org-in-src-block-p)
                     (org-in-src-block-p t))))))

(define-obsolete-function-alias 'nim-suggest-available-p 'nimsuggest-available-p "2017/9/02")

(defun nimsuggest--safe-execute (file func)
  "Execute FUNC only if FILE buffer exists."
  (save-current-buffer
    (let ((buf (get-file-buffer file)))
      (when buf
        (unless (eq buf (current-buffer)) (set-buffer buf))
        (funcall func)))))

(defun nimsuggest--set-state (state file)
  "Set STATE for FILE's buffer."
  (nimsuggest--safe-execute
   file (lambda () (setq-local nimsuggest--state state))))

(defun nimsuggest--start-epc-deferred (file method callback)
  "Start EPC process for FILE."
  (deferred:nextc (nimsuggest--start-server-deferred nimsuggest-path file)
    (lambda (mngr)
      (push (cons file mngr) nimsuggest--epc-processes-alist)
      (nimsuggest--set-state 'ready file)
      (when (eq method 'chk)
        (nimsuggest--query method callback (nimsuggest--get-epc-process file))
        ;; Reset `nimsuggest--state' if all epc processes for the file are dead.
        ;; Not sure if this is related to --refresh option.
        (catch 'exit
          (cl-loop for (f . _) in nimsuggest--epc-processes-alist
                   if (equal file f)
                   do (throw 'exit t)
                   finally (nimsuggest--set-state 'not-started file)))))))

(defun nimsuggest--start-server-deferred (server-prog file)
  "Copied from `epc:start-server-deferred' because original function uses `lexicall-let'.
It unable to use from this nim-suggest.el due to some error. (void-variable self or something)
Almost structure is same, but below two values should be changed depending on
nimsuggest's loading time:
  - `nimsuggest-accept-process-delay'
  - `nimsuggest-accept-process-timeout-count'"
  ;; TODO: let EPC author knows the issue
  (let* ((server-args (nimsuggest-get-options file))
         (uid (epc:uid))
         (process-name (epc:server-process-name uid))
         (process-buffer (get-buffer-create (epc:server-buffer-name uid)))
         (process (apply 'start-process
                         process-name process-buffer
                         server-prog server-args))
         (mngr (make-epc:manager
                :server-process process
                :commands (cons server-prog server-args)
                :title (mapconcat 'identity (cons server-prog server-args) " ")))
         (cont 1) port)
    (set-process-query-on-exit-flag process nil)
    (deferred:$
      (deferred:next
        ;; self recursion during `deferred:lambda' til it gets port
        ;; number or emits timeout error.
        (deferred:lambda (_)
          (accept-process-output process 0 nil t)
          (let ((port-str (with-current-buffer process-buffer
                            (buffer-string))))
            (cond
             ((string-match "^[0-9]+$" port-str)
              (setq port (string-to-number port-str)
                    cont nil))
             ((< 0 (length port-str))
              (error "Server may raise an error \
Use \"M-x epc:pop-to-last-server-process-buffer RET\" \
to see full traceback:\n%s" port-str))
             ((not (eq 'run (process-status process)))
              (setq cont nil))
             (t
              (incf cont)
              (when (< nimsuggest-accept-process-timeout-count cont)
                (nimsuggest--set-state 'no-response file)
                ;; timeout 15 seconds (100 * 150)
                (error "Timeout server response"))
              (deferred:nextc (deferred:wait nimsuggest-accept-process-delay)
                self))))))
      (deferred:nextc it
        (lambda (_)
          (setf (epc:manager-port mngr) port)
          (setf (epc:manager-connection mngr) (epc:connect "localhost" port))
          mngr))
      (deferred:nextc it
        (lambda (mngr) (epc:init-epc-layer mngr))))))

(defun nimsuggest--query (method callback epc-process)
  "Query to nimsuggest of EPC-PROCESS with METHOD.
CALLBACK function will be applied when nimsuggest returns the result."
  (when (and (nimsuggest-available-p) epc-process)
    ;; See also compiler/modulegraphs.nim for dirty file
    (let ((temp-dirty-file (nimsuggest--save-buffer-temporarly))
          (buf (current-buffer)))
      (deferred:$
        (epc:call-deferred
         epc-process
         (prog1 method
           (nim-log "EPC-1 %S" (symbol-name method)))
         (cl-case method
           ((chk highlight outline)
            (list (buffer-file-name)
                  -1 -1
                  temp-dirty-file))
           (t
            (list (buffer-file-name)
                  (line-number-at-pos)
                  (current-column)
                  temp-dirty-file))))
        (deferred:nextc it
          (lambda (x)
            (nim-log "EPC(%S) nextc" (symbol-name method))
            (when x (funcall callback (nimsuggest--parse-epc x method)))))
        (deferred:watch it
          (lambda (_)
            (unless (get-buffer buf)
              (nim-log "EPC(%S) delete %s" (symbol-name method) buf)
              (delete-file temp-dirty-file))))
        (deferred:error it
          (lambda (err)
            ;; Note that it seems like this error clause is not *rare* to
            ;; called when you write broken nim code. (probably with
            ;; template or macro) So, just put a log function and see
            ;; what's going on only if users or I are interested.
            (nim-log "EPC(%S) ERROR %s"
                     (symbol-name method) (error-message-string err))))))))

(defun nimsuggest--call-epc (method callback)
  "Call the nimsuggest process on point.

Call the nimsuggest process responsible for the current buffer.
All commands work with the current cursor position.  METHOD can be
one of:

sug: suggest a symbol
con: suggest, but called at fun(_ <-
def: where the symbol is defined
use: where the symbol is used
dus: def + use

The CALLBACK is called with a list of ‘nim--epc’ structs."
  (let ((file (buffer-file-name)))
    (cl-case nimsuggest--state
      ((never connecting) nil) ; do nothing
      (no-response
       (nimsuggest--set-state 'never file)
       ;; Maybe M-x `epc:pop-to-last-server-process-buffer' would be
       ;; helpful to check the cause.
       (message "nimsuggest-mode reached timeout (about %dsec) due to no response from server.
This feature will be blocked on this %s."
                (/ (* nimsuggest-accept-process-delay
                      nimsuggest-accept-process-timeout-count)
                      1000)
                file))
      (ready
       (nimsuggest--query method callback (nimsuggest--get-epc-process file))
       ;; Reset `nimsuggest--state' if all epc processes for the file are dead.
       ;; Not sure if this is related to --refresh option.
       (catch 'exit
         (cl-loop for (f . _) in nimsuggest--epc-processes-alist
                  if (equal file f)
                  do (throw 'exit t)
                  finally (nimsuggest--set-state 'not-started file))))
      (not-started
       (setq-local nimsuggest--state 'connecting)
       (deferred:$
         (deferred:next
           (nimsuggest--start-epc-deferred file method callback))
         (deferred:error it
           (lambda (err) (nim-log "EPC(startup) ERROR %s" (error-message-string err))))))
      (t
       (error (format "This shouldn't happen: nimsuggest--state is %s" nimsuggest--state))))))

(defun nimsuggest--call-sync (method callback)
  "Synchronous call for nimsuggest using METHOD.
The CALLBACK function is called when it got the response."
  (let* ((buf (current-buffer))
         (start (time-to-seconds))
         (res 'trash))
    (nimsuggest--call-epc
     method
     (lambda (candidates)
       (when (eq (current-buffer) buf)
         (setq res (funcall callback candidates)))))
    (while (and (eq 'trash res) (eq (current-buffer) buf))
      (if (> (- (time-to-seconds) start) 2)
          (nim-log "EPC-sync(%s): timeout %d sec" (symbol-name method) 2)
        (sleep-for 0.03)))
    (unless (eq 'trash res)
      res)))

(defun nimsuggest--get-dirty-dir ()
  "Return temp directory.
The directory name consists of `nimsuggest-dirty-directory' and current
frame number.  The frame number is required to prevent Emacs
crash when some emacsclients open the same file."
  (let* ((frame-num (car (last (split-string (format "%s" (selected-frame)) " "))))
         (frame-num-str (substring frame-num 0 (1- (length frame-num)))))
    (file-name-as-directory (concat nimsuggest-dirty-directory frame-num-str))))

(defun nimsuggest--get-temp-file-name ()
  "Get temp file name."
  (mapconcat 'directory-file-name
             `(,(nimsuggest--get-dirty-dir)
               ,(cl-case system-type
                  ((ms-dos windows-nt cygwin)
                   ;; For bug #119, convert ":" to "꞉" (U+A789)
                   (concat "/"
                           (replace-regexp-in-string
                            ":" (char-to-string #xA789)
                            buffer-file-name)))
                  (t ; for *nix system
                   buffer-file-name)))
             ""))

(defun nimsuggest--make-tempdir (tempfile)
  "Make temporary directory for TEMPFILE."
  (let* ((tempdir (file-name-directory tempfile)))
    (unless (file-exists-p tempdir)
      (make-directory tempdir t))))

(defun nimsuggest--save-buffer-temporarly ()
  "Save the current buffer and return the location."
  (let* ((temporary-file-directory nimsuggest-dirty-directory)
         (filename (nimsuggest--get-temp-file-name)))
    (nimsuggest--make-tempdir filename)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) filename nil 1))
    filename))

(add-hook 'kill-emacs-hook 'nimsugget--delete-temp-directory)
(defun nimsugget--delete-temp-directory ()
  "Delete temporary files directory for nimsuggest."
  (when (file-exists-p nimsuggest-dirty-directory)
    (delete-directory (file-name-directory nimsuggest-dirty-directory) t)))

(defun nimsuggest--kill-zombie-processes (&optional ppath)
  "Kill needless zombie processes, which correspond to PPATH."
  (setq nimsuggest--epc-processes-alist
        (cl-loop for (file . manager) in nimsuggest--epc-processes-alist
                 if (and (epc:live-p manager)
                         (or (and ppath (equal ppath file))
                             (not ppath)))
                 collect (cons file manager)
                 else do (epc:stop-epc manager))))

(defvar nimsuggest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'nimsuggest-show-doc)
    map))

(defcustom nimsuggest-mode-hook nil
  "Hook run when entering Nimsuggest mode."
  :options '(flycheck-nimsuggest-setup nimsuggest-flymake-setup nimsuggest-xref-setup)
  :type 'hook
  :group 'nim)

;;;###autoload
(define-minor-mode nimsuggest-mode
  "Minor mode for nimsuggest."
  :lighter " nimsuggest"
  :keymap nimsuggest-mode-map
  (when nimsuggest-mode
    (nimsuggest-ensure)))

(defun nimsuggest-force-stop ()
  "Try to stop nimsuggest related things, but not well tested."
  (interactive)
  (remove-hook 'flycheck-checkers 'nim-nimsuggest)
  (remove-hook 'flymake-diagnostic-functions 'flymake-nimsuggest t)
  (nim-eldoc-off)
  (nimsuggest-xref-on-or-off 'off))

(defun nimsuggest-ensure ()
  "Ensure that users installed nimsuggest executable."
  ;; I've seen so many people just stacked to install nimsuggest at
  ;; first time. Probably this package's name is kinda confusing.
  (interactive)
  (let ((msg "Nimsuggest-mode needs external tool called nimsuggest.
Generally you can build by './koch tools' or '.koch nimsuggest'
on Nim repo (check koch.nim file), but it's good to check README
on Nim's official repository on yourself in case this document
was outdated."))
    (when (or (bound-and-true-p eldoc-mode)
              (bound-and-true-p global-eldoc-mode))
      (setq nimsuggest-eldoc-function 'nimsuggest-eldoc--nimsuggest))
    (when (not nimsuggest-path)
      (nimsuggest-force-stop)
      (error msg))
    (when (not (file-executable-p nimsuggest-path))
      (nimsuggest-force-stop)
      (error "`nimsuggest-path' isn't executable; %s" msg))
    (if nimsuggest-mode
        (nim-log "nimsuggest-mode started")
      (nim-log "nimsuggest-mode stopped"))))


;; Utilities

(defun nimsuggest--put-face (text face)
  "Put FACE on the TEXT."
  (when (and text (string< "" text))
    (add-text-properties
     0 (length text)
     `(face ,face)
     text)))

(defun nimsuggest--parse (forth)
  "Parse FORTH element."
  (when (string-match
         (rx (group (1+ word)) (0+ " ")
             (group (1+ nonl)))
         forth)
    (let ((first (match-string 1 forth))
          (other (match-string 2 forth)))
      (cons first other))))

(defun nimsuggest--trim (str)
  "Adjust STR for mini buffer."
  (let ((max-width (- (frame-width) 4))) ; <- just for buffer, probably
    (if (< (length str) max-width)       ; it depends on terminal or GUI Emacs
        str
      (let* ((short-str (substring str 0 (- (frame-width) 4)))
             (minus-offset
              (cl-loop with num = 0
                       for s in (delq "" (split-string (reverse short-str) ""))
                       if (equal s ".") do (cl-return num)
                       else do (cl-incf num)
                       finally return 0)))
        (substring short-str 0 (- (length short-str) minus-offset))))))

(defun nimsuggest--format (forth symKind qpath doc)
  "Highlight returned result from nimsuggest of FORTH, SYMKIND, QPATH, and DOC."
  (let* ((doc (mapconcat 'identity (split-string doc "\n") " "))
         (name
          (if (eq (length (cdr qpath)) 1)
              (cadr qpath)
            (mapconcat 'identity (cdr qpath) "."))))
    (nimsuggest--put-face doc font-lock-doc-face)
    (pcase (list symKind)
      (`(,(or "skProc" "skField" "skTemplate" "skMacro"))
       (when (string< "" forth)
         (cl-destructuring-bind (ptype . typeinfo) (nimsuggest--parse forth)
           (when (equal "proc" ptype)
             (nimsuggest--put-face name font-lock-function-name-face)
             (let* ((func  (format "%s %s" name typeinfo)))
               (nimsuggest--trim
                (if (string= "" doc)
                    (format "%s" func)
                  (format "%s %s" func doc))))))))
      (`(,(or "skVar" "skLet" "skConst" "skResult" "skParam"))
       (let ((sym (downcase (substring symKind 2 (length symKind)))))
         (nimsuggest--put-face sym font-lock-keyword-face)
         (nimsuggest--put-face name
                             (cond ((member symKind '("skVar" "skResult"))
                                    '(face font-lock-variable-name-face))
                                   ((member symKind '("skLet" "skConst"))
                                    '(face font-lock-constant-face))
                                   (t '(face font-lock-keyword-face))))
         (nimsuggest--trim
          (format "%s %s : %s" sym name
                  (cond
                   ((string< "" forth) forth)
                   (t "no doc"))))))
      (`("skType")
       (nimsuggest--put-face name font-lock-type-face)
       (nimsuggest--trim
        (if (not (string< "" doc))
            (format "%s: no doc" name)
          (format "%s: %s" name doc)))))))


;;; misc

;; work in progress

(defcustom nimsuggest-doc-directive
  'def
  "Directive passed by nimsuggest for `nimsuggest-show-doc'."
  :type '(choice
          (const :tag "suggest" 'sug)
          (const :tag "definition" 'def))
  :group 'nim)

(defvar nimsuggest--doc-args nil
  "Internal variable to store document data.")

(defun nimsuggest-show-doc ()
  "Show document in dedicated *nim-doc* buffer."
  (interactive)
  (nimsuggest--call-epc
   nimsuggest-doc-directive
   (lambda (args)
     (if (and (not args) (not (eq 'sug nimsuggest-doc-directive)))
         ;; Fallback if there is no result from nimsuggest by 'sug
         (let ((nimsuggest-doc-directive 'sug))
           (nimsuggest-show-doc))
       ;; TODO: should I filter returned result by current position's identifier?
       (setq nimsuggest--doc-args (cl-loop for i from 0 to (1- (length args))
                                           collect (cons (1+ i) (nth i args))))
       (nimsuggest--show-doc)))))

(defun nimsuggest--format-doc-org (doc)
  (mapc
   (lambda (x) (setq doc (replace-regexp-in-string (car x) (nth 1 x) doc)))
   `((,(concat
        ".+::.*\n"
        "\\(\\("
        "\\([ ]\\{1,\\}.+\\)?\n?"
        "\\)*\\)")
      "#+BEGIN_SRC nim\n\\1\n#+END_SRC")     ; turn code blocks into org babel
     ("\n*\\(\n#\\+BEGIN_SRC nim\n\\)\n*" "\n\\1") ; cleanup extra newlines
     ("\n*\\(\n#\\+END_SRC\\)\n*" "\\1\n\n") ; cleanup extra newlines
     ("`\\([^`]+\\)`:idx:" "\\1")            ; clean :idx: fields
     ("``" "~")                              ; inline code highlighting
     ("`\\([A-z0-9\\-]+\\) *`" "~\\1~")      ; inline code highlighting
     ("`\\([^<]+[^\n ]\\)[ \n]*<[^ \n]+>`_" "~\\1~") ; turn doc links into inline code
     ("^\\* " "- ")                          ; * bullets into -
     ("\\*\\*\\([^*]+\\)\\*\\*" "*\\1*")     ; bold
     ("^\\(.+\\)\n[=]+$" "** \\1")           ; format headers
     ("^\\(.+\\)\n[-]+$" "*** \\1")
     ("^\\(.+\\)\n[~]+$" "**** \\1")
     ("^\\(~.+~\\)\\([ ]\\{2,\\}\\)" "\\1  \\2"))) ; add missing spaces to tables
  doc)

(defun nimsuggest--link-location-org (location)
  (replace-regexp-in-string ".+" "[[file:\\&][\\&]]" location))

(defun nimsuggest--doc-insert-nav (def)
  (let ((nominator (caar nimsuggest--doc-args))
        (denominator (length nimsuggest--doc-args)))
    (format "%s %s\n"
            (mapconcat 'identity (nim--epc-qpath def) " ")
            (if (eq 1 denominator)
                ""
              (format "%s/%s %s" nominator denominator
                      "-- < next, > previous")))))

(defun nimsuggest--header-rst(header)
  (format "%s\n%s" header (make-string (length header) ?#)))

(defun nimsuggest--show-doc-rst (def)
  "Display Nim docs using rst-mode."
  (cl-mapcar
   (lambda (x) (insert (concat x "\n")))
   (list
    ;; (format "debug %s\n" nimsuggest--doc-args)
    (nimsuggest--doc-insert-nav def)
    (format "%s\n%s %s\n"
            (nimsuggest--header-rst "Signature")
            (nim--epc-symkind def)
            (nim--epc-forth def))
    (unless (string= "" (nim--epc-doc def))
      (format "%s\n%s\n"
              (nimsuggest--header-rst "Documentation")
              (nim--epc-doc def)))
    (format "%s\n%s\n"
            (nimsuggest--header-rst "Location")
            (nim--epc-file def))))
  (when (fboundp 'rst-mode)
    (rst-mode)))

(defun nimsuggest--show-doc-org (def)
  "Display Nim docs using org-mode formatting."
  (cl-mapcar
   (lambda (x) (insert (concat x "\n")))
   (list
    ;; (format "debug %s\n" nimsuggest--doc-args)
    (nimsuggest--doc-insert-nav def)
    (format "* Signature\n%s %s\n"
            (nim--epc-symkind def)
            (nim--epc-forth def))
    (unless (string= "" (nim--epc-doc def))
      (format "* Documentation\n%s\n"
              (nimsuggest--format-doc-org (nim--epc-doc def))))
    (format "* Location\n%s\n"
            (nimsuggest--link-location-org (nim--epc-file def)))))
  (when (fboundp 'org-mode)
    (org-mode)
    (org-show-all)))

(defun nimsuggest--show-doc ()
  "Internal function for `nimsuggest-show-doc'."
  (let ((def (cdar nimsuggest--doc-args)))
    (get-buffer-create "*nim-doc*")
    (unless (equal (current-buffer) (get-buffer "*nim-doc*"))
      (switch-to-buffer-other-window "*nim-doc*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (funcall nimsuggest-show-doc-function def)
    (when (fboundp 'evil-mode)
      (evil-make-intercept-map nimsuggest-doc-mode-map))
    (use-local-map nimsuggest-doc-mode-map)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun nimsuggest-doc-next ()
  "Move to next page."
  (interactive)
  (if (not (< 0 (length nimsuggest--doc-args)))
      (minibuffer-message "there is no next")
    (let ((popped (pop nimsuggest--doc-args)))
      (setq nimsuggest--doc-args (append nimsuggest--doc-args (list popped)))
      (nimsuggest--show-doc))))

(defun nimsuggest-doc-previous ()
  "Move to previous page."
  (interactive)
  (if (not (< 0 (length nimsuggest--doc-args)))
      (minibuffer-message "there is no previous")
    (let* ((rargs (reverse nimsuggest--doc-args))
           (popped (pop rargs)))
      (setq rargs (append rargs (list popped))
            nimsuggest--doc-args (reverse rargs))
      (nimsuggest--show-doc))))


;;; Flymake integration

;; From Emacs 26, flymake was re-written by João Távora.
;; It supports asynchronous backend, so enable it if users
;; turned on the flymake-mode.

;; Manual configuration:
;;   (add-hook 'nimsuggest-mode-hook 'nimsuggest-flymake-setup)

;; TODO: specify more specific version
(when (version<= "26" (number-to-string emacs-major-version))
  (add-hook 'nimsuggest-mode-hook 'nimsuggest-flymake-setup))

;;;###autoload
(defun nimsuggest-flymake-setup()
  "Kinda experimental function to use flymake on Emacs 26."
  (when (and (bound-and-true-p flymake-mode)
             (not (bound-and-true-p flycheck-mode)))
    (if nimsuggest-mode
        (add-hook  'flymake-diagnostic-functions 'flymake-nimsuggest nil t)
      (remove-hook 'flymake-diagnostic-functions 'flymake-nimsuggest t))))

(defun nimsuggest--flymake-filter (errors buffer)
  "Remove not related ERRORS in the BUFFER."
  (cl-loop for (_ _ _ file type line col text _) in errors
           if (and (eq buffer (get-file-buffer file))
                   (<= 1 line) (<= 0 col))
           ;; column needs to be increased by 1 to highlight correctly
           collect (list file type line (1+ col) text)))

(defun nimsuggest--flymake-region (file buf line col)
  "Calculate beg and end for FILE, BUF, LINE, and COL.
Workaround for https://github.com/nim-lang/nim-mode/issues/183."
  (if (not (eq (get-file-buffer file) (current-buffer)))
      (cons 0 1)
    (cl-letf* (((symbol-function 'end-of-thing)
                (lambda (&rest _r) nil)))
      (funcall 'flymake-diag-region buf line col))))

(defun nimsuggest--flymake-error-parser (errors buffer)
  "Return list of result of `flymake-make-diagnostic' from ERRORS.
The list can be nil.  ERRORS will be skipped if BUFFER and
parsed file was different."
  (cl-loop with errs = (nimsuggest--flymake-filter errors buffer)
           for (file typ line col text) in errs
           for type = (cl-case (string-to-char typ)
                        (?E :error)
                        (?W :warning)
                        (t  :note))
           for (beg . end) = (nimsuggest--flymake-region file buffer line col)
           collect (funcall 'flymake-make-diagnostic buffer beg end type text)))

(defun flymake-nimsuggest (report-fn &rest _args)
  "A Flymake backend for Nim language using Nimsuggest.
See `flymake-diagnostic-functions' for REPORT-FN and ARGS."
  (let ((buffer (current-buffer)))
    (nimsuggest--call-epc
     'chk
     (lambda (errors)
       (nim-log "FLYMAKE(start): report(s) number of %i" (length errors))
       (condition-case err
           (let ((report-action
                  (nimsuggest--flymake-error-parser errors buffer)))
             (funcall report-fn report-action))
         (error
          (nim-log "FLYMAKE(error): %s" (error-message-string err))))))))

;; TODO: is this really needed?
(defun nimsuggest-flymake--panic (report-fn err)
  "TODO: not sure where to use this yet...
Using this function cause to stop flymake completely which is not
suitable for nimsuggest because nimsuggest re-start after its
crush.

You can find explanation REPORT-FN at `flymake-diagnostic-functions'
and the ERR is captured error."
  (when (member 'flymake-nimsuggest flymake-diagnostic-functions)
    (nim-log-err "FLYMAKE(ERR): %s" err)
    (funcall report-fn :panic :explanation err)))


;;; ElDoc for nimsuggest

(defvar nimsuggest-eldoc--data nil)

;;;###autoload
(defun nimsuggest-eldoc--nimsuggest ()
  "Update `eldoc-last-message' by nimsuggest's information."
  (when (nimsuggest-available-p)
    (unless (nimsuggest-eldoc--same-try-p)
      (nimsuggest-eldoc--call))
    (when (eq (line-number-at-pos)
              (assoc-default :line nimsuggest-eldoc--data))
      (assoc-default :str nimsuggest-eldoc--data))))

(defun nimsuggest-eldoc--same-try-p ()
  "Predicate function if same try or not."
  (or (and (equal (nim-current-symbol)
                  (assoc-default :name nimsuggest-eldoc--data))
           (eq (assoc-default :line nimsuggest-eldoc--data)
               (line-number-at-pos)))
      (and (nim-eldoc-inside-paren-p)
           (save-excursion
             (nimsuggest-eldoc--move)
             (or
              ;; for template
              (eq (point) (assoc-default :pos nimsuggest-eldoc--data))
              ;; for proc
              (eq (1- (point)) (assoc-default :pos nimsuggest-eldoc--data)))))))

(defun nimsuggest-eldoc--move ()
  "Move cursor appropriate point where calling nimsuggest is suitable."
  (let ((pos  (point))
        (ppss (syntax-ppss)))
    (when (nim-eldoc-inside-paren-p)
      (save-excursion
        (goto-char (nth 1 ppss))
        (when (looking-back nim-eldoc--skip-regex nil)
          (goto-char pos))))))

(defun nim-eldoc-format-string (defs)
  "Format data inside DEFS for eldoc.
DEFS is group of definitions from nimsuggest."
  ;; TODO: switch if there are multiple defs
  (nim-log "ELDOC format")
  (let ((data (cl-first defs)))
    (apply 'nimsuggest--format
           (mapcar (lambda (x) (funcall x data))
           '(nim--epc-forth nim--epc-symkind nim--epc-qpath nim--epc-doc)))))

(defun nimsuggest-eldoc--call ()
  "Call nimsuggest for eldoc."
  (save-excursion
    (nimsuggest-eldoc--move)
    (nim-log "ELDOC-1")
    (nimsuggest--call-epc 'dus 'nimsuggest-eldoc--update)))

(defun nimsuggest-eldoc--update (def-use)
  "Update eldoc information from DEF-USE of nimsuggest."
  (if (not (nim-eldoc--try-p))
      (nim-log "ELDOC stop update")
    (nim-log "ELDOC update")
    (if def-use
        (nimsuggest-eldoc--update-1 def-use)
      (save-excursion
        (when (nim-eldoc-inside-paren-p)
          (nimsuggest-eldoc--move)
          (backward-char)
          (nimsuggest--call-epc 'dus 'nimsuggest-eldoc--update-1))))))

(defun nimsuggest-eldoc--update-1 (epc-result)
  "Save EPC-RESULT into `nimsuggest-eldoc--data'.
And show message of `eldoc-last-message'.
The EPC-RESULT can be result of both def and/or dus."
  (when epc-result
    (setq nimsuggest-eldoc--data
          (list
           (cons :str  (nim-eldoc-format-string epc-result))
           (cons :line (line-number-at-pos))
           (cons :name (nim-current-symbol))
           (cons :pos  (point))))
    (setq eldoc-last-message (assoc-default :str nimsuggest-eldoc--data))
    (message eldoc-last-message)))


;;; xref integration
;; This package likely be supported on Emacs 25.1 or later

(defvar nimsuggest-find-definition-function nil
  "Function for `nimsuggest-find-definition'.")

;;;###autoload
(add-hook 'nimsuggest-mode-hook 'nimsuggest-xref-setup)
;;;###autoload
(defun nimsuggest-xref-setup ()
  "Setup xref backend for nimsuggest."
  (cond
   ((not (nimsuggest-available-p))
    (nim-log "xref package needs nimsuggest"))
   ((not (require 'xref nil t))
    (setq nimsuggest-find-definition-function 'nimsuggest-find-definition-old)
    ;; Note below configuration were removed on the future
    (define-key nimsuggest-mode-map (kbd "M-.") #'nimsuggest-find-definition)
    (define-key nimsuggest-mode-map (kbd "M-,") #'pop-tag-mark))
   ((version<= "25.1.0" emacs-version)
    (require 'xref)
    (setq nimsuggest-find-definition-function 'xref-find-definitions)
    (nimsuggest-xref-on-or-off (if nimsuggest-mode 'on 'off)))
   (t (nim-log "xref unexpected condition"))))

(defun nimsuggest-xref-on-or-off (on-or-off)
  "Turn on or off xref feature for nimsuggest backend.
You can specify `on' or `off' symbol as the ON-OR-OFF."
  (cl-case on-or-off
    (on  (add-hook 'xref-backend-functions #'nimsuggest--xref-backend nil t))
    (off (remove-hook 'xref-backend-functions #'nimsuggest--xref-backend t))))

(defun nimsuggest-find-definition ()
  "This function is preserved for backward compatibility.
If your Emacs support cross reference library `xref' (from Emacs
25.1), you might want to use `xref-find-definition' instead which
binds to `M-.' in default."
  (interactive)
  (call-interactively nimsuggest-find-definition-function))

;; Define xref backend for nimsuggest
(with-eval-after-load "xref"
  (defun nimsuggest--xref-backend () 'nimsuggest)
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql nimsuggest)))
    "Return string or nil for identifier at point."
    ;; Well this function may not needed for current xref functions for
    ;; nimsuggest backend.
    (with-syntax-table nim-dotty-syntax-table
      (let ((thing (thing-at-point 'symbol)))
        (and thing (substring-no-properties thing)))))

  (defun nimsuggest--xref-make-obj (id def)
    (let ((summary id)
          (location (xref-make-file-location
                     (nim--epc-file def)
                     (nim--epc-line def)
                     (nim--epc-column def))))
      (xref-make summary location)))

  (defun nimsuggest--xref (query id)
    (nimsuggest--call-sync
     query
     (lambda (results)
       (cond
        ((null results) nil)
        ((listp results)
         (cl-loop for result in results
                  collect (nimsuggest--xref-make-obj id result)))))))

  (cl-defmethod xref-backend-definitions ((_backend (eql nimsuggest)) id)
    (nimsuggest--xref 'def id))

  (cl-defmethod xref-backend-references ((_backend (eql nimsuggest)) id)
    (nimsuggest--xref 'dus id))

  ;; just define empty backend to use `xref-backend-references' for
  ;; nimsuggest.
  (cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql nimsuggest))))

  ;; Not implement yet, or not sure maybe, won't...
  ;; (cl-defmethod xref-backend-apropos ((_backend (eql nimsuggest)) pattern))

  ) ; end of with-eval-after-load xref

;; Workaround for old Emacsen
;; TODO: remove those stuff after Emacs 25 or 26 is dominant.
(require 'etags)
(defun nimsuggest-find-definition-old ()
  "Go to the definition of the symbol currently under the cursor."
  (nimsuggest--call-epc
   'def
   (lambda (defs)
     (let ((def (cl-first defs)))
       (when (not def) (error "Definition not found"))
       (if (fboundp 'xref-push-marker-stack)
           (xref-push-marker-stack)
         (with-no-warnings
           (ring-insert find-tag-marker-ring (point-marker))))
       (find-file (nim--epc-file def))
       (goto-char (point-min))
       (forward-line (1- (nim--epc-line def)))))))

(define-obsolete-function-alias 'nim-goto-sym 'nimsuggest-find-definition
  "2017/9/02")


;;; Debug
(defun nimsuggest--put (text where)
  "Put text property to TEXT of WHERE."
  (put-text-property where (1+ where) 'face 'success text))

(defun nimsuggest--debug-prompt ()
  "Return string for read key."
  (let ((msgs '(sug (def . 2) dus use con highlight outline)))
    (cl-loop for msg in msgs
             for text = (symbol-name (if (symbolp msg) msg (car msg)))
             for where = (if (symbolp msg) 0 (cdr msg))
             do (nimsuggest--put text where)
             collect text into res
             finally return (mapconcat 'identity res " "))))

(defun nimsuggest--debug-print ()
  "Print result of nimsuggest's epc call."
  (interactive)
  (let ((input (read-key (nimsuggest--debug-prompt))))
    (let ((key (cl-case input
                 (?s 'sug)
                 (?f 'def)
                 (?d 'dus)
                 (?u 'use)
                 (?c 'con)
                 (?h 'highlight)
                 (?o 'outline))))
      (if (null key)
          (minibuffer-message "unexpected key %c" input)
        (message "calling nimsuggest ...")
        (nimsuggest--call-epc
         key
         (lambda (args)
           (message "nimsuggest's result:\n%s" args)))))))

;; (define-key nimsuggest-mode-map (kbd "C-8") 'nimsuggest--debug-print)

(provide 'nim-suggest)
;;; nim-suggest.el ends here

