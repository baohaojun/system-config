;;; ac-clang.el --- Auto Completion source by libclang for GNU Emacs -*- lexical-binding: t; -*-

;;; last updated : 2018/07/10.14:40:56

;; Copyright (C) 2010       Brian Jiang
;; Copyright (C) 2012       Taylan Ulrich Bayirli/Kammer
;; Copyright (C) 2013       Golevka
;; Copyright (C) 2013-2018  yaruopooner
;; 
;; Original Authors: Brian Jiang <brianjcj@gmail.com>
;;                   Golevka [https://github.com/Golevka]
;;                   Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;                   Many others
;; Author: yaruopooner [https://github.com/yaruopooner]
;; URL: https://github.com/yaruopooner/ac-clang
;; Keywords: completion, convenience, intellisense
;; Version: 2.1.3
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (auto-complete "1.4.0") (pos-tip "0.4.6") (yasnippet "0.8.0"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; 
;; * INTRODUCTION:
;;   This program fork from auto-complete-clang-async.el
;;   ac-clang provide code completion and arguments expand.
;;   This program consists of the client(elisp) and server(binary).
;;   The server is executable file, and a self-build is necessary.
;;   The server achieve code completion using libclang of LLVM.
;; 
;; * FEATURES:
;;   - Basic(same auto-complete-clang-async)
;;     Code Completion by libclang.
;;     Auto Completion support. 
;;     Uses a "completion server" process to utilize libclang.
;;     C/C++/Objective-C mode support.
;;     Jump to definition or declaration. return from jumped location. 
;;     Jump is an on-the-fly that doesn't use the tag file.
;;     Also provides flymake syntax checking.
;;     A few bugfix and refactoring.
;;    
;;   - Extension
;;     "completion server" process is 1 process per Emacs. (original version is per buffer)
;;     Template Parameters expand. 
;;     Manual Completion.
;;     Display Brief Comment of completion candidate.
;;     libclang CXTranslationUnit Flags support. 
;;     libclang CXCodeComplete Flags support. 
;;     Multibyte support. 
;;     Jump to inclusion-file. return from jumped location. 
;;     IPC packet format can be specified.
;;     Debug Logger Buffer. 
;;     Performance Profiler.
;;     more a few modified. (client & server)
;;    
;;   - Optional
;;     CMake support.
;;     clang-server.exe and libclang.dll built with Microsoft Visual Studio 2017/2015/2013.
;;     x86_64 Machine Architecture + Windows Platform support. (Visual Studio Predefined Macros)
;; 
;; * EASY INSTALLATION(Windows Only):
;;   - Visual C++ Redistributable Packages for Visual Studio 2017/2015/2013
;;     Must be installed if don't have a Visual Studio 2017/2015/2013.
;; 
;;     - 2017
;;       [https://www.visualstudio.com/downloads/?q=#other]
;;     - 2015
;;       [http://www.microsoft.com/download/details.aspx?id=53587]
;;     - 2013/2012/2010/2008
;;       [http://www.standaloneofflineinstallers.com/2015/12/Microsoft-Visual-C-Redistributable-2015-2013-2012-2010-2008-2005-32-bit-x86-64-bit-x64-Standalone-Offline-Installer-for-Windows.html]
;;    
;;   - Completion Server Program
;;     Built with Microsoft Visual Studio 2017/2015/2013.
;;     [https://github.com/yaruopooner/ac-clang/releases]
;;     1. download clang-server.zip
;;     2. clang-server.exe and libclang.dll is expected to be available in the PATH or in Emacs' exec-path.
;;    
;; * STANDARD INSTALLATION(Linux, Windows):
;;   Generate a Unix Makefile or a Visual Studio Project by CMake.
;; 
;;   - Self-Build step
;;     1. LLVM
;;        checkout, apply patch, generate project, build
;;        It is recommended that you use this shell.
;;        [https://github.com/yaruopooner/llvm-build-shells.git]
;; 
;;     2. Clang Server
;;        generate project, build
;; 
;;     see clang-server's reference manual.
;;     ac-clang/clang-server/readme.org
;; 
;; * NOTICE:
;;   - LLVM libclang.[dll, so, ...]
;;     This binary is not official binary.
;;     Because offical libclang has mmap lock problem.
;;     Applied a patch to LLVM's source code in order to solve this problem.
;; 
;;     see clang-server's reference manual.
;;     ac-clang/clang-server/readme.org
;; 


;; Usage:
;; * DETAILED MANUAL:
;;   For more information and detailed usage, refer to the project page:
;;   [https://github.com/yaruopooner/ac-clang]
;; 
;; * SETUP:
;;   (require 'ac-clang)
;; 
;;   ;; Windows Only
;;   (when (eq system-type 'windows-nt)
;;     (setq w32-pipe-read-delay 0))
;; 
;;   (when (ac-clang-initialize)
;;     (add-hook 'c-mode-common-hook '(lambda ()
;;                                      (setq clang-server-cflags CFLAGS)
;;                                      (ac-clang-activate-after-modify))))
;; 
;; * DEFAULT KEYBIND
;;   - start auto completion
;;     code completion & arguments expand
;;     `.` `->` `::`
;;   - start manual completion
;;     code completion & arguments expand
;;     `<tab>`
;;   - jump to inclusion-file, definition, declaration / return from it
;;     this is nestable jump.
;;     `M-.` / `M-,`
;; 

;;; Code:



(eval-when-compile (require 'cl-lib))
(require 'clang-server)
(require 'auto-complete)
(require 'pos-tip)
(require 'yasnippet)
(require 'flymake)




(defconst ac-clang-version "2.1.3")




;;;
;;; for auto-complete Variables
;;;

;; clang-server response filter pattern for auto-complete candidates
;; (defconst ac-clang--completion-pattern "^COMPLETION: \\(%s[^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)")

;; auto-complete behaviors
(defvar ac-clang-async-autocompletion-automatically-p t
  "If autocompletion is automatically triggered when you type `.', `->', `::'")

(defvar ac-clang-async-autocompletion-manualtrigger-key "<tab>")


(defvar ac-clang-quick-help-prefer-pos-tip-p nil
  "Specify the popup package used for auto-complete.
Overwrite to `ac-quick-help-prefer-pos-tip' by this value.
This value has a big impact on popup scroll performance.
`t'   : use `pos-tip.el' package. Degrade popup scroll response.
`nil' : use `popup.el' package. Improve popup scroll response.
")



;; auto-complete faces
(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)



;; mode definitions
(defvar ac-clang--mode-key-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") #'ac-clang-async-autocomplete-autotrigger)
    (define-key map (kbd ">") #'ac-clang-async-autocomplete-autotrigger)
    (define-key map (kbd ":") #'ac-clang-async-autocomplete-autotrigger)
    (define-key map (kbd ac-clang-async-autocompletion-manualtrigger-key) #'ac-clang-async-autocomplete-manualtrigger)
    map)
  "ac-clang mode key map")





;;;
;;; Server Variables
;;;

;; clang-server behavior
(defvaralias 'ac-clang--activate-buffers 'clang-server-session-establishing-buffers)


;; clang translation unit behavior
(defvaralias 'ac-clang-clang-translation-unit-flags 'clang-server-translation-unit-flags)
(defvaralias 'ac-clang-clang-complete-at-flags 'clang-server-complete-at-flags)
(defvaralias 'ac-clang-cflags 'clang-server-cflags)




;;;
;;; Session Variables
;;;

;; For flymake suspend/resume control during snippet expansion
(defvar-local ac-clang--snippet-expanding-p nil)


;; auto-complete ac-sources backup
(defvar-local ac-clang--ac-sources-backup nil)


;; auto-complete candidates and completion start-point
(defvar-local ac-clang--candidates nil)
(defvar-local ac-clang--start-point nil)
(defvar-local ac-clang--template-candidates nil)
(defvar-local ac-clang--template-start-point nil)


(defvar ac-clang--completion-command-result-data nil
  "This variable is for completion feature.
Backup for reference from delay execution function.")


(defvar ac-clang--jump-stack nil
  "This variable is for jump feature.
The jump stack (keeps track of jumps via jump-inclusion, jump-definition, jump-declaration, jump-smart)") 




;;;
;;; receive clang-server responses. 
;;; build completion candidates and fire auto-complete.
;;;

(defun ac-clang--build-completion-candidates (data start-word)
  ;; (message "ac-clang--build-completion-candidates")
  (let* ((results (plist-get data :Results))
         (pattern (regexp-quote start-word))
         candidates
         candidate
         declaration
         (index 0)
         (prev-candidate ""))

    (mapc (lambda (element)
            (let ((name (plist-get element :Name))
                  (prototype (plist-get element :Prototype)))
              (when (string-match-p pattern name)
                (setq candidate name
                      declaration prototype)

                (if (string= candidate prev-candidate)
                    (progn
                      (when declaration
                        (setq candidate (propertize candidate :detail (concat (get-text-property 0 :detail (car candidates)) "\n" declaration)
                                                    :indices (append (get-text-property 0 :indices (car candidates)) `(,index))))
                        (setf (car candidates) candidate)))
                  (setq prev-candidate candidate)
                  (when declaration
                    (setq candidate (propertize candidate :detail declaration :indices `(,index))))
                  (push candidate candidates))))
            (setq index (1+ index)))
          results)
    (nreverse candidates)))


(defun ac-clang--receive-completion (data args)
  (setq ac-clang--candidates (ac-clang--build-completion-candidates data (plist-get args :start-word)))
  (setq ac-clang--start-point (plist-get args :start-point))
  ;; backup for reference from delay execution function.
  (setq ac-clang--completion-command-result-data data)

  ;; (setq ac-show-menu t)
  ;; (ac-start :force-init t)
  ;; (ac-update))
  (ac-complete-clang-async))



(defun ac-clang--get-autotrigger-start-point (&optional point)
  (unless point
    (setq point (point)))
  (let ((c (char-before point)))
    (when (or 
           ;; '.'
           (eq ?. c)
           ;; '->'
           (and (eq ?> c)
                (eq ?- (char-before (1- point))))
           ;; '::'
           (and (eq ?: c)
                (eq ?: (char-before (1- point)))))
      point)))


(defun ac-clang--get-manualtrigger-start-point ()
  (let* ((symbol-point (ac-prefix-symbol))
         (point (or symbol-point (point)))
         (c (char-before point)))
    (when (or 
           (ac-clang--get-autotrigger-start-point point)
           ;; ' ' for manual completion
           (eq ?\s c))
      point)))


(defsubst ac-clang--async-completion (start-point)
  (when start-point
    (clang-server-request-transaction
     #'clang-server-send-completion-command
     #'ac-clang--receive-completion
     `(:start-word ,(buffer-substring-no-properties start-point (point)) :start-point ,start-point))))


(defun ac-clang-async-autocomplete-autotrigger ()
  (interactive)

  (self-insert-command 1)
  (when ac-clang-async-autocompletion-automatically-p
    (ac-clang--async-completion (ac-clang--get-autotrigger-start-point))))


(defun ac-clang-async-autocomplete-manualtrigger ()
  (interactive)

  (ac-clang--async-completion (ac-clang--get-manualtrigger-start-point)))




;;;
;;; auto-complete ac-source build functions
;;;

(defsubst ac-clang--candidates ()
  ac-clang--candidates)


(defsubst ac-clang--prefix ()
  ac-clang--start-point)


(defsubst ac-clang--clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>\\|{#\\|#}\\|\\[#" "" s))
    (setq s (replace-regexp-in-string "#\\]" " " s)))
  s)


(defsubst ac-clang--in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))


(defun ac-clang--action ()
  (interactive)

  ;; (ac-last-quick-help)
  (let* ((func-name (regexp-quote (substring-no-properties (cdr ac-last-completion))))
         (c/c++-pattern (format "\\(?:^.*%s\\)\\([<(].*[>)]\\)" func-name))
         (objc-pattern (format "\\(?:^.*%s\\)\\(:.*\\)" func-name))
         (detail (get-text-property 0 :detail (cdr ac-last-completion)))
         (indices (get-text-property 0 :indices (cdr ac-last-completion)))
         (help (ac-clang--clean-document detail))
         (declarations (split-string detail "\n"))
         args
         (ret-t "")
         ret-f
         index
         candidates)

    ;; parse function or method overload declarations
    (cl-dolist (declaration declarations)
      (setq index (pop indices))

      ;; function result type
      (when (string-match "\\[#\\(.*\\)#\\]" declaration)
        (setq ret-t (match-string 1 declaration)))
      ;; remove result type
      (setq declaration (replace-regexp-in-string "\\[#.*?#\\]" "" declaration))

      ;; (message (format "comp--action: func-name=%s, detail=%s" func-name detail))

      ;; parse arguments
      (cond (;; C/C++ standard argument
             (string-match c/c++-pattern declaration)
             (setq args (match-string 1 declaration))
             (push (propertize (ac-clang--clean-document args) :detail ret-t :args args :indices `(,index)) candidates)
             ;; default argument
             (when (string-match "{#" args)
               (setq args (replace-regexp-in-string "{#.*#}" "" args))
               (push (propertize (ac-clang--clean-document args) :detail ret-t :args args :indices `(,index)) candidates))
             ;; variadic argument
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize (ac-clang--clean-document args) :detail ret-t :args args :indices `(,index)) candidates)))

            (;; check whether it is a function ptr
             (string-match "^\\([^(]*\\)(\\*)\\((.*)\\)" ret-t)
             (setq ret-f (match-string 1 ret-t)
                   args (match-string 2 ret-t))
             (push (propertize args :detail ret-f :args "" :indices `(,index)) candidates)
             ;; variadic argument
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize args :detail ret-f :args "" :indices `(,index)) candidates)))

            (;; Objective-C/C++ argument
             (string-match objc-pattern declaration)
             (setq args (match-string 1 declaration))
             (push (propertize (ac-clang--clean-document args) :detail ret-t :args args :indices `(,index)) candidates))))

    (cond (candidates
           (setq candidates (delete-dups candidates))
           (setq candidates (nreverse candidates))
           (setq ac-clang--template-candidates candidates)
           (setq ac-clang--template-start-point (point))
           (ac-complete-clang-template)

           (unless (cdr candidates) ;; unless length > 1
             (message (replace-regexp-in-string "\n" "   ;    " help))))
          (t
           (message (replace-regexp-in-string "\n" "   ;    " help))))))


(defun ac-clang--document (item)
  (if (stringp item)
      (let* ((detail (get-text-property 0 :detail item))
             (indices (get-text-property 0 :indices item))
             (results (plist-get ac-clang--completion-command-result-data :Results))
             (element (aref results (car indices)))
             (bc (plist-get element :BriefComment)))
        (when bc
          (message "BriefComment : %s" bc))
        ;; (message (format "clang--document: item=%s, detail=%s" item detail))
        (ac-clang--clean-document detail)))
  ;; (popup-item-property item :detail)
  ;; nil
  )



(ac-define-source clang-async
  '((candidates     . ac-clang--candidates)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix         . ac-clang--prefix)
    (requires       . 0)
    (action         . ac-clang--action)
    (document       . ac-clang--document)
    (cache)
    (symbol         . "c")))



(defun ac-clang--same-count-in-string (c1 c2 s)
  (let ((count 0)
        (cur 0)
        (end (length s))
        c)
    (while (< cur end)
      (setq c (aref s cur))
      (cond ((eq c1 c)
             (setq count (1+ count)))
            ((eq c2 c)
             (setq count (1- count))))
      (setq cur (1+ cur)))
    (= count 0)))


(defun ac-clang--split-args (args)
  (let ((arg-list (split-string args ", *")))
    (cond ((string-match "<\\|(" args)
           (let (res
                 (pre "")
                 subs)
             (while arg-list
               (setq subs (pop arg-list))
               (unless (string= pre "")
                 (setq subs (concat pre ", " subs))
                 (setq pre ""))
               (cond ((and (ac-clang--same-count-in-string ?\< ?\> subs)
                           (ac-clang--same-count-in-string ?\( ?\) subs))
                      ;; (cond ((ac-clang--same-count-in-string ?\< ?\> subs)
                      (push subs res))
                     (t
                      (setq pre subs))))
             (nreverse res)))
          (t
           arg-list))))


(defsubst ac-clang--template-candidates ()
  ac-clang--template-candidates)


(defsubst ac-clang--template-prefix ()
  ac-clang--template-start-point)


(defun ac-clang--template-action ()
  (interactive)

  (when ac-clang--template-start-point
    (let ((args (get-text-property 0 :args (cdr ac-last-completion)))
          (point (point))
          (snp "")
          arg-list)
      ;; (message (format "org=%s" s))
      (cond (;; function ptr call
             (string= args "")
             (setq args (cdr ac-last-completion))
             (setq args (replace-regexp-in-string "^(\\|)$" "" args))
             (setq arg-list (ac-clang--split-args args))
             (cl-dolist (arg arg-list)
               (setq snp (concat snp ", ${" arg "}")))
             ;; (message (format "t0:arg-list=%s, args=%s, snp=%s" arg-list args snp))
             (yas-expand-snippet (concat "("  (substring snp 2) ")") ac-clang--template-start-point point))
            (;; function args
             t
             (unless (string= args "()")
               ;; NOTICE:Be sure to replace the backslash at the beginning.
               (setq args (replace-regexp-in-string "\\\\" "\\\\" args nil t))
               ;; NOTICE:Replace the escape character string after backslash replacement. (prevent re-replace backslash)
               (setq args (replace-regexp-in-string "\"" "\\\"" args nil t))
               ;; The abstract holder replace to snippet holder.
               (setq args (replace-regexp-in-string "{#" "${" args))
               (setq args (replace-regexp-in-string "#}" "}" args))
               (setq args (replace-regexp-in-string "<#" "${" args))
               (setq args (replace-regexp-in-string "#>" "}" args))
               (setq args (replace-regexp-in-string ", \\.\\.\\." "}, ${..." args))
               ;; (message (format "t1:args=%s" args))
               (yas-expand-snippet args ac-clang--template-start-point point)))))))


;; This source shall only be used internally.
(ac-define-source clang-template
  '((candidates . ac-clang--template-candidates)
    (prefix     . ac-clang--template-prefix)
    (requires   . 0)
    (action     . ac-clang--template-action)
    (document   . ac-clang--document)
    (cache)
    (symbol     . "t")))




;;;
;;; receive clang-server responses. 
;;; syntax checking with flymake
;;;

(defun ac-clang--receive-diagnostics (data args)
  ;; official flymake execution sequence.
  ;; flymake-process-sentinel(step in)-> flymake-parse-residual(step over)-> flymake-post-syntax-check(step over) -> flymake-process-sentinel(step out)

  (when buffer-file-name
    (let* ((results (plist-get data :Results))
           (diagnostics (plist-get results :Diagnostics)))
      (flymake-log 3 "received data")
      (flymake-parse-output-and-residual diagnostics))

    (save-restriction
      (widen)
      (flymake-parse-residual)
      ;; below logic is copy from part of flymake-post-syntax-check.
      (setq flymake-err-info flymake-new-err-info)
      (setq flymake-new-err-info nil)
      (setq flymake-err-info (flymake-fix-line-numbers flymake-err-info 1 (count-lines (point-min) (point-max))))
      (flymake-delete-own-overlays)
      (flymake-highlight-err-lines flymake-err-info)

      (let ((err-count (flymake-get-err-count flymake-err-info "e"))
            (warn-count (flymake-get-err-count flymake-err-info "w")))
        (flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
                     (buffer-name) err-count warn-count
                     (- (float-time) (plist-get args :start-time)))
        (if (and (equal 0 err-count) (equal 0 warn-count))
            (flymake-report-status "" "") ; PASSED
          (flymake-report-status (format "%d/%d" err-count warn-count) ""))))))


(defun ac-clang-diagnostics ()
  (interactive)

  (ac-clang-mode--on)

  (clang-server-request-transaction #'clang-server-send-diagnostics-command #'ac-clang--receive-diagnostics `(:start-time ,(float-time))))




;;;
;;; receive clang-server responses. 
;;; jump declaration/definition/smart-jump
;;;

(defun ac-clang--receive-jump (data _arg)
  (let* ((results (plist-get data :Results))
         (filename (plist-get results :Path))
         (line (plist-get results :Line))
         (column (1- (plist-get results :Column)))
         (new-loc `(,filename ,line ,column))
         (current-loc (list (buffer-file-name) (line-number-at-pos) (current-column))))
    (when (not (equal current-loc new-loc))
      (push current-loc ac-clang--jump-stack)
      (ac-clang--jump new-loc))))


(defun ac-clang--jump (location)
  (let* ((filename (pop location))
         (line (pop location))
         (column (pop location)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (ac-clang--inherit-environment)))


(defun ac-clang-jump-back ()
  (interactive)

  (when ac-clang--jump-stack
    (ac-clang--jump (pop ac-clang--jump-stack))))


(defun ac-clang--inherit-environment ()
  "jump function utility.
It executions if `clang-server-cflags' is nil when the jump function is executed.
This feature is assumed to jump to the system library file or third party library file.
Because, those files don't belong to the project or don't have CFLAGS.
In such a case, I think that it is appropriate to adopt CFLAGS of the jump source file for CFLAGS of the jump destination file."

  (when (and (not clang-server-cflags) ac-clang--jump-stack)
    ;; get information from last jump buffer.
    (let* ((path (caar ac-clang--jump-stack))
           (buffer (get-file-buffer path))
           cflags
           mode)
      (when buffer
        ;; get variable from previous buffer
        (with-current-buffer buffer
          (setq cflags clang-server-cflags)
          (setq mode major-mode))
        ;; set to current buffer
        (unless (eq major-mode mode)
          (funcall mode))
        ;; Set clang-server-cflags after changing major mode.
        ;; Because, clang-server-cflags is updated by major-mode-hooks.
        (setq clang-server-cflags cflags)))))


(defun ac-clang-jump-inclusion ()
  (interactive)

  (ac-clang-mode--on)

  (clang-server-request-transaction #'clang-server-send-inclusion-command #'ac-clang--receive-jump nil))


(defun ac-clang-jump-definition ()
  (interactive)

  (ac-clang-mode--on)

  (clang-server-request-transaction #'clang-server-send-definition-command #'ac-clang--receive-jump nil))


(defun ac-clang-jump-declaration ()
  (interactive)

  (ac-clang-mode--on)

  (clang-server-request-transaction #'clang-server-send-declaration-command #'ac-clang--receive-jump nil))


(defun ac-clang-jump-smart ()
  (interactive)

  (ac-clang-mode--on)

  (clang-server-request-transaction #'clang-server-send-smart-jump-command #'ac-clang--receive-jump nil))




;;;
;;; The session control functions
;;;

(defun ac-clang-activate ()
  (interactive)

  (remove-hook 'first-change-hook #'ac-clang-activate t)

  (when (clang-server-activate-session)
    (setq ac-clang--ac-sources-backup ac-sources)
    (setq ac-sources '(ac-source-clang-async))

    (add-hook 'before-revert-hook #'ac-clang-deactivate nil t)
    ;; ac-clang-activate don't add to after-revert-hook.
    ;; because it will call from c-mode-common-hook.
    ;; sequence is revert-buffer -> c-mode-common-hook.
    (add-hook 'kill-buffer-hook #'ac-clang-deactivate nil t)

    (add-hook 'yas-before-expand-snippet-hook #'ac-clang--enter-snippet-expand nil t)
    (add-hook 'yas-after-exit-snippet-hook #'ac-clang--leave-snippet-expand nil t)
    t))


(defun ac-clang-deactivate ()
  (interactive)

  (when (clang-server-deactivate-session)
    (remove-hook 'before-revert-hook #'ac-clang-deactivate t)
    (remove-hook 'kill-buffer-hook #'ac-clang-deactivate t)

    (remove-hook 'yas-before-expand-snippet-hook #'ac-clang--enter-snippet-expand t)
    (remove-hook 'yas-after-exit-snippet-hook #'ac-clang--leave-snippet-expand t)

    (setq ac-sources ac-clang--ac-sources-backup)
    (setq ac-clang--ac-sources-backup nil)
    t))


(defun ac-clang-activate-after-modify ()
  (interactive)

  (if (buffer-modified-p)
      (ac-clang-mode)
    (add-hook 'first-change-hook #'ac-clang-mode nil t)))


(defsubst ac-clang--enter-snippet-expand ()
  (setq ac-clang--snippet-expanding-p t))


(defsubst ac-clang--leave-snippet-expand ()
  (setq ac-clang--snippet-expanding-p nil))


(defalias 'ac-clang-reparse-buffer 'clang-server-reparse-buffer)


(defalias 'ac-clang-update-cflags 'clang-server-update-cflags)


(defalias 'ac-clang-set-cflags 'clang-server-set-cflags)


(defalias 'ac-clang-set-cflags-from-shell-command 'clang-server-set-cflags-from-shell-command)




;;;
;;; The server control functions
;;;

(defalias 'ac-clang-update-clang-parameters 'clang-server-update-clang-parameters)


(defalias 'ac-clang-reset-server 'clang-server-reset)


(defalias 'ac-clang-reboot-server 'clang-server-reboot)




(define-minor-mode ac-clang-mode
  "AutoComplete extension ClangAssist mode"
  :lighter " ClangAssist"
  :keymap ac-clang--mode-key-map
  :group 'ac-clang
  (if ac-clang-mode
      (ac-clang-activate)
    (ac-clang-deactivate)))


(defsubst ac-clang-mode--on ()
  (ac-clang-mode 1))

(defsubst ac-clang-mode--off ()
  (ac-clang-mode 0))




(defun ac-clang-initialize ()
  (interactive)

  (when (clang-server-initialize)
    ;; Change popup package used for auto-complete
    (setq ac-quick-help-prefer-pos-tip ac-clang-quick-help-prefer-pos-tip-p)

    ;; Optional keybindings
    (define-key ac-mode-map (kbd "M-.") #'ac-clang-jump-smart)
    (define-key ac-mode-map (kbd "M-,") #'ac-clang-jump-back)
    ;; (define-key ac-mode-map (kbd "C-c `") #'ac-clang-diagnostics))

    (defadvice flymake-on-timer-event (around ac-clang--flymake-suspend-advice last activate)
      (unless ac-clang--snippet-expanding-p
        ad-do-it))

    (add-hook 'clang-server-session-establishing-buffers-finalize-hooks #'ac-clang-mode--off)
    (add-hook 'kill-emacs-hook #'ac-clang-finalize)

    t))


(defun ac-clang-finalize ()
  (interactive)

  ;; (message "ac-clang-finalize")
  (when (clang-server-finalize)
    (define-key ac-mode-map (kbd "M-.") nil)
    (define-key ac-mode-map (kbd "M-,") nil)
    ;; (define-key ac-mode-map (kbd "C-c `") nil)

    (ad-disable-advice 'flymake-on-timer-event 'around 'ac-clang--flymake-suspend-advice)

    t))





(provide 'ac-clang)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ac-clang.el ends here
