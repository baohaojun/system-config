;;; evcxr.el --- Evcxr minor mode for Rust Repl support

;; Copyright (C) 2018 Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>
;; Created: 26 Sep 2018
;; Version: 0.0.1
;; Keywords: rust languages repl
;; URL: https://github.com/serialdev/evcxr-mode
;; Package-Requires: ((emacs "24.3", parsec ))
;;; Commentary:
;; Rust Repl support through evcxr repl

(require 'comint)
(require 'parsec)

;; Parsing

(defun evcxr-parse-title ()
  (parsec-collect*
   (parsec-optional* (parsec-str "["))
   (parsec-many-as-string (parsec-letter))
   (parsec-optional* (parsec-str "]"))
   (parsec-optional* (parsec-option (parsec-newline) (parsec-eof)))))


(defun evcxr-parse-l ()
  (parsec-collect*
   (parsec-many-as-string (parsec-option (parsec-optional* (parsec-str " "))
					 (parsec-or (parsec-letter)
						    (parsec-str "\"")
						    (parsec-digit)
						    (parsec-str "-")
						    (parsec-str "[")
						    (parsec-str "]")
						    (parsec-str ".")
						    (parsec-str "_"))))
   (parsec-optional* (parsec-lookahead "="))))

(defun evcxr-parse-r ()
  (parsec-collect*
   (parsec-optional* (parsec-str "="))
   (parsec-many-as-string (parsec-option (parsec-optional* (parsec-str " "))
					 (parsec-or (parsec-letter)
						    (parsec-str "\"")
						    (parsec-digit)
						    (parsec-str "-")
						    (parsec-str "[")
						    (parsec-str "]")
						    (parsec-str ".")
						    (parsec-str "_"))))
   (parsec-optional* (parsec-option (parsec-newline) (parsec-eof)))))


(defun evcxr-parse-toml (input)
  (parsec-with-input input
    (parsec-many
     (parsec-collect*
      (car (evcxr-parse-title))
      (parsec-many
       (parsec-collect
	(car(evcxr-parse-l))
	(car(evcxr-parse-r))))
      (parsec-optional* (parsec-option (parsec-newline) (parsec-eof)))))))


;; Usage


(defun evcxr--load-cargo(list)
  (when list
    (evcxr-check-header (car list))
    (evcxr--load-cargo (cdr list))))

(defun evcxr-check-header (list)
  (when list
    (if (equal "dependencies" (car list))
	(evcxr--install-cargo (car(cdr list)))
      )))

(defun evcxr--install-cargo (list)
  (when list
    (evcxr--add-dep (car (car list)) (car(cdr(car list))))
    (evcxr--install-cargo (cdr list))))

(defun evcxr--add-dep(dep version)
  (let ((dependency (concat ":dep " (format "%s" dep) " = " (format "%s" version) )))
    (progn
      (comint-send-string evcxr-shell-buffer-name dependency)
      (comint-send-string evcxr-shell-buffer-name "\n")
      (print (concat "Dependency " dependency " added "))
      )
    ))

(defun evcxr-load-cargo()
  (interactive)
  (evcxr--load-cargo (evcxr-get-cargo-file)))


(defun evcxr-is-running? ()
  "Return non-nil if evcxr is running."
  (comint-check-proc evcxr-shell-buffer-name))
(defalias 'evcxr-is-running-p #'evcxr-is-running?)


;;;###autoload
(defun evcxr (&optional arg)
  "Run evcxr.
Unless ARG is non-nil, switch to the buffer."
  (interactive "P")
  (let ((buffer (get-buffer-create evcxr-shell-buffer-name)))
    (unless arg
      (pop-to-buffer buffer))
    (unless (evcxr-is-running?)
      (with-current-buffer buffer
        (evcxr-startup)
        (inferior-evcxr-mode)
	)
      (pop-to-buffer buffer)
      (other-window -1)
      )
    ;; (with-current-buffer buffer (inferior-evcxr-mode))
    buffer))


;;;###autoload
(defalias 'run-rust #'evcxr)
;;;###autoload
(defalias 'inferior-rust #'evcxr)

(defun evcxr-startup ()
  "Start evcxr."
  (comint-exec evcxr-shell-buffer-name
               "evcxr"
               evcxr-program
               (when (file-exists-p evcxr-startup-file)
                 evcxr-startup-file)
               evcxr-args))

(defun evcxr-eval-region (begin end)
  "Evaluate region between BEGIN and END."
  (interactive "r")
  (evcxr t)
  (comint-send-string evcxr-shell-buffer-name
    (message "%s" (replace-regexp-in-string "\n[[:space:]]?" " "(buffer-substring-no-properties begin end))))
  (comint-send-string evcxr-shell-buffer-name "\n"))

(defun evcxr-type-check ()
  (interactive)
  (comint-send-string evcxr-shell-buffer-name (concat "let evcxrmodetype: () = " (thing-at-point 'symbol) ";"))
  (comint-send-string evcxr-shell-buffer-name "\n")
  )

(defun evcxr-type-check-in-container ()
  (interactive)
  (comint-send-string evcxr-shell-buffer-name (concat "let evcxrmodetype: () = " (thing-at-point 'symbol) "[0];"))
  (comint-send-string evcxr-shell-buffer-name "\n")
  )

(defun evcxr-add-dep(dep version)
  (interactive "sDependency name:
sDependency version: ")
  (let ((dependency (concat ":dep " dep " = " "\"" version "\"")))
  (progn
    (comint-send-string evcxr-shell-buffer-name dependency)
    (comint-send-string evcxr-shell-buffer-name "\n"))
    (print (concat "Dependency " dependency " added "))))



(defun evcxr-parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun evcxr-find-file-in-hierarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((file (concat current-dir fname))
        (parent (evcxr-parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (evcxr-find-file-in-hierarchy parent fname)))))


(defun evcxr-get-string-from-file (filePath)
  "Return filePath's file content.
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02
"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defun evcxr-get-cargo-file()
  (interactive)
  (let ((cargo-file (evcxr-get-string-from-file
		   (evcxr-find-file-in-hierarchy (file-name-directory buffer-file-name) "Cargo.toml"))))
      (evcxr-parse-toml cargo-file)))


(defun evcxr-help-bound-vars ()
  (interactive)
  (comint-send-string evcxr-shell-buffer-name ":vars")
  (comint-send-string evcxr-shell-buffer-name "\n"))

(defun evcxr-help-clear-state ()
  (interactive)
  (comint-send-string evcxr-shell-buffer-name ":clear")
  (comint-send-string evcxr-shell-buffer-name "\n"))

(defun evcxr-help-toggle-optimization ()
  (interactive)
  (comint-send-string evcxr-shell-buffer-name ":opt")
  (comint-send-string evcxr-shell-buffer-name "\n"))

(defun evcxr-help-explain-error ()
  (interactive)
  (comint-send-string evcxr-shell-buffer-name ":explain")
  (comint-send-string evcxr-shell-buffer-name "\n"))


(defun evcxr-eval-buffer ()
  "Evaluate complete buffer."
  (interactive)
  (evcxr-eval-region (point-min) (point-max)))

(defun evcxr-eval-line (&optional arg)
  "Evaluate current line.
If ARG is a positive prefix then evaluate ARG number of lines starting with the
current one."
  (interactive "P")
  (unless arg
    (setq arg 1))
  (when (> arg 0)
    (evcxr-eval-region
     (line-beginning-position)
     (line-end-position arg))))


;;; Shell integration

(defcustom evcxr-shell-buffer-name "*Evcxr*"
  "Name of buffer for evcxr."
  :group 'evcxr
  :type 'string)

(defcustom evcxr-shell-interpreter "evcxr"
  "default repl for shell"
  :type 'string
  :group 'evcxr)

(defcustom evcxr-shell-internal-buffer-name "Evcxr Internal"
  "Default buffer name for the internal process"
  :type 'string
  :group 'python
  :safe 'stringp)


(defcustom evcxr-shell-prompt-regexp ">> "
  "Regexp to match prompts for evcxr.
   Matchint top\-level input prompt"
  :group 'evcxr
  :type 'regexp
  :safe 'stringp)

(defcustom evcxr-shell-prompt-block-regexp " "
  "Regular expression matching block input prompt"
  :type 'string
  :group 'evcxr
  :safe 'stringp)

(defcustom evcxr-shell-prompt-output-regexp ""
  "Regular Expression matching output prompt of evxcr"
  :type 'string
  :group 'evcxr
  :safe 'stringp)

(defcustom evcxr-shell-enable-font-lock t
  "Should syntax highlighting be enabled in the evcxr shell buffer?"
  :type 'boolean
  :group 'evcxr
  :safe 'booleanp)

(defcustom evcxr-shell-compilation-regexp-alist '(("[[:space:]]\\^+?"))
  "Compilation regexp alist for inferior evcxr"
  :type '(alist string))

(defgroup evcxr nil
  "Rust interactive mode"
  :link '(url-link "https://github.com/serialdev/evcxr-mode")
  :prefix "evcxr"
  :group 'languages)

(defcustom evcxr-program (executable-find "evcxr")
  "Program invoked by `evcxr'."
  :group 'evcxr
  :type 'file)


(defcustom evcxr-args nil
  "Command line arguments for `evcxr-program'."
  :group 'evcxr
  :type '(repeat string))


(defcustom evcxr-startup-file (locate-user-emacs-file "init_evcxr.rs"
                                                      ".emacs-evcxr.rs")
  "Startup file for `evcxr'."
  :group 'evcxr
  :type 'file)


(defcustom evcxr-prompt-read-only t
  "Make the prompt read only.
See `comint-prompt-read-only' for details."
  :group 'evcxr
  :type 'boolean)

(defun evcxr-comint-output-filter-function (output)
  "Hook run after content is put into comint buffer.
   OUTPUT is a string with the contents of the buffer"
  (ansi-color-filter-apply output))


(defun evcxr--cargo-doc-tree()
  "Get the mccabe complexity for this buffer."
  (interactive)
  (message
   (shell-command-to-string(message "tree -d %starget/doc -L 1 " (projectile-project-root)))))


(define-derived-mode inferior-evcxr-mode comint-mode "Evcxr"
  (setq comint-process-echoes t)
  ;; (setq comint-prompt-regexp (format "^\\(?:%s\\|%s\\)"
  ;; 				     evcxr-shell-prompt-regexp
  ;; 				     evcxr-shell-prompt-block-regexp))
  (setq comint-prompt-regexp "\\>")

  (setq mode-line-process '(":%s"))
  (make-local-variable 'comint-output-filter-functions)
  (add-hook 'comint-output-filter-functions
  	    'evcxr-comint-output-filter-function)
  (set (make-local-variable 'compilation-error-regexp-alist)
       evcxr-shell-compilation-regexp-alist)
  (setq comint-use-prompt-regexp t)
  (setq comint-inhibit-carriage-motion nil)
  (setq-local comint-prompt-read-only evcxr-prompt-read-only)
  (when evcxr-shell-enable-font-lock
    (set-syntax-table rust-mode-syntax-table)
    (set (make-local-variable 'font-lock-defaults)
	 '(rust-mode-font-lock-keywords nil nil nil nil))
    (set (make-local-variable 'syntax-propertize-function)
    	 (eval
    	  "Unfortunately eval is needed to make use of the dynamic value of comint-prompt-regexp"
    	  '(syntax-propertize-rules
    	    '(comint-prompt-regexp
    	       (0 (ignore
    		   (put-text-property
    		    comint-last-input-start end 'syntax-table
    		    python-shell-output-syntax-table)
    		   (font-lock-unfontify--region comint-last-input-start end))))
    	    )))
    (compilation-shell-minor-mode 1)))

(if (require 'hydra nil 'noerror)
    (defhydra evcxr-hydra- (:color pink
					  :hint nil)
      "
											       ╭──────┐
     Repl              ^ ^        Types      ^ ^       Info        ^ ^                               │*Evcxr│
    ╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
    ^^
    _C-r_: Eval Region        _C-t_: Type Check        _C-o_: Toggle optimization
    _C-l_: Eval line          _C-i_: Type in container _C-s_: Clear State
    _C-b_: Eval Buffer            ^ ^	               _C-e_: Explain Error
    _C-v_: Check bound vars       ^ ^       	       _C-;_: Load Cargo
    _C-p_: Start Repl         _C-d_: Add Dependency    _C-c_: Cargo hydra

    "
      ("C-r" evcxr-eval-region :color pink)
      ("C-l" evcxr-eval-line :color pink)
      ("C-b" evcxr-eval-buffer :color blue)
      ("C-v" evcxr-help-bound-vars :color pink)

      ("C-t" evcxr-type-check :color pink)
      ("C-i" evcxr-type-check-in-container :color pink)

      ("C-o" evcxr-help-toggle-optimization :color blue)
      ("C-s" evcxr-help-clear-state :color blue)
      ("C-e" evcxr-help-explain-error :color blue)

      ("C-p" evcxr :color pink)
      ("C-d" evcxr-add-dep :color pink)
      ("C-;" evcxr-load-cargo :color blue)
      ("C-c" hydra-rust/body :color blue)

      ("ESC" nil "Exit")
      ("q" nil "Exit")
      )
  (message "Hydra keymap disabled since no hydras installed"))

(if (require 'hydra nil 'noerror)
    (defhydra hydra-rust (:color pink :hint nil)
      "
    ^Rust Cargo commands^
    ----------------------------------------------------------------------------------
    _r_: Run          _i_: Init          _u_: Update               _+r_: Release O
    _x_: Run-example  _n_: New           _c_: Repeat               _+b_: Build O
    _b_: Build        _f_: Current-test  _e_: Bench
    _l_: Clean        _s_: Search        _o_: Current-file-tests
    _d_: Doc          _t_: Test          _m_: Fmt
    _|_: Doc Tree     _k_: Check         _q_: Clippy
    "
      ("e"   cargo-process-bench :color blue)
      ("b"   cargo-process-build :color blue)
      ("l"   cargo-process-clean :color blue)
      ("d"   cargo-process-doc :color blue)
      ("n"   cargo-process-new :color blue)
      ("i"   cargo-process-init :color blue)
      ("r"   cargo-process-run :color blue)
      ("x"   cargo-process-run-example :color blue)
      ("s"   cargo-process-search :color blue)
      ("t"   cargo-process-test :color blue)
      ("u"   cargo-process-update :color blue)
      ("c"   cargo-process-repeat :color blue)
      ("f"   cargo-process-current-test :color blue)
      ("o"   cargo-process-current-file-tests :color blue)
      ("m"   cargo-process-fmt :color blue)
      ("+r" cargo-process-run-optimized :color blue)
      ("+b" cargo-process-build-optimized :color blue)
      ("|"   evcxr--cargo-doc-tree :color blue)
      ("k"   cargo-process-check color: red)
      ("q" cargo-process-clippy :color blue)
      ("ESC" nil "Exit"))
  (message "Hydra cargo keymap disabled since no hydras installed"))


; ------------------------------------------------------------------------- ;
;                                Keybindings                                ;
; ------------------------------------------------------------------------- ;

(define-key rust-mode-map (kbd "C-c C-c") #'evcxr-eval-buffer)
(define-key rust-mode-map (kbd "C-c C-d") #'evcxr-add-dep)
(define-key rust-mode-map (kbd "C-c C-r") #'evcxr-eval-region)
(define-key rust-mode-map (kbd "C-c C-l") #'evcxr-eval-line)
(define-key rust-mode-map (kbd "C-c C-t") #'evcxr-type-check)
(define-key rust-mode-map (kbd "C-c C-i") #'evcxr-type-check-in-container)
(define-key rust-mode-map (kbd "C-c C-o") #'evcxr-help-toggle-optimization)
(define-key rust-mode-map (kbd "C-c C-s") #'evcxr-help-clear-state)
(define-key rust-mode-map (kbd "C-c C-v") #'evcxr-help-bound-vars)
(define-key rust-mode-map (kbd "C-c C-e") #'evcxr-help-explain-error)
(define-key rust-mode-map (kbd "C-c C-p") #'evcxr)

(if (require 'hydra nil 'noerror)
    (define-key rust-mode-map (kbd "C-c C-c") #'evcxr-hydra-/body)
  )


(provide 'evcxr)

;;; evcxr.el ends here
