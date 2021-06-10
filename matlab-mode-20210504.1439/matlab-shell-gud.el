;;; matlab-shell-gud.el --- GUD support in matlab-shell.
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <eludlam@emacsvm>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; GUD (grand unified debugger) support for MATLAB shell.
;;
;; Includes setting up gud mode in the shell, and all filters, etc specific
;; to supporting gud.

(require 'matlab-shell)

(eval-and-compile
  (require 'gud)
  (require 'eieio)
  )

;;; Code:
(defcustom matlab-shell-debug-tooltips-p nil
  "*Enable tooltips displaying data values when at the K>> prompt.
Disable this option if the tooltips are too slow in your setup."
  :group 'matlab-shell
  :type 'boolean)

(defvar gud-matlab-debug-active nil
  "Non-nil if MATLAB has a K>> prompt up.")
(defvar gud-matlab-debug-activate-hook nil
  "Hooks run when MATLAB detects a K>> prompt after a >> prompt.")
(defvar gud-matlab-debug-deactivate-hook nil
  "Hooks run when MATLAB detects a >> prompt after a K>> prompt.")

(defvar gud-matlab-tool-bar-map nil
  "Toolbar keymap used when in MATLAB debug mode.")

(declare-function matlab-netshell-eval "matlab-netshell" (mode))

(defmacro matlab-at-fcn (cmd)
  "Define CMD to be a GUD command that works w/ shell or netshell."
  ;; Note `arg' comes from gud-def declaration
  `(if (matlab-shell-active-p)
       (gud-call (concat ,cmd "%%") arg)
     (if (matlab-netshell-active-p)
	 (matlab-netshell-eval (gud-format-command ,cmd arg))
       (error "No MATLAB shell active"))))

(defmacro matlab-gud-fcn (cmd)
  "Define CMD forms to be sent to a MATLAB shell."
  ;; Note `arg' comes from gud-def declaration
  `(if gud-matlab-debug-active
       (matlab-at-fcn ,cmd)
     (error "MATLAB debugging not active")))

;;;###autoload
(defun matlab-shell-mode-gud-enable-bindings ()
  "Enable GUD features for `matlab-shell' in the current buffer."

  ;; Make sure this is safe to use gud to debug MATLAB
  (when (not (fboundp 'gud-def))
    (error "Your Emacs is missing `gud-def' which means matlab-shell won't work correctly.  Stopping"))

  (gud-def gud-break  (matlab-at-fcn "ebstop in %d%f at %l")  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove (matlab-at-fcn "ebclear in %d%f at %l") "\C-x" "Remove breakpoint at current line.")
  (gud-def gud-step   (matlab-gud-fcn "dbstep in")   "\C-s" "Step one source line, possibly into a function.")
  (gud-def gud-next   (matlab-gud-fcn "dbstep %p")   "\C-n" "Step over one source line.")
  (gud-def gud-cont   (matlab-gud-fcn "dbcont")      "\C-r" "Continue with display.")
  (gud-def gud-stop-subjob (matlab-gud-fcn "dbquit") "\C-q" "Quit debugging.") ;; gud toolbar stop
  (gud-def gud-finish (matlab-gud-fcn "dbstep out")  "\C-f" "Finish executing current function.")
  (gud-def gud-up     (matlab-gud-fcn "dbup")        "<"    "Up N stack frames (numeric arg).")
  (gud-def gud-down   (matlab-gud-fcn "dbdown")      ">"    "Down N stack frames (numeric arg).")
  (gud-def gud-list-breakpoints (matlab-at-fcn "ebstatus")  "\C-v"    "List breakpoints")
  (gud-def gud-show-stack       (matlab-at-fcn "ebstack")   "\C-w"    "Show stack")
  ;; using (gud-def gud-print  "%e" "\C-p" "Eval expression at point") fails
  ;; (gud-def gud-print  "% gud-print not available" "\C-p" "gud-print not available.")

  (when window-system
    (matlab-frame-init)

    (setq gud-matlab-tool-bar-map
	  (let ((map (make-sparse-keymap)))
	    (dolist (x '((gud-break . "gud/break")
			 (gud-remove . "gud/remove")
			 (gud-cont . "gud/cont")
			 (gud-next . "gud/next")
			 (gud-step . "gud/step")
			 (gud-finish . "gud/finish")
			 (gud-stop-subjob . "gud/stop")
			 (mlg-show-stack . "gud/all")
			 (gud-list-breakpoints . "describe")
			 ))
	      (tool-bar-local-item-from-menu
	       (car x) (cdr x) map matlab-mode-map))
	    map))

    )
  
  (if (fboundp 'gud-make-debug-menu)
      (gud-make-debug-menu))

  (when (boundp 'tool-bar-map)            ; not --without-x
    (kill-local-variable 'tool-bar-map))
  )

;;;###autoload
(defun matlab-shell-gud-startup ()
  "Configure GUD when a new `matlab-shell' is initialized."
  (gud-mode)

  ;; type of gud mode
  (setq gud-minor-mode 'matlab)
  
  ;; This starts us supporting gud tooltips.
  (add-to-list 'gud-tooltip-modes 'matlab-mode)
  
  (make-local-variable 'gud-marker-filter)
  (setq gud-marker-filter 'gud-matlab-marker-filter)
  (make-local-variable 'gud-find-file)
  (setq gud-find-file 'gud-matlab-find-file)

  ;; XEmacs doesn't seem to have this concept already.  Oh well.
  (make-local-variable 'gud-marker-acc)
  (setq gud-marker-acc nil)

  ;; Setup our debug tracker.
  (add-hook 'matlab-shell-prompt-appears-hook #'gud-matlab-debug-tracker)
  
  (gud-set-buffer))

;;; GUD Functions
(defun gud-matlab-massage-args (file args)
  "Argument message for starting matlab file.
I don't think I have to do anything, but I'm not sure.
FILE is ignored, and ARGS is returned."
  args)

(defun gud-matlab-find-file (f)
  "Find file F when debugging frames in MATLAB."
  (save-excursion
    (let* ((realfname (if (string-match "\\.\\(p\\)$" f)
			  (progn
			    (aset f (match-beginning 1) ?m)
			    f)
			f))
	   (buf (find-file-noselect realfname t)))
      (set-buffer buf)
      (if (fboundp 'gud-make-debug-menu)
	  (gud-make-debug-menu))
      buf)))


;;; GUD Filter Function
;;
;; MATLAB's process filter handles output from the MATLAB process and
;; interprets it for formatting text, and for running the debugger.

(defvar gud-matlab-marker-regexp-plain-prompt "^K?>>"
  "Regular expression for finding a prompt.")

(defvar gud-matlab-marker-regexp-K>> "^K>>"
  "Regular expression for finding a file line-number.")
(defvar gud-matlab-marker-regexp->> "^>>"
  "Regular expression for finding a file line-number.")

(defvar gud-matlab-dbhotlink nil
  "Track if we've sent a dbhotlink request.")
(make-variable-buffer-local 'gud-matlab-dbhotlink)

(defun gud-matlab-marker-filter (string)
  "Filters STRING for the Unified Debugger based on MATLAB output."

  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output "") (frame nil))

    ;; ERROR DELIMITERS
    ;; Newer MATLAB's wrap error text in {^H  }^H characters.
    ;; Convert into something COMINT won't delete so we can scan them.
    (while (string-match "{" gud-marker-acc)
      (setq gud-marker-acc (replace-match matlab-shell-errortext-start-text t t gud-marker-acc 0)))

    (while (string-match "}" gud-marker-acc)
      (setq gud-marker-acc (replace-match matlab-shell-errortext-end-text t t gud-marker-acc 0)))
    
    ;; DEBUG PROMPTS
    (when (string-match gud-matlab-marker-regexp-K>> gud-marker-acc)

      ;; Newer MATLAB's don't print useful info.  We'll have to
      ;; search backward for the previous line to see if a frame was
      ;; displayed.
      (when (and (not frame) (not gud-matlab-dbhotlink))
	(let ((dbhlcmd (if matlab-shell-echoes
			   "dbhotlink()%%%\n"
			 ;; If no echo, force an echo
			 "disp(['dbhotlink()%%%' newline]);dbhotlink();\n")))
	  ;;(when matlab-shell-io-testing (message "!!> [%s]" dbhlcmd))
	  (process-send-string (get-buffer-process gud-comint-buffer) dbhlcmd)
	  )
	(setq gud-matlab-dbhotlink t)
	)
      )

    ;; If we're forced to ask for a stack hotlink, we will see it come in via the
    ;; process output.  Don't output anything until a K prompt is seen after the display
    ;; of the dbhotlink command.
    (when gud-matlab-dbhotlink
      (let ((start (string-match "dbhotlink()%%%" gud-marker-acc))
	    (endprompt nil))
	(if start
	    (progn
	      (setq output (substring gud-marker-acc 0 start)
		    gud-marker-acc (substring gud-marker-acc start))

	      ;; The hotlink text will persist until we see the K prompt.
	      (when (string-match gud-matlab-marker-regexp-plain-prompt gud-marker-acc)
		(setq endprompt (match-end 0))

		;; (when matlab-shell-io-testing (message "!!xx [%s]" (substring gud-marker-acc 0 endprompt)))

		;; We're done with the text!
		;; Capture the text that describes the new stack frame.
		(save-match-data
		  (let* ((expr-end (match-beginning 0))
			 (m1 (string-match "dbhotlink()%%%\n" gud-marker-acc))
			 (expr-start (match-end 0))
			 (expression (substring gud-marker-acc expr-start expr-end)))

		    (when (> (length expression) 0)
		      (condition-case ERR
			  (let ((forms (read expression)))
			    (when forms
			      ;;(message "About to evaluate forms: \"%S\"" forms)
			      (eval forms)))
			(error
			 (message "Failed to evaluate dbhotlink expression: \"%s\"" expression)
			 (message "Error is: %S" ERR)
			 )
			))
		    ))

		;;Remove it from the accumulator.
		(setq gud-marker-acc (substring gud-marker-acc endprompt))
		;; If we got all this at the same time, push output back onto the accumulator for
		;; the next code bit to push it out.
		(setq gud-marker-acc (concat output gud-marker-acc)
		      output ""
		      gud-matlab-dbhotlink nil)
		))
	  ;; Else, waiting for a link, but hasn't shown up yet.
	  ;; TODO - what can I do here to fix var setting if it gets
	  ;; locked?
	  (when (string-match gud-matlab-marker-regexp->> gud-marker-acc)
	    ;; A non-k prompt showed up.  We're not going to get out request.
	    (setq gud-matlab-dbhotlink nil))
	  )))
    
    ;; This if makes sure that the entirety of an error output is brought in
    ;; so that matlab-shell-mode doesn't try to display a file that only partially
    ;; exists in the buffer.  Thus, if MATLAB output:
    ;;  error: /home/me/my/mo/mello.m,10,12
    ;; All of that is in the buffer, and it goes to mello.m, not just
    ;; the first half of that file name.
    ;; The below used to match against the prompt, not \n, but then text that
    ;; had error: in it for some other reason wouldn't display at all.
    (if (and matlab-prompt-seen ;; don't pause output if prompt not seen
	     gud-matlab-dbhotlink ;; pause output if waiting on debugger
	     )
	;; We could be collecting debug info.  Wait before output.
	nil
      ;; Finish off this part of the output.  None of our special stuff
      ;; ends with a \n, so display those as they show up...
      (while (string-match "^[^\n]*\n" gud-marker-acc)
	(setq output (concat output (substring gud-marker-acc 0 (match-end 0)))
	      gud-marker-acc (substring gud-marker-acc (match-end 0))))

      (if (string-match (concat gud-matlab-marker-regexp-plain-prompt "\\s-*$") gud-marker-acc)
	  (setq output (concat output gud-marker-acc)
		gud-marker-acc ""))
      
      ;; Check our output for a prompt, and existence of a frame.
      ;; If this is true, throw out the debug arrow stuff.
      (if (and (string-match (concat gud-matlab-marker-regexp->> "\\s-*$") output)
	       gud-last-last-frame)
	  (progn
	    ;; Clean up gud stuff.
	    (setq overlay-arrow-position nil
		  gud-last-last-frame nil
		  gud-overlay-arrow-position nil)
	    ;; If stack is showing, clean it up.
	    (let* ((buff (mlg-set-stack nil))
		   (win (get-buffer-window buff)))
	      (when win
		(select-window win)
		(mlg-stack-quit)
		))
	    ;; Refresh stuff
	    (sit-for 0)
	    ))

      ;; Check for any text that would be embarrassing to display partially.
      ;; If we don't see any, feel free to dump the rest of the accumulation buffer
      (unless (or (string-match (regexp-quote "<a href=") gud-marker-acc)
		  (string-match (regexp-quote "<EMACSCAP") gud-marker-acc)
		  (string-match (regexp-quote "<ERROR") gud-marker-acc))
	(setq output (concat output gud-marker-acc)
	      gud-marker-acc "")
	)
      
      )

    (if frame (setq gud-last-frame frame))

    (when matlab-shell-io-testing
      (message "-->[%s] [%s]" output gud-marker-acc))

    ;;(message "Looking for prompt in %S" output)
    (when (and (not matlab-shell-suppress-prompt-hooks)
	       (string-match gud-matlab-marker-regexp-plain-prompt output))
      ;; Now that we are about to dump this, run our prompt hook.
      ;;(message "PROMPT!")
      (setq matlab-shell-prompt-hook-cookie t))
    
    output))

;;; Stack tracking
;;
(defclass mlg-stack-frame ()
  ((file :initarg :file
	 :type string
	 :documentation
	 "The filename this frame belongs to.")
   (name :initarg :name
	 :type string
	 :documentation
	 "The name of the location of this frame")
   (line :initarg :line
	 :type integer
	 :documentation
	 "The line number for this frame"))
  "A single stack frame from MATLAB.")

(cl-defmethod mlg-print ((frame mlg-stack-frame) longestname)
  "Use print to output this stack FRAME.
LONGESTNAME specifies the how long the longest name we can expect is."
  (let* ((namefmt (concat "%" (number-to-string (or longestname 10)) "s"))
	 (str (concat (propertize (format namefmt (oref frame name)) 'face 'font-lock-function-name-face)
		      " "
	              (propertize (format "%3d" (oref frame line)) 'face 'bold)
		      " "
		      (propertize (oref frame file) 'face 'font-lock-constant-face))))
    (setq str (propertize str 'object frame))
    str))

(defvar mlg-stack nil
  "The last stack sent to us from MATLAB.")
(defvar mlg-frame nil
  "The last frame sent to use from MATLAB.")

(defun mlg-set-stack (newstack)
  "Specify a NEWSTACK provided by MATLAB to replace the old one."
  (setq mlg-stack nil)
  (dolist (L newstack)
    (push (mlg-stack-frame ""
	   :file (nth 0 L)
	   :name (nth 1 L)
	   :line (nth 2 L))
	  mlg-stack))
  (setq mlg-stack (nreverse mlg-stack))
  (mlg-refresh-stack-buffer)
  ;;(message "Updated Stack")
  )

(defun mlg-set-stack-frame (newframe)
  "Specify a NEWFRAME provided by MATLAB we should visit."
  (setq mlg-frame newframe)
  (mlg-show-stack)
  (mlg-show-frame newframe)
  )

(defun mlg-set-stack-frame-via-gud (newframe)
  "Specify a NEWFRAME provided by MATLAB we should visit."
  (setq mlg-frame newframe)
  (let ((file (oref (nth (1- newframe) mlg-stack) file))
	(line (oref (nth (1- newframe) mlg-stack) line)))
    (if (< line 0) (setq line (- line)))
    (setq gud-last-frame (cons file line))
    ;;(message "Gud FRAME set to %S" gud-last-frame)
    )
  )

(defun mlg-show-frame (&optional frame)
  "Setup windows to show FRAME from the current stack frame."
  (let ((newframe (or frame mlg-frame)))
    (if (and mlg-stack (<= newframe (length mlg-stack)))
	;; Make sure we have a stack window.
	(let* ((buff (get-buffer "*MATLAB stack*"))
	       (win (get-buffer-window buff)))
	  (if (or (not buff) (not win))
	      (mlg-show-stack)
	    ;; else, do refresh stuff.
	    (select-window win))
	  
	  ;; Still around, go do it.
	  (goto-char (point-min))
	  (forward-line (1- frame))
	  (mlg-stack-choose)
	  )
      ;; Else no frame.  Look for the window, and close it.
      (let* ((buff (get-buffer "*MATLAB stack*"))
	     (win (get-buffer-window buff)))

	(when win (delete-window win)))
      )))

(defun mlg-refresh-stack-buffer ()
  "Refresh the buffer displaying stack."
  (save-excursion
    (let ((buff (get-buffer-create "*MATLAB stack*"))
	  (namelen 5)
	  (inhibit-read-only t))

      (dolist (S mlg-stack)
	(when (> (length (oref S name)) namelen)
	  (setq namelen (length (oref S name)))))
    
      (set-buffer buff)
      (erase-buffer)

      (let ((cnt 1))
	(dolist (F mlg-stack)
	  (insert (format "%2d" cnt))
	  (if (and mlg-frame (= cnt mlg-frame))
	      (insert " >> ")
	    (insert " -- "))
	  (insert (mlg-print F namelen) "\n")
	  (setq cnt (1+ cnt))))

      (mlg-stack-mode)
      (goto-char (point-min))
      (current-buffer))))
  
(defun mlg-show-stack ()
  "Display the MATLAB stack in an interactive buffer."
  (interactive)
  (let ((buff (mlg-refresh-stack-buffer)))
  
    (display-buffer
     buff
     '((display-buffer-at-bottom)
       (inhibit-same-window . t)
       (window-height . fit-window-to-buffer))
     )

    (select-window (get-buffer-window buff))
    (goto-char 3)
    ))
  
(defvar mlg-stack-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'mlg-stack-choose)
    (define-key km "q" 'mlg-stack-quit)
    (define-key km "n" 'mlg-stack-next)
    (define-key km "p" 'mlg-stack-prev)
    (define-key km [mouse-2] 'mlg-stack-click)
    (define-key km [mouse-1] 'mlg-stack-click)
    km)
  "Keymap used in MATLAB stack mode.")

;; Need this to fix weird problem in define-derived-mode
(defvar mlg-stack-mode-syntax-table (make-syntax-table)
  "Syntax table used in `matlab-shell-help-mode'.")

(define-derived-mode mlg-stack-mode
  fundamental-mode "MStack"
  "Major mode for viewing a MATLAB stack.

Commands:
\\{mlg-stack-mode-map}"
  :syntax-table mlg-stack-mode-syntax-table
  (setq buffer-read-only t)
  )

(defun mlg-stack-quit ()
  "Quit the MATLAB stack view."
  (interactive)
  (if (= (length (window-list)) 1)
      (bury-buffer)
    (delete-window (selected-window))))

(defun mlg-stack-next ()
  "Visit stack on next line."
  (interactive)
  (forward-line 1)
  (forward-char 2)
  (mlg-stack-choose))

(defun mlg-stack-prev ()
  "Visit stack on next line."
  (interactive)
  (forward-line -1)
  (forward-char 2)
  (mlg-stack-choose))

(defun mlg-stack-click (e)
  "Click on a stack frame to visit it.
Must be bound to event E."
  (interactive "e")
  (mouse-set-point e)
  (mlg-stack-choose))

(defun mlg-stack-choose ()
  "Choose the stack the under the cursor.
Visit the file presented in that stack frame."
  (interactive)
  (let ((topic nil) (fun nil) (p (point)))
    (save-excursion
      (beginning-of-line)
      (forward-char 10)
      (let* ((sf (get-text-property (point) 'object))
	     (f (oref sf file))
	     (l (oref sf line))
	     (buff (find-file-noselect f t)))
	(display-buffer
	 buff
	 '((display-buffer-reuse-window display-buffer-use-some-window)
	   (inhibit-same-window . t))
	 )
	(let ((win (selected-window)))
	  (select-window (get-buffer-window buff))
	  (goto-char (point-min))
	  (forward-line (1- l))
	  (select-window win))
	))))

;;; Breakpoint Trackers
;;
(defclass mlg-breakpoint ()
  ((file :initarg :file
	 :type string
	 :documentation
	 "The filename this breakpoint belongs to.")
   (name :initarg :name
	 :type string
	 :documentation
	 "Name of the function this breakpoint is in.")
   (line :initarg :line
	 :type integer
	 :documentation
	 "The line number for this breakpoint")
   (overlay :documentation
	    :default nil
	    "The overlay indicating the presence of this breakpoint.")
   )
  "Representation of a breakpoint.
Used to track active breakpoints, and how to show them.")

(cl-defmethod mlg-print ((break mlg-breakpoint) longestname)
  "Use print to output this breakpoint BREAK.
LONGESTNAME specifies the how long the longest name we can expect is."
  (let* ((namefmt (concat "%" (number-to-string (or longestname 10)) "s"))
	 (str (concat (propertize (format namefmt (oref break name)) 'face 'font-lock-function-name-face)
		      " "
	              (propertize (format "%3d" (oref break line)) 'face 'bold)
		      " "
		      (propertize (oref break file) 'face 'font-lock-constant-face))))
    (setq str (propertize str 'object break))
    str))

(defvar matlab-gud-visible-breakpoints nil
  "List of breakpoints MATLAB has sent to us.")

;;;###autoload
(defun mlg-reset-breakpoints ()
  "Remove all cached breakpoints."
  (dolist (BP matlab-gud-visible-breakpoints)
    (mlg-deactivate BP))
  (setq matlab-gud-visible-breakpoints nil))

(defun mlg-add-breakpoint (file fcn line)
  "Add a visible breakpoint to FILE at LINE."
  (let ((found nil))
    (dolist (BP matlab-gud-visible-breakpoints)
      (when (and (string= (oref BP file) file)
		 (= (oref BP line) line))
	(setq found t)))
    (when (not found)
      (setq matlab-gud-visible-breakpoints
	    (cons (mlg-breakpoint "" :file file
				  :name fcn
				  :line line)
		  matlab-gud-visible-breakpoints))
      (mlg-activate (car matlab-gud-visible-breakpoints))
      ))
  ;; The first time breakpoints are added, make sure we can activate breakpoints
  ;; when new files are opened in a buffer.
  (add-hook 'matlab-mode-hook 'mlg-breakpoint-activate-buffer-opened-hook)
  )

(defun mlg-del-breakpoint (file fcn line)
  "Add a visible breakpoint to FILE at LINE."
  (let ((BPS matlab-gud-visible-breakpoints)
	(NBPS nil))
    (while BPS
      (if (and (string= (oref (car BPS) file) file)
	       (= (oref (car BPS) line) line))
	  ;; Deactivate
	  (mlg-deactivate (car BPS))
	;; Not being removed, add to list.
	(setq NBPS (cons (car BPS) NBPS)))
      (setq BPS (cdr BPS)))
    
    (setq matlab-gud-visible-breakpoints
	  (nreverse NBPS))))

(defface mlg-breakpoint-face
  (list
   (list t
	 (list :background nil
	       :foreground nil
	       :underline "red1")))
  "*Face to use to highlight breakpoints."
  :group 'matlab-shell)

(cl-defmethod mlg-activate ((bp mlg-breakpoint))
  "Activate breakpoint BP if needed."
  ;; yes overlay, but inactive
  (when (and (slot-boundp bp 'overlay)
	     (oref bp overlay)
	     (not (overlay-buffer (oref bp overlay))))
      (oset bp overlay nil))
    
    (let ((buff (find-buffer-visiting (oref bp file))))
      ;; No overlay, and we can make one.
      (when (and (or (not (slot-boundp bp 'overlay))
		     (not (oref bp overlay)))
		 buff)
	(with-current-buffer buff
	  (goto-char (point-min))
	  (forward-line (1- (oref bp line)))
	  (let ((ol (matlab-make-overlay (save-excursion
					   (back-to-indentation)
					   (point))
					 (point-at-eol) buff nil nil)))
	    ;; Store it
	    (oset bp overlay ol)
	    ;; Setup cool stuff
	    (matlab-overlay-put ol 'face 'mlg-breakpoint-face)
	    (matlab-overlay-put ol 'before-string
				(propertize "#"
					    'display
					    '(left-fringe
					      filled-square
					      matlab-shell-error-face))
				))))
      ))

(cl-defmethod mlg-deactivate ((bp mlg-breakpoint))
  "Deactivate this breakpoint BP."
  (when (slot-boundp bp 'overlay)
    (with-slots (overlay) bp
      (when (and overlay (overlayp overlay))
	(delete-overlay overlay)
	(setq overlay nil)))))

(defun mlg-breakpoint-activate-buffer-opened-hook ()
  "Activate any breakpoints in a buffer when that buffer is read in."
  (if (not (matlab-shell-active-p))
      (mlg-reset-breakpoints)

    ;; Still going, activate.
    (dolist (BP matlab-gud-visible-breakpoints)
      (mlg-activate BP)
      )))

(defun mlg-breakpoint-flush-and-reactivate ()
  "Flush existing breakpoint markers, and reactivate."
  (interactive)
  (dolist (BP matlab-gud-visible-breakpoints)
    (mlg-deactivate BP)
    (mlg-activate BP))
  )


(defun mlg-refresh-breakpoint-buffer ()
  "Refresh the buffer displaying breakpoints."
  (save-excursion
    (let ((buff (get-buffer-create "*MATLAB breakpoints*"))
	  (namelen 5)
	  (inhibit-read-only t))

      (dolist (S matlab-gud-visible-breakpoints)
	(when (> (length (oref S name)) namelen)
	  (setq namelen (length (oref S name)))))
    
      (set-buffer buff)
      (erase-buffer)

      (let ((cnt 1))
	(dolist (F matlab-gud-visible-breakpoints)
	  (insert (format "%2d - " cnt))
	  (insert (mlg-print F namelen) "\n")
	  (setq cnt (1+ cnt))))

      (mlg-breakpoint-mode)
      (goto-char (point-min))
      (current-buffer))))
  
(defun mlg-show-breakpoints ()
  "Display the MATLAB stack in an interactive buffer."
  (interactive)
  (let ((buff (mlg-refresh-breakpoint-buffer)))
  
    (display-buffer
     buff
     '((display-buffer-at-bottom)
       (inhibit-same-window . t)
       (window-height . fit-window-to-buffer))
     )

    (select-window (get-buffer-window buff))
    (goto-char 3)
    ))
  
  
(defvar mlg-breakpoint-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'mlg-breakpoint-choose)
    (define-key km "q" 'mlg-breakpoint-quit)
    (define-key km "n" 'mlg-breakpoint-next)
    (define-key km "p" 'mlg-breakpoint-prev)
    (define-key km [mouse-2] 'mlg-breakpoint-click)
    (define-key km [mouse-1] 'mlg-breakpoint-click)
    km)
  "Keymap used in MATLAB breakpoint mode.")

;; Need this to fix weird problem in define-derived-mode
(defvar mlg-breakpoint-mode-syntax-table (make-syntax-table)
  "Syntax table used in `matlab-shell-help-mode'.")

(define-derived-mode mlg-breakpoint-mode
  fundamental-mode "MBreakpoints"
  "Major mode for viewing a MATLAB breakpoints.

Commands:
\\{mlg-breakpoint-mode-map}"
  :syntax-table mlg-breakpoint-mode-syntax-table
  (setq buffer-read-only t)
  )

(defun mlg-breakpoint-quit ()
  "Quit the MATLAB breakpoint view."
  (interactive)
  (if (= (length (window-list)) 1)
      (bury-buffer)
    (delete-window (selected-window))))

(defun mlg-breakpoint-next ()
  "Visit breakpoint on next line."
  (interactive)
  (forward-line 1)
  (forward-char 2)
  (mlg-breakpoint-choose))

(defun mlg-breakpoint-prev ()
  "Visit breakpoint on next line."
  (interactive)
  (forward-line -1)
  (forward-char 2)
  (mlg-breakpoint-choose))

(defun mlg-breakpoint-click (e)
  "Click on a breakpoint frame to visit it.
Must be bound to event E."
  (interactive "e")
  (mouse-set-point e)
  (mlg-breakpoint-choose))

(defun mlg-breakpoint-choose ()
  "Choose the breakpoint the under the cursor.
Visit the file presented in that breakpoint frame."
  (interactive)
  (let ((topic nil) (fun nil) (p (point)))
    (save-excursion
      (beginning-of-line)
      (forward-char 10)
      (let* ((sf (get-text-property (point) 'object))
	     (f (oref sf file))
	     (l (oref sf line))
	     (buff (find-file-noselect f t)))
	(display-buffer
	 buff
	 '((display-buffer-reuse-window display-buffer-use-some-window)
	   (inhibit-same-window . t))
	 )
	(let ((win (selected-window)))
	  (select-window (get-buffer-window buff))
	  (goto-char (point-min))
	  (forward-line (1- l))
	  (select-window win))
	))))


;;; K prompt state and hooks.
;;

(defun gud-matlab-debug-tracker ()
  "Function called when new prompts appear.
Call debug activate/deactivate features."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (goto-char (point-max))
      (beginning-of-line)
      (cond
       ((and gud-matlab-debug-active (looking-at gud-matlab-marker-regexp->>))
	(setq gud-matlab-debug-active nil)
	(when (boundp 'tool-bar-map)            ; not --without-x
	  (with-current-buffer (matlab-shell-active-p) (kill-local-variable 'tool-bar-map)))
	(global-matlab-shell-gud-minor-mode -1)
	(run-hooks 'gud-matlab-debug-deactivate-hook))
       ((and (not gud-matlab-debug-active) (looking-at gud-matlab-marker-regexp-K>>))
	(setq gud-matlab-debug-active t)
	(when (boundp 'tool-bar-map)            ; not --without-x
	  (with-current-buffer (matlab-shell-active-p)
	    (setq-local tool-bar-map gud-matlab-tool-bar-map)))
	(global-matlab-shell-gud-minor-mode 1)
	(run-hooks 'gud-matlab-debug-activate-hook))
       (t
	;; All clear
	))))
  )

;;; MATLAB SHELL GUD Minor Mode
;;
;; When K prompt is active, this minor mode is applied to frame buffers so
;; that GUD commands are easy to get to.

(defvar matlab-shell-gud-minor-mode-map
  (let ((km (make-sparse-keymap))
	(key ?\ ))
    (while (<= key ?~)
      (define-key km (string key) 'matlab-shell-gud-mode-help-notice)
      (setq key (1+ key)))
    (define-key km "h" 'matlab-shell-gud-mode-help)

    ;; gud bindings.
    (define-key km "b" 'gud-break)
    (define-key km "x" 'gud-remove)
    (define-key km "c" 'gud-cont)
    (define-key km "s" 'gud-step)
    (define-key km " " 'gud-step)
    (define-key km "n" 'gud-next)
    (define-key km "f" 'gud-finish)
    (define-key km "q" 'gud-stop-subjob)
    ;(define-key km "u" 'gud-up)
    ;(define-key km "d" 'gud-down)
    (define-key km "<" 'gud-up)
    (define-key km ">" 'gud-down)
    (define-key km "w" 'mlg-show-stack)
    (define-key km "v" 'gud-list-breakpoints)
    (define-key km "e" 'matlab-shell-gud-show-symbol-value)
    ;; (define-key km "p" gud-print)

    ;;(define-key km "" 'matlab-shell-gud-mode-edit)
    (define-key km "\C-x\C-q" 'matlab-shell-gud-mode-edit) ; like toggle-read-only
    
    km)
  "Keymap used by matlab mode maintainers.")

;;;###autoload
(define-minor-mode matlab-shell-gud-minor-mode
  "Minor mode activated when `matlab-shell' K>> prompt is active.
This minor mode makes MATLAB buffers read only so simple keystrokes
activate debug commands.  It also enables tooltips to appear when the
mouse hovers over a symbol when debugging.
\\<matlab-shell-gud-minor-mode-map>
Debug commands are:
 \\[matlab-shell-gud-mode-edit]  - Edit file (toggle read-only)
            Allows editing file without causing MATLAB to exit debug mode.
 \\[gud-break]        - Add breakpoint (ebstop in FILE at point)
 \\[gud-remove]        - Remove breakpoint (ebclear in FILE at point)
 \\[gud-list-breakpoints]        - List breakpoints (ebstatus)
 \\[gud-step]        - Step (dbstep in)
 \\[gud-next]        - Next (dbstep)
 \\[gud-finish]        - Finish function (dbstep out)
 \\[gud-cont]        - Continue (dbcont)
 \\[matlab-shell-gud-show-symbol-value]        - Evaluate expression
 \\[mlg-show-stack]        - Where am I (ebstack)
 \\[gud-stop-subjob]        - Quit (dbquit)"
  nil " MGUD" matlab-shell-gud-minor-mode-map
  
  ;; Make the buffer read only
  (if matlab-shell-gud-minor-mode
      (progn
	;; Enable
	(when (buffer-file-name) (setq buffer-read-only t))
	(when matlab-shell-debug-tooltips-p
	  (gud-tooltip-mode 1)
	  (add-hook 'tooltip-functions 'gud-matlab-tooltip-tips)
	  )
	;; Replace gud's toolbar which keeps stomping
	;; on our toolbar.
	(make-local-variable 'gud-tool-bar-map)
	(setq gud-tool-bar-map gud-matlab-tool-bar-map)
	)
    ;; Disable
    (when (buffer-file-name)
      (setq buffer-read-only (not (file-writable-p (buffer-file-name)))))
      
    ;; Always disable tooltips, in case configured while in the mode.
    (gud-tooltip-mode -1)
    (remove-hook 'tooltip-functions 'gud-matlab-tooltip-tips)

    ;; Disable the debug toolboar
    (when (boundp 'tool-bar-map)            ; not --without-x
      (kill-local-variable 'tool-bar-map))
    
    )
  )

;;;###autoload
(define-global-minor-mode global-matlab-shell-gud-minor-mode
  matlab-shell-gud-minor-mode
  (lambda ()
    "Should we turn on in this buffer? Only if in a MATLAB mode."
    (when (eq major-mode 'matlab-mode)
      (matlab-shell-gud-minor-mode 1)))
  )

(defun matlab-shell-gud-show-symbol-value (sym)
  "Show the value of the symbol SYM under point from MATLAB shell."
  (interactive
   (list
    (if (use-region-p)
	;; Don't ask user anything, just take it.
	(buffer-substring-no-properties (region-beginning) (region-end))
      (let ((word (matlab-read-word-at-point)))
	(read-from-minibuffer "MATLAB variable: " (cons word 0))))))
  (let ((txt (matlab-shell-collect-command-output
	      (concat "disp(" sym ")"))))
    (if (not (string-match "ERRORTXT" txt))
	(matlab-output-to-temp-buffer "*MATLAB Help*" txt)
      (message "Error evaluating MATLAB expression"))))


(defun matlab-shell-gud-mode-edit ()
  "Turn off `matlab-shell-gud-minor-mode' so you can edit again."
  (interactive)
  (global-matlab-shell-gud-minor-mode -1))

(defun matlab-shell-gud-mode-help-notice ()
  "Default binding for most keys in `matlab-shell-gud-minor-mode'.
Shows a help message in the mini buffer."
  (interactive)
  (error "MATLAB shell GUD minor-mode: Press 'h' for help, 'e' to go back to editing"))

(defun matlab-shell-gud-mode-help ()
  "Show the default binding for most keys in `matlab-shell-gud-minor-mode'."
  (interactive)
  (describe-minor-mode 'matlab-shell-gud-minor-mode)
  )

;;; Tooltips
;;
;; Using the gud tooltip feature for a bunch of setup, but then
;; just override the tooltip fcn (see the mode) with this function
;; as an additional piece.
(defun gud-matlab-tooltip-tips (event)
  "Implementation of the tooltip feature for MATLAB.
Much of this was copied from `gud-tooltip-tips'.

This function must return nil if it doesn't handle EVENT."
  (when (and (eventp event) (tooltip-event-buffer event))
    (with-current-buffer (tooltip-event-buffer event)
      (when (and gud-tooltip-mode
		 matlab-shell-gud-minor-mode
		 (buffer-name gud-comint-buffer) ; might be killed
		 )
	(let ((expr (matlab-shell-gud-find-tooltip-expression event))
	      (txt nil))
	  (when expr
	    (setq txt (matlab-shell-collect-command-output
		       (concat "emacstipstring(" expr ")")))

	    (when (not (string-match "ERRORTXT" txt))

	      (tooltip-show (concat expr "=\n" txt)
			    (or gud-tooltip-echo-area
				tooltip-use-echo-area
				(not tooltip-mode)))
	      t)))))))

(defun matlab-shell-gud-find-tooltip-expression (event)
  "Identify an expression to output in a tooltip at EVENT.
Unlike `tooltip-expr-to-print', this looks at the symbol, and
if it looks like a function call, it will return nil."
  (interactive)

  (with-current-buffer (tooltip-event-buffer event)
    ;; Only do this for MATLAB stuff.
    (when matlab-shell-gud-minor-mode
      
      (let ((point (posn-point (event-end event))))
	(if (use-region-p)
	    (when (and (<= (region-beginning) point) (<= point (region-end)))
	      (buffer-substring (region-beginning) (region-end)))

	  ;; This snippet copied from tooltip.el, then modified to
	  ;; detect matlab functions
	  (save-excursion
	    (goto-char point)
	    (let* ((origin (point))
		   (start (progn
			    (skip-syntax-backward "w_")
			    ;; find full . expression
			    (while (= (preceding-char) ?.)
			      (forward-char -1)
			      (skip-syntax-backward "w_"))
			    (point)))
		   (pstate (syntax-ppss)))
	      (unless (or (looking-at "[0-9]")
			  (nth 3 pstate)
			  (nth 4 pstate))
		(goto-char origin)
		(skip-syntax-forward "w_")
		(when (> (point) start)
		  ;; At this point, look to see we are looking at (.  If so
		  ;; we need to grab that stuff too.
		  (if (not (looking-at "\\s-*("))
		      (buffer-substring-no-properties start (point))
		    ;; Also grab the arguments
		    (matlab-forward-sexp)
		    (buffer-substring-no-properties start (point)))
		  )))))))))

(provide 'matlab-shell-gud)

;;; matlab-shell-gud.el ends here

;; LocalWords:  el Ludlam eludlam emacsvm eieio defcustom keymap dolist subjob
;; LocalWords:  cdr netshell defmacro defun fboundp ebstop ebclear ebstatus
;; LocalWords:  ebstack boundp setq realfname progn aset buf noselect dbhotlink
;; LocalWords:  COMINT errortext dbhlcmd comint endprompt mello mlg EMACSCAP
;; LocalWords:  defclass initarg defmethod longestname namefmt propertize oref
;; LocalWords:  newstack nreverse newframe namelen cnt prev MStack BP del NBPS
;; LocalWords:  defface bp oset ol eol overlayp MBreakpoints MGUD gud's
;; LocalWords:  toolboar minibuffer ERRORTXT eventp emacstipstring posn pstate
;; LocalWords:  ppss sexp
