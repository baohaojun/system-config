;; prompt.jl -- read line from user
;; Time-stamp: <2000-02-25 22:02:54 tjp>
;;
;; Copyright (C) 2008 Sergey I. Sharybin <sharybin@nm.ru>
;; Copyright (C) 2000 Topi Paavola <tjp@iki.fi>
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Commentary:

;; See the documentation for prompt.

(define-structure bhj.prompt

    (export bhj-prompt
            bhj-prompt-for-symbol
            bhj-prompt-for-function
            bhj-prompt-for-variable
            bhj-prompt-for-command

            ;; motion / editing commands
            bhj-prompt-backward-character
            bhj-prompt-forward-character
            bhj-prompt-backward-word
            bhj-prompt-forward-word
            bhj-prompt-beginning-of-line
            bhj-prompt-end-of-line
            bhj-prompt-previous
            bhj-prompt-next
            bhj-prompt-yank
            bhj-prompt-accept
            bhj-prompt-complete
            bhj-prompt-clear
            bhj-prompt-backspace
            bhj-prompt-kill-line
            bhj-prompt-exit

            ;; autoloaded from bhj-prompt-extras
            bhj-prompt-for-file
            bhj-prompt-for-directory
            bhj-prompt-from-list
            bhj-prompt-for-string
            bhj-prompt-for-number
            pwd-bhj-prompt

            ;; autoloaded from bhj-prompt-wm
            bhj-prompt-for-window
            bhj-prompt-for-workspace)

    (open rep
          rep.io.files
          rep.system
          rep.regexp
          rep.data.ring
          sawfish.wm.misc
          sawfish.wm.colors
          sawfish.wm.events
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.fonts)

  (defgroup messages "Messages" :group misc)

  (defcustom bhj-prompt-font default-font
    "Font for bhj-prompt: \\w"
    :type font
    :group (misc messages))

  (defcustom bhj-prompt-color (cons (get-color "black") (get-color "white"))
    "Bhj-Prompt message's colors."
    :type (pair (labelled "Foreground:" color) (labelled "Background:" color))
    :group (misc messages))

  (defcustom bhj-prompt-keymap (make-keymap)
    "Keymap containing bindings active when reading a string from the user."
    :group bindings
    :type keymap)

  (defcustom bhj-prompt-esc-keymap (make-keymap)
    "Keymap containing binding keys for the ESC prefix."
    :group bindings
    :type keymap)

  (defvar bhj-prompt-max-display 20
    "Maximum number of completions to display under the input string.")

  (defvar bhj-prompt-word-regexp "[0-9a-z_]"
    "Regexp that determines which characters are to be considered part
of a word when moving.")

  (defvar bhj-prompt-file-exclude '"\\.(o|jlc|x)$|~$|^#.*#$|^\\.\\.?$"
    "A regexp, if it matches the file being considered for completion, the file
is rejected.")

  (defvar bhj-prompt-list nil
    "List of possible entries for bhj-prompt-from-list.")

  (defvar bhj-prompt-list-fold-case nil
    "Whether bhj-prompt-from-list should ignore case.")

  (defvar bhj-prompt-history (make-ring 16)
    "Ring buffer containing strings most-recently entered through the `bhj-prompt'
function.")

  (defvar bhj-prompt-window-position
    (cons (- (quotient (screen-width) 2) 200) -200)
    "A cons cell defining the screen position at which the `bhj-prompt' window is
displayed. See the `display-message' function for more details.")

  (defvar bhj-prompt-result nil)
  (defvar bhj-prompt-result-save nil)
  (defvar bhj-prompt-bhj-prompt nil)
  (defvar bhj-prompt-completion-fun nil)
  (defvar bhj-prompt-validation-fun nil)
  (defvar bhj-prompt-abbrev-fun nil)
  (defvar bhj-prompt-display-fun nil)
  (defvar bhj-prompt-position 0)
  (defvar bhj-prompt-completion-position nil)
  (defvar bhj-prompt-completions nil)
  (defvar bhj-prompt-completions-outdated nil)
  (defvar bhj-prompt-history-pos nil)
  (defvar bhj-prompt-saved nil)
  (defvar bhj-prompt-attr nil)


  ;; From merlin
  ;; But maybe better if we'd include this util?

  ;; string/font -> font
  (define (bhj-prompt-fontify font)
    (if (stringp font) (get-font font) font))

  ;; string/color -> color
  (define (bhj-prompt-colorify color)
    (if (stringp color) (get-color color) color))

  ;; assq with default
  (define (bhj-prompt-assqd key alist default)
    (if (assq key alist)
        (assq key alist)
      (cons key default)))

  (defun bhj-prompt-exit ()
    "Cancel string input."
    (cond
     (bhj-prompt-result-save
      (setq bhj-prompt-result (car bhj-prompt-result-save)
            bhj-prompt-result-save (cdr bhj-prompt-result-save))
      (bhj-prompt-end-of-line)
      (bhj-prompt-update-display))
     ((and bhj-prompt-result (not (string= "" bhj-prompt-result)))
      (setq bhj-prompt-result "")
      (bhj-prompt-end-of-line)
      (bhj-prompt-update-display))
     (t
      (throw 'bhj-prompt-exit nil))))

  (defun bhj-prompt-accept ()
    "End input and accept current string."
    (let ((result (if (not bhj-prompt-validation-fun)
                      bhj-prompt-result
                    (bhj-prompt-validation-fun bhj-prompt-result))))
      (if result
          (progn
            (unless (or (null bhj-prompt-history)
                        (equal (get-from-ring bhj-prompt-history 1) bhj-prompt-result))
              (add-to-ring bhj-prompt-history bhj-prompt-result))
            (throw 'bhj-prompt-exit result))
        (beep))))

  (defun bhj-prompt-next (count)
    (interactive "p")
    (when bhj-prompt-history
      (setq count (- bhj-prompt-history-pos count))
      (if (zerop count)
          (progn
            (setq bhj-prompt-result bhj-prompt-saved)
            (setq bhj-prompt-history-pos count))
        (let
            ((string (get-from-ring bhj-prompt-history count)))
          (when string
            (when (zerop bhj-prompt-history-pos)
              (setq bhj-prompt-saved bhj-prompt-result))
            (setq bhj-prompt-result string)
            (setq bhj-prompt-history-pos count))))
      (bhj-prompt-changed)
      (bhj-prompt-end-of-line)
      (bhj-prompt-update-display)))

  (defun bhj-prompt-previous (count)
    (interactive "p")
    (bhj-prompt-next (- count)))

  (defun bhj-prompt-changed ()
    (setq bhj-prompt-completions-outdated t))

  (defun bhj-prompt-clear ()
    "Clear input buffer."
    (setq bhj-prompt-result "")
    (setq bhj-prompt-position 0)
    (bhj-prompt-changed)
    (bhj-prompt-update-display))

  (defun bhj-prompt-backspace ()
    "Remove previous character from buffer."
    (when (> bhj-prompt-position 0)
      (let ((cutoff (max (- bhj-prompt-position 1) 0)))
        (setq bhj-prompt-result
              (concat (substring bhj-prompt-result 0 cutoff)
                      (substring bhj-prompt-result (1+ cutoff))))
        (setq bhj-prompt-position (max 0 (1- bhj-prompt-position)))
        (bhj-prompt-changed)
        (bhj-prompt-update-display))))

  (defun bhj-prompt-delete-char ()
    "Delete the following character."
    (bhj-prompt-forward-character)
    (bhj-prompt-backspace))

  (defun bhj-prompt-kill-line ()
    "Delete rest of line."
    (setq bhj-prompt-result (substring bhj-prompt-result 0 bhj-prompt-position))
    (bhj-prompt-changed)
    (bhj-prompt-update-display))

  (defun bhj-prompt-move (num)
    "Move NUM characters forward or backward."
    (let ((new-pos (+ bhj-prompt-position num)))
      (and (>= new-pos 0) (<= new-pos (length bhj-prompt-result))
           (setq bhj-prompt-position new-pos)
           (bhj-prompt-update-display))))

  (defun bhj-prompt-forward-word ()
    "Move to next non-word character."
    (setq bhj-prompt-position (1+ bhj-prompt-position))
    (while (and (< bhj-prompt-position (length bhj-prompt-result))
                (string-looking-at bhj-prompt-word-regexp
                                   bhj-prompt-result bhj-prompt-position t))
      (setq bhj-prompt-position (1+ bhj-prompt-position)))
    (setq bhj-prompt-position (min bhj-prompt-position
                                   (length bhj-prompt-result)))
    (bhj-prompt-update-display))

  (defun bhj-prompt-backward-word ()
    "Move to previous non-word character."
    (setq bhj-prompt-position (1- bhj-prompt-position))
    (while (and (> bhj-prompt-position 0)
                (string-looking-at "\\s"
                                   bhj-prompt-result bhj-prompt-position t))
      (setq bhj-prompt-position (1- bhj-prompt-position)))
    (while (and (> bhj-prompt-position 0)
                (string-looking-at bhj-prompt-word-regexp
                                   bhj-prompt-result bhj-prompt-position t))
      (setq bhj-prompt-position (1- bhj-prompt-position)))
    (setq bhj-prompt-position (max bhj-prompt-position 0))
    (bhj-prompt-update-display))

  (defun bhj-prompt-backward-kill-word ()
    (bhj-prompt-backward-word)
    (bhj-prompt-kill-word))

  (defun bhj-prompt-kill-word ()
    "Kill the next word."
    (let ((old-bhj-prompt-position bhj-prompt-position))
      (setq bhj-prompt-position (1+ bhj-prompt-position))
      (while (and (< bhj-prompt-position (length bhj-prompt-result))
                  (string-looking-at bhj-prompt-word-regexp
                                     bhj-prompt-result bhj-prompt-position t))
        (setq bhj-prompt-position (1+ bhj-prompt-position)))
      (setq bhj-prompt-position (min bhj-prompt-position
                                     (length bhj-prompt-result)))
      (setq bhj-prompt-result (concat (substring bhj-prompt-result 0 old-bhj-prompt-position)
                                      (substring bhj-prompt-result bhj-prompt-position (length bhj-prompt-result)))
            bhj-prompt-position old-bhj-prompt-position)
      (bhj-prompt-update-display))
    (throw 'bhj-prompt-esc-exit nil))

  (defun bhj-prompt-forward-character ()
    "Move forward one character."
    (bhj-prompt-move 1))

  (defun bhj-prompt-backward-character ()
    "Move backward one character."
    (bhj-prompt-move -1))

  (defun bhj-prompt-beginning-of-line ()
    "Move to beginning of line."
    (setq bhj-prompt-position 0)
    (bhj-prompt-update-display))

  (defun bhj-prompt-end-of-line ()
    "Move to end of line."
    (setq bhj-prompt-position (length bhj-prompt-result))
    (bhj-prompt-update-display))

  (defun bhj-prompt-complete ()
    (if (and (not bhj-prompt-completions-outdated) bhj-prompt-completion-position)
        (let
            ((new (min (max 0 (- (length bhj-prompt-completions)
                                 bhj-prompt-max-display))
                       (+ bhj-prompt-completion-position bhj-prompt-max-display))))
          (setq bhj-prompt-completion-position
                (if (= new bhj-prompt-completion-position)
                    0
                  new)))
      (when bhj-prompt-completion-fun
        (let
            (compl)
          (setq bhj-prompt-completions (bhj-prompt-completion-fun bhj-prompt-result))
          (setq compl (if (= 1 (length bhj-prompt-completions))
                          (progn
                            (setq bhj-prompt-result-save (cons bhj-prompt-result bhj-prompt-result-save))
                            (car bhj-prompt-completions))
                        (setq bhj-prompt-result-save nil)
                        (or (complete-string bhj-prompt-result bhj-prompt-completions)
                            bhj-prompt-result)))
          (when compl
            (if (string= compl bhj-prompt-result)
                (setq bhj-prompt-completions-outdated nil)
              (setq bhj-prompt-result compl)
              (setq bhj-prompt-completions
                    (sort (delete-if-not (lambda (x)
                                           (string-head-eq x compl))
                                         bhj-prompt-completions))))
            (bhj-prompt-end-of-line)
            (when (cdr bhj-prompt-completions)
              (setq bhj-prompt-completion-position 0))))))
    (bhj-prompt-update-display))

  (defun bhj-prompt-format-completions ()
    (when (numberp bhj-prompt-completion-position)
      (let ((compl (nthcdr bhj-prompt-completion-position bhj-prompt-completions))
            (continued nil))
        (when (nthcdr bhj-prompt-max-display compl)
          (setq compl (reverse (nthcdr (- (length compl) bhj-prompt-max-display)
                                       (reverse compl))))
          (setq continued "[...]\n"))
        (concat (and (/= bhj-prompt-completion-position 0) "[...]\n")
                (apply concat (mapcar (lambda (x)
                                        (format nil "%s\n"
                                                (if bhj-prompt-abbrev-fun
                                                    (bhj-prompt-abbrev-fun x)
                                                  x)))
                                      compl))
                continued))))

  (defun bhj-prompt-update-display ()
    (let ((result (if bhj-prompt-display-fun
                      (bhj-prompt-display-fun bhj-prompt-result)
                    bhj-prompt-result))
          (completions (bhj-prompt-format-completions)))
      (let
          (
           (fg (bhj-prompt-colorify (cdr (bhj-prompt-assqd 'foreground bhj-prompt-attr (car bhj-prompt-color)))))
           (bg (bhj-prompt-colorify (cdr (bhj-prompt-assqd 'background bhj-prompt-attr (cdr bhj-prompt-color)))))
           (font (bhj-prompt-fontify (cdr (bhj-prompt-assqd 'font bhj-prompt-attr bhj-prompt-font))))
           )
        (display-message
         (concat completions
                 (when completions "\n\n")
                 bhj-prompt-bhj-prompt
                 (substring result 0 bhj-prompt-position)
                 ?| (substring result bhj-prompt-position))
         `((position . ,bhj-prompt-window-position)
           (foreground . ,fg)
           (background . ,bg)
           (font . , font)
           )))))

  (defun bhj-prompt-yank ()
    "Yank from system clipboard."
    (system "timeout 2 getclip > /tmp/sawfish.clip")
    (let* ((clipfile (open-file "/tmp/sawfish.clip" 'read))
           (line "")
           (clip ""))
      (while (setq line (read-line clipfile))
        (setq clip (concat clip line)))
      (close-file clipfile)
      (setq bhj-prompt-result
            (concat (substring bhj-prompt-result 0 bhj-prompt-position)
                    clip
                    (substring bhj-prompt-result bhj-prompt-position)))
      (setq bhj-prompt-position (+ bhj-prompt-position (length clip)))
      (bhj-prompt-changed)
      (bhj-prompt-update-display)
      t))

  ;; Insert all unbound keys to result.
  (defun bhj-prompt-unbound-callback ()
    (let ((key (current-event-string)))
      (setq bhj-prompt-result
            (concat (substring bhj-prompt-result 0 bhj-prompt-position)
                    key
                    (substring bhj-prompt-result bhj-prompt-position)))
      (setq bhj-prompt-position (+ bhj-prompt-position (length key)))
      (bhj-prompt-changed)
      (bhj-prompt-update-display)
      t))

  (defun bhj-prompt (#!optional title start attributes)
    "Bhj-Prompt the user for a string."
    (unless (stringp title)
      (setq title "Enter string:"))
    (unless (string-match " $" title)
      (setq title (concat title ? )))
    (call-with-keyboard-grabbed
     (lambda ()
       (unwind-protect
           (let* ((override-keymap bhj-prompt-keymap)
                  (bhj-prompt-result (or start ""))
                  (bhj-prompt-bhj-prompt title)
                  (bhj-prompt-position (length bhj-prompt-result))
                  (bhj-prompt-history-pos 0)
                  (bhj-prompt-saved nil)
                  (bhj-prompt-attr attributes)
                  (bhj-prompt-completion-position nil)
                  (bhj-prompt-completions nil)
                  (bhj-prompt-completions-outdated t)
                  (unbound-key-hook (list bhj-prompt-unbound-callback)))
             (bhj-prompt-update-display)
             (catch 'bhj-prompt-exit
               (recursive-edit)))
         (display-message nil)))))

  (defun bhj-prompt2 (#!optional title start attributes)
    "Bhj-Prompt the user for a string."
    (unless (stringp title)
      (setq title "Enter string:"))
    (unless (string-match " $" title)
      (setq title (concat title ? )))
    (call-with-keyboard-grabbed
     (lambda ()
       (unwind-protect
           (let* ((override-keymap bhj-prompt-keymap)
                  (bhj-prompt-result (or start ""))
                  (bhj-prompt-bhj-prompt title)
                  (bhj-prompt-position (length bhj-prompt-result))
                  (bhj-prompt-history-pos 0)
                  (bhj-prompt-saved nil)
                  (bhj-prompt-attr attributes)
                  (bhj-prompt-completion-position nil)
                  (bhj-prompt-completions nil)
                  (bhj-prompt-completions-outdated t)
                  (unbound-key-hook (list bhj-prompt-unbound-callback)))
             (bhj-prompt-update-display)
             (catch 'bhj-prompt-exit
               (recursive-edit)))
         (display-message nil)))))

  (defun bhj-prompt-esc ()
    "Esc map, for a true keymap can't be used."
    (let* ((override-keymap bhj-prompt-esc-keymap)
           (unbound-key-hook (list (lambda () (throw 'bhj-prompt-esc-exit nil)))))
      (catch 'bhj-prompt-esc-exit
        (recursive-edit))))


  (defun bhj-prompt-for-symbol (#!optional title predicate validator)
    (let ((bhj-prompt-completion-fun
           (lambda (x)
             (mapcar symbol-name
                     (apropos (concat ?^ (quote-regexp x)) predicate))))
          (bhj-prompt-validation-fun
           (lambda (x)
             (let
                 ((symbol (intern x)))
               (if validator
                   (and (validator symbol) symbol)
                 symbol)))))
      (bhj-prompt title)))

  (defun bhj-prompt-for-function (#!optional title)
    "Bhj-Prompt for a function."
    (bhj-prompt-for-symbol (or title "Enter name of function:")
                           (lambda (x)
                             (and (boundp x)
                                  (let ((value (symbol-value x)))
                                    (or (functionp value)
                                        (macrop value)
                                        (special-form-p value)))))))

  (defun bhj-prompt-for-variable (#!optional title)
    "Bhj-Prompt for a variable."
    (bhj-prompt-for-symbol (or title "Enter name of variable:") boundp))

  (defun bhj-prompt-for-command (#!optional title)
    (bhj-prompt-for-symbol title commandp commandp))


;;; autoloads

  (autoload 'bhj-prompt-for-file "sawfish/wm/util/bhj-prompt-extras")
  (autoload 'bhj-prompt-for-directory "sawfish/wm/util/bhj-prompt-extras")
  (autoload 'bhj-prompt-from-list "sawfish/wm/util/bhj-prompt-extras")
  (autoload 'bhj-prompt-for-string "sawfish/wm/util/bhj-prompt-extras")
  (autoload 'bhj-prompt-for-number "sawfish/wm/util/bhj-prompt-extras")
  (autoload 'pwd-bhj-prompt "sawfish/wm/util/bhj-prompt-extras")

  (autoload 'bhj-prompt-for-window "sawfish/wm/util/bhj-prompt-wm")
  (autoload 'bhj-prompt-for-workspace "sawfish/wm/util/bhj-prompt-wm")


;;; init keymap

  ;; start code-generator "^\\s *;*\\s *"
  ;; for x in bhj-prompt-backward-word bhj-prompt-forward-word bhj-prompt-exit; do
  ;;     cat <<EOF
  ;; (defun $x-esc()
  ;; ($x)
  ;; (throw 'bhj-prompt-esc-exit nil))
  ;;
  ;; EOF
  ;; done
  ;; end code-generator
  ;; start generated code
  (defun bhj-prompt-backward-word-esc()
    (bhj-prompt-backward-word)
    (throw 'bhj-prompt-esc-exit nil))

  (defun bhj-prompt-forward-word-esc()
    (bhj-prompt-forward-word)
    (throw 'bhj-prompt-esc-exit nil))

  (defun bhj-prompt-exit-esc()
    (bhj-prompt-exit)
    (throw 'bhj-prompt-esc-exit nil))


  ;; end generated code

  (bind-keys bhj-prompt-esc-keymap
             "b" bhj-prompt-backward-word-esc
             "ESC" bhj-prompt-exit-esc
             "f" bhj-prompt-forward-word-esc
             "d" bhj-prompt-kill-word
             "BS" bhj-prompt-backward-kill-word)

  (bind-keys bhj-prompt-keymap
             "ESC" bhj-prompt-esc
             "Super-Button1-Click" bhj-prompt-accept
             "Super-Button2-Click" bhj-prompt-accept
             "Super-Button3-Click" bhj-prompt-accept
             "Super-RET" bhj-prompt-accept
             "C-g" bhj-prompt-exit
             "C-u" bhj-prompt-clear
             "BS" bhj-prompt-backspace
             "C-d" bhj-prompt-delete-char
             "C-k" bhj-prompt-kill-line
             "Left" bhj-prompt-backward-character
             "C-b" bhj-prompt-backward-character
             "Right" bhj-prompt-forward-character
             "C-f" bhj-prompt-forward-character
             "C-Left" bhj-prompt-backward-word
             "M-b" bhj-prompt-backward-word
             "A-b" bhj-prompt-backward-word
             "C-Right" bhj-prompt-forward-word
             "M-f" bhj-prompt-forward-word
             "A-f" bhj-prompt-forward-word
             "C-a" bhj-prompt-beginning-of-line
             "C-e" bhj-prompt-end-of-line
             "TAB" bhj-prompt-complete
             "RET" bhj-prompt-accept
             "Up" bhj-prompt-previous
             "Down" bhj-prompt-next
             "C-p" bhj-prompt-previous
             "C-n" bhj-prompt-next
             "M-n" bhj-prompt-next
             "M-p" bhj-prompt-previous
             "A-n" bhj-prompt-next
             "C-y" bhj-prompt-yank
             "A-p" bhj-prompt-previous))
