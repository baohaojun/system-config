;;; keydef.el --- a simpler way to define keys, with kbd syntax

;; Emacs Lisp Archive Entry
;; Filename: keydef.el
;; Author: Michael John Downes <mjd@ams.org>
;; Created: 2001/01/18
;; Keywords: convenience lisp customization keyboard keys
;; Version: 1.16
;; $Revision: 1.1.1.1 $ $Date: 2003-04-04 20:16:06 $

;; This program was placed in the public domain on 2001/01/18 by the
;; Author. The program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;;; The macro keydef provides a simplified interface to define-key that
;;; smoothly handles a number of common complications.

;;; The global-set-key command isn't ideal for novices because of its
;;; relatively complex syntax. And I always found it a little
;;; inconvenient to have to quote the name of the command---that is, I
;;; tend to forget the quote every once in a while and then have to go
;;; back and fix it after getting a load error.

;;; One of the best features is that you can give an Emacs lisp form (or
;;; even a series of forms) as the key definition argument, instead of a
;;; command name, and the keydef macro will automatically add an
;;; interactive lambda wrapper. I use this to get, for example, a more
;;; emphatic kill-buffer command (no confirmation query) by writing
;;;
;;;   (keydef "<S-f11>" (kill-buffer nil))
;;;
;;; For keydef the key sequence is expected to be given uniformly in the
;;; form of a string for the 'kbd' macro, with one or two refinements
;;; that are intended to conceal from users certain points of confusion,
;;; such as (for those whose keyboards lack a Meta key) the whole
;;; Meta/ESC/escape muddle.

;;; I have had some trouble in the past regarding the distinction
;;; between ESC and [escape] (in a certain combination of circumstances
;;; using the latter form caused definitions made with the other form to
;;; be masked---most puzzling when I wasn't expecting it). Therefore the
;;; ESC form is actually preprocessed a bit to ensure that the binding
;;; goes into esc-map.

;;; There is one other special feature of the key sequence syntax
;;; expected by the keydef macro: You can designate a key definition for
;;; a particular mode-map by giving the name of the mode together with
;;; the key sequence string in list form, for example
;;;
;;;   (keydef (latex "C-c %") comment-region)
;;;
;;; This means that the key will be defined in latex-mode-map. [The
;;; point of using this particular example will be made clear below.] I
;;; arranged for the mode name to be given in symbol form just because I
;;; didn't want to have to type extra quotes if I could get away with
;;; it. For the same reason this kind of first arg is not written in
;;; dotted pair form.

;;; If the given mode-map is not defined, keydef "does the right thing"
;;; using eval-after-load. In order to determine what library the
;;; mode-map will be loaded from, it uses the following algorithm:
;;;
;;; First check if foo-mode has autoload information. If not, check
;;; whether "foo-mode" is the name of a library that can be found
;;; somewhere in the load-path (using locate-library); otherwise check
;;; whether "foo" is the name of a locatable library. Failing that, give
;;; up and return nil.
;;;
;;; There is a fall-back mechanism, however, to handle exceptional
;;; cases. If foo-mode-map is undefined but the list mode-map-alist
;;; contains an entry of the form (foo-mode-map foo-other-name-map),
;;; then foo-other-name-map is used as the name of the
;;; keymap.
;;;
;;; If the mode-map is not loaded yet AND the command being bound to a
;;; key is undefined at the time of the keydef assignment, it presents
;;; further problems. The simplest solution is to assume that after the
;;; package is loaded that defines the mode-map, the given command will
;;; be defined and satisfy commandp. With some extra effort it should be
;;; possible to determine more accurately whether the command will be
;;; defined or not, but I'm not sure I want to go to that extreme, since
;;; as far as I can see it would require opening the package file and
;;; searching through it for a matching defun/defalias/fset statement.
;;;
;;; If the mode name matches the mode map name, but foo-mode is not
;;; autoloaded, then some autoload information may need to be provided.
;;; For example, the following line allows definitions to be made for
;;; debugger-mode-map even before debug.el is loaded.
;;;
;;;  (autoload 'debugger-mode "debug" "Autoloaded." 'interactive)
;;;
;;; Although there is no easy way provided by keydef for
;;; gnus-summary-limit-map to be accessed directly, because
;;; its name does not include "mode", you can get a binding into
;;; such a map by writing
;;;
;;;   (keydef (gnus-summary "/ z") gnus-summary-limit-to-zapped)
;;;
;;; which binds /z in gnus-summary-mode-map, which is equivalent to
;;; binding z in gnus-summary-limit-map.
;;;
;;; You might need to add an autoload statement for gnus-summary-mode
;;; in order for this to work, so that keydef knows that it should use
;;; eval-after-load and that the file the mode function will be loaded
;;; from is called "gnus-sum" rather than "gnus-summary-mode". (If it
;;; were the latter, keydef would be able to resolve everything
;;; automatically.)

;;; We COULD HAVE just put the definitions into the mode hook in the
;;; standard way, instead of using eval-after-load, but that would mean
;;; the key definitions get executed repetitiously every time the mode
;;; function gets called, which seems better to avoid, if only for
;;; esthetic reasons (if it can be done without too much trouble).

;;; The following examples show some typical keydef lines followed by the
;;; results of the macro expansion.

;;; Simplest kind of definition:
;;;
;;; (keydef "C-x m" gnus-group-mail)
;;;
;;;   -->(define-key global-map (kbd "C-x m") (quote gnus-group-mail))

;;; What if the command name is misspelled?
;;;
;;; (keydef "C-x m" gnus-gruop-mail)
;;;
;;;   -->(message "keydef: gnus-gruop-mail unknown \
;;;                \(perhaps misspelled, or not loaded yet\)")

;;; A leading ESC gets special handling to go through esc-map.
;;;
;;; (keydef "ESC &" query-replace-regexp)
;;;
;;;   -->(define-key esc-map (kbd "&") (quote query-replace-regexp))

;;; Undefine a key:
;;;
;;; (keydef "ESC `")
;;;
;;;   -->(define-key esc-map (kbd "`") nil)

;;; If the second arg is a string, keydef defines the given key sequence
;;; as a keyboard macro. The following macro puts in TeX-style double
;;; quotes and then moves the cursor backward to leave it in the middle:
;;;
;;; (keydef "\"" "``''\C-b\C-b")
;;;
;;;   -->(define-key global-map (kbd "\"") "``''\002\002")

;;; Reset a key to self-insert
;;;
;;; (keydef "\"" "\"")
;;;
;;;   -->(define-key global-map (kbd "\"") (quote self-insert-command))

;;; If the second arg is a list, wrap it in an interactive lambda form.
;;;
;;; (keydef "C-z"
;;;   (message "Control-Z key disabled---redefine it if desired."))
;;;
;;;   -->(define-key global-map
;;;       (kbd "C-z")
;;;       (lambda (arg)
;;;         "anonymous keydef function"
;;;         (interactive "p")
;;;         (message "Control-Z key disabled---redefine it if desired.")))
;;;
;;; Note that the interactive lambda wrapper added by keydef, when the
;;; CMD does not satisfy commandp, always takes a single prefix argument
;;; named "arg", which is read in the usual way with (interactive "p");
;;; so this could be used in the body of the function if need be.

;;; This shows the notation for F-keys.
;;;
;;; (keydef "<C-f17>" (kill-buffer nil))
;;;
;;;   -->(define-key global-map
;;;       (kbd "<C-f17>")
;;;       (lambda (arg)
;;;         "*Anonymous function created by keydef."
;;;         (interactive "p")
;;;         (kill-buffer nil)))

;;; Because of the confusing Meta/Escape complications, I recommend to
;;; the users that I support that they use the ESC notation
;;; consistently if that is what they type from their keyboard, even
;;; for F-key definitions that might normally be written with <M-...>
;;; notation.
;;;
;;; (keydef "ESC <f3>" find-file-read-only)
;;;
;;;   -->(define-key esc-map (kbd "<f3>") (quote find-file-read-only))

;;; The next two definitions go together. The second one shows how to
;;; write a mode-specific definition.
;;;
;;; (keydef "<f5>" isearch-forward)
;;;
;;;   -->(define-key global-map (kbd "<f5>") (quote isearch-forward))
;;;
;;; (keydef (isearch "<f5>") isearch-repeat-forward)
;;;
;;;   -->(define-key isearch-mode-map (kbd "<f5>")
;;;                                   (quote isearch-repeat-forward))

;;; Making a definition for a mode-map that is not loaded yet.
;;;
;;; (keydef (latex "C-c %") comment-region)
;;;
;;;   -->(eval-after-load "tex-mode"
;;;        (quote
;;;         (define-key latex-mode-map
;;;           (kbd "C-c %")
;;;           (quote comment-region))))

;;; Code:

;;; TO DO:
;;;
;;; ---If someone wants to do massive alterations or additions to a
;;; mode-map that is not yet loaded, it might be a good idea to
;;; provide another macro that will bundle them into a single
;;; eval-after-load call rather than dozens of separate ones.
;;;
;;; ---More error-checking would probably be a good idea, when SEQ
;;; satisfies listp but the contents of the list are not usable in the
;;; way that we expect.

;; This variable is needed because the information is not readily
;; available for look-up in any other way. (Well, I don't want to get
;; into defadvice'ing use-local-map and stuff like that.)
(defvar mode-map-alist
  (list
   (quote (latex-mode tex-mode-map))
   (quote (shell-script-mode sh-mode-map)))
  "If the local keymap for foo-mode is bar-mode-map instead of
foo-mode-map, this alist allows you to specify what corresponds to
what. The car of each pair should be a major mode name and the cdr
should be the name of the local map that is used for that mode.")

;;; If the mode name matches the mode map name, but foo-mode is not
;;; autoloaded, then some autoload information may need to be provided.
;;; For example, the following line allows definitions to be made for
;;; debugger-mode-map even before debug.el is loaded. This line would
;;; not be necessary if debugger-mode were already declared as an
;;; autoloaded function.
(autoload 'debugger-mode "debug" "Autoloaded." 'interactive)

(defun keydef-lib-lookup (mode)
  "For a not-already-loaded mode function, try to determine what library
it would be loaded from: First check for autoload information, otherwise
check if a library file matching the mode name can be found in the load
path, with or without the -mode suffix. Failing that, give up."
  (let* ((modesym (intern mode))
         (fcar (and (fboundp modesym) (car (symbol-function modesym)))))
    (cond
     ((eq fcar 'autoload)
      (car (cdr (symbol-function modesym))))
     ((locate-library mode)
      mode)
     (t
      (let ((shortmode (substring mode 0 -5))) ; chop "-mode" from the end
        (if (locate-library shortmode)
            shortmode))))))

;;;###autoload
(defmacro keydef (seq &rest cmd)
  "Define the key sequence SEQ, written in kbd form, to run CMD.
CMD is automatically wrapped in an anonymous interactive function if it
is Emacs Lisp code rather than a command name. SEQ may also have the form
\(MODE SEQ\) where the car is a mode name\; for example

  \(keydef \(latex \"C-c %\"\) comment-region\)

means to define the given key in latex-mode-map. And this will work even
if latex-mode is not loaded yet, provided that it is possible to deduce
the file that it will be loaded from, either from the autoload info or
by searching for a matching file name in the Emacs load path.

For best results, the \"mode name\" that you use here should yield the
proper foo-mode-map symbol when \"-mode-map\" is appended\; although
this will normally match the mode name as given in the mode line,
Shell-script is one example I can think of where it doesn't---the map is
named sh-mode-map. The common cases that I know about, including
shell-script-mode and latex-mode, are handled as exceptions through the
variable mode-map-alist. But for other cases you will need to look up
the name of the mode-map that goes with the given mode."
  (let ((map (quote global-map))
        (modestring)
        (loaded t))
    ;; If seq is a list, the car indicates a mode-specific map that we
    ;; should use instead of global-map.
    (if (and (listp seq)
             (symbolp (car seq))
             (stringp (car (cdr seq))))
        (let ((othermap))
          (setq modestring
                (format "%s-mode"
                        (downcase (symbol-name (car seq)))))
          (setq othermap (assq (intern modestring) mode-map-alist))
          (if othermap
              (setq map (nth 1 othermap))
            (setq map (intern (format "%s-map" modestring))))
          (if (not (and (boundp map) (keymapp (symbol-value map))))
              (setq loaded nil))
          (setq seq (car (cdr seq)))))
    (cond
     ((stringp seq)
      (if (string-match "^ESC " seq)
          (progn
            (setq seq (substring seq 4))
            (setq map (quote esc-map)))))
     (t
      (if (vectorp seq)
          (error
"keydef: '%s' vector form disallowed here, use kbd syntax instead."
           (prin1-to-string seq))
        (error "keydef: Invalid key sequence '%s'" (prin1-to-string seq)))))
    (if (not (null cmd))
        (let ((token (car cmd)))
          ;; Note that commandp is true for strings. So we have to be a
          ;; little careful about the order of tests here.
          (cond
           ;; This case arises when an explicit second arg of nil is given.
           ((eq token nil)
            (setq cmd nil))
           ;; If someone forgets that keydef does not require you to
           ;; quote the command name, we had better make sure it works
           ;; anyway.
           ((eq (car-safe token) 'quote)
            (setq cmd token))
           ;; If the CMD is a one-character string that matches the SEQ, use
           ;; self-insert-command as the binding. Otherwise it will be a macro
           ;; that will run an infinite loop until specpdl-size is exceeded.
           ((stringp token)
            (if (and (= (length token) 1)
                     (string-equal token seq))
                (setq cmd '(quote self-insert-command))
              (setq cmd token)))        ; kbd macro string
           ;; If the command is a simple command name---or a keymap,
           ;; such as help-command---use it directly as the
           ;; definition.
           ((and (or (commandp token) (keymapp token))
                 (= (length cmd) 1))
            (setq cmd `(quote ,token)))
           ;; If the command looks like a simple command name but fails the
           ;; commandp test, then probably it was misspelled; if it passes the
           ;; fboundp test, however, make a lambda wrapper similar to the next
           ;; case. Could try to work harder at getting the arguments right in
           ;; that case, but for now just assume it has zero args.
           ((and (= (length cmd) 1) (symbolp token))
            (cond
             ((fboundp token)
              (setq cmd
                    (append
                     '(lambda (arg) "*Anonymous function created by keydef."
                        (interactive "p"))
                     (list cmd))))
             ((not loaded)
              ;; If the mode-map is not loaded yet, assume that the
              ;; command will become defined when the package is loaded.
              (setq cmd `(quote ,token)))
             (t
              ;; Unknown command is being added to a known map. Probably
              ;; misspelled?
              (setq cmd `(quote ,token))
              (message
               "keydef: '%s' unknown %s"
               (prin1-to-string token)
               "\(perhaps misspelled, or needs autoload info?\)"))))
           (t
            ;; We have what seems to be a list of code elements. Create
            ;; an anonymous function wrapper.
            (setq cmd
                  (append
                   '(lambda (arg)
                      "*Anonymous function created by keydef."
                      (interactive "p"))
                   cmd))))))
    (if (and (not loaded) modestring)
        (let ((loadfrom (keydef-lib-lookup modestring)))
          (if loadfrom
              `(eval-after-load ,loadfrom
                 (quote (define-key ,map (kbd ,seq) ,cmd)))
            (message "keydef: '%s' unknown %s"
                     modestring
                     "\(perhaps misspelled, or needs autoload info?\)")))
      `(define-key ,map (kbd ,seq) ,cmd))))

(provide 'keydef)
;;; keydef.el ends here
