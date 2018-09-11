;;; daemons.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

;; Copyright (c) 2018 Chris Bowdon
;;
;; Author: Chris Bowdon
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: February 13, 2018
;; Modified: February 13, 2018
;; Version: 2.0.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;; A UI for managing init system daemons (services).
;;
;; There is support for the following init systems and service managers:
;;
;; - systemd (RHEL7, Fedora, openSUSE, Debian, Ubuntu, Arch, etc.)
;; - SysVinit (RHEL6)
;; - brew services (macOS)
;; - GNU Shepherd for user services (GuixSD)
;;
;; Managing daemons on remote systems is supported with TRAMP.
;;
;;; Code:
(require 'seq)
(require 'map)

;; Fix for Emacs 27 byte compilation errors relating to thread-last. See:
;; https://github.com/cbowdon/daemons.el/issues/10#issuecomment-392261477
(eval-when-compile (require 'subr-x))

;; customization
(defgroup daemons nil
  "Customization group for Daemons mode"
  :group 'daemons)

(defcustom daemons-always-sudo nil
  "Whether to always attempt to sudo up when using ‘daemons-mode’ locally.

Note that this has no effect on remote systems.

This defaults to off because in some systems at least you can query status
without special privileges and will be prompted for a root password if you try
anything else.  But at other times it's much more convenient to just assume sudo
powers when the buffer loads and enact everything as root.

Security wise - off is safer of course, to avoid unnecessary privilege."
  :type 'boolean
  :group 'daemons)

(defcustom daemons-init-system-submodules '(daemons-systemd
                                            daemons-sysvinit
                                            daemons-brew
                                            daemons-shepherd)
  "List of available init system submodules for `daemons'.
When running `daemons' each of these will be `required'd and the \"test\" form
in each will be evaluated to determine if it is the right backend to use for
the current buffer.

If you implement your own init-system-submodule, add it to this list and make
sure it is on your load path.

See `daemons-define-submodule' for how to implement your own."
  :type '(repeat symbol))

;; declarations
(defvar daemons--shell-command-fun 'shell-command
  "Contains a `shell-command' function.

Override this to your own value for mocking out shell calls in tests.")

(defvar daemons--shell-command-to-string-fun 'shell-command-to-string
  "Contains a `shell-command-to-string' function.
Override this to your own value for mocking out shell calls in tests.")

(defvar daemons--init-system-submodules-alist nil
  "An alist of the available init system submodules.")

(defvar daemons-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'daemons-status-at-point)
    (define-key map (kbd "s") 'daemons-start-at-point)
    (define-key map (kbd "S") 'daemons-stop-at-point)
    (define-key map (kbd "R") 'daemons-restart-at-point)
    (define-key map (kbd "r") 'daemons-reload-at-point)
    map)
  "Keymap for daemons mode.")

(defvar daemons-output-mode-map (copy-keymap daemons-mode-map)
  "Keymap for daemons output mode.")

;; TODO this needs to become buffer-local
(defvar daemons--current-id nil
  "ID of the daemon in the current output buffer.")

;; defuns
(defun daemons--split-lines (string)
  "Split STRING Into list of lines."
  (split-string string "[\n\r]+" t))

(defun daemons--using-tramp-path-p (path)
  "Return non-nil if tramp is loaded and PATH is a tramp path."
  (and (functionp 'tramp-tramp-file-p)
       (tramp-tramp-file-p path)))

(defun daemons--get-user-and-hostname (path)
  "Get the user and hostname of the given PATH, in format \"user@hostname\"."
  (if (daemons--using-tramp-path-p path)
      (let* ((dissected-path (tramp-dissect-file-name path))
             (user (tramp-file-name-user dissected-path))
             (host (tramp-file-name-host dissected-path)))
        (cond ((and user host) (format "%s@%s" user host))
              ((and (not user) host) host)
              ((and user (not host) user))
              (t path)))
    (format "%s@%s"
            (user-login-name)
            (system-name))))

(defun daemons--get-list-buffer-name (hostname)
  "Return the buffer name for daemons on HOSTNAME."
  (format "*daemons for %s*" hostname))

(defun daemons--get-output-buffer-name (hostname)
  "Return the buffer name for daemons output on HOSTNAME."
  (format "*daemons-output for %s*" hostname))

(defun daemons--shell-command (&rest args)
  "Dynamically bound alias for `shell-command' (to enable test mocks).
ARGS are passed to the underlying function."
  (apply daemons--shell-command-fun args))

(defun daemons--shell-command-to-string (&rest args)
  "Dynamically bound alias for `shell-command-to-string' (to enable test mocks).
ARGS are passed to the underlying function."
  (apply daemons--shell-command-to-string-fun args))

(defun daemons--daemon-at-point ()
  "Return the id of the daemon of the current line if in the list buffer.
Otherwise, return value of ‘daemons--current-id’ variable (set by ‘daemons--run’)."
  (if (derived-mode-p 'tabulated-list-mode)
      (tabulated-list-get-id)
    daemons--current-id))

(defun daemons--insert-header (text)
  "Insert an underlined TEXT header into the buffer."
  (insert (concat (propertize text 'face 'underline) "\n\n")))

(defun daemons--switch-output-buffer-create (hostname)
  "Switch to output buffer for HOSTNAME if it exists, else create it and switch."
  (let ((output-buffer-name (daemons--get-output-buffer-name hostname)))
    (when (not (equal (buffer-name) output-buffer-name))
      (switch-to-buffer-other-window output-buffer-name))))

(defun daemons--run-with-output-buffer (command daemon-name)
  "Run the given COMMAND on DAEMON-NAME.  Show results in an output buffer.

The output buffer is in `daemons-output-mode' and will be switched to if not active."
  (let ((hostname (daemons--get-user-and-hostname default-directory)))
    (with-current-buffer (get-buffer-create (daemons--get-output-buffer-name hostname))
      (setq buffer-read-only nil
            daemons--current-id daemon-name)
      (when daemons-always-sudo (daemons--sudo))
      (delete-region (point-min) (point-max))
      (daemons--insert-header (format "Output of `%s` on `%s` (%s):" command daemon-name hostname))
      (daemons--run command daemon-name)
      (daemons-output-mode))
    (daemons--switch-output-buffer-create hostname)))

(defun daemons--run (command daemon-name)
  "Run the given COMMAND on DAEMON-NAME.  Insert the results into the current buffer."
  (let ((command-fun (daemons--command command (daemons-init-system-submodule))))
    (when (not command-fun)
      (error "No such daemon command: %s" command))
    (daemons--shell-command (funcall command-fun daemon-name) t)))

(defun daemons-init-system-submodule ()
  "Test each installed submodule to find the appropriate one for this system."
  (seq-do (lambda (submodule) (ignore-errors (require submodule)))
          daemons-init-system-submodules)
  (or
   (seq-find 'daemons--test-submodule daemons-init-system-submodules)
   (error "I'm sorry, your init system isn't supported yet!")))

(defun daemons--get-submodule (name)
  "Get the submodule definition for NAME."
  (alist-get name daemons--init-system-submodules-alist))

(defun daemons--test-submodule (name)
  "Execute the submodule test for NAME."
  (let ((submodule (daemons--get-submodule name)))
    (funcall (plist-get submodule :test))))

(defun daemons--list (submodule-name)
  "Return the list of all daemons for SUBMODULE-NAME.

It should return a list in the right format for `tabulated-list-entries'.

The precise format of the results will depend on the specific subcommand.
It will be different for different init systems, and must match
`daemons--list-headers'."
  (let ((submodule (daemons--get-submodule submodule-name)))
    (funcall (plist-get submodule :list))))

(defun daemons--list-headers (submodule-name)
  "Return the headers for the list of all daemons for SUBMODULE-NAME.

It should return a vector in the right format for `tabulated-list-format'.

The results will correspond to the format of each item in `daemons--list'."
  (let ((submodule (daemons--get-submodule submodule-name)))
    (funcall (plist-get submodule :headers))))

(defun daemons--commands-alist (submodule-name)
  "Get the daemons commands alist for SUBMODULE-NAME.

The car of each pair is the command symbol (e.g. 'stop).
The cdr of each pair is a function taking a daemon name and returning a shell
command to execute.

e.g. '((start . (lambda (x) (format \"service %s start\" x)))
       (stop . (lambda (x) (format \"service %s stop\" x))))"
  (let  ((submodule (daemons--get-submodule submodule-name)))
    (plist-get submodule :commands)))

(defun daemons--command (command submodule-name)
  "Get the daemons COMMAND for SUBMODULE-NAME."
  (alist-get command (daemons--commands-alist submodule-name)))

(defun daemons--sudo ()
  "Become root using TRAMP (if on local system).

Switches to a temporary directory to minimise damage potential.

Note that this only works on the local system, not remote systems.  For a remote
system you need to specify your own TRAMP path with a privileged user.

e.g. /ssh:me@example.com|sudo:example.com:"
  (unless (daemons--using-tramp-path-p default-directory)
    (let ((tempdir (daemons--shell-command-to-string "mktemp -d")))
      (cd (format "/sudo::%s" tempdir)))))

(defun daemons--refresh-list ()
  "Refresh the list of daemons."
  (let ((hostname (daemons--get-user-and-hostname default-directory)))
    (with-current-buffer (get-buffer-create (daemons--get-list-buffer-name hostname))
      (setq-local tabulated-list-entries (lambda () (daemons--list (daemons-init-system-submodule)))))))

;; interactive functions
(defun daemons-status-at-point (name)
  "Show the status of the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'status name))

(defun daemons-start-at-point (name)
  "Start the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'start name))

(defun daemons-stop-at-point (name)
  "Stop the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'stop name))

(defun daemons-restart-at-point (name)
  "Restart the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'restart name))

(defun daemons-reload-at-point (name)
  "Reload the daemon NAME at point in the daemons buffer."
  (interactive (list (daemons--daemon-at-point)))
  (daemons--run-with-output-buffer 'reload name))

(defun daemons--completing-read ()
  "Call `completing-read' with the current daemons list."
  ;; TODO some caching
  (completing-read "Daemon name: " (daemons--list (daemons-init-system-submodule))))

;;;###autoload
(defun daemons-status (name)
  "Show the status of the daemon NAME."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'status name))

;;;###autoload
(defun daemons-start (name)
  "Start the daemon NAME.  Show results in an output buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'start name))

;;;###autoload
(defun daemons-stop (name)
  "Stop the daemon NAME.  Show results in an output buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'stop name))

;;;###autoload
(defun daemons-restart (name)
  "Restart the daemon NAME.  Show results in an ouptut buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'restart name))

;;;###autoload
(defun daemons-reload (name)
  "Reload the daemon NAME.  Show results in an output buffer."
  (interactive (list (daemons--completing-read)))
  (daemons--run-with-output-buffer 'reload name))

;;;###autoload
(defun daemons ()
  "Open the list of system daemons (services) for user management.

This opens a ‘daemons-mode’ list buffer.  Move the cursor to a daemon line and
execute one of the commands in `describe-mode' to show status and manage the
state of the daemon."
  (interactive)
  (let* ((hostname (daemons--get-user-and-hostname default-directory))
         (list-buffer (get-buffer-create (daemons--get-list-buffer-name hostname))))
    (with-current-buffer list-buffer
      (display-buffer-pop-up-window list-buffer nil)
      (switch-to-buffer-other-window list-buffer)
      (when daemons-always-sudo (daemons--sudo))
      (daemons-mode)
      (daemons--refresh-list)
      (tabulated-list-print t t))))

;; macros
(defmacro daemons-define-submodule (name docstring &rest forms)
  "Define a new daemons submodule called NAME, described by DOCSTRING.

FORMS begins with a plist with these properties:

:test - An expression to evaluate that will return true if this submodule is
        appropriate for this system

        This could be something like:
        (and (eq system-type 'gnu/linux)
             (eq 0 (daemons--shell-command \"which service\")))

:commands - An alist of user commands (see `daemons--commands-alist')

:list - An expression to get daemons list (see `daemons--list')

:headers - An expression to get list headers (see `daemons--list-headers')

The remainder of FORMS will be ignored."
  (declare (indent defun))
  (let ((submodule-props-plist (seq-take forms 8)))
    (when (or (not (equal 8 (length submodule-props-plist)))
              (not (plist-member submodule-props-plist :test))
              (not (plist-member submodule-props-plist :commands))
              (not (plist-member submodule-props-plist :list))
              (not (plist-member submodule-props-plist :headers)))
      (error "The submodule definition is not complete"))
    `(map-put daemons--init-system-submodules-alist (quote ,name)
              (list
               :docstring ,docstring
               :test (lambda () ,(plist-get submodule-props-plist :test))
               :commands ,(plist-get submodule-props-plist :commands)
               :list (lambda () ,(plist-get submodule-props-plist :list))
               :headers (lambda () ,(plist-get submodule-props-plist :headers))))))

;; mode definitions
(define-derived-mode daemons-mode tabulated-list-mode
  "Daemons"
  "UI for viewing and controlling system daemons"
  :group 'daemons
  (setq tabulated-list-format (daemons--list-headers (daemons-init-system-submodule))
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'daemons--refresh-list)
  (tabulated-list-init-header))

(define-derived-mode daemons-output-mode special-mode
  "Daemons Output"
  "Mode for displaying output of Daemons commands"
  :group 'daemons)

(provide 'daemons)
;;; daemons.el ends here
