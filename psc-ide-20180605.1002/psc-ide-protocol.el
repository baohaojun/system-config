;;; -*- lexical-binding: t -*-
;; License: GNU General Public License version 3, or (at your option) any later version
(require 'json)
(require 'dash-functional)
(require 's)

(defun psc-ide-send-sync (cmd)
  (with-temp-buffer
    (condition-case err
        (let ((proc (make-network-process
                     :name "psc-ide-server"
                     :buffer (buffer-name (current-buffer))
                     :family 'ipv4
                     :host psc-ide-host
                     :service psc-ide-port)))
          (process-send-string proc (s-prepend cmd "\n"))

          (let ((timed-out nil))
            ;; As long as the process is running and we're not timed out
            (while (not (or (string= (process-status proc) "closed") timed-out))
              ;; Wait for the process in a blocking manner for a maximum of 2
              ;; seconds, and if we don't receive any output, set timed-out
              (unless (accept-process-output proc 2)
                (setq timed-out t))))

          (delete-process proc)
          (json-read-from-string (-first-item (s-lines (buffer-string)))))
      (error
       (error
        (s-join " "
                '("It seems like the server is not running. You can"
                  "start it using psc-ide-server-start.")))))))

(defun psc-ide-send (cmd callback)
  (let ((buffer (generate-new-buffer "*psc-ide-network-proc*")))
    (condition-case err
        (let ((proc (make-network-process
                     :name "psc-ide-server"
                     :buffer buffer
                     :family 'ipv4
                     :host psc-ide-host
                     :service psc-ide-port
                     :sentinel (-partial 'wrap-psc-ide-callback callback buffer (current-buffer)))))
          (process-send-string proc (s-prepend cmd "\n")))
      ;; Catch all the errors that happen when trying to connect
      (error
       (progn
         (kill-buffer buffer)
         (error
          (s-join " "
                  '("It seems like the server is not running. You can"
                    "start it using psc-ide-server-start."))))))))

(defun wrap-psc-ide-callback (callback buffer current proc status)
  "Wraps a function that expects a parsed psc-ide response.
Evaluates the CALLBACK in the context of the CURRENT buffer that initiated call if it still exists."
  (when (string= "closed" (process-status proc))
    (let ((parsed
           (with-current-buffer buffer
             (json-read-from-string
              (buffer-substring (point-min) (point-max))))))
        (kill-buffer buffer)
        (when (buffer-live-p current)
          (with-current-buffer current
            (funcall callback parsed))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Protocol commands.

;; TODO localise
(defvar psc-ide-command-cwd (json-encode (list :command "cwd")))
(defvar psc-ide-command-quit (json-encode (list :command "quit")))
(defvar psc-ide-command-load-all (json-encode (list :command "load")))

(defun psc-ide-command-load (modules deps)
  (json-encode
   (list :command "load"
         :params (list
                  :modules modules
                  :dependencies deps ))))

(defun psc-ide-command-show-type (filters search &optional module)
  (json-encode
   (list :command "type"
         :params (-filter #'identity
                          `(,@(when filters (list :filters filters))
                            ,@(when search (list :search search))
                            ,@(when module (list :currentModule module)))))))

(defun psc-ide-command-complete (filters &optional matcher module options)
  (json-encode
   (list :command "complete"
         :params (-filter #'identity
                          `(,@(when filters (list :filters filters))
                            ,@(when matcher (list :matcher matcher))
                            ,@(when module (list :currentModule module))
                            ,@(when options (list :options options)))))))

(defun psc-ide-command-case-split (line begin end type)
  (json-encode
   (list :command "caseSplit"
         :params (list
                  :line line
                  :begin begin
                  :end end
                  :annotations json-false
                  :type type ))))

(defun psc-ide-command-add-clause (line annotations)
  (json-encode
   (list :command "addClause"
         :params (list
                  :line line
                  :annotations (if annotations t json-false)))))

(defun psc-ide-command-add-import (identifier &optional filters file outfile)
  (json-encode
   (list :command "import"
         :params (list
                  :file (or file (buffer-file-name))
                  :outfile (or outfile (buffer-file-name))
                  :filters filters
                  :importCommand (list
                                  :importCommand "addImport"
                                  :identifier identifier)))))

(defun psc-ide-command-add-qualified-import (modulename qualifier &optional file outfile)
  (json-encode
   (list :command "import"
         :params (list
                  :file (or file (buffer-file-name))
                  :outfile (or outfile (buffer-file-name))
                  :importCommand (list
                                  :importCommand "addQualifiedImport"
                                  :module modulename
                                  :qualifier qualifier)))))

(defun psc-ide-command-rebuild (&optional filepath actualFile)
  (json-encode
   (list :command "rebuild"
         :params (-filter #'identity
                          `(,@(list :file (or filepath (buffer-file-name)))
                            ,@(when actualFile (list :actualFile actualFile)))))))

(defun psc-ide-command-list-imports (&optional filepath)
  (json-encode
   (list :command "list"
         :params (list
                  :type "import"
                  :file (or filepath (buffer-file-name (current-buffer)))))))

(defun psc-ide-command-usages (module namespace identifier)
  (json-encode
   (list :command "usages"
         :params (list
                  :module module
                  :namespace namespace
                  :identifier identifier))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Protocol utilities.

(defun psc-ide-generic-filter (name params)
   (list :filter name
         :params params))

(defun psc-ide-filter-exact (filter-str)
  (psc-ide-generic-filter "exact" (list :search filter-str)))

(defun psc-ide-filter-prefix (prefix-str)
  (psc-ide-generic-filter "prefix" (list :search prefix-str)))

(defun psc-ide-filter-modules (modules-list)      ;; modules without dependencies
  (psc-ide-generic-filter "modules" (list :modules modules-list)))

(defun psc-ide-filter-dependencies (modules-list) ;; modules with dependencies
  (psc-ide-generic-filter "dependencies" (list :modules modules-list)))


(defun psc-ide-generic-matcher (name params)
   (list :matcher name
         :params params))

(defun psc-ide-matcher-flex (match-str)
  (psc-ide-generic-matcher "flex" (list :search match-str)))

(defun psc-ide-matcher-distance (match-str max-dist)
  (psc-ide-generic-matcher "distance" (list :search match-str
                                            :maxDist max-dist)))

(defun psc-ide-completion-options (&optional max-results group-reexports)
  (-filter #'identity
           `(,@(when max-results (list :maxResults max-results))
             ,@(when group-reexports (list :groupReexports group-reexports)))))

(defun psc-ide-unwrap-result (res)
  "Unwraps the result from psc-ide and in case of an error throws it"
  (let ((result-type (cdr (assoc 'resultType res)))
        (result (cdr (assoc 'result res))))
    (if (string= result-type "error") (error "%s" result) result)))

(provide 'psc-ide-protocol)
