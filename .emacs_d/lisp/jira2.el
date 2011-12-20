;;; jira2.el -- Provide connectivity to JIRA SOAP service

;; Copyright (C) 2009  Alex Harsanyi <AlexHarsanyi@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Alexandru Harsanyi (AlexHarsanyi@gmail.com)
;; Created: December, 2009
;; Keywords: soap, web-services, jira
;; Homepage: http://code.google.com/p/emacs-soap-client

;; This file provides a programatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; Jira References:
;;
;; http://confluence.atlassian.com/display/JIRA/Creating+a+SOAP+Client
;;
;; JavaDoc for the Jira SOAP service
;; http://docs.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/soap/JiraSoapService.html

(require 'cl)
(require 'soap-client)

(defgroup jira2 nil
  "Jira2 customization group."
  :group 'applications)

(defgroup jira2-faces nil 
  "Faces for displaying Jira2 information."
  :group 'jira2)

(defcustom jira2-url ""
  "User customizable URL to Jira2 server."
  :group 'jira2
  :type 'string
  :initialize 'custom-initialize-set)

(defcustom jira2-host ""
  "User customizable host name of the Jira2 server, will be used to compute jira2-url if the latter is not set."
  :group 'jira2
  :type 'string
  :initialize 'custom-initialize-set)

(defface jira2-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira2-faces)

(defface jira2-issue-info-header-face
  '((t (:bold t :inherit 'jira2-issue-info-face)))
  "Base face for issue headers."
  :group 'jira2-faces)

(defface jira2-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira2-faces)

(defface jira2-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira2-faces)

(defface jira2-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira2-faces)

(defface jira2-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira2-faces)

(defface jira2-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jira2-faces)

(defface jira2-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jira2-faces)

(defvar jira2-mode-hook nil)

(defvar jira2-mode-map nil)

(defcustom jira2-wsdl-descriptor-url
  "http://bible/jira/rpc/soap/jirasoapservice-v2?wsdl"
  "The location for the WSDL descriptor for the JIRA service.
This is specific to your local JIRA installation.  The URL is
tipically:

  http://YOUR_INSTALLATION/rpc/soap/jirasoapservice-v2?wsdl

The default value works if JIRA is located at a hostname named
'jira'."
  :type 'string
  :group 'jira2)

(defcustom jira2-host-url
  "http://bible/jira"
  "The address of the jira host."
  :type 'string
  :group 'jira2)

(defvar jira2-token nil
  "JIRA token used for authentication")

(defvar jira2-user-login-name nil
  "The name of the user logged into JIRA.
This is maintained by `jira2-login'.")

(defvar jira2-wsdl nil)



;;; jira2.el --- Connect to JIRA2 issue tracking software

;; Copyright (C) 2009 Brian Zwahr
;; original Copyright (C) 2007  Dave Benjamin

;; Authors: 
;; Brian Zwahr <echosa@gmail.com>
;; Dave Benjamin <dave@ramenlabs.com>
;; Version: 0.3.3
;; Last modified: October 12, 2009

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; **********
;;; Commentary
;;; **********

;; This file provides jira2-mode, an emacs major mode for connecting to and 
;; using a Jira2 server. (http://www.atlassian.com/software/jira/). This 
;; jira2-mode is far from complete (more below) but is mostly usable as is 
;; for the features that are present.

;; Note that some functions/processes can be a bit slow. I believe this has 
;; something to do with XMLRPC.

;; Also, XMLRPC access to jira2 is incomplete. Certain Jira2 features cannot be 
;; used via XMLRPC such as (but not limited to):
;; - Changing ticket status
;; - Closing/resolving tickets
;; - Watching a ticket

;; All of the XML-RPC API is wrapped, though not all of the API is exposed
;; via interactive functions. For API details, see:

;; http://confluence.atlassian.com/pages/viewpage.action?pageId=1035
;; http://www.atlassian.com/software/jira/docs/api/rpc-jira2-plugin/latest/com/atlassian/jira/rpc/xmlrpc/XmlRpcService.html

;;; *************
;;; Configuration
;;; *************

;; 1.) Load the file jira2.el, either manuall or place (require 'jira2) in your .emacs with jira2.el in the load path.
;; 2.) Customize the variable jira2-url to point to the XML-RPC url of the Jira2
;; installation to be accessed.
;; 3.) The faces can be customized for different look/feel/highlighting.

;;; *****
;;; Usage
;;; *****

;; M-x jira2-mode will load the major mode into a new buffer named *Jira2*.
;; You will be asked to login; use the username/password for the Jira2 server.
;; A few internal lists should be populated automatically containing a list
;; of projects, issue types, etc. 

;; The following commands/keyboard shorcuts can be used:

;; li - jira2-list-issues
;; lp - jira2-list-projects
;; lf - jira2-list-filters
;; si - jira2-search-issues
;; sp - jira2-search-project-issues
;; i - jira2-show-issue
;; c - jira2-create-ticket
;; o - jira2-comment-ticket
;; r - jira2-refresh-ticket
;; a - jira2-assign-ticket
;; n - jira2-next-comment
;; p - jira2-previous-comment
;; jl - jira2-login
;; jL - jira2-logout
;; Q - jira2-mode-quit

;; When viewing an issues, pressing o, r, etc. acts upon that issue. 
;; For instance, while viewing an issue, pressing o will ask for a comment. 
;; That comment will be posted to the issue currently being viewed.

;; Some prompts have tab completions in the minibuffer/echo area. Try it out to
;; see which prompts do and which do not.

;;; Code:


;; **************************
;; Jira2 Mode - by Brian Zwahr
;; **************************

(defun jira2-load-wsdl ()
  "Load the JIRA WSDL descriptor."
  (setq jira2-wsdl (soap-load-wsdl-from-url jira2-wsdl-descriptor-url)))

(defun jira2-login (username password)
  "Login into JIRA and store the authentication token in `jira2-token'"
  ;; NOTE that we cannot rely on `jira2-call' because `jira2-call' relies on
  ;; us ;-)
  (interactive (let ((found (nth 0 (auth-source-search :max 1
                                           :host jira2-host
                                           :port 80
                                           :require '(:user :secret)
                                           :create t)))
	  user secret)
      (when found
	  (setq user (plist-get found :user)
		secret
		(let ((sec (plist-get found :secret)))
		  (if (functionp sec)
		      (funcall sec)
		    sec)))
	  (list user secret))))
  (unless jira2-wsdl 
    (jira2-load-wsdl))
  (setq jira2-token 
        (car (soap-invoke jira2-wsdl "jirasoapservice-v2" "login" username password)))
  (setq jira2-user-login-name username)

  ;; At this poing, soap-invoke didn't raise an error, so the login
  ;; credentials are OK.  use them to log into the web interface as
  ;; well, as this will be used to link issues (an operation which is
  ;; not exposed to the SOAP interface.  
  ;;
  ;; Note that we don't validate the response at all -- not sure how we
  ;; would do it...
  
  (let ((url (concat jira2-host-url "/secure/Dashboard.jspa?"
                     (format "&os_username=%s&os_password=%s&os_cookie=true" 
                             username password))))
    (let ((url-request-method "POST")
          (url-package-name "Emacs jira2.el")
          (url-package-version "1.0")
          (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
          (url-request-data "abc")
          (url-request-coding-system 'utf-8)
          (url-http-attempt-keepalives t))
      (let ((buffer (url-retrieve-synchronously url)))
        ;; This is just a basic check that the page was retrieved
        ;; correctly.  No error does not indicate a succesfull login,
        ;; we would have to parse the HTML page to find that out...
        (with-current-buffer buffer
          (declare (special url-http-response-status))
          (if (> url-http-response-status 299)
              (error "Error logging into JIRA Web interface %s" 
                     url-http-response-status)))
        (kill-buffer buffer)))))

(defun jira2-call (method &rest params)
  (car (apply 'jira2-call-it method params)))

(defun jira2-call-it (method &rest params)
  "Invoke the JIRA METHOD with supplied PARAMS.
This should be used for all JIRA inteface calls, as the method
ensures the user is logged in and invokes `soap-invoke' with the
correct service name and authentication token.

All JIRA inteface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter. For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `jira2-call', the call shoulbe be:

  (jira2-call \"getIssue\" KEY)
"
  (when (symbolp method)
    (setq method (symbol-name method)))
  (unless jira2-token
    (call-interactively 'jira2-login))
  (condition-case data
      (apply 'soap-invoke jira2-wsdl "jirasoapservice-v2" 
             method jira2-token params)
    (soap-error
     ;; If we are here, we had a token, but it expired.  Re-login and try
     ;; again.
     (setq jira2-token nil)
     (call-interactively 'jira2-login)
     (apply 'soap-invoke jira2-wsdl "jirasoapservice-v2" 
            method jira2-token params))))


;;;; Some utility functions

(defun jira2-make-assoc-list (data key-field value-field)
  "Create an association list from a SOAP structure array.

DATA is a list of association lists (a SOAP array-of type)
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"
  (loop for element in data
     collect (cons (cdr (assoc key-field element))
		   (cdr (assoc value-field element)))))

(defun jira2-make-remote-field-values (fields)
  "Transform a (KEY . VALUE) list into a RemoteFieldValue structure.

Each (KEY . VALUE) pair is transformed into 
 ((id . KEY) (values . (VALUE)))

This method exists because Several JIRA methods require a
RemoteFieldValue list, but it is easier to work with ALISTS in
emacs-lisp"
  (let ((remote-field-values))

    ;; we accept an ALIST of field-name field-values parameter, but we need to
    ;; construct a structure that encodes as a RemoteFieldValue which is what
    ;; updateIssue wants
    (dolist (field fields)
      (let ((name (car field))
            (value (cdr field)))
        (when (symbolp name)
          (setq name (symbol-name name)))
        ;; Value must be an "array" (for which soap-client accepts lists) even
        ;; if it is just one value
        (unless (vectorp value)
          (setq value (vector value)))
        (push `((id . ,name) (values . ,value)) 
              remote-field-values)))
    
    (apply 'vector (nreverse remote-field-values))))

;;;; Wrappers around JIRA methods

(defun jira2-get-issue (key)
  (car (jira2-call "getIssue" key)))

(defun jira2-get-comments (key)
  (car (jira2-call "getComments" key)))

(defun jira2-add-comment (key comment)
  (car (jira2-call "addComment" key `((body . ,comment)))))

(defun jira2-update-issue (key fields)
  (car (jira2-call "updateIssue" key (jira2-make-remote-field-values fields))))

(defun jira2-create-issue (fields)
  (car (jira2-call "createIssue" fields)))

(defvar jira2-status-codes-cache nil)

(defun jira2-get-statuses ()
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-status-codes-cache
    (setq jira2-status-codes-cache
	  (jira2-make-assoc-list (car (jira2-call "getStatuses")) 'id 'name)))
  jira2-status-codes-cache)

(defvar jira2-issue-types-cache nil)

(defun jira2-get-issue-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-issue-types-cache
    (setq jira2-issue-types-cache
	  (jira2-make-assoc-list (car (jira2-call "getIssueTypes")) 'id 'name)))
  jira2-issue-types-cache)

(defvar jira2-priority-codes-cache nil)

(defun jira2-get-priorities ()
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-priority-codes-cache
    (setq jira2-priority-codes-cache
	  (jira2-make-assoc-list (car (jira2-call "getPriorities")) 'id 'name)))
  jira2-priority-codes-cache)

(defvar jira2-resolution-code-cache nil)

(defun jira2-get-resolutions ()
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-resolution-code-cache
    (setq jira2-resolution-code-cache
	  (jira2-make-assoc-list (car (jira2-call "getResolutions")) 'id 'name)))
  jira2-resolution-code-cache)

(defvar jira2-issue-regexp nil)

;; NOTE: it is not such a good ideea to use this, as it needs a JIRA
;; connection to construct the regexp (the user might be prompted for a JIRA
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.

(defun jira2-get-issue-regexp ()
  "Return a regexp that matches an issue name.
The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless jira2-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (car (jira2-call 'getProjectsNoSchemes)))))
      (setq jira2-issue-regexp (concat "\\<" (regexp-opt projects) "-[0-9]+\\>"))))
  jira2-issue-regexp)

(defun jira2-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that JIRA
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query." 
  (unless (or limit (numberp limit))
    (setq limit 100))
  (car (jira2-call "getIssuesFromJqlSearch" jql limit)))

(defun jira2-get-available-actions (issue-key)
  "Return the available workflow actions for ISSUE-KEY.
This runs the getAvailableActions SOAP method."
  (jira2-make-assoc-list 
   (car (jira2-call "getAvailableActions" issue-key))
   'id 'name))

(defun jira2-get-fields-for-action (issue-key action-id)
  "Return the required fields for the ACTION-ID."
  (jira2-make-assoc-list
   (car (jira2-call "getFieldsForAction" issue-key action-id))
   'id 'name))

(defun jira2-progress-workflow-action (issue-key action-id params)
  (car (jira2-call "progressWorkflowAction" issue-key action-id params)))

(defun jira2-add-worklog-and-autoadjust-remaining-estimate (issue-key start-date time-spent comment)
  "Log time spent on ISSUE-KEY to its worklog.
The time worked begings at START-DATE and has a TIME-SPENT
duration. JIRA will automatically update the remaining estimate
by subtracting TIME-SPENT from it.

START-DATE should be in the format 2010-02-05T14:30:00Z 

TIME-SPENT can be in one of the following formats: 10m, 120m
hours; 10h, 120h days; 10d, 120d weeks."
  (car (jira2-call "addWorklogAndAutoAdjustRemainingEstimate"
                   issue-key
                   `((startDate . ,start-date)
                     (timeSpent . ,time-spent)
                     (comment   . ,comment)))))

(defun jira2-link-issue (issue-key link-type other-issue-key)
  "Create a link between ISSUE-KEY and OTHER-ISSUE-KEY.
LINK-TYPE is a string representing the type of the link, e.g
\"requires\", \"depends on\", etc.  I believe each JIRA
installation can define its own link types."
  
  ;; IMPLEMENTATION NOTES: The linking jira issues functionality is
  ;; not exposed through the SOAP api, we must use the web interface
  ;; to do the linking.  Unfortunately, we cannot parse the result, so
  ;; we don't know that the linking was succesfull or not.  To reduce
  ;; the risk, we use the SOAP api to retrieve the issues for
  ;; ISSUE-KEY and OTHER-ISSUE-KEY.  This will ensure that we are
  ;; logged in (see also jira2-login) and that both issues exist. We
  ;; don't validate the LINK-TYPE, not sure how to do it.
  ;;

  (let ((issue (jira2-get-issue issue-key))
        (other-issue (jira2-get-issue other-issue-key)))
    (let ((url (concat jira2-host-url 
                       "/secure/LinkExistingIssue.jspa?"
                       (format "linkDesc=%s&linkKey=%s&id=%s&Link=Link" 
                               link-type other-issue-key (cdr (assq 'id issue))))))
      (let ((url-request-method "POST")
            (url-package-name "Emacs scratch.el")
            (url-package-version "1.0")
            (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
            (url-request-data "abc")
            (url-request-coding-system 'utf-8)
            (url-http-attempt-keepalives t)
            ;; see http://confluence.atlassian.com/display/JIRA/Form+Token+Handling
            (url-request-extra-headers '(("X-Atlassian-Token" . "no-check"))))

       (let ((buffer (url-retrieve-synchronously url)))
        ;; This is just a basic check that the page was retrieved
         ;; correctly.  No error does not indicate a success as we
         ;; have to parse the HTML page to find that out...
         (with-current-buffer buffer
           (declare (special url-http-response-status))
           (if (> url-http-response-status 299)
               (error "Error linking issue through JIRA Web interface %s" 
                      url-http-response-status)))
           (kill-buffer buffer))))))


;................................................ issue field accessors ....

(defun jira2-issue-key (issue)
  "Return the key of ISSUE."
  (cdr (assoc 'key issue)))

(defun jira2-issue-owner (issue)
  "Return the owner of ISSUE."
  (cdr (assq 'assignee issue)))

(defun jira2-issue-status (issue)
  "Return the status of ISSUE as a status string (not as a number!)"
  (let ((status-code (cdr (assq 'status issue))))
    (cdr (assoc status-code (jira2-get-statuses)))))

(defun jira2-custom-field-value (custom-field issue)
  "Return the value of CUSTOM-FIELD for ISSUE.
Return nil if the field is not found"
  (catch 'found
    (dolist (field (cdr (assq 'customFieldValues issue)))
      (when (equal (cdr (assq 'customfieldId field)) custom-field)
        (throw 'found (cadr (assq 'values field)))))))
  


(if jira2-mode-map
    nil
  (progn
    (setq jira2-mode-map (make-sparse-keymap))
    (define-key jira2-mode-map "li" 'jira2-list-issues)
    (define-key jira2-mode-map "lp" 'jira2-list-projects)
    (define-key jira2-mode-map "lf" 'jira2-list-filters)
    (define-key jira2-mode-map "si" 'jira2-search-issues)
    (define-key jira2-mode-map "sp" 'jira2-search-project-issues)
    (define-key jira2-mode-map "i" 'jira2-show-issue)
    (define-key jira2-mode-map "c" 'jira2-create-ticket)
    (define-key jira2-mode-map "o" 'jira2-comment-ticket)
    (define-key jira2-mode-map "r" 'jira2-refresh-ticket)
    (define-key jira2-mode-map "a" 'jira2-assign-ticket)
    (define-key jira2-mode-map "n" 'jira2-next-comment)
    (define-key jira2-mode-map "p" 'jira2-previous-comment)
    (define-key jira2-mode-map "jl" 'jira2-login)
    (define-key jira2-mode-map "jL" 'jira2-logout)
    (define-key jira2-mode-map "Q" 'jira2-mode-quit)
    (define-key jira2-mode-map [return] 'jira2-return)))

(defun jira2-mode ()
  "A mode for working with the Jira2 ticketing system. XMLRPC is used via xmlrpc.el. Things run a bit slow, though sometimes they seems to run faster when doing multiple things at once to the same ticket: i.e. retrieve a ticket, its slow, comment the tickets, its faster, refresh the ticket its faster, wait a while then refresh and its slow again. 

\\{jira2-mode-map}"
  (interactive)
  (if (and (or (not jira2-host)
	      (equal jira2-host ""))
	   (or (equal jira2-url nil)
	       (equal jira2-url "")))
      (message "jira2-url not set! Please use 'M-x customize-variable RET jira2-url RET'!")
    (when (or (equal jira2-url nil)
	      (equal jira2-url ""))
      (setq jira2-url (concat "http://" jira2-host "/jira/rpc/xmlrpc")))
    (progn
      (switch-to-buffer "*Jira2*")
      (kill-all-local-variables)
      (setq major-mode 'jira2-mode)
      (setq mode-name "Jira2")
      (use-local-map jira2-mode-map)
      (run-hooks 'jira2-mode-hook)
      (jira2-store-projects)
      (jira2-store-priorities)
      (jira2-store-statuses)
      (jira2-store-types)
      (insert "Welcome to jira2-mode!")
      (message "jira2 mode loaded!"))))

(defvar jira2-current-issue nil
  "This holds the currently selected issue.")

(defvar jira2-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira2-types nil
  "This holds a list of issues types.")

(defvar jira2-statuses nil
  "This holds a list of statuses.")

(defvar jira2-priorities nil
  "This holds a list of priorities.")

(defvar jira2-user-fullnames nil
  "This holds a list of user fullnames.")

(defun jira2-mode-quit ()
  (interactive)
  (jira2-logout)
  (kill-buffer "*Jira2*"))

(defun jira2-create-ticket (project type summary description)
  (interactive (list (read-string "Project: ")
                     (read-string "Type: ")
                     (read-string "Summary: ")
                     (read-string "Description: ")))
  (if (or (equal project "")
          (equal type "")
          (equal summary "")
          (equal description ""))
      (message "Must provide all information!")
    (progn
      (setq ticket-alist (list (cons "project" project) 
                               (cons "type" type) 
                               (cons "summary" summary) 
                               (cons "description" description)))
      (jira2-create-issue ticket-alist))))

(defun jira2-refresh-ticket ()
  (interactive)
  (jira2-show-issue jira2-current-issue))

(defun jira2-comment-ticket (comment)
  (interactive (list (read-string "Comment: ")))
  (if (equal comment "")
      (message "Must provide comment!")
    (progn
      (jira2-add-comment jira2-current-issue comment)
      (jira2-refresh-ticket))))

(defun jira2-assign-ticket (assignee)
  (interactive (list (read-string "Assignee: ")))
  (if (equal assignee "")
      (message "Must provide assignee!")
    (progn
      (setq ticket-alist (list (cons "assignee" (vector assignee))))
      (jira2-update-issue jira2-current-issue ticket-alist)
      (jira2-refresh-ticket))))

(defun jira2-update-ticket-summary (summary)
  (interactive (list (read-string "Summary: ")))
  (if (equal summary "")
      (message "Must provide summary!")
    (progn
      (setq ticket-alist (list (cons "summary" (vector summary))))
      (jira2-update-issue jira2-current-issue ticket-alist)
      (jira2-refresh-ticket))))

(defun jira2-start-ticket ()
  (interactive)
  (setq ticket-alist (list (cons "status" (vector "3"))))
  (jira2-update-issue jira2-current-issue ticket-alist))

(defun jira2-store-projects ()
  (setf jira2-projects-list (jira2-get-projects)))

(defun jira2-store-types ()
  (setf jira2-types (jira2-get-issue-types)))

(defun jira2-store-statuses ()
  (setf jira2-statuses (jira2-get-statuses)))

(defun jira2-store-priorities ()
  (setf jira2-priorities (jira2-get-priorities)))

(defun jira2-get-project-name (key)
  (let ((projects jira2-projects-list)
        (name nil))
    (dolist (project projects)
      (if (equal (cdr (assoc 'key project)) key)
          (setf name (cdr (assoc 'name project)))))
    name))

(defun jira2-get-type-name (id)
  (let ((types jira2-types)
        (name nil))
    (dolist (type types)
      (if (equal (cdr (assoc 'id type)) id)
          (setf name (cdr (assoc 'name type)))))
    name))

(defun jira2-get-status-name (id)
  (let ((statuses jira2-statuses)
        (name nil))
    (dolist (status statuses)
      (if (equal (cdr (assoc 'id status)) id)
          (setf name (cdr (assoc 'name status)))))
    name))

(defun jira2-get-priority-name (id)
  (let ((priorities jira2-priorities)
        (name nil))
    (dolist (priority priorities)
      (if (equal (cdr (assoc 'id priority)) id)
          (setf name (cdr (assoc 'name priority)))))
    (message name)))

(defun jira2-get-user-fullname (username)
  (if (assoc username jira2-user-fullnames)
      (cdr (assoc username jira2-user-fullnames))
    (progn
      (let ((user (jira2-get-user username)))
        (setf jira2-user-fullnames (append jira2-user-fullnames (list (cons username (cdr (assoc 'fullname user))))))
        (cdr (assoc 'fullname user))))))

(defun jira2-next-comment ()
  (interactive)
  (let ((p (point)))
    (if (search-forward "Comment #" nil t)
        (progn
          (if (equal p (- (point) 9))
              (search-forward "Comment #" nil t))
          (recenter 0)
          (beginning-of-line)))))

(defun jira2-previous-comment ()
  (interactive)
  (if (search-backward "Comment #" nil t)
      (progn
        (recenter 0)
        (beginning-of-line))
    (goto-char 0)))

(defun jira2-return ()
  (interactive)
  (if (equal (face-at-point) 'jira2-link-issue-face)
      (jira2-show-issue (thing-at-point 'sexp)))
  (if (equal (face-at-point) 'jira2-link-project-face)
      (jira2-search-project-issues (thing-at-point 'sexp) "" 20))
  (if (equal (face-at-point) 'jira2-link-filter-face)
      (jira2-list-issues (thing-at-point 'sexp))))

(defun point-on-issue-p ()
  (save-excursion
    (search-backward " ")))

(defun delete-eob-whitespace ()
  (end-of-buffer)
  (delete-horizontal-space)
  (delete-char -1)
  (beginning-of-buffer))

;; ***********************************
;; original functions by Dave Benjamin
;; modifications by Brian Zwahr noted
;; ***********************************

(defun jira2-logout ()
  "Logs the user out of JIRA2"
  (interactive)
  (jira2-call 'logout)
  (setq jira2-token nil))

(defun jira2-list-projects ()
  "Displays a list of all available JIRA2 projects"
  (interactive)
  (let ((projects (jira2-get-projects)))
    (jira2-with-jira2-buffer
     (insert (number-to-string (length projects)) " JIRA2 projects found:\n\n")
     (dolist (project projects)
       (insert (format "%-12s" " "))
       (beginning-of-line)
       (add-text-properties
        (point)
        (save-excursion
          (insert 
           (cdr (assoc 'key project)))
          (point))
        '(face jira2-link-project-face))
       (beginning-of-line)
       (forward-char 12)
       (insert (format "%s\n"
                       (cdr (assoc 'name project)))))))
  (delete-eob-whitespace))

(defun jira2-list-filters ()
  "Displays a list of all saved JIRA2 filters"
  (interactive)
  (let ((filters (jira2-get-saved-filters)))
    (jira2-with-jira2-buffer
     (insert (number-to-string (length filters)) " JIRA2 filters found:\n\n")
     (dolist (filter filters)
       (insert (format "%-8s" " "))
       (beginning-of-line)
       (add-text-properties
        (point)
        (save-excursion
          (insert (cdr (assoc 'id filter)))
          (point))
        '(face jira2-link-filter-face))
       (beginning-of-line)
       (forward-char 8)
       (insert (format " %s\n"
                       (cdr (assoc 'name filter)))))))
  (delete-eob-whitespace))

(defun jira2-list-issues (filter-id)
  "Displays a list of issues matching a filter"
  (interactive
   (list (let ((filter-alist (jira2-get-filter-alist)))
           (cdr (assoc (completing-read "Filter: " filter-alist nil t)
                filter-alist)))))
    (when filter-id
      (let ((filter (jira2-get-filter filter-id))
            (issues (jira2-get-issues-from-filter filter-id)))
        (jira2-with-jira2-buffer
         (insert "Filter:\n" (cdr (assoc 'name filter))
                 " (" (cdr (assoc 'id filter)) ")\n\n")
         (when (cdr (assoc 'description filter))
           (insert "Description:\n")
           (let ((start (point)))
             (insert (cdr (assoc 'description filter)) "\n\n")
             (fill-region start (point))))
         (jira2-display-issues issues)))))

(defun jira2-search-issues (text)
  "Displays a list of issues maching a fulltext search"
  (interactive "sSearch: ")
  (let ((issues (jira2-get-issues-from-text-search text)))
    (jira2-with-jira2-buffer
     (insert "Search: " text "\n\n")
     (jira2-display-issues issues))))

(defun jira2-search-project-issues (project text max-results)
  "Displays a list of issues within a project matching a fulltext search"
  (interactive
   (let ((project-keys
          (mapcar (lambda (project)
                    (cdr (assoc 'key project)))
                  (jira2-get-projects))))
     (list
      (completing-read "Project Key: " project-keys nil t)
      (read-string "Search: ")
      (read-number "Max Results: " 20))))
  (let ((issues (jira2-get-issues-from-text-search-with-project
                 (list project) (if (equal text "") " " text) max-results)))
    (jira2-with-jira2-buffer
     (insert "Project Key: " project "\n"
             "Search: " text "\n"
             "Max Results: " (number-to-string max-results) "\n\n")
     (jira2-display-issues issues))))

; Modified by Brian Zwahr to store issue key and improve layout/readability.
(defun jira2-show-issue (issue-key)
  "Displays details about a particular issue."
  (interactive "sIssue Key: ")
  (let ((issue (jira2-get-issue issue-key))
        (comments (jira2-get-comments issue-key)))
    (setf jira2-current-issue issue-key)
    (jira2-with-jira2-buffer
     (setq truncate-lines nil)
     (let ((s "Project:   "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (jira2-get-project-name (cdr (assoc 'project issue)))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Key:       "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (cdr (assoc 'key issue))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Type:      "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (jira2-get-type-name (cdr (assoc 'type issue)))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Status:    "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (jira2-get-status-name (cdr (assoc 'status issue)))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Priority:  "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (jira2-get-priority-name (cdr (assoc 'priority issue)))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Assignee:  "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (jira2-get-user-fullname (cdr (assoc 'assignee issue)))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Reporter:  "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (jira2-get-user-fullname (cdr (assoc 'reporter issue)))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Created:   "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (cdr (assoc 'created issue))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Updated:   "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (cdr (assoc 'updated issue))))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n"))

     (let ((s "Watchers:  "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s "N/A"))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n\n"))

     (let ((s "Component(s): "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (if (cdr (assoc 'name (cdr (assoc 'components issue)))) (cdr (assoc 'name (cdr (assoc 'components issue)))) "None")))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n\n"))

     (let ((s "Fix Version(s): "))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-header-face s)
       (insert s))
     (let ((s (if (cdr (assoc 'fixVersions issue)) (cdr (assoc 'fixVersions issue)) "None")))
       (put-text-property 0 (length s) 'face 'jira2-issue-info-face s)
       (insert s "\n\n"))

     (let ((s (cdr (assoc 'summary issue))))
       (put-text-property 0 (length s) 'face 'jira2-issue-summary-face s)
       (insert s "\n\n"))

     (insert (concatenate 'string (cdr (assoc 'description issue)) "\n\n"))

     (when comments
       (let ((count 1))
         (dolist (comment comments)
           (insert "Comment #" (int-to-string count) "\n")
           (let ((s (concatenate 'string (jira2-get-user-fullname (cdr (assoc 'author comment))) " - " (cdr (assoc 'created comment)))))
             (put-text-property 0 (length s) 'face 'jira2-comment-header-face s)
             (insert s "\n"))
           (let ((c (jira2-strip-cr (cdr (assoc 'body comment)))))
             
             (put-text-property 0 (length c) 'face 'jira2-comment-face c)
             (insert c "\n\n"))
           (setf count (1+ count))))))))

(defun jira2-send-region-as-comment (start end issue-key)
  "Send the currently selected region as an issue comment"
  (interactive "r\nsIssue Key: ")
  (jira2-add-comment issue-key (buffer-substring start end)))

(defun jira2-get-filter (filter-id)
  "Returns a filter given its filter ID."
  (flet ((id-match (filter)
                   (equal filter-id (cdr (assoc 'id filter)))))
    (find-if 'id-match (jira2-get-saved-filters))))

(defun jira2-get-filter-alist ()
  "Returns an association list mapping filter names to IDs"
  (mapcar (lambda (filter)
            (cons (cdr (assoc 'name filter))
                  (cdr (assoc 'id filter))))
          (jira2-get-saved-filters)))

(defun jira2-get-status-abbrevs ()
  "Returns an association list of status IDs to abreviated names"
  (flet ((pair (status)
               (cons (cdr (assoc 'id status))
                     (let ((status-name (cdr (assoc 'name status))))
                       (substring (replace-regexp-in-string
                                   " *" "" status-name)
                                  0 (min 3 (length status-name)))))))
    (mapcar 'pair (jira2-get-statuses))))

(defun jira2-display-issues (issues)
  "Inserts a list of issues into the current buffer"
  (let ((status-abbrevs (jira2-get-status-abbrevs))
        (last-status))
    (insert (number-to-string (length issues))
            " JIRA2 issues found:\n")
    (dolist (issue issues)
      (let ((status (cdr (assoc 'status issue)))
            (priority (cdr (assoc 'priority issue))))
        (when (not (equal last-status status))
          (setq last-status status)
          (insert "\n"))
        (insert (format "%-16s" " "))
        (beginning-of-line)
        (add-text-properties
         (point)
         (save-excursion
           (insert 
            (cdr (assoc 'key issue)))
           (point))
         '(face jira2-link-issue-face))
        (beginning-of-line)
        (forward-char 16)
        (insert (format "%-10s %s %5s %s\n"
                        (cdr (assoc 'assignee issue))
                        (cdr (assoc status status-abbrevs))
                        (if priority
                            (make-string (- 6 (string-to-number priority))
                                         ?*)
                          "")
                        (cdr (assoc 'summary issue)))))))
  (delete-eob-whitespace))

(defun jira2-add-comment (issue-key comment)
  "Adds a comment to an issue"
  (jira2-call 'addComment issue-key comment))

(defun jira2-create-issue (r-issue-struct)
  "Creates an issue in JIRA2 from a Hashtable object."
  (jira2-call 'createIssue r-issue-struct))

(defun jira2-get-comments (issue-key)
  "Returns all comments associated with the issue"
  (jira2-call 'getComments issue-key))

(defun jira2-get-components (project-key)
  "Returns all components available in the specified project"
  (jira2-call 'getComponents project-key))

(defun jira2-get-issue (issue-key)
  "Gets an issue from a given issue key."
  (jira2-call 'getIssue issue-key))

(defun jira2-get-issues-from-filter (filter-id)
  "Executes a saved filter"
  (jira2-call 'getIssuesFromFilter filter-id))

(defun jira2-get-issues-from-text-search (search-terms)
  "Find issues using a free text search"
  (jira2-call 'getIssuesFromTextSearch search-terms))

(defun jira2-get-issues-from-text-search-with-project
  (project-keys search-terms max-num-results)
  "Find issues using a free text search, limited to certain projects"
  (jira2-call 'getIssuesFromTextSearchWithProject
             project-keys search-terms max-num-results))

(defun jira2-get-issue-types ()
  "Returns all visible issue types in the system"
  (jira2-call 'getIssueTypes))

(defun jira2-get-priorities ()
  "Returns all priorities in the system"
  (jira2-call 'getPriorities))

;; Modified by Brian Zwahr to use getProjectsNoSchemes instead of getProjects
(defun jira2-get-projects ()
  "Returns a list of projects available to the user"
  (jira2-call "getProjectsNoSchemes"))

(defun jira2-get-resolutions ()
  "Returns all resolutions in the system"
  (jira2-call 'getResolutions))

(defun jira2-get-saved-filters ()
  "Gets all saved filters available for the currently logged in user"
  (jira2-call 'getSavedFilters))

(defun jira2-get-server-info ()
  "Returns the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jira2-call 'getServerInfo))

(defun jira2-get-statuses ()
  "Returns all statuses in the system"
  (jira2-call 'getStatuses))

(defun jira2-get-sub-task-issue-types ()
  "Returns all visible subtask issue types in the system"
  (jira2-call 'getSubTaskIssueTypes))

(defun jira2-get-user (username)
  "Returns a user's information given a username"
  (jira2-call 'getUser username))

(defun jira2-get-versions (project-key)
  "Returns all versions available in the specified project"
  (jira2-call 'getVersions project-key))

(defun jira2-update-issue (issue-key field-values)
  "Updates an issue in JIRA2 from a Hashtable object."
  (jira2-call 'updateIssue issue-key field-values))

(defun jira2-ensure-token ()
  "Makes sure that a JIRA2 token has been set, logging in if necessary."
  (unless jira2-token
    (let ((found (nth 0 (auth-source-search :max 1
                                           :host jira2-host
                                           :port 80
                                           :require '(:user :secret)
                                           :create t)))
	  user secret)
      (when found
	  (setq user (plist-get found :user)
		secret
		(let ((sec (plist-get found :secret)))
		  (if (functionp sec)
		      (funcall sec)
		    sec)))
	  (jira2-login user secret)))))


(defun jira2-strip-cr (string)
  "Removes carriage returns from a string"
  (when string (replace-regexp-in-string "\r" "" string)))

;; Modified by Brian Zwahr to a specific *Jira2* buffer, not a temp buffer
(defmacro jira2-with-jira2-buffer (&rest body)
  "Sends all output and buffer modifications to *Jira2* buffer."
  `(with-current-buffer "*Jira2*" 
     (delete-region (point-min) (point-max))
     (setq truncate-lines t)
     ,@body
     (beginning-of-buffer)))


(provide 'jira2)
