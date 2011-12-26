;;; org-jira.el --- blog from Org mode to wordpress

;; Author: Bao Haojun <baohaojun@gmail.com>

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

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


(require 'org)
(require 'jira2)

(defgroup org-jira nil
  "Post to weblogs from Emacs"
  :group 'org-jira)

(defcustom org-jira-serv-alist nil
  "Association list to set information for each jira server.
Each element of the alist is a jira server name.  The CAR of each
element is a string, uniquely identifying the server.  The CDR
of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the server.

     (:property value :property value ... )

When a property is given a value in org-jira-serv-alist, its
setting overrides the value of the corresponding user
variable (if any) during syncing.

Most properties are optional, but some should always be set:

  :url                     soap url of the jira server.
  :username                username to be used.
  :host                    hostname of the jira server (TODO: compute it from ~url~).

All the other properties are optional. They over-ride the global variables.

  :password                password to be used, will be prompted if missing.
"
  :group 'org-jira
  :type '(alist :value-type plist))


(defvar org-jira-serv nil
  "Parameters of the currently selected blog.")

(defvar org-jira-serv-name nil
  "Name of the blog, to pick from `org-jira-serv-alist'")

(defvar org-jira-projects-list nil
  "List of jira projects.")

(defvar org-jira-current-project nil
  "currently selected (i.e., active project).")

(defvar org-jira-issues-list nil
  "List of jira issues under the current project.")

(defvar org-jira-server-rpc-url nil
  "Jira server soap URL")

(defvar org-jira-server-userid nil
  "Jira server user id")

(defvar org-jira-proj-id nil
  "Jira project ID")

(defvar org-jira-entry-mode-map nil
  "Keymap for blog entry buffer")

(defvar org-jira-logged-in nil
  "Flag whether user is logged-in or not")

(defvar org-jira-buffer-name "*org-jira-%s*"
  "Name of the jira buffer")

(defvar org-jira-buffer-kill-prompt t
  "Ask before killing buffer")
(make-variable-buffer-local 'org-jira-buffer-kill-prompt)

(defconst org-jira-version "0.1"
  "Current version of org-jira.el")

(defvar org-jira-mode-hook nil
  "Hook to run upon entry into mode.")

(defun org-jira-kill-buffer-hook ()
  "Prompt before killing buffer."
  (if (and org-jira-buffer-kill-prompt
	   (not (buffer-file-name)))
    (if (y-or-n-p "Save Jira?")
        (progn
          (save-buffer)
          (org-jira-save-details (org-jira-parse-entry) nil
                                 (y-or-n-p "Published?"))))))

(unless org-jira-entry-mode-map
  (setq org-jira-entry-mode-map
	(let ((org-jira-map (make-sparse-keymap)))
	  (set-keymap-parent org-jira-map org-mode-map)
	  (define-key org-jira-map (kbd "C-c gp") 'org-jira-get-projects)
	  (define-key org-jira-map (kbd "C-c gi") 'org-jira-get-issues)
	  (define-key org-jira-map (kbd "C-c gc") 'org-jira-get-comments)
	  (define-key org-jira-map (kbd "C-c ni") 'org-jira-new-issue)
	  (define-key org-jira-map (kbd "C-c nc") 'org-jira-new-comment)
	  (define-key org-jira-map (kbd "C-c uc") 'org-jira-update-comment)
	  org-jira-map)))

;;;###autoload
(define-minor-mode org-jira-mode
  "Toggle org-jira mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org-jira-entry-mode-map}

Entry to this mode calls the value of `org-jira-mode-hook'."

  :init-value nil
  :lighter " oj"
  :group 'org-jira
  :keymap org-jira-entry-mode-map

  (if org-jira-mode
      (run-mode-hooks 'org-jira-mode-hook)))

(defun org-jira-get-projects ()
  "Get list of projects."
  (interactive)
  (save-excursion
    (let* ((oj-projs (jira2-get-projects)))
      (mapc (lambda (proj)
	      (let* ((proj-key (cdr (assoc 'key proj)))
		     (headline (concat "Project: " proj-key)))
		(save-restriction
		  (widen)
		  (goto-char (point-min))
		  (show-all)
		  (setq p (org-find-exact-headline-in-buffer headline))
		  (if (and p (>= p (point-min))
			   (<= p (point-max)))
		      (progn 
			(goto-char p) 
			(org-narrow-to-subtree)
			(end-of-line))
		    (goto-char (point-max))
		    (unless (looking-at "^")
		      (insert "\n"))
		    (insert "* ") 
		    (insert headline)
		    (org-narrow-to-subtree))
		  (org-entry-put (point) "name" (cdr (assoc 'name proj)))
		  (org-entry-put (point) "key" (cdr (assoc 'key proj)))
		  (org-entry-put (point) "lead" (cdr (assoc 'lead proj)))
		  (org-entry-put (point) "id" (cdr (assoc 'id proj)))
		  (org-entry-put (point) "url" (cdr (assoc 'url proj))))))
	    oj-projs))))

(defun org-jira-get-issues ()
  "Get list of issues."
  (interactive)
  (save-excursion
    (let* ((issues (jira2-get-issues-from-filter (cdr (assoc "my-open" (jira2-get-filter-alist))))))
      (mapc (lambda (issue)
	      (let* ((proj-key (cdr (assoc 'project issue)))
		     (proj-headline (concat "Project: " proj-key))
		     (issue-key (cdr (assoc 'key issue)))
		     (issue-headline (concat "Issue: " issue-key)))
		(save-restriction
		  (widen)
		  (goto-char (point-min))
		  (show-all)
		  (setq p (org-find-exact-headline-in-buffer proj-headline))
		  (save-restriction
		    (if (and p (>= p (point-min))
			     (<= p (point-max)))
		      (progn 
			(goto-char p) 
			(org-narrow-to-subtree)
			(end-of-line))
		    (error "Project %s not found" proj-key))
		  (setq p (org-find-exact-headline-in-buffer issue-headline))
		  (save-restriction
		    (if (and p (>= p (point-min))
			     (<= p (point-max)))
			(progn
			  (goto-char p)
			  (org-narrow-to-subtree)
			  (end-of-line))
		      (goto-char (point-max))
		      (unless (looking-at "^")
			(insert "\n"))
		      (insert "** ") 
		      (insert issue-headline)
		      (org-narrow-to-subtree))
		    (mapc (lambda (entry)
			    (org-entry-put (point) (symbol-name entry) (cdr (assoc entry issue))))
			  '(key assignee reporter type project priority resolution))

		  (mapc (lambda (heading-entry)
			  (save-restriction
			    (widen)
			    (goto-char (point-min))
			    (show-all)
			    (let* ((entry-heading (concat (symbol-name heading-entry) ": " issue-key)))
			    (setq p (org-find-exact-headline-in-buffer entry-heading))
			    (if (and p (>= p (point-min))
				     (<= p (point-max)))
				(progn
				  (goto-char p)
				  (org-narrow-to-subtree)
				  (delete-region (point-min) (point-max))))
			    (goto-char (point-max))
			    (unless (looking-at "^")
			      (insert "\n"))
			    (insert "*** ")
			    (insert entry-heading "\n")
			    (insert (or (cdr (assoc heading-entry issue)) "")))))
			'(summary description))
		  
		  (let* ((comments (jira2-get-comments issue-key)))
		    (mapc (lambda (comment)
			      (save-restriction
			    (let* ((comment-id (cdr (assoc 'id comment)))
				  (comment-headline (concat "Comment: " comment-id)))
			      (setq p (org-find-exact-headline-in-buffer comment-headline))
			      (if (and p (>= p (point-min))
				     (<= p (point-max)))
				  (progn
				    (goto-char p)
				    (org-narrow-to-subtree)
				    (delete-region (point-min) (point-max))))
			      (goto-char (point-max))
			      (unless (looking-at "^")
				(insert "\n"))
			      (insert "*** ")
			      (insert comment-headline "\n")
			      (org-narrow-to-subtree)
			      (mapc (lambda (entry)
				      (org-entry-put (point) (symbol-name entry) (or (cdr (assoc entry comment)) "")))
				    '(id author updateAuthor updated created))
			      (goto-char (point-max))
			      (insert (or (cdr (assoc 'body comment)) "")))))
			    comments)))))))
	    issues))))

(defun org-jira-update-comment ()
  "update a comment for the current issue"
  (interactive)
  (let* ((issue-key (org-jira-get 'issue 'key))
	(comment-id (org-jira-get 'comment 'id))
	(comment (org-jira-get-comment-body comment-id)))
    (if comment-id
	(jira2-edit-comment comment-id comment)
      (jira2-add-comment issue-key `((body . ,comment)))
      (org-jira-delete-current-comment)
      (org-jira-update-comments-for-current-issue))))

(defun org-jira-update-issue ()
  "update or create an issue"
  (let ((issue-key (org-jira-get-issue-key)))
    (if issue-key
	(org-jira-update-issue-details)
      (org-jira-create-issue)
      (org-jira-delete-current-issue)
      (org-jira-get-issues))))

(defun org-jira-update-issue-details ()
  "update the issue for the key/value pairs"
  (ensure-on-issue
   (let* ((issue-key (org-jira-get 'issue 'key))

(defun org-jira-get (type entry)
  "get org ENTRY for heading of TYPE.

TYPE can be 'issue, or 'comment.

ENTRY will vary with regard to the TYPE, if it is a symbol, it will be converted to string"

  (when (symbolp entry)
    (setq entry (symbol-name entry)))

  (if (eq type 'issue)
      (org-jira-get-issue-entry entry)
    (if (eq type 'comment)
	(org-jira-get-comment-entry entry)
      (error "unknown type %s" type))))

(defun org-jira-get-issue-entry (entry)
  "get the jira issue field value for ENTRY for current issue"
  (ensure-on-issue
   (org-entry-get (point) (if (symbolp entry)
			       (symbol-name entry)
			     entry))))

(defun org-jira-get-comment-entry (entry)
  "get the jira issue field value for ENTRY of current comment item"
  (ensure-on-comment
   (org-entry-get (point) (if (symbolp entry)
			       (symbol-name entry)
			     entry))))

(defun org-jira-get-comment-body (&optional comment-id)
  (ensure-on-comment
   (save-restriction
     (org-narrow-to-subtree)
     (goto-char (point-min))
     (org-entry-put (point) "id" comment-id)
     (search-forward ":END:")
     (forward-line)
     (buffer-substring-no-properties (point) (point-max)))))

(defmacro ensure-on-issue (&rest body)
  "Make sure we are on an issue heading"

  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Issue: ")
       (org-up-heading-all 1)
       (forward-thing 'whitespace)
       (unless (looking-at "Issue: ")
	 (error "Not on a issue region!")))
     ,@body))

(defmacro ensure-on-issue-key (issue-key &rest body)
  "Make sure we are on an issue heading"

  `(save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (show-all)
       
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Issue: ")
       (org-up-heading-all 1)
       (forward-thing 'whitespace)
       (unless (looking-at "Issue: ")
	 (error "Not on a issue region!")))
     ,@body))

(defmacro ensure-on-comment (&rest body)
  "Make sure we are on a comment heading"
  `(save-excursion
    (org-back-to-heading)
    (forward-thing 'whitespace)
    (unless (looking-at "Comment: ")
      (error "Not on a comment region!"))
    ,@body))

(provide 'org-jira)
