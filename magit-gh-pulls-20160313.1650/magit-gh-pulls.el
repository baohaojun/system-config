;;; magit-gh-pulls.el --- GitHub pull requests extension for Magit

;; Copyright (C) 2011-2015  Yann Hodique, Alexander Yakushev

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: git tools
;; Package-Version: 20160313.1650
;; Version: 0.5.2
;; URL: https://github.com/sigma/magit-gh-pulls
;; Package-Requires: ((emacs "24") (gh "0.9.1") (magit "2.1.0") (pcache "0.2.3") (s "1.6.1"))

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a Magit extension for manipulating GitHub pull requests

;; No configuration is needed in the repository if any of your remotes contain a
;; URL to Github's remote repository. If for some reason you don't have any
;; Github remotes in your config, you can specify username and repository
;; explicitly:

;; $ git config magit.gh-pulls-repo <user>/<repo> # your github repository

;; Add these lines to your init.el:

;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; These are the bindings for pull requests, defined in magit-gh-pulls-mode-map:
;; # g --- refreshes the list of pull requests
;; # f --- fetches the commits associated with the pull request at point
;; # b --- helps you creating a topic branch from a review request
;; # m --- merges the PR on top of the current branch
;; # c --- creates a PR from the current branch
;; # o --- opens a pull request on GitHub in your default browser

;; Then, you can do whatever you want with the commit objects associated with
;; the pull request (merge, cherry-pick, diff, ...)

;; When you create a new pull request, you can enable -w option to automatically
;; open it on GitHub in your default browser.

;;; Code:

(require 'eieio)

(require 'magit)
(require 'git-commit)
(require 'gh)
(require 'gh-pulls)
(require 'pcache)
(require 's)

(defgroup magit-gh-pulls nil
  "Github.com pull-requests for Magit."
  :group 'magit-extensions)

(defcustom magit-gh-pulls-open-new-pr-in-browser nil
  "DEPRECATED: use magit switch instead."
  :group 'magit-gh-pulls
  :type 'boolean)

(defvar magit-gh-pulls-maybe-filter-pulls 'identity
  "Filter function which should validate pulls you want to be
  viewed in magit. It receives a list of pull requests and should
  return a list of pull requests.")

(defvar magit-gh-pulls-collapse-commits t
  "Collapse commits in pull requests listing.")

(defvar magit-gh-pulls-pull-detail-limit 10
  "Pull in additional information for each pull request in the
   status buffer only if the total number of open PRs is <=
   this number. Additional information includes individual
   commits in each PR and highlighting based on the merge
   status of the PR. Increasing this number may adversely
   affect performance on repos with many PRs.")

(defvar-local magit-gh-pulls-previous-winconf nil)

(defvar magit-gh-pulls-editor-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-gh-pulls-pull-editor-finish)
    (define-key map (kbd "C-c C-k") 'magit-gh-pulls-pull-editor-quit)
    map))

(define-derived-mode magit-gh-pulls-editor-mode text-mode "Magit GitHub Pulls Editor"
  (font-lock-add-keywords nil (git-commit-mode-font-lock-keywords) t))

(easy-menu-define magit-gh-pulls-editor-mode-menu magit-gh-pulls-editor-mode-map
  "Magit GitHub Pulls Editor Menu"
  '("Magit GitHub Pulls"
    ["Submit Pull Request" magit-gh-pulls-pull-editor-finish t]
    ["Cancel" magit-gh-pulls-pull-editor-quit t]))

(defun magit-gh-pulls-get-api ()
  (gh-pulls-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-pulls-get-repo-from-config ()
  "Return (user . project) pair read from magit.gh-pulls-repo
config option."
  (let* ((cfg (magit-get "magit" "gh-pulls-repo")))
    (when cfg
      (let* ((split (split-string cfg "/")))
        (cons (car split) (cadr split))))))


;;Find all the Hostname Lines until we hit the end of config-lines or the
;;next Host line.  Return '(remaining-config-lines list-of-hostnames)
(defun magit-gh-pulls-collect-hostnames (config-lines)
  (let ((cur-line (car config-lines))
        (rest config-lines)
        (result '()))
    (while (and cur-line (not (string= (cadr cur-line) "Host")))
      (setq result (cons (cadr (cdr cur-line)) result))
      (setq rest (cdr rest))
      (setq cur-line (car rest)))
    (list rest result)))


(defun magit-gh-pulls-get-host-hostnames (config-lines)
  (let (result-alist
        (curline (car config-lines))
        (rest-lines (cdr config-lines)))
    (while rest-lines
      (if (string= (cadr curline) "Host")
          (let ((hosts (s-split "\\s*" (cadr (cdr curline)))) ;;List of the host aliases
                (rest-result (magit-gh-pulls-collect-hostnames rest-lines)))
            (dolist (host hosts)
              ;;Host must be lowercase because the url parser lowercases the string
              (setq result-alist (cons (cons (downcase host) (cadr rest-result)) result-alist)))
            (setq curline (caar rest-result))
            (setq rest-lines (cdar rest-result)))
        (progn
          (setq curline (car rest-lines))
          (setq rest-lines (cdr rest-lines)))))
    result-alist))

(defun -magit-gh-pulls-filter-and-split-host-lines (lines)
  (delq nil
        (mapcar (lambda (line)
                  (s-match "^[ \t]*\\(Host\\|HostName\\|Hostname\\)[ \t]+\\(.+\\)$" line))
                lines)))


;; Port of github/hub's SSHConfig
(defun magit-gh-pulls-get-ssh-config-hosts ()
  (let* ((file-lines (mapcar (lambda (path)
                             (if (file-exists-p path)
                                 (with-temp-buffer
                                   (insert-file-contents path)
                                   (split-string (buffer-string) "\n" t))
                               '()))
                           (list
                            (concat (file-name-as-directory (getenv "HOME")) ".ssh/config")
                            "/etc/ssh_config"
                            "/etc/ssh/ssh_config")))
         (all-lines (apply #'append file-lines))
         (matched-lines (-magit-gh-pulls-filter-and-split-host-lines all-lines)))
    (magit-gh-pulls-get-host-hostnames matched-lines)))


;; Port of github/hub's ParseURL, with modifications to align with existing parse-url
(defun magit-gh-pulls-parse-url (url ssh-config-hosts)
  (let* ((fixed-url (if (and (not (s-matches? "^[a-zA-Z_-]+://" url))
                            (s-matches? ":" url)
                            (not (s-matches? "\\\\\\\\" url))) ;;Two literal backlashes
                       (concat "ssh://" (s-replace ":" "/" url))
                      url))
         (parsed-url (url-generic-parse-url fixed-url))
         (ssh-host (when (string= (url-type parsed-url) "ssh")
                     (assoc (url-host parsed-url) ssh-config-hosts))))
    (when (and ssh-host (cadr ssh-host))
      (setf (url-host parsed-url) (cadr ssh-host)))
    (when (and
           (string= (url-host parsed-url) "github.com")
           (s-matches? "\\(git\\|ssh\\|https?\\)" (url-type parsed-url)))
      (let ((creds (s-match "/\\(.+\\)/\\([^/]+\\)/?$" (url-filename parsed-url))))
        (when creds
          (cons (cadr creds) (s-chop-suffix ".git" (cadr (cdr creds)))))))))


(defun magit-gh-pulls-guess-repo-from-origin ()
  "Return (user . project) pair inferred from remotes in
.git/config."
  (let ((creds nil)
        (ssh-config-hosts (magit-gh-pulls-get-ssh-config-hosts)))
    (dolist (remote (magit-git-lines "remote") creds)
      (let ((parsed (magit-gh-pulls-parse-url
                     (magit-get "remote" remote "url")
                     ssh-config-hosts)))
        (when parsed
          (setq creds parsed))))))

(defun magit-gh-pulls-guess-repo ()
  "Return (user . project) pair obtained either from explicit
option, or inferred from remotes."
  (or (magit-gh-pulls-get-repo-from-config)
      (magit-gh-pulls-guess-repo-from-origin)))

(defun magit-gh-pulls-requests-cached-p (api user proj)
  "Returns T if the API request to the given USER and PROJ is cached."
  (let ((cache-repo (format "/repos/%s/%s/pulls" user proj))
        (cached? nil))
    (pcache-map (oref api :cache)
                (lambda (key _) (when (equal (car key) cache-repo)
                             (setq cached? t))))
    cached?))

(defun magit-gh-pulls-insert-gh-pulls ()
  (condition-case-unless-debug print-section
      (progn
        (let* ((repo (magit-gh-pulls-guess-repo)))
          (when repo
            (let* ((api (magit-gh-pulls-get-api))
                   (user (car repo))
                   (proj (cdr repo))
                   (cached? (magit-gh-pulls-requests-cached-p api user proj))
                   (stubs (when cached?
                            (funcall magit-gh-pulls-maybe-filter-pulls
                                     (oref (gh-pulls-list api user proj) :data))))
                   (num-total-stubs (length stubs))
                   (i 0)
                   (branch (magit-get-current-branch)))
              (when (or (> (length stubs) 0) (not cached?))
                (magit-insert-section (pulls)
                  (magit-insert-heading "Pull Requests:")
                  (dolist (stub stubs)
                    (incf i)
                    (let* ((id (oref stub :number))
                           (base-sha (oref (oref stub :base) :sha))
                           (base-ref (oref (oref stub :base) :ref))
                           (head-sha (oref (oref stub :head) :sha))
                           ;; branch has been deleted in the meantime...
                           (invalid (equal (oref (oref stub :head) :ref) head-sha))
                           (have-commits
                            (and (>= magit-gh-pulls-pull-detail-limit i)
                                 (eql 0 (magit-git-exit-code "cat-file" "-e" base-sha))
                                 (eql 0 (magit-git-exit-code "cat-file" "-e" head-sha))))
                           (applied (and have-commits
                                         (magit-git-string "branch" branch
                                                           (format "--contains=%s" head-sha))))
                           (heading
                            (format "[%s@%s] %s\n"
                                    (propertize (number-to-string id)
                                                'face 'magit-tag)
                                    (if (string= base-ref branch)
                                        (propertize base-ref
                                                    'face 'magit-branch-local)
                                      base-ref)
                                    (propertize
                                     (oref stub :title) 'face
                                     (cond (applied 'magit-cherry-equivalent)
                                           (have-commits nil)
                                           (invalid 'error)
                                           (t 'italic)))))
                           (info (list user proj id)))
                      (cond
                       (have-commits
                        (magit-insert-section
                          (pull info magit-gh-pulls-collapse-commits)
                          (insert heading)
                          (magit-insert-heading)
                          (when (and have-commits (not applied))
                            (magit-git-wash
                                (apply-partially 'magit-log-wash-log 'cherry)
                              "cherry" "-v" (magit-abbrev-arg)
                              base-sha head-sha))))
                       (invalid
                        (magit-insert-section (invalid-pull info)
                          (insert heading)))
                       (t
                        (magit-insert-section (unfetched-pull info)
                          (insert heading))))))
                  (when (not cached?)
                    (insert "Press `# g` to update the pull request list.\n\n"))
                  (when (> (length stubs) 0)
                    (insert "\n"))))))))
    (error nil)))

(defun magit-gh-pulls-guess-topic-name (req)
  (let ((user (oref (oref req :user) :login))
        (topic (oref (oref req :head) :ref)))
    (format "%s/%s" user topic)))

(defun magit-gh-section-req-data (&optional section)
  (oref (apply #'gh-pulls-get
               (magit-gh-pulls-get-api)
               (magit-section-value (or section (magit-current-section))))
        :data))

(defun magit-gh-pulls-diff-pull-request ()
  (interactive)
  (magit-section-case
    (pull
     (let* ((req (magit-gh-section-req-data))
            (inhibit-magit-refresh t))
       (magit-diff (concat (oref (oref req :base) :sha) ".."
                           (oref (oref req :head) :sha))))
     (magit-refresh))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))


(defun magit-gh-pulls-create-branch ()
  (interactive)
  (magit-section-case
    (pull
     (let* ((req (magit-gh-section-req-data))
            (branch (read-from-minibuffer
                     "Branch name: " (magit-gh-pulls-guess-topic-name req)))
            (base (magit-read-branch-or-commit
                   "Branch base: "
                   (oref (oref req :base) :ref)))
            (inhibit-magit-refresh t))
       (magit-branch-and-checkout branch base)
       (magit-merge (oref (oref req :head) :sha)))
     (magit-refresh))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-merge-pull-request ()
  (interactive)
  (magit-section-case
    (pull
     (let* ((req (magit-gh-section-req-data))
            (branch (magit-gh-pulls-guess-topic-name req))
            (base (oref (oref req :base) :ref))
            (inhibit-magit-refresh t))
       (magit-branch-and-checkout branch base)
       (magit-merge (oref (oref req :head) :sha))
       (magit-checkout base)
       (magit-merge branch (when (member "--no-ff" (magit-gh-pulls-arguments))
                             '("--no-ff")))
       (magit-call-git "branch" "-D" branch))
     (magit-refresh))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-fetch-commits ()
  (interactive)
  (magit-section-case
    (unfetched-pull
     (let* ((req (magit-gh-section-req-data))
            (head (oref req :head)))
       (magit-run-git "fetch" (oref (oref head :repo) :ssh-url)
                      (oref head :ref))))
    (pull nil)
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-url-for-pull (info)
  "Return github url for a pull request using INFO."
  (let ((url "https://github.com/%s/%s/pull/%s"))
    (apply 'format url info)))

(defun magit-gh-pulls-open-in-browser ()
  (interactive)
  (let ((info (magit-section-value (magit-current-section))))
    (magit-section-case
      (pull           (browse-url (magit-gh-pulls-url-for-pull info)))
      (unfetched-pull (browse-url (magit-gh-pulls-url-for-pull info))))))

(defun magit-gh-pulls-purge-cache ()
  (let* ((api (magit-gh-pulls-get-api))
         (cache (oref api :cache))
         (repo (magit-gh-pulls-guess-repo)))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))

(defun magit-gh-pulls-get-remote-default (&optional remote-name-override)
  (let ((remote-name (or remote-name-override "origin"))
        (remote-branches (magit-git-lines "branch" "-r"))
         remote-head)
    (while (and remote-branches (not remote-head))
      (let ((m (s-match (format "^\\s-*%s/HEAD -> %s/\\(\\w*\\)" remote-name remote-name) (car remote-branches))))
        (if m
            (setq remote-head (cadr m))
          (setq remote-branches (cdr remote-branches)))))
    remote-head))

(defun magit-gh-pulls-build-req (api user proj callback)
  "Builds a request entity for a new pull request. Under
   synchronous flow (editor disabled), fires CALLBACK with
   API, USER, PROJ and the new REQUEST as args. Under
   asynchronous flow, passes all ARGS through to the PR
   editor which is responsible for continuing the flow."
  (let* ((current (magit-get-current-branch))
         (current-default (magit-gh-pulls-get-remote-default))
         (base-branch (magit-read-other-branch-or-commit "Base" nil current-default))
         (head-branch (magit-read-other-branch-or-commit "Head" nil current)))
    (let* ((head-remote (concat (magit-get-remote base-branch) "/" head-branch))
           (pushed-p (and (magit-branch-p head-remote)
                          (null (magit-git-lines "diff" (concat head-remote ".." head-branch))))))
      (when (and (not pushed-p)
                 (yes-or-no-p "PR branch doesn't appear to be pushed. Push it?"))
        (magit-push current (magit-get-remote base-branch))))
    (let* ((base
            (make-instance 'gh-repos-ref :user (make-instance 'gh-users-user :name user)
                           :repo (make-instance 'gh-repos-repo :name proj)
                           :ref base-branch))
           (head
            (make-instance 'gh-repos-ref :user (make-instance 'gh-users-user :name user)
                           :repo (make-instance 'gh-repos-repo :name proj)
                           :ref head-branch))
           (default-title (magit-git-string "log"
                                            (format "%s..%s" base-branch head-branch)
                                            "--format=%s" "--reverse"))
           (default-body (mapconcat 'identity (magit-git-items "log"
                                                               (format "%s..%s" base-branch head-branch)
                                                               "--reverse" "--format=**%s**%n%b") "\n")))

      (if (member "--use-pr-editor" (magit-gh-pulls-arguments))
        (magit-gh-pulls-init-pull-editor api user proj default-title default-body base head callback)
        (let* ((title (read-string "Title: " default-title))
               (body (read-string "Description: " default-body))
               (req (make-instance 'gh-pulls-request :head head :base base :body body :title title)))
          (funcall callback api user proj req))))))

(defun magit-gh-pulls-init-pull-editor (api user proj default-title default-body base head callback)
  "Create a new buffer for editing this pull request and
   switch to it. The context needed to finalize the
   pull request is stored in a buffer-local var in the
   newly created buffer."
  (let ((winconf (current-window-configuration))
        (buffer (get-buffer-create (format "*magit-gh-pulls: %s*" proj)))
        (context (make-hash-table :test 'equal)))
    (dolist (var '(api user proj base head callback))
      (puthash (symbol-name var) (eval var) context))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buffer)
    (funcall 'magit-gh-pulls-editor-mode)
    (insert (or default-title "") "\n\n" default-body)
    (goto-char (point-min))
    (message "Opening pull request editor. C-c C-c to finish, C-c C-k to quit.")
    (setq-local magit-gh-pulls-editor-context context)
    (setq magit-gh-pulls-previous-winconf winconf)))

(defun magit-gh-pulls-pull-editor-finish ()
  "Finish editing the current pull request and continue
   to submit it. This should be called interactively
   from within a pull request editor buffer."
  (interactive)
  (if (eq nil magit-gh-pulls-editor-context)
      (message "This function can only be run in a pull editor buffer.")
    (let* ((context magit-gh-pulls-editor-context)
           (end-of-first-line (save-excursion
                                (beginning-of-buffer)
                                (line-end-position)))
           (title (s-trim (buffer-substring-no-properties 1 end-of-first-line)))
           (body (s-trim (buffer-substring-no-properties end-of-first-line (point-max))))
           (req (make-instance 'gh-pulls-request
                               :head (gethash "head" context)
                               :base (gethash "base" context)
                               :body body :title title)))
      (funcall (gethash "callback" context)
               (gethash "api" context)
               (gethash "user" context)
               (gethash "proj" context)
               req)
      (magit-gh-pulls-pull-editor-quit))))

(defun magit-gh-pulls-pull-editor-quit ()
  "Cleanup the current pull request editor and restore
   the previous window config."
  (interactive)
  (if (eq nil magit-gh-pulls-editor-context)
      (message "This function can only be run in a pull editor buffer.")
    (let ((winconf magit-gh-pulls-previous-winconf))
      (kill-buffer)
      (kill-local-variable 'magit-gh-pulls-previous-winconf)
      (when winconf
        (set-window-configuration winconf)))))

(defun magit-gh-pulls-create-pull-request ()
  "Entrypoint for creating a new pull request."
  (interactive)
  (let ((repo (magit-gh-pulls-guess-repo)))
    (when repo
      (let* ((current-branch (magit-get-current-branch))
            (api (magit-gh-pulls-get-api))
            (user (car repo))
            (proj (cdr repo)))
        (magit-gh-pulls-build-req api user proj 'magit-gh-pulls-submit-pull-request)))))

(defun magit-gh-pulls-submit-pull-request (api user proj req)
  "Endpoint for creating a new pull request. Sync and async
   flows should both call this function to finish creating
   a new pull request."
  (interactive)
  (let* ((a (gh-pulls-new api user proj req)))
    (if (not (= (oref a :http-status) 201))
        (message "Error creating pull-request: %s.  Have you pushed the branch to github?" (cdr (assoc "Status" (oref a :headers))))
      (let ((url (oref (oref a :data) :html-url)))
        (message (concat "Created pull-request and copied URL to kill ring: " url))
        (when (member "--open-new-in-browser" (magit-gh-pulls-arguments))
          (browse-url url))
        (kill-new url)))))

(defun magit-gh-pulls-reload ()
  (interactive)
  (let ((creds (magit-gh-pulls-guess-repo)))
    (if (not (and creds (car creds) (cdr creds)))
        (message "Remote repository is not configured or incorrect.")
      (magit-gh-pulls-purge-cache)
      (gh-pulls-list (magit-gh-pulls-get-api) (car creds) (cdr creds))
      (magit-refresh))))

(easy-menu-define magit-gh-pulls-extension-menu
  nil
  "GitHub Pull Requests extension menu"
  '("GitHub Pull Requests"
    :visible magit-gh-pulls-mode
    ["Reload pull request" magit-gh-pulls-reload]
    ["Create pull request branch" magit-gh-pulls-create-branch]
    ["Fetch pull request commits" magit-gh-pulls-fetch-commits]
    ["Open pull request in browser" magit-gh-pulls-open-in-browser]
    ))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-gh-pulls-extension-menu)

(magit-define-section-jumper magit-jump-to-pulls "Pull Requests" pulls)
(define-key magit-status-mode-map (kbd "jq") 'magit-jump-to-pulls)

(defvar magit-gh-pulls-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "#") 'magit-gh-pulls-popup)
    map))

(defvar magit-gh-pulls-mode-lighter " Pulls")

;;;###autoload
(define-minor-mode magit-gh-pulls-mode "Pull requests support for Magit"
  :lighter  magit-gh-pulls-mode-lighter
  :require 'magit-gh-pulls
  :keymap  'magit-gh-pulls-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gh-pulls-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-pulls-insert-gh-pulls
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-pulls-insert-gh-pulls))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gh-pulls ()
  "Unconditionally turn on `magit-pulls-mode'."
  (magit-gh-pulls-mode 1))

(magit-define-popup magit-gh-pulls-popup
  "Show popup buffer featuring Github Pull Requests commands."
  'magit-commands
  :switches '((?c "Produce merge commit" "--no-ff")
              (?w "Open new PR in browser" "--open-new-in-browser")
              (?e "Edit PR in full buffer" "--use-pr-editor"))
  :actions  '((?g "Reload" magit-gh-pulls-reload)
              (?f "Fetch" magit-gh-pulls-fetch-commits)
              (?d "Diff" magit-gh-pulls-diff-pull-request)
              (?b "Make branch" magit-gh-pulls-create-branch)
              (?m "Merge"    magit-gh-pulls-merge-pull-request)
              (?c "Create new PR" magit-gh-pulls-create-pull-request)
              (?o "Open in browser" magit-gh-pulls-open-in-browser))
  :default-action 'magit-gh-pulls-reload)

(provide 'magit-gh-pulls)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-gh-pulls.el ends here
