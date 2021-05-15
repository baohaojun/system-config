;;; browse-at-remote.el --- Open github/gitlab/bitbucket/stash/gist/phab/sourcehut page from Emacs -*- lexical-binding:t -*-

;; Copyright © 2015-2020
;;
;; Author:     Rustem Muslimov <r.muslimov@gmail.com>
;; Version:    0.14.0
;; Package-Version: 20210405.430
;; Package-Commit: e02ad2189c87da33f80bf4967a968772ce3e4431
;; Keywords:   github, gitlab, bitbucket, gist, stash, phabricator, sourcehut, pagure
;; Homepage:   https://github.com/rmuslimov/browse-at-remote
;; Package-Requires: ((f "0.17.2") (s "1.9.0") (cl-lib "0.5"))

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Easily open target page on github/gitlab (or bitbucket) from Emacs.
;; by calling `browse-at-remote` function. Support dired buffers and opens
;; them in tree mode at destination.

;;; Code:

(require 'f)
(require 's)
(require 'cl-lib)
(require 'vc-git)
(require 'url-parse)

(defgroup browse-at-remote nil
  "Open target on github/gitlab/bitbucket/stash/etc."
  :prefix "browse-at-remote-"
  :group 'applications)

(defcustom browse-at-remote-remote-type-domains
  '(("bitbucket.org" ."bitbucket")
    ("github.com" . "github")
    ("gitlab.com" . "gitlab")
    ("git.savannah.gnu.org" . "gnu")
    ("gist.github.com" . "gist")
    ("git.sr.ht" . "sourcehut")
    ("vs-ssh.visualstudio.com" . "ado")
    ("pagure.io" . "pagure")
    ("src.fedoraproject.org" . "pagure"))
  "Alist of domain patterns to remote types."

  :type '(alist :key-type (string :tag "Domain")
                :value-type (choice
                             (const :tag "GitHub" "github")
                             (const :tag "GitLab" "gitlab")
                             (const :tag "Bitbucket" "bitbucket")
                             (const :tag "Stash/Bitbucket Server" "stash")
                             (const :tag "git.savannah.gnu.org" "gnu")
                             (const :tag "Azure DevOps" "ado")
                             (const :tag "Phabricator" "phabricator")
                             (const :tag "gist.github.com" "gist")
                             (const :tag "sourcehut" "sourcehut")
                             (const :tag "pagure" "pagure")))
  :group 'browse-at-remote)

(defcustom browse-at-remote-prefer-symbolic t
  "Whether to prefer symbolic references when linking.

When t, uses the branch name, if available. This generates easier to
read URLs, but for long-lived links, the content of the linked file
may change, producing link root.

When nil, uses the commit hash. The contents will never change."
  :type 'boolean
  :group 'browse-at-remote)

(defcustom browse-at-remote-add-line-number-if-no-region-selected t
  "Always add line number even if region is not selected in buffer.
When is option is t, bar-browse adds line number to URL even if region was not selected.

By default is true."
  :type 'boolean
  :group 'browse-at-remote)

(defcustom browse-at-remote-use-http nil
  "List of domains where the web URL should be http."
  :type '(repeat string))

(defun browse-at-remote--get-url-from-remote (remote-url)
  "Return (DOMAIN . URL) from REMOTE-URL."
  ;; If the protocol isn't specified, git treats it as an SSH URL.
  (unless (s-contains-p "://" remote-url)
    (setq remote-url (concat "ssh://" remote-url)))
  (let* ((parsed (url-generic-parse-url remote-url))
         (host (url-host parsed))
         (port (url-port-if-non-default parsed))
         (web-proto
          (if (equal (url-type parsed) "http") "http" "https"))
         (filename (url-filename parsed)))
    ;; SSH URLs can contain colons in the host part, e.g. ssh://example.com:foo.
    (when (s-contains-p ":" host)
      (let ((parts (s-split ":" host)))
        (setq host (cl-first parts))
        (when (member host browse-at-remote-use-http)
          (setq web-proto "http"))
        (setq filename (concat "/" (cl-second parts) filename))))
    ;; when protocol is not http(s) port must always be stripped
    (unless (member (url-type parsed) '("http" "https"))
      (setq port nil))
    ;; Drop .git at the end of `remote-url'.
    (setq filename (s-chop-suffix ".git" filename))
    ;; Preserve the port.
    (when port
      (setq host (format "%s:%d" host port)))
    (cons host
          (format "%s://%s%s" web-proto host filename))))

(defun browse-at-remote--remote-ref (&optional filename)
  "Return (REMOTE-URL . REF) which contains FILENAME.
Returns nil if no appropriate remote or ref can be found."
  (let ((local-branch (browse-at-remote--get-local-branch))
        (revision (if (and (fboundp 'vc-git--symbolic-ref)
                           browse-at-remote-prefer-symbolic)
                      (vc-git--symbolic-ref (or filename "."))
                    (vc-git-working-revision (or filename "."))))
        remote-branch
        remote-name)
    ;; If we're on a branch, try to find a corresponding remote
    ;; branch.
    (if local-branch
      (let ((remote-and-branch (browse-at-remote--get-remote-branch local-branch)))
        (setq remote-name (car remote-and-branch))
        (setq remote-branch (cdr remote-and-branch)))
    ;; Otherwise, we have a detached head. Choose a remote
    ;; arbitrarily.
    (setq remote-name (car (browse-at-remote--get-remotes))))

    (when remote-name
      (cons
       (browse-at-remote--get-remote-url remote-name)
       (if (and browse-at-remote-prefer-symbolic remote-branch)
           ;; If the user has requested an URL with a branch name, and we
           ;; have a remote branch, use that.
           remote-branch
         ;; Otherwise, just use the commit hash.
         revision)))))

(defun browse-at-remote--get-local-branch ()
  "Return the name of the current local branch name.
If HEAD is detached, return nil."
  ;; Based on http://stackoverflow.com/a/1593487/509706
  (with-temp-buffer
    (let ((exit-code (vc-git--call t "symbolic-ref" "HEAD")))
      (when (zerop exit-code)
        (s-chop-prefix "refs/heads/" (s-trim (buffer-string)))))))

(defun browse-at-remote--get-remote-branch (local-branch)
  "If LOCAL-BRANCH is tracking a remote branch, return
\(REMOTE-NAME . REMOTE-BRANCH-NAME). Returns nil otherwise."
  (let ((remote-and-branch
         ;; Try pushRemote first
         (let ((push-remote (vc-git--run-command-string
                             nil "config"
                             (format "branch.%s.pushRemote" local-branch))))
           (if push-remote
               (format "%s/%s" (s-trim push-remote) local-branch)

             ;; If there's no pushRemote, fall back to upstream
             (vc-git--run-command-string
              nil "rev-parse"
              "--symbolic-full-name"
              "--abbrev-ref"
              (format "%s@{upstream}" local-branch))
             ))))
    ;; `remote-and-branch' is of the form "origin/master"
    (if remote-and-branch
      ;; Split into two-item list, then convert to a pair.
      (apply #'cons
             (s-split-up-to "/" (s-trim remote-and-branch) 1))
      (cons (car (browse-at-remote--get-remotes)) local-branch))))

(defun browse-at-remote--get-remote-url (remote)
  "Get URL of REMOTE from current repo."
  (with-temp-buffer
    (vc-git--call t "ls-remote" "--get-url" remote)
    (s-replace "\n" "" (buffer-string))))

(defun browse-at-remote--get-remotes ()
  "Get a list of known remotes."
  (with-temp-buffer
    (vc-git--call t "remote")
    (let ((remotes (s-trim (buffer-string))))
      (unless (string= remotes "")
        (s-split "\\W+" remotes)))))

(defun browse-at-remote--get-remote-type-from-config ()
  "Get remote type from current repo."
  (browse-at-remote--get-from-config "browseAtRemote.type"))

(defun browse-at-remote--get-from-config (key)
  (with-temp-buffer
    (vc-git--call t "config" "--get" key)
    (s-trim (buffer-string))))

(defun browse-at-remote--get-remote-type (target-repo)
  (let* ((domain (car target-repo))
         (remote-type-from-config (browse-at-remote--get-remote-type-from-config)))
    (or
     (if (s-present? remote-type-from-config)
         remote-type-from-config
       (cl-loop for pt in browse-at-remote-remote-type-domains
                when (string= (car pt) domain)
                return (cdr pt)))

     (error (format "Sorry, not sure what to do with domain `%s' (consider adding it to `browse-at-remote-remote-type-domains')"
                    domain)))))

(defun browse-at-remote--get-formatter (formatter-type remote-type)
  "Get formatter function for given FORMATTER-TYPE (region-url or commit-url) and REMOTE-TYPE (github or bitbucket)"
  (let ((formatter (intern (format "browse-at-remote--format-%s-as-%s" formatter-type remote-type))))
    (if (fboundp formatter)
        formatter nil)))

(defun browse-at-remote-gnu-format-url (repo-url)
  "Get a gnu formatted URL."
  (replace-regexp-in-string
   (concat "https://" (car (rassoc "gnu" browse-at-remote-remote-type-domains))
           "/\\(git\\).*\\'")
   "cgit" repo-url nil nil 1))

(defun browse-at-remote--format-region-url-as-gnu (repo-url location filename &optional linestart lineend)
  "URL formatter for gnu."
  (let ((repo-url (browse-at-remote-gnu-format-url repo-url)))
    (cond
     (linestart (format "%s.git/tree/%s?h=%s#n%d" repo-url filename location linestart))
     (t (format "%s.git/tree/%s?h=%s" repo-url filename location)))))

(defun browse-at-remote--format-commit-url-as-gnu (repo-url commithash)
  "Commit URL formatted for gnu"
  (format "%s.git/commit/?id=%s" (browse-at-remote-gnu-format-url repo-url) commithash))

(defun browse-at-remote--format-region-url-as-github (repo-url location filename &optional linestart lineend)
  "URL formatted for github."
  (cond
   ((and linestart lineend)
    (format "%s/blob/%s/%s#L%d-L%d" repo-url location filename linestart lineend))
   (linestart (format "%s/blob/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/tree/%s/%s" repo-url location filename))))

(defun browse-at-remote--format-commit-url-as-github (repo-url commithash)
  "Commit URL formatted for github"
  (format "%s/commit/%s" repo-url commithash))

(defun browse-at-remote-ado-format-url (repo-url)
  "Get an ado formatted URL."
  (let* ((s (split-string repo-url "/")))
    ;; [protocol]//[organization].visualstudio.com/[project]/_git/[repository]
    (format "%s//%s/%s/_git/%s"
            (nth 0 s)
            (replace-regexp-in-string "^vs-ssh" (nth 4 s) (nth 2 s))
            (nth 5 s)
            (nth 6 s))))

(defun browse-at-remote--format-region-url-as-ado (repo-url location filename &optional linestart lineend)
  "URL formatted for ado"
  (let* (
         ;; NOTE: I'm not sure what's the meaning of the "GB"
         ;; prefix. My guess is that it stands for a "Git Branch".
         (base-url (format "%s?version=%s%s&path=/%s"
                           (browse-at-remote-ado-format-url repo-url)
                           "GB"
                           location
                           filename)))
  (cond
   ((and linestart lineend)
    (format "%s&line=%d&lineEnd=%d&lineStartColumn=1&lineEndColumn=1" base-url linestart (+ 1 lineend)))
   (linestart (format "%s&line=%d&lineStartColumn=1&lineEndColumn=1" base-url linestart))
   (t base-url))))

(defun browse-at-remote--format-commit-url-as-ado (repo-url commithash)
  "Commit URL formatted for ado"
  ;; They does not seem to have anything like permalinks from github.
  (error "The ado version of the commit-url is not implemented"))

(defun browse-at-remote--format-region-url-as-bitbucket (repo-url location filename &optional linestart lineend)
  "URL formatted for bitbucket"
  (cond
   ((and linestart lineend)
    (format "%s/src/%s/%s#cl-%d:%d" repo-url location filename linestart lineend))
   (linestart (format "%s/src/%s/%s#cl-%d" repo-url location filename linestart))
   (t (format "%s/src/%s/%s" repo-url location filename))))

(defun browse-at-remote--format-commit-url-as-bitbucket (repo-url commithash)
  "Commit URL formatted for bitbucket"
  (format "%s/commits/%s" repo-url commithash))

(defun browse-at-remote--format-region-url-as-gist (repo-url location filename &optional linestart lineend)
  "URL formatted for gist."
  (concat
   (format "%s#file-%s" repo-url
           (replace-regexp-in-string "[^a-z0-9_]+" "-" filename))
   (cond
    ((and linestart lineend) (format "-L%d-L%d" linestart lineend))
    (linestart (format "-L%d" linestart))
    (t ""))))

(defun browse-at-remote--format-commit-url-as-gist (repo-url commithash)
  "Commit URL formatted for gist"
  (cond
   ((equal commithash "master")
    repo-url)
   (t
    (format "%s/%s" repo-url commithash))))

(defun browse-at-remote--fix-repo-url-stash (repo-url)
  "Inserts 'projects' and 'repos' in #repo-url"
	(let* ((reversed-url (reverse (split-string repo-url "/")))
         (project (car reversed-url))
         (repo (nth 1 reversed-url)))
    (string-join (reverse (append (list project "repos" repo "projects") (nthcdr 2 reversed-url))) "/")))

(defun browse-at-remote--format-region-url-as-stash (repo-url location filename &optional linestart lineend)
  "URL formatted for stash"
	(let* ((branch (cond
                  ((string= location "master") "")
                  (t (string-join (list "?at=refs%2Fheads%2F" location)))))
         (lines (cond
                 (lineend (format "#%d-%d" linestart lineend))
                 (linestart (format "#%d" linestart))
                 (t ""))))
    (format "%s/browse/%s%s%s" (browse-at-remote--fix-repo-url-stash repo-url) filename branch lines)))

(defun browse-at-remote--format-commit-url-as-stash (repo-url commithash)
  "Commit URL formatted for stash"
  (format "%s/commits/%s" (browse-at-remote--fix-repo-url-stash repo-url) commithash))

(defun browse-at-remote--format-region-url-as-phabricator (repo-url location filename &optional linestart lineend)
  "URL formatted for Phabricator"
  	(let* ((lines (cond
                 (lineend (format "\$%d-%d" linestart lineend))
                 (linestart (format "\$%d" linestart))
                 (t ""))))
  (format "%s/browse/%s/%s%s" repo-url location filename lines)))

(defun browse-at-remote--format-commit-url-as-phabricator (repo-url commithash)
  "Commit URL formatted for Phabricator"
  (message repo-url)
  (format "%s/%s%s" (replace-regexp-in-string "\/source/.*" "" repo-url)  (read-string "Please input the callsign for this repository:") commithash))

(defun browse-at-remote--format-region-url-as-gitlab (repo-url location filename &optional linestart lineend)
  "URL formatted for gitlab.
The only difference from github is format of region: L1-2 instead of L1-L2"
  (cond
   ((and linestart lineend)
    (format "%s/blob/%s/%s#L%d-%d" repo-url location filename linestart lineend))
   (linestart (format "%s/blob/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/tree/%s/%s" repo-url location filename))))

(defun browse-at-remote--format-region-url-as-sourcehut (repo-url location filename &optional linestart lineend)
  "URL formatted for sourcehut."
  (cond
   ((and linestart lineend)
    (format "%s/tree/%s/%s#L%d-%d" repo-url location filename linestart lineend))
   (linestart (format "%s/tree/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/tree/%s/%s" repo-url location filename))))

(defun browse-at-remote--format-commit-url-as-gitlab (repo-url commithash)
  "Commit URL formatted for gitlab.
Currently the same as for github."
  (format "%s/commit/%s" repo-url commithash))

(defun browse-at-remote--format-commit-url-as-sourcehut (repo-url commithash)
  "Commit URL formatted for sourcehut."
  (format "%s/commit/%s" repo-url commithash))

(defun browse-at-remote--format-region-url-as-pagure (repo-url location filename &optional linestart lineend)
  (let* ((repo-url (s-replace "/forks/" "/fork/" repo-url))
         (markup_ext (list ".rst" ".mk" ".md" ".markdown"))
         (markup? (seq-contains (mapcar (lambda (x) (string-suffix-p x filename)) markup_ext) t))
         (filename (cond (markup? (concat filename "?text=True"))
                         (t filename))))
    (cond
     ((and linestart lineend)
      (format "%s/blob/%s/f/%s#_%d-%d" repo-url location filename linestart lineend))
     (linestart (format "%s/blob/%s/f/%s#_%d" repo-url location filename linestart))
     (t (format "%s/blob/%s/f/%s" repo-url location filename)))))

(defun browse-at-remote--format-commit-url-as-pagure (repo-url commithash)
  "Commit URL formatted for github"
  (format "%s/commit/%s" repo-url commithash))


(defun browse-at-remote--commit-url (commithash)
  "Return the URL to browse COMMITHASH."
  (let* ((remote (car (browse-at-remote--remote-ref)))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (repo-url (cdr target-repo))
         (remote-type (browse-at-remote--get-remote-type target-repo))
         (clear-commithash (s-chop-prefixes '("^") commithash))
         (url-formatter (browse-at-remote--get-formatter 'commit-url remote-type)))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))
    (funcall url-formatter repo-url clear-commithash)))

(defun browse-at-remote--file-url (filename &optional start end)
  "Return the URL to browse FILENAME from lines START to END. "
  (let* ((remote-ref (browse-at-remote--remote-ref filename))
         (remote (car remote-ref))
         (ref (cdr remote-ref))
         (relname (f-relative filename (f-expand (vc-git-root filename))))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (remote-type (browse-at-remote--get-remote-type target-repo))
         (repo-url (cdr target-repo))
         (url-formatter (browse-at-remote--get-formatter 'region-url remote-type))
         (start-line (when start (line-number-at-pos start)))
         (end-line (when end (line-number-at-pos end))))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))

    (funcall url-formatter repo-url ref relname
             (if start-line start-line)
             (if (and end-line (not (equal start-line end-line))) end-line))))

(defun browse-at-remote-get-url ()
  "Main method, returns URL to browse."

  (cond
   ;; dired-mode
   ((eq major-mode 'dired-mode)
    (browse-at-remote--file-url (dired-current-directory)))

   ;; magit-status-mode
   ((eq major-mode 'magit-status-mode)
    (browse-at-remote--file-url default-directory))

   ;; magit-log-mode
   ((or (eq major-mode 'magit-log-mode) (eq major-mode 'vc-annotate-mode))
    (browse-at-remote--commit-url
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (line-beginning-position))
         (search-forward " ")
         (buffer-substring-no-properties (line-beginning-position) (- (point) 1))))))

   ;; magit-commit-mode and magit-revision-mode
   ((or (eq major-mode 'magit-commit-mode) (eq major-mode 'magit-revision-mode))
    (save-excursion
      ;; Search for the SHA1 on the first line.
      (goto-char (point-min))
      (let* ((first-line
              (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (commithash (cl-loop for word in (s-split " " first-line)
                                  when (eq 40 (length word))
                                  return word)))
        (browse-at-remote--commit-url commithash))))

   ;; log-view-mode
   ((derived-mode-p 'log-view-mode)
    (browse-at-remote--commit-url (cadr (log-view-current-entry))))

   ;; We're inside of file-attached buffer with active region
   ((and buffer-file-name (use-region-p))
    (let ((point-begin (min (region-beginning) (region-end)))
          (point-end (max (region-beginning) (region-end))))
      (browse-at-remote--file-url
       buffer-file-name point-begin
       (if (eq (char-before point-end) ?\n) (- point-end 1) point-end))))

   ;; We're inside of file-attached buffer without region
   (buffer-file-name
    (let ((line (when browse-at-remote-add-line-number-if-no-region-selected (point))))
      (browse-at-remote--file-url (buffer-file-name) line)))

   (t (error "Sorry, I'm not sure what to do with this."))))

;;;###autoload
(defun browse-at-remote ()
  "Browse the current file with `browse-url'."
  (interactive)
  (browse-url (browse-at-remote-get-url)))

;;;###autoload
(defun browse-at-remote-kill ()
  "Add the URL of the current file to the kill ring.

Works like `browse-at-remote', but puts the address in the
kill ring instead of opening it with `browse-url'."
  (interactive)
  (kill-new (browse-at-remote-get-url)))

;;;###autoload
(defalias 'bar-browse 'browse-at-remote
  "Browse the current file with `browse-url'.")

;;;###autoload
(defalias 'bar-to-clipboard 'browse-at-remote-kill
  "Add the URL of the current file to the kill ring.

Works like `browse-at-remote', but puts the address in the
kill ring instead of opening it with `browse-url'.")

(provide 'browse-at-remote)

;;; browse-at-remote.el ends here
