;;; confluence.el --- Emacs mode for interacting with confluence wikis

;; Copyright (C) 2008-2011 Kyle Burton, James Ahlborn

;; Author: James Ahlborn
;; Author: Kyle Burton <kyle.burton@gmail.com>
;; URL: http://code.google.com/p/confluence-el/
;; Keywords: confluence, wiki, xmlrpc
;; Version: 1.8-beta
;; Package-Requires: ((xml-rpc "1.6.4"))
;; EmacsWiki: ConfluenceMode

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;
;; DOWNLOADING
;;
;; This module is available at Google Code:
;;
;;   http://code.google.com/p/confluence-el/
;;
;; INSTALLATION 
;;
;; You must set confluence-url in your .emacs file before using the functions
;; in this module.  It's best to place confluence.el and confluence-edit.el
;; and xml-rpc.el on your load path; often ~/.emacs.d or ~/elisp.
;;
;; Some examples:
;;
;;   ;; loading xml-rpc.el may not be necessary, it depends on your
;;   ;; installed version of Emacs, it was necessary on 22.1.1.
;;   ;; Both xml-rpc.el and confluence.el should be on your load-path.
;;
;;   (require 'confluence)
;;   (setq confluence-url "http://intranet/confluence/rpc/xmlrpc")
;;
;; There are various additional customzation options.  These can be explored
;; by executing M-x customize-group and then entering the group "confluence"
;; (or by browsing the file below).
;;
;; USING CONFLUENCE MODE
;;
;; To open a page, M-x confluence-get-page and enter the path to the
;; page, for example, to open a page in your home space: ~username/Tasks
;;
;; It is often convienient to bind this to a global key \C-xwf in your .emacs file:
;;   
;;    (global-set-key "\C-xwf" 'confluence-get-page)
;;
;; Once you have opened a page, made changes, simply saving the page
;; ("\C-x\C-s") will push the changes back to the wiki.
;;
;; To view the changes in your page versus what is in the wiki, type
;; \C-xw=, or run M-x confluence-ediff-current-page.
;;
;; Also, if you want keybindings for confluence-mode, you can put the
;; following in your .emacs file:
;;
;; (add-hook 'confluence-edit-mode-hook
;;   (local-set-key "\C-xw" confluence-prefix-map)
;;   (local-set-key "\M-j" 'confluence-newline-and-indent)
;;   (local-set-key "\M-;" 'confluence-list-indent-dwim))
;;
;; CONFLUENCE 4.0+
;;
;; In confluence version 4.0, Atlassian decided to change the wiki format.
;; They did away with the "wiki" format and changed the internal document
;; format to xml.  This makes editing confluence pages via emacs much less
;; enjoyable.  However, as of version 1.6 of this library, it is possible.
;; The somewhat simplistic confluence-xml-mode (simplistic compared to
;; confluence-mode) is an extension of nxml-mode.  It adds some minor
;; font-lock support, but otherwise leaves you with standard xml editing
;; support.  Confluence still has built in support for translating wiki format
;; pages to xml format pages, however, the reverse translation is more
;; problematic.
;;
;; Leveraging the excellent work of Graham Hannington
;; (http://www.amnet.net.au/~ghannington/confluence/readme.html), this package
;; provides a "basic" converter from xml to wiki format, however it can be
;; "lossy" depending on what advanced features a page contains.  A confluence
;; xml page can be converted to the wiki format using M-x
;; confluence-toggle-page-content-type.  This page can be saved as wiki format
;; (allowing confluence to do the reverse conversion on save) or can be
;; converted back to xml format (using the same command) and then saved
;; (allowing you to check the final content).  Note that the conversion from
;; xml to wiki format requires the external "xsltproc" program, which is
;; available on most unices and cygwin.
;;
;; For the truly brave, you can set the custom variable
;; confluence-xml-convert-to-wiki-on-load to t in order to automatically
;; convert xml content to wiki content on page load.
;;
;; LONGLINES
;;
;;   http://www.emacswiki.org/emacs-en/LongLines
;;
;; Confluence uses a wiki-markup that treats linebreask as <p> HTML
;; tags.  Since this is the case, it is very common for paragraphs in
;; the Confluence markup to wrap around your buffer multiple times.
;; The LongLines mode allows those lines to be viewed within Emacs
;; with 'soft' linebreaks - which are inserted automatically, or via
;; M-q.  This makes it much more pleasant to work with large
;; paragraphs of text in the Confluence markup without introducing
;; unwanted paragraphs.
;;
;; See below for more advice on using LongLines and confluence-mode.
;;
;;
;; EXAMPLE .emacs CONFIGURATION
;;
;; (require 'confluence)
;;
;; ;; note, all customization must be in *one* custom-set-variables block
;; (custom-set-variables
;;  ;; ... other custimization
;;
;;  ;; confluence customization
;;  '(confluence-url "http://intranet/confluence/rpc/xmlrpc")
;;  '(confluence-default-space-alist (list (cons confluence-url "your-default-space-name"))))
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; confluence editing support (with longlines mode)
;;
;; (autoload 'confluence-get-page "confluence" nil t)
;;
;; (eval-after-load "confluence"
;;   '(progn
;;      (require 'longlines)
;;      (progn
;;        (add-hook 'confluence-mode-hook 'longlines-mode)
;;        (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
;;        (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
;;        (add-hook 'confluence-mode-hook (lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))
;;
;; ;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
;; (autoload 'longlines-mode "longlines" "LongLines Mode." t)
;;
;; (eval-after-load "longlines"
;;   '(progn
;;      (defvar longlines-mode-was-active nil)
;;      (make-variable-buffer-local 'longlines-mode-was-active)
;;
;;      (defun longlines-suspend ()
;;        (if longlines-mode
;;            (progn
;;              (setq longlines-mode-was-active t)
;;              (longlines-mode 0))))
;;
;;      (defun longlines-restore ()
;;        (if longlines-mode-was-active
;;            (progn
;;              (setq longlines-mode-was-active nil)
;;              (longlines-mode 1))))
;;
;;      ;; longlines doesn't play well with ediff, so suspend it during diffs
;;      (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
;;                                              activate compile preactivate)
;;        "Suspend longlines when running ediff."
;;        (with-current-buffer (ad-get-arg 0)
;;          (longlines-suspend)))
;;
;;     
;;      (add-hook 'ediff-cleanup-hook 
;;                (lambda ()
;;                   (dolist (tmp-buf (list ediff-buffer-A
;;                                          ediff-buffer-B
;;                                          ediff-buffer-C))
;;                     (if (buffer-live-p tmp-buf)
;;                         (with-current-buffer tmp-buf
;;                           (longlines-restore))))))))
;;
;; 

;;; Code:

(require 'xml-rpc)
(require 'url-http)
(require 'ediff)
(require 'thingatpt)
(require 'browse-url)
(require 'image-file)
(require 'confluence-edit)
(require 'confluence-xml-edit)
(require 'url-parse)

(defgroup confluence nil
  "Support for editing confluence wikis."
  :tag "Confluence"
  :prefix "confluence-"
  :group 'hypermedia)

(defcustom confluence-url nil
  "Url of the confluence service to interact with.  This must
point to the XML-RPC api URL for your confluence installation.

If your confluence installation is at http://intranet/confluence,
then the XML-RPC URL is probably
http://intranet/confluence/rpc/xmlrpc.  Setting this in your
.emacs is necessary before interacting with the Wiki."
  :group 'confluence
  :type 'string)

(defcustom confluence-default-space-alist nil
  "AList of default confluence spaces to use ('url' -> 'space')."
  :group 'confluence
  :type '(alist :key-type string :value-type string))

(defcustom confluence-search-max-results 20
  "Maximum number of results to return from a search."
  :group 'confluence
  :type 'integer)

(defcustom confluence-prompt-page-function 'confluence-prompt-page-by-component
  "The function to used to prompt for pages when opening new
pages."
  :group 'confluence
  :type 'function)

(defcustom confluence-min-page-completion-length 3
  "The minimum number of characters at which to attempt page
completion.  Set to -1 to disable page completion."
  :group 'confluence
  :type 'integer)

(defcustom confluence-min-page-repeat-completion-length 3
  "The minimum number of new characters at which to re-attempt
page completion."
  :group 'confluence
  :type 'integer)

(defcustom confluence-max-completion-results 30
  "The maximum number of results to find when attempting completion."
  :group 'confluence
  :type 'integer)

(defcustom confluence-coding-alist nil
  "Obsolete variable, no longer necessary."
  :group 'confluence
  :type '(alist  :key-type string :value-type coding-system))

(defcustom confluence-show-attachment-images window-system
  "If not nil, attachments which are images will be displayed as
such (if possible), otherwise images will be treated the same as
other attachments."
  :group 'confluence
  :type 'boolean)

(defcustom confluence-save-credentials nil
  "If not nil, username and password will be saved after entry
for subsequent re-login (in memory only).  This is useful for
long running emacs sessions."
  :group 'confluence
  :type 'boolean)

(defcustom confluence-save-page-minor-edits 'ask
  "Whether a page save should be considered a 'minor edit' (no
notifications sent).  Note, this feature is only enabled if the
confluence server is version 2.10 or higher.
Possible values:
 `t'   -- Always save pages as minor edits.
 `ask' -- Ask on every page save.
 `nil' -- Never save pages as minor edits."
  :group 'confluence
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" ask)
                 (const :tag "Never" nil)))

(defcustom confluence-save-page-comments 'major
  "Whether a version comment should be included with a page save.
Note, this feature is only enabled if the confluence server is
version 2.10 or higher.
Possible values:
 `t'     -- Always ask for a comment.
 `major' -- Only ask for comments for major edits.
 `nil'   -- Never ask for a comment."
  :group 'confluence
  :type '(choice (const :tag "Always" t)
                 (const :tag "Major Only" major)
                 (const :tag "Never" nil)))

(defcustom confluence-auto-save-dir nil
  "Directory to which edited but not saved pages should be
temporarily saved (using the emacs auto-save-mode functionality).
If nil, auto save is disabled for confluence buffers."
  :group 'confluence
  :type 'string)

(defcustom confluence-xml-reformat-on-load nil
  "If not nil, confluence xml buffers will be reformated
automatically when loaded."
  :group 'confluence
  :type 'boolean)

(defcustom confluence-xml-convert-to-wiki-on-load nil
  "If not nil, confluence xml buffers will be
automatically converted to wiki format when loaded."
  :group 'confluence
  :type 'boolean)

(defvar confluence-before-save-hook nil
  "List of functions to be called before saving a confluence page.")

(defvar confluence-before-revert-hook nil
  "List of functions to be called before reverting a confluence page.")

(defvar confluence-xml-before-save-hook nil
  "List of functions to be called before saving a confluence xml page.")

(defvar confluence-xml-before-revert-hook nil
  "List of functions to be called before reverting a confluence xml page.")

(defvar confluence-login-token-alist nil
  "AList of 'url' -> 'token' login information.")

(defvar confluence-login-credential-alist nil
  "AList of 'url' -> ('username' . 'password') login information, if
`confluence-save-credentials' is t.")

(defvar confluence-server-info-alist nil
  "AList of 'url' -> 'server-info' information.")

(defvar confluence-path-history nil
  "History list of paths accessed.")

(defvar confluence-space-history nil
  "History list of spaces accessed.")

(defvar confluence-page-history nil
  "History list of pages accessed.")

(defvar confluence-search-history nil
  "History list of queries.")

(defvar confluence-label-history nil
  "History labels used.")

(defvar confluence-attachment-history nil
  "History labels used.")

(defvar confluence-page-url nil
  "The url used to load the current buffer.")
(make-variable-buffer-local 'confluence-page-url)
(put 'confluence-page-url 'permanent-local t)

(defvar confluence-page-struct nil
  "The full metadata about the page in the current buffer.")
(make-variable-buffer-local 'confluence-page-struct)
(put 'confluence-page-struct 'permanent-local t)

(defvar confluence-page-id nil
  "The id of the page in the current buffer.")
(make-variable-buffer-local 'confluence-page-id)
(put 'confluence-page-id 'permanent-local t)

(defvar confluence-browse-function nil
  "The function to use for browsing links in the current buffer.")
(make-variable-buffer-local 'confluence-browse-function)
(put 'confluence-browse-function 'permanent-local t)

(defvar confluence-load-info nil
  "The information necessary to reload the page.")
(make-variable-buffer-local 'confluence-load-info)
(put 'confluence-load-info 'permanent-local t)

(defvar confluence-page-content-type nil
  "The type of content on this confluence page.")
(make-variable-buffer-local 'confluence-page-content-type)
(put 'confluence-page-content-type 'permanent-local t)

(defvar confluence-tag-stack nil
  "TAGs style stack support for push (\\C-xw.) and pop (\\C-xw*)")

(setq confluence-get-attachment-names-function 'cfln-get-attachment-names)

(defconst confluence-search-types (list (cons "content" t) (cons "title" t) (cons "label" t))
  "Supported search types.")

(defconst confluence-xml-substitute-special (fboundp 'xml-substitute-special)
  "Whether or not confluence can override `xml-substitute-special'.")

(defconst confluence-xml-escape-string 
  (and (fboundp 'xml-escape-string)
       (not (string-match ".*FOO&amp;BAR.*" 
                          (with-temp-buffer
                            (xml-print (xml-rpc-value-to-xml-list "FOO&BAR"))
                            (buffer-string)))))
  "Whether or not confluence can/should override `xml-escape-string'.")

(defconst confluence-xml-entity-alist
  '(("quot" . "\"")
    ("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")
    ("apos" . "'"))
  "Basic xml entities.")

(defconst confluence-xml-page-decl
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<!DOCTYPE ac:confluence SYSTEM \"confluence.dtd\">\n")
  "Implicit xml declaration for confluence xml pages")

(defconst confluence-xml-page-header
  (concat
   "<ac:confluence xmlns:ac=\"http://www.atlassian.com/schema/confluence/4/ac/\" xmlns:ri=\"http://www.atlassian.com/schema/confluence/4/ri/\" xmlns=\"http://www.atlassian.com/schema/confluence/4/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.atlassian.com/schema/confluence/4/ac/ confluence.xsd\">\n")
  "Implicit xml header for confluence xml pages")

(defconst confluence-xml-page-footer "\n</ac:confluence>"
  "Implicit xml footer for confluence xml pages")

;; these are never set directly, only defined here to make the compiler happy
(defvar confluence-do-coding nil)
(defvar confluence-input-url nil)
(defvar confluence-no-push nil)

;; newer versions of emacs don't seem to have this defined (24.4+)
(unless (boundp 'thing-at-point-url-regexp)
  (defvar thing-at-point-url-regexp
    (concat "\\<\\(" (mapconcat 'identity thing-at-point-uri-schemes "\\|") "\\)"
            thing-at-point-url-path-regexp)
    "A regular expression probably matching a complete URL."))

(defun confluence-login (&optional arg)
  "Logs into the current confluence url, if necessary.  With ARG, forces
re-login to the current url."
  (interactive "P")
  (let ((confluence-input-url (cfln-get-url)))
    (if arg
        (cfln-set-struct-value 'confluence-login-token-alist
                             confluence-input-url nil))
    ;; we may need to prompt for a password while already at the minibuffer
    ;; prompt, so enable recursive minibuffers
    (let ((enable-recursive-minibuffers t)
          (credentials (and confluence-save-credentials
                            (cfln-get-struct-value confluence-login-credential-alist confluence-input-url)))
          (cur-token (cfln-get-struct-value confluence-login-token-alist
                                          confluence-input-url))
          (username nil)
          (password nil))
      (while (not cur-token)
        (condition-case err
            (progn
              (setq cur-token
                    (cfln-rpc-execute-internal 
                     "confluence1.login"
                     (setq username
                           (or (car-safe credentials)
                               (read-string (format "Confluence Username [%s]: " user-login-name)
                                            nil nil user-login-name t)))
                     (setq password
                           (or (cdr-safe credentials)
                               (read-passwd "Confluence Password: ")))))
              (cfln-set-struct-value 'confluence-login-token-alist
                                   confluence-input-url cur-token)
              (if confluence-save-credentials 
                  (cfln-set-struct-value 'confluence-login-credential-alist
                                       confluence-input-url (cons username password))))
          (error
           (progn
             (message "Failed logging in: %s" (error-message-string err))
             ;; clear any saved credentials (so re-attempts prompt for new info)
             (setq credentials nil)))))
      cur-token)))

;;;###autoload
(defun confluence-get-page (&optional page-name space-name anchor-name)
  "Loads a confluence page for the given SPACE-NAME and PAGE-NAME
into a buffer (if not already loaded) and switches to it.
Analogous to `find-file'.  Every time you navitage to a page with
this function (or M-. `confluence-get-page-at-point'), it is
saved off into a stack (`confluence-tag-stack') that you can then
pop back out of to return back through your navigation path (with
M-* `confluence-pop-tag-stack')."
  (interactive)
  ;; for "entry point" methods, login first (if necessary), otherwise prompts
  ;; can get confusing
  (confluence-login)
  (cfln-prompt-page-info nil 'page-name 'space-name)
  (cfln-show-page (cfln-rpc-get-page-by-name space-name page-name) 
                anchor-name))

(defun confluence-get-page-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the get
page call (based on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-input-url (and arg (cfln-prompt-url))))
    (confluence-get-page)))

(defun confluence-get-page-at-point (&optional arg)
  "Opens the confluence page at the current point.  If the link is a url,
opens the page using `browse-url', otherwise attempts to load it as a
confluence page.  Analogous to M-. (`find-tag').  Any ARG is passed to
`confluence-get-page-with-url' if nothing is found at point."
  (interactive "P")
  (let ((url nil)
        (is-embedded-content nil))
    ;; look for normal links, image/embedded content links, or raw links
    (cond
     ((thing-at-point-looking-at "{include:[ \t]*\\([^|\n}]+\\)}")
      (setq url (match-string 1)))
     ((thing-at-point-looking-at "\\[\\(\\([^|\n]*\\)[|]\\)?\\([^]\n]+\\)\\]")
      (setq url (match-string 3)))
     ((thing-at-point-looking-at "[!]\\([^]|\n]+\\)\\([|]\\([^]\n]*\\)\\)?[!]")
      (setq url (match-string 1))
      (setq is-embedded-content t))
     ((thing-at-point-looking-at thing-at-point-url-regexp)
      (setq url (match-string 0))))
    ;; determine how to handle the link (may be straight-up url)
    (if url
        (progn
          (set-text-properties 0 (length url) nil url)
          (if (string-match thing-at-point-url-regexp url)
              (browse-url url)
            (progn
              (if (and is-embedded-content (not (string-match "\\^" url)))
                  ;; embedded content links are really just attachments
                  (setq url (concat "^" url)))
              (if confluence-browse-function
                  (funcall confluence-browse-function url)
                (cfln-simple-browse-function url)))))
      (confluence-get-page-with-url arg))))

(defun confluence-get-parent-page ()
  "Loads a confluence page for the parent of the current
confluence page into a buffer (if not already loaded) and
switches to it."
  (interactive)
  (let ((parent-page-id (cfln-get-struct-value confluence-page-struct "parentId" "0")))
    (if (equal parent-page-id "0")
        (message "Current page has no parent page")
      (cfln-show-page (cfln-rpc-get-page-by-id parent-page-id)))))

(defun confluence-get-attachment (&optional page-name space-name file-name 
                                            page-id)
  "Gets the attachment with the given info and optionally displaying it in a
buffer for viewing or downloading it to a local file."
  (interactive)
  ;; get page and space names if not given
  (cfln-prompt-page-info "Attachment " 'page-name 'space-name
                       (cfln-get-struct-value confluence-page-struct "title"))
  ;; find page-id if not given
  (if (not page-id)
      (with-quiet-rpc
        (setq page-id (cfln-get-struct-value (cfln-rpc-get-page-by-name
                                            space-name page-name) "id"))))
  ;; get file name if not given
  (if (not file-name)
      (let ((cur-attachments (with-quiet-rpc
                              (cfln-result-to-completion-list
                               (cfln-rpc-get-attachments page-id) "fileName"))))
        (if (= (length cur-attachments) 0)
            (message "Current page has no attachments...")
          (setq file-name (cfln-read-string-simple "Confluence attachment file name: " 'confluence-attachment-history cur-attachments t)))))

  (if (and (cfln-string-notempty page-id)
           (cfln-string-notempty file-name))
      (let ((save-only-file-name nil))
        ;; determine if caller wants to view the file or merely download it
        (if (equal "d"
                   (cfln-read-char "(v)iew confluence attachment or (d)ownload only [v]: " "[vd]" "v"))
            (setq save-only-file-name (expand-file-name 
                                       (read-file-name "Download file name: " 
                                                       nil file-name))))
        (cfln-show-attachment page-name space-name file-name page-id 
                           save-only-file-name))))

(defun confluence-get-attachment-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the get
attachment call (based on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-input-url (and arg (cfln-prompt-url "Attachment "))))
    (confluence-get-attachment)))

(defun confluence-goto-anchor (&optional anchor-name)
  "Moves to the given ANCHOR-NAME in the current confluence buffer."
  (interactive)
  (if (not anchor-name)
      (let ((cur-anchors (cfln-get-page-anchors)))
            (if (= (length cur-anchors) 0)
                (message "Current page has no anchors...")
              (setq anchor-name (cfln-read-string-simple "Confluence Anchor Name: " 
                                                       nil cur-anchors t)))))
  (if (cfln-string-notempty anchor-name)
      (let ((anchor-position nil))
        (save-excursion
          (goto-char (point-min))
          (setq anchor-position
                (search-forward (concat "{anchor:" anchor-name "}") nil t))
          ;; headings are also implicit anchors
          (if (not anchor-position)
              (setq anchor-position
                    (re-search-forward (concat "^h[1-9][.]\\s-+" (regexp-quote anchor-name)) nil t)))
          ;; 'top' and 'bottom' can be used as anchors in some situations
          (if (and (not anchor-position)
                   (string= "top" anchor-name))
              (setq anchor-position (point-min)))
          (if (and (not anchor-position)
                   (string= "bottom" anchor-name))
              (setq anchor-position (point-max))))
        (if anchor-position
            (goto-char anchor-position)
          (message "Could not find anchor %s in page..." anchor-name)))))

(defun confluence-create-page (&optional page-name space-name)
  "Creates a new confluence page for the given SPACE-NAME and
PAGE-NAME and loads it into a new buffer."
  (interactive)
  (cfln-prompt-page-info "New " 'page-name 'space-name)
  (let ((new-page (list (cons "content" "")))
        (parent-page-id (cfln-get-parent-page-id t space-name)))
    (cfln-set-struct-value 'new-page "title" page-name)
    (cfln-set-struct-value 'new-page "space" space-name)
    (if parent-page-id
        (cfln-set-struct-value 'new-page "parentId" parent-page-id))
    (cfln-show-page (cfln-rpc-save-page new-page))))

(defun confluence-create-page-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the create page call
(based on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-input-url (and arg (cfln-prompt-url))))
    (confluence-create-page)))

(defun confluence-get-related-page (&optional rel-type arg)
  "Gets a page related to the current page.  Prompts for type, one of
(ancestor, child, descendent, attachment, parent)."
  (interactive "i\nP")
  (if (not rel-type)
      (setq rel-type (intern
                      (cfln-read-string-simple
                       (format "Confluence Relation Type [%s]: "
                               (if arg "ancestor" "child")) 
                       nil
                       '(("attachment" t) ("ancestor" t) ("child" t)
                         ("parent" t) ("descendent" t)) 
                       t nil
                       (if arg "ancestor" "child")))))
  (let ((space-name (cfln-get-struct-value confluence-page-struct "space"))
        (page-name (cfln-get-struct-value confluence-page-struct "title"))
        (page-id confluence-page-id)
        (rel-page-names nil))
    (if (and (cfln-string-notempty space-name)
             (cfln-string-notempty page-name)
             page-id)
        (cond
         ;; show page attachment
         ((eq rel-type 'attachment)
          (confluence-get-attachment page-name space-name nil page-id))
         ;; show page parent
         ((eq rel-type 'parent)
          (confluence-get-parent-page))
         (t
          (setq rel-page-names
                (with-quiet-rpc
                 (cfln-result-to-completion-list
                  (cond
                   ;; retrieve available ancestors
                   ((eq rel-type 'ancestor)
                    (cfln-rpc-get-page-ancestors page-id))
                   ;; retrieve available children
                   ((eq rel-type 'child)
                    (cfln-rpc-get-page-children page-id))
                   ;; retrieve available descendents
                   ((eq rel-type 'descendent)
                    (cfln-rpc-get-page-descendents page-id))
                   (t 
                    (error "Unknown relationship type %s" rel-type)))
                  "title")))
          (if (= (length rel-page-names) 0)
              (message "Current page has no relations of type %s"
                       rel-type)
            ;; prompt for actual related page to load
            (confluence-get-page
             (cfln-read-string nil
                             "Confluence Page Name: "
                             'confluence-page-history 
                             (cons space-name (cfln-get-url))
                             rel-page-names t)
             space-name)))))))

(defun confluence-browse-page ()
  "Runs `browse-url' with the url of the current confluence page."
  (interactive)
  (let ((url (cfln-get-struct-value confluence-page-struct "url")))
    (if (cfln-string-notempty url)
        (browse-url url))))

(defmacro cfln-destructure-tags-stack-entry (entry &rest body)
  "Destructure a tags-stack tuple.  NB this is not a hygenic
macro, it intentionally binds named variables that match the
structure of the stack entry.  The structure and the variable
bindings are:

  ((page-type confluence-input-url page-id-or-query &optional 
    space-name page-name file-name) old-point)

old-point is the point on the page which was pushed.  The 
preceding list of info is the load-info described in 
`cfln-destructure-load-info'.
"
  `(destructuring-bind
       ((page-type confluence-input-url page-id-or-query 
         &optional space-name  page-name file-name) 
        old-point)
       ,entry
     ,@body))

(defmacro cfln-destructure-load-info (load-info &rest body)
  "Destructure a load-info tuple.  NB this is not a hygenic
macro, it intentionally binds named variables that match the
structure of the stack entry.  The structure and the variable
bindings are:

  (page-type confluence-input-url page-id-or-query &optional 
   space-name page-name file-name)

Each load-info can be either the result of a search query (in
which case page-type will be the symbol 'search, a page
visitation (and page-type will be 'page), or an attachment 
download (and page-type will be 'attachment). page-id-or-query 
will be either a page-id or a query - depending on 
the type of stack entry (page or query).  space-name will be 
populated when page-type is 'search or 'attachment.  page-name 
and file-name will be populated when page-type is 'attachment.
"
  `(destructuring-bind
       (page-type confluence-input-url page-id-or-query 
                  &optional space-name page-name file-name)
       ,load-info
     ,@body))

(defun confluence-pop-tag-stack ()
  "Returns to the last previously visited space/page by popping
the tags stack."
  (interactive)
  (if (null confluence-tag-stack)
      (message "Stack is empty...")
    (let ((confluence-no-push t))
      (cfln-destructure-tags-stack-entry
       (pop confluence-tag-stack)
       (cond 
        ;; load a normal page by id
        ((eq page-type 'page)
         (cfln-show-page (cfln-rpc-get-page-by-id page-id-or-query)))
        ;; run a previous search query
        ((eq page-type 'search)
         (cfln-show-search-results 
          (cfln-rpc-search page-id-or-query space-name)
          load-info))
        ;; load an attachment
        ((eq page-type 'attachment)
         (cfln-show-attachment page-name space-name file-name page-id-or-query nil))
        (t
         (error "Invalid stack info")))
       (goto-char old-point)))))

(defun confluence-push-tag-stack ()
  "Pushes the current page onto the visited stack if it is a confluence page."
  (interactive)
  (if (and (not confluence-no-push) confluence-load-info)
      (push (list confluence-load-info (point)) confluence-tag-stack)))

(defun confluence-ediff-merge-current-page ()
  "Starts an ediff session diffing the current confluence page against the
latest version of that page saved in confluence with intent of saving the
result as the latest version of the page."
  (interactive)
  (cfln-ediff-current-page t))

(defun confluence-ediff-current-page ()
  "Starts an ediff session diffing the current confluence page against the
latest version of that page saved in confluence."
  (interactive)
  (cfln-ediff-current-page nil))

(defun confluence-reparent-page ()
  "Changes the parent of the current confluence page."
  (interactive)
  (let ((parent-page-id (cfln-get-parent-page-id nil)))
    (if (and parent-page-id
             (not (equal parent-page-id (cfln-get-struct-value confluence-page-struct "parentId"))))
        (progn
          (cfln-set-struct-value 'confluence-page-struct "parentId" parent-page-id)
          (set-buffer-modified-p t)))))

(defun confluence-rename-page ()
  "Changes the name (title) of the current confluence page."
  (interactive)
  (let ((page-name (cfln-prompt-page-name 
                    (cfln-get-struct-value confluence-page-struct "space") 
                    "New ")))
    (if (and (cfln-string-notempty page-name)
             (not (equal page-name (cfln-get-struct-value confluence-page-struct "title"))))
        (progn
          (cfln-set-struct-value 'confluence-page-struct "title" page-name)
          (cfln-update-buffer-name)
          (set-buffer-modified-p t)))))

(defun confluence-add-label (&optional label-name)
  "Adds the label with the given name to the current confluence page."
  (interactive)
  (if confluence-page-id
      (progn
        (if (not label-name)
            (setq label-name
                  (cfln-read-string-simple "New Confluence Label: " 'confluence-label-history 'cfln-complete-recent-label-name)))
        (if (cfln-string-notempty label-name)
            (cfln-rpc-add-label label-name confluence-page-id)))))

(defun confluence-remove-label (&optional label-name)
  "Removes the label with the given name to the current confluence page."
  (interactive)
  (if confluence-page-id
      (progn
        (if (not label-name)
          (let ((cur-labels (with-quiet-rpc
                             (cfln-result-to-completion-list (cfln-rpc-get-labels confluence-page-id) "name"))))
            (if (= (length cur-labels) 0)
                (message "Current page has no labels...")
              (progn
                (or label-name
                    (setq label-name (cfln-read-string-simple "Old Confluence Label: " 'confluence-label-history cur-labels t)))))))
        (if (cfln-string-notempty label-name)
            (cfln-rpc-remove-label label-name confluence-page-id)))))

(defun confluence-get-labels ()
  "Shows the labels of the current page."
  (interactive)
  (if confluence-page-id
      (let ((cur-labels (mapcar
                         (lambda (el)
                            (cfln-get-struct-value el "name"))
                         (cfln-rpc-get-labels confluence-page-id))))
        (if (= (length cur-labels) 0)
            (message "Current page has no labels...")
          (message "Current Confluence Labels: %s" cur-labels)))))
  
(defun confluence-delete-page (&optional arg)
  "Deletes the current confluence page.  Asks first, unless ARG is given."
  (interactive "P")
  (if (or arg
          (yes-or-no-p  "Really delete confluence page? "))
      (progn
        (if (not confluence-page-id)
            (error "Could not delete Confluence page %s, missing page id"
                   (buffer-name)))
        (cfln-rpc-execute "removePage" confluence-page-id)
        ;; remove this page from the tag stack
        (while (assoc confluence-load-info confluence-tag-stack)
          (setq confluence-tag-stack
                (remove (assoc confluence-load-info confluence-tag-stack)
                        confluence-tag-stack)))
        (kill-buffer (current-buffer)))))

;;;###autoload
(defun confluence-search (&optional query space-name)
  "Runs a confluence search for QUERY, optionally restricting the results to
the given SPACE-NAME."
  (interactive)
  ;; for "entry point" methods, login first (if necessary), otherwise prompts
  ;; can get confusing
  (confluence-login)
  (confluence-search-by-type 'content query space-name))

(defun confluence-search-in-space (&optional query)
  "Runs a confluence search for QUERY, restricting the results to the space of
the current buffer."
  (interactive)
  (confluence-search-by-type-in-space 'content query))

(defun confluence-search-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the search call (based
on `confluence-default-space-alist')."
  (interactive "P")
  (confluence-search-by-type-with-url arg 'content))

(defun confluence-search-by-type (&optional query-type query space-name)
  "Runs a confluence search by type (content, title, label) for QUERY, optionally restricting the results to
the given SPACE-NAME."
  (interactive)
  (or query-type
      (setq query-type (cfln-read-string-simple "Confluence Search Type [content]: "
                                        nil confluence-search-types
                                        t nil "content")))
  (if (stringp query-type)
      (setq query-type (intern query-type)))
  (let ((query-prompt-prefix "")
        (query-prefix ""))
    (cond
     ((eq query-type 'title)
      (setq query-prompt-prefix "Title ")
      (setq query-prefix "title: "))
     ((eq query-type 'label)
      (setq query-prompt-prefix "Label ")
      (setq query-prefix "labelText: ")))
    (or query
        (setq query (read-string (concat "Confluence " query-prompt-prefix "Query: ") nil 
                                 'confluence-search-history nil t)))
    (setq query (concat query-prefix query))
    (cfln-show-search-results (cfln-rpc-search query space-name)
                            (list 'search (cfln-get-url) query space-name))))

(defun confluence-search-by-type-in-space (&optional query-type query)
  "Runs a confluence search by type (content, title, label) for QUERY, restricting the results to the space of
the current buffer."
  (interactive)
  (confluence-search-by-type query-type query (cfln-get-struct-value confluence-page-struct "space")))

(defun confluence-search-by-type-with-url (&optional arg query-type)
  "With ARG, prompts for the confluence url to use for the search call (based
on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-input-url (and arg (cfln-prompt-url))))
    (confluence-search-by-type query-type)))

(defun confluence-preview ()
  "Preview the content in the current confluence buffer."
  (interactive)
  (if (and confluence-page-id
           confluence-page-struct)
      (let ((source-content "")
            (rendered-content nil)
            (render-buf (get-buffer-create " *Confluence-preview*")))
        ;; if the local content is modified, submit the content
        (if (buffer-modified-p)
            (progn
              (setq source-content (buffer-string))
              ;; if there are save hooks, copy the content into a temp buf and run them on the content before
              ;; submitting it
              (let ((cur-before-save-hook (if (eq confluence-page-content-type 'xml) 
                                              'confluence-xml-before-save-hook
                                            'confluence-before-save-hook)))
                  (if (symbol-value cur-before-save-hook)
                      (with-current-buffer (get-buffer-create " *Confluence-decode*")
                        (erase-buffer)
                        (insert source-content)
                        (run-hooks (symbol-value 'cur-before-save-hook))
                        (setq source-content (buffer-string)))))
              ))
        ;; render the current page, optionally with locally modified content
        (setq rendered-content (cfln-rpc-render-page (cfln-get-struct-value confluence-page-struct "space")
                                                   confluence-page-id source-content))
        ;; shove the html into a temp buffer
        (with-current-buffer render-buf
          (widen)
          (erase-buffer)
          (fundamental-mode)
          (insert rendered-content)
          ;; fix bug in stylesheet
          (goto-char (point-min))
          (if (re-search-forward "^\\s-*body\\s-*{[^}]*}" nil t)
              (progn
                (goto-char (match-beginning 0))
                (if (re-search-forward "text-align:\\s-*\\(center\\)" (match-end 0) t)
                    (replace-match "left" t t nil 1))))
          (set-buffer-modified-p nil))
        ;; launch browser with rendered content
        (message "Launching browser with preview content...")
        (browse-url-of-buffer render-buf))))

(defun confluence-get-info ()
  "Gets information on confluence."
  (interactive)
  (message "Confluence Server Info: %s" (cfln-get-server-info)))

(defun confluence-get-info-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the get
info call (based on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-input-url (and arg (cfln-prompt-url))))
    (confluence-get-info)))

(defun confluence-toggle-page-content-type ()
  "Converts the contents of the current page between xml and wiki
format.  Does nothing if the current page is not a confluence
page."
  (interactive)
  (if (eq confluence-page-content-type 'xml)
      (cfln-convert-xml-to-wiki)
    (cfln-convert-wiki-to-xml)))

(defun cfln-convert-xml-to-wiki ()
  "Converts the contents of the current page from xml to wiki
format.  Does nothing if the current page is not xml format.
Note, this function requires the external 'xsltproc' program,
available on most unices and cygwin."
  (interactive)
  (if (not (eq confluence-page-content-type 'xml))
      (message "Current buffer does not have confluence xml content")

    (let ((wiki-content nil))

      ;; first, "revert" current content
      (run-hooks 'confluence-xml-before-revert-hook)
      (widen)
      (setq wiki-content 
        (cfln-convert-xml-string-to-wiki-string (buffer-string)))

      ;; insert new contents to original buffer
      (cfln-set-struct-value 'confluence-page-struct "content" wiki-content)
      (cfln-insert-page confluence-page-struct confluence-load-info 
                        confluence-browse-function
                        nil 'confluence-mode 'wiki))))

(defun cfln-convert-xml-string-to-wiki-string (xml-content)
  "Converts the given xml content from xml to wiki
format."
  (let ((conf-dir (file-name-directory (locate-library "confluence")))
        (tmp-file-name (make-temp-file "confxml"))
        (wiki-content nil)
        (xml-decl confluence-xml-page-decl))

    (setq xml-decl (replace-regexp-in-string "\"\\(confluence[.]dtd\\)\""
                                             (expand-file-name "confluence.dtd" conf-dir)
                                             xml-decl t t 1))
    (with-temp-file tmp-file-name
      (insert xml-decl)
      (insert xml-content))
    (message "Processing xml content...")
    (with-current-buffer (get-buffer-create " *Confluence-decode*")
      (erase-buffer)
      (let ((convert-status (call-process "xsltproc" nil (current-buffer) nil
                                          (expand-file-name "confluence2wiki.xsl" conf-dir)
                                          tmp-file-name)))
        (unless (eq convert-status 0)
          (error "Failed converting xml content to wiki format [%s]" 
                 convert-status)))
      (setq wiki-content (buffer-string)))
    (message "Finished converting xml content")
    (delete-file tmp-file-name)
    wiki-content))

(defun cfln-convert-wiki-to-xml ()
  "Converts the contents of the current page from wiki to xml
format (assuming the server supports it.  Does nothing if the
current page is not wiki format."
  (interactive)
  (if (not (eq confluence-page-content-type 'wiki))
      (message "Current buffer does not have confluence wiki content")
    (if (not (cfln-is-version-at-least 4 0))
        (message "Current server does not support confluence xml content")

      (let ((wiki-content nil)
            (xml-content nil))

        ;; first, "revert" current content
        (run-hooks 'confluence-before-revert-hook)
        (widen)
        (setq wiki-content (buffer-string))

        (message "Processing wiki content...")
        (setq xml-content (cfln-rpc-execute "convertWikiToStorageFormat" wiki-content))
        (message "Finished converting wiki content")

        ;; insert new contents to original buffer
        (cfln-set-struct-value 'confluence-page-struct "content" xml-content)
        (cfln-insert-page confluence-page-struct confluence-load-info confluence-browse-function
                          nil 'confluence-xml-mode 'xml)))))


(defun cfln-rpc-execute (method-name &rest params)
  "Executes a confluence rpc call, managing the login token and logging in if
necessary."
  (apply 'cfln-rpc-execute-async nil method-name params))

(defun cfln-rpc-execute-async (async-callback method-name &rest params)
  "Executes a confluence rpc call, managing the login token and logging in if
necessary."
  (condition-case err
      (apply 'cfln-rpc-execute-internal-async async-callback method-name (confluence-login) params)
    (error
     ;; if we get a fault with the given keywords, try the call again after a
     ;; re-login (we force re-login), otherwise, just rethrow the error
     (if (and xml-rpc-fault-string
              (string-match "\\<authenticated\\>\\|\\<expired\\>" xml-rpc-fault-string))
         (apply 'cfln-rpc-execute-internal-async async-callback method-name (confluence-login t) params)
       (error (error-message-string err))))))

(defun cfln-rpc-execute-internal (method-name &rest params)
  "Executes a raw confluence rpc call.  Handles all necessary encoding/decoding of strings."
  (apply 'cfln-rpc-execute-internal-async nil method-name params))

(defun cfln-rpc-execute-internal-async (async-callback method-name &rest params)
  "Executes a raw confluence rpc call.  Handles all necessary encoding/decoding of strings."
  (setq xml-rpc-fault-string nil)
  (setq xml-rpc-fault-code   nil)
  (let* ((url-http-version "1.0")  ;; this make the xml-rpc parser happy
         (url-http-attempt-keepalives nil)
         (page-url (cfln-get-url))   ;; figure out which url to use
         (confluence-do-coding t))
    (if (not page-url)
        (error "No confluence url configured"))
    (condition-case err
        (let ((rpc-result 
               (if async-callback
                   (apply 'xml-rpc-method-call-async async-callback page-url 
                          (cfln-get-rpc-method-name method-name) params)
               (cfln-maybe-url-decode-entities-in-value (apply 'xml-rpc-method-call page-url 
                                                               (cfln-get-rpc-method-name method-name) params)))
               ))
          ;; clear any url messages before returning
          (message nil)
          rpc-result)
      (error
       ;; decode the fault string and the error message (often includes the
       ;; fault string) and then rethrow the error
       (if xml-rpc-fault-string
           (setq xml-rpc-fault-string (cfln-maybe-url-decode-entities-in-value 
                                       xml-rpc-fault-string))
         (if (and (consp err) (eq 'wrong-type-argument (car err)))
             (error (format "Failed parsing xml-rpc result, please check your confluence-url configuration, currently '%s' (actual error: %s)"
                            page-url (error-message-string err)))))
       (error (cfln-maybe-url-decode-entities-in-value (error-message-string err)))))))

(defun cfln-get-rpc-method-name (method-name)
  "Determines the actual rpc method name from the given simple method name."
  (if (string-prefix-p "confluence" method-name)
      ;; method name is already fully qualified
      method-name
    ;; need to determine the appropriate prefix
    (concat (if (cfln-is-version-at-least 4 0) "confluence2." "confluence1.")
            method-name)))

(defun cfln-rpc-get-page-by-name (space-name page-name)
  "Executes a confluence 'getPage' rpc call with space and page names."
  (cfln-rpc-execute "getPage" space-name page-name))

(defun cfln-rpc-get-page-by-id (page-id)
  "Executes a confluence 'getPage' rpc call with a page id."
  (cfln-rpc-execute "getPage" page-id))

(defun cfln-rpc-search (query space-name &optional max-results)
  "Executes a confluence 'search' rpc call, optionally restricted by the given
SPACE-NAME."
  (let ((params (list (cons "type" "page"))))
    (if (cfln-string-notempty space-name)
        (cfln-set-struct-value 'params "spaceKey" space-name))
    (cfln-rpc-execute "search" query
                    params (or max-results confluence-search-max-results))))

(defun cfln-rpc-save-page (page-struct &optional comment minor-edit)
  "Executes a confluence 'storePage' rpc call with a page struct (or
'updatePage' if comment or minorEdit flag are specified)."
  (let ((method-prefix (if (eq confluence-page-content-type 'xml) "confluence2." "confluence1.")))
    (if (or (cfln-string-notempty comment) minor-edit)
        (let ((page-options (list (cons "versionComment" (or comment "")) 
                                  (cons "minorEdit" minor-edit))))
          (cfln-rpc-execute (concat method-prefix "updatePage") page-struct page-options))
      (cfln-rpc-execute (concat method-prefix "storePage") page-struct))))

(defun cfln-get-attachment-names ()
  "Gets the names of the attachments for the current page, if a
confluence page."
  (if confluence-page-id
      (with-quiet-rpc
       (cfln-result-to-completion-list
        (cfln-rpc-get-attachments confluence-page-id) "fileName"))
    nil))

(defun cfln-rpc-get-spaces ()
  "Executes a confluence 'getSpaces' rpc call."
  (cfln-rpc-execute "getSpaces"))

(defun cfln-rpc-get-space (space-name)
  "Executes a confluence 'getSpace' rpc call with space name."
  (cfln-rpc-execute "getSpace" space-name))

(defun cfln-rpc-get-labels (obj-id)
  "Executes a confluence 'getLabelsById' rpc call with object id."
  (cfln-rpc-execute "getLabelsById" obj-id))

(defun cfln-rpc-get-recent-labels (max-results)
  "Executes a confluence 'getRecentlyUsedLabels' rpc call with the given max results."
  (cfln-rpc-execute "getRecentlyUsedLabels" max-results))

(defun cfln-rpc-add-label (label-name obj-id)
  "Executes a confluence 'addLabelByName' rpc call with label name and object id."
  (cfln-rpc-execute "addLabelByName" label-name obj-id))

(defun cfln-rpc-remove-label (label-name obj-id)
  "Executes a confluence 'removeLabelByName' rpc call with label name and object id."
  (cfln-rpc-execute "removeLabelByName" label-name obj-id))

(defun cfln-rpc-render-page (space-name page-id &optional content)
  "Executes a confluence 'renderContent' rpc call with space and page id and optional content."
  (cfln-rpc-execute "renderContent" space-name page-id (or content "")))

(defun cfln-rpc-get-attachments (page-id)
  "Executes a confluence 'getAttachments' rpc call with page id."
  (cfln-rpc-execute "getAttachments" page-id))

(defun cfln-rpc-get-attachment (page-id file-name &optional version)
  "Executes a confluence 'getAttachment' rpc call with page id, file name and
optional version number."
  ;; "0" gets the latest version
  (cfln-rpc-execute "getAttachment" page-id file-name 
                  (or version "0")))

(defun cfln-rpc-get-page-children (page-id)
  "Executes a confluence 'getChildren' rpc call with page id."
  (cfln-rpc-execute "getChildren" page-id))

(defun cfln-rpc-get-page-ancestors (page-id)
  "Executes a confluence 'getAncestors' rpc call with page id."
  (cfln-rpc-execute "getAncestors" page-id))

(defun cfln-rpc-get-page-descendents (page-id)
  "Executes a confluence 'getDescendents' rpc call with page id."
  (cfln-rpc-execute "getDescendents" page-id))

(defun cfln-rpc-get-server-info ()
  "Executes a confluence 'getServerInfo' rpc call."
  (cfln-rpc-execute "confluence1.getServerInfo"))

(defun cfln-ediff-current-page (update-cur-version)
  "Starts an ediff session for the current confluence page, optionally
updating the saved metadata to the latest version."
  (if (not confluence-page-id)
      (error "Could not diff Confluence page %s, missing page id"
             (buffer-name)))
  (let ((rev-buf)
        (cur-buf (current-buffer))
        (rev-page (cfln-rpc-get-page-by-id confluence-page-id)))
    (setq rev-buf
          (get-buffer-create (format "%s.~%s~" (buffer-name cur-buf) (cfln-get-struct-value rev-page "version" 0))))
    ;; create read-only temp buffer w/ the latest page data
    (with-current-buffer rev-buf
      (cfln-insert-page rev-page)
      (toggle-read-only 1))
    ;; optionally update the metadata in the current buffer (and update the
    ;; buffer name in case the page title changed)
    (if update-cur-version
        (progn
          (setq confluence-page-struct 
                (cfln-set-struct-value-copy rev-page "content" ""))
          (cfln-update-buffer-name)))
    (ediff-buffers cur-buf rev-buf nil 'confluence-diff)))

(defun cfln-save-page ()
  "Saves the current confluence page and updates the buffer with the latest
page."
  (if (not confluence-page-id)
      (error "Could not save Confluence page %s, missing page id"
             (buffer-name)))
  (let* ((minor-edit nil)
         (comment nil))
    (if (cfln-is-version-at-least 2 10)
        (progn
          ;; only ask for these values if the server API supports sending them
          (setq minor-edit (cfln-save-is-minor-edit))
          (setq comment (cfln-save-get-comment minor-edit))))
    (widen)
    (run-hooks (if (eq confluence-page-content-type 'xml) 
                   'confluence-xml-before-save-hook
                 'confluence-before-save-hook))
    (cfln-insert-page (cfln-rpc-save-page 
                     (cfln-set-struct-value-copy confluence-page-struct 
                                               "content" (buffer-string))
                     comment minor-edit) 
                    nil nil t)
    (when confluence-auto-save-dir
      (delete-auto-save-file-if-necessary)))
  t)

(defun cfln-save-is-minor-edit ()
  (and confluence-save-page-minor-edits
       (or (not (eq confluence-save-page-minor-edits 'ask))
           (not (y-or-n-p "Major edit? ")))))

(defun cfln-save-get-comment (minor-edit)
  (if (and confluence-save-page-comments
           (or (not (eq confluence-save-page-comments 'major))
               (not minor-edit)))
      (read-string "Modification comment: " nil nil nil t)
    nil))

(defun cfln-revert-page (&optional arg noconfirm)
  "Reverts the current buffer to the latest version of the current confluence
page."
  (if (and confluence-load-info
           (or noconfirm
               (yes-or-no-p "Revert confluence page? ")))
      (let ((confluence-no-push t)
            (inhibit-read-only t))
        ;; use the load-info to reload the page, so we can reload normal pages
        ;; and search pages
        (cfln-destructure-load-info confluence-load-info
          (cond 
           ;; reload normal page data
           ((eq page-type 'page)
            (cfln-insert-page (cfln-rpc-get-page-by-id page-id-or-query) confluence-load-info)
            (cfln-update-buffer-name))
           ;; reload search page data
           ((eq page-type 'search)
            (cfln-insert-search-results 
             (cfln-rpc-search page-id-or-query space-name)
             confluence-load-info))
           ;; reload attachment data
           ((eq page-type 'attachment)
            (if (or (not (file-exists-p buffer-file-name))
                    (equal "d"
                           (cfln-read-char "Revert attachment from Confluence (d)ownload or local (f)ile [d]: " 
                            "[df]" "d")))
                (cfln-insert-attachment page-name space-name file-name 
                                      page-id-or-query (current-buffer) nil
                                      confluence-load-info)
              (let ((revert-buffer-function nil))
                (revert-buffer arg t))))
           (t
            (error "Invalid load info")))))))

(defun cfln-show-page (full-page &optional anchor-name)
  "Does the work of finding or creating a buffer for the given confluence page
and loading the data if necessary."
  (confluence-push-tag-stack)
  ;; note, we save the current url as confluence-input-url in case the buffer
  ;; has a different value locally from a previous search (this value will
  ;; override it)
  (let* ((confluence-input-url (cfln-get-url))
         (load-info (list 'page confluence-input-url (cfln-get-struct-value full-page "id")))
         (page-buffer (get-buffer-create (cfln-format-buffer-name
                                          (cfln-get-struct-value full-page "title")
                                          (cfln-get-struct-value full-page "space")))))
    ;; only insert the data if the buffer is new, otherwise just show current
    ;; data
    (with-current-buffer page-buffer
      (if (not (equal confluence-load-info load-info))
          (progn
            (cfln-insert-page full-page load-info)
            (goto-char (point-min))))
      (if anchor-name
          (confluence-goto-anchor anchor-name)))
    (switch-to-buffer page-buffer)))

(defun cfln-insert-page (full-page &optional load-info browse-function 
                                   keep-undo page-mode page-content-type)
  "Does the work of loading confluence page data into the current buffer.  If
KEEP-UNDO, the current undo state will not be erased.  The LOAD-INFO is the 
information necessary to reload the page (if nil, normal page info is used)."
  ;; if this is an old buffer (already has confluence-page-content-type),
  ;; run revert hooks before writing new data
  (if confluence-page-content-type
      (run-hooks (if (eq confluence-page-content-type 'xml) 
                     'confluence-xml-before-revert-hook
                   'confluence-before-revert-hook)))
  (let ((autoconvert-xml nil))
    ;; determine new mode/content-type
    (unless page-mode
      (if (cfln-is-version-at-least 4 0)
          ;; default storage type for 4.0+ is xml.  only auto-convert xml if
          ;; option is enabled and page-mode was not set explicitly
          (setq page-mode 'confluence-xml-mode
                page-content-type 'xml
                autoconvert-xml confluence-xml-convert-to-wiki-on-load)
        (setq page-mode 'confluence-mode
              page-content-type 'wiki)))
    (if (not (eq major-mode page-mode))
        (fundamental-mode))
    ;; now, insert new contents
    (let ((old-point (point))
          (was-read-only buffer-read-only))
      (if was-read-only
          (toggle-read-only))
      ;; save/update various page metadata
      (setq confluence-page-struct full-page)
      (setq confluence-page-url (cfln-get-url))
      (setq confluence-page-content-type page-content-type)
      (setq confluence-page-id (cfln-get-struct-value confluence-page-struct "id"))
      (setq confluence-load-info 
            (or load-info
                (list 'page confluence-page-url confluence-page-id)))
      (if browse-function
          (setq confluence-browse-function browse-function))
      ;; don't save the buffer edits on the undo list (we might keep it)
      (let ((buffer-undo-list t)
            (inhibit-read-only t)
            (page-content (cfln-get-struct-value confluence-page-struct 
                                                 "content" "")))
        (when (eq page-content-type 'xml)
          ;; massage the contents a bit first (trim extra whitespace, add
          ;; header/footer)
          (setq page-content
                (replace-regexp-in-string "[ \t\n]+\\'" ""
                                          (replace-regexp-in-string "\\`[ \t\n]+" "" page-content)))
          (setq page-content (concat confluence-xml-page-header
                                     page-content
                                     confluence-xml-page-footer))
          (when autoconvert-xml
            ;; attempt to convert xml to wiki, leave xml content on failure
            (condition-case err
                (progn
                  (setq page-content 
                        (cfln-convert-xml-string-to-wiki-string page-content))
                  (setq page-mode 'confluence-mode
                        page-content-type 'wiki)
                  (setq confluence-page-content-type page-content-type))
                (error 
                 (message "Could not auto convert xml to wiki format: %s" err)))))
        (widen)
        (erase-buffer)
        ;; actually insert the new page contents
        (insert page-content)
        (goto-char old-point))
      ;; remove the contents from the page metadata
      (cfln-set-struct-value 'confluence-page-struct "content" "")
      ;; restore/setup buffer state
      (set-buffer-modified-p nil)
      (or keep-undo
          (eq buffer-undo-list t)
          (setq buffer-undo-list nil))
      (funcall page-mode)
      (if was-read-only
          (toggle-read-only 1)))))

(defun cfln-show-search-results (search-results load-info)
  "Does the work of finding or creating a buffer for the given confluence
search results and loading the data into that page."
  (confluence-push-tag-stack)
  ;; note, we save the current url as confluence-input-url in case the buffer
  ;; has a different value locally from a previous searcg (this value will
  ;; override it)
  (let ((confluence-input-url (cfln-get-url))
        (search-buffer (get-buffer-create "*Confluence Search Results*")))
    (with-current-buffer search-buffer
      ;; only reload the page if this is a new search, otherwise keep current
      ;; data
      (if (not (equal confluence-load-info load-info))
          (progn
            (cfln-insert-search-results search-results load-info)
            (goto-char (point-min))
            (toggle-read-only 1))))  ;; always make search results read-only
    (switch-to-buffer search-buffer)))

(defun cfln-insert-search-results (search-results load-info)
  "Does the work of loading confluence search data into the current buffer."
  (let ((search-page (list (cons "title" "Confluence Search Results"))))
    ;; turn the search results into a wiki-like page
    (with-temp-buffer
      (insert "h1. Confluence Search Results for '" (nth 2 load-info) "'\n\n")
      (dolist (search-result search-results)
        (insert (format "[%s|%s]\n"
                        (cfln-get-struct-value search-result "title")
                        (cfln-get-struct-value search-result "id")))
        (let ((excerpt (cfln-get-struct-value search-result "excerpt")))
          (if (cfln-string-notempty excerpt)
              (insert excerpt "\n")))
        (insert "\n"))
      (cfln-set-struct-value 'search-page "content" (buffer-string)))
    ;; install a special browse-function for loading the search urls (which
    ;; use page ids)
    (cfln-insert-page search-page load-info 'cfln-search-browse-function nil 'confluence-search-mode 'wiki)))

(defun cfln-search-browse-function (url)
  "Browse function used in search buffers (the links are page ids)."
  (cfln-show-page (cfln-rpc-get-page-by-id url)))

(defun cfln-simple-browse-function (url)
  "Simple browse function used in page buffers."
  (let ((space-name (cfln-get-struct-value confluence-page-struct "space"))
        (page-name url)
        (anchor-name nil)
        (attachment-name nil)
        (explicit-space nil)
        (page-id nil))
    ;; split "space:page" links
    (if (string-match "^\\([^:\n]*\\)[:]\\(.*\\)$" page-name)
        (progn
          (setq explicit-space t)
          (setq space-name (match-string 1 page-name))
          (setq page-name (match-string 2 page-name))))
    ;; strip off any trailing "|link tip"
    (if (string-match "^\\([^|\n]*\\)[|]\\(.*\\)$" page-name)
        (setq page-name (match-string 1 page-name)))
    ;; get '^' (attachment) or '#' (anchor)
    (if (string-match "^\\(.*?\\)\\([#^]\\)\\(.*\\)$" page-name)
        (progn
          (if (equal (match-string 2 page-name) "^")
              (setq attachment-name (match-string 3 page-name))
            (setq anchor-name (match-string 3 page-name)))
          (setq page-name (match-string 1 page-name))))
    (cond
     ;; open an attachment
     ((cfln-string-notempty attachment-name)
      (if (cfln-string-empty page-name)
          (progn
            (setq page-name (cfln-get-struct-value 
                             confluence-page-struct "title"))
            (setq page-id confluence-page-id)))
      (confluence-get-attachment page-name space-name attachment-name page-id))
     ;; goto anchor in this page
     ((and (cfln-string-notempty anchor-name)
           (cfln-string-empty page-name))
      (confluence-goto-anchor anchor-name))
     ;; goto space "home" page
     ((and explicit-space
           (cfln-string-notempty space-name)
           (cfln-string-empty page-name))
      (cfln-show-page
       (cfln-rpc-get-page-by-id (cfln-get-struct-value 
                               (cfln-rpc-get-space space-name) "homePage"))))
     ;; goto user profile page (load like space "home" page)
     ((and (not explicit-space)
           (string-match "^[~].+$" page-name))
      (cfln-show-page
       (cfln-rpc-get-page-by-id (cfln-get-struct-value 
                               (cfln-rpc-get-space page-name) "homePage"))))
     (t
      (confluence-get-page page-name space-name anchor-name)))))

(defun cfln-get-parent-page-id (try-current-page &optional space-name)
  "Gets a confluence parent page id, optionally using the one in the current
buffer."
  ;; if current page is a confluence page and try-current-page, ask if use
  ;; wants to use it as the parent page
  (if (and try-current-page
           confluence-page-id
           (yes-or-no-p "Use current confluence page for parent? "))
      confluence-page-id
    ;; otherwise, prompt for parent page
    (let ((parent-space-name (or space-name (cfln-get-struct-value confluence-page-struct "space")))
          (parent-page-name nil))
      (cfln-prompt-page-info "Parent " 'parent-page-name 'parent-space-name)
      (if (and (cfln-string-notempty parent-space-name)
               (cfln-string-notempty parent-page-name))
          (cfln-get-struct-value (cfln-rpc-get-page-by-name parent-space-name parent-page-name) "id")
        nil))))

(defun cfln-show-attachment (page-name space-name file-name page-id 
                           save-only-file-name)
  "Does the work of finding or creating a buffer for the given confluence
attachment and loading the data if necessary."
  (confluence-push-tag-stack)
  ;; note, we save the current url as confluence-input-url in case the buffer
  ;; has a different value locally
  (let* ((confluence-input-url (cfln-get-url))
         (load-info (list 'attachment confluence-input-url page-id
                          space-name page-name file-name))
         (result-buffer (get-buffer-create 
                         (cfln-format-attachment-buffer-name 
                          file-name page-name space-name))))
    ;; only insert the data if the buffer is new, otherwise just show current
    ;; data
    (unwind-protect
        (if (or save-only-file-name
                (not (equal
                      (with-current-buffer result-buffer
                        confluence-load-info)
                      load-info)))
            (cfln-insert-attachment page-name space-name file-name
                                  page-id result-buffer save-only-file-name
                                  load-info))
      ;; on save only, kill temp buf
      (if save-only-file-name
          (kill-buffer result-buffer)))
    ;; finish up
    (if save-only-file-name
        (message "File successfully downloaded to %s" save-only-file-name)
      (switch-to-buffer result-buffer))))

(defun cfln-insert-attachment (page-name space-name file-name page-id 
                             result-buffer save-only-file-name load-info)
  "Downloads and inserts the attachment with the given info into the given
RESULT-BUFFER for viewing.  If the image is a supported image type and
`confluence-show-attachment-images' is enabled, the data will be viewed as an
image.  If SAVE-ONLY-FILE-NAME is non-nil, the attachment will instead be
saved to this file name and not viewed."
  ;; we use lexical-let so the lambda form below can easily interact with the
  ;; variables defined here
  (lexical-let ((retrieval-done nil)
		(asynch-buffer nil)
                (download-error nil)
                (attachment-struct nil))

    ;; grab attachment struct
    (setq attachment-struct (with-quiet-rpc (cfln-rpc-get-attachment page-id file-name)))
    
    ;; prep result buffer
    (with-current-buffer result-buffer
      (widen)
      (erase-buffer)
      (fundamental-mode)
      ;; save load-info so we can revert the buffer using our custom
      ;; revert-buffer-function and push/pop
      (setq confluence-load-info load-info)
      ;; set page-struct w/ url only, so confluence-browse-page will work
      (setq confluence-page-struct (list (cons "url" (cfln-get-struct-value attachment-struct "url"))))
      (make-local-variable 'revert-buffer-function)
      (setq revert-buffer-function 'cfln-revert-page)
      (if (not buffer-file-name)
          (setq buffer-file-name 
                (or save-only-file-name
                    (cfln-create-temp-attachment-file file-name)))))

    ;; start async attachment download
    (setq asynch-buffer
          (cfln-rpc-execute-async 
           (lambda (&rest cb-args)
             (unwind-protect
                 (condition-case err
                     (cfln-attachment-download-callback result-buffer)
                   (error
                    (setq download-error (error-message-string err))))
               (setq retrieval-done t
                     asynch-buffer (current-buffer))))
           "getAttachmentData" 
           page-id file-name "0")) ;; "0" gets the latest version

    ;; wait for download to finish (this logic ripped from
    ;; url-retrieve-synchronously)
    (let ((proc (and asynch-buffer (get-buffer-process asynch-buffer))))
      (if (null proc)
	  nil
	(while (not retrieval-done)
	  (if (memq (process-status proc) '(closed exit signal failed))
              (setq retrieval-done t)
            (unless (accept-process-output proc)
              (setq proc (get-buffer-process asynch-buffer)))))))

    ;; just bailout if the download failed
    (if download-error
        (error download-error))

    (with-current-buffer result-buffer
      (if save-only-file-name
          ;; if we are not viewing the file, just save the result buffer
          (let ((save-buffer-coding-system 'no-conversion)
                (buffer-file-coding-system 'no-conversion)
                (coding-system-for-write 'no-conversion)
                (file-coding-system-alist nil))
            (write-region (point-min) (point-max) buffer-file-name nil 'quiet))
        ;; otherwise, prep the buffer for viewing
        (if (or (not confluence-show-attachment-images)
                (not (cfln-insert-image file-name)))
          (set-auto-mode))
        (set-buffer-modified-p nil)
        (goto-char (point-min))))))

(defun cfln-insert-image (attachment-file-name)
  "Determines if the attachment data in the current buffer with
ATTACHMENT-FILE-NAME is supported image data and, if so, displays the image
data.  Returns t if the data was successfully displayed as an image, nil
otherwise."
  (let ((buf-is-multibyte enable-multibyte-characters))
    (if (not
         (catch 'inserted-image
           
           ;; convert bmps to tifs (emacs does not seem to handle bmps)
           (if (let ((case-fold-search t))
                 (string-match "\\.bmp\\'" attachment-file-name))
               (progn
                 (cfln-bmp-to-tif)
                 (setq attachment-file-name (concat attachment-file-name ".tif"))))
           
           ;; don't even bother if the file name does not match supported
           ;; image types
           (if (not (string-match (image-file-name-regexp) 
                                  attachment-file-name))
               (throw 'inserted-image nil))
           ;; switch buffer to "binary" mode and grab image data
           (set-buffer-multibyte nil)
           (let ((img-data (string-make-unibyte
                            (buffer-substring-no-properties 
                             (point-min) (point-max))))
                 (img-type nil)
                 (image nil))
             ;; attempt to determine image type
             (setq img-type (image-type-from-data img-data))
             (if (not img-type)
                 (progn
                   (setq img-type (file-name-extension attachment-file-name))
                   (if (cfln-string-notempty img-type)
                       (setq img-type (intern (downcase img-type)))
                     (setq img-type nil))))
             (if (not img-type)
                 (throw 'inserted-image nil))
             ;; attempt to create image data
             (setq image (create-image img-data img-type t))
             (if (not image)
                 (throw 'inserted-image nil))
             ;; insert the image data, this logic borrowed from
             ;; insert-image-file
             (add-text-properties 
              (point-min) (point-max)
              `(display ,image
                        intangible ,image
                        rear-nonsticky (display intangible)
                        read-only t
                        front-sticky (read-only)))
             ;; indicate the buffer contents are "binary"
             (setq buffer-file-coding-system 'no-conversion)
             (make-local-variable 'find-file-literally)
             (setq find-file-literally t)
             (buffer-disable-undo))
           t))
        (progn
          ;; restore previous buffer state
          (set-buffer-multibyte buf-is-multibyte)
          (message "Unsupported image type %s" attachment-file-name)
          nil)
      t)))

(defun cfln-attachment-download-callback (result-buffer)
  "Handles an attachment xml-rpc download result buffer.  Copies the
attachment data to the given RESULT-BUFFER (base64 decoding or entity decoding
if necessary)."
  (url-mark-buffer-as-dead (current-buffer))
  (if (not (numberp url-http-response-status))
      (error "Why? url-http-response-status is %s"
             url-http-response-status))
  (if (> url-http-response-status 299)
      (error "Error during request: %s"
             url-http-response-status))
  (goto-char (point-min))
  (let ((value-start nil)
        (value-end nil)
        (base64-encoded nil)
        (case-fold-search t))
    (if (re-search-forward "<value>\\(<base64>\\)?" nil t)
        (progn
          (if (match-string 1)
              (setq base64-encoded t))
          (setq value-start (match-end 0)))
      (error "Could not find (start) attachment data"))
    (if (re-search-forward "\\(</base64>\\)</value>" nil t)
        (setq value-end (match-beginning 0))
      (error "Could not find (end) attachment data"))
    (copy-to-buffer result-buffer value-start value-end)
    (with-current-buffer result-buffer
      (if base64-encoded
          ;; if result was base64 encoded, just decode that
          (let ((save-buffer-coding-system 'no-conversion)
                (buffer-file-coding-system 'no-conversion)
                (coding-system-for-write 'no-conversion)
                (coding-system-for-read 'no-conversion)
                (file-coding-system-alist nil)
                (buf-is-multibyte enable-multibyte-characters))
            (if buf-is-multibyte
                (set-buffer-multibyte nil))
            (base64-decode-region (point-min) (point-max))
            (if buf-is-multibyte
                (set-buffer-multibyte t)))
        ;; otherwise, we need to do entity decoding
        (let ((confluence-do-coding t))
          (cfln-url-decode-entities-in-buffer result-buffer)))
      (set-buffer-modified-p nil))))

(defun cfln-bmp-to-tif ()
  "Converts a bmp to a tif in the current buffer."
  (let ((tmp-src-file (make-temp-file "bmp"))
        (tmp-dst-file (make-temp-file "tif")))

    ;; save bmp data to temp file
    (let ((save-buffer-coding-system 'no-conversion)
          (buffer-file-coding-system 'no-conversion)
          (coding-system-for-write 'no-conversion)
          (file-coding-system-alist nil))
      (write-region (point-min) (point-max) tmp-src-file nil 'quiet))

    ;; convert bmp to tiff
    (if (/= (call-process "bmp2tiff" nil nil nil tmp-src-file tmp-dst-file) 0)
        (progn
          (message "Failed executing bmp2tiff")
          (throw 'inserted-image nil)))

    ;; replace bmp data w/ tif data
    (insert-file-contents-literally tmp-dst-file nil 0 (nth 7 (file-attributes tmp-dst-file)) t)

    ;; ditch tmp files
    (delete-file tmp-src-file)
    (delete-file tmp-dst-file)))

(defun cfln-format-attachment-buffer-name (file-name page-name space-name)
  "Creates a buffer name for an attachment with the given info."
  (if (and (cfln-string-notempty page-name)
           (cfln-string-notempty space-name))
      (format "%s<%s/%s>" file-name space-name page-name)
    file-name))

(defun cfln-create-temp-attachment-file (file-name)
  "Creates a temporary file name for an attachment with the given info with
the pattern '<temp-dir>/<file-prefix>-<temp-id>.<file-ext>'."
  (save-match-data
  (let ((prefix file-name)
        (suffix ""))
    (if (string-match "^\\([^.]+\\)\\([.].*\\)$" file-name)
        (progn
          (setq prefix (match-string 1 file-name))
          (setq suffix (match-string 2 file-name))))
    (concat (make-temp-name
             (expand-file-name (concat prefix "-") temporary-file-directory))
            suffix))))

(defun cfln-get-server-info ()
  "Gets the (possibly cached) server info."
  (let ((server-info (cfln-get-struct-value confluence-server-info-alist (cfln-get-url))))
    (if (not server-info)
        (progn
          (setq server-info (cfln-rpc-get-server-info))
          (cfln-set-struct-value 'confluence-server-info-alist (cfln-get-url) server-info)))
    server-info))

(defun cfln-is-version-at-least (major-version minor-version)
  "Return t if the server version is at least the given version, nil otherwise."
  (let* ((server-info (cfln-get-server-info))
         (cur-major-version (string-to-number (cfln-get-struct-value server-info "majorVersion" "0")))
         (cur-minor-version (string-to-number (cfln-get-struct-value server-info "minorVersion" "0"))))
    (or (> cur-major-version major-version)
        (and (= cur-major-version major-version)
             (>= cur-minor-version minor-version)))))

(defun cfln-prompt-page-info (prompt-prefix page-name-var space-name-var &optional def-page-name)
  "Prompts for page info using the appropriate input function and sets the given vars appropriately."
  (let ((result-list
         (funcall confluence-prompt-page-function prompt-prefix
                  (symbol-value page-name-var) (symbol-value space-name-var) def-page-name)))
    (set page-name-var (nth 0 result-list))
    (set space-name-var (nth 1 result-list))))

(defun confluence-prompt-page-by-component (prompt-prefix page-name space-name def-page-name)
  "Builds a list of (page-name space-name <url>) by prompting the user for each.  Suitable for use with
`confluence-prompt-page-function'."
  (let ((result-list nil))
    ;; now, prompt for space and page if not already defined by caller
    (if (not space-name)
        (setq space-name (cfln-prompt-space-name prompt-prefix)))
    (push space-name result-list)
    (push (or page-name
              (cfln-prompt-page-name space-name prompt-prefix def-page-name)) result-list)
    result-list))

(defun confluence-prompt-page-by-path (prompt-prefix page-name space-name def-page-name)
  "Builds a list of (page-name space-name <url>) by prompting the user for each (where page and space name are
specified as one path).  Suitable for use with `confluence-prompt-page-function'."
  (let ((result-list nil)
        (page-path nil))
    ;; now, prompt for space/page if both are not already defined by caller
    (if (and page-name space-name)
        (setq result-list (cons page-name (cons space-name result-list)))
      (progn
        (setq page-path (cfln-prompt-path prompt-prefix page-name space-name def-page-name))
        ;; split path into space and page
        (push (cfln-get-space-name-from-path page-path) result-list)
        (push (cfln-get-page-name-from-path page-path) result-list)))
    result-list))

(defun cfln-prompt-url (&optional prompt-prefix)
  "Prompts for a confluence url."
  (let ((temp-url-hist (and confluence-default-space-alist
                            (mapcar 'car confluence-default-space-alist))))
    (read-string (concat (or prompt-prefix "") "Confluence Url: ") nil 'temp-url-hist (cfln-get-url) t)))

(defun cfln-prompt-space-name (&optional prompt-prefix)
  "Prompts for a confluence space name."
  (let* ((def-space-name (cfln-get-default-space))
         (init-space-name (cfln-get-struct-value confluence-page-struct "space"))
         (space-prompt (if def-space-name
                           (format "Confluence Space [%s]: " def-space-name)
                         "Confluence Space: ")))
    (cfln-read-string prompt-prefix space-prompt 'confluence-space-history
                    (cfln-get-url)
                    'cfln-complete-space-name t
                    (if (not (equal init-space-name def-space-name))
                        init-space-name
                      nil)
                    def-space-name)))

(defun cfln-prompt-page-name (space-name &optional prompt-prefix def-page-name)
  "Prompts for a confluence page name."
  (let ((page-prompt (if def-page-name
                         (format "Confluence Page Name [%s]: " def-page-name)
                       "Confluence Page Name: "))
        (completion-ignore-case t))
    (cfln-read-string prompt-prefix page-prompt
                    'confluence-page-history (cons space-name (cfln-get-url))
                    'cfln-complete-page-name nil nil def-page-name)))

(defun cfln-prompt-path (prompt-prefix page-name space-name def-page-name)
  "Prompts for a confluence page path."
  (cfln-read-string prompt-prefix "Confluence Space/PageName: "
                  'confluence-path-history (cfln-get-url)
                  'cfln-complete-page-path nil
                  (if space-name
                      (concat space-name "/" (or def-page-name ""))
                    nil)))

(defun cfln-minibuffer-setup ()
  "Minibuffer setup hook which changes some keybindings for confluence completion."
  (if confluence-completing-read
      ;; don't do completion when spaces are entered (just confusing)
      (local-set-key " " 'self-insert-command)))

(add-hook 'minibuffer-setup-hook 'cfln-minibuffer-setup t)

(defun cfln-get-space-name-from-path (page-path)
  "Parses the space name from the given PAGE-PATH."
  (if (string-match "\\([^/]+\\)[/]\\(.*\\)" page-path)
      (match-string 1 page-path)
    (cfln-get-default-space)))

(defun cfln-get-page-name-from-path (page-path)
  "Parses the page name from the given PAGE-PATH."
  (if (string-match "\\([^/]+\\)[/]\\(.*\\)" page-path)
      (match-string 2 page-path)
    page-path))

(defun cfln-complete-space-name (comp-str pred comp-flag)
  "Completion function for confluence spaces."
  (condition-case err
      (if (not cfln-read-current-other-completions)
          (with-current-buffer cfln-read-completion-buffer
            (setq cfln-read-current-other-completions (cfln-result-to-completion-list (cfln-rpc-get-spaces) "key"))))
    (error
     ;; just proceed with current info (probably a communication error, which will be worked out later)
     (message "Failed loading confluence spaces for completion: %s" (error-message-string err))
     (setq cfln-read-current-other-completions (list (cons comp-str t)))))
  (cfln-complete comp-str pred comp-flag cfln-read-current-other-completions))

(defun cfln-complete-page-name (comp-str pred comp-flag)
  "Completion function for confluence pages."

  ;; clear previous completion info if beginning of current string does not match previous string
  (let ((tmp-comp-str (replace-regexp-in-string "^\\(\\s-\\|\\W\\)*\\(.*?\\)\\(\\s-\\|\\W\\)*$"
                                                "\\2" (downcase comp-str) t))
        (old-current-completions nil))
    (if (and cfln-read-last-comp-str
             (not (eq t (compare-strings cfln-read-last-comp-str 0 (length cfln-read-last-comp-str)
                                         tmp-comp-str 0 (length cfln-read-last-comp-str) t))))
        (progn
          (setq cfln-read-last-comp-str nil)
          (setq cfln-read-current-completions nil))
      ;; if the new string is over the repeat search threshold, clear previous search results
      (if (and cfln-read-last-comp-str
               (<= (+ (length cfln-read-last-comp-str) confluence-min-page-repeat-completion-length)
                   (length tmp-comp-str)))
          (progn
            (setq old-current-completions cfln-read-current-completions)
            (setq cfln-read-current-completions nil))))
    
  ;; retrieve page completions if necessary
  (if (and (>= confluence-min-page-completion-length 0)
           (not cfln-read-current-completions)
           (>= (length tmp-comp-str) confluence-min-page-completion-length))
      (let ((title-query
             (replace-regexp-in-string "\\(\\W\\)" "\\\\\\&" tmp-comp-str t)))
        (setq title-query (concat "title:" title-query))
        (setq cfln-read-last-comp-str tmp-comp-str)
        (with-current-buffer cfln-read-completion-buffer
          (setq cfln-read-current-completions (cfln-result-to-completion-list
                                     (cfln-rpc-search title-query space-name confluence-max-completion-results)
                                     "title"))
          (when (= (length cfln-read-current-completions) 0)
            ;; next, try search w/ '*' (never know which one will work)
            (setq title-query (concat title-query "*"))
            (setq cfln-read-current-completions (cfln-result-to-completion-list
                                                 (cfln-rpc-search title-query space-name
                                                                  confluence-max-completion-results)
                                                 "title"))))
        ;; the query results are flaky, if we had results before and none now, reuse the old list
        (if (and (= (length cfln-read-current-completions) 0)
                 old-current-completions)
            (setq cfln-read-current-completions old-current-completions))
        )))
  
  (cfln-complete comp-str pred comp-flag cfln-read-current-completions))

(defun cfln-complete-page-path (comp-str pred comp-flag)
  "Completion function for confluence page paths."
  (let ((space-name comp-str)
        (page-name nil))
    (if (string-match "\\([^/]+\\)[/]\\(.*\\)" comp-str)
        (progn
          (setq space-name (match-string 1 comp-str))
          (setq page-name (match-string 2 comp-str))))
    (if (not page-name)
        (cfln-complete-space-name comp-str pred comp-flag)
      (let ((page-comp-result (cfln-complete-page-name page-name pred comp-flag)))
        (cond
         ((stringp page-comp-result)
          (concat space-name "/" page-comp-result))
         ((listp page-comp-result)
          (mapcar
           (lambda (el)
              (concat space-name "/" el)) page-comp-result))
         (t page-comp-result))))))

(defun cfln-complete-recent-label-name (comp-str pred comp-flag)
  "Completion function for confluence labels."
  (if (not cfln-read-current-completions)
      (with-current-buffer cfln-read-completion-buffer
        (setq cfln-read-current-completions (cfln-result-to-completion-list (cfln-rpc-get-recent-labels
                                                                 confluence-max-completion-results) "name"))))
  (cfln-complete comp-str pred comp-flag cfln-read-current-completions))

(defun cfln-update-buffer-name ()
  "Sets the buffer name based on the buffer info if it is a page buffer."
  (let ((page-name (cfln-get-struct-value confluence-page-struct "title"))
        (space-name (cfln-get-struct-value confluence-page-struct "space")))
    ;; only update if the current buffer has title and space (this method will
    ;; do nothing on search pages)
    (if (and (cfln-string-notempty page-name)
             (cfln-string-notempty space-name))
        (rename-buffer (cfln-format-buffer-name page-name space-name)))))

(defun cfln-format-buffer-name (page-name space-name)
  "Formats the name of the buffer given the page and space name."
  (format "%s<%s>" page-name space-name))

(defun cfln-get-url ()
  "Gets the confluence url to use for the current operation."
  ;; get the relevant url, by precedence:
  ;; - input url - optionally defined by current operation
  ;; - page url - if current buffer is confluence buffer, this will be the url
  ;;              from which it was loaded
  ;; - confluence-url user configured default
  (or confluence-input-url confluence-page-url confluence-url))

(defun cfln-get-default-space ()
  "Gets the default confluence space to use for the current operation."
  (cfln-get-struct-value confluence-default-space-alist (cfln-get-url)))

(defun cfln-maybe-url-decode-entities-in-value (value)
  "Decodes XML entities in the given value, which may be a struct, list or
something else.  This is only done if the xml-substitute-special
function was not successfully overridden."
  (if (not confluence-xml-substitute-special)
      (cond 
       ((listp value)
        (dolist (struct-val value)
          (setcdr struct-val 
                  (cfln-maybe-url-decode-entities-in-value (cdr struct-val)))))
       ((stringp value)
        (setq value (cfln-url-decode-entities-in-string value)))))
  value)

(defun cfln-url-decode-entities-in-string (string)
  "Convert XML entities to string values:
    &amp;    ==>  &
    &lt;     ==>  <
    &gt;     ==>  >
    &quot;   ==>  \"
    &apos;   ==>  '
    &#[0-9]+ ==>  <appropriate char>"
  (if (and (stringp string)
           (string-match "[&\r]" string))
      (save-excursion
	(set-buffer (get-buffer-create " *Confluence-decode*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(insert string)
        (cfln-url-decode-entities-in-buffer (current-buffer))

        ;; always convert to unix newlines
	(goto-char (point-min))
        (while (re-search-forward "\r\n" nil t)
          (replace-match "\n" t t))
	(buffer-string))
    string))

(defun cfln-url-decode-entities-in-buffer (decode-buffer)
  "Convert XML entities to string values:
    &amp;    ==>  &
    &lt;     ==>  <
    &gt;     ==>  >
    &quot;   ==>  \"
    &apos;   ==>  '
    &#[0-9]+ ==>  <appropriate char>"
  (save-excursion
    (set-buffer decode-buffer)
    (goto-char (point-min))
    (while (re-search-forward "&\\([^;\n]+\\);" nil t)
      (let ((ent-str (match-string-no-properties 1))
            (ent-point (match-beginning 1))
            untrans-char)
        (setq untrans-char
              (catch 'untrans-char
                (replace-match 
                 (cond
                  ;; simple xml entities
                  ((cdr-safe (assoc ent-str confluence-xml-entity-alist)))
                  ;; decimal number character entities
                  ((save-match-data
                     (and (string-match "^#\\([0-9]+\\)$" ent-str)
                          (cfln-number-entity-to-string (string-to-number (match-string-no-properties 1 ent-str))))))
                  ;; hexidecimal number character entities
                  ((save-match-data
                     (and (string-match "^#x\\([0-9A-Fa-f]+\\)$" ent-str)
                          (cfln-number-entity-to-string (string-to-number (match-string-no-properties 1 ent-str) 16)))))
                  ;; unknown entity
                  (t (concat "&" ent-str ";")))
                 t t)
                nil))
        (if untrans-char
            (progn 
              (message "Warning: could not decode character with code point %S" untrans-char)
              (replace-match "")
              (cfln-insert-untranslated-entity untrans-char)))
        (goto-char ent-point)))))

(defun cfln-number-entity-to-string (num)
  "Convert an xml number entity value to the appropriate character string."
  (let ((c (decode-char 'ucs num)))
    (if (not c)
        (throw 'untrans-char num))
    (string c)))

(defun cfln-insert-untranslated-entity (num)
  "Insert codes for an untranslateable entity (compatible with how later
versions of emacs handle untranslateable characters)."
  (let ((cur-pos (point))
        (disp-char (cfln-number-entity-to-string #xFFFD)))
;;     (insert disp-char)
    (insert ?\0)
    (put-text-property cur-pos (point) 'untranslated-utf-8 num)
    (compose-region cur-pos (point) disp-char)
    (message "FOO inserted %S" (buffer-substring cur-pos (point)))
))

(defun cfln-string-to-number-entity (str)
  "Convert a single character string to the appropriate xml number entity
value"
  (let ((c (encode-char (string-to-char str) 'ucs)))
    (if (or (not c)
            (get-text-property 0 'untranslated-utf-8 str))
          (throw 'untrans-char str))
    c))
        
(defun cfln-remove-untranslated-entity (pos)
  "Remove codes for an untranslateable entity (compatible with how later
versions of emacs handle untranslateable characters).  Returns the original
code point."
  (let ((untrans-value (get-text-property pos 'untranslated-utf-8)))
    (if untrans-value
        (progn
          (message "FOO deleting %S" (buffer-substring
                                      (1+ pos)
                         (next-single-property-change pos 'composition 
                                                      (current-buffer) 
                                                      (point-max))))
          ;; delete an _extra_ bytes, caller will replace first byte
          (delete-region (1+ pos)
                         (next-single-property-change pos 'composition 
                                                      (current-buffer) 
                                                      (point-max))))
      )
    untrans-value))


(defun cfln-url-encode-nonascii-entities-in-string (value)
  "Entity encodes any non-ascii values in the given string."
  (if (string-match "[^[:ascii:]]" value)
      (with-temp-buffer
        (insert value)
        (goto-char (point-min))
        (while (re-search-forward "[^[:ascii:]]" nil t) 
          (let ((match-str (match-string 0))
                encoded-char)
            (if (catch 'untrans-char
                  (setq encoded-char (cfln-string-to-number-entity match-str))
                  nil)
                (setq encoded-char (cfln-remove-untranslated-entity
                                    (match-beginning 0))))
	    (if encoded-char 
		(replace-match 
		 (concat "&#" (number-to-string encoded-char) ";") t t)
	      (message "Could not encode '%S'" match-str))
))
        (setq value (buffer-string))))
  value)

(if confluence-xml-substitute-special 
    (defadvice xml-substitute-special (around xml-substitute-special-fixed
                                              activate compile preactivate)
      "Fix (possibly) broken entity decoding in `xml-substitute-special'."
      (if confluence-do-coding
          (let ((decoded-string (cfln-url-decode-entities-in-string (ad-get-arg 0))))
            ;; if xml-rpc is expecting utf-8, re-encode this string
            (if xml-rpc-allow-unicode-string
                (setq decoded-string (encode-coding-string decoded-string 'utf-8 t)))
            (setq ad-return-value decoded-string))
        ad-do-it)))

(if confluence-xml-escape-string 
    (defadvice xml-escape-string (around xml-escape-string-fixed
                                         activate compile preactivate)
      "Fix double entity encoding caused by `xml-escape-string'."
      (if confluence-do-coding
          (setq ad-return-value (ad-get-arg 0))
        ad-do-it)))

(defadvice url-insert-entities-in-string (around url-insert-entities-in-string-nonascii
                                          activate compile preactivate)
  "Encode any non-ascii characters in the xml string after encoding the
basic entities."
  ad-do-it
  (if confluence-do-coding
      (setq ad-return-value 
            (cfln-url-encode-nonascii-entities-in-string ad-return-value))))

(defadvice url-display-percentage (around url-display-percentage-quiet
                                          activate compile preactivate)
  "Make `url-display-percentage' respect `url-show-status'."
  (if url-show-status
      ad-do-it))

;;;;;;;;;;;;;;;;;;;
;;; Confluence mode


(defvar confluence-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'confluence-get-page)
    (define-key map "c" 'confluence-create-page)
    (define-key map "=" 'confluence-ediff-current-page)
    (define-key map "m" 'confluence-ediff-merge-current-page)
    (define-key map "p" 'confluence-get-parent-page)
    (define-key map "r" 'confluence-rename-page)
    (define-key map "s" 'confluence-search)
    (define-key map "." 'confluence-get-page-at-point)
    (define-key map "*" 'confluence-pop-tag-stack)
    (define-key map "v" 'confluence-preview)
    (define-key map "a" 'confluence-get-attachment)
    (define-key map "b" 'confluence-browse-page)
    (define-key map "x" 'confluence-get-related-page)
    (define-key map "j" confluence-format-prefix-map)
    (define-key map "l"
      (let ((label-map (make-sparse-keymap)))
        (define-key label-map "a" 'confluence-add-label)
        (define-key label-map "r" 'confluence-remove-label)
        (define-key label-map "g" 'confluence-get-labels)        
        label-map))
    map)
  "Keybinding prefix map which can be bound for common functions in confluence
mode.")

(defun cfln-make-safe-file-name (fname)
  "Make a super-safe file name in pseudo-url-encoded format,
borrowed from `make-auto-save-file-name'"
  (let ((limit 0))
    (while (string-match "[^A-Za-z0-9-_.~#+]" fname limit)
	(let* ((character (aref fname (match-beginning 0)))
	       (replacement
                ;; For multibyte characters, this will produce more than
                ;; 2 hex digits, so is not true URL encoding.
                (format "%%%02X" character)))
	  (setq fname (replace-match replacement t t fname))
	  (setq limit (1+ (match-end 0))))))
  fname)

(defun cfln-make-auto-save-file-name (operation &rest args)
  "Creates the auto save file name for a confluence buffer by combining
the configured `confluence-auto-save-dir', the url host name, the
confluence space name, and the buffer name."
  (let ((file-name-handler-alist cfln-file-name-handler-alist))
    (let ((save-dir
           (concat (expand-file-name confluence-auto-save-dir) "/"
                   (cfln-make-safe-file-name (url-host (url-generic-parse-url confluence-page-url))) "/"
                   (cfln-make-safe-file-name (cfln-get-struct-value confluence-page-struct "space")))))
      (if (not (file-directory-p save-dir))
          (make-directory save-dir t))
      (concat save-dir "/" (cfln-make-safe-file-name (buffer-name))))))

(defun confluence-recover-this-page ()
  "Recovers a confluence page buffer with existing auto save information."
  (interactive)
  (when (and buffer-auto-save-file-name
             (yes-or-no-p (format "Recover auto save file %s? " buffer-auto-save-file-name)))
    (let ((file-contents nil)
          (recover-fname buffer-auto-save-file-name))
      (setq file-contents
            (with-temp-buffer
              (insert-file-contents recover-fname nil)
              (buffer-string)))
      (cfln-set-struct-value 'confluence-page-struct "content" file-contents)
      (cfln-insert-page confluence-page-struct nil nil t)
      (set-buffer-modified-p t)
      (set-buffer-auto-saved))
  ))

(defun confluence-base-mode-init ()
  "Init major mode for editing Confluence pages."
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'cfln-revert-page)
  (make-local-variable 'make-backup-files)
  (setq make-backup-files nil)
  (add-hook 'write-contents-hooks 'cfln-save-page)
  ;; we set this to some nonsense so save-buffer works
  (setq buffer-file-name (expand-file-name (concat "." (buffer-name)) "~/"))
)

(defun confluence-auto-save-init ()
  "Sets up auto save mode if enabled."
  (when (and confluence-auto-save-dir
             (not buffer-auto-save-file-name))
    (let ((cfln-file-name-handler-alist file-name-handler-alist)
          (file-name-handler-alist (list '(".*" . cfln-make-auto-save-file-name))))
      (auto-save-mode t))
    (if (and buffer-auto-save-file-name
             (file-exists-p buffer-auto-save-file-name))
        (message "%s has auto save data; consider M-x confluence-recover-this-page" (buffer-name)))))

(define-derived-mode confluence-mode confluence-edit-mode "Confluence"
  (confluence-base-mode-init)
  (confluence-auto-save-init))

(define-derived-mode confluence-search-mode confluence-edit-mode "ConfluenceSearch"
  (confluence-base-mode-init)
  "Set major mode for viewing Confluence Search results."
  (local-set-key [return] 'confluence-get-page-at-point)
)

(define-derived-mode confluence-xml-mode confluence-xml-edit-mode "ConfluenceXml"
  (confluence-base-mode-init)
  (confluence-auto-save-init)

  (when confluence-xml-reformat-on-load
    (let ((buffer-undo-list t)
              (inhibit-read-only t)
	      (after-change-functions nil)
              (mod (buffer-modified-p))
	      buffer-file-name buffer-file-truename)
	  (save-restriction
	    (widen)
	    (confluence-xml-reformat))
          (set-buffer-modified-p mod)))
)


;; TODO 
;; - extended link support
;;   - [$id] links?
;; - add more label support?
;; - change page preview to use async like attachments (xml parsing issues)
;; - add more structured browsing?
;; - funky searches:
;;   - labelText:<label>
;;   - title:<title> -- completion

(provide 'confluence)
;;; confluence.el ends here
