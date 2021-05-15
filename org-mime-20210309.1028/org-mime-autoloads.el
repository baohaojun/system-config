;;; org-mime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-mime" "org-mime.el" (0 0 0 0))
;;; Generated autoloads from org-mime.el

(autoload 'org-mime-htmlize "org-mime" "\
Export a portion of an email to html using `org-mode'.
If called with an active region only export that region, otherwise entire body." t nil)

(autoload 'org-mime-org-buffer-htmlize "org-mime" "\
Create an email buffer of the current org buffer.
The email buffer will contain both html and in org formats as mime
alternatives.

The following file keywords can be used to control the headers:
#+MAIL_TO: some1@some.place
#+MAIL_SUBJECT: a subject line
#+MAIL_CC: some2@some.place
#+MAIL_BCC: some3@some.place
#+MAIL_FROM: sender@some.place

The cursor ends in the TO field." t nil)

(autoload 'org-mime-org-subtree-htmlize "org-mime" "\
Create an email buffer from current subtree.
If HTMLIZE-FIRST-LEVEL is t, first level subtree of current node is htmlized.

Following headline properties can determine the mail headers.
* subtree heading
  :PROPERTIES:
  :MAIL_SUBJECT: mail title
  :MAIL_TO: person1@gmail.com
  :MAIL_CC: person2@gmail.com
  :MAIL_BCC: person3@gmail.com
  :MAIL_FROM: sender@gmail.com
  :END:

\(fn &optional HTMLIZE-FIRST-LEVEL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mime" '("org-mime-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-mime-autoloads.el ends here
