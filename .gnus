(require 'gnus-cite)
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq
 user-full-name "Haojun Bao"
 nnml-directory "~/private/mail/"
 message-default-charset 'utf-8
 gnus-select-method '(nnimap "localhost"
                             (nnimap-address "localhost")
                             (nnimap-server-port 143)
                             (nnimap-stream network))
 gnus-signature-limit 500
 message-cite-function 'message-cite-original-without-signature
 )

;; this puts the messages in several groups according to regexp
(add-hook 'gnus-article-display-hook
          '(lambda ()
             (gnus-article-de-quoted-unreadable)
             (gnus-article-emphasize)
             (gnus-article-hide-boring-headers)
             (gnus-article-hide-headers-if-wanted)
             (gnus-article-hide-pgp)
             (gnus-article-highlight)
             (gnus-article-highlight-citation)
             ))


(setq message-send-mail-function 'smtpmail-send-it
      user-mail-address "hjbao@marvell.com"
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 2025) ;set this port in davmail

(setq gnus-default-charset 'chinese-iso-8bit
      gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit))
      gnus-summary-show-article-charset-alist
      '((1 . chinese-iso-8bit)
        (2 . gbk)
        (3 . big5)
        (4 . utf-Cool)
        gnus-newsgroup-ignored-charsets
        '(unknown-8bit x-unknown iso-8859-1)))

(setq gnus-group-line-format "%m%M%L%5N/%-5R %25G: %D\n"
      gnus-topic-line-format "%i%n %A (%G) %v\n"
      gnus-summary-line-format ":%U%R%B%s%-80=  %-20,20f|%4L |\n")

(setq gnus-visible-headers
      "^\\(From:\\|To:\\|Cc:\\|Subject:\\|Date:\\|Followup-To:\\|X-Newsreader:\\|User-Agent:\\|X-Mailer:\\)")

(if window-system
    (setq gnus-sum-thread-tree-root ">>"
          gnus-sum-thread-tree-single-indent " >"
          gnus-sum-thread-tree-leaf-with-other "+-> "
          gnus-sum-thread-tree-indent " "
          gnus-sum-thread-tree-vertical "|"
          gnus-sum-thread-tree-single-leaf "`-> "
          gnus-sum-thread-tree-false-root "~>"))

(add-to-list 'gnus-newsgroup-variables 'mm-coding-system-priorities)
(setq gnus-parameters
      (nconc
       ;; Some charsets are just examples!
       '(("\\bcn\\.bbs\\..*" ;; Chinese
          (mm-coding-system-priorities
           '(iso-8859-1 gbk utf-8))))
       gnus-parameters))

(setq gnus-message-archive-group
      '((cond
         ((message-news-p)
          ;; News
          "sent-messages")
         ((message-mail-p)
          ;; Mail
          "sent-mails")))) 

(when (eq system-type 'windows-nt)
  (setq gnus-select-method
	'(nnmaildir "Gmail"
		    (directory "~/../Maildir")
		    (directory-files nnheader-directory-files-safe) 
		    (get-new-mail nil)))
  (setq smtpmail-auth-credentials 
    '(("localhost"
       2025
       "hjbao@marvell.com"
       nil)))
  (setq nntp-authinfo-file "~/../.authinfo"
	auth-sources '((:source "~/../.authinfo" :host t :protocol t))))
(autoload 'bbdb/send-hook "moy-bbdb" 
  "Function to be added to `message-send-hook' to notice records when sending messages" t)
 
(add-hook 'message-send-hook 'bbdb/send-hook) ; If you use Gnus

(add-hook 'mail-send-hook 'bbdb/send-hook)
;;Local Variables: ***
;;coding: utf-8 ***
;;End: ***
