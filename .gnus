(require 'gnus-cite)
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq
 user-full-name "Haojun Bao" ; fixme
 nnml-directory "~/private/mail/"
 message-default-charset 'utf-8
 gnus-signature-limit 500
 message-cite-function 'message-cite-original-without-signature
 )

;; this puts the messages in several groups according to regexp
(add-hook 'gnus-article-mode-hook
          '(lambda ()
             (gnus-article-de-quoted-unreadable)
             (gnus-article-emphasize)
             (gnus-article-hide-boring-headers)
             (gnus-article-hide-headers-if-wanted)
             (gnus-article-highlight)
             (gnus-article-highlight-citation)
             ))

(setq gnus-default-charset 'chinese-iso-8bit
      gnus-summary-show-article-charset-alist
      '((1 . chinese-iso-8bit)
        (2 . gbk)
        (3 . big5)
        (4 . utf-Cool)
        gnus-newsgroup-ignored-charsets
        '(unknown-8bit x-unknown iso-8859-1)))

(setq gnus-group-line-format "%m%M%L%5N/%-5R %25G: %D\n"
      gnus-summary-line-format ":%U%R%B%s%-80=  %-20,20f|%4L |\n")

(setq gnus-visible-headers
      "^\\(From:\\|To:\\|Cc:\\|Subject:\\|Date:\\|Followup-To:\\|X-Newsreader:\\|User-Agent:\\|X-Mailer:\\)")

(add-to-list 'gnus-newsgroup-variables 'mm-coding-system-priorities)

(setq gnus-message-archive-group
      '((cond
         ((message-news-p)
          ;; News
          "sent-messages")
         ((message-mail-p)
          ;; Mail
          "nnmaildir+Gmail:SentMails"))))

(setq gnus-select-method
      '(nnmaildir "Gmail"
                  (directory "~/Maildir")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil)))

(defadvice nnmaildir--prepare (around no-ephmeral (server group) activate)
  (let ((server
         (if (and (stringp server)
             (string= server "Gmail-ephemeral"))
             "Gmail"
           server)))
    ad-do-it))

(bhj-set-smtp-cred-to-company-mail)
(autoload 'bbdb/send-hook "moy-bbdb"
  "Function to be added to `message-send-hook' to notice records when sending messages" t)

(add-hook 'message-send-hook 'bbdb/send-hook) ; If you use Gnus

(add-hook 'mail-send-hook 'bbdb/send-hook)

;;Local Variables: ***
;;coding: utf-8 ***
;;End: ***
