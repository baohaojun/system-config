(require 'gnus-cite)

(setq
 user-full-name "Haojun Bao"
 nnml-directory "~/private/mail/"
 message-default-charset 'utf-8
 gnus-select-method '(nnml "")
 gnus-signature-limit 500
 message-cite-function 'message-cite-original-without-signature
)

 ; this puts the messages in several groups according to regexp
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

(setq gnus-message-archive-method
       '(nnfolder "archive"
                  (nnfolder-inhibit-expiry t)
                  (nnfolder-active-file "~/Mail/sent-mail/active")
                  (nnfolder-directory "~/Mail/sent-mail/")
                  (nnfolder-get-new-mail nil)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "baohaojun@gmail.com" nil))
      user-mail-address "baohaojun@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mail-default-reply-to "haojun.bao@borqs.com"
      smtpmail-local-domain "borqs.com")


;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.bizmail.yahoo.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.bizmail.yahoo.com" 587 "haojun.bao@borqs.com" nil))
;;       smtpmail-default-smtp-server "smtp.bizmail.yahoo.com"
;;       user-mail-address "haojun.bao@borqs.com"
;;       smtpmail-smtp-server "smtp.bizmail.yahoo.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-local-domain "yahoo.com")

(setq gnus-default-charset 'chinese-iso-8bit
      gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit))
      gnus-summary-show-article-charset-alist
      '((1 . chinese-iso-8bit)
        (2 . gbk)
        (3 . big5)
        (4 . utf-Cool)
        gnus-newsgroup-ignored-charsets
        '(unknown-8bit x-unknown iso-8859-1)))

(setq nnmail-split-methods
      '(
        ("mail.sent.mail" "^From:.*haojun.bao")
        ("mail.hotmail" "^To:.*baohj_zj@hotmail.com")
        ("mail.yahoo" "^To:.*baohaojun@yahoo.com")
        ("mail.gmail" "^To:.*baohaojun@gmail.com")
        ("mail.haojun.bao" "^To:.*haojun.bao")
        ("mail.haojun.bao" "^Cc.*haojun.bao")
        ("mail.a22242" "^To:.*a22242")
        ("mail.a22242" "^Cc:.*a22242")
        ("mail.samba" "^To:.*samba")
        ("mail.samba" "^Cc:.*samba")
        ("mail.misc" "")))

(setq gnus-message-archive-group
      '((if (message-news-p)
            "nnml:mail.sent.news"
          "nnml:mail.sent.mail")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq message-send-mail-function 'smtpmail-send-it ;;
;;       smtpmail-default-smtp-server "bhj3"          ;;
;;       smtpmail-smtp-server "bhj3")                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Local Variables: ***
;;coding: utf-8 ***
;;End: ***
