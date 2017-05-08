;;; org-mac-link-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-mac-link" "org-mac-link.el" (22794 35033
;;;;;;  0 0))
;;; Generated autoloads from org-mac-link.el

(autoload 'org-mac-grab-link "org-mac-link" "\
Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point.

\(fn)" t nil)

(autoload 'org-mac-firefox-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-firefox-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-vimperator-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-vimperator-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-chrome-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-chrome-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-safari-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-safari-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-together-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-together-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-finder-item-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-finder-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-addressbook-item-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-addressbook-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-skim-get-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-skim-insert-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-acrobat-get-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-acrobat-insert-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-outlook-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-outlook-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject
of the active mail in Microsoft Outlook.app and make a link out
of it.

\(fn)" t nil)

(autoload 'org-mac-outlook-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all mac-outlook:// links within
heading's first level.  If heading doesn't exist, create it at
point-max.  Insert list of mac-outlook:// links to flagged mail
after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-evernote-note-insert-selected "org-mac-link" "\
Insert a link to the notes currently selected in Evernote.app.
This will use AppleScript to get the note id and the title of the
note(s) in Evernote.app and make a link out of it/them.

\(fn)" t nil)

(autoload 'org-mac-devonthink-item-insert-selected "org-mac-link" "\
Insert a link to the item(s) currently selected in DEVONthink Pro Office.
This will use AppleScript to get the `uuid'(s) and the name(s) of the
selected items in DEVONthink Pro Office and make link(s) out of it/them.

\(fn)" t nil)

(autoload 'org-mac-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it.

\(fn)" t nil)

(autoload 'org-mac-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all message:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of message:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-mac-link-autoloads.el ends here
