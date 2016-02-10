;;; bbdb-vcard.el --- vCard import/export for BBDB

;; Copyright (c) 2010 Bert Burgemeister

;; Author: Bert Burgemeister <trebbu@googlemail.com>
;;         Toke Høiland-Jørgensen
;;         Kevin Brubeck Unhammer
;;         Steve Purcell
;;         Vincent Geddes <vincent.geddes@gmail.com>
;; Keywords: data calendar mail news
;; URL: http://github.com/vgeddes/bbdb-vcard
;; Package-Requires: ((bbdb "3.0"))
;; Version: 0.4.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; The exporter functionality is based on code from
;; bbdb-vcard-export.el by Jim Hourihan and Alex Schroeder.

;;; Commentary:
;;
;; Import and export of vCards as defined in RFC 2425 and RFC 2426
;; to/from The Insidious Big Brother Database (BBDB) v3.
;;
;; For user and developer documentation, refer to the bundled BBDB vCard
;; info manual.
;;
;; Mapping of vCard types to BBDB types:
;;
;; +-------------------------+-----------------------------------------+
;; | VCARD TYPE;PARAMETERS   | STORAGE IN BBDB                         |
;; |                         |                                         |
;; |-------------------------+-----------------------------------------|
;; | VERSION                 | -                                       |
;; | REV                     | -                                       |
;; |-------------------------+-----------------------------------------|
;; | N                       | First occurrence:                       |
;; |                         | Firstname                               |
;; |                         | Lastname                                |
;; |                         |                                         |
;; |                         | Rest:                                   |
;; |                         | AKAs (append)                           |
;; |-------------------------+-----------------------------------------|
;; | FN                      | AKAs (append)                           |
;; | NICKNAME                | AKAs (append)                           |
;; |-------------------------+-----------------------------------------|
;; | ORG                     | Organizations (append)                  |
;; |-------------------------+-----------------------------------------|
;; | ADR;TYPE=x,HOME,y       | Addresses<Home                          |
;; | ADR;TYPE=x;TYPE=HOME    | Addresses<Home                          |
;; | ADR;TYPE=x,WORK,y       | Addresses<Office                        |
;; | ADR;TYPE=x;TYPE=WORK    | Addresses<Office                        |
;; | ADR;TYPE=x,y,z          | Addresses<x,y,z                         |
;; | ADR;TYPE=x;TYPE=y       | Addresses<x,y                           |
;; | ADR                     | Addresses<Office                        |
;; |-------------------------+-----------------------------------------|
;; | TEL;TYPE=x,HOME,y       | Phones<Home (append)                    |
;; | TEL;TYPE=x;TYPE=HOME    | Phones<Home (append)                    |
;; | TEL;TYPE=x,WORK,y       | Phones<Office (append)                  |
;; | TEL;TYPE=x;TYPE=WORK    | Phones<Office (append)                  |
;; | TEL;TYPE=x,CELL,y       | Phones<Mobile (append)                  |
;; | TEL;TYPE=x;TYPE=CELL    | Phones<Mobile (append)                  |
;; | TEL;TYPE=x,y,z          | Phones<x,y,z (append)                   |
;; | TEL;TYPE=x;TYPE=y       | Phones<x,y (append)                     |
;; | TEL                     | Phones<Office (append)                  |
;; |-------------------------+-----------------------------------------|
;; | EMAIL;TYPE=x,y,z        | Net-Addresses (append)                  |
;; | URL                     | Xfields<url                             |
;; |-------------------------+-----------------------------------------|
;; | BDAY                    | Xfields<anniversary (append as birthday)|
;; | X-BBDB-ANNIVERSARY      | Xfields<anniversary (append)            |
;; |-------------------------+-----------------------------------------|
;; | PHOTO (inline base64)   | Xfields<image-filename                  |
;; |       (uri)             | Xfields<image-uri                       |
;; |-------------------------+-----------------------------------------|
;; | SOUND (inline base64)   | Xfields<sound-filename                  |
;; |       (uri)             | Xfields<sound-uri                       |
;; |-------------------------+-----------------------------------------|
;; | KEY   (inline base64)   | Xfields<gpg-key-filename                |
;; |       (uri)             | Xfields<gpg-key-uri                     |
;; |-------------------------+-----------------------------------------|
;; | NOTE                    | Xfields<notes (append)                  |
;; | CATEGORIES              | Xfields<mail-alias (append)             |
;; | SORT-STRING             | Xfields<sort-string                     |
;; | GEO                     | Xfields<geo                             |
;; | TZ                      | Xfields<tz                              |
;; | LABEL                   | Xfields<label                           |
;; | LOGO                    | Xfields<logo                            |
;; | TITLE                   | Xfields<title                           |
;; | ROLE                    | Xfields<role                            |
;; | AGENT                   | Xfields<agent                           |
;; | MAILER                  | Xfields<mailer                          |
;; | UID                     | Xfields<uid                             |
;; | PRODID                  | Xfields<prodid                          |
;; | CLASS                   | Xfields<class                           |
;; | X-BBDB-FOO              | Xfields<foo                             |
;; | X-FOO                   | Xfields<x-foo                           |
;; |-------------------------+-----------------------------------------|
;; | ANYJUNK;a=x;b=y         | Xfields<anyjunk;a=x;b=y                 |
;; +-------------------------+-----------------------------------------+
;;

;;; Code:

(require 'cl-lib)
(require 'bbdb)
(require 'vcard)
(require 'bbdb-com)

(defconst bbdb-vcard-version "0.4.1"
  "Version of the vCard importer/exporter.
The major part increases on user-visible changes.")

;;; Custom Variables:

(defgroup bbdb-vcard nil
  "Customizations for vCards"
  :group 'bbdb)

(defcustom bbdb-vcard-directory
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory)
           "bbdb-vcard"))
  "The directory under which vCard media attachments are stored"
  :group 'bbdb-vcard
  :type 'directory
  :set-after '(user-emacs-directory)
)

(defcustom bbdb-vcard-skip-on-import '("^X-GSM-")
  "Regexps-list describing vCard elements that are to be discarded during import.
For example: (\"^X-GSM-\" \"^X-MS-\")"
  :group 'bbdb-vcard)

(defcustom bbdb-vcard-skip-on-export nil
  "Regexp-list describing bbdb fields that are to be discarded during export.
Example: `(\"field1\\|field2\" \"field3\")'."
  :group 'bbdb-vcard)

(defcustom bbdb-vcard-skip-valueless t
  "Skip vCard element types with an empty value.
Nil means insert empty types into BBDB."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-import-translation-table
  '(("CELL\\|CAR" . "cell")
    ("WORK" . "work")
    ("DOM\\|HOME" . "home"))
  "Label translation on vCard import.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (VCARD-LABEL-REGEXP . BBDB-LABEL).  One entry
should map a default BBDB label to the empty string (`\"^$\"') which
corresponds to unlabelled vCard elements."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-try-merge t
  "Try to merge vCards into existing BBDB records.
Nil means create a fresh bbdb record each time a vCard is read."
  :group 'bbdb-vcard
  :type 'boolean)

(defcustom bbdb-vcard-type-canonicalizer 'upcase
  "Function to apply to vCard type names on export.
Most reasonable choices are `upcase' and `downcase'."
  :group 'bbdb-vcard
  :type 'function)

(defcustom bbdb-vcard-x-bbdb-candidates
  '(attribution
    finger-host
    gnus-score
    mark-char
    mail-name
    face
    tex-name
    aka)                                ; not sure what this is for
  "List of translatable BBDB user field names.
On export to a vCard, they are transformed into vCard-compliant
extended types by prepending `X-BBDB-'.  On (re-)import, this prefix
is removed again."
  :group 'bbdb-vcard
  :type '(repeat symbol))

(defcustom bbdb-vcard-export-translation-table
  '(("Mobile" . "cell")
    ("Office" . "work"))
  "Label translation on vCard export.
Alist with translations of location labels for addresses and phone
numbers.  Cells are (BBDB-LABEL-REGEXP . VCARD-LABEL)."
  :group 'bbdb-vcard
  :type '(alist :key-type
                (choice regexp (const :tag "Empty (as default)" "^$"))
                :value-type string))

(defcustom bbdb-vcard-export-coding-system
  'utf-8-dos        ; dos line endings mandatory according to RFC 2426
  "Coding system to use when writing vCard files."
  :group 'bbdb-vcard
  :type 'symbol)

(defcustom bbdb-vcard-default-dir "~/exported-vcards/"
  "Default storage directory for exported vCards.
Nil means current directory."
  :group 'bbdb-vcard
  :type '(choice directory (const :tag "Current directory" nil)))

(defcustom bbdb-vcard-name-imported-priority
  '(first-last formated-name bbdb-vcard-generate-bbdb-name)
  "Set which name should be store into bbdb database when import a vcard.
the valid name first found will be used.

User can add a function into this list, the return value of function will
regard as a candidate of bbdb name. first-name, last-name and formated-name
of vcard will be inputed as arguments. user can see reference function:
`bbdb-vcard-generate-bbdb-name'."
  :group 'bbdb-vcard)

(defvar bbdb-vcard-media-directory
  (file-name-as-directory "media")
  "The relative subdirectory under `bbdb-vcard-directory' where
media objects are stored")


(defun bbdb-vcard-image-basename (record)
  "Returns the image filename (sans suffix), for a record.
This is meant to be bound to `bbdb-image', so that BBBD can lookup the
image for the record."
  (let ((image-filename (bbdb-vcard-bbdb-record-field record 'image-filename)))
    (if image-filename
        (file-name-base image-filename)
      nil)))

(defvar bbdb-vcard-media-types
  ;; sounds
  '(("basic" ("sound" "snd" "audio/basic"))
    ("wav" ("sound" "wav" "audio/wav"))
    ("ogg" ("sound" "ogg" "audio/ogg"))
    ("mp3" ("sound" "mp3" "audio/mpeg"))
    ("m4a" ("sound" "m4a" "audio/mpeg"))
    ("aac" ("sound" "aac" "audio/aac"))
    ;; images
    ("png" ("image" "png" "image/png"))
    ("jpeg" ("image" "jpg" "image/jpeg"))
    ("gif" ("image" "gif" "image/gif"))
    ("tiff" ("image" "tiff" "image/tiff"))
    ("xbm"  ("image" "xbm" "image/xbm"))
    ("xpm"  ("image" "xpm" "image/xpm"))
    ;; keys
    ("gpg" ("key" "asc" "application/pgp-keys"))
    ("pgp" ("key" "asc" "application/pgp-keys")))
  "A list of supported media types. Each item is a media descriptor of
the form (TYPE (PREFIX SUFFIX MIMETYPE)). TYPE is equivalent to the
corresponding 'TYPE' parameter value in a vCard field. The string PREFIX and
SUFFIX are used to form a unique filename for the media object in order to
form a unique filename. MIMETYPE is currently unused.")

(defvar bbdb-vcard-mime-types
  '(("audio/basic" . "basic")
    ("audio/wav" . "wav")
    ("audio/ogg" . "ogg")
    ("audio/mp3" . "mp3")
    ("audio/mpeg" . "mp3")
    ("audio/mp4" . "m4a")
    ("audio/aac" . "aac")
    ("image/png" . "png")
    ("image/jpeg" . "jpeg")
    ("image/gif" . "gif")
    ("image/tiff" . "tiff")
    ("image/xbm" . "xbm")
    ("image/xpm" . "xpm")
    ("application/pgp-keys" . "gpg"))
  "Maps a mime-type to the name of a media descriptor")


(defun bbdb-vcard-initialize ()
  "Initializes bbdb-vcard, particularly adding the media directory
to `bbdb-image-path'."
  ;; bbdb-image-path
  (if (and bbdb-image-path (listp bbdb-image-path))
     (add-to-list 'bbdb-image-path
                (concat bbdb-vcard-directory bbdb-vcard-media-directory))
     (setq bbdb-image-path
           (list (concat bbdb-vcard-directory bbdb-vcard-media-directory))))
  ;; bbdb-image
  (unless bbdb-image
    (setq bbdb-image #'bbdb-vcard-image-basename)))

;;;###autoload
(defun bbdb-vcard-import-region (begin end &optional source-name)
  "Import the vCards between BEGIN and END into BBDB.
Existing BBDB records may be altered."
  (interactive "r")
  (let ((results
         (cl-remove-if 'null
                       (bbdb-vcard-iterate-vcards
                        'bbdb-vcard-import-vcard
                        (buffer-substring-no-properties begin end)))))
    (cond
     ((and (null results) source-name)
      (message "No vCard objects were found in %s" source-name))
     ((null results)
      (message "No vCard objects were found in region"))
     ((= (length results) 1)
      (message "Imported 1 vCard object into BBDB"))
     (t
      (message "Imported %s vCard objects into BBDB" (length results))))))

;;;###autoload
(defun bbdb-vcard-import-buffer (vcard-buffer)
  "Import vCards from VCARD-BUFFER into BBDB.
Existing BBDB records may be altered."
  (interactive (list (current-buffer)))
  (set-buffer vcard-buffer)
  (bbdb-vcard-import-region (point-min) (point-max) (buffer-name vcard-buffer)))

;;;###autoload
(defun bbdb-vcard-import-file (vcard-file)
  "Import vCards from VCARD-FILE into BBDB.
If VCARD-FILE is a wildcard, import each matching file.  Existing BBDB
records may be altered."
  (interactive "fvCard file: ")
    (with-temp-buffer
      (insert-file-contents vcard-file)
      (bbdb-vcard-import-region (point-min) (point-max) vcard-file)))

;;;###autoload
(defun bbdb-vcard-export
  (filename-or-directory all-records-p one-file-per-record-p &optional allow-overwrite)
  "From Buffer *BBDB*, write one or more record(s) as vCard(s) to file(s).
\\<bbdb-mode-map>\
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-vcard-export]\"\
is used instead of simply \"\\[bbdb-vcard-export]\", then export all \
records currently
in the *BBDB* buffer.  If used with prefix argument, store records
in individual files."
  (interactive
   (let ((default-filename              ; argument filename-or-directory
           (bbdb-vcard-make-file-name (bbdb-current-record nil)))
         (all-records-p (bbdb-do-all-records)))
     (list
      (if all-records-p
          (if current-prefix-arg
              (read-directory-name "Write vCard files to directory: "
                                   bbdb-vcard-default-dir nil 42)
            (read-file-name
             "Write vCards to file: "
             bbdb-vcard-default-dir
             nil nil
             (format-time-string "%Y-%m-%dT%H%M.vcf" (current-time))))
        (read-file-name "Write current record to vCard file: "
                        bbdb-vcard-default-dir nil nil default-filename))
      all-records-p           ; argument all-records-p
      current-prefix-arg)))   ; argument one-file-per-record-p
  (if all-records-p
      (let ((records (progn (set-buffer bbdb-buffer-name)
                            (mapcar 'car bbdb-records)))
            used-up-basenames)          ; keep them unique
        (if one-file-per-record-p
            (progn
              (dolist (record records)
                (with-temp-buffer
                  (let ((basename
                         (bbdb-vcard-make-file-name record
                                                    used-up-basenames)))
                    (insert (bbdb-vcard-from record))
                    (bbdb-vcard-write-buffer
                     (concat filename-or-directory basename)
                     allow-overwrite)
                    (push basename used-up-basenames))))
              (message "Wrote %d vCards to %s"
                       (length used-up-basenames) filename-or-directory))
          (with-temp-buffer     ; all visible BBDB records in one file
            (dolist (record records)
              (insert (bbdb-vcard-from record)))
            (bbdb-vcard-write-buffer filename-or-directory allow-overwrite))))
    (let ((vcard (bbdb-vcard-from (bbdb-current-record nil)))) ; current record
      (with-temp-buffer
        (insert vcard)
        (bbdb-vcard-write-buffer filename-or-directory allow-overwrite)))))

;;;###autoload
(defun bbdb-vcard-export-to-kill-ring (all-records-p)
  "From Buffer *BBDB*, copy one or more record(s) as vCard(s) to the kill ring.
\\<bbdb-mode-map>\
If \"\\[bbdb-apply-next-command-to-all-records]\
\\[bbdb-vcard-export-to-kill-ring]\"\
is used instead of simply \"\\[bbdb-vcard-export-to-kill-ring]\", \
then export all records currently in
the *BBDB* buffer."
  (interactive (let ((all-records-p (bbdb-do-all-records)))
                 (list all-records-p)))
  (if all-records-p
      (let ((records (progn (set-buffer bbdb-buffer-name)
                            (mapcar 'car bbdb-records))))
        (kill-new "")
        (dolist (record records)
          (kill-append (bbdb-vcard-from record) nil))
        (message "Saved %d records as vCards" (length records)))
    (kill-new (bbdb-vcard-from (bbdb-current-record nil)))
    (message "Saved record as vCard")))

(define-key bbdb-mode-map [(v)] 'bbdb-vcard-export)
(define-key bbdb-mode-map [(V)] 'bbdb-vcard-export-to-kill-ring)

(defun bbdb-vcard-iterate-vcards (vcard-processor vcards)
  "Apply VCARD-PROCESSOR successively to each vCard in string VCARDS.
When VCARDS is nil, return nil.  Otherwise, return t."
  (with-temp-buffer
    (insert vcards)
    (goto-char (point-min))
    ;; Change CRLF into CR if necessary, dealing with inconsistent line
    ;; endings.
    (while (re-search-forward "\r\n" nil t)
      (replace-match "\n" nil nil nil 1))
    (let ((vcards-normalized (bbdb-vcard-unfold-lines (buffer-string)))
          (results nil))
      (erase-buffer)
      (insert vcards-normalized)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([[:alnum:]-]*\\.\\)?*BEGIN:VCARD[\n[:print:][:cntrl:]]*?\\(^\\([[:alnum:]-]*\\.\\)?END:VCARD\\)"
              nil t)
        (let ((vcard (match-string 0)))
          (if (string= "3.0" (bbdb-vcard-version-of vcard))
              (push (funcall vcard-processor vcard) results)
            (push (funcall vcard-processor      ; probably a v2.1 vCard
                           (bbdb-vcard-unfold-lines
                            (bbdb-vcard-convert-to-3.0 vcard)))
                  results))))
      results)))

(defun bbdb-vcard-version-of (vcard)
  "Return version number string of VCARD."
  (with-temp-buffer
    (insert vcard)
    (car (bbdb-vcard-values-of-type "version" "content"))))


(defvar bbdb-vcard-type-spec
  '(("FN" * nil nil t)
    ("N" * t t t)
    ("NICKNAME" * nil t t)
    ("ORG" * t nil t)
    ("EMAIL" * nil nil t)
    ("TEL" * nil nil t)
    ("ADR" * t t t)
    ("URL" * nil nil t)
    ("NOTE" * nil nil t)
    ("BDAY" * nil nil t)
    ("CATEGORIES" * nil t t)
    ("MAILER" * nil nil t)
    ("PHOTO" * nil nil t)
    ("SOUND" * nil nil t)
    ("KEY" * nil nil t)
    ("UID" * nil nil t)
    ("X-BBDB-ANNIVERSARY" * nil nil t)
    ("X-BBDB-WEDDING" * nil nil t)
    ("X-ABUID" * nil nil t))
  "A list of recognized vCard entries. Each member is of the
form (TYPE CARDINALITY STRUCTURED-P LIST-P UNESCAPE-P). TYPE is the name of the
entry, STRUCTURED-P indicates that the value is structured and each component is
separated by ';'. LIST-P indicates that the value is a list of text items,
separated by ','. If both STRUCTURED-P and LIST-P are non-nil, then the
value is considered a structured value where each component is a
list of text items. if UNESCAPE-P is non-nil the value is unescaped")


(defun bbdb-vcard-scardize (vcard)
  "Converts a vCard into an Sexp-Card of the form:
 ((TYPE (((PARAM . VALUE) ...) ...)) ...)

TYPE is a symbol and a member of `bbdb-vcard-import-types'. PARAM is a
a symbol that represents a vCard property parameter. VALUE is a string or
nil.

PARAM is not limited to, but can be equal to any of the
following values:

  `content': The actual content of the vCard field.
  `value': The 'VALUE' property parameter
  `type': The 'TYPE' property parameter

See http://tools.ietf.org/search/rfc6350#section-5 for a full list
of possible property parameters"

  (with-temp-buffer
    (insert vcard)
    (let ((scard
           (cl-remove-if
            (lambda (element)
              (null (cadr element)))
            (mapcar 'bbdb-vcard-scardize-type bbdb-vcard-type-spec))))
      scard)))

(defun bbdb-vcard-scardize-type (type)
  (cl-destructuring-bind
      (name cardinality structured-p list-p unescape-p) type
    (list name
          (mapcar
           (lambda (element)
             (mapcar
              (lambda (param)
                (let ((value
                       (if (and (equal (car param) "content") list-p)
                           (bbdb-vcard-process-strings
                            (lambda (value)
                              (bbdb-vcard-split-structured-text value ","))
                            (cdr param))
                         (cdr param))))
                  (list (car param)
                        (if (listp value)
                            (mapcar
                             (lambda (value)
                               (cond
                                ((and unescape-p (listp value))
                                 (mapcar
                                  (lambda (list-of-strings)
                                    (bbdb-vcard-unescape-strings list-of-strings))
                                  value))
                                ((and unescape-p (stringp value))
                                 (bbdb-vcard-unescape-strings value))
                                (t value)))
                             value)
                          (if unescape-p
                              (bbdb-vcard-unescape-strings value)
                            value)))))
              element))
           (bbdb-vcard-elements-of-type name nil structured-p)))))

(defun bbdb-vcard-search (scard type &optional param)
  "Search bbdb records from `scard' by `type' and `param'.
if `type' name match `bbdb-vcard-skip-on-import' or its
elements (when it is a regexps-list), return `nil'."
  (let* ((elements (cadr (assoc type scard)))
         (regexps bbdb-vcard-skip-on-import)
         (skip-import-p
          (if (stringp regexps)
              (string-match-p regexps type)
            (cl-some
             #'(lambda (regexp)
                 (when regexp
                   (string-match-p regexp type)))
             regexps))))
    (unless skip-import-p
      (if param
          (cl-remove-if 'null
                        (mapcar (lambda (element)
                                  (cadr (assoc param element)))
                                elements))
        elements))))

(defmacro bbdb-vcard-search-intersection
  (records &optional name organization mail xfields phone)
  "Search RECORDS for records that match each non-nil argument."
  (let*
      ((phone-search
        (if phone `(when ,phone
                     (bbdb-search ,records nil nil nil nil ,phone nil))
          records))
       (xfields-search
        (if xfields `(when ,xfields
                       (bbdb-search ,phone-search nil nil nil ,xfields nil nil))
          phone-search))
       (mail-search
        (if mail `(when ,mail
                    (bbdb-search ,xfields-search nil nil ,mail nil nil nil))
          xfields-search))
       (organization-search
        (if organization `(when ,organization
                            (bbdb-search
                             ,mail-search nil ,organization nil nil nil nil))
          mail-search))
       (name-search
        (if name `(when ,name (bbdb-search ,organization-search ,name))
          organization-search)))
    name-search))

(defun bbdb-vcard-import-vcard (vcard)
  (condition-case-unless-debug err
      (bbdb-vcard-import-vcard-internal vcard)
    ((error nil)
     (progn
       (message "Error encountered while parsing vcard: %s" err)
       nil))))

(defun bbdb-vcard-generate-bbdb-name (first-name last-name formated-name)
  "Generate bbdb name depend on `first-name', `last-name' and `formated-name',
Please see also `bbdb-vcard-name-imported-priority'."
  (let ((first-name (or first-name ""))
        (last-name  (or last-name "")))
    (cond
     ((and (string-match-p "\\cc" first-name)
           (string-match-p "\\cc" last-name))
      (concat last-name first-name))
     (t (concat first-name " " last-name)))))

(defun bbdb-vcard-import-vcard-internal (vcard-or-scard)
  "Store VCARD (version 3.0) in BBDB.
Extend existing BBDB records where possible."
  (let* ((scard (if (stringp vcard-or-scard)
                    (bbdb-vcard-scardize vcard-or-scard)
                  vcard-or-scard))
         (raw-name (car (bbdb-vcard-search scard "N" "content")))
         (name-components (bbdb-vcard-unvcardize-name raw-name))
         (vcard-formatted-name (car (bbdb-vcard-search scard "FN" "content")))
         ;; Name suitable for storing in BBDB
         (name (cl-find-if
                #'(lambda (x)
                    (if (stringp x)
                        (> (length x) 0)
                      (or (car x) (cdr x))))
                (mapcar
                 #'(lambda (x)
                     (cond
                      ((eq x 'first-last)
                       (cons (nth 0 name-components)
                             (nth 1 name-components)))
                      ((eq x 'formated-name) vcard-formatted-name)
                      ((functionp x)
                       (funcall x (nth 0 name-components)
                                (nth 1 name-components)
                                vcard-formatted-name))))
                 bbdb-vcard-name-imported-priority)))
         ;; A unique name, which will replace origin name
         ;; when encounter "need-solved-by-hand" conflict.
         (name-used-mark-conflict
          (concat "@Conflict"
                  (format-time-string "%M%S" nil t)
                  "@-"
                  (or vcard-formatted-name
                      (bbdb-vcard-generate-bbdb-name
                       (nth 0 name-components)
                       (nth 1 name-components)
                       vcard-formatted-name)
                      "???")))
         (name-need-manual-edit-p nil)
         ;; Affixes suitable for storing in BBDB
         (vcard-affixes (nth 2 name-components))
         ;; Name to search for in BBDB now:
         (name-to-search-for
          (when raw-name (if (stringp raw-name)
                             raw-name
                           (let* ((given-name (nth 1 raw-name))
                                  (family-name (nth 0 raw-name))
                                  (separator
                                   (if (or (not given-name)  ; if given-name or family-name
                                           (not family-name) ; is `nil', whitespace is needless.
                                           ;; No whitespace needed between
                                           ;; given-name and family-name for CJK users.
                                           (string-match-p "\\cc" (or given-name ""))
                                           (string-match-p "\\cc" (or family-name "")))
                                       " *"
                                     " +")))
                             (concat "^\\(" given-name separator family-name "\\)$\\|"
                                     "^\\(" family-name separator given-name "\\)$")))))
         (vcard-nicknames
          (bbdb-vcard-flatten (bbdb-vcard-search scard "NICKNAME" "content")))
         (vcard-nicknames-backup nil)
         ;; Organization suitable for storing in BBDB:
         (vcard-org
          (mapcar (lambda (org)
                    (bbdb-vcard-unvcardize-org org))
                  (bbdb-vcard-search scard "ORG" "content")))
         ;; Organization to search for in BBDB now:
         (org-to-search-for (car vcard-org))
         ;; Email suitable for storing in BBDB:
         (vcard-email (bbdb-vcard-search scard "EMAIL" "content"))
         ;; Email to search for in BBDB now:
         (email-to-search-for
          (when vcard-email
            (concat "\\(" (bbdb-join vcard-email "\\)\\|\\(") "\\)")))
         ;; Phone numbers suitable for storing in BBDB:
         (vcard-email-backup nil)
         (vcard-tels
          (mapcar (lambda (tel)
                    (vector (bbdb-vcard-translate (cadr (assoc "type" tel)))
                            (cadr (assoc "content" tel))))
                  (bbdb-vcard-search scard "TEL")))
         ;; Phone numbers to search for in BBDB now:
         (tel-to-search-for
          (when vcard-tels
            (concat "\\("
                    (mapconcat (lambda (x) (elt x 1))
                               vcard-tels "\\)\\|\\(")
                    "\\)")))
         (vcard-tels-backup nil)
         ;; Addresses
         (vcard-adrs
          (mapcar 'bbdb-vcard-unvcardize-adr
                  (bbdb-vcard-search scard "ADR")))
         ;; URLs
         (vcard-url (car (bbdb-vcard-search scard "URL" "content")))
         ;; Notes
         (vcard-notes (car (bbdb-vcard-search scard "NOTE" "content")))
         ;; Bdays
         (vcard-bday (bbdb-vcard-unvcardize-date-time
                      (car (bbdb-vcard-search scard "BDAY" "content"))))
         (bday-to-search-for vcard-bday)
         ;; Non-birthday anniversaries, probably exported by ourselves:
         (vcard-x-bbdb-anniversaries
          (car (bbdb-vcard-search scard "X-BBDB-ANNIVERSARY" "content")))
         (vcard-x-bbdb-weddings
          (car (bbdb-vcard-search scard "X-BBDB-WEDDING" "content")))
         ;; UIDs
         (vcard-uid
          (car (bbdb-vcard-search scard "UID" "content")))
         (vcard-x-abuid
          (car (bbdb-vcard-search scard "X-ABUID" "content")))
         ;; Categories
         (vcard-categories
          (bbdb-concat 'mail-alias
                       (car (bbdb-vcard-search scard "CATEGORIES" "content"))))
         (vcard-photo (car (bbdb-vcard-search scard "PHOTO")))
         (vcard-sound (car (bbdb-vcard-search scard "SOUND")))
         (vcard-key (car (bbdb-vcard-search scard "KEY")))
         (vcard-xfields nil)
         (record
          (or
           ;; (a) try organization and mail and name:
           (car (and bbdb-vcard-try-merge
                     (bbdb-vcard-search-intersection
                      (bbdb-records)
                      name-to-search-for
                      org-to-search-for email-to-search-for)))
           ;; (b) try organization and name:
           (car (and bbdb-vcard-try-merge
                     (bbdb-vcard-search-intersection
                      (bbdb-records) name-to-search-for org-to-search-for)))
           ;; (c) try net and name; we may change organization here:
           (car (and bbdb-vcard-try-merge
                     (bbdb-vcard-search-intersection
                      (bbdb-records)
                      name-to-search-for nil email-to-search-for)))
           ;; (d) try birthday and name; we may change organization here:
           (car (and bbdb-vcard-try-merge
                     (bbdb-vcard-search-intersection
                      (bbdb-records)
                      name-to-search-for nil nil bday-to-search-for)))
           ;; (e) try phone and name; we may change organization here:
           (car (and bbdb-vcard-try-merge
                     (bbdb-vcard-search-intersection
                      (bbdb-records)
                      name-to-search-for nil nil nil tel-to-search-for)))
           ;; (f) if only match name; import with a new name, which format
           ;;     is: <name>-imported-<time-string>
           ;;     use can merge it by hand.
           (and bbdb-vcard-try-merge
                (let ((name-exist-p
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        name-to-search-for))
                      (email-exist-p
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        nil nil email-to-search-for))
                      (tels-exist-p
                       (bbdb-vcard-search-intersection
                        (bbdb-records)
                        nil nil nil nil tel-to-search-for)))
                  (when (or name-exist-p email-exist-p tels-exist-p)
                    ;; When encounter contacts conflict which must resolved by hand.
                    ;; we rename origin contact name to a unique name.
                    (setq name-need-manual-edit-p t)
                    (setq vcard-nicknames-backup t))
                  (when email-exist-p
                    ;; When current imported email is existed BBDB database,
                    ;; We will backup it in new bbdb field: backup-mail,
                    ;; Which can tell user to merge by hand in BBDB buffer.
                    (setq vcard-email-backup t))
                  (when tels-exist-p
                    ;; When current imported phone is existed BBDB database,
                    ;; We will backup it in new bbdb field: backup-phone,
                    ;; Which can tell user to merge by hand in BBDB buffer.
                    (setq vcard-tels-backup t))
                  nil))
           ;; No existing record found; make a fresh one:
           (let ((record (make-vector bbdb-record-length nil)))
             (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
             (run-hook-with-args 'bbdb-create-hook record)
             (bbdb-change-record record t t)
             record))))
    (when name-need-manual-edit-p
      (message (format "Need edit by hand: %S in your *contacts source* and %S in your *BBDB buffer*."
                       name
                       name-used-mark-conflict))
      (setq name name-used-mark-conflict))
    (when name
      (if (stringp name)
          (bbdb-record-set-field record 'name name)
        (progn
          (bbdb-record-set-field record 'firstname (car name))
          (bbdb-record-set-field record 'lastname (cdr name)))))
    (when vcard-nicknames
      (if vcard-nicknames-backup
          (bbdb-record-set-field record 'backup-aka
                                 (mapconcat #'identity vcard-nicknames ", ")
                                 t)
        (let* ((fn (bbdb-vcard-bbdb-record-field record 'firstname))
               (ln (bbdb-vcard-bbdb-record-field record 'lastname))
               (aka
                (nreverse
                 (cl-set-difference
                  (cl-reduce (lambda (x y) (cl-union x y :test 'string=))
                             (list vcard-nicknames))
                  (list (concat fn " " ln) fn ln)
                  :test 'string=))))
          (bbdb-record-set-field record 'aka aka t))))
    (when vcard-affixes
      (bbdb-record-set-field
       record 'affix vcard-affixes t))
    (when vcard-org
      (bbdb-record-set-field
       record 'organization vcard-org t))
    (when vcard-email
      (if vcard-email-backup
          (bbdb-record-set-field record 'backup-mail
                                 (mapconcat #'identity vcard-email ", ")
                                 t)
        (bbdb-record-set-field
         record 'mail vcard-email t)))
    (when vcard-adrs
      (bbdb-record-set-field
       record 'address vcard-adrs t))
    (when vcard-tels
      (if vcard-tels-backup
          (bbdb-record-set-field record 'backup-phone
                                 (mapconcat
                                  #'(lambda (x)
                                      (elt x 1))
                                  vcard-tels ", ")
                                 t)
        (bbdb-record-set-field
         record 'phone vcard-tels t)))
    (when vcard-url
      (bbdb-record-set-field
       record 'url vcard-url t))
    (when vcard-notes
      (bbdb-record-set-field
       record 'notes vcard-notes t))
    (when vcard-bday
      (bbdb-record-set-field
       record 'birthday vcard-bday t))
    (when vcard-bday
      (bbdb-record-set-field
       record 'birthday vcard-bday t))
    (when vcard-x-bbdb-weddings
      (bbdb-record-set-field
       record 'wedding vcard-x-bbdb-weddings t))
    (when vcard-x-bbdb-anniversaries
      (bbdb-record-set-field
       record 'anniversary vcard-x-bbdb-anniversaries t))
    (when vcard-photo
      (bbdb-vcard-import-media record 'image 'image-uri vcard-photo))
    (when vcard-key
      (bbdb-vcard-import-media record 'gpg-key 'gpg-key-uri vcard-key))
    (when vcard-sound
      (bbdb-vcard-import-media record 'sound 'sound-uri vcard-sound))
    (when vcard-categories
      (bbdb-record-set-field
       record 'mail-alias vcard-categories t))
    (when vcard-uid
      (bbdb-record-set-field
       record 'vcard-uid vcard-uid t))
    (when vcard-x-abuid
      (bbdb-record-set-field
       record 'vcard-x-abuid vcard-x-abuid t))
    (while nil
      (when (string-match "^\\([[:alnum:]-]*\\.\\)?AGENT"
                          (symbol-name (car other-vcard-type)))
        ;; Notice other vCards inside the current one.
        (bbdb-vcard-iterate-vcards
         'bbdb-vcard-import-vcard    ; needed for inner v2.1 vCards:
         (replace-regexp-in-string "\\\\" "" (cdr other-vcard-type)))))
    (bbdb-record-set-field
     record 'xfields vcard-xfields t)
    (bbdb-change-record record t t)
    record))

(defun bbdb-vcard-import-media (record field-fn field-uri vcard-media)
  (if (and (equal "b" (cadr (assoc "encoding" vcard-media)))
           (cadr (assoc "content" vcard-media)))
      (let ((filename (bbdb-vcard-import-inline-media vcard-media)))
        (when filename
            (bbdb-record-set-field record field-fn filename)))
    (bbdb-record-set-field
     record field-uri
     (bbdb-vcard-unescape-strings (cadr (assoc "content" vcard-media))))))

(defun bbdb-vcard-flatten (objects)
  "Flattens nested lists. For example, when applied to
list ((A B C) D (E F)), the result would be (A B C D E F)"
  (apply 'append (mapcar (lambda (obj)
                           (if (not (listp obj))
                               (list obj)
                             obj))
                         objects)))

(defun bbdb-vcard-bbdb-record-field (record field)
  "For bbdb RECORD return the value of FIELD, if `field' name match
`bbdb-vcard-skip-on-export' or its elements (when it is a regexp list),
return `nil'."
  (let* ((field-name (symbol-name field))
         (regexps bbdb-vcard-skip-on-export)
         (skip-export-p
          (if (stringp regexps)
              (string-match-p regexps field-name)
            (cl-some
             #'(lambda (regexp)
                 (when regexp
                   (string-match-p regexp field-name)))
             regexps))))
    (unless skip-export-p
      (bbdb-record-field record field))))

(defun bbdb-vcard-from (record)
  "Return BBDB RECORD as a vCard."
  (with-temp-buffer
    (let* ((name (bbdb-vcard-bbdb-record-field record 'name))
           (first-name (bbdb-vcard-bbdb-record-field record 'firstname))
           (last-name (bbdb-vcard-bbdb-record-field record 'lastname))
           (aka (bbdb-vcard-bbdb-record-field record 'aka))
           (organization (bbdb-vcard-bbdb-record-field record 'organization))
           (net (bbdb-vcard-bbdb-record-field record 'mail))
           (phones (bbdb-vcard-bbdb-record-field record 'phone))
           (addresses (bbdb-vcard-bbdb-record-field record 'address))
           (url (bbdb-vcard-bbdb-record-field record 'url))
           (notes (bbdb-vcard-bbdb-record-field record 'notes))
           (raw-anniversaries
            (let ((anniversary (bbdb-vcard-bbdb-record-field record 'anniversary)))
              (when anniversary
                (bbdb-vcard-split-structured-text anniversary "\n" t))))
           (birthday-regexp
            "\\([0-9]\\{4\\}-[01][0-9]-[0-3][0-9][t:0-9]*[-+z:0-9]*\\)\\([[:blank:]]+birthday\\)?\\'")
           (birthday
            (when raw-anniversaries
              (car (bbdb-vcard-split-structured-text
                    (cl-find-if (lambda (x) (string-match birthday-regexp x))
                                raw-anniversaries)
                    " " t))))
           (other-anniversaries
            (when raw-anniversaries
              (cl-remove-if (lambda (x) (string-match birthday-regexp x))
                            raw-anniversaries :count 1)))
           (timestamp (bbdb-vcard-bbdb-record-field record 'timestamp))
           (mail-aliases (bbdb-vcard-bbdb-record-field record 'mail-alias))
           (raw-notes (copy-alist (bbdb-record-xfields record))))
      (bbdb-vcard-insert-vcard-element "BEGIN" "VCARD")
      (bbdb-vcard-insert-vcard-element "VERSION" "3.0")
      (when name
        (bbdb-vcard-insert-vcard-element "FN" (bbdb-vcard-escape-strings name)))
      (when (or last-name first-name)
        (bbdb-vcard-insert-vcard-element
         "N" (bbdb-vcard-escape-strings last-name)
         ";" (bbdb-vcard-escape-strings first-name)
         ";;;")) ; Additional Names, Honorific Prefixes, Honorific Suffixes
      (when aka
        (bbdb-vcard-insert-vcard-element
         "NICKNAME" (bbdb-join (bbdb-vcard-escape-strings aka) ",")))
      (dolist (org organization)
        (bbdb-vcard-insert-vcard-element
         "ORG" (bbdb-vcard-escape-strings org)))
      (dolist (mail net)
        (bbdb-vcard-insert-vcard-element
         "EMAIL;TYPE=INTERNET" (bbdb-vcard-escape-strings mail)))
      (dolist (phone phones)
        (bbdb-vcard-insert-vcard-element
         (concat
          "TEL;TYPE="
          (bbdb-vcard-escape-strings
           (bbdb-vcard-translate (bbdb-phone-label phone) t)))
         (bbdb-vcard-escape-strings (bbdb-phone-string phone))))
      (dolist (address addresses)
        (bbdb-vcard-insert-vcard-element
         (concat
          "ADR;TYPE="
          (bbdb-vcard-escape-strings
           (bbdb-vcard-translate (bbdb-address-label address) t)))
         ";;"                           ; no Postbox, no Extended
         (bbdb-join (bbdb-vcard-escape-strings (bbdb-address-streets address))
                    ",")
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-city address)))
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-state address)))
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-postcode address)))
         ";" (bbdb-vcard-vcardize-address-element
              (bbdb-vcard-escape-strings (bbdb-address-country address)))))
      (when url
        (bbdb-vcard-insert-vcard-element "URL" url))
      (when notes
        (bbdb-vcard-insert-vcard-element "NOTE" (bbdb-vcard-escape-strings notes)))
      (when birthday
        (bbdb-vcard-insert-vcard-element "BDAY" birthday))
      (when other-anniversaries
        (bbdb-vcard-insert-vcard-element  ; non-birthday anniversaries
         "X-BBDB-ANNIVERSARY" (bbdb-join other-anniversaries "\\n")))
      (when timestamp
        (bbdb-vcard-insert-vcard-element "REV" timestamp))
      (when mail-aliases
        (bbdb-vcard-insert-vcard-element
         "CATEGORIES"
         (bbdb-join (bbdb-vcard-escape-strings
                     (bbdb-vcard-split-structured-text mail-aliases "," t)) ",")))
      ;; If fields have been export, prune from raw-notes ...
      (dolist (key `(url notes anniversary mail-alias creation-date timestamp))
        (setq raw-notes (assq-delete-all key raw-notes)))
      ;; ... and output what's left
      (dolist (raw-note raw-notes)
        (when raw-note
          (bbdb-vcard-insert-vcard-element
           (symbol-name (bbdb-vcard-prepend-x-bbdb-maybe (car raw-note)))
           (bbdb-vcard-escape-strings (cdr raw-note)))))
      (bbdb-vcard-insert-vcard-element "END" "VCARD")
      (bbdb-vcard-insert-vcard-element nil)) ; newline
    (buffer-string)))

(defun bbdb-vcard-convert-to-3.0 (vcard)
  "Convert VCARD from v2.1 to v3.0.
Return a version 3.0 vCard as a string.  Don't bother about the vCard
v3.0 mandatory elements N and FN."
  ;; Prevent customization of vcard.el's from being changed behind our back:
  (let ((vcard-standard-filters '(vcard-filter-html)))
    (with-temp-buffer
      (bbdb-vcard-insert-vcard-element "BEGIN" "VCARD")
      (bbdb-vcard-insert-vcard-element "VERSION" "3.0")
      (dolist (element (cl-remove
                        "VERSION" (vcard-parse-string vcard)
                        :key (lambda (x) (upcase (caar x))) :test 'string=))
        (bbdb-vcard-insert-vcard-element
         (concat (caar element)
                 (mapconcat 'bbdb-vcard-parameter-pair (cdar element) ""))
         (bbdb-join (bbdb-vcard-escape-strings (cdr element)) ";")))
      (bbdb-vcard-insert-vcard-element "END" "VCARD")
      (bbdb-vcard-insert-vcard-element nil)
      (buffer-string))))

(defun bbdb-vcard-parameter-pair (input)
  "Return \"parameter=value\" made from INPUT.
INPUT is its representation in vcard.el.  Return empty string if INPUT
is nil."
  (cond ((consp input) (concat ";" (car input) "=" (cdr input)))
        ((stringp input) (concat ";TYPE=" input))
        ((null input) "")))

(defun bbdb-vcard-values-of-type
  (type parameter &optional one-is-enough-p split-value-at-semi-colon-p)
  "Return in a list the values of PARAMETER of vCard element of TYPE.
The VCard element is read and deleted from current buffer which is
supposed to contain a single vCard.  If ONE-IS-ENOUGH-P is non-nil,
read and delete only the first element of TYPE.  If PARAMETER is
\"value\" and SPLIT-VALUE-AT-SEMI-COLON-P is non-nil, split the value
at semi-colons into a list."
  (mapcar (lambda (x) (cdr (assoc parameter x)))
          (bbdb-vcard-elements-of-type
           type one-is-enough-p split-value-at-semi-colon-p)))

(defun bbdb-vcard-elements-of-type
  (type &optional one-is-enough-p split-value-at-semi-colon-p)
  "From current buffer read and delete the vCard elements of TYPE.
The current buffer is supposed to contain a single vCard.  If
ONE-IS-ENOUGH-P is non-nil, read and delete only the first element of
TYPE.  Return a list of alists, one per element.  Each alist has a
cell with key \"content\" containing the element's value, and may have
other elements of the form \(parameter-name . parameter-value).  If
SPLIT-VALUE-AT-SEMI-COLON-P is non-nil, split the value at key
\"value\" at semi-colons into a list."
  (goto-char (point-min))
  (let (values parameters read-enough raw-params index)
    (while
        (and
         (not read-enough)
         (re-search-forward
          (concat
           "^\\([[:alnum:]-]*\\.\\)?\\(" type "\\)\\(;[^:]*\\)?:\\(.*\\)$")
          nil t))
      (goto-char (match-end 2))
      (setq parameters nil)
      (setf raw-params (match-string 3))
      (push (cons "content" (if split-value-at-semi-colon-p
                              (bbdb-vcard-split-structured-text
                               (match-string 4) ";")
                            (match-string 4)))
            parameters)
      (setf index 0)
      (when raw-params
        (while (string-match "\\([^;:=]+\\)=\\([^;:]+\\)" raw-params index)
          (let* ((parameter-key (downcase (match-string 1 raw-params)))
                 (parameter-value (bbdb-vcard-split-structured-text
                                   (downcase (match-string 2 raw-params))
                                   ","))
                 (parameter-sibling (assoc parameter-key parameters)))
            (when parameter-sibling         ; i.e., pair with equal key
                ;; collect vCard parameter list `;a=x;a=y;a=z'
                ;; into vCard value list `;a=x,y,z'; becoming ("a" . "x,y,z")
              (when (stringp (cdr parameter-sibling))
                (setf (cdr parameter-sibling)
                      (list (cdr parameter-sibling))))
              (setf (cdr parameter-sibling)
                    (cons parameter-value (cdr parameter-sibling))))
              ;; vCard parameter pair `;key=value;' with new key
            (unless parameter-sibling
              (push (cons parameter-key parameter-value) parameters))
            (setf index (match-end 0)))))
      (push parameters values)
      (delete-region (line-end-position 0) (line-end-position))
      (when one-is-enough-p
        (setq read-enough t)))
    (nreverse values)))

(defun bbdb-vcard-insert-vcard-element (type &rest values)
  "Insert a vCard element comprising TYPE, `:', VALUES into current buffer.
Take care of TYPE canonicalization, line folding, and closing newline.
Do nothing if TYPE is non-nil and VALUES are empty.  Insert just a
newline if TYPE is nil."
  (if type
      (let ((value (bbdb-join values "")))
        (unless (zerop (length value))
          (insert (bbdb-vcard-fold-line
                   (concat (bbdb-vcard-canonicalize-vcard-type type)
                           ":" value)))))
    (insert (bbdb-vcard-fold-line ""))))

(defun bbdb-vcard-unfold-lines (vcards)
  "Return folded vCard lines from VCARDS unfolded."
  (replace-regexp-in-string  "\n\\( \\|\t\\)" "" vcards))

(defun bbdb-vcard-fold-line (long-line)
  "Insert after every 75th position in LONG-LINE a newline and a space."
  (with-temp-buffer (insert long-line)
                    (goto-char (point-min))
                    (while (< (goto-char (+ (point) 75))
                              (point-max))
                      (insert "\n "))
                    (insert "\n")
                    (buffer-string)))

(defun bbdb-escape (x)
  (replace-regexp-in-string ; from 2.1 conversion:
   "\r" "" (replace-regexp-in-string
            "\n" "\\\\n" (replace-regexp-in-string
                          "\\(\\)[,;\\]" "\\\\" (or x "")
                          nil nil 1))))

(defun bbdb-unescape (x)
  (replace-regexp-in-string
   "\\([\\\\]\\)\\([,;\\]\\)" ""
   (replace-regexp-in-string "\\\\n" "\n" x)
   nil nil 1))

(defun bbdb-vcard-unescape-strings (escaped-strings)
  "Unescape escaped `;', `,', `\\', and newlines in ESCAPED-STRINGS.
ESCAPED-STRINGS may be a string or a sequence of strings."
    (bbdb-vcard-process-strings 'bbdb-unescape escaped-strings))

(defun bbdb-vcard-escape-strings (unescaped-strings )
  "Escape `;', `,', `\\', and newlines in UNESCAPED-STRINGS.
UNESCAPED-STRINGS may be a string or a sequence of strings."
    (bbdb-vcard-process-strings 'bbdb-escape unescaped-strings))

(defun bbdb-vcard-process-strings (string-processor strings)
  "Apply STRING-PROCESSOR to STRINGS.
STRINGS may be a string or a sequence of strings."
  (if (stringp strings)
      (funcall string-processor strings)
    (mapcar string-processor strings)))

(defun bbdb-vcard-remove-x-bbdb (vcard-element)
  "Remove the `X-BBDB-' prefix from the type part of VCARD-ELEMENT if any."
  (cons (intern (replace-regexp-in-string
                 "^X-BBDB-" "" (symbol-name (car vcard-element))))
        (cdr vcard-element)))

(defun bbdb-vcard-prepend-x-bbdb-maybe (bbdb-fieldname)
  "If BBDB-FIELDNAME is in `bbdb-vcard-x-bbdb-candidates', prepend `X-BBDB'."
  (if (member bbdb-fieldname bbdb-vcard-x-bbdb-candidates)
      (intern (concat "x-bbdb-" (symbol-name bbdb-fieldname)))
    bbdb-fieldname))                  ; lowercase more consistent here

(defun bbdb-vcard-unvcardize-name (vcard-name)
  "Convert VCARD-NAME into (type N) into a list (FIRST LAST AFFIXES).
LAST and FIRST are strings or nil, and AFFIXES is either nil
or a list of strings."
  (cond
   ((stringp vcard-name)
    (let ((name (bbdb-divide-name vcard-name)))
      (list (and (not (zerop (length (car name)))) (car name))
            (and (not (zerop (length (cdr name)))) (cdr name))
            nil)))
   ((and vcard-name (listp vcard-name))
    (let* ((first (if (stringp (nth 1 vcard-name))
                      (bbdb-concat " " (nth 1 vcard-name) (nth 2 vcard-name))
                    nil))
           (last (if (stringp (nth 0 vcard-name))
                     (bbdb-concat " " (nth 0 vcard-name))
                   nil))
           (prefixes-raw (nth 3 vcard-name))
           (prefixes (if (stringp prefixes-raw)
                         (list prefixes-raw)
                       prefixes-raw))
           (suffixes-raw (nth 4 vcard-name))
           (suffixes (if (stringp suffixes-raw)
                         (list suffixes-raw)
                       suffixes-raw)))
      (list first last (append prefixes suffixes))))))

(defun bbdb-vcard-unvcardize-org (vcard-org)
  "Convert VCARD-ORG (type ORG), which may be a list, into a string."
  (if (or (null vcard-org)
          (stringp vcard-org)) ; unstructured, probably non-standard ORG
      vcard-org                ; Organization, unit 1, unit 2...
    (bbdb-join vcard-org "\n")))

(defun bbdb-vcard-unvcardize-adr (vcard-adr)
  "Convert VCARD-ADR into BBDB format.
Turn a vCard element of type ADR into (TYPE STREETS CITY STATE POSTCODE
COUNTRY)."
  (let ((adr-type (cadr (assoc "type" vcard-adr)))
        ; Postbox, Extended, Streets go into one list
        (streets
         (bbdb-vcard-flatten
          (cl-subseq (cadr (assoc "content" vcard-adr)) 0 3)))
        (non-streets          ; turn comma-separated substructure into
         (mapcar              ; newline-separated text
          (lambda (x) (bbdb-concat "\n" x))
          (cl-subseq (cadr (assoc "content" vcard-adr))
                  3 nil))))
    (vector (bbdb-vcard-translate adr-type)
            streets
            (or (elt non-streets 0) "")    ; City
            (or (elt non-streets 1) "")    ; State
            (or (elt non-streets 2) "")    ; Postcode
            (or (elt non-streets 3) "")))) ; Country

(defun bbdb-vcard-unvcardize-date-time (date-time)
  "If necessary, make DATE-TIME usable for storage in BBDB.
Convert yyyymmdd, yyyymmddThhmmss, or yyymmddThhmmssZhhmm into
yyyy-mm-dd, yyyy-mm-ddThh:mm:ss, or yyy-mm-ddThh:mm:ssZhh:mm
respectively.  Discard fractions of a second.  Return anything else
unchanged."
  (if (and (stringp date-time)
           (string-match
            "\\([0-9]\\{4\\}\\)-?\\([0-2][0-9]\\)-?\\([0-3][0-9]\\)\\(?:t\\([0-5][0-9]\\):?\\([0-5][0-9]\\):?\\([0-5][0-9]\\)\\(?:[,.0-9]*\\(\\([+-][0-5][0-9]\\):?\\([0-5][0-9]\\)?\\|z\\)\\)?\\)?"
            date-time))
      (concat
       (match-string 1 date-time) "-"
       (match-string 2 date-time) "-" (match-string 3 date-time)
       (when (match-string 6 date-time) ; seconds part of time
         (concat
          "T" (match-string 4 date-time) ":"
          (match-string 5 date-time) ":" (match-string 6 date-time)
          (when (match-string 7 date-time) ; time zone
            (if (match-string 9 date-time) ; time zone minute
                  (concat (match-string 8 date-time) ; time zone hour
                          ":" (match-string 9 date-time)) ; time zone minute
              "Z")))))
    date-time))

(defun bbdb-vcard-vcardize-address-element (address-element)
  "Replace escaped newlines in ADDRESS-ELEMENT by commas."
  (replace-regexp-in-string "\\\\n" "," address-element))

(defun bbdb-vcard-translate (label &optional exportp)
  "Translate LABEL from vCard to BBDB or, if EXPORTP is non-nil, vice versa.
Translations are defined in `bbdb-vcard-import-translation-table' and
`bbdb-vcard-export-translation-table' respectively."
  (if exportp
      (or (assoc-default label
                         bbdb-vcard-export-translation-table
                         'string-match)
          label)
    (if label
        (cl-block escape
          (let* ((tokens (if (stringp label)
                             (list label)
                           label))
                 (default (car tokens)))
            (while tokens
              (let* ((token (pop tokens))
                 (result
                  (assoc-default token
                                 bbdb-vcard-import-translation-table
                                 'string-match)))
                (when result
                  (cl-return-from escape result))))
            (downcase default)))
      "work")))


(defun bbdb-vcard-merge-strings (old-string new-strings separator)
  "Merge strings successively from list NEW-STRINGS into OLD-STRING.
If an element of NEW-STRINGS is already in OLD-STRING, leave
OLD-STRING unchanged.  Otherwise append SEPARATOR and NEW-STRING."
  (with-temp-buffer
    (insert old-string)
    (dolist (new-string new-strings)
      (unless (prog1 (search-backward new-string nil t)
                (goto-char (point-max)))
        (unless (zerop (buffer-size)) (insert separator))
        (insert new-string)))
    (buffer-string)))

(defun bbdb-vcard-split-structured-text
  (text separator &optional return-always-list-p)
  "Split TEXT at unescaped occurrences of SEPARATOR; return parts in a list.
Return text unchanged if there aren't any separators and RETURN-ALWAYS-LIST-P
is nil."
  (when (stringp text)
    (let ((string-elements
           (mapcar (lambda (substr)
                     (and (not (zerop (length substr))) substr))
                   (split-string
                    (replace-regexp-in-string
                     (concat "\\\\\r" separator) (concat "\\\\" separator)
                     (replace-regexp-in-string separator
                                               (concat "\r" separator) text))
                    (concat "\r" separator)))))
      (if (and (null return-always-list-p)
               (= 1 (length string-elements)))
          (car string-elements)
        string-elements))))

(defun bbdb-vcard-canonicalize-vcard-type (&rest strings)
  "Concatenate STRINGS and apply `bbdb-vcard-type-canonicalizer' to them."
  (funcall bbdb-vcard-type-canonicalizer (bbdb-join strings "")))

(defun bbdb-vcard-write-buffer (vcard-file-name &optional allow-overwrite)
  "Write current buffer to VCARD-FILE-NAME.
Create directories where necessary."
  (make-directory (file-name-directory vcard-file-name) t)
  (let ((buffer-file-coding-system bbdb-vcard-export-coding-system))
    (write-region nil nil vcard-file-name nil nil nil (not allow-overwrite))))

(defun bbdb-vcard-make-file-name (bbdb-record &optional used-up-basenames)
  "Come up with a vCard filename given a BBDB-RECORD.
Make it unique against the list USED-UP-BASENAMES."
  (let ((name (bbdb-record-name bbdb-record))
        (aka (car (bbdb-record-aka bbdb-record)))
        (unique-number 0)
        filename)
    (while (member
            (setq filename
                  (concat
                   (replace-regexp-in-string
                    "[[:blank:]]+" "_"
                    (or (unless (zerop (length name)) name)
                        (unless (zerop (length aka)) aka)
                        "bbdb-record"))
                   (unless (zerop unique-number)
                     (concat "-" (number-to-string unique-number)))
                   ".vcf"))
            used-up-basenames)
      (cl-incf unique-number))
    filename))

(defun bbdb-join (list separator)
  "Join a LIST to a string where the list elements are separated by SEPARATOR.
The inverse function of `bbdb-split'."
  (when list
    (mapconcat 'identity list separator)))

(defun bbdb-vcard-compute-media-id (data)
  "Compute a representative id for a data blob. Basically a sha1sum."
  (sha1 data))

(defun bbdb-vcard-build-filename (descriptor data)
  (expand-file-name (concat (nth 0 descriptor)
                            "-" (bbdb-vcard-compute-media-id data)
                            "." (nth 1 descriptor))
                    (concat bbdb-vcard-directory
                            bbdb-vcard-media-directory)))

(defun bbdb-vcard-string-chomp (string)
  (if (string-match "[ \t\n]*$" string)
    (replace-match "" nil nil string)
    string))

(defun bbdb-vcard-sniff-mime-type (data)
  (if (not (member system-type '("windows-nt" "ms-dos")))
      (with-temp-buffer
        (insert data)
        (if (zerop (call-process-region 1 (+ 1 (buffer-size))
                                        "file"
                                        t t nil
                                        "-b" "--mime-type" "-"))
            (bbdb-vcard-string-chomp (buffer-string))
          nil))
    nil))

(defun bbdb-vcard-import-inline-media (vcard-media)
  "imports inline binary content and saves it to disk."
  (let* ((type (cadr (assoc "type" vcard-media)))
         (encoding (cadr (assoc "encoding" vcard-media)))
         (value (cadr (assoc "content" vcard-media)))
         (data (condition-case nil
                   (base64-decode-string value)
                 (error nil)))
         (mime-type (and data (bbdb-vcard-sniff-mime-type data)))
         (descriptor (cadr (assoc type bbdb-vcard-media-types))))
    (when (and (not descriptor) mime-type)
        (setq descriptor
              (cadr (assoc (cdr (assoc mime-type bbdb-vcard-mime-types))
                           bbdb-vcard-media-types))))
    ;; sanity checks
    (if (and (equal encoding "b") data descriptor)
        (let ((filename (bbdb-vcard-build-filename descriptor data))
              (coding-system-for-write 'no-conversion))
          (condition-case nil
              (progn
                (unless (file-exists-p (file-name-directory filename))
                  (make-directory (file-name-directory filename) t))
                (write-region data nil filename)
                (concat bbdb-vcard-media-directory (file-name-nondirectory filename)))
            (error nil)))
      nil)))

;;;; do stuff after BBDB is loaded
(eval-after-load 'bbdb '(bbdb-vcard-initialize))

(provide 'bbdb-vcard)

;;; bbdb-vcard.el ends here

; LocalWords:  vcard firstname
