;;; bbdb-vcard-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "bbdb-vcard" "bbdb-vcard.el" (21927 15104 784767
;;;;;;  236000))
;;; Generated autoloads from bbdb-vcard.el

(autoload 'bbdb-vcard-import-region "bbdb-vcard" "\
Import the vCards between BEGIN and END into BBDB.
Existing BBDB records may be altered.

\(fn BEGIN END &optional SOURCE-NAME)" t nil)

(autoload 'bbdb-vcard-import-buffer "bbdb-vcard" "\
Import vCards from VCARD-BUFFER into BBDB.
Existing BBDB records may be altered.

\(fn VCARD-BUFFER)" t nil)

(autoload 'bbdb-vcard-import-file "bbdb-vcard" "\
Import vCards from VCARD-FILE into BBDB.
If VCARD-FILE is a wildcard, import each matching file.  Existing BBDB
records may be altered.

\(fn VCARD-FILE)" t nil)

(autoload 'bbdb-vcard-export "bbdb-vcard" "\
From Buffer *BBDB*, write one or more record(s) as vCard(s) to file(s).
\\<bbdb-mode-map>If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-vcard-export]\"is used instead of simply \"\\[bbdb-vcard-export]\", then export all records currently
in the *BBDB* buffer.  If used with prefix argument, store records
in individual files.

\(fn FILENAME-OR-DIRECTORY ALL-RECORDS-P ONE-FILE-PER-RECORD-P &optional ALLOW-OVERWRITE)" t nil)

(autoload 'bbdb-vcard-export-to-kill-ring "bbdb-vcard" "\
From Buffer *BBDB*, copy one or more record(s) as vCard(s) to the kill ring.
\\<bbdb-mode-map>If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-vcard-export-to-kill-ring]\"is used instead of simply \"\\[bbdb-vcard-export-to-kill-ring]\", then export all records currently in
the *BBDB* buffer.

\(fn ALL-RECORDS-P)" t nil)

;;;***

;;;### (autoloads nil "vcard" "vcard.el" (21927 15104 708768 8000))
;;; Generated autoloads from vcard.el

(defvar vcard-pretty-print-function 'vcard-format-sample-box "\
*Formatting function used by `vcard-pretty-print'.")

(custom-autoload 'vcard-pretty-print-function "vcard" t)

(defvar vcard-standard-filters '(vcard-filter-html vcard-filter-adr-newlines vcard-filter-tel-normalize vcard-filter-textprop-cr) "\
*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'.")

(custom-autoload 'vcard-standard-filters "vcard" t)

(autoload 'vcard-pretty-print "vcard" "\
Format VCARD into a string suitable for display to user.
VCARD can be an unparsed string containing raw VCF vcard data
or a parsed vcard alist as returned by `vcard-parse-string'.

The result is a string with formatted vcard information suitable for
insertion into a mime presentation buffer.

The function specified by the variable `vcard-pretty-print-function'
actually performs the formatting.  That function will always receive a
parsed vcard alist.

\(fn VCARD)" nil nil)

(autoload 'vcard-parse-string "vcard" "\
Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to each
attribute.  If no filter is specified, `vcard-standard-filter' is used.

Filters should accept two arguments: the property list and the value list.
Modifying in place the property or value list will affect the resulting
attribute in the vcard alist.

Vcard data is normally in the form

    begin:                        vcard
    prop1a:                       value1a
    prop2a;prop2b;prop2c=param2c: value2a
    prop3a;prop3b:                value3a;value3b;value3c
    end:                          vcard

\(Whitespace around the `:' separating properties and values is optional.)
If supplied to this function an alist of the form

    (((\"prop1a\") \"value1a\")
     ((\"prop2a\" \"prop2b\" (\"prop2c\" . \"param2c\")) \"value2a\")
     ((\"prop3a\" \"prop3b\") \"value3a\" \"value3b\" \"value3c\"))

would be returned.

\(fn RAW &optional FILTER)" nil nil)

(autoload 'vcard-parse-region "vcard" "\
Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!

\(fn BEG END &optional FILTER)" nil nil)

;;;***

;;;### (autoloads nil nil ("bbdb-vcard-pkg.el") (21927 15104 854612
;;;;;;  927000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bbdb-vcard-autoloads.el ends here
