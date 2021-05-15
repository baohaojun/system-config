;;; xml-rpc.el --- An elisp implementation of clientside XML-RPC  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2020 Mark A. Hershberger
;; Copyright (C) 2001 CodeFactory AB.
;; Copyright (C) 2001 Daniel Lundin.
;; Copyright (C) 2006 Shun-ichi Goto
;;   Modified for non-ASCII character handling.

;; Maintainer: Mark A. Hershberger <mah@everybody.org>
;; Original Author: Daniel Lundin <daniel@codefactory.se>
;; Version: 1.6.15
;; Package-Version: 20200907.42
;; Package-Commit: 8020ccd176986d8e49e0bb5dd9f4e756cf12eafc
;; Created: May 13 2001
;; Keywords: xml rpc network
;; URL: http://github.com/xml-rpc-el/xml-rpc-el
;; Last Modified: <2020-09-06 20:07:23 mah>

(defconst xml-rpc-version "1.6.15"
  "Current version of xml-rpc.el")

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'xml)
(require 'url-http)
(require 'timezone)

(defconst xml-rpc-maintainer-address "mah@everybody.org"
  "The address where bug reports should be sent.")

(defcustom xml-rpc-load-hook nil
  "*Hook run after loading xml-rpc."
  :type 'hook :group 'xml-rpc)

(defcustom xml-rpc-use-coding-system
  (if (coding-system-p 'utf-8) 'utf-8 'iso-8859-1)
  "The coding system to use."
  :type 'symbol :group 'xml-rpc)

(defcustom xml-rpc-allow-unicode-string (coding-system-p 'utf-8)
  "If non-nil, non-ASCII data is composed as 'value' instead of 'base64'.
And this option overrides `xml-rpc-base64-encode-unicode' and
`xml-rpc-base64-decode-unicode' if set as non-nil."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-base64-encode-unicode (coding-system-p 'utf-8)
  "If non-nil, then strings with non-ascii characters will be turned
into Base64."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-base64-decode-unicode (coding-system-p 'utf-8)
  "If non-nil, then base64 strings will be decoded using the
utf-8 coding system."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-debug 0
  "Set this to 1 or greater to avoid killing temporary buffers.
Set it higher to get some info in the *Messages* buffer"
  :type 'integer :group 'xml-rpc)

(defvar xml-rpc-parse-region-function
  (if (and (fboundp 'libxml-available-p)
           (libxml-available-p))
      #'libxml-parse-xml-region
    #'xml-parse-region)
  "Function to use for parsing XML data.")

(defvar xml-rpc-fault-string nil
  "Contains the fault string if a fault is returned")

(defvar xml-rpc-fault-code nil
  "Contains the fault code if a fault is returned")

(defvar xml-rpc-request-extra-headers nil
  "A list of extra headers to send with the next request.
Should be an assoc list of headers/contents.  See
`url-request-extra-headers'")

;;
;; Value type handling functions
;;

(defsubst xml-rpc-value-intp (value)
  "Return t if VALUE is an integer."
  (integerp value))

(defsubst xml-rpc-value-doublep (value)
  "Return t if VALUE is a double precision number."
  (floatp value))

(defsubst xml-rpc-value-stringp (value)
  "Return t if VALUE is a string."
  (stringp value))

;; An XML-RPC struct is a list where every car is cons or a list of
;; length 1 or 2 and has a string for car.
(defsubst xml-rpc-value-structp (value)
  "Return t if VALUE is an XML-RPC struct."
  (and (listp value)
       (let ((vals value)
             (result t)
             curval)
         (while (and vals result)
           (setq result (and
                         (setq curval (car-safe vals))
                         (consp curval)
                         (stringp (car-safe curval))
                         (xml-rpc-valuep (cdr curval))))
           (setq vals (cdr-safe vals)))
         result)))

;; A somewhat lazy predicate for arrays
(defsubst xml-rpc-value-arrayp (value)
  "Return t if VALUE is an XML-RPC array - specified by keyword :array or
a list that is not datetime, base64 or struct."
  (and (listp value)
       (or
	(eq (car value) :array)
	(and
	 (not (xml-rpc-value-datetimep value))
	 (not (xml-rpc-value-base64p value))
	 (not (xml-rpc-value-structp value))))))

(defsubst xml-rpc-value-vectorp (value)
  "Return t if VALUE is a vector - used to pass in empty lists"
  (vectorp value))

(defvar xml-rpc--date-parses-as nil)
(defvar xml-rpc--tz-pd-defined-in nil)

(defun xml-rpc-submit-bug-report ()
 "Submit a bug report on xml-rpc."
 (interactive)
 (require 'reporter)
 (let ((xml-rpc--tz-pd-defined-in
        (if (fboundp 'find-lisp-object-file-name)
            (find-lisp-object-file-name
             'timezone-parse-date (symbol-function 'timezone-parse-date))
          (symbol-file 'timezone-parse-date)))
       (xml-rpc--date-parses-as (timezone-parse-date "20091130T00:52:53")))
   (reporter-submit-bug-report
    xml-rpc-maintainer-address
    (concat "xml-rpc.el " xml-rpc-version)
    (list 'xml-rpc--tz-pd-defined-in
          'xml-rpc--date-parses-as
          'xml-rpc-load-hook
          'xml-rpc-use-coding-system
          'xml-rpc-allow-unicode-string
          'xml-rpc-base64-encode-unicode
          'xml-rpc-base64-decode-unicode))))

(defun xml-rpc-value-booleanp (value)
  "Return t if VALUE is a boolean."
  (or (eq value nil)
      (eq value t)))

(defun xml-rpc-value-datetimep (value)
  "Return t if VALUE is a datetime.  For Emacs XML-RPC
implementation, you must put time keyword :datetime before the
time, or it will be confused for a list."
  (and (listp value)
       (eq (car value) :datetime)))

(defun xml-rpc-value-base64p (value)
  "Return t if VALUE is a base64 byte array.  For Emacs XML-RPC
implementation, you must put keyword :base64 before the
sequence, or it will be confused for a list."
  (and (listp value)
       (eq (car value) :base64)))

(defsubst xml-rpc-valuep (value)
  "Return t if VALUE is any sort of xml-rpc structure.

Return nil otherwise."
  (or (xml-rpc-value-intp value)
      (xml-rpc-value-doublep value)
      (xml-rpc-value-stringp value)
      (xml-rpc-value-structp value)
      (xml-rpc-value-arrayp value)
      (xml-rpc-value-vectorp value)
      (xml-rpc-value-booleanp value)
      (xml-rpc-value-datetimep value)
      (xml-rpc-value-base64p value)))

(defun xml-rpc-string-to-boolean (value)
  "Return t if VALUE is a boolean"
  (or (string-equal value "true") (string-equal value "1")))

(defun xml-rpc-caddar-safe (list)
  "Assume that LIST is '((value nil REST)) and return REST.  If
REST is nil, then return \"\""
  (let ((rest (car-safe (cdr-safe (cdr-safe (car-safe list))))))
    (if rest
	rest
      "")))

(defun xml-rpc-xml-list-to-value (xml-list)
  "Convert an XML-RPC structure in an xml.el style XML-LIST to an elisp list,
interpreting and simplifying it while retaining its structure."
  (let (valtype valvalue)
    (cond
     ((and (xml-rpc-caddar-safe xml-list)
           (listp (xml-rpc-caddar-safe xml-list)))
      (setq valtype (car (caddar xml-list))
            valvalue (caddr (caddar xml-list)))
      (cond
       ;; Base64
       ((eq valtype 'base64)
                                        ; for some reason, Emacs wraps this in
                                        ; a second encoding
        (list :base64 (base64-decode-string valvalue)))
       ;; Boolean
       ((eq valtype 'boolean)
        (xml-rpc-string-to-boolean valvalue))
       ;; String
       ((eq valtype 'string)
        valvalue)
       ;; Integer
       ((or (eq valtype 'int) (eq valtype 'i4) (eq valtype 'i8))
        (string-to-number (or valvalue "0")))
       ;; Double/float
       ((eq valtype 'double)
        (string-to-number (or valvalue "0.0")))
       ;; Struct
       ((eq valtype 'struct)
        (mapcar (lambda (member)
                  (let ((membername (cadr (cdaddr member)))
                        (membervalue (xml-rpc-xml-list-to-value
                                      (cdddr member))))
                    (cons membername membervalue)))
                (cddr (caddar xml-list))))
       ;; Fault
       ((eq valtype 'fault)
        (let* ((struct (xml-rpc-xml-list-to-value (list valvalue)))
               (fault-string (cdr (assoc "faultString" struct)))
               (fault-code (cdr (assoc "faultCode" struct))))
          (list 'fault fault-code fault-string)))
       ;; DateTime
       ((or (eq valtype 'dateTime.iso8601)
            (eq valtype 'dateTime))
        (list :datetime (date-to-time valvalue)))
       ;; Array
       ((eq valtype 'array)
        (mapcar (lambda (arrval)
                  (xml-rpc-xml-list-to-value (list arrval)))
                (cddr valvalue)))))
     (t
      (xml-rpc-caddar-safe xml-list)))))

(defun xml-rpc-boolean-to-string (value)
  "Convert a boolean value to a string"
  (if value
      "1"
    "0"))

(defun xml-rpc-datetime-to-string (value)
  "Convert a date time to a valid XML-RPC date"
  (format-time-string "%Y%m%dT%H:%M:%S" (cadr value)))

(defun xml-rpc-value-to-xml-list (value)
  "Return XML representation of VALUE properly formatted for use with the
functions in xml.el."
  (cond
   ;; boolean
   ((xml-rpc-value-booleanp value)
    `((value nil (boolean nil ,(xml-rpc-boolean-to-string value)))))
   ;; Date
   ((xml-rpc-value-datetimep value)
    `((value nil (dateTime.iso8601 nil ,(xml-rpc-datetime-to-string value)))))
   ;; base64 (explicit)
   ((xml-rpc-value-base64p value)
                                        ; strip keyword; for some reason,
                                        ; Emacs decodes this twice
    `((value nil (base64 nil ,(base64-encode-string (cadr value))))))
   ;; array as vector (for empty lists)
   ((xml-rpc-value-vectorp value)
    (let ((result nil)
          (xmlval nil))
      (dotimes (i (length value))
	(setq xmlval (xml-rpc-value-to-xml-list (elt value i))
	      result (if result (append result xmlval) xmlval)))
      `((value nil (array nil ,(append '(data nil) result))))))
   ;; array as list
   ((xml-rpc-value-arrayp value)
                                        ; strip keyword if any
    (setq value (if (eq (car value) :array) (cadr value) value))
    (let ((result nil)
          (xmlval nil))
      (while (setq xmlval (xml-rpc-value-to-xml-list (car value))
                   result (if result (append result xmlval)
                            xmlval)
                   value (cdr value)))
      `((value nil (array nil ,(append '(data nil) result))))))
   ;; struct
   ((xml-rpc-value-structp value)
    (let ((result nil)
          (xmlval nil))
      (while (setq xmlval `((member nil (name nil ,(caar value))
                                    ,(car (xml-rpc-value-to-xml-list
                                           (cdar value)))))
                   result (append result xmlval)
                   value (cdr value)))
      `((value nil ,(append '(struct nil) result)))))
   ;; Value is a scalar
   ((xml-rpc-value-intp value)
    `((value nil (int nil ,(int-to-string value)))))
   ((xml-rpc-value-stringp value)
    (let ((charset-list (find-charset-string value)))
      (if (or xml-rpc-allow-unicode-string
              (and (eq 1 (length charset-list))
                   (eq 'ascii (car charset-list)))
              (not xml-rpc-base64-encode-unicode))
          `((value nil (string nil ,value)))
        `((value nil (string nil ,(if xml-rpc-base64-encode-unicode
                                      (base64-encode-string
                                       (encode-coding-string
                                        value xml-rpc-use-coding-system))
                                    (base64-encode-string value))))))))
   ((xml-rpc-value-doublep value)
    `((value nil (double nil ,(number-to-string value)))))
   (t
    `((value nil (string nil ,(base64-encode-string value)))))))

(defun xml-rpc-xml-to-string (xml)
  "Return a string representation of the XML tree as valid XML markup."
  (let ((tree (xml-node-children xml))
        (result (concat "<" (symbol-name (xml-node-name xml)) ">")))
    (while tree
      (cond
       ((listp (car tree))
        (setq result (concat result (xml-rpc-xml-to-string (car tree)))))
       ((stringp (car tree))
        (setq result (concat result (car tree))))
       (t
        (error "Invalid XML tree")))
      (setq tree (cdr tree)))
    (setq result (concat result "</" (symbol-name (xml-node-name xml)) ">"))
    result))

;;
;; Response handling
;;

(defsubst xml-rpc-response-errorp (response)
  "An 'xml-rpc-method-call'  result value is always a list, where the first
element in RESPONSE is either nil or if an error occured, a cons pair
according to (errnum .  \"Error string\"),"
  (eq 'fault (car-safe (caddar response))))

(defsubst xml-rpc-response-error-code (response)
  "Return the error code from RESPONSE."
  (and (xml-rpc-response-errorp response)
       (nth 1 (xml-rpc-xml-list-to-value response))))

(defsubst xml-rpc-response-error-string (response)
  "Return the error code from RESPONSE."
  (and (xml-rpc-response-errorp response)
       (nth 2 (xml-rpc-xml-list-to-value response))))

(defun xml-rpc-xml-to-response (xml)
  "Convert an XML list to a method response list.  An error is
signaled if there is a fault or if the response does not appear
to be an XML-RPC response (i.e. no methodResponse).  Otherwise,
the parsed XML response is returned."
  ;; Check if we have a methodResponse
  (cond
   ((not (eq (car-safe (car-safe xml)) 'methodResponse))
    (error "No methodResponse found"))

   ;; Did we get a fault response
   ((xml-rpc-response-errorp xml)
    (let ((resp (xml-rpc-xml-list-to-value xml)))
      (setq xml-rpc-fault-string (nth 2 resp))
      (setq xml-rpc-fault-code   (nth 1 resp))
      (error "XML-RPC fault `%s'" xml-rpc-fault-string)))

   ;; Interpret the XML list and produce a more useful data structure
   (t
    (let ((valpart (cdr (cdaddr (caddar xml)))))
      (xml-rpc-xml-list-to-value valpart)))))

;;
;; Method handling
;;

(defvar url-current-callback-data)
(defvar url-current-callback-func)
(defvar url-http-response-status)
(defvar url-request-coding-system)

(defun xml-rpc-request (server-url xml &optional async-callback-function)
  "Perform http post request to SERVER-URL using XML.

If ASYNC-CALLBACK-FUNCTION is non-nil, the request will be performed
asynchronously and ASYNC-CALLBACK-FUNCTION should be a callback function to
be called when the reuest is finished.  ASYNC-CALLBACK-FUNCTION is called with
a single argument being an xml.el style XML list.

It returns an XML list containing the method response from the XML-RPC server,
or nil if called with ASYNC-CALLBACK-FUNCTION."
  (unwind-protect
      (save-excursion
        (let ((url-request-method "POST")
              (url-package-name "xml-rpc.el")
              (url-package-version xml-rpc-version)
              (url-request-data
	       (concat "<?xml version=\"1.0\""
                       " encoding=\"UTF-8\"?>\n"
                       (with-temp-buffer
                         (xml-print xml)
                         (if xml-rpc-allow-unicode-string
                             (string-as-unibyte
			      (encode-coding-string (buffer-string) 'utf-8))
                           (buffer-string)))
                       "\n"))
              (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
              (url-request-coding-system xml-rpc-use-coding-system)
              (url-http-attempt-keepalives t)
              (url-request-extra-headers (append
                                          (list
                                           (cons "Connection" "close")
                                           (cons "Content-Type"
                                                 "text/xml; charset=utf-8"))
                                          xml-rpc-request-extra-headers)))
          (when (> xml-rpc-debug 1)
            (print url-request-data (create-file-buffer "request-data")))

          (cond ((boundp 'url-be-asynchronous) ; Sniff for w3 lib capability
                 (if async-callback-function
                     (setq url-be-asynchronous t
                           url-current-callback-data (list
                                                      async-callback-function
                                                      (current-buffer))
                           url-current-callback-func
                           'xml-rpc-request-callback-handler)
                   (setq url-be-asynchronous nil))
                 (url-retrieve server-url t)

                 (when (not url-be-asynchronous)
                   (let ((result (xml-rpc-request-process-buffer
                                  (current-buffer))))
                     (when (> xml-rpc-debug 1)
                       (print result (create-file-buffer "result-data")))
                     result)))
                (t                      ; Post emacs20 w3-el
                 (if async-callback-function
                     (let ((cbargs (list async-callback-function)))
                       (url-retrieve server-url
                                     'xml-new-rpc-request-callback-handler
                                     cbargs))
                   (let ((buffer (url-retrieve-synchronously server-url)))
                     (with-current-buffer buffer
                       (when (not (numberp url-http-response-status))
                         ;; this error may occur when keep-alive bug
                         ;; of url-http.el is not cleared.
                         (error "Why? url-http-response-status is %s"
                                url-http-response-status))
                       (when (> url-http-response-status 299)
                         (error "Error during request: %s"
                                url-http-response-status)))
                     (xml-rpc-request-process-buffer buffer)))))))))


(defun xml-rpc-clean-string (s)
  (if (string-match "\\`[ \t\n\r]*\\'" s)
      ;;"^[ \t\n]*$" s)
      nil
    s))

(defun xml-rpc-clean (l)
  (cond
   ((listp l)
    (let (elem
          (result nil))
      (while l
        ;; iterate
        (setq elem (car l)
              l (cdr l))
        ;; test the head
        (cond
         ;; a string, so clean it.
         ((stringp elem)
          (let ((tmp (xml-rpc-clean-string elem)))
            (when (and tmp xml-rpc-allow-unicode-string)
              (setq tmp (decode-coding-string tmp xml-rpc-use-coding-system)))
            (if tmp
                (setq result (append result (list tmp)))
              result)))
         ;; a list, so recurse.
         ((listp elem)
          (setq result (append result (list (xml-rpc-clean elem)))))

         ;; everthing else, as is.
         (t
          (setq result (append result (list elem))))))
      result))

   ((stringp l)                   ; will returning nil be acceptable ?
    nil)

   (t l)))

(defun xml-rpc-request-process-buffer (xml-buffer)
  "Process buffer XML-BUFFER."
  (unwind-protect
      (with-current-buffer xml-buffer
        (when (fboundp 'url-uncompress)
          (let ((url-working-buffer xml-buffer))
            (url-uncompress)))
        (goto-char (point-min))
        (search-forward-regexp "<\\?xml" nil t)
        (move-to-column 0)
        ;; Gather the results
        (let* ((status (if (boundp 'url-http-response-status)
                           ;; Old URL lib doesn't save the result.
                           url-http-response-status 200))
               (result (cond
                        ;; A probable XML response
                        ((search-forward "<?xml " nil t)
                         (xml-rpc-clean
                          (funcall xml-rpc-parse-region-function
                                   (match-beginning 0)
                                   (point-max))))

                        ;; No HTTP status returned
                        ((not status)
                         (let ((errstart
                                (search-forward "\n---- Error was: ----\n")))
                           (and errstart
                                (buffer-substring errstart (point-max)))))

                        ;; Maybe they just gave us an the XML w/o PI?
                        ((search-forward "<methodResponse>" nil t)
                         (xml-rpc-clean
                          (funcall xml-rpc-parse-region-function
                                   (match-beginning 0)
                                   (point-max))))

                        ;; Valid HTTP status
                        (t
                         (int-to-string status)))))
          (when (< xml-rpc-debug 3)
            (kill-buffer (current-buffer)))
          ;; Normalize result: `libxml-parse-xml-region' gives response like
          ;; `(methodResponse ...)' but `xml-parse-region' gives
          ;; `((methodResponse ...))'.
          (if (eq xml-rpc-parse-region-function 'libxml-parse-xml-region)
              (list result)
            result)))))


(defun xml-rpc-request-callback-handler (callback-fun xml-buffer)
  "Marshall a callback function request to CALLBACK-FUN with the results
handled from XML-BUFFER."
  (let ((xml-response (xml-rpc-request-process-buffer xml-buffer)))
    (when (< xml-rpc-debug 1)
      (kill-buffer xml-buffer))
    (funcall callback-fun (xml-rpc-xml-to-response xml-response))))


(defun xml-new-rpc-request-callback-handler (_status callback-fun)
  "Handle a new style `url-retrieve' callback passing `STATUS'
and `CALLBACK-FUN'."
  (let ((xml-buffer (current-buffer)))
    (xml-rpc-request-callback-handler callback-fun xml-buffer)))


(defun xml-rpc-method-call-async (async-callback-func server-url method
                                                      &rest params)
  "Call an XML-RPC method asynchronously at SERVER-URL named METHOD with
PARAMS as parameters. When the method returns, ASYNC-CALLBACK-FUNC will be
called with the result as parameter."
  (let* ((m-name (if (stringp method)
                     method
                   (symbol-name method)))
         (m-params (mapcar #'(lambda (p)
                               `(param nil ,(car (xml-rpc-value-to-xml-list
                                                  p))))
                           (if async-callback-func
                               params
                             (car-safe params))))
         (m-func-call `((methodCall nil (methodName nil ,m-name)
                                    ,(append '(params nil) m-params)))))
    (when (> xml-rpc-debug 1)
      (print m-func-call (create-file-buffer "func-call")))
    (xml-rpc-request server-url m-func-call async-callback-func)))

(defun xml-rpc-method-call (server-url method &rest params)
  "Call an XML-RPC method at SERVER-URL named METHOD with PARAMS as
parameters."
  (let ((response
         (xml-rpc-method-call-async nil server-url method params)))
    (cond ((stringp response)
           (list (cons nil (concat "URL/HTTP Error: " response))))
          (t
           (xml-rpc-xml-to-response response)))))

(provide 'xml-rpc)

;; Local Variables:
;; time-stamp-pattern: "20/^;; Last Modified: <%%>$"
;; End:

;;; xml-rpc.el ends here
