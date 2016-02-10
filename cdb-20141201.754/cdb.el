;;; cdb.el --- constant database (cdb) reader for Emacs Lisp

;; Copyright (C) 2010 Yusuke Shinyama <yusuke at cs . nyu . edu>

;; Author: Yusuke Shinyama <yusuke at cs . nyu . edu>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: cdb
;; Package-Version: 20141201.754
;; Version: 20100719+
;; Last Modified: $Date: 2014/12/01 07:54:45 $

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;; 
;; For more information, please refer to <http://unlicense.org>

;;; Commentary:

;;  Usage:
;;    (cdb-init "foo.cdb") -> nil
;;    (cdb-get "foo.cdb" "abc") -> "123"
;;    (cdb-keys "foo.cdb") -> ("abc" ...)
;;    (cdb-uninit "foo.cdb") -> nil
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defconst cdb-version "20100719+")


;;  uint32le functions
;;    Represents little endian uint32 value as a 4-byte string.
;;    We need them for making up with Emacs' 30-bit integer system.
;;    This should work on 64-bit machines, but I haven't tested yet.
;; 
(defun uint32le (x)
  "creates uint32le string from an integer."
  (string (% x 256)
	  (% (lsh x -8) 256)
	  (% (lsh x -16) 256)
	  (% (lsh x -24) 256)))

(defun uint32le-int (v)
  "converts uint32le string v into an integer (if they can fit)."
  (logior (aref v 0)
	  (lsh (aref v 1) 8)
	  (lsh (aref v 2) 16)
	  (lsh (aref v 3) 24)))

(defun uint32le-int8l (v)
  "returns the lower 8 bits of the value."
  (aref v 0))

(defun uint32le-int24u (v) 
  "returns the upper 8 bits of the value."
  (logior (aref v 1)
	  (lsh (aref v 2) 8)
	  (lsh (aref v 3) 16)))

(defun uint32le-eq (v1 v2)
  "returns true of the two uint32le values are equal."
  (and (= (aref v1 0) (aref v2 0))
       (= (aref v1 1) (aref v2 1))
       (= (aref v1 2) (aref v2 2))
       (= (aref v1 3) (aref v2 3))))
       
(defun uint32le-add (v1 v2)
  "adds two uint32le values v1 and v2."
  (let ((a (string 0 0 0 0))
	(x (+ (aref v1 0) (aref v2 0))))
    (aset a 0 (% x 256))
    (setq x (+ (aref v1 1) (aref v2 1) (/ x 256)))
    (aset a 1 (% x 256))
    (setq x (+ (aref v1 2) (aref v2 2) (/ x 256)))
    (aset a 2 (% x 256))
    (setq x (+ (aref v1 3) (aref v2 3) (/ x 256)))
    (aset a 3 (% x 256))
    a))

(defun uint32le-lsh (v n)
  "left-shifts the value v by n bits."
  (let ((a (string 0 0 0 0))
	(x (lsh (aref v 0) n)))
    (aset a 0 (% x 256))
    (setq x (logior (lsh (aref v 1) n) (/ x 256)))
    (aset a 1 (% x 256))
    (setq x (logior (lsh (aref v 2) n) (/ x 256)))
    (aset a 2 (% x 256))
    (setq x (logior (lsh (aref v 3) n) (/ x 256)))
    (aset a 3 (% x 256))
    a))

(defun uint32le-xor (v1 v2)
  "computes the exclusive-or of v1 and v2."
  (string (logxor (aref v1 0) (aref v2 0))
	  (logxor (aref v1 1) (aref v2 1))
	  (logxor (aref v1 2) (aref v2 2))
	  (logxor (aref v1 3) (aref v2 3))))


;;  cdb functions
;;
;;  A cdb reader object is associated with a buffer.
;;  Each cdb buffer contains a fixed-length (2048 bytes) header
;;  and temporary strings that are read from the file.
;;
(defconst cdb-header-size 2048)
(defconst cdb-hash-initvalue 5381)
(defvar cdb-header nil)
(make-variable-buffer-local 'cdb-header)

(defun cdb-sget (s offset n)
  (substring-no-properties s offset (+ n offset)))

(defun cdb-hash (s)
  "computes an uint32le hash value for string s."
  (let ((h (uint32le cdb-hash-initvalue)))
    (mapc (lambda (c) (setq h (uint32le-xor (uint32le-add h (uint32le-lsh h 5)) (uint32le c))))
	  s)
    h))

(defun cdb-buffer-name (path)
  "returns a buffer name associated with a cdb file."
  (concat " *cdb:" path "*"))

(defun cdb-read-file (path offset length)
  "reads from a cdb file and returns its contents as a string."
  (with-current-buffer (cdb-buffer-name path)
    (erase-buffer)
    (insert-file-contents-literally path nil offset (+ length offset))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cdb-init (path)
  "initializes an cdb reader that is associated with the given pathname."
  (let ((buffer-name (cdb-buffer-name path)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
	(set-buffer-multibyte nil)  ; necessary to handle binary bits.
	(buffer-disable-undo)
	(auto-save-mode -1)
	(setq cdb-header (cdb-read-file path 0 cdb-header-size))))
    buffer-name))

(defun cdb-uninit (path)
  "uninitialize an cdb reader associated with the given pathname."
  (let ((buffer (get-buffer (cdb-buffer-name path))))
    (when buffer (kill-buffer buffer))))

(defun cdb-get (path key)
  "obtains the value that is associated with a key, or nil if not found."
  (let ((buffer (get-buffer (cdb-buffer-name path)))
	(hashval (cdb-hash key)))
    (unless buffer (error "cdb not initialized: %s" path))
    (with-current-buffer buffer
      (let* ((boffset (* 8 (uint32le-int8l hashval)))
	     (foffset (uint32le-int (cdb-sget cdb-header boffset 4)))
	     (nents (uint32le-int (cdb-sget cdb-header (+ 4 boffset) 4))))
	;; get the subtable associated with the lower 8 bits of the hash value.
	(let ((ents (cdb-read-file path foffset (* nents 8)))
	      (o (uint32le-int24u hashval))
	      (n 0))
	  (catch 'found
	    (while (< n nents)
	      ;; check each hash value in the subtable.
	      (let ((i (% (+ o n) nents)))
		(when (uint32le-eq hashval (cdb-sget ents (* i 8) 4))
		  (let ((foffset (uint32le-int (cdb-sget ents (+ 4 (* i 8)) 4))))
		    (unless (zerop foffset)
		      ;; hash value matches, now look into the actual key.
		      (let ((keylen (uint32le-int (cdb-read-file path foffset 4))))
			(when (equal key (cdb-read-file path (+ 8 foffset) keylen))
			  ;; got it. get the value associated with the key.
			  (let ((vallen (uint32le-int (cdb-read-file path (+ 4 foffset) 4))))
			    (throw 'found (cdb-read-file path (+ keylen 8 foffset) vallen))
			    )))) ; unless
		    )) ; when
		) ; let
	      (setq n (1+ n))
	      )) ; catch
	  )))))

(defun cdb-mapc (path func)
  "apply the function to each key/value pair in the database."
  (let ((buffer (get-buffer (cdb-buffer-name path))))
    (unless buffer (error "cdb not initialized: %s" path))
    (with-current-buffer buffer
      (let ((nkeys 0)
	    (foffset cdb-header-size)
	    (foffset-end (uint32le-int (cdb-read-file path 0 4))))
	;; read until it reaches the beginning of the first subtable.
	(while (< foffset foffset-end)
	  (let ((keylen (uint32le-int (cdb-read-file path foffset 4)))
		(vallen (uint32le-int (cdb-read-file path (+ 4 foffset) 4))))
	    ;; get a key and value.
	    (funcall func
		     (cdb-read-file path (+ 8 foffset) keylen)
		     (cdb-read-file path (+ keylen 8 foffset) vallen))
	    (setq foffset (+ keylen vallen 8 foffset))
	    ) ; let
	  (setq nkeys (1+ nkeys))
	  ) ; while
	nkeys))))

(defun cdb-keys (path)
  "returns all keys in the database."
  (let (keys)
    (cdb-mapc path (lambda (key value) (setq keys (cons key keys))))
    (nreverse keys)))
  
(defun cdb-values (path)
  "returns all values in the database."
  (let (values)
    (cdb-mapc path (lambda (key value) (setq values (cons value values))))
    (nreverse values)))

(provide 'cdb)

;;  Simplistic test suite
;; 
(defun cdb-test-simple (path)
  ;; test.cdb should be:
  ;;	+2,8:de->00596d84
  ;;	+2,8:eD->00596d84
  ;;	+2,8:dE->00596da4
  ;;	+3,8:xxx->0b8791dd
  ;;	+5,8:xxxxx->0bb36ddd
  ;;	+6,8:xxxxxx->82212905
  ;;	+7,8:xxxxxxx->c64649dd
  ;;	+3,8:abc->0b873285
  ;;	+2,2:\xa4\xa2->\xa4\xa4
  ;;
  (cdb-init path)
  (assert (string= (cdb-get path "de") "00596d84"))
  (assert (string= (cdb-get path "eD") "00596d84"))
  (assert (string= (cdb-get path "dE") "00596da4"))
  (assert (not (cdb-get path "ed")))
  (assert (not (cdb-get path "x")))
  (assert (string= (cdb-get path "xxx") "0b8791dd"))
  (assert (not (cdb-get path "xxxx")))
  (assert (string= (cdb-get path "xxxxx") "0bb36ddd"))
  (assert (string= (cdb-get path "xxxxxx") "82212905"))
  (assert (string= (cdb-get path "xxxxxxx") "c64649dd"))
  (assert (string= (cdb-get path "abc") "0b873285"))
  (assert (not (cdb-get path "bbbb")))
  (assert (not (cdb-get path "dd")))
  (assert (string= (cdb-get path "\244\242") "\244\244"))
  (assert (equal (cdb-keys path) 
		 '("de" "eD" "dE" "xxx" "xxxxx" "xxxxxx" 
		   "xxxxxxx" "abc" "\244\242")))
  (cdb-uninit path)
  )
;(cdb-test-simple "test.cdb")

;;; cdb.el ends here
