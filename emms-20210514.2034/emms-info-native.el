;;; emms-info-native.el --- Native Emacs Lisp info method for EMMS -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; Author: Petteri Hintsanen <petterih@iki.fi>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; This file provides a native emms-info-method for EMMS.  Here
;; "native" means a pure Emacs Lisp implementation instead of one
;; relying on external tools or libraries like `emms-info-ogginfo' or
;; `emms-info-libtag'.
;;
;; To use this method, add `emms-info-native' to
;; `emms-info-functions'.
;;
;; The following file formats are supported:
;;
;; - Vorbis: Ogg Vorbis I Profile, filename extension `.ogg',
;;   elementary streams only.  Based on xiph.org's Vorbis I
;;   specification, see URL
;;   `https://xiph.org/vorbis/doc/Vorbis_I_spec.html'.
;;
;; - Opus: Ogg Opus profile, filename extension `.opus', elementary
;;   streams only.  Based on RFC 7845, see URL
;;   `https://tools.ietf.org/html/rfc7845.html'.
;;
;; - FLAC streams in native encapsulation format, filename extension
;;   `.flac'.  Based on xiph.org's FLAC format specification, see URL
;;   `https://xiph.org/flac/format.html'.
;;
;; - MP3 files with extension `.mp3' and id3v2 tags.  All id3v2
;;   versions should work, but many features like CRC, compression and
;;   encryption are not supported.  Based on id3v2 Informal Standards,
;;   see URL `https://id3.org'.
;;
;; Format detection is based solely on filename extension, which is
;; matched case-insensitively.

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'emms-info)
(require 'seq)
(require 'subr-x)

(defconst emms-info-native--max-peek-size (* 2048 1024)
  "Maximum buffer size for metadata decoding.
Functions called by `emms-info-native' read certain amounts of
data into a temporary buffer while decoding metadata.  This
variable controls the maximum size of that buffer: if more than
`emms-info-native--max-peek-size' bytes are needed, an error is
signaled.

Technically metadata blocks can have almost arbitrary lengths,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.")

(defvar emms-info-native--opus-channel-count 0
  "Last decoded Opus channel count.
This is a kludge; it is needed because bindat spec cannot refer
outside itself.")

(defvar emms-info-native--id3v2-version 0
  "Last decoded id3v2 version.
This is a kludge; it is needed because bindat spec cannot refer
outside itself.")

;;;; Vorbis code

(defconst emms-info-native--max-num-vorbis-comments 1024
  "Maximum number of Vorbis comment fields in a stream.
Technically a single Vorbis stream may have up to 2^32 comments,
but in practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.

This limit is used with Opus and FLAC streams as well, since
their comments have almost the same format as Vorbis.")

(defconst emms-info-native--max-vorbis-comment-size (* 64 1024)
  "Maximum length for a single Vorbis comment field.
Technically a single Vorbis comment may have a length up to 2^32
bytes, but in practice processing must be constrained to prevent
memory exhaustion in case of garbled or malicious inputs.

This limit is used with Opus and FLAC streams as well, since
their comments have almost the same format as Vorbis.")

(defconst emms-info-native--max-vorbis-vendor-length 1024
  "Maximum length of Vorbis vendor string.
Technically a vendor string can be up to 2^32 bytes long, but in
practice processing must be constrained to prevent memory
exhaustion in case of garbled or malicious inputs.

This limit is used with Opus and FLAC streams as well, since
their comments have almost the same format as Vorbis.")

(defconst emms-info-native--accepted-vorbis-fields
  '("album"
    "albumartist"
    "albumartistsort"
    "albumsort"
    "artist"
    "artistsort"
    "composer"
    "composersort"
    "date"
    "discnumber"
    "genre"
    "label"
    "originaldate"
    "originalyear"
    "performer"
    "title"
    "titlesort"
    "tracknumber"
    "year")
  "EMMS info fields that are extracted from Vorbis comments.")

(defconst emms-info-native--vorbis-headers-bindat-spec
  '((identification-header struct emms-info-native--vorbis-identification-header-bindat-spec)
    (comment-header struct emms-info-native--vorbis-comment-header-bindat-spec))
  "Specification for first two Vorbis header packets.
They are always an identification header followed by a comment
header.")

(defconst emms-info-native--vorbis-identification-header-bindat-spec
  '((packet-type u8)
    (eval (unless (= last 1)
            (error "Vorbis header type mismatch: expected 1, got %s"
                   last)))
    (vorbis vec 6)
    (eval (unless (equal last emms-info-native--vorbis-magic-array)
            (error "Vorbis framing mismatch: expected `%s', got `%s'"
                   emms-info-native--vorbis-magic-array
                   last)))
    (vorbis-version u32r)
    (eval (unless (= last 0)
            (error "Vorbis version mismatch: expected 0, got %s"
                   last)))
    (audio-channels u8)
    (audio-sample-rate u32r)
    (bitrate-maximum u32r)
    (bitrate-nominal u32r)
    (bitrate-minimum u32r)
    (blocksize u8)
    (framing-flag u8)
    (eval (unless (= last 1))
          (error "Vorbis framing bit mismatch: expected 1, got %s"
                 last)))
  "Vorbis identification header specification.")

(defconst emms-info-native--vorbis-magic-array
  [118 111 114 98 105 115]
  "Header packet magic pattern `vorbis'.")

(defconst emms-info-native--vorbis-comment-header-bindat-spec
  '((packet-type u8)
    (eval (unless (= last 3)
            (error "Vorbis header type mismatch: expected 3, got %s"
                   last)))
    (vorbis vec 6)
    (eval (unless (equal last emms-info-native--vorbis-magic-array)
            (error "Vorbis framing mismatch: expected `%s', got `%s'"
                   emms-info-native--vorbis-magic-array
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "Vorbis vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Vorbis user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-native--vorbis-comment-field-bindat-spec))
    (framing-bit u8)
    (eval (unless (= last 1))
          (error "Vorbis framing bit mismatch: expected 1, got %s"
                 last)))
  "Vorbis comment header specification.")

(defconst emms-info-native--vorbis-comment-field-bindat-spec
  '((length u32r)
    (eval (when (> last emms-info-native--max-vorbis-comment-size)
            (error "Vorbis comment length %s is too long" last)))
    (user-comment vec (length)))
  "Vorbis comment field specification.")

(defun emms-info-native--extract-vorbis-comments (user-comments)
  "Return a decoded list of comments from USER-COMMENTS.
USER-COMMENTS should be a list of Vorbis comments according to
`user-comments' field in
`emms-info-native--vorbis-comment-header-bindat-spec',
`emms-info-native--opus-comment-header-bindat-spec' or
`emms-info-native--flac-comment-block-bindat-spec'.

Return comments in a list of (FIELD . VALUE) cons cells.  Only
FIELDs that are listed in
`emms-info-native--accepted-vorbis-fields' are returned."
  (let (comments)
    (dolist (user-comment user-comments)
      (let* ((comment (cdr (assoc 'user-comment user-comment)))
             (pair (emms-info-native--split-vorbis-comment comment)))
        (push pair comments)))
    (seq-filter (lambda (elt)
                  (member (car elt)
                          emms-info-native--accepted-vorbis-fields))
                comments)))

(defun emms-info-native--split-vorbis-comment (comment)
  "Split Vorbis comment to a field-value pair.
Vorbis comments are of form `FIELD=VALUE'.  FIELD is a
case-insensitive field name with a restricted set of ASCII
characters.  VALUE is an arbitrary UTF-8 encoded octet stream.

Return a cons cell (FIELD . VALUE), where FIELD is converted to
lower case and VALUE is the decoded value."
  (let ((comment-string (decode-coding-string (mapconcat
                                               #'byte-to-string
                                               comment
                                               nil)
                                              'utf-8)))
    (when (string-match "^\\(.+?\\)=\\(.+?\\)$" comment-string)
      (cons (downcase (match-string 1 comment-string))
            (match-string 2 comment-string)))))

;;;; Opus code

(defconst emms-info-native--opus-headers-bindat-spec
  '((identification-header struct emms-info-native--opus-identification-header-bindat-spec)
    (comment-header struct emms-info-native--opus-comment-header-bindat-spec))
  "Specification for two first Opus header packets.
They are always an identification header followed by a comment
header.")

(defconst emms-info-native--opus-identification-header-bindat-spec
  '((opus-head vec 8)
    (eval (unless (equal last emms-info-native--opus-head-magic-array)
            (error "Opus framing mismatch: expected `%s', got `%s'"
                   emms-info-native--opus-head-magic-array
                   last)))
    (opus-version u8)
    (eval (unless (< last 16)
            (error "Opus version mismatch: expected < 16, got %s"
                   last)))
    (channel-count u8)
    (eval (setq emms-info-native--opus-channel-count last))
    (pre-skip u16r)
    (sample-rate u32r)
    (output-gain u16r)
    (channel-mapping-family u8)
    (union (channel-mapping-family)
           (0 nil)
           (t (struct emms-info-native--opus-channel-mapping-table))))
  "Opus identification header specification.")

(defconst emms-info-native--opus-head-magic-array
  [79 112 117 115 72 101 97 100]
  "Opus identification header magic pattern `OpusHead'.")

(defconst emms-info-native--opus-channel-mapping-table
  '((stream-count u8)
    (coupled-count u8)
    (channel-mapping vec (eval emms-info-native--opus-channel-count)))
  "Opus channel mapping table specification.")

(defconst emms-info-native--opus-comment-header-bindat-spec
  '((opus-tags vec 8)
    (eval (unless (equal last emms-info-native--opus-tags-magic-array)
            (error "Opus framing mismatch: expected `%s', got `%s'"
                   emms-info-native--opus-tags-magic-array
                   last)))
    (vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "Opus vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "Opus user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-native--vorbis-comment-field-bindat-spec)))
  "Opus comment header specification.")

(defconst emms-info-native--opus-tags-magic-array
  [79 112 117 115 84 97 103 115]
  "Opus comment header magic pattern `OpusTags'.")

;;;; Ogg code

(defconst emms-info-native--ogg-page-size 65307
  "Maximum size for a single Ogg container page.")

(defconst emms-info-native--ogg-page-bindat-spec
  '((capture-pattern vec 4)
    (eval (unless (equal last emms-info-native--ogg-magic-array)
            (error "Ogg framing mismatch: expected `%s', got `%s'"
                   emms-info-native--ogg-magic-array
                   last)))
    (stream-structure-version u8)
    (eval (unless (= last 0)
            (error ("Ogg version mismatch: expected 0, got %s")
                   last)))
    (header-type-flag u8)
    (granule-position vec 8)
    (stream-serial-number vec 4)
    (page-sequence-no vec 4)
    (page-checksum vec 4)
    (page-segments u8)
    (segment-table vec (page-segments))
    (payload vec (eval (seq-reduce #'+ last 0))))
  "Ogg page structure specification.")

(defconst emms-info-native--ogg-magic-array
  [79 103 103 83]
  "Ogg format magic capture pattern `OggS'.")

(defun emms-info-native--decode-ogg-comments (filename stream-type)
  "Read and decode comments from Ogg file FILENAME.
The file is assumed to contain a single stream of type
STREAM-TYPE, which must either `vorbis' or `opus'.

Return comments in a list of (FIELD . VALUE) cons cells.  See
`emms-info-native--split-vorbis-comment' for details."
  (let* ((packets (emms-info-native--decode-ogg-packets filename 2))
         (headers (emms-info-native--decode-ogg-headers packets
                                                        stream-type))
         (comments (bindat-get-field headers
                                     'comment-header
                                     'user-comments)))
    (emms-info-native--extract-vorbis-comments comments)))

(defun emms-info-native--decode-ogg-packets (filename packets)
  "Read and decode packets from Ogg file FILENAME.
Read in data from the start of FILENAME, remove Ogg packet
frames, and concatenate payloads until at least PACKETS number of
packets have been decoded.  Return the decoded packets in a
vector, concatenated.

Data is read in `emms-info-native--ogg-page-size' chunks.  If the
total length of concatenated packets becomes greater than
`emms-info-native--max-peek-size', an error is signaled.

Only elementary streams are supported, that is, FILENAME should
contain only a single logical stream.  Note that this assumption
is not verified: with non-elementary streams packets from
different streams will be mixed together without an error."
  (let ((num-packets 0)
        (offset 0)
        (stream (vector)))
    (while (< num-packets packets)
      (let ((page (emms-info-native--decode-ogg-page filename
                                                     offset)))
        (cl-incf num-packets (or (plist-get page :num-packets) 0))
        (cl-incf offset (plist-get page :num-bytes))
        (setq stream (vconcat stream (plist-get page :stream)))
        (when (> (length stream) emms-info-native--max-peek-size)
          (error "Ogg payload is too large"))))
    stream))

(defun emms-info-native--decode-ogg-page (filename offset)
  "Read and decode a single Ogg page from FILENAME.
Starting reading data from byte offset OFFSET.

Return a plist (:num-packets N :num-bytes B :stream S), where N
is the number of packets in the page, B is the size of the page
in bytes, and S is the unframed logical bitstream in a vector.
Note that N can be zero."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename
                                    nil
                                    offset
                                    (+ offset
                                       emms-info-native--ogg-page-size))
    (let* ((page (bindat-unpack emms-info-native--ogg-page-bindat-spec
                                (buffer-string)))
           (num-packets (emms-info-native--num-of-packets page))
           (num-bytes (bindat-length emms-info-native--ogg-page-bindat-spec
                                     page))
           (stream (bindat-get-field page 'payload)))
      (list :num-packets num-packets
            :num-bytes num-bytes
            :stream stream))))

(defun emms-info-native--num-of-packets (page)
  "Return the number of packets in Ogg page PAGE.
PAGE must correspond to
`emms-info-native--ogg-page-bindat-spec'."
  ;; Every element that is less than 255 in the segment table
  ;; represents a packet boundary.
  (length (seq-filter (lambda (elt) (< elt 255))
                      (bindat-get-field page 'segment-table))))

(defun emms-info-native--decode-ogg-headers (packets stream-type)
  "Decode first two stream headers from PACKETS for STREAM-TYPE.
STREAM-TYPE must be either `vorbis' or `opus'.

Return a structure that corresponds to either
`emms-info-native--opus-headers-bindat-spec' or
`emms-info-native--vorbis-headers-bindat-spec'."
  (cond ((eq stream-type 'vorbis)
         (bindat-unpack emms-info-native--vorbis-headers-bindat-spec
                        packets))
        ((eq stream-type 'opus)
         (let (emms-info-native--opus-channel-count)
           (bindat-unpack emms-info-native--opus-headers-bindat-spec
                          packets)))
        (t (error "Unknown stream type %s" stream-type))))

;;;; FLAC code

(defconst emms-info-native--flac-metadata-block-header-bindat-spec
  '((flags u8)
    (length u24)
    (eval (when (or (> last emms-info-native--max-peek-size)
                    (= last 0))
            (error "FLAC block length %s is invalid" last))))
  "FLAC metadata block header specification.")

(defconst emms-info-native--flac-comment-block-bindat-spec
  '((vendor-length u32r)
    (eval (when (> last emms-info-native--max-vorbis-vendor-length)
            (error "FLAC vendor length %s is too long" last)))
    (vendor-string vec (vendor-length))
    (user-comments-list-length u32r)
    (eval (when (> last emms-info-native--max-num-vorbis-comments)
            (error "FLAC user comment list length %s is too long"
                   last)))
    (user-comments repeat
                   (user-comments-list-length)
                   (struct emms-info-native--vorbis-comment-field-bindat-spec)))
  "FLAC Vorbis comment block specification.")

(defun emms-info-native--decode-flac-comments (filename)
  "Read and decode comments from FLAC file FILENAME.
Return comments in a list of (FIELD . VALUE) cons cells.  Only
FIELDs that are listed in
`emms-info-native--accepted-vorbis-fields' are returned."
  (unless (emms-info-native--has-flac-signature filename)
    (error "Invalid FLAC stream"))
  (let* ((block (emms-info-native--decode-flac-comment-block
                 filename))
         (unpacked (bindat-unpack emms-info-native--flac-comment-block-bindat-spec
                                  block))
         (user-comments (bindat-get-field unpacked 'user-comments)))
    (emms-info-native--extract-vorbis-comments user-comments)))

(defun emms-info-native--has-flac-signature (filename)
  "Check for FLAC stream marker at the beginning of FILENAME.
Return t if there is a valid stream marker, nil otherwise."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 4)
    (looking-at "fLaC")))

(defun emms-info-native--decode-flac-comment-block (filename)
  "Read and decode a comment block from FLAC file FILENAME.
Return the comment block data in a vector."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let (comment-block
          last-flag
          (offset 4))
      (while (and (not comment-block) (not last-flag))
        (insert-file-contents-literally filename
                                        nil
                                        offset
                                        (cl-incf offset 4))
        (let* ((header (bindat-unpack emms-info-native--flac-metadata-block-header-bindat-spec
                                      (buffer-string)))
               (end (+ offset (bindat-get-field header 'length)))
               (flags (bindat-get-field header 'flags))
               (block-type (logand flags #x7F)))
          (setq last-flag (> (logand flags #x80) 0))
          (when (> block-type 6)
            (error "FLAC block type error: expected <= 6, got %s"
                   block-type))
          (when (= block-type 4)
            ;; Comment block found, extract it.
            (insert-file-contents-literally filename nil offset end t)
            (setq comment-block (vconcat (buffer-string))))
          (setq offset end)))
      comment-block)))

;;;; id3v2 (MP3) code

(defconst emms-info-native--id3v2-header-bindat-spec
  '((file-identifier vec 3)
    (eval (unless (equal last emms-info-native--id3v2-magic-array)
            (error "id3v2 framing mismatch: expected `%s', got `%s'"
                   emms-info-native--id3v2-magic-array
                   last)))
    (version u8)
    (eval (setq emms-info-native--id3v2-version last))
    (revision u8)
    (flags bits 1)
    (size-bytes vec 4)
    (size eval (emms-info-native--checked-id3v2-size 'tag last)))
  "id3v2 header specification.")

(defconst emms-info-native--id3v2-magic-array
  [#x49 #x44 #x33]
  "id3v2 header magic pattern `ID3'.")

(defconst emms-info-native--id3v2-frame-header-bindat-spec
  '((id str (eval (if (= emms-info-native--id3v2-version 2) 3 4)))
    (eval (unless (emms-info-native--valid-id3v2-frame-id-p last)
            (error "id3v2 frame id `%s' is invalid" last)))
    (size-bytes vec (eval (if (= emms-info-native--id3v2-version 2) 3 4)))
    (size eval (emms-info-native--checked-id3v2-size 'frame last))
    (flags bits (eval (if (= emms-info-native--id3v2-version 2) 0 2))))
  "id3v2 frame header specification.")

(defconst emms-info-native--id3v2-frame-to-info
  '(("TAL"  . "album")
    ("TALB" . "album")
    ("TPE2" . "albumartist")
    ("TSO2" . "albumartistsort")
    ("TSOA" . "albumsort")
    ("TP1"  . "artist")
    ("TPE1" . "artist")
    ("TSOP" . "artistsort")
    ("TCM"  . "composer")
    ("TCOM" . "composer")
    ("TSOC" . "composersort")
    ("TDRC" . "date")
    ("TPA"  . "discnumber")
    ("TPOS" . "discnumber")
    ("TCON" . genre)
    ("TPUB" . "label")
    ("TDOR" . "originaldate")
    ("TOR"  . "originalyear")
    ("TORY" . "originalyear")
    ("TIT2" . "title")
    ("TT2"  . "title")
    ("TSOT" . "titlesort")
    ("TRK"  . "tracknumber")
    ("TRCK" . "tracknumber")
    ("TYE"  . "year")
    ("TYER" . "year")
    ("TXXX" . user-defined))
  "Mapping from id3v2 frame identifiers to EMMS info fields.

Sources:

- URL `https://picard-docs.musicbrainz.org/en/appendices/tag_mapping.html'
- URL `http://wiki.hydrogenaud.io/index.php?title=Foobar2000:ID3_Tag_Mapping'")

(defconst emms-info-native--id3v1-genres
  '((0 . "Blues")
    (1 . "Classic Rock")
    (2 . "Country")
    (3 . "Dance")
    (4 . "Disco")
    (5 . "Funk")
    (6 . "Grunge")
    (7 . "Hip-Hop")
    (8 . "Jazz")
    (9 . "Metal")
    (10 . "New Age")
    (11 . "Oldies")
    (12 . "Other")
    (13 . "Pop")
    (14 . "R&B")
    (15 . "Rap")
    (16 . "Reggae")
    (17 . "Rock")
    (18 . "Techno")
    (19 . "Industrial")
    (20 . "Alternative")
    (21 . "Ska")
    (22 . "Death Metal")
    (23 . "Pranks")
    (24 . "Soundtrack")
    (25 . "Euro-Techno")
    (26 . "Ambient")
    (27 . "Trip-Hop")
    (28 . "Vocal")
    (29 . "Jazz+Funk")
    (30 . "Fusion")
    (31 . "Trance")
    (32 . "Classical")
    (33 . "Instrumental")
    (34 . "Acid")
    (35 . "House")
    (36 . "Game")
    (37 . "Sound Clip")
    (38 . "Gospel")
    (39 . "Noise")
    (40 . "AlternRock")
    (41 . "Bass")
    (42 . "Soul")
    (43 . "Punk")
    (44 . "Space")
    (45 . "Meditative")
    (46 . "Instrumental Pop")
    (47 . "Instrumental Rock")
    (48 . "Ethnic")
    (49 . "Gothic")
    (50 . "Darkwave")
    (51 . "Techno-Industrial")
    (52 . "Electronic")
    (53 . "Pop-Folk")
    (54 . "Eurodance")
    (55 . "Dream")
    (56 . "Southern Rock")
    (57 . "Comedy")
    (58 . "Cult")
    (59 . "Gangsta")
    (60 . "Top 40")
    (61 . "Christian Rap")
    (62 . "Pop/Funk")
    (63 . "Jungle")
    (64 . "Native American")
    (65 . "Cabaret")
    (66 . "New Wave")
    (67 . "Psychadelic")
    (68 . "Rave")
    (69 . "Showtunes")
    (70 . "Trailer")
    (71 . "Lo-Fi")
    (72 . "Tribal")
    (73 . "Acid Punk")
    (74 . "Acid Jazz")
    (75 . "Polka")
    (76 . "Retro")
    (77 . "Musical")
    (78 . "Rock & Roll")
    (79 . "Hard Rock")
    (80 . "Folk")
    (81 . "Folk-Rock")
    (82 . "National Folk")
    (83 . "Swing")
    (84 . "Fast Fusion")
    (85 . "Bebob")
    (86 . "Latin")
    (87 . "Revival")
    (88 . "Celtic")
    (89 . "Bluegrass")
    (90 . "Avantgarde")
    (91 . "Gothic Rock")
    (92 . "Progressive Rock")
    (93 . "Psychedelic Rock")
    (94 . "Symphonic Rock")
    (95 . "Slow Rock")
    (96 . "Big Band")
    (97 . "Chorus")
    (98 . "Easy Listening")
    (99 . "Acoustic")
    (100 . "Humour")
    (101 . "Speech")
    (102 . "Chanson")
    (103 . "Opera")
    (104 . "Chamber Music")
    (105 . "Sonata")
    (106 . "Symphony")
    (107 . "Booty Bass")
    (108 . "Primus")
    (109 . "Porn Groove")
    (110 . "Satire")
    (111 . "Slow Jam")
    (112 . "Club")
    (113 . "Tango")
    (114 . "Samba")
    (115 . "Folklore")
    (116 . "Ballad")
    (117 . "Power Ballad")
    (118 . "Rhythmic Soul")
    (119 . "Freestyle")
    (120 . "Duet")
    (121 . "Punk Rock")
    (122 . "Drum Solo")
    (123 . "A cappella")
    (124 . "Euro-House")
    (125 . "Dance Hall"))
  "id3v1 genres.")

(defconst emms-info-native--id3v2-text-encodings
  '((0 . latin-1)
    (1 . utf-16)
    (2 . uft-16be)
    (3 . utf-8))
  "id3v2 text encodings.")

(defun emms-info-native--valid-id3v2-frame-id-p (id)
  "Return t if ID is a proper id3v2 frame identifier, nil otherwise."
  (if (= emms-info-native--id3v2-version 2)
      (string-match "[A-Z0-9]\\{3\\}" id)
    (string-match "[A-Z0-9]\\{4\\}" id)))

(defun emms-info-native--checked-id3v2-size (elt bytes)
  "Calculate id3v2 element ELT size from BYTES.
ELT must be either 'tag or 'frame.

Return the size.  Signal an error if the size is zero."
  (let ((size (cond ((eq elt 'tag)
                     (emms-info-native--decode-id3v2-size bytes t))
                    ((eq elt 'frame)
                     (if (= emms-info-native--id3v2-version 4)
                         (emms-info-native--decode-id3v2-size bytes t)
                       (emms-info-native--decode-id3v2-size bytes nil))))))
    (if (zerop size)
        (error "id3v2 tag/frame size is zero")
      size)))

(defun emms-info-native--decode-id3v2-size (bytes syncsafe)
  "Decode id3v2 element size from BYTES.
Depending on SYNCSAFE, BYTES are interpreted as 7- or 8-bit
bytes, MSB first.

Return the decoded size."
  (let ((num-bits (if syncsafe 7 8)))
    (apply '+ (seq-map-indexed (lambda (elt idx)
                                 (* (expt 2 (* num-bits idx)) elt))
                               (reverse bytes)))))

(defun emms-info-native--decode-id3v2 (filename)
  "Read and decode id3v2 metadata from FILENAME.
Return metadata in a list of (FIELD . VALUE) cons cells, or nil
in case of errors or if there were no known fields in FILENAME.

See `emms-info-native--id3v2-frame-to-info' for recognized
fields."
  (condition-case nil
      (let* (emms-info-native--id3v2-version
             (header (emms-info-native--decode-id3v2-header filename))
             (tag-size (bindat-get-field header 'size))
             (unsync (memq 7 (bindat-get-field header 'flags)))
             (offset 10))
        (when (memq 6 (bindat-get-field header 'flags))
          ;; Skip the extended header.
          (cl-incf offset
                   (emms-info-native--checked-id3v2-ext-header-size filename)))
        (emms-info-native--decode-id3v2-frames filename
                                               offset
                                               (+ tag-size 10)
                                               unsync))
    (error nil)))

(defun emms-info-native--decode-id3v2-header (filename)
  "Read and decode id3v2 header from FILENAME."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 10)
    (bindat-unpack emms-info-native--id3v2-header-bindat-spec
                   (buffer-string))))

(defun emms-info-native--checked-id3v2-ext-header-size (filename)
  "Read and decode id3v2 extended header size from FILENAME.
Return the size.  Signal an error if the size is zero."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 10 14)
    (emms-info-native--checked-id3v2-size 'frame (buffer-string))))

(defun emms-info-native--decode-id3v2-frames (filename begin end unsync)
  "Read and decode id3v2 text frames from FILENAME.
BEGIN should be the offset of first byte of the first frame, and
END should be the offset after the complete id3v2 tag.

If UNSYNC is t, the frames are assumed to have gone through
unsynchronization and decoded as such.

Return metadata in a list of (FIELD . VALUE) cons cells."
  (let ((offset begin)
        (limit (- end (emms-info-native--id3v2-frame-header-size)))
        comments)
    (condition-case nil
        (while (< offset limit)
          (let* ((frame-data (emms-info-native--decode-id3v2-frame filename
                                                                   offset
                                                                   unsync))
                 (next-frame-offset (car frame-data))
                 (comment (cdr frame-data)))
            (when comment (push comment comments))
            (setq offset next-frame-offset)))
      (error nil))
    comments))

(defun emms-info-native--id3v2-frame-header-size ()
  "Return the last decoded header size in bytes."
  (if (= emms-info-native--id3v2-version 2) 6 10))

(defun emms-info-native--decode-id3v2-frame (filename offset unsync)
  (let* ((header (emms-info-native--decode-id3v2-frame-header filename
                                                              offset))
         (info-id (emms-info-native--id3v2-frame-info-id header))
         (data-offset (car header))
         (size (bindat-get-field (cdr header) 'size)))
    (if (or info-id unsync)
        ;; Note that if unsync is t, we have to always read the frame
        ;; to determine next-frame-offset.
        (let* ((data (emms-info-native--read-id3v2-frame-data filename
                                                              data-offset
                                                              size
                                                              unsync))
               (next-frame-offset (car data))
               (value (emms-info-native--decode-id3v2-frame-data (cdr data)
                                                                 info-id)))
          (cons next-frame-offset value))
      ;; Skip the frame.
      (cons (+ data-offset size) nil))))

(defun emms-info-native--decode-id3v2-frame-header (filename begin)
  "Read and decode id3v2 frame header from FILENAME.
Start reading from offset BEGIN.

Return a cons cell (OFFSET . FRAME), where OFFSET is the byte
offset after the frame header, and FRAME is the decoded frame."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((end (+ begin (emms-info-native--id3v2-frame-header-size))))
      (insert-file-contents-literally filename nil begin end)
      (cons end (bindat-unpack emms-info-native--id3v2-frame-header-bindat-spec
                               (buffer-string))))))

(defun emms-info-native--id3v2-frame-info-id (frame)
  "Return the emms-info identifier for FRAME.
If there is no such identifier, return nil."
  (cdr (assoc (bindat-get-field frame 'id)
              emms-info-native--id3v2-frame-to-info)))

(defun emms-info-native--read-id3v2-frame-data (filename
                                                begin
                                                num-bytes
                                                unsync)
  "Read NUM-BYTES of raw id3v2 frame data from FILENAME.
Start reading from offset BEGIN.  If UNSYNC is t, all 'FF 00'
byte combinations are replaced by 'FF'.  Replaced byte pairs are
counted as one, instead of two, towards NUM-BYTES.

Return a cons cell (OFFSET . DATA), where OFFSET is the byte
offset after NUM-BYTES bytes have been read, and DATA is the raw
data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if unsync
        ;; Reverse unsynchronization.
        (let ((peek-end (+ begin (* 2 num-bytes)))
              (end num-bytes))
          (insert-file-contents-literally filename nil begin peek-end)
          (goto-char (point-min))
          (while (and (re-search-forward (string 255 0) nil t)
                      (< (point) end))
            (replace-match (string 255))
            (cl-incf end 1))
          (delete-region (1+ num-bytes) (point-max))
          (cons (+ begin end) (buffer-string)))
      ;; No unsynchronization: read the data as-is.
      (let ((end (+ begin num-bytes)))
        (insert-file-contents-literally filename nil begin end)
        (cons end (buffer-string))))))

(defun emms-info-native--decode-id3v2-frame-data (data info-id)
  "Decode id3v2 text frame data DATA.
If INFO-ID is `user-defined', assume that DATA is a TXXX frame
with key/value-pair.  Extract the key and, if it is a mapped
element in `emms-info-native--id3v2-frame-to-info', use it as
INFO-ID.

If INFO-ID is `genre', assume that DATA is either an integral
id3v1 genre reference or a plain genre string.  In the former
case map the reference to a string via
`emms-info-native--id3v1-genres'; in the latter case use the
genre string verbatim.

Return a cons cell (INFO-ID . VALUE) where VALUE is the decoded
string."
  (when info-id
    (let ((str (emms-info-native--decode-id3v2-string data)))
      (cond ((stringp info-id) (cons info-id str))
            ((eq info-id 'genre)
             (if (string-match "^(?\\([0-9]+\\))?" str)
                 (let ((v1-genre (assoc (string-to-number (match-string 1 str))
                                        emms-info-native--id3v1-genres)))
                   (when v1-genre (cons "genre" (cdr v1-genre))))
               (cons "genre" str)))
            ((eq info-id 'user-defined)
             (let* ((key-val (split-string str (string 0)))
                    (key (downcase (car key-val)))
                    (val (cadr key-val)))
               (when (rassoc key emms-info-native--id3v2-frame-to-info)
                 (cons key val))))))))

(defun emms-info-native--decode-id3v2-string (bytes)
  "Decode id3v2 text information from BYTES.
Remove the terminating null byte, if any.

Return the text as string."
  (let* ((encoding (emms-info-native--id3v2-text-encoding bytes))
         (string (mapconcat #'byte-to-string (seq-rest bytes) ""))
         (decoded (decode-coding-string string encoding)))
    (when (> (length decoded) 0)
      (if (equal (substring decoded -1) "\0")
          (substring decoded 0 -1)
        decoded))))

(defun emms-info-native--id3v2-text-encoding (bytes)
  "Return the encoding for text information BYTES."
  (cdr (assoc (seq-first bytes)
              emms-info-native--id3v2-text-encodings)))

;;;; EMMS code

(defun emms-info-native (track)
  "Set info fields for TRACK.
Supports Ogg Vorbis/Opus, FLAC, and MP3 files."
  (condition-case env
      (let* ((filename (emms-track-name track))
             (info-fields (emms-info-native--decode-info-fields filename)))
	(dolist (field info-fields)
	  (let ((name (intern (concat "info-" (car field))))
		(value (cdr field)))
            (unless (zerop (length value))
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
				  (string-to-number value)
				(string-trim-right value)))))))
    (error (message "emms-info-native error processing %s: %s"
		    (emms-track-name track) env))))

(defun emms-info-native--decode-info-fields (filename)
  "Decode info fields from FILENAME.
Return a list of (FIELD . VALUE) cons cells, where FIELD is an
info field and VALUE is the corresponding info value.  Both are
strings."
  (let ((stream-type (emms-info-native--find-stream-type filename)))
    (cond ((or (eq stream-type 'vorbis) (eq stream-type 'opus))
           (emms-info-native--decode-ogg-comments filename stream-type))
          ((eq stream-type 'flac)
           (emms-info-native--decode-flac-comments filename))
          ((eq stream-type 'mp3)
           (emms-info-native--decode-id3v2 filename))
          (t nil))))

(defun emms-info-native--find-stream-type (filename)
  "Deduce the stream type from FILENAME.
This is a naive implementation that relies solely on filename
extension.

Return one of symbols `vorbis', `opus', `flac', or `mp3'."
  (let ((case-fold-search t))
    (cond ((string-match ".ogg$" filename) 'vorbis)
          ((string-match ".opus$" filename) 'opus)
          ((string-match ".flac$" filename) 'flac)
          ((string-match ".mp3$" filename) 'mp3)
          (t nil))))

(provide 'emms-info-native)

;;; emms-info-native.el ends here
