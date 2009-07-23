;;; Use raw text to fetch this code, see EmacsForMoinMoin for tutorial and discussion.

;;; Download link: http://moinmo.in/EmacsForMoinMoin/MoinMoinMode?action=raw

;;; moinmoin-mode.el --- a major mode to edit MoinMoin wiki pages

;; Written by ASK, distributed under GPL

;; Filename:      moinmoin-mode.el
;; Keywords:      moinmoin, wiki
;; Description:   a major mode to edit MoinMoin wiki pages
;; Compatibility: GNU Emacs 22.0.50.1 (probably others)
;; Last modified: 2006-04-15

;; This file is NOT part of GNU Emacs.

(require 'screen-lines) ; uses screen-lines.el by Yuji Minejima, tested with 0.55


;;; Definition of font faces
(defgroup moinmoin nil
  "Major mode for MoinMoin wiki pages"
  :prefix "moinmoin-")

(defface moinmoin-h5 '((t (:foreground "tan3" :slant italic)))
  "Face name to use for 5-level headings in moinmoin"
  :group 'moinmoin)
(defface moinmoin-h4 '((t (:inherit moinmoin-h5 :slant normal :foreground "tan4")))
  "Face name to use for 4-level headings in moinmoin"
  :group 'moinmoin)
(defface moinmoin-h3 '((t (:inherit moinmoin-h4 :weight bold)))
  "Face name to use for 3-level headings in moinmoin"
  :group 'moinmoin)
(defface moinmoin-h2 '((t (:inherit moinmoin-h3 :height 1.2)))
  "Face name to use for 3-level headings in moinmoin"
  :group 'moinmoin)
(defface moinmoin-h1 '((t (:inherit moinmoin-h2 :height 1.4)))
  "Face name to use for 3-level headings in moinmoin"
  :group 'moinmoin)

(defface moinmoin-smiley '((t (:foreground "green4" :weight bold)))
  "Face name to use smileys in moinmoin"
  :group 'moinmoin)

(defface moinmoin-table-separator '((t (:foreground "salmon" :weight bold)))
  "Face name to use for separation of columns in tables of moinmoin"
  :group 'moinmoin)
(defface moinmoin-table-pi '((t (:foreground "plum3")))
  "Face name to use for processing instructions in tables of moinmoin"
  :group 'moinmoin)

(defface moinmoin-anchor '((t (:foreground "tan3" :height 0.8)))
  "Face name to use for anchors (targets) in moinmoin"
  :group 'moinmoin)
(defface moinmoin-anchor-ref-id '((t (:foreground "blue2" :height 0.8 :underline t)))
  "Face name to use for id in anchor reference in moinmoin"
  :group 'moinmoin)
(defface moinmoin-anchor-ref-title '((t (:foreground "blue4" :underline t)))
  "Face name to use for title in anchors reference in moinmoin"
  :group 'moinmoin)

(defface moinmoin-macro-name '((t (:foreground "plum3")))
  "Face name to use for names of macros in moinmoin"
  :group 'moinmoin)
(defface moinmoin-macro-parameters '((t (:foreground "plum4")))
  "Face name to use for parameters of macros in moinmoin"
  :group 'moinmoin)

(defface moinmoin-item '((t (:foreground "brown" :weight bold)))
  "Face name to use for items in lists in moinmoin"
  :group 'moinmoin)
(defface moinmoin-item-2 '((t (:foreground "brown")))
  "Face name to use for second-level items in moinmoin"
  :group 'moinmoin)
(defface moinmoin-blockquote-indent '((t (:background "aquamarine1")))
  "Face name to use for blockquote indentation in moinmoin"
  :group 'moinmoin)
(defface moinmoin-blockquote-text '((t (:foreground "aquamarine4")))
  "Face name to use for blockquote text in moinmoin"
  :group 'moinmoin)

(defface moinmoin-code '((t (:foreground "purple4")))
  "Face name to use for code inside braces in moinmoin"
  :group 'moinmoin)
(defface moinmoin-code-braces '((t (:foreground "plum3")))
  "Face name to use for baces which delimit code inside braces in moinmoin"
  :group 'moinmoin)

(defface moinmoin-pi '((t (:foreground "plum3" :weight bold)))
  "Face name to use for processing instructions in moinmoin"
  :group 'moinmoin)
(defface moinmoin-comment '((t (:foreground "maroon3")))
  "Face name to use for comments in moinmoin"
  :group 'moinmoin)
(defface moinmoin-rule '((t (:foreground "tomato2" :weight bold)))
  "Face name to use for rules in moinmoin"
  :group 'moinmoin)
(defface moinmoin-ss '((t (:foreground "grey" ))) ;; try also :height 0.1
  "Face name to use for syntactic sugar in moinmoin"
  :group 'moinmoin)
(defface moinmoin-tt '((t (:foreground "cyan4")))
  "Face name to use for typewriter text in moinmoin"
  :group 'moinmoin)
(defface moinmoin-entity '((t (:foreground "grey")))
  "Face name to use for HTML entities in moinmoin"
  :group 'moinmoin)
(defface moinmoin-email '((t (:foreground "blue2")))
  "Face name to use for emails in moinmoin"
  :group 'moinmoin)
(defface moinmoin-url '((t (:foreground "blue2" :height 0.8)))
  "Face name to use for URLs in moinmoin"
  :group 'moinmoin)
(defface moinmoin-url-title '((t (:foreground "blue4" :underline t)))
  "Face name to use for title of URL in moinmoin"
  :group 'moinmoin)
(defface moinmoin-wiki-link '((t (:foreground "blue4" :weight bold)))
  "Face name to use for CamelCase links in moinmoin"
  :group 'moinmoin)
(defface moinmoin-inter-wiki-link '((t (:foreground "blue3" :weight bold)))
  "Face name to use for inter wiki links in moinmoin"
  :group 'moinmoin)
(defface moinmoin-bold '((t (:weight bold)))
  "Face name to use for bold text in moinmoin"
  :group 'moinmoin)
(defface moinmoin-italic '((t (:slant italic)))
  "Face name to use for italic text in moinmoin"
  :group 'moinmoin)
(defface moinmoin-underline '((t (:underline t)))
  "Face name to use for underlined text in moinmoin"
  :group 'moinmoin)
(defface moinmoin-stroke '((t (:strike-through t)))
  "Face name to use for stroked text in moinmoin"
  :group 'moinmoin)
(defface moinmoin-subscript '((t (:height 0.8)))
  "Face name to use for subscripts in moinmoin"
  :group 'moinmoin)
(defface moinmoin-superscript '((t (:height 0.8)))
  "Face name to use for superscripts in moinmoin"
  :group 'moinmoin)


;;; Font lock setup
(defconst moinmoin-url-prefix
  "\\(?:http\\|https\\|ftp\\|nntp\\|news\\|mailto\\|telnet\\|wiki\\|file\\|irc\\|attachment\\|inline\\|drawing\\)"
  "Bracketed regexp matching URL prefixes in moinmoin")
(defconst moinmoin-url-punctuation
  "]\"'}|:,.)?!"                        ; start with ]
  "Punctuation in URLs of moinmoin")
(defconst moinmoin-pi-re
  "^#\\(?:format\\|refresh\\|redirect\\|deprecated\\|pragma\\|form\\|acl\\|language\\).*$"
  "Regexp for processing instructions in moinmoin")
(defconst moinmoin-smiley-re
  "\\(?:<:(\\|X-(\\|:)\\|:-))\\|:(\\|/!\\\\\\|{X}\\|{OK}\\|B-)\\|{2}\\|>:>\\|;-)\\|<!>\\|:o\\||-)\\|;)\\||)\\|(!)\\|:-(\\|:-)\\|{o}\\|:D\\|(./)\\|B)\\|{*}\\|:\\\|:-?\\|{i}\\|{3}\\|{1}\\|:)\\)"
  "Regexp for smileys in moinmoin")

(defun moinmoin-formatted-code-matcher (bound)
  "Search for formatted code
This is supposed to be bug-to-bug compatible with moinmoin-1.5.2"
  (catch 'match
    (while (< (point) bound)
      (unless (search-forward-regexp "{{{" bound t)
        (throw 'match nil))
      (let ((start-brace-begin (match-beginning 0))
            (start-brace-end (match-end 0))
            pi-begin pi-end
            code-begin code-end
            end-brace-begin end-brace-end)
        (unless (get-text-property start-brace-begin 'moinmoin-verbatim)
          (goto-char start-brace-end)
          (if (looking-at "#!.*\n")
              (setq pi-begin (match-beginning 0)
                    pi-end (match-end 0)
                    code-begin (match-end 0))
            (setq code-begin start-brace-end))
          (goto-char code-begin)
          (let ((not-first-line))
            (while (looking-at
                    "\\(?:##.*\\|`.*?`\\|{{{.*?}}}\\|.\\)*?\\(?:\\(}}}\\)\\|\n\\)")
              (goto-char (match-end 0))
              (when (and (match-beginning 1)    ; non-paired `}}}'
                         not-first-line)
                (setq code-end (match-beginning 1)
                      end-brace-begin (match-beginning 1)
                      end-brace-end (match-end 1))
                (set-match-data (list start-brace-begin end-brace-end
                                      start-brace-begin start-brace-end
                                      pi-begin          pi-end
                                      code-begin        code-end
                                      end-brace-begin   end-brace-end))
                (throw 'match t))
              (setq not-first-line t)))
          (throw 'match nil))))))


(defun moinmoin-bracketed-url-matcher (bound)
  "Search for bracketed URLs"
  (catch 'match
    (while (< (point) bound)
      (unless (search-forward-regexp
               (concat "\\(\\[\\)"
                       "\\(" moinmoin-url-prefix "?:.*?\\) +"
                       "\\(.*?\\)?"
                       "\\(\\]\\)") bound t)
        (throw 'match nil))
      (unless (get-text-property (match-beginning 0) 'moinmoin-verbatim)
        (throw 'match t)))))

(defun moinmoin-setup-font-lock ()
  (setq font-lock-beginning-of-syntax-function '(lambda () (goto-char 1)))
  (setq font-lock-multiline t)
  (make-local-variable 'font-lock-extra-managed-props)
  (add-to-list 'font-lock-extra-managed-props 'moinmoin-verbatim) ; Not Comment Start
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords nil `(
    ("\\(?:^\\|[^|]\\)\\(\\(?:||\\)+\\)\\(<.*?>\\)?"
     (1 'moinmoin-table-separator)
     (2 'moinmoin-table-pi t t))
    ("'''\\(.*?\\)'''" (1 'moinmoin-bold prepend))
    ("\\(?:[^']\\|^\\)''\\([^'].*?[^']\\)''[^']" (1 'moinmoin-italic prepend))
    ("\\(__\\)\\(.*?\\)\\(__\\)"
     (1 'moinmoin-ss t) (2 'moinmoin-underline prepend) (3 'moinmoin-ss t))
    ("\\(--(\\)\\(.*?\\)\\()--\\)"
     (1 'moinmoin-ss t) (2 'moinmoin-stroke prepend) (3 'moinmoin-ss t))
    ("\\(,,\\)\\(.*?\\)\\(,,\\)"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-subscript 'display '(raise -0.3)) prepend)
     (3 'moinmoin-ss t))
    ("\\(\\^\\)\\(.*?\\)\\(\\^\\)"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-superscript 'display '(raise 0.3)) prepend)
     (3 'moinmoin-ss t))
    ("''+" (0 'moinmoin-ss t))
    ("^  +\\(?:\\*\\|.*?::\\|\\(?:[0-9]+\\|[aAiI]\\)\\.\\) "
     (0 'moinmoin-item-2))
    ("^ \\(?:\\*\\|.*?::\\|\\(?:[0-9]+\\|[aAiI]\\)\\.\\) "
     (0 'moinmoin-item))
    ("^\\( +\\)\\(.*\\)"
     (1 'moinmoin-blockquote-indent)
     (2 'moinmoin-blockquote-text))
    ("&[A-Za-z0-9]*;" (0 'moinmoin-entity prepend))
    (,(concat "\\(?:^\\| \\)\\(" moinmoin-smiley-re "\\)[[:space:]]")
     (1 'moinmoin-smiley t))
    ("\\(?:^\\|[^A-Za-z!]\\)\\(\\(?:\\.\\./\\)?/?[A-Z][a-z]+[A-Z][a-z][A-Za-z]*\\(?:/[A-Z][a-z]+[A-Z][a-z][A-Za-z]*\\)?\\)"
     (1 'moinmoin-wiki-link t))
    (,(concat "[A-Z][A-Za-z]+:"
              "[^[:space:]'\":<|]"
              "\\(?:[^" moinmoin-url-punctuation "[:space:]]\\|"
                   "[" moinmoin-url-punctuation "]"
                    "[^" moinmoin-url-punctuation "[:space:]]\\)+")
     (0 'moinmoin-inter-wiki-link t))
    ("\\(\\[\"\\)\\(?:\\(.*?:.*\\)\\|\\(.*?\\)\\)\\(\"\\]\\)"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-inter-wiki-link 'moinmoin-verbatim t) t t)
     (3 (list 'face 'moinmoin-wiki-link 'moinmoin-verbatim t) t t)
     (4 'moinmoin-ss t))
    ;;ur'%(url_guard)s(%(url)s)\:([^\s\<%(punct)s]|([%(punct)s][^\s\<%(punct)s]))+'
    (,(concat "\\<" moinmoin-url-prefix ":"
              "\\(?:[^" moinmoin-url-punctuation "<[:space:]]\\|"
                   "[" moinmoin-url-punctuation "]"
                    "[^" moinmoin-url-punctuation "<[:space:]]\\)+")
     (0 'moinmoin-url t))
    ("[A-Za-z0-9_+-]+@[A-Za-z0-9_-]+\\(?:\\.[A-Za-z0-9_-]+\\)+"
     (0 'moinmoin-email t))
    ("-\\{4,\\}" (0 'moinmoin-rule t))
    ;; macros
    ("\\(\\[\\[\\)\\([A-Za-z0-9]+\\)\\(?:\\((\\)\\(.*?\\)\\()\\)\\)?\\(\\]\\]\\)"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-macro-name 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss t t)
     (4 (list 'face 'moinmoin-macro-parameters 'moinmoin-verbatim t) t t)
     (5 'moinmoin-ss t t)
     (6 'moinmoin-ss t))
    ("\\(\\[\\[Anchor(\\)\\(.*?\\)\\()\\]\\]\\)"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-anchor 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss t))
    ("\\(\\[#\\)\\(.*?\\) +\\(.*?\\)?\\(\\]\\)"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-anchor-ref-id 'moinmoin-verbatim t) t)
     (3 (list 'face 'moinmoin-anchor-ref-title 'moinmoin-verbatim t) t t)
     (4 (list 'face 'moinmoin-ss  'moinmoin-verbatim t) t))
    ;; typewriter (tt) overrides the above
    ("\\({{{\\)\\(.*?\\)\\(}}}\\)"
     (1 (list 'face 'moinmoin-ss 'moinmoin-verbatim t) keep)
     (2 (list 'face 'moinmoin-tt 'moinmoin-verbatim t) t)
     (3 (list 'face 'moinmoin-ss 'moinmoin-verbatim t) keep))
    ("\\(`\\)\\(.*?\\)\\(`\\)"
     (1 'moinmoin-ss keep)   ; cannot match `{{{' thus no need for ncs
     (2 (list 'face 'moinmoin-tt 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss keep))
    ;; headers overrides tt and biu
    ("^\\(= \\)\\(.*\\)\\( =\\)$"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-h1 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss t))
    ("^\\(== \\)\\(.*\\)\\( ==\\)$"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-h2 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss t))
    ("^\\(=== \\)\\(.*\\)\\( ===\\)$"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-h3 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss t))
    ("^\\(==== \\)\\(.*\\)\\( ====\\)$"
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-h4 'moinmoin-verbatim t) t)
     (3 'moinmoin-ss t))
    ("^\\(=====\\(=*\\) \\)\\(.*\\)\\( \\2=====\\)$" ; 5th and all others
     (1 'moinmoin-ss t)
     (3 (list 'face 'moinmoin-h5 'moinmoin-verbatim t) t)
     (4 'moinmoin-ss t))
    ;; comments and pi
    ("^##.*$"
     (0 (list 'face 'moinmoin-comment 'moinmoin-verbatim t) t))
    (,moinmoin-pi-re
     (0 (list 'face 'moinmoin-pi 'moinmoin-verbatim t) t))
    (moinmoin-bracketed-url-matcher
     (1 'moinmoin-ss t)
     (2 (list 'face 'moinmoin-url       'moinmoin-verbatim t) t)
     (3 (list 'face 'moinmoin-url-title 'moinmoin-verbatim t) t t)
     (4 (list 'face 'moinmoin-ss        'moinmoin-verbatim t) t))
    (moinmoin-formatted-code-matcher
     (1 (list 'face 'moinmoin-code-braces 'moinmoin-verbatim t) t)
     (2 (list 'face 'moinmoin-pi          'moinmoin-verbatim t) t t)
     (3 (list 'face 'moinmoin-code        'moinmoin-verbatim t) t)
     (4 (list 'face 'moinmoin-code-braces 'moinmoin-verbatim t) t))
    ) 'set))


;;; Automagic typing helpers
(defun moinmoin-insert-quote ()
  "Convert double quote to HTML entity (&ldquo; or &rdquo;)"
  (interactive)
  (cond
   ((or (get-text-property (point) 'moinmoin-verbatim)
        (looking-back "\\[\\|= *\\|^\\(?:[^\"]*\"[^\"]*\"\\)*[^\"]*\"[^\"]*"))
    (insert "\""))
   ((or (looking-back "[[:space:]]") (eq (point) 1))
    (insert "&ldquo;"))
   (t (insert "&rdquo;"))))

(defun moinmoin-insert-dash ()
  "Insert different types of dashes.
`----' and `-' stays unchanged
`--' is converted to `&ndash;'
`---' is converted to `&mdash;'
`)--' is not changed'
`<-' is converted to `&larr;'"
  (interactive)
  (cond
   ((get-text-property (point) 'moinmoin-verbatim)
    (insert "-"))
   ((looking-back "---\\|&mdash;")
    (replace-match "----" t t))
   ((looking-back "--\\|&ndash;")
    (replace-match "&mdash;" t t))
   ((looking-back ")-")
    (replace-match ")--" t t))
   ((looking-back "<")
    (replace-match "&larr;" t t))
   ((looking-back "-")
    (replace-match "&ndash;" t t))
   (t (insert "-"))))

(defun moinmoin-insert-lparen ()
  "&ndash;( to --("
  (interactive)
  (cond
   ((get-text-property (point) 'moinmoin-verbatim)
    (insert "("))
   ((looking-back "&ndash;")
    (replace-match "--(" t t))
   (t (insert "("))))

(defun moinmoin-insert-greater-than ()
  "-> to &rarr;"
  (interactive)
  (cond
   ((get-text-property (point) 'moinmoin-verbatim)
    (insert ">"))
   ((looking-back "-")
    (replace-match "&rarr;" t t))
   (t (insert ">"))))

(defun moinmoin-insert-item ()
  "Start new item or row
If current line starts with item prefix insert newline and the same
prefix in front of the rest of line.
If it is table then finish the line and add the new one"
  (interactive)
  (when (bolp) (backward-char))
  (cond
   ((looking-back "^\\( *||\\).*")      ; in a table
    (let ((prefix (match-string 1)))
      (end-of-line)
      (looking-back "[^|]\\(|*\\)")
      (replace-match "||" t t nil 1)
      (insert "\n" prefix)))
   ((looking-back "^\\( +\\(?:\\* \\|\\(?:[0-9]+\\|[aAiI]\\)\\. \\|\\)\\).*")
    (let ((prefix (match-string 1)))
     (insert "\n")
      (insert prefix)))))


;;; Header manipulation helpers
(defun moinmoin-is-header ()
  "Is point in a header line?
If yes the title is in \\1"
  (save-excursion
    (beginning-of-line)
    (looking-at "=+ \\(.*\\) =+$")))

(defun moinmoin-increase-header-level ()
  "Add `=' in the beginning and end of current line"
  (save-excursion
    (beginning-of-line) (insert "=")
    (end-of-line) (insert "=")))

(defun moinmoin-decrease-header-level ()
  "Decrease level of the header
Warns if point in a top-level header or not in a header"
  (save-excursion
    (beginning-of-line)
    (if (looking-at "=\\(=+ .* =+\\)=$")
        (replace-match "\\1" t)
      (warn "cannot decrease level"))
    ))

(defun moinmoin-change-header-level (increment)
  "Increase or decrease level of header according to INCREMENT"
  (interactive "r")
  (if (eq increment 1)
      (moinmoin-increase-header-level)
    (moinmoin-decrease-header-level)))

(defun moinmoin-change-header-levels-in-region (increment start end)
  "Increase or decrease level of all headers in the region according to INCREMENT"
  (interactive "p\nr")
  (save-excursion
    (goto-char start)
    (while (progn
      (when (moinmoin-is-header)
        (moinmoin-change-header-level increment))
      (and (zerop (forward-line)) (< (point) end))))
  ))

(defun moinmoin-insert-equal (increment)
  "Do-what-I-mean with header level or insert `='.
With active region increase or decrease level of all headers in region.
On an empty line starts new header.
On a header line increase or decrease level.
Otherwise just insert `='"
  (interactive "p")
  (if mark-active
      (progn
        (moinmoin-change-header-levels-in-region
          increment (region-beginning) (region-end))
        (setq deactivate-mark nil))
    (cond
     ((looking-at "^$")
      (insert "=  =")
      (backward-char 2))
     ((moinmoin-is-header)
      (moinmoin-change-header-level increment))
     (t (insert "=")))))


;;; Anchor insertion and navigation
(defun moinmoin-list-anchors ()
  "List anchors in the current buffer"
  (let ((lst))
    (save-excursion
      (goto-char 1)
      (while (search-forward-regexp "\\[\\[Anchor(\\([^])\n]*\\))\\]\\]" nil t)
        (setq lst (cons (match-string-no-properties 1) lst))))
    lst))

(defvar moinmoin-anchor-history "Minibuffer history of anchors")
(defun moinmoin-completing-read-anchor (prompt &optional require-match)
  "Read non-empty anchor using complition of all the anchors in the current buffer"
  (let ((anchor
         (completing-read "Anchor: " (moinmoin-list-anchors)
                          nil require-match
                          nil moinmoin-anchor-history)))
    (when (string-equal anchor "")
      (error "Empty anchor"))
    anchor))

(defun moinmoin-insert-number-sign (&optional no-title)
  (interactive "P")
  "After `[' insert a reference to anchor inputed using completition,
If no argument given also insert the title of the section which contains
the anchor."
  (cond
   ((and (looking-back "\\[")
         (not (get-text-property (point) 'moinmoin-verbatim)))
    (let* ((anchor (moinmoin-completing-read-anchor "Anchor: "))
           (title
            (unless no-title
              (condition-case nil
                  (moinmoin-get-anchored-header anchor)
                (error (message "No header for anchor %s" anchor) nil)))))
      (insert "#" anchor " ")
      (if title
          (insert title "]")
        (insert "]") (backward-char))))
   (t                                   ; not after "["
    (insert "#"))))

(defun moinmoin-insert-anchor ()
  "Insert anchor (using the title if point is in a header)"
  (interactive)
  (cond
   ((moinmoin-is-header)
    (let ((anchor-name
           (replace-regexp-in-string   ; remove non-anchor symbols
            "[^A-Za-z0-9]+" "-"
            (match-string-no-properties 1) t t)))
      (forward-line)
      (insert "[[Anchor(" anchor-name ")]]\n")))
   (t
    (insert "[[Anchor()]]") (backward-char 3))))

(defun moinmoin-anchor-read-or-ask (&optional prompt)
  "DWIM to get anchor: read [#... ] on current line (before point) or ask user."
  (cond
   ((and
     (search-backward
      "[#" (save-excursion (beginning-of-line) (point)) t)
     (looking-at "\\[#\\([^]\n ]+\\)"))
    (match-string-no-properties 1))
   (t
    (moinmoin-completing-read-anchor (or prompt "Anchor: ") t))))

(defun moinmoin-goto-anchor (&optional anchor)
  "Go to anchor
If ANCHOR is nil (e.g., if called interactively) read reference
to it from the current line or ask user.  So if there is an
anchor on the current line but you want to jump to something
different go to the beginning of the line first."
  (interactive)
  (unless anchor
    (setq anchor (moinmoin-anchor-read-or-ask)))
  (goto-char 1)
  (search-forward (concat "[[Anchor(" anchor ")]]")))

(defun moinmoin-get-anchored-header (anchor)
  "Get title of the section which contains ANCHOR"
  (save-excursion
    (moinmoin-goto-anchor anchor)
    (search-backward-regexp "^=+ \\(.*\\) =+$" nil)
    (match-string-no-properties 1)))

(defun moinmoin-insert-anchored-header (&optional to-kill-ring)
  "Insert title of the section which contains anchor, with prefix save it to kill-ring"
  (interactive "P")
  (let ((header (moinmoin-get-anchored-header nil)))
    (if to-kill-ring
        (kill-new header)
      (insert header))))


;;; Setup
(define-derived-mode moinmoin-mode outline-mode "MoinMoin"
  "Set major mode for editing MoinMoin pages"
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "=+")
  (make-local-variable 'outline-heading-end-regexp)
  (setq outline-heading-end-regexp " =+\n")
  (local-set-key "\"" 'moinmoin-insert-quote)
  (local-set-key "-" 'moinmoin-insert-dash)
  (local-set-key "(" 'moinmoin-insert-lparen)
  (local-set-key ">" 'moinmoin-insert-greater-than)
  (local-set-key "=" 'moinmoin-insert-equal)
  (local-set-key "#" 'moinmoin-insert-number-sign)
  (local-set-key (kbd "M-RET") 'moinmoin-insert-item)
  (local-set-key (kbd "C-c a") 'moinmoin-insert-anchor)
  (local-set-key (kbd "C-c g") 'moinmoin-goto-anchor)
  (toggle-truncate-lines 0)                    ; do not truncate
  (screen-lines-mode 1)                        ; use screen lines
  (moinmoin-setup-font-lock)
  (abbrev-mode 1)
  (set-fill-column 65000)
  (auto-fill-mode 0))

(add-to-list 'auto-mode-alist '("\\.wiki$" . moinmoin-mode))

(provide 'moinmoin-mode)
