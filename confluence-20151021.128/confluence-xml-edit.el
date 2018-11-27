;;; confluence-xml-edit.el --- Emacs mode for editing confluence 4.x+ (xml) content buffers

;; Copyright (C) 2013 James Ahlborn

;; Author: James Ahlborn <james@boomi.com>
;; Keywords: confluence, wiki
;; Version: 1.8-beta
;; Package-Requires:: 
;; EmacsWiki: ConfluenceMode

;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Generally, confluence-xml-edit-mode will not be used directly.  However, there
;; may be times when a buffer is being edited using confluence xml syntax but
;; without interacting directly with a live confluence instance.  In this
;; case, it may be desirable to use this mode directly.
;;
;; INSTALLATION
;;
;; This mode can be used standalone (without confuence.el and its
;; dependencies) by simply adding confluence-xml-edit.el to your load path.
;; In general, you can follow the instructions in confluence.el for
;; configuring this mode, just substitute "confluence-xml-edit-mode"
;; everywhere you see "confluence-mode".
;; 

;;; Code:

(require 'confluence-edit)
;;(require 'nxml-mode)

(defun confluence-xml-reformat ()
  "Reformats xml to make it readable (respects current selection)."
  (interactive)
  (save-excursion
    (let ((beg (point-min))
          (end (point-max)))
      (if (and mark-active transient-mark-mode)
          (progn
            (setq beg (min (point) (mark)))
            (setq end (max (point) (mark))))
        (widen))
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (re-search-forward ">\\s-*<" end t)
        (replace-match ">\n<" t t))
      (goto-char beg)
      (indent-region beg end nil)
      (set-marker end nil))))

(defconst confluence-xml-font-lock-keywords-1
  (list
   '(nxml-fontify-matcher)

   ;; '("{\\([^{}:\n]+:?\\)[^{}\n]*}"
   ;;   (1 'font-lock-constant-face))
  
   ;; '("{[^{}\n]+[:|]title=\\([^}|\n]+\\)[^{}\n]*}"
   ;;   (1 'bold append))
  
   ;; '("{warning\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){warning}"
   ;;   (1 'font-lock-warning-face prepend))
   ;; '("{note\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){note}"
   ;;   (1 'font-lock-minor-warning-face prepend))
   ;; '("{info\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){info}"
   ;;   (1 'font-lock-doc-face prepend))
   ;; '("{tip\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){tip}"
   ;;   (1 'font-lock-comment-face prepend))
  
   ;; ;; bold
   ;; '("[^[:word:]\\*#\n][*]\\([^*\n]+\\)[*]\\W"
   ;;   (1 'bold))
   '("<b>\\([^<]*?\\)</b>"
     (1 'bold prepend))
   '("<strong>\\([^<]*?\\)</strong>"
     (1 'bold prepend))
   
   ;; ;; code
   ;; '("{{\\([^}\n]+\\)}}"
   ;;   (1 'confluence-code-face t))
   
   ;; ;; italics/emphasised
   ;; '("[^[:word:]\\]_\\([^_\n]+\\)_\\W"
   ;;   (1 'italic prepend))
   ;; '("[^[:word:]\\][?]\\{2\\}\\([^?\n]+\\)[?]\\{2\\}\\W"
   ;;   (1 'italic prepend))
   '("<em>\\([^<]*?\\)</em>"
     (1 'italic prepend))
   '("<i>\\([^<]*?\\)</i>"
     (1 'italic prepend))

   ;; ;; underline
   ;; '("[^[:word:]\\][+]\\([^+\n]+\\)[+]\\W"
   ;;   (1 'underline prepend))
   '("<u>\\([^<]*?\\)</u>"
     (1 'underline prepend))

   ;; ;; strike-through
   ;; '("[^[:word:]\\][-]\\([^-\n]+\\)[-]\\W"
   ;;   (1 '(:strike-through t) prepend))

   ;; headings
   '("<h1>\\(\\(.\\|[\n]\\)*?>\\)?\\([^<]*?\\)</h1>"
     (3 '(bold underline) prepend))
   '("<h2>\\(\\(.\\|[\n]\\)*?>\\)?\\([^<]*?\\)</h2>"
     (3 '(bold italic underline) prepend))
   '("<h3>\\(\\(.\\|[\n]\\)*?>\\)?\\([^<]*?\\)</h3>"
     (3 '(italic underline) prepend))
   '("<h[4-9]>\\(\\(.\\|[\n]\\)*?>\\)?\\([^<]*?\\)</h[4-9]>"
     (3 'underline prepend))
   '("<th>\\([^<]*?\\)</th>"
     (1 'bold prepend))

   ;; ;; bullet points
   ;; '("^\\([*#]+\\)\\s-"
   ;;   (1 'font-lock-constant-face))
   
   ;; ;; links
   ;; '("\\(\\[\\)\\([^]|\n]*\\)[|]\\([^]\n]+\\)\\(\\]\\)"
   ;;   (1 'font-lock-constant-face)
   ;;   (2 'font-lock-string-face)
   ;;   (3 'underline)
   ;;   (4 'font-lock-constant-face))
   ;; '("\\(\\[\\)\\([^]|\n]+\\)\\(\\]\\)"
   ;;   (1 'font-lock-constant-face)
   ;;   (2 '(font-lock-string-face underline))
   ;;   (3 'font-lock-constant-face))
   ;; '("{anchor:\\([^{}\n]+\\)}"
   ;;   (1 'font-lock-string-face))

   ;; ;; images, embedded content
   ;; '("\\([!]\\)\\([^|\n]+\\)[|]\\(?:[^!\n]*\\)\\([!]\\)"
   ;;   (1 'font-lock-constant-face)
   ;;   (2 confluence-embedded-link-face)
   ;;   (3 'font-lock-constant-face))
   ;; '("\\([!]\\)\\([^!|\n]+\\)\\([!]\\)"
   ;;   (1 'font-lock-constant-face)
   ;;   (2 confluence-embedded-link-face)
   ;;   (3 'font-lock-constant-face))
   
   ;; ;; tables
   ;; '("[|]\\{2\\}\\([^|\n]+\\)"
   ;;   (1 'bold))
   ;; '("\\([|]\\{1,2\\}\\)"
   ;;   (1 'font-lock-constant-face))
   )
  
  "Basic level highlighting for confluence xml mode.")

(defconst confluence-xml-font-lock-keywords-2
  (append confluence-xml-font-lock-keywords-1
          (list
  
           ;; ;; code/preformatted blocks
           ;; '("{noformat\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){noformat}"
           ;;   (1 'confluence-code-face t))
           ;; '("{code\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){code}"
           ;;   (1 'confluence-code-face t))
           '("<ac:plain-text-body>\\(\\(.\\|[\r\n]\\)*?\\)</ac:plain-text-body>"
             (1 'confluence-code-face t))

           ;; ;; panels
           ;; '("{panel\\(?:[:][^}\n]*\\)?}\\(?:\\s-*[\r]?[\n]\\)?\\(\\(.\\|[\n]\\)*?\\){panel}"
           ;;   (1 'confluence-panel-face append))
           ))
  "Gaudy level highlighting for confluence xml mode.")

(defvar confluence-xml-font-lock-keywords confluence-xml-font-lock-keywords-1
  "Default expressions to highlight in Confluence xml modes.")


(define-derived-mode confluence-xml-edit-mode nxml-mode "ConfluenceXmlEdit"
  "Set major mode for editing Confluence xml page buffers."
  (setq font-lock-defaults
        '((confluence-xml-font-lock-keywords confluence-xml-font-lock-keywords-1
                                             confluence-xml-font-lock-keywords-2)
          t nil nil nil 
          (font-lock-extend-after-change-region-function
           . nxml-extend-after-change-region)
          (font-lock-extend-region-functions . (nxml-extend-region))
          (jit-lock-contextually . t)
          (font-lock-unfontify-region-function . nxml-unfontify-region)
          (font-lock-multiline . t)))
)

(provide 'confluence-xml-edit)
;;; confluence-xml-edit.el ends here
