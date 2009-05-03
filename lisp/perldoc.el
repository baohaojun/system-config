;;; perldoc.el --- 

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: perldoc.el,v 0.0 2007/08/22 19:49:58 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perldoc)
;;   (perldoc-recache-everyday)    ; this is optional
;;   (add-hook 'cperl-mode-hook
;;             (lambda () (help-dwim-active-type 'perldoc)))

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'woman)
(require 'pde-vars)
(require 'tree-mode)
(require 'windata)

(defgroup perldoc nil
  "Search document using perldoc"
  :group 'tools
  :group 'pde)

(defcustom perldoc-cache-el
  (expand-file-name "tools/perldoc-cache.el" pde-load-path)
  "*Cache file name for build `perldoc-obarray'."
  :type 'file
  :group 'perldoc)

(defcustom perldoc-cache-pl
  (expand-file-name "tools/perldoc-cache.pl" pde-load-path)
  "*Perl script to generate `perldoc-cache-el'"
  :type 'file
  :group 'perldoc)

(defcustom perldoc-pod2man "pod2man"
  "*Program name of pod2man"
  :type 'string
  :group 'perldoc)

(defcustom perldoc-buffer-format "*WoMan Perldoc %S*"
  "*Buffer name for perldoc buffer."
  :type 'string
  :group 'perldoc)

(defvar perldoc-module-chars "0-9a-zA-Z_:"
  "*Characters may occur in perl module name.")

(defcustom perldoc-auto-encoding t
  "*Non-nil means handler encoding by emacs."
  :type 'boolean
  :group 'perldoc)

;;;###autoload 
(defcustom perldoc-pod-encoding-list
  '(("perltw" . big5))
  "*Encoding for pods"
  :type '(alist :key-type string :value-type coding-system)
  :group 'perldoc)

(defvar perldoc-obarray nil
  "All perl modules name and functions.
Functions in obarray have a value, can be predicated by `boundp'.
Modules are only interned. Pragmas are listed in
`perldoc-pragmas'. And core document can be recognize by prefix
\"perl\". Note that \"open\" and \"sort\" are known as function
and pragma, the pragma is add \".pod\" to distinguish from function.")

(defcustom perldoc-tree-buffer "*Perldoc*"
  "*Buffer name for `perldoc-tree'"
  :type 'string
  :group 'perldoc)

(defcustom perldoc-tree-theme "default"
  "*Theme of tree-widget."
  :type 'string
  :group 'perldoc)

(defcustom perldoc-tree-windata '(frame left 0.3 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :type 'sexp
  :group 'perldoc)

(defvar perldoc-pragmas
  '("attributes" "attrs" "autouse" "base" "bigint" "bignum" "bigrat"
    "blib" "bytes" "charnames" "constant" "diagnostics" "encoding"
    "fields" "filetest" "if" "integer" "less" "lib" "locale" "open"
    "ops" "overload" "perlpod" "perlpodspec" "re" "sigtrap" "sort"
    "strict" "subs" "threads" "threads::shared" "utf8" "vars" "vmsish"
    "warnings" "warnings::register")
  "For progma node in the tree.")

(defvar perldoc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tree-mode-map)
    (define-key map "f" 'perldoc-find-module-ap)
    (define-key map "m" 'perldoc-goto-module)
    (define-key map "\C-o" 'perldoc-find-module-other-window)
    map)
  "*Local map use in perldoc tree.")

(defface perldoc-directory-face
  '((t :foreground "Blue1" :inherit widget-button-face))
  "Face for non-module node"
  :group 'perldoc)

(defun perldoc-build-obarray (&optional re-cache)
  "Build perldoc obarray from cache.
With prefix arguments force cache update."
  (interactive "P")
  (if (and (null re-cache)
           (file-exists-p perldoc-cache-el))
      (load perldoc-cache-el)
    (message "This may take some time, please wait...")
    ;; full name for perl to locate file
    (setq perldoc-cache-pl (expand-file-name perldoc-cache-pl)
          perldoc-cache-el (expand-file-name perldoc-cache-el))
    (unless (file-exists-p perldoc-cache-pl)
      (perldoc-create-pl))
    (set-process-sentinel
     (start-process "perldoc-build" nil pde-perl-program
                    perldoc-cache-pl perldoc-cache-el)
     (lambda (proc event)
       (if (zerop (process-exit-status proc))
           (progn
             (message "Create perldoc cache successfully!")
             (load perldoc-cache-el)
             (if (get-buffer perldoc-tree-buffer)
                 (with-current-buffer perldoc-tree-buffer
                   (tree-mode-reflesh))))
         (error "Create perldoc cache failed! %s" event))))))

(defun perldoc-recache-everyday (&optional days)
  "If the cache file is expired DAYS, force caches update."
  (unless (and (file-exists-p perldoc-cache-el)
               (< (time-to-seconds
                   (time-subtract
                    (current-time)
                    (nth 5 (file-attributes perldoc-cache-el))))
                  (* 60 60 24 (or days 1))))
    (perldoc-build-obarray t)
    ;; indicate cache update
    t))

(defun perldoc-init (&optional check)
  (unless perldoc-obarray
    (if check
        (if (get-process "perldoc-build")
            (error "The cache is building, please wait")
          (or perldoc-obarray
              (error "Something is wrong. Please check the cache file `perldoc-cache-el' and build cache manually.")))
      (perldoc-build-obarray))))

(defun perldoc-symbol-type (sym)
  (cond ((boundp sym) 'function)
        ((string-match "^perl" (symbol-name sym)) 'core-document)
        ;; ((member (replace-regexp-in-string "\.pod$" (symbol-name sym))
        ;;          perldoc-pragmas) 'pragma)
        (t 'module)))

;;;###autoload 
(defun perldoc (symbol &optional modulep)
  "Display perldoc using woman.
The SYMBOL can be a module name or a function. If the module and
function is the same, add \".pod\" for the module name. For example,
\"open.pod\" for the progma open and \"open\" for function open."
  (interactive
   (list
    (intern (perldoc-read-module "Perldoc" t) perldoc-obarray)))
  ;; if sure it is module and the symbol can be a function, add suffix ".pod"
  (and modulep
       (eq (perldoc-symbol-type symbol) 'function)
       (setq symbol (intern-soft (format "%s.pod" symbol) perldoc-obarray)))
  (let ((buf (format perldoc-buffer-format symbol))
        (name (symbol-name symbol))
        (old-coding default-process-coding-system)
        (default-process-coding-system default-process-coding-system)
        (process-coding-system-alist process-coding-system-alist)
        encoding)
    (if (buffer-live-p (get-buffer buf))
        (display-buffer buf)
      (when symbol
        (with-current-buffer (get-buffer-create buf)
          (when perldoc-auto-encoding
            (setq default-process-coding-system
                  (cons 'raw-text (cdr default-process-coding-system))
                  process-coding-system-alist nil))
          (if (eq (perldoc-symbol-type symbol) 'function) ; function
              (progn
                (call-process pde-perldoc-program nil t nil "-u" "-f" name)
                (goto-char (point-min))
                (insert (format "=head1 %s\n\n=over 4\n\n" name))
                (goto-char (point-max))
                (insert (format "\n=back\n\n=cut")))
            (if (string-match "\\.pod$" name)
                (setq name (replace-match "" nil nil name)))
            (call-process pde-perldoc-program  nil t nil "-u" name))
          (goto-char (point-min))
          (when (re-search-forward "^=encoding\\s-+\\(\\S-+\\)" nil t)
            (setq encoding (intern-soft (match-string 1)))
            ;; pod2man can't understand =encoding
            (delete-region (match-beginning 0) (match-end 0)))
          ;; detect encoding by emacs
          (when perldoc-auto-encoding
            (let ((detect-coding (detect-coding-region (point-min) (min (point-max) 10000) t)))
              (when (or (null encoding) (not (coding-system-p encoding)))
                (setq encoding
                      ;; as far as I known, perltw can't detect encoding
                      (or (cdr (assoc name perldoc-pod-encoding-list))
                          detect-coding)))
              ;; choose the eol-type from detected encoding
              (setq encoding (merge-coding-systems encoding detect-coding)))
            (decode-coding-region (point-min) (point-max) encoding)
            (setq default-process-coding-system old-coding))
          (call-process-region (point-min) (point-max)
                               perldoc-pod2man t t nil "-n" name)
          (condition-case nil
              (woman-process-buffer)
            (error
             ;; if error, show the document as text
             (erase-buffer)
             (apply 'call-process
                    `(,pde-perldoc-program nil t nil ,@(if (eq (perldoc-symbol-type symbol) 'function) "-f") ,name))))
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun perldoc-module-ap (&optional modulep)
  "Perl module at point."
  (perldoc-init t)
  (let* ((chars perldoc-module-chars)
         (mod (save-excursion
                (buffer-substring
                 (progn (skip-chars-backward chars) (point))
                 (progn (skip-chars-forward chars) (point)))))
         (def (intern-soft mod perldoc-obarray))
         case-fold-search)
    ;; if it is a Package::sub, remove the sub
    (unless def
      (setq def (replace-regexp-in-string "::[_a-z][^:]*$" "" mod))
      (setq def (intern-soft def perldoc-obarray))
      (if def
          (setq mod (symbol-name def))
        (if (and (string-match "::" mod)
                 (yes-or-no-p (concat mod " Seem like a Perl module. Add it temporarily (use C-u M-x perldoc-build-obarray to update)")))
            (setq def (intern mod perldoc-obarray)))))
    (and def
         (or (not modulep)
             (eq (perldoc-symbol-type def) 'module))
         mod)))

(defsubst perldoc-locate-module (module)
  "Find location of the module"
  (locate-file 
   (replace-regexp-in-string "::" "/" module)
   pde-perl-inc '(".pm" ".pod")))

(defun perldoc-read-module (prompt &optional require-match init modulep)
  "Read perl module.
When a module name at point, the default input is the module at point.
Don't add \": \" in PROMPT."
  (let ((default (perldoc-module-ap modulep)))
    (completing-read (if default
                         (format "%s (default %s): " prompt default)
                       (concat prompt ": "))
                     perldoc-obarray
                     (if modulep
                         (lambda (m)
                           ;; not a function
                           (eq (perldoc-symbol-type m) 'module)))
                     require-match init nil default)))

(defun perldoc-find-module (&optional module other-window)
  "Find the file of perl module."
  (interactive
   (list (perldoc-read-module "Find perl module" t)
         current-prefix-arg))
  (funcall (if other-window 'find-file-other-window 'find-file)
           (perldoc-locate-module module)))

(defun perldoc-find-module-ap (&optional other-window)
  "Find perl module at point."
  (interactive "P")
  (let ((def (perldoc-module-ap)))
    (if def
        (perldoc-find-module def other-window)
      (message "No perl module at point"))))

(defun perldoc-find-module-other-window ()
  "Find perl module other window."
  (interactive)
  (perldoc-find-module-ap t))

(defun perldoc-create-pl ()
  (with-temp-buffer
    (insert
     "#! /usr/bin/perl -w\n"
     "use File::Find;\n"
     "use Data::Dumper qw(Dumper);\n"
     "use Text::Wrap qw(wrap);\n"
     "if ( @ARGV ) {\n"
     "    my $file = shift;\n"
     "    open(STDOUT, \">\", $file) or die \"Can't create $file: $!\";\n"
     "}\n"
     "my $fn = build_function();\n"
     "print <<'EL';\n"
     "(setq perldoc-obarray (make-vector 1519 nil))\n"
     ";; Functions\n"
     "(mapc (lambda (func)\n"
     "         (set (intern func perldoc-obarray) t))\n"
     "'(\n"
     "EL\n"
     "my $i = 1;\n"
     "print wrap('', '', join(' ', map {qq(\"$_\")} sort keys %$fn )), \"))\\n\\n\";\n"
     "\n"
     "print <<'EL';\n"
     ";; Modules\n"
     "(mapc (lambda (mod)\n"
     "         (intern mod perldoc-obarray))\n"
     "'(\n"
     "EL\n"
     "my $mod = build_modules();\n"
     "print wrap('', '', join(' ', map {qq(\"$_) . (exists $fn->{$_} ? \".pod\" : \"\") . '\"'} sort keys %$mod )), \"))\\n\";\n"
     "\n"
     "sub build_modules {\n"
     "    my %mod;\n"
     "    for my $dir ( @INC ) {\n"
     "        next if $dir eq '.';\n"
     "        next unless -d $dir;\n"
     "        my $len = length($dir)+1;\n"
     "        find( { wanted => sub {\n"
     "                    if ( -f $_ && /\\.(pm|pod)$/i ) {\n"
     "                        my $mod = substr($File::Find::name, $len);\n"
     "                        $mod =~ s#^[pP]od/(?=a2p|perl)##;\n"
     "                        $mod =~ s/.(pm|pod)$//;\n"
     "                        $mod =~ s#/#::#g;\n"
     "                        $mod{$mod}++;\n"
     "                    }\n"
     "                },\n"
     "                follow => 1\n"
     "            }, $dir);\n"
     "    }\n"
     "    return \\%mod;\n"
     "}\n"
     "\n"
     "sub build_function {\n"
     "    chomp(my $file = `perldoc -l perlfunc`);\n"
     "    my %fn;\n"
     "    open(FH, $file) or die \"Can't open file $file: $!\";\n"
     "    while ( <FH> ) {\n"
     "        last if /^=head2 Alphabetical/;\n"
     "    }\n"
     "    while ( <FH> ) {\n"
     "        last if /^=over/;\n"
     "    }\n"
     "    my $stat = 1;\n"
     "    while ( <FH> ) {\n"
     "        if ( /^=item/ ) {\n"
     "            if ( $stat ) {\n"
     "                my $fn = (split /\\s+/, $_)[1];\n"
     "                $fn =~ s#/.*$##;  #  y///, m// and so on\n"
     "                $fn =~ s/\\(.*$//; # chomp(, chop(\n"
     "                $fn{$fn}++;\n"
     "            }\n"
     "        } elsif ( /^=over/ ) {\n"
     "            $stat = 0;\n"
     "        } elsif ( /^=back/ ) {\n"
     "            $stat = 1;\n"
     "        }\n"
     "    }\n"
     "    map { $fn{'-'.$_}++ } qw/A B C M O R S T W X b c d e f g k l o p r s t u w x z/;\n"
     "    return \\%fn;\n"
     "}\n")
    (write-region (point-min) (point-max) perldoc-cache-pl)))

;;{{{  perldoc-tree
(define-derived-mode perldoc-mode tree-mode "Perldoc"
  "List perl module using tree-widget.

\\{perldoc-mode-map}"
  (tree-widget-set-theme perldoc-tree-theme))

;;;###autoload 
(defun perldoc-tree ()
  "Create pod tree."
  (interactive)
  (perldoc-init t)
  (unless (get-buffer perldoc-tree-buffer)
    (with-current-buffer (get-buffer-create perldoc-tree-buffer)
      (perldoc-mode)
      (widget-create (perldoc-tree-widget))
      (widget-setup)
      (goto-char (point-min))))
  (select-window (apply 'windata-display-buffer
                        (get-buffer perldoc-tree-buffer)
                        perldoc-tree-windata)))

(define-widget 'perldoc-directory 'push-button
  "A perldoc diretory node"
  :button-face 'perldoc-directory-face
  :notify 'tree-mode-toggle-expand-node
  :format "%[%t%]\n")

(defun perldoc-goto-module (module)
  "Move to the node of the MODULE. Expand tree when need."
  (interactive (list (completing-read "Go to module: " perldoc-obarray
                                      nil t)))
  (let ((sym (intern-soft module perldoc-obarray))
        case-fold-search wid cat)
    (when sym
      (cond ((eq (perldoc-symbol-type sym) 'function)
             (setq cat "Function"))
            ((eq (perldoc-symbol-type sym) 'core-document)
             (setq cat "Core document"))
            (t (setq module (replace-regexp-in-string "\\.pod$" "" module))
               (if (member module perldoc-pragmas)
                   (setq cat "Pragam")
                 (setq cat "Modules")
                 (let (path str)
                   (dolist (name (split-string module "::"))
                     (push (setq str (concat (if str (concat str "::")) name)) path))
                   (setq module (nreverse path))))))
      (setq wid (tree-mode-find-node (tree-mode-tree-ap)
                                     (if (listp module)
                                         (cons cat module)
                                       (list cat module))))
      (goto-char (widget-get (car wid) :from)))))

(defun perldoc-tree-widget ()
  `(tree-widget
    :node (perldoc-directory :tag "Perldoc")
    :open t
    ,@(mapcar
       (lambda (cat)
         `(tree-widget
           :node (perldoc-directory :tag ,(car cat))
           :dynargs ,(cdr cat)))
       '(("Function" . perldoc-function-expand)
         ("Core document" . perldoc-coredoc-expand)
         ("Pragams" . perldoc-pragam-expand)
         ("Modules" . perldoc-modules-expand)))))

(defun perldoc-function-expand (tree)
  (or (widget-get tree :args)
      (mapcar (lambda (f)
                (perldoc-item f t))
              (sort (all-completions "" perldoc-obarray
                                     (lambda (sym)
                                       (and sym (eq (perldoc-symbol-type  sym) 'function))))
                    'string<))))

(defun perldoc-coredoc-expand (tree)
  (or (widget-get tree :args)
      (mapcar 'perldoc-item 
              (sort
               (let (completion-ignore-case)
                 (all-completions "perl" perldoc-obarray))
               'string<))))

(defun perldoc-pragam-expand (tree)
  (or (widget-get tree :args)
      (mapcar 'perldoc-item perldoc-pragmas)))

(defun perldoc-modules-expand (tree)
  (or (widget-get tree :args)
      (let ((hash (make-hash-table :test 'equal))
            case-fold-search
            modules)
        (mapatoms
         (lambda (sym)
           (and (eq (perldoc-symbol-type sym) 'module)
                (not (string-match "\.pod$" (symbol-name sym)))
                (not (member (symbol-name sym) perldoc-pragmas))
                (puthash (car (split-string (symbol-name sym) "::")) t hash)))
         perldoc-obarray)
        (maphash (lambda (key val) (push key modules)) hash)
        (mapcar 'perldoc-module-item (sort modules 'string<)))))

(defun perldoc-has-submodp (module)
  "Test whether the MODULE has submodule."
  (not (null (try-completion (concat module "::") perldoc-obarray))))

(defun perldoc-sub-module-expand (tree)
  (or (widget-get tree :args)
      (let ((name (widget-get (tree-widget-node tree) :tag))
            module)
        (mapc (lambda (mod)
                (if (string-match (concat (regexp-quote name) "::[^:]+")
                                  mod)
                    (add-to-list 'module (match-string 0 mod))))
              (all-completions name perldoc-obarray))
        (mapcar
         'perldoc-module-item
         (sort module
               (lambda (m1 m2)
                 (let ((p1 (perldoc-has-submodp m1))
                       (p2 (perldoc-has-submodp m2)))
                   (if (eq p1 p2)
                       (string< m1 m2)
                     p1))))))))

(defun perldoc-module-item (mod)
  (if (perldoc-has-submodp mod)
      `(tree-widget
        :node ,(if (intern-soft mod perldoc-obarray)
                   `(push-button
                     :tag ,mod
                     :format "%[%t%]\n"
                     :notify perldoc-select)
                 `(perldoc-directory :tag ,mod))
        :dynargs perldoc-sub-module-expand)
    `(push-button
      :tag ,mod
      :format "%[%t%]\n"
      :notify perldoc-select)))

(defun perldoc-item (name &optional funcp)
  `(push-button
    :tag ,name
    :format "%[%t%]\n"
    :funtion ,funcp
    :notify perldoc-select))

(defun perldoc-select (node &rest ignore)
  "Show the manpage of the module in the NODE."
  (let ((name (widget-get node :tag))
        (funcp (widget-get node :funtion))
        sym)
    (setq sym (intern-soft name perldoc-obarray))
    (if (not sym)
        (message "No perldoc found for %s" name)
      (perldoc sym (not funcp)))))
;;}}}

(perldoc-init)
(provide 'perldoc)
;;; perldoc.el ends here
