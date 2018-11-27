DOWNLOADING

This module is available at Google Code:

  http://code.google.com/p/confluence-el/

INSTALLATION

You must set confluence-url in your .emacs file before using the functions
in this module.  It's best to place confluence.el and confluence-edit.el
and xml-rpc.el on your load path; often ~/.emacs.d or ~/elisp.

Some examples:

  ;; loading xml-rpc.el may not be necessary, it depends on your
  ;; installed version of Emacs, it was necessary on 22.1.1.
  ;; Both xml-rpc.el and confluence.el should be on your load-path.

  (require 'confluence)
  (setq confluence-url "http://intranet/confluence/rpc/xmlrpc")

There are various additional customzation options.  These can be explored
by executing M-x customize-group and then entering the group "confluence"
(or by browsing the file below).

USING CONFLUENCE MODE

To open a page, M-x confluence-get-page and enter the path to the
page, for example, to open a page in your home space: ~username/Tasks

It is often convienient to bind this to a global key \C-xwf in your .emacs file:

   (global-set-key "\C-xwf" 'confluence-get-page)

Once you have opened a page, made changes, simply saving the page
("\C-x\C-s") will push the changes back to the wiki.

To view the changes in your page versus what is in the wiki, type
\C-xw=, or run M-x confluence-ediff-current-page.

Also, if you want keybindings for confluence-mode, you can put the
following in your .emacs file:

(add-hook 'confluence-edit-mode-hook
  (local-set-key "\C-xw" confluence-prefix-map)
  (local-set-key "\M-j" 'confluence-newline-and-indent)
  (local-set-key "\M-;" 'confluence-list-indent-dwim))

CONFLUENCE 4.0+

In confluence version 4.0, Atlassian decided to change the wiki format.
They did away with the "wiki" format and changed the internal document
format to xml.  This makes editing confluence pages via emacs much less
enjoyable.  However, as of version 1.6 of this library, it is possible.
The somewhat simplistic confluence-xml-mode (simplistic compared to
confluence-mode) is an extension of nxml-mode.  It adds some minor
font-lock support, but otherwise leaves you with standard xml editing
support.  Confluence still has built in support for translating wiki format
pages to xml format pages, however, the reverse translation is more
problematic.

Leveraging the excellent work of Graham Hannington
(http://www.amnet.net.au/~ghannington/confluence/readme.html), this package
provides a "basic" converter from xml to wiki format, however it can be
"lossy" depending on what advanced features a page contains.  A confluence
xml page can be converted to the wiki format using M-x
confluence-toggle-page-content-type.  This page can be saved as wiki format
(allowing confluence to do the reverse conversion on save) or can be
converted back to xml format (using the same command) and then saved
(allowing you to check the final content).  Note that the conversion from
xml to wiki format requires the external "xsltproc" program, which is
available on most unices and cygwin.

For the truly brave, you can set the custom variable
confluence-xml-convert-to-wiki-on-load to t in order to automatically
convert xml content to wiki content on page load.

LONGLINES

  http://www.emacswiki.org/emacs-en/LongLines

Confluence uses a wiki-markup that treats linebreask as <p> HTML
tags.  Since this is the case, it is very common for paragraphs in
the Confluence markup to wrap around your buffer multiple times.
The LongLines mode allows those lines to be viewed within Emacs
with 'soft' linebreaks - which are inserted automatically, or via
M-q.  This makes it much more pleasant to work with large
paragraphs of text in the Confluence markup without introducing
unwanted paragraphs.

See below for more advice on using LongLines and confluence-mode.


EXAMPLE .emacs CONFIGURATION

(require 'confluence)

note, all customization must be in *one* custom-set-variables block
(custom-set-variables
 ;; ... other custimization

 ;; confluence customization
 '(confluence-url "http://intranet/confluence/rpc/xmlrpc")
 '(confluence-default-space-alist (list (cons confluence-url "your-default-space-name"))))


confluence editing support (with longlines mode)

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook (lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))


     (add-hook 'ediff-cleanup-hook
               (lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))
