;;; pde-abbv.el --- Abbrevs for perl

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 30 Dec 2007
;; Version: 0.01
;; Keywords: abbrev, convenience

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

;; These abbrevs are available:
;;  newx  -  Create subroutine new for Object module.
;;  dumpx -  expand for Data::Dumper::Dumper.
;;  usetk -  Use Tk
;;  exportx - Write Exporter tags
;;  opend -  expand for build-in function `opendir'. snippet available
;;  openf -  expand for build-in function `open'. snippet available
;;  fromto - expand for Encode::from_to, only snippet available
;;  usegtk - Use Gtk2
;;  useenc - Use Encode
;;  usedump - Use Data::Dumper

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'pde-abbv)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tempo-x)

(defcustom pde-abbv-use-snippet t
  "*Non-nil means use snippet instead of standard tempo."
  :type 'boolean
  :group 'pde)

(defvar tempo-perl-tags nil
  "Tempo tags for cperl-mode")

(tempo-define-template
 "perl-new"
 '("sub new {"
   n> "my $_class = shift;"
   n> "my $class = ref $_class || $_class;"
   n> "my $self = {};"
   n> "bless $self, $class;"
   n> p
   n> "return $self;"
   n "}" >)
 "newx"
 "Create subroutine new for Object module."
 'tempo-perl-tags)

(tempo-define-template
 "perl-dump"
 '("print Dumper(" p "), \"\\n\";")
 "dumpx"
 "expand for Data::Dumper::Dumper."
 'tempo-perl-tags)

(tempo-define-template
 "perl-usetk"
 '("use Tk;"
   n> "my $mw = MainWindow->new;"
   n> p 
   n "MainLoop;" >)
 "usetk"
 "Use Tk"
 'tempo-perl-tags)

(tempo-define-template
 "perl-export"
 '("require Exporter;"
   n> "our @ISA = qw(Exporter);"
   n> "our %EXPORT_TAGS = ( 'all' => [ qw( " p ") ] );"
   n> "our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );"
   n "our @EXPORT = qw(  );" >)
 "exportx"
 "Write Exporter tags"
 'tempo-perl-tags)

;; abbrevs not using snippet
(when (null pde-abbv-use-snippet)
  (tempo-define-template
   "perl-opendir"
   '("opendir(" (pi "Dir handle: " "DIR") ", "
     (p "Directory: " dir t)
     (let ((dir (tempo-lookup-named 'dir)))
       (if (string-match "^[$]" dir)
           dir
         (concat "\"" dir "\"")))
     ") or die \"Can't open directory " (s dir) ": $!\";"
     n>)
   "opend"
   "expand for build-in function `opendir'"
   'tempo-perl-tags)

  (tempo-define-template
   "perl-open"
   '("open(" (pi "File handle: " "FH") ", "
     (p "File: " file t)
     (let ((file (tempo-lookup-named 'file)) mode)
       (when (string-match "^\\([><+]+\\)\\s-*" file)
         (setq mode (match-string 1 file)
               file (substring file (match-end 0)))
         (tempo-save-named 'file file))
       (unless (string-match "^[$]" file)
         (setq file (concat "\"" file "\"")))
       (tempo-save-named 'mode "open")
       (if mode
           (if (string-match ">" mode)
               (tempo-save-named 'mode "create"))
         (setq mode "<"))
       (format "\"%s\", %s" mode file))
     ") or die \"Can't " (s mode) " file " (s file) ": $!\";"
     n>)
   "openf"
   "expand for build-in function `open'"
   'tempo-perl-tags))

;; the same abbrevs using snippet
(when pde-abbv-use-snippet
  (tempo-define-template
   "perl-opendir"
   '((snippet
      "opendir(" (S DIR) ", " (S dir "$dir")
      ") or die \"Can't open directory "
      (F (dir)
         (replace-regexp-in-string "['\"]?\\(.*\\)['\"]" "\\1" dir))
      ": $!\";"
      n>))
   "opend"
   "expand for build-in function `opendir'"
   'tempo-perl-tags)

  (tempo-define-template
   "perl-open"
   '((snippet
      "open("
      (S FH "my $fh" t) ", \"" (S op "<") "\", "
      (S file "$file")
      ") or die \"Can't "
      (F (op)
         (if (string-match ">" op)
             "create" "open"))
      " file "
      (F (file)
         (replace-regexp-in-string "['\"]?\\(.*\\)['\"]" "\\1" file))
      ": $!\";"
      n>))
   "openf"
   "expand for build-in function `open'"
   'tempo-perl-tags)
  (tempo-define-template
   "perl-from2"
   '((snippet
      "from_to(" (S oct "$octets") ", \"" (S from_enc) "\", \"" (S to_enc) "\");"
      n>))
   "fromto"
   "expand for Encode::from_to."
   'tempo-perl-tags)
   )

(defun pde-abbv-no-blank ()
  "Expand abbrev without insert blank."
  (if (string= (this-command-keys) " ") t))
(put 'pde-abbv-no-blank 'no-self-insert t)

;; standard abbrev that don't need tempo
(define-abbrev-table 'cperl-mode-abbrev-table
  '(("usegtk" "use Gtk2 '-init';
use Glib qw(TRUE FALSE);

my $window = Gtk2::Window->new('toplevel');
$window->signal_connect('delete_event' => sub { Gtk2->main_quit; });"
     pde-abbv-no-blank 0)
    ("useenc" "use Encode qw(encode decode from_to);"
     pde-abbv-no-blank 0)
    ("usedump" "use Data::Dumper qw(Dumper);"
     pde-abbv-no-blank 0)
    ("useopt" "use Getopt::Long;
GetOptions();" pde-abbv-no-blank 0)))
  
(tempo-define-template
 "filevar"
 '((pi "Prefix: " comment-start prefix) "Local Variables: " "***\n"
   (R (s prefix)
      (pi ("Variable: " ("mode" "coding")) nil var)
      (& var (delete-region recursion-start (point)))
      ": " (p "Value: ")
      " ***\n")
   (s prefix) "End: ***\n")
 "filevarx")

(provide 'pde-abbv)
;;; pde-abbv.el ends here
