package Emacs::PDE;

use strict; 
use warnings;

use Carp;
use version; our $VERSION = qv("0.2.16");

1;

=head1 NAME

Emacs::PDE - Perl Development Environment in emacs

=head1 SYNOPSIS

Put this to you .emacs:

     (add-to-list 'load-path "/path/to/pde/")
     (load "pde-load")

=head1 DESCRIPTION

This is not a perl module. It is a collection of emacs lisp extension
and configuration for perl development environment.

=head1 EXTENSIONS

Here is a list of extensions included in PDE.

=over

=item  template-simple

A simple text template process library. Support for expand template
when create new file or empty file according to the suffix of current
file name.

=item compile-dwim

Automatic generate compile-command according to the file name or
major-mode.

=item help-dwim

Integrate various finding document command to one command. Useful for
bind a global key to search help information.

=item perldoc

Perldoc using woman. Provide a tree view of all Pods.

=item perlapi

Search document in perlapi.

=item perltidy

Call perltidy when editing.

=item imenu-tree

Tree view show imenu for the source file.

=item tags-tree

Tree view show tags for the sources.

=item inf-perl

Interactive perl shell.

=item perldb-ui

A simple perl debugger user interface.

=back

=head1 BUGS

Tell me if you found one, or you want to discuss anything related,
please drop me a note at I<wenbinye@gmail.com>

=head1 SEE ALSO

More document for PDE:

L<lisp/doc/pde/index.html>  - PDE User Guide

L<lisp/doc/QuickStartEn.html>  - A tutor of emacs configuration for perl


More emacs lisp libraries or Perl Modules:

L<Sepia>  - Simple Emacs-Perl Interface

perlnow.el  - Emacs extensions to speed development of perl code.

L<Devel::PerlySense> - IntelliSense for Perl

L<PPI> - Parse, Analyze and Manipulate Perl (without perl)

L<http://www.emacswiki.org/cgi-bin/wiki/PerlLanguage> - Discussion in EmacsWiki

=cut
