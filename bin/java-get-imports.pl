#!/usr/bin/perl

use strict;
use String::ShellQuote;
use Getopt::Long;
my $code_dir = $ENV{PWD};
GetOptions(
    "d=s" => \$code_dir,
    );

open(my $debug, ">", glob("~/.logs/java-get-imports.log"))
    or die "Can not open debug log file ~/.logs/java-get-imports.log";
sub debug(@) {
    print $debug "@_\n";
}



my $id_re = qr(\b[a-zA-Z_][a-zA-Z0-9_]*\b);
my $qualified_re = qr($id_re(?:\.$id_re)*\b);
my $connect_re = qr((?: |(?:\[\])+));



my @keywords = ("abstract", "assert", "boolean", "break", "byte",
	      "case", "catch", "char", "class", "const", "continue", "default",
	      "double", "else", "enum", "extends", "false", "final", "finally",
	      "float", "for", "goto", "implements", "import", "instanceof", "int",
	      "interface", "long", "native", "new", "null", "package", "private",
	      "protected", "public", "return", "short", "static", "strictfp",
	      "super", "switch", "synchronized", "this", "throw", "throws",
	      "transient", "true", "try", "void", "volatile", "while"
    );

my $keywords = join('|', @keywords);
my $keywords_re = qr($keywords);
my %keywords;

for my $key (@keywords) {
    $keywords{$key} = 1;
}

sub match_args($) 
{
    return 0;
}

my $package;
my %imports;
my %wild_imports;
my %refs;
my %defs;
my %import_defs;

sub define_it($)
{
    $defs{$_[0]} = 1;
}

while (<>) {
    if (m/^package ($qualified_re);/) { #package
	$package = $1;
    } elsif (m/^import (?:static )?($qualified_re);/) { #import
	$imports{$1} = 1;
	my $import = $1;
	$import =~ s/.*\.//;
	$import_defs{$import} = 1;
    } elsif (m/^import (?:static )?($qualified_re)(?:\.\*);/) { #import
	$wild_imports{$1} = 1;
    } elsif (m/(?:class|interface) ($id_re)(.*)\{/) { #class|interface
	define_it($1);
	my $ext = $2;
	while ($ext =~ m/($qualified_re)/g) {
	    $refs{$1} = 1 unless $keywords{$1};
	}
    } elsif (m/new ($qualified_re)\((.*)\)\{/) { #anonymous class definition
	$refs{$1} = 1;
	match_args($2);
    } elsif (m/($qualified_re)$connect_re($id_re)\((.*)\)\{/) { #method definition
	define_it($2);
	$refs{$1} = 1 unless $keywords{$1};

	my $params = $3;
	$params =~ s/\b$keywords_re\b//g;
	while ($params =~ m/($qualified_re)$connect_re($id_re)/g) {
	    define_it($2);
	    $refs{$1} = 1;
	}
    } elsif (m/($qualified_re)$connect_re($id_re)(,|=.*)?;/) { #var definition
	define_it($2);
	$refs{$1} = 1;
	my $assign = $3;
	while ($assign =~ m/($qualified_re)/g) {
	    $refs{$1} = 1;
	}
    } else {
	while (m/($qualified_re)=/g) {
	    define_it($1);
	}

	while (m/($qualified_re)/g) {
	    $refs{$1} = 1;
	}
    }
}

for my $def (keys %defs) {
}



sub get_default_packages($)
{
    my $package = $_[0];
    return unless $package;
    open(my $pipe, "-|", "grep-gtags -e $package -d $code_dir -t package -s -c")
	or die "can not open grep-gtags";

    while (<$pipe>) {
	m#/([^/]+)\.(?:java|aidl):.*# or next;
	define_it($1);
    }
    close($pipe);
}

sub get_wildcards($)
{
    my $import = $_[0];
    get_default_packages($import);
    $import =~ s/.*\.//;
    my %def_files;
    open(my $pipe, "-|", "grep-gtags -e $import -d $code_dir -s")
	or die "can not open grep-gtags";
    
    while (<$pipe>) {
	m#(.*?):# or next;
	$def_files{$1} = 1;
    }
    close $pipe;

    $import = shell_quote($import);
    for my $file (keys %def_files) {
	open(my $pipe, "-|", "global-ctags $import..* $code_dir/$file")
	    or die "can not open global-ctags";
	while (<$pipe>) {
	    my ($def) = split;
	    $def =~ s/.*\.//;
	    define_it($def);
	}
	close $pipe;
    }	
}

get_default_packages($package);
get_default_packages("java.lang");
for my $wild (keys %wild_imports) {
    get_wildcards($wild);
}

for my $ref (keys %refs) {
    my $ref_save = $ref;
    $ref =~ s/\..*//;
    unless ($keywords{$ref} or $defs{$ref}) {
	if ($import_defs{$ref}) {
	    $import_defs{$ref} ++;
	} elsif ($ref =~ m/^[A-Z]/) {
	    print "refed: $ref_save"
	}
    }
}

for my $import (keys %import_defs) {
    debug "import $import not used" if $import_defs{$import} == 1;
}
