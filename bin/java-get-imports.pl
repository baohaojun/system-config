#!/usr/bin/perl

use strict;
use String::ShellQuote;
use Getopt::Long;
my $code_dir = $ENV{PWD};
my %files_package;
my $do_hierarchy;
my $verbose;
GetOptions(
    "d=s" => \$code_dir,
    "h!"  => \$do_hierarchy,
    "v!"  => \$verbose,
    );

open(my $debug, ">", glob("~/.logs/java-get-imports.log"))
    or die "Can not open debug log file ~/.logs/java-get-imports.log";
sub debug(@) {
    print $debug "@_\n";
}

my $id_re = qr(\b[a-zA-Z_][a-zA-Z0-9_]*\b);
my $qualified_re = qr($id_re(?:\.$id_re)*\b);
my $connect_re = qr((?: |(?:\[\])+));
my %super_classes;



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
my %import_qualifieds;
my %wild_import_qualifieds;
my %refs;
my %defs;
my %import_simples;
my %simple_qualified_map;

sub define_it($)
{
    $defs{$_[0]} = 1;
}

sub import_it($)
{
    my $q = $_[0];
    my $s = $q;
    $s =~ s/.*\.//;
    $import_qualifieds{$q} = 1;
    $import_simples{$s} = 1;
    $simple_qualified_map{$s} = $q;
}

my $working_file;
if (@ARGV != 1) {
    die "Usage: $0 JAVA_FILE";
}

$working_file = $ARGV[0];
chomp($working_file = qx(readlink -f $working_file));

while (<>) {
    if (m/^package ($qualified_re);/) { #package
	$package = $1;
    } elsif (m/^import (?:static )?($qualified_re);/) { #import
	import_it($1);
    } elsif (m/^import (?:static )?($qualified_re)(?:\.\*);/) { #import
	$wild_import_qualifieds{$1} = 1;
    } elsif (m/(?:class|interface) ($id_re)(.*)\{/) { #class|interface
	define_it($1);
	my $class = $1;
	my $ext = $2;
	$super_classes{$class} = {} unless exists $super_classes{$class};
	while ($ext =~ m/($qualified_re)/g) {
	    next if $keywords{$1};
	    $refs{$1} = 1;
	    $super_classes{$class}{$1} = 1;	    
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
	import_it("$package.$1");
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

sub find_import_for($)
{
    my $def = $_[0];
    return 0 unless $def;
    open(my $pipe, "-|", "grep-gtags -e $def -d $code_dir -t 'class|interface' -s")
	or die "can not open grep-gtags";
    
    while (<$pipe>) {
	m/^(.*?):.*?<(.*)>/ or next;
	
	my ($file, $tag) = ($1, $2);
	my $package = $files_package{$file};
	unless ($package) {
	    chomp($package = qx(java-get-package $code_dir/$file));
	    $files_package{$file} = $package;
	}
	print "import $package.$tag\n";
    }
}

get_default_packages($package);
get_default_packages("java.lang");
for my $wild (keys %wild_import_qualifieds) {
    get_wildcards($wild);
}


if ($do_hierarchy) {
    my $all_defs = qx(grep-imenu -e '.*' -f $working_file);
    my %q_super_classes;
    for my $class (sort keys %super_classes) {
	debug "handling $class";
	while ($all_defs =~ m/(?:class|interface): <(.*?\b$class)>/g) {
	    my $q_class = "$package.$1";
	    if (keys $super_classes{$class}) {
		for my $super (keys $super_classes{$class}) {
		    my $prefix = $super;
		    $prefix =~ s/\..*//;
		    if ($simple_qualified_map{$prefix}) {
			$q_super_classes{$q_class} = {} unless exists $q_super_classes{$q_class};
			my $q_super = $simple_qualified_map{$prefix} . substr($super, length($prefix));
			$q_super_classes{$q_class}{$q_super} = 1;
		    }
		}
	    }
	}
    }

    for my $q_class (sort keys %q_super_classes) {
	print "$q_class\n";
	for my $q_super (sort keys %{$q_super_classes{$q_class}}) {
	    print "  => $q_super\n";
	}
	print "\n";
    }
}

for my $ref (keys %refs) {
    my $ref_save = $ref;
    $ref =~ s/\..*//;
    unless ($keywords{$ref} or $defs{$ref}) {
	if ($import_simples{$ref}) {
	    $import_simples{$ref} ++;
	} elsif ($ref =~ m/^[A-Z]/) {
	    find_import_for($ref_save) unless $do_hierarchy;
	}
    }
}

if ($verbose) {
    for my $import (keys %import_simples) {
	debug "import $simple_qualified_map{$import} not used" if $import_simples{$import} == 1;
    }
}
