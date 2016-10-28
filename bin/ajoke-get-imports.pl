#!/usr/bin/perl

use strict;
use String::ShellQuote;
use Getopt::Long;

use BhjJava;
sub debug(@) {
    print $debug "@_\n";
}
my $verbose;
my $resolve;
GetOptions(
    "v!"  => \$verbose,
    "r=s" => \$resolve,
    );

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
my %var_type_map;

sub is_defined($)
{
    return $defs{$_[0]};
}

sub is_keyword($)
{
    return $keywords{$_[0]};
}

sub is_keyword_or_defined($)
{
    return (is_keyword($_[0]) or is_defined($_[0]));
}

sub type_it($$)
{
    debug "type it: $_[0] $_[1]";
    if ($_[1] eq "instanceof") {
        return;
    }
    $defs{$_[0]} = 1;
    $refs{$_[1]} = 1;
    $var_type_map{$_[0]} = {} unless exists $var_type_map{$_[0]};
    $var_type_map{$_[0]}{$_[1]} = 1;
}

sub define_it($)
{
    debug "define_it $_[0]";
    $defs{$_[0]} = 1;
}

sub import_it($)
{
    my $q = $_[0];
    debug "import_it: $q";
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
chomp($working_file = qx(java-flatten-cache $working_file));
@ARGV = ($working_file);

while (<>) {
    debug "got $_";

    while (s/\@($qualified_re)//g) {
        $refs{$1} = 1;
    }
    if (m/^package ($qualified_re);/) { #package
        $package = $1;
    } elsif (m/^import (?:static )?($qualified_re);/) { #import
        import_it($1);
    } elsif (m/^import (?:static )?($qualified_re)(?:\.\*);/) { #import
        $wild_import_qualifieds{$1} = 1;
    } elsif (m/(?:class|interface|enum(?!-constant)) ($id_re)(.*)\{/) { #class|interface|enum
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
    } elsif (m/($qualified_re)$connect_re($id_re)\((.*)\)((?:throws $id_re(?:,$id_re)*)?)\{/) { #method definition
        debug "got method: $1 $2";
        if ($4) {
            debug "throws: $4";
            my $throws = $4;
            $throws =~ s/^throws //;

            while ($throws =~ m/($qualified_re)/g) {
                $refs{$1} = 1;
            }
        }
        type_it($2, $1);

        my $params = $3;
        $params =~ s/$modifier_re //g;
        while ($params =~ m/($qualified_re)(?:$connect_re|<$id_re>)($id_re)/g) {
            type_it($2, $1);
        }
    } elsif (m/($qualified_re)$connect_re($id_re)\)/) { #arguments
        s/$modifier_re //g;
        my $line = $_;
        while ($line =~ m/($qualified_re)$connect_re($id_re)(?=,|\))/g) {
            debug "got $1 $2";
            type_it($2, $1);
        }
    } elsif (m/($qualified_re)(?:$connect_re|<$id_re(?:,$id_re)*>)($id_re)(,|=.*|;)/) { #var definition
        type_it($2, $1);
        my $assign = $3;
        while ($assign =~ m/($qualified_re)/g) {
            $refs{$1} = 1;
        }
    } elsif (m/for\(($qualified_re) ($id_re):/) { #for (FILE ss: files[])
        type_it($2, $1);
    } else {
        debug "not matched: $_";
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
    debug "getting imports for $package";
    return unless $package;
    my $package_q = shell_quote("$package\..*");
    my $command = "beatags -e $package_q -t 'class|interface' | pn 1 | grep -P '\\Q$package\\E\\.\\w+\$'";
    debug "exec to import: $command";
    open(my $pipe, "-|", $command)
        or die "can not open grep-gtags";

    while (<$pipe>) {
        chomp;
        import_it("$_");
    }
    close($pipe);
}

sub get_wildcards($)
{
    my $import = $_[0];
    get_default_packages($import);
    $import =~ s/.*\.//;
    my %def_files;
    open(my $pipe, "-|", "grep-gtags -e $import -s")
        or die "can not open grep-gtags";

    while (<$pipe>) {
        m#(.*?):# or next;
        $def_files{$1} = 1;
    }
    close $pipe;

    $import = shell_quote($import);
    for my $file (keys %def_files) {
        if (not -e "$code_dir/$file") {
            debug("file not exist: $code_dir/$file");
            next;
        }
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
    if (length($def) == 1) {
        print STDERR "$def: generics type parameter?\n";
        return 0;
    }
    return 0 unless $def;
    if ($def =~ m/\./) {
        our %import_quoted_map;
        $def =~ s/\..*//;
        if ($def eq "R") {
            debug "do not import for R";
            return 0;
        }
        return 0 if exists $import_quoted_map{$def};
        $import_quoted_map{$def} = 1;
    }
    debug "beatags -e $def -t 'class|interface|enum(?!-constant)' -p '\\.java|\\.aidl|\\.jar'";
    open(my $pipe, "-|", "beatags -e $def -t 'class|interface|enum(?!-constant)' -p '\\.java|\\.aidl|\\.jar'")
        or die "can not open beatags";

    my @imports;
    while (<$pipe>) {
        m/^(.*?)\s+\S+\s+\d+\s+(\S+)/ or next;

        my ($tag, $file) = ($1, $2);
        push @imports, "$tag";
    }

    unless ($resolve) {
        our %printed_imports;
        my $deleted = 0;

        for (0..@imports) {
            if ($printed_imports{$imports[$_]}) {
                delete $imports[$_];
                $deleted = 1;
            } else {
                $printed_imports{$imports[$_]} = 1;
            }
        }

        if (@imports == 1) {
            print "import @imports\n";
        } elsif (@imports) {
            print "import-multi @imports\n";
        } else {
            print "can not import $def\n" unless $deleted;
        }
    }
}

get_default_packages($package);
get_default_packages("java.lang");
for my $wild (keys %wild_import_qualifieds) {
    get_wildcards($wild);
}


for my $ref (keys %refs) {
    my $ref_save = $ref;
    $ref =~ s/\..*//;
    unless (is_keyword_or_defined($ref)) {
        debug "need import: $ref";
        if ($import_simples{$ref}) {
            $import_simples{$ref} ++;
        } elsif ($ref =~ m/^[A-Z]/) {
            debug "find_import_for $ref_save\n";
            find_import_for($ref_save);
        }
    }
}

if ($verbose) {
    for my $import (keys %import_simples) {
        debug "import $simple_qualified_map{$import} not used" if $import_simples{$import} == 1;
    }
}

sub prefix($)
{
    my ($s) = @_;
    $s =~ s/\..*//;
    return $s;
}

if ($resolve) {
    my $postfix = "";
    if ($resolve =~ m/\./) {
        $postfix = $resolve;
        $postfix =~ s/.*?(?=\.)//;
        debug "postfix is $postfix";
        $resolve =~ s/\..*//;
    }
    if ($simple_qualified_map{$resolve}) {
        print $simple_qualified_map{$resolve} . "$postfix";
    } elsif ($var_type_map{$resolve}) {
        for my $type (keys %{$var_type_map{$resolve}}) {
            my $prefix = prefix($type);
            if ($simple_qualified_map{$prefix}) {
                print $simple_qualified_map{$prefix} . substr($type, length($prefix)) . "$postfix\n";
            } else {
                print "$type$postfix\n";
            }
        }
    } else {
        $resolve = shell_quote($resolve);
        system("ajoke-get-type.pl $resolve");
    }
}
