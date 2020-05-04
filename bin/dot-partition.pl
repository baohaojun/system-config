#!/usr/bin/env perl
use strict;
use English;

my %partition;
my %reverse_partition;

sub debug(@) {
    print STDERR "@_\n";
}

use Getopt::Long;
my $max_level = 5;
my $start_def;
my $max_reverse_level = 0;
my $max_called_by = 20000;
GetOptions(
    "m=n" => \$max_level,
    "s=s" => \$start_def,
    "r=n" => \$max_reverse_level,
    "b=n" => \$max_called_by,
    );

unless ($start_def) {
    $max_reverse_level = $max_level = 20000;
}

while (<>) {
    chomp;
    if (m/ -> /) {
	my $source = $PREMATCH;
	my $target = $POSTMATCH;

	$partition{$source} = {} unless exists $partition{$source};
	$partition{$source}{$target} = "$source -> $target";

	$reverse_partition{$target} = {} unless exists $reverse_partition{$target};
	$reverse_partition{$target}{$source} = "$source -> $target";
    }
}

my %visited;
my @parts = keys %partition;

my $level = 0;
my $part_file;
my $filename;
my %printed;
sub do_part($) {
    my ($source) = @_;
    debug " " x $level . "enter $source, level is $level" unless $visited{$source};
    return if ($level >= $max_level);
    return if exists $visited{$source};
    $visited{$source} = 1;

    if ($level++ == 0) {
	$filename = "$source.dot";
	$filename =~ s/"//g;
	open ($part_file, ">", "$filename")
	    or die "Can not open $filename";
	print $part_file "digraph {\nrankdir=LR;\ngraph [ ratio=.5 ];\n";
    }

    do {
	foreach my $target (keys %{$partition{$source}}) {
	    print $part_file "$partition{$source}{$target}\n" unless $printed{$partition{$source}{$target}};
	    $printed{$partition{$source}{$target}} = 1;
	    do_part($target);
	    if ($level <= $max_reverse_level and keys(%{$reverse_partition{$source}}) < $max_called_by) {
		foreach my $reverse (keys %{$reverse_partition{$source}}) {
		    print $part_file "$reverse_partition{$source}{$reverse}\n" unless $printed{$reverse_partition{$source}{$reverse}};
		    $printed{$reverse_partition{$source}{$reverse}} = 1;
		}
	    }
	}
    } while (0);
    $level--;
    debug " " x $level . "exit $source, level is $level";
    if ($level == 0) {
	print $part_file "}\n";
	close($part_file);
	system("(dot -Tpdf -o $filename.pdf $filename; acroread $filename.pdf) >/dev/null 2>&1&");
    }
	    }

if ($start_def) {

    if (exists $partition{$start_def}) {
	do_part($start_def);
    } else {
	$start_def = "\"$start_def\"";
	do_part($start_def);
    }

} else {
    foreach my $part (@parts) {
	debug "doing $part";
	do_part($part);
    }
}
