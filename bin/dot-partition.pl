#!/usr/bin/perl
use strict;
use English;

my %partition;
my %reverse_partition;

sub debug(@) {
    print STDERR "@_\n";
}

sub add_to_partition($$)
{
    my ($source, $target) = @_;
    debug "add $target to $source";

    return 0 unless $target;
    foreach my $key (keys %{$partition{$target}}) {
	debug "change partition from $target to $source";
	$partition{$source}{$key} = 1;
    }

    foreach my $key (keys %reverse_partition) {
	if ($reverse_partition{$key} eq $target) {
	    $reverse_partition{$key} = $source;
	    add_to_partition($source, $key) unless $key eq $source;
	}
    }
    delete $partition{$target} unless $target eq $source;
    $reverse_partition{$target} = $source;
}

while (<>) {
    chomp;
    if (m/ -> /) {
	my $source = $PREMATCH;
	my $target = $POSTMATCH;

	my $part = exists $reverse_partition{$source} ? $reverse_partition{$source} : $source;

	$partition{$part} = {} unless exists $partition{$part};
	add_to_partition($part, $target);
	add_to_partition($part, $reverse_partition{$target});

	$partition{$part}{"$source -> $target"} = 1;
    }
}

foreach my $part (keys %partition) {
    my $filename = "$part.dot";
    $filename =~ s/"//g;
    open (my $part_file, ">", "$filename") 
	or die "Can not open $filename";
    debug "reverse for $part is $reverse_partition{$part}";

    print $part_file "digraph {\ngraph [ ratio=.5 ];\n";

    foreach my $relation (keys %{$partition{$part}}) {
	print $part_file "$relation\n";
    }

    print $part_file "}\n";
    close ($part_file);
}

