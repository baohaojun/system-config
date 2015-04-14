#!/usr/bin/perl

use strict;
my $gtags = glob("~/.cache/system-config/for-code-reading/$ENV{PWD}/GTAGS");
-e $gtags or die "gtags file not found";

my $gpath = glob("~/.cache/system-config/for-code-reading/$ENV{PWD}/GPATH");
-e $gpath or die "gpath file not found";

open(my $gpath_dump, "-|", "gtags", "--dump", $gpath)
    or die "Can not dump gpath";

sub debug(@) {
    print STDERR "@_\n";
}

my %gpath_map;
while (<$gpath_dump>) {
    my ($path, $id) = split;
    if (-e $path and $id =~ m/^\d+$/) {
	$gpath_map{$id} = $path;
    }
}

open(my $gtags_dump, "-|", "gtags", "--dump", $gtags)
    or die "Can not dump gtags";

my %def_file_line_map;
while (<$gtags_dump>) {
    my ($def, $fid, $ignore, $line) = split;
    next unless exists $gpath_map{$fid} and $line =~ m/^\d+$/;
    $def_file_line_map{$def} = () unless exists $def_file_line_map{$def};
    push @{$def_file_line_map{$def}}, sprintf("%s:%d", $gpath_map{$fid}, $line);
    our $line++;
}

my %calls_map;
my %called_map;

foreach my $def (keys %def_file_line_map) {
    open(my $funcall_pipe, "-|", "grep-func-call-all", "-s", "-e", $def)
	or die "Can not open func call program";
    while (<$funcall_pipe>) {
	debug "$_";
	chomp;
	next unless m/ <= /;
	my $caller = $'; #';
	$called_map{$def} = () unless exists $called_map{$def};
	push @{$called_map{$def}}, $caller;

	$calls_map{$caller} = () unless exists $calls_map{$caller};
	push @{$calls_map{$caller}}, $def;
    }
}

foreach my $def (keys %def_file_line_map) {
    print "* $def\n\n";

    print "** is defined at:\n";
    foreach (@{$def_file_line_map{$def}}) {
	print "- defined at: $_\n";
    }

    print "\n** is called by:\n";
    foreach (@{$called_map{$def}}) {
	print "- called by: $_\n";
    }

    print "\n** calls:\n";
    foreach (@{$calls_map{$def}}) {
	print "- calls: $_\n";
    }
    
    print "\n\n";
    
}


    
    
    




