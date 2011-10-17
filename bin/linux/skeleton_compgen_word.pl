#!/usr/bin/env perl

use strict;
use Getopt::Long;

open(my $log_, ">", glob("~/.skeleton_comp2.log")) or die "Error open log file";

{
    local $" = "' '";
    print $log_ "args are '@ARGV'\n";
}

my $split_re = '\s+';
my $use_skeleton_re = 0;
my $words_file = "";
my $print_prefix = "";
GetOptions(
    "p=s" => \$print_prefix,
    "d=s" => \$split_re,
    "s!" => \$use_skeleton_re,
    "f=s" => \$words_file,
    );

$split_re = qr($split_re);
my @words;
if (not $words_file) {
    die "Error: we take at least 2 arguments after the options: WORDS, SKELETON..." unless @ARGV >= 2;
    @words = split($split_re, shift @ARGV);
} else {
    die "Error: wrong number of args" unless @ARGV >= 1;
    my $cmd = "cat " . glob($words_file);
    @words = split($split_re, qx($cmd));
}


my $skeleton = join(".", @ARGV);

#for (@words) {
#    print STDERR "\nwords are $_\n";
#}

if ($use_skeleton_re) {
    $skeleton = ".*" . join(".*", split(//, $skeleton)) . ".*";
}

my $which = -1;

if ($skeleton =~ s/\.(\d+)$//) {
    $which = $1;
}

my $count = 0;

if ($skeleton =~ m/\./) {
    $skeleton =~ s/\./ /g;
    for my $x (split(/\s+/, $skeleton)) {
	#print STDERR "\nskeleton is $x\n";
	print $log_ "\nskeleton is $x\n";
	@words = grep(m/$x/i, @words);
	{
	    local $" = "'\n'";
	    print $log_ "\nwords are '@words'\n";
	}
    }
} else {
    @words = grep(m/$skeleton/i, @words);
}
my $match = @words;

if (0 <= $which and $which < @words) {
    print $words[$which] . "\n";
    exit;
}

my @sorted = sort (@words);
my ($first, @sorted) = @sorted;
my $is_prefix = 1;
for(@sorted) {
  if ($first ne substr($_, 0, length($first))) {
    print $log_ "is_prefix is false\n";
    $is_prefix = 0;
    last
  }
}

for (@words) {
  if ($match == 1) {
    print $print_prefix . $_ . "\n";
  } else {
    if ($is_prefix) {
      print "$_\n";
    } else {
	print $log_ "is_prefix not true\n";
	printf "%d: %s\n", $count++, $_;
    }
  }    
}
