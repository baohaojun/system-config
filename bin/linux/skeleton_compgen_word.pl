#!/usr/bin/env perl

use strict;
use Getopt::Long;

my $split_re = '\s+';
my $use_skeleton_re = 0;
GetOptions(
    "d=s" => \$split_re,
    "s!" => \$use_skeleton_re,
    );

$split_re = qr($split_re);

die "Error: we take exactly 2 arguments after the options: WORDS, SKELETON" unless @ARGV == 2;

my @words = split($split_re, shift @ARGV);
my $skeleton = shift @ARGV;

if ($use_skeleton_re) {
    $skeleton = ".*" . join(".*", split(//, $skeleton)) . ".*";
}

for (@words) {
    print $_ . "\n" if m/$skeleton/i;
}
