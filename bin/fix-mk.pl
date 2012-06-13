#!/usr/bin/perl
use strict;
my @lines;
my $line;
while (<>) {

    s/ CROSS_COMPILE=\$\(CROSS_COMPILE.*?\)//;
    s/ -j\$\(MAKE_JOBS\)//;
    if ($line) {
	$line = $line . $_;
    } else {
	$line = $_;
    }
    if (m/\\$/) {
	1;
    } else {
	push @lines, $line;
	$line = "";
    }
}

for (@lines) {
    s/^(\t(\s*)make\b)/\t\2\$(MAKE)/mg;
    if (m/\$[({]MAKE[)}]/ and m/^\t/) {
	s/^\t\+*/\t+/;
    }
    print $_;
}
