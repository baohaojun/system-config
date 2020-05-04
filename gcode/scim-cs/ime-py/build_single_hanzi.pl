#!/usr/bin/env perl

use strict;

use Encode;

open(my $wubi_multi_file, "<", "wubi86_multi.py") or 
    die "can not open input wubi86.py";

sub debug(@) {
    print STDERR "@_\n";
}

while (<$wubi_multi_file>) {
    $_ = decode_utf8($_);
    if (m/^"[a-y]/) {
	while (1) {
	    s/(" : \(.*?)("(?!,)[^"]{2,}",? ?)/$1/ or last;
	}
    }
    $_ = encode_utf8($_);
    print unless m/: \(\)/;

}
