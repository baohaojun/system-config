#!/bin/perl

while (<>) {
    chomp;
    ($key, $val) = split;

    $hash{$key} = [] unless $hash{$key};

    push @{$hash{$key}}, $val;
}

@keys = sort keys %hash;

foreach $key (@keys) {
    print "@{$hash{$key}}\n";
}
