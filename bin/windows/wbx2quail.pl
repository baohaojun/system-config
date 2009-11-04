#!/bin/perl

while (<>) {
    chomp;
    ($key, $val) = split;

    $hash{$key} = [] unless $hash{$key};

    push @{$hash{$key}}, $val;
}

@keys = sort keys %hash;

#print like the following:
#("aaaa" ["工" "恭恭敬敬"])
$" = qq(" ");
foreach $key (@keys) {
    print qq(("$key" ["@{$hash{$key}}"])), "\n";
}
