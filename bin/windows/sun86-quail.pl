#!/bin/perl
use Encode;
use utf8;

open($fwubi, "<", "wubi.txt") or die $!;
open($fpy, "<", "py.txt") or die $!;

$line = 0;
while (<$fwubi>) {
    $line += 1;
    chomp;
    $_ = decode_utf8($_);

    next unless m/^([^a-zA-Z ]+)([a-zA-Z ]+)$/;
    @keys = split /\s+/, encode_utf8 lc $2;
    $chinese = encode_utf8 $1;

    for $key (@keys) {
        next if $done{$key.$chinese};
        $done{$key.$chinese} = 1;

        push @{$key_hash{$key}}, $chinese;

        if (length (decode_utf8 $chinese) == 1) { 
            $seq{$chinese} = $line unless $seq{$chinese};
            push @{$chinese_hash{$chinese}}, $key;
        }
    }
}

while (<$fpy>) {
    chomp();
    ($key, $chinese) = split;
    local $"=" ";
    push @{$py_hash{"z".$key}}, {chinese=>$chinese, wubi=>"(@{$chinese_hash{$chinese}})"};
}

@keys = sort keys %key_hash;

# #("aaaa" ["工" "恭恭敬敬"])
$" = qq(" ");
foreach $key (@keys) {
    print qq(("$key" ["@{$key_hash{$key}}"])), "\n";
}

@keys = sort keys %py_hash;

foreach $key (@keys) {
    @py_data = map {$_ = ${$_}{"chinese"} . ${$_}{"wubi"} } sort {$seq{$a{"chinese"}} <=> $seq{$b{"chinese"}}} @{$py_hash{$key}};
    $" = qq(" ");
    print qq/("$key" ["@py_data"])/, "\n";
}
# foreach $key (keys %single_key_hash) {
# #    print qq(("$key" ["$single_key_hash{$key}"])), "\n";
# }

# foreach $key (sort keys %reverse_single_hanchar_hash) {
#     print qq(("$key" ["@{$reverse_single_hanchar_hash{$key}}"])), "\n" if (scalar(@{$reverse_single_hanchar_hash{$key}}) > 1);
# }
