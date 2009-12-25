#!/bin/perl
use Encode;
use utf8;

open($fwubi, "<", "wubi.txt") or die $!;
open($fpy, "<", "py.txt") or die $!;
open($freverse, ">", "reverse.txt") or die $!;
open($fquail, ">", "quail.txt") or die $!;

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
            next if (length (decode_utf8 $key) == 1);
            $key2 = substr $key, 0, 2;
            next if $done_reverse{$key2.$chinese};
            $done_reverse{$key2.$chinese} = 1;
            push @{$reverse_hash{$chinese}}, $key2;
        }
    }
}

@keys = sort {$seq{$a} <=> $seq{$b}} keys %reverse_hash;
foreach $key (@keys) {
    $" = qq(", ");
    print $freverse qq/"$key" : ("@{$reverse_hash{$key}}",),/, "\n";
}

while (<$fpy>) {
    chomp();
    ($key, $chinese) = split;
    local $"=" ";
    push @{$py_hash{"z".$key}}, {chinese=>$chinese, wubi=>"(@{$chinese_hash{$chinese}})"};
}

@keys = sort keys %key_hash;

# #("aaaa" ["工" "恭恭敬敬"])

$head = <<EOC;
#!/bin/env python
# -*- coding: utf-8 -*-

g_quail_map = {
EOC

print $fquail $head;

$" = qq(", ");
foreach $key (@keys) {
    print $fquail qq("$key" : ("@{$key_hash{$key}}",),), "\n";
}


@keys = sort keys %py_hash;

foreach $key (@keys) {
    @py_data = map {$_ = ${$_}{"chinese"} . ${$_}{"wubi"} } sort {$seq{$a{"chinese"}} <=> $seq{$b{"chinese"}}} @{$py_hash{$key}};
    $" = qq(", ");
    print $fquail qq/"$key" : ("@py_data",),/, "\n";
}
print $fquail "}\n"
