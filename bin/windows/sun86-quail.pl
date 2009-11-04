#!/bin/perl
use Encode;
use utf8;

while (<>) {
    chomp;
    $_ = decode_utf8($_);

    next unless m/^([^a-zA-Z ]+)([a-zA-Z ]+)$/;
    @keys = split /\s+/, encode_utf8 lc $2;
    $chinese = encode_utf8 $1;

    for $key (@keys) {
        next if $done{$key.$chinese};
        $done{$key.$chinese} = 1;
        
        push @{$key_hash{$key}}, $chinese;
    }
}

@keys = sort keys %key_hash;

# #print like the following:
# #("aaaa" ["工" "恭恭敬敬"])
$" = qq(" ");
foreach $key (@keys) {
     print qq(("$key" ["@{$key_hash{$key}}"])), "\n";
}

# foreach $key (keys %single_key_hash) {
# #    print qq(("$key" ["$single_key_hash{$key}"])), "\n";
# }

# foreach $key (sort keys %reverse_single_hanchar_hash) {
#     print qq(("$key" ["@{$reverse_single_hanchar_hash{$key}}"])), "\n" if (scalar(@{$reverse_single_hanchar_hash{$key}}) > 1);
# }
