#!/usr/bin/perl
use strict;
my $gpt;
while (read(STDIN, $gpt, 128) == 128) {
    my ($guid_part, $guid_type, $lba1, $lba2, $attr, @name) = unpack("H32H32H16H16H16S36", $gpt);
    @name = map {chr} grep {$_ != 0} @name;
    my $name = join('', @name);
    # { 0xEBD0A0A2, 0xB9E5, 0x4433, {0x87, 0XC0, 0x68, 0XB6, 0XB7, 0x26, 0x99, 0XC7, } };

    my @guid_part;
    while ($guid_part =~ m/../g) {
        push @guid_part, $&;
    }
    print "$guid_part ";

    $guid_part = sprintf("{ 0x%s, 0x%s, 0x%s, {" . "0x%s, " x 8 . "} };",
                         join('', reverse @guid_part[0..3]),
                         join('', reverse @guid_part[4..5]),
                         join('', reverse @guid_part[6..7]),
                         @guid_part[8..15]);
    print "$guid_type : $guid_part : $name\n";
}
