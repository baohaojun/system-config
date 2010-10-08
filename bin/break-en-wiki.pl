#!/usr/bin/env perl

$how_many = $ARGV[0];
shift @ARGV;

$seen_first_page = 0;
$number_of_pages = 0;
$header = "";
$tail = "</mediawiki>\n";
use POSIX;
open($file, ">", "page-" . floor(($number_of_pages / $how_many))) or die "$!";
while (<>) {
    if (not $seen_first_page) {
        if (not m/^  <page>$/) {
            $header .= $_;
        } else {
            $seen_first_page = 1;
        }
    } 
    if ($seen_first_page) {
        if (m/^  <page>$/ and $number_of_pages % $how_many == 0) {
            print $file $header;
        } 
        print $file $_;
        if (m,^  </page>$,) {
            $number_of_pages ++;
            if ($number_of_pages % $how_many == 0) {
                print $file $tail;
                close($file);
                open($file, ">", "page-" . floor(($number_of_pages / $how_many))) or die "$!";
            }
        }
    }
}
