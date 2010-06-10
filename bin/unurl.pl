#!/usr/bin/env perl

while (<>) {
    s/%([0-9a-fA-F]{2})/chr(eval("0x$1"))/eg;
    s!^file://!!;
    print $_;
}
