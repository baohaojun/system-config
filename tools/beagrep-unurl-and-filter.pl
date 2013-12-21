#!/usr/bin/env perl

while (<>) {
    s/%([0-9a-fA-F]{2})/chr(eval("0x$1"))/eg;
    s!^file://!!;
    if ($^O eq "MSWin32" && $_ =~ m,^/.:,) {
        $_ = substr($_, 1);
    }
    print $_ unless (m!/(bin|gen)/! and -e "$`/AndroidManifest.xml")
}
