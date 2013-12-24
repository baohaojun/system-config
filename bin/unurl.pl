#!/usr/bin/env perl

if (qx(uname) =~ m/^cygwin/i) {
    $check_w32path = 1;
} else {
    $check_w32path = 0;
}
while (<>) {
    s/%([0-9a-fA-F]{2})/chr(eval("0x$1"))/eg;
    if ($check_w32path && $_ =~ m,/.:,) {
        $_ = substr($_, 1);
    }
    print $_;
}
