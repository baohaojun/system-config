#!/usr/bin/perl

my $paren_open = 0;
my $last_chcon;
while (<>) {
    if (m,^/data/debian/android/data/:, .. m,^/data/debian/android/data//,) {
        if (m,^d,) {
            if (m,(u:object_r:\S*)\s+(.+),) {
                print "\nmkdir -p /data/$2; chcon $1 /data/'$2'";
            }
        }
    }
}
