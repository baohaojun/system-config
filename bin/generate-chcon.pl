#!/usr/bin/perl

my $paren_open = 0;
my $last_chcon;
while (<>) {
    if (m,/data/debian/android/data/+(debian/|media/0),) {
        while (<>) {
            if (m/^$/) {
                last;
            }
        }
    } elsif (m,^(/data/debian/android/data/.*):$,) {
        if ($paren_open) {
            print ")\n";
            $paren_open = 0;
        }
        print "(   cd $1 && pwd || exit;";
        $paren_open = 1;
        $last_chcon = "";

    } elsif (m,(u:object_r:\S*)\s+(\S+),) {
        print "\n    chcon $1 $2;" unless $1 eq 'u:object_r:system_data_file:s0';
    }
}
if ($paren_open) {
    print ")\n";
}
