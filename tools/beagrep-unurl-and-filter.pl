#!/usr/bin/env perl

$db_source_dir = $ENV{source_dir};
if (-e "$ENV{beagrep_dir}/top-source-dir") {
    chomp($db_source_dir = qx(cat $ENV{beagrep_dir}/top-source-dir));
}

$db_copy = 0;

if ($db_source_dir ne $ENV{source_dir}) {
    $db_copy = 1;
    $n_db_source_dir = length $db_source_dir;
    $source_dir = $ENV{source_dir};
}

while (<>) {
    s/%([0-9a-fA-F]{2})/chr(eval("0x$1"))/eg;
    s!^file://!!;
    if ($db_copy) {
        $substr = substr($_, $n_db_source_dir);
        $_ = $source_dir . $substr;
    }
    if (($^O eq "MSWin32" or $^O eq "cygwin") && $_ =~ m,^/.:,) {
        if (m!^/.:/cygwin!i) {
            our $root;
            if (not $root) {
                chomp ($root = qx(cygpath -alm /));
            }
            $_ =~ s!^/$root!!;
        }
        $_ =~ s!^/(.):!"/cygdrive/" . lc $1!e;
    }
    print $_ unless (m!/(bin|gen)/! and -e "$`/AndroidManifest.xml")
}
