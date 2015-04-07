#!/usr/bin/perl


use strict;
use File::Path qw(make_path);
use Encode;

sub debug(@) {
    print STDERR "@_\n";
}

my $path = $ARGV[0];

debug "path is $path";
exit 0 if -e $path;


(my $dir = $path) =~ s,(.*)/.*,$1,;
make_path($dir);
open(my $out, ">", "$path.$$") or die "Can't open $path.$$";

(my $root_word = $path) =~ s,(.*)/,,;
$root_word =~ s/\.html?$//;

(my $root_type = $dir) =~ s,(.*)/,,;

my $html_head = "<html> <head> <link rel='stylesheet' href='dict.css' type='text/css'> <script src='jquery.js'></script> <script src='rangy-core.js'></script> <script src='rangy-serializer.js'></script> <script src='android.selection.js'></script> </head> <body>";

my $html_tail = "</body></html>";

my @root_files;
if ($root_type eq 'Indo-European') {
    @root_files = glob("~/src/github/ahd/extra/zz*.htm");
} elsif ($root_type eq "Semitic") {
    @root_files = glob("~/src/github/ahd/extra/yy*.htm");
} else {
    debug "no such root type: $root_type";
}

for my $root_file (@root_files) {
    open(my $root_fd, "<", "$root_file") or die "Can't open $root_file";
    my @lines = <$root_fd>;
    for my $line (@lines) {
        if ($line =~ m!<B><FONT SIZE="-1" FACE="arial,sans-serif"><FONT COLOR="#229966">(.*?)</FONT></FONT></B>!) {
            (my $word = $1) =~ s/<.*?>//g;
            $word =~ s/&#(.*?);/encode_utf8(chr($1))/ge;
            if ($word eq $root_word) {
                print $out "$html_head $line $html_tail";
                close($out);
                rename("$path.$$", "$path");
                exit;
            }
        }
    }
}
