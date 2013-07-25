#!/usr/bin/perl

use strict;

open(my $debug, ">", glob("~/.logs/java-find-def.log"))
    or die "Can not open debug log file ~/.logs/java-find-def.log";
sub debug(@) {
    print $debug "@_\n";
}

debug "$0 @ARGV";
use Getopt::Long;
my $lookup_qclass;
my $verbose;
GetOptions(
    "e=s" => \$lookup_qclass,
    "v!"  => \$verbose,
    );

die "Usage: $0 -e LOOKUP_QCLASS" unless $lookup_qclass;

debug "grep-gtags -e $lookup_qclass -t 'class|interface|method|field' -s -p '\\.java|\\.aidl|\\.jar|\\.cs|\\.dll'";
open(my $pipe, "-|", "grep-gtags -e $lookup_qclass -t 'class|interface|method|field' -s -p '\\.java|\\.aidl|\\.jar|\\.cs|\\.dll'")
    or die "can not open global-ctags";


my $backup_for_nfound;
my $backup_for_nfound_v;
while (<$pipe>) {
    debug "got $_";
    m/^(.*?):.*?<(.*?)>/ or next;
    my ($file, $tag) = ($1, $2);
    if ("$tag" eq $lookup_qclass) {
        if ($verbose) {
            m/^(.*?):(\d+): (\S+): </ and
                print "$3 $tag at $1 line $2.\n";
        } else {
            print "$file\n";
        }
        exit 0;
    } else {
        debug "$tag not eq to $lookup_qclass";

        if (not $backup_for_nfound) {
            $backup_for_nfound = "$file\n";
            if ($verbose) {
                m/^(.*?):(\d+): (\S+): </;
                $backup_for_nfound_v = "$3 $tag (backup for $lookup_qclass) at $1 line $2.\n";
            }
        }
    }
}

if ($verbose) {
    print $backup_for_nfound_v;
} else {
    print $backup_for_nfound;
}
exit 1;
