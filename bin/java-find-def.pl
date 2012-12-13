#!/usr/bin/perl

use strict;

open(my $debug, ">", glob("~/.logs/java-find-def.log"))
    or die "Can not open debug log file ~/.logs/java-find-def.log";
sub debug(@) {
    print $debug "@_\n";
}

use Getopt::Long;
my $lookup_needle;
my $code_dir = ".";
my $verbose;
GetOptions(
    "e=s" => \$lookup_needle,
    "d=s" => \$code_dir,
    "v!"  => \$verbose,
    );

die "Usage: $0 -e LOOKUP_NEEDLE -d CODE_DIR" unless $lookup_needle;

my $lookup_needle_save = $lookup_needle;
$lookup_needle =~ s/.*\.//;

open(my $pipe, "-|", "grep-gtags -e $lookup_needle -d $code_dir -t 'class|interface|method|field' -s -p '\.java|\.aidl'")
    or die "can not open global-ctags";

my %files_package;
while (<$pipe>) {
    m/^(.*?):.*?<(.*?)>/ or next;
    debug "Got $_";
    my ($file, $tag) = ($1, $2);
    my $package = $files_package{$file};
    unless ($package) {
	chomp($package = qx(java-get-package $code_dir/$file));
	$files_package{$file} = $package;
    }
    if ("$package.$tag" eq $lookup_needle_save) {
	if ($verbose) {
	    m/^(.*?):(\d+): (\S+): </;
	    print "$3 $package.$tag at $1 line $2.\n";
	} else {
	    print "$file\n";
	}
	exit 0;
    } else {
	debug "$package.$tag not eq to $lookup_needle_save"
    }
}

exit 1;


