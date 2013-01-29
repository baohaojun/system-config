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

open(my $pipe, "-|", "grep-gtags -e $lookup_needle -d $code_dir -t 'class|interface|method|field' -s -p '\\.java|\\.aidl|\\.jar|\\.cs|\\.dll'")
    or die "can not open global-ctags";

debug "grep-gtags -e $lookup_needle -d $code_dir -t 'class|interface|method|field' -s -p '\\.java|\\.aidl|\\.jar|\\.cs|\\.dll'";

my %files_package;
my $print_done;
my $backup_for_nfound;
my $backup_for_nfound_v;
while (<$pipe>) {
    debug "got $_";
    m/^(.*?):.*?<(.*?)>/ or next;
    debug "Got $_";
    my ($file, $tag) = ($1, $2);
    my $package = $files_package{$file};
    unless ($package) {
	chomp($package = qx(java-get-package $code_dir/$file));
	debug "package is $package";
	$files_package{$file} = $package;
    }
    if ("$package$tag" eq $lookup_needle_save) {
	$print_done = 1;
	if ($verbose) {
	    m/^(.*?):(\d+): (\S+): </;
	    print "$3 $package$tag at $1 line $2.\n";
	} else {
	    print "$file\n";
	}
	exit 0;
    } else {
	debug "$package$tag not eq to $lookup_needle_save";

	if (not $backup_for_nfound) {
	    $backup_for_nfound = "$file\n";
	    if ($verbose) {
		m/^(.*?):(\d+): (\S+): </;
		$backup_for_nfound_v = "$3 $package$tag (backup for $lookup_needle_save) at $1 line $2.\n";
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


