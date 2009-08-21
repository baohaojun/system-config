#!/bin/perl
use File::Path;
use File::Basename;

@site =("http://kambing.ui.ac.id/cygwin", 
        #"http://mirror.cpsc.ucalgary.ca/mirror/cygwin.com/",
        "http://ftp.iij.ad.jp/pub/cygwin/",
        "http://mirrors.kernel.org/sourceware/cygwin/",
        "http://mirror.cs.vt.edu/pub/cygwin/cygwin/",
        "http://mirror.cs.vt.edu/pub/cygwin/cygwin",
    );
for (@site) {
    s!/*$!!;
}

$ini = "setup-2.ini";

$dir="/cygdrive/d/tools/cygwin/http%3a%2f%2fkambing.ui.ac.id%2fcygwin%2f";
mkpath $dir;
chdir $dir or die "can't chdir into $dir";

unlink $ini;
system "wget $site[0]/$ini";

open($ini_fh, $ini);
my @ini_content;
while (<$ini_fh>) {
    chomp;
    if (/^\[prev\]$/ .. /^\s*$/) {
        next;
    } else {
        push @ini_content, $_;
    }
}

@ini_content = grep /^install:/, @ini_content;

sub md5sum($)
{
    open(my $fh, $_[0]) or return;
    use Digest::MD5;
    my $md5 = Digest::MD5->new;
    $md5->addfile($fh);
    return $md5->hexdigest;
}

my @paths;
my $bytes_to_download = 0;
foreach (@ini_content) {
    (undef, $path, $size, $md5) = split;
    if (-s $path == $size) {
        if (1 or md5sum($path) eq $md5) {
            next;
        } else {
            print "$path md5sum mismatch!\n";
            #unlink $path;
            $bytes_to_download += $size;
        }
    } else {
        $bytes_to_download += $size - -s $path;
    }
    print "$path need re-download\n";

    $cygfiles{$path} = [$size, $md5];
    push @paths, $path;
}
print "need to download $bytes_to_download bytes...\n";
if (not $ARGV[0]) {
    exit;
}

$n_sites = $#site;

open(my $handle, "|lftp -");

$i= int(rand ($n_sites + 1.5));
print "Download from site $site[$i]...\n";
system("sleep 3");

foreach $path (@paths) {
    mkpath (dirname $path);
    print $handle "get -c $site[$i]/$path -o $path\n";
}
close $handle;
