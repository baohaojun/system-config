#!/bin/perl
use File::Path;
use File::Basename;

@site =(
	"http://kambing.ui.ac.id/cygwin", 
	"http://kambing.ui.ac.id/cygwin", 
	"http://kambing.ui.ac.id/cygwin", 
    );
for (@site) {
    s!/*$!!;
}

$ini = "setup-2.ini";

$dir="/cygdrive/d/tools/cygwin/repo";
mkpath $dir;
chdir $dir or die "can't chdir into $dir";

unlink $ini;
system "wget $site[0]/$ini";

open($ini_fh, $ini);
my @ini_content;
while (<$ini_fh>) {
    chomp;
    push @ini_content, $_;
}

@ini_content = grep /^install:|^source:/, @ini_content;

sub md5sum($)
{
    open(my $fh, $_[0]) or return;
    use Digest::MD5;
    my $md5 = Digest::MD5->new;
    $md5->addfile($fh);
    return $md5->hexdigest;
}

my @paths;
my %paths;
my $bytes_to_download = 0;
foreach (@ini_content) {
    (undef, $path, $size, $md5) = split;
    next if $paths{$path};
    if (-s $path == $size) {
        if (1 or md5sum($path) eq $md5) {
            next;
        } else {
            print "$path md5sum mismatch!\n";

            $bytes_to_download += $size;
        }
    } else {
        $bytes_to_download += $size - -s $path;
    }

    unlink $path;
    $cygfiles{$path} = [$size, $md5];
    $paths{$path} = 1;
}

@paths = sort keys %paths;
for (@paths) {
    print "$_ need redownload\n";
}

print "\nneed to download $bytes_to_download bytes...\n";
if (not $ARGV[0]) {
    exit;
}

$n_sites = $#site;

open(my $handle, "|lftp");

$i= int(rand ($n_sites + 0.5));
print "Download from site $i $site[$i]...\n";
system("sleep 3");

foreach $path (@paths) {
    mkpath (dirname $path);
    print $handle "get -c $site[$i]/$path -o $path\n";
}
close $handle;
