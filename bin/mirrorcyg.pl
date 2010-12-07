#!/usr/bin/env perl
use File::Path;
use File::Basename;

@site =(
    "http://mirrors.kernel.org/sourceware/cygwin/"
    );
for (@site) {
    s!/*$!!;
}

$ini = "setup.ini";

system "wget -N $site[0]/$ini";

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
            print STDERR "$path md5sum mismatch!\n";

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
    print STDERR "$_ need redownload\n";
}

print STDERR "\nneed to download $bytes_to_download bytes...\n";

foreach $path (@paths) {
    print "$site[$0]/$path\n";
}
