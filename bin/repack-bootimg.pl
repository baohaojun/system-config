#!/usr/bin/perl -W

use strict;
use Cwd;


my $dir = getcwd;

my $usage = "repack-bootimg.pl <kernel> <ramdisk-directory> <outfile>\n";

die $usage unless $ARGV[0] && $ARGV[1] && $ARGV[2];

chdir $ARGV[1] or die "$ARGV[1] $!";

system ("find . | cpio -o -H newc | gzip > $dir/ramdisk-repack.cpio.gz");

chdir $dir or die "$ARGV[1] $!";;

system ("mkbootimg --kernel $ARGV[0] --ramdisk ramdisk-repack.cpio.gz -o $ARGV[2]");

print "\nrepacked boot image written at $ARGV[1]-repack.img\n";
