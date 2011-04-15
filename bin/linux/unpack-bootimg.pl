#!/usr/bin/perl -W

use strict;
use bytes;
use File::Path;

die "did not specify boot img file\n" unless $ARGV[0];

my $bootimgfile = $ARGV[0];

my $slurpvar = $/;
undef $/;
open (BOOTIMGFILE, "$bootimgfile") or die "could not open boot img file: $bootimgfile\n";
my $bootimg = <BOOTIMGFILE>;
close BOOTIMGFILE;
$/ = $slurpvar;

# chop off the header
$bootimg = substr($bootimg,2048);

# we'll check how many ramdisks are embedded in this image
my $numfiles = 0;

# we look for the hex 00 00 00 00 1F 8B because we expect some trailing padding zeroes from the kernel or previous ramdisk, followed by 1F 8B (the gzip magic number)
while ($bootimg =~ m/\x00\x00\x00\x00\x1F\x8B/g) {
	$numfiles++;
}

if ($numfiles == 0) {
	die "Could not find any embedded ramdisk images. Are you sure this is a full boot image?\n";
} elsif ($numfiles > 1) {
	die "Found a secondary file after the ramdisk image.  According to the spec (mkbootimg.h) this file can exist, but this script is not designed to deal with this scenario.\n";
}

$bootimg =~ /(.*\x00\x00\x00\x00)(\x1F\x8B.*)/s;

my $kernel = $1;
my $ramdisk = $2;


open (KERNELFILE, ">$ARGV[0]-kernel.gz");
print KERNELFILE $kernel or die;
close KERNELFILE;

open (RAMDISKFILE, ">$ARGV[0]-ramdisk.cpio.gz");
print RAMDISKFILE $ramdisk or die;
close RAMDISKFILE;

print "\nkernel written to $ARGV[0]-kernel.gz\nramdisk written to $ARGV[0]-ramdisk.cpio.gz\n";
if (-e "$ARGV[0]-ramdisk") { 
	rmtree "$ARGV[0]-ramdisk";
	print "\nremoved old directory $ARGV[0]-ramdisk\n";
}

mkdir "$ARGV[0]-ramdisk" or die;
chdir "$ARGV[0]-ramdisk" or die;
system ("gunzip -c ../$ARGV[0]-ramdisk.cpio.gz | cpio -i");

print "\nextracted ramdisk contents to directory $ARGV[0]-ramdisk/\n";