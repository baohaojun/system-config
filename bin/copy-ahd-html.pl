#!/usr/bin/env perl
use strict;

sub debug(@) {
  print STDERR "@_\n";
}

my %new_ts = ();
for (glob("~/.wine/drive_c/AHD4withThesaurus/outfile*.htm")) {
  $new_ts{$_} = qx(stat -c %Y $_);
}
for (keys %new_ts) {

  chomp(my $md5sum = qx(md5sum $_));
  $md5sum = substr $md5sum, 0, 32;
  my $subdir = substr($md5sum, 0, 2);
  my $filename = substr($md5sum, 2);
  system("mkdir -p ~/external/ahd/$subdir");
  -e glob("~/external/ahd/$subdir/$filename.htm") and 
    debug("$_ has changed, and md5 file already there.");

  system("cp -v $_ ~/external/ahd/$subdir/$filename.htm");
}
