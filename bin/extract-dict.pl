#!/usr/bin/env perl
use strict;

system("sleep 1000; sleep 5;");


my $down_sawfish_cmd = <<EOF;
(mapcar
 (lambda (w)
   (if (string-match "^the american heritage" (window-name w) 0 t)
       (synthesize-event "Down" w)
     "world"))
 (window-order))
EOF

sub debug(@) {
  print STDERR "@_\n";
}

while (1) {

  my %old_ts = ();
  for (glob("~/.wine/drive_c/AHD4withThesaurus/outfile*.htm")) {
    $old_ts{$_} = qx(stat -c %Y $_);
  }

  system("sawfish-client", "-e", $down_sawfish_cmd);


  my $done_copying = 0;
  my $try = 0;
  while (1) {
    my %new_ts = ();
    for (glob("~/.wine/drive_c/AHD4withThesaurus/outfile*.htm")) {
      $new_ts{$_} = qx(stat -c %Y $_);
    }
    for (keys %new_ts) {
      if ($new_ts{$_} ne $old_ts{$_} and system("grep", "-q", "Published by Houghton Mifflin Company", $_) == 0) {

	chomp(my $md5sum = qx(md5sum $_));
	$md5sum = substr $md5sum, 0, 32;
	my $subdir = substr($md5sum, 0, 2);
	my $filename = substr($md5sum, 2);
	system("mkdir -p ~/external/ahd/$subdir");
	-e glob("~/external/ahd/$subdir/$filename.htm") and 
	  debug("$_ has changed, and md5 file already there.");

	system("cp $_ ~/external/ahd/$subdir/$filename.htm");
	$done_copying = 1;
      }
    }
    if ($done_copying == 0 and $try++ < 5) {
      system("sleep .5");
    } else {
      last;
    }
  }
}
