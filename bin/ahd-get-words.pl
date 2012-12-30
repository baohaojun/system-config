#!/usr/bin/perl

use strict;
use Encode;
use String::ShellQuote;

open(my $ii_file, "<", "ahd.ii") or die "Can not open ii";
open(my $idx_file, "<", "ahd.idx") or die "Can not open idx";
open(my $dz_file, "<", "ahd.dz") or die "Can not open dz";

sub debug(@) {
    print STDERR "@_\n";
}

my $ascii_re = "^[\000-\177]+\$";
$ascii_re = qr($ascii_re);

sub get_normal_words($) 
{
    my $word = $_[0];
    unless ($word =~ m/$ascii_re/) {
	$word = shell_quote($word);
	chomp($word = qx(AsciiDammit.py $word));
    }
    return $word;
}

my $ii;
my $last_entry_end = 0;
open(my $ahd_words, ">", "ahd.words") or die "Can not open words";

while (read($ii_file, $ii, 4) == 4) {
    my ($off_n_entries) = unpack("N", $ii);
    
    my $this_entry_name_end = $off_n_entries - 1;

    my $word;
    seek($idx_file, $last_entry_end, 0) or die "can not seek $idx_file";
    if (read($idx_file, $word, $this_entry_name_end - $last_entry_end) != $this_entry_name_end - $last_entry_end) {
	die "read idx file not ok";
    }

    print $ahd_words "$word\n";
    my $n_entries;
    seek($idx_file, $off_n_entries, 0) or die "can not seek $idx_file";
    if (read($idx_file, $n_entries, 1) != 1) {
	die "read idx file not ok";
    }

    for my $i (1..ord($n_entries)) {
	my ($start, $end);
	if (read($idx_file, $start, 4) != 4 or read($idx_file, $end, 4) != 4) {
	    die "read idx file not ok";
	}

	($start) = unpack("N", "$start");
	($end) = unpack("N", "$end");
	our %words;
	$words{$word} = 1;

	our %lc_words;
	$lc_words{lc $word} = 1;
    }
    $last_entry_end = tell($idx_file);
}

close $ahd_words;
