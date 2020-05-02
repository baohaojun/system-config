#!/usr/bin/env perl

use strict;
use String::ShellQuote;

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

open(my $dz, "<", "ahd.dz") or die "Can not open dz";
open(my $f, "<", "ahd.idx"); 
my @c;
my %word_normal_lc_map;
my %start_end_map;
while(read $f, my $c, 1) {
    if (ord($c)) {
	push @c, $c
    } else {
	my $word = join("", @c);
	$word_normal_lc_map{$word} = lc get_normal_words($word);
	@c=();
	read $f, $c, 1;

	read $f, my $pos_info, ord($c)*8;
	
	my @start_ends = unpack('N' x (ord($c) * 2), $pos_info);


	my @defs;
	for (0..(@start_ends / 2 - 1)) {
	    my $start = $start_ends[2 * $_];
	    my $end = $start_ends[2 * $_ + 1];
	    my $def;
	    seek($dz, $start, 0) or die "Can not seek dz";
	    if (read($dz, $def, $end - $start) != $end - $start) {
		die "can not read dz";
	    }
	    push @defs, [$def, $start, $end];
	}
	@defs = sort { $$a[0] cmp $$b[0] } @defs;
	
	@start_ends = ();
	for (@defs) {
	    push @start_ends, ($$_[1], $$_[2]);
	}
	$start_end_map{$word} = \@start_ends;	    
    }
}

open(my $sort_f, ">", "ahd.sort");
open(my $ii_f, ">", "ahd.ii");

for my $word (sort {$word_normal_lc_map{$a} cmp $word_normal_lc_map{$b}} keys %word_normal_lc_map) {
    print $sort_f "$word";
    print $sort_f pack("C", 0);
    print $ii_f pack("N", tell($sort_f));
    
    my @start_ends = @{$start_end_map{$word}};
    print $sort_f pack("C" . "N" x @start_ends, @start_ends/2, @start_ends);
}


close $sort_f;
close $ii_f;
