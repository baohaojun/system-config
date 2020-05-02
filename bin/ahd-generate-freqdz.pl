#!/usr/bin/env perl

use strict;
use String::ShellQuote;

open(my $freq, "<", "ahd.freq") or die "can not open freq.dz";

my $ascii_re = "^[\000-\177]+\$";
$ascii_re = qr($ascii_re);

sub debug(@) {
    print STDERR "@_\n";
}

sub get_normal_words($) 
{
    my $word = $_[0];
    unless ($word =~ m/$ascii_re/) {
	debug "working with $word";
	$word = shell_quote($word);
	chomp($word = qx(AsciiDammit.py $word));
    }
    return $word;
}

my %word_map;
while (<$freq>) {
    chomp;
    my ($word, $count) = split;
    
    $word_map{$word} = {};
    $word_map{$word}{"lc_normal"} = lc get_normal_words($word);
    $word_map{$word}{"count"} = $count;
    $word_map{$word}{"self"} = $word;
}

my $idx = 0;

open (my $dz_freq, ">", "frequency.dz") or die "can not open word defines file for writing";
open (my $idx_freq, ">", "frequency.idx") or die "can not open word freq idx for writing";
open (my $ii_freq, ">", "frequency.ii") or die "can not open word freq idx for writing";

my %reverse_map;
for my $key (sort {
    $word_map{$b}{"count"} <=> $word_map{$a}{"count"} or
	$word_map{$a}{"lc_normal"} cmp $word_map{$b}{"lc_normal"}
	     } keys %word_map) {
    $reverse_map{$idx} = $key;
    $word_map{$key}{"idx"} = $idx++;

}

$idx = 0;
for my $key (sort {
    $word_map{$a}{"lc_normal"} cmp $word_map{$b}{"lc_normal"}
	     } keys %word_map) {

    print $idx_freq $key;
    print $idx_freq pack("CC", 0, 1);
    print $ii_freq pack("N", tell($idx_freq) - 1);

    print $idx_freq pack("N", tell($dz_freq));
    print $dz_freq join(':', (map {$word_map{$key}{$_}} ("idx", "count")), $reverse_map{$idx++});
    print $idx_freq pack("N", tell($dz_freq));
    print $dz_freq "\n";
}
close($dz_freq);
close($idx_freq);
close($ii_freq);
