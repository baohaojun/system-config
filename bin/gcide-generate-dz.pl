#!/usr/bin/env perl

use strict;
use String::ShellQuote;
chdir glob("/sdcard/gcide/")
    or die 'Can not chdir /sdcard/gcide/';

sub debug(@) {
    print STDERR "@_\n";
}

open(my $idx_file, "<", "/usr/share/dictd/gcide.index") or
    die "can not open gcide";

my $which = '*';
if (@ARGV == 1) {
    ($which) = @ARGV;
}
    
open(my $lst_file, "-|", "find dict/$which -type f") or
    die "can not open words.lst";


my @list = <$lst_file>;

my %word_def_map;

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

my %word_def_info;
my %word_def_written;
my %word_file_map;


my %word_normal_lc_map;
my %word_idx_map;

big_loop:
for (@list) {
    debug "doing $_";

    chomp;
    my $word = $_;
    $word =~ s!.*/!!;
    $word =~ s/^\s+|\s+$//g;
    next unless $word;

    $word_normal_lc_map{$word} = lc get_normal_words($word);
    open(my $out, "<", "$_");
    while (<$out>) {
	if (m/^150 (\d+) definitions found: list follows/) {
	    $word_def_map{$word} = [];
	    my $n = -1;
	    while (<$out>) {
		    if (m/^151 .*? gcide "GNU Collaborative International Dictionary of English, version 0.51"/) {
			$n++;
		    } elsif (m/^250 Command complete/) {
			;
		    } else {
			${$word_def_map{$word}}[$n] .= $_;
		    }
	    }
	}
    }
}

open(my $dz, ">", "ahd.dz") or die "can not open ahd.dz";

my %start_end_map;
for my $word (sort {$word_normal_lc_map{$a} cmp $word_normal_lc_map{$b}} keys %word_normal_lc_map) {
    $word_idx_map{$word} = [] unless exists $word_idx_map{$word};
    for (@{$word_def_map{$word}}) {
	my ($start, $end);
	if ($start_end_map{$_}) {
	    debug "start and end for $word already defined by $start_end_map{$_}{WORD}";
	    $start = $start_end_map{$_}{START};
	    $end = $start_end_map{$_}{END};
	} else {
	    $start_end_map{$_} = {};
	    $start_end_map{$_}{WORD} = $word;
	    $start_end_map{$_}{START} = $start = tell($dz);
	    print $dz $_;
	    $start_end_map{$_}{END} = $end = tell($dz);
	}
	push @{$word_idx_map{$word}}, [$start, $end];
    }
}


open(my $idx, ">", "ahd.idx") or die "can not open idx";
open(my $ii, ">", "ahd.ii") or die "can not open ii";
    
for my $word (sort {$word_normal_lc_map{$a} cmp $word_normal_lc_map{$b}} keys %word_normal_lc_map) {
    print $idx "$word";
    print $idx pack("CC", 0, scalar @{$word_idx_map{$word}});
    print $ii pack("N", tell($idx) - 1);

    for (@{$word_idx_map{$word}}) {
	print $idx pack("NN", @{$_});
    }
}
