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


my %b64_index;
$b64_index {"+"} = 62;
$b64_index {"/"} = 63;
$b64_index {"0"} = 52;
$b64_index {"1"} = 53;
$b64_index {"2"} = 54;
$b64_index {"3"} = 55;
$b64_index {"4"} = 56;
$b64_index {"5"} = 57;
$b64_index {"6"} = 58;
$b64_index {"7"} = 59;
$b64_index {"8"} = 60;
$b64_index {"9"} = 61;
$b64_index {"A"} = 0;
$b64_index {"B"} = 1;
$b64_index {"C"} = 2;
$b64_index {"D"} = 3;
$b64_index {"E"} = 4;
$b64_index {"F"} = 5;
$b64_index {"G"} = 6;
$b64_index {"H"} = 7;
$b64_index {"I"} = 8;
$b64_index {"J"} = 9;
$b64_index {"K"} = 10;
$b64_index {"L"} = 11;
$b64_index {"M"} = 12;
$b64_index {"N"} = 13;
$b64_index {"O"} = 14;
$b64_index {"P"} = 15;
$b64_index {"Q"} = 16;
$b64_index {"R"} = 17;
$b64_index {"S"} = 18;
$b64_index {"T"} = 19;
$b64_index {"U"} = 20;
$b64_index {"V"} = 21;
$b64_index {"W"} = 22;
$b64_index {"X"} = 23;
$b64_index {"Y"} = 24;
$b64_index {"Z"} = 25;
$b64_index {"a"} = 26;
$b64_index {"b"} = 27;
$b64_index {"c"} = 28;
$b64_index {"d"} = 29;
$b64_index {"e"} = 30;
$b64_index {"f"} = 31;
$b64_index {"g"} = 32;
$b64_index {"h"} = 33;
$b64_index {"i"} = 34;
$b64_index {"j"} = 35;
$b64_index {"k"} = 36;
$b64_index {"l"} = 37;
$b64_index {"m"} = 38;
$b64_index {"n"} = 39;
$b64_index {"o"} = 40;
$b64_index {"p"} = 41;
$b64_index {"q"} = 42;
$b64_index {"r"} = 43;
$b64_index {"s"} = 44;
$b64_index {"t"} = 45;
$b64_index {"u"} = 46;
$b64_index {"v"} = 47;
$b64_index {"w"} = 48;
$b64_index {"x"} = 49;
$b64_index {"y"} = 50;
$b64_index {"z"} = 51;

sub b64_decode
{
    my $b = shift;
    my $sum = 0;
    for (split('', $b)) {
	die "invalid input '$b'" unless exists $b64_index{$_};
	$sum = $sum * 64 + $b64_index{$_};
    }
    return $sum;
}

    
my %word_def_info;
my %word_def_written;
my %word_file_map;

my $ascii_re = "^[\000-\177]+\$";
$ascii_re = qr($ascii_re);

sub get_normal_words($) 
{
    my $word = $_[0];
    unless ($word =~ m/$ascii_re/) {
	$word = shell_quote($word);
	chomp($word = qx(AsciiDammit.py $word));
    }
    $word =~ s/'//g;
    return $word;
}

my %word_normal_map;
my %word_normal_lc_map;
my %word_idx_map;
while (<$idx_file>) {
    chomp;
    my ($word, $offset, $len) = split("\t", $_);
    unless (exists $word_normal_lc_map{$word}) {
	$word_normal_lc_map{$word} = lc get_normal_words($word);
    }

    $offset = b64_decode($offset);
    $len = b64_decode($len);

    $word_idx_map{$word} = [] unless exists $word_idx_map{$word};
    push @{$word_idx_map{$word}}, [$offset, $offset + $len];
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
