#!/usr/bin/env perl

use strict;
use Encode;
use Fcntl qw(SEEK_SET);
use File::Path;

sub read_string($) {
  (my $file) = @_;
  my ($res, $chr);

  while (read($file, $chr, 1)) {
    if ($chr eq "\0") {
      return $res;
    }
    $res .= $chr
  }

  return $res;
}

sub name_to_path($) {
  my ($name) = @_;
  $name =~ s#(%|/)#sprintf("%%%02x", ord($1))#ge;
  return $name;
}

sub path_to_name($) {
  my ($path) = @_;
  $path =~ s#%(..)#chr(hex("0x$1"))#ge;
  return $path;
}

sub read_int($) {
  (my $file) = @_;
  my $buf;

  die "can not read 4 bytes"
    if (read($file, $buf, 4) != 4);

  return unpack("N", $buf);
}

sub md5sum($) {
  my ($data) = @_;
  use Digest::MD5;
  my $md5 = Digest::MD5->new;
  $md5->add($data);
  return $md5->hexdigest;
}

open(my $idx_file, "<", $ARGV[0]) or die "can not open " . $ARGV[0];
open(my $data_file, "<", $ARGV[1]) or die "can not open " . $ARGV[1];


my %word_freqs;
my %defines;
while (1) {
  my $key = read_string($idx_file) or last;
  $key =~ s,&#(.*?);,encode_utf8(chr($1)),eg;
  my $key_path = name_to_path($key);
  my $key_md5 = md5sum(lc $key);
  my ($d1, $d2, $d3) = (split('', $key_md5))[0..2];

  mkpath("$d1/$d2/$d3");

  my $start = read_int($idx_file);
  my $size = read_int($idx_file);

  seek($data_file, $start, SEEK_SET);
  my $data;
  read($data_file, $data, $size) != $size and die "can not read from data file for $size bytes at offset $start";

  #width='-34' height='-1'>

  $data =~ s/width='.*?' height='.*?'>/>/g;
  $data =~ s/&#147;/&#8220;/g;
  $data =~ s/&#148;/&#8221;/g;

  my $data_strip = lc $data;
  $data_strip =~ s#^<B>.*?</B> \(.*?\)##;
  $data_strip =~ s/<.*?>/ /g;
  $data_strip =~ s,&#(.*?);,encode_utf8(chr($1)),eg;
  for (split(/(?:\s|\[|\]|\(|\)|;|:|,|“|”)+/, $data_strip)) {
    next unless $_;
    next if m/ə|ā|ē|ō|â|ä|ô|û|ŏ|ĭ|ă|ě|à|á|æ|å/;
    s/·//g;
    s/\.$//;
    $word_freqs{$_} = 0 unless exists $word_freqs{$_};
    $word_freqs{$_}++;

    $defines{$_} = {} unless exists $defines{$_};
    $defines{$_}{$key} = 1;
  }
  open(my $word_file, ">", "$d1/$d2/$d3/$key_path") or die "can not open $key for writing";
  print $word_file $data;
  close($word_file);
  print "done key $key\n";
}

open(my $file_freq, ">", "word_freq.txt") or die "can not open word freq file for writing";
for (sort { $word_freqs{$a} <=> $word_freqs{$b} } keys %word_freqs) {
  printf $file_freq "%s %d\n", $_, $word_freqs{$_};
}
close($file_freq);

open (my $file_defines, ">", "word_defines.txt") or die "can not open word defines file for writing";
for (sort { $a cmp $b } keys %defines) {
  my $key = $_;

  if (keys %{$defines{$key}} > 5000) {
    print "should skip define key $key as it is too common\n";
    next;
  }

  printf $file_defines "%s: %s\n", $key, join(':', sort { $a cmp $b } keys %{$defines{$key}});
  print "done define key $key\n";
}
close($file_defines);

print 'all done.\n'
