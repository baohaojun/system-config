#!/usr/bin/env perl

use strict;
use File::Path;
use Encode;

chdir glob("~/external/ahd") or die "can not chdir into ahd";
my %entries;

# <B><FONT SIZE="-1" FACE="arial,sans-serif"><FONT
# COLOR="#229966">a</FONT></FONT></B><SUP><B><FONT SIZE="-1"
# FACE="arial,sans-serif"><FONT COLOR="#229966">1</FONT></FONT></B></SUP> or
# <B><FONT SIZE="-1" FACE="arial,sans-serif">A</FONT></B>

my $entry_main_re = '<B><FONT SIZE="-1" FACE="arial,sans-serif"><FONT COLOR="#229966">(.+?)</FONT></FONT></B>';
my $sup_re = '<SUP><B><FONT SIZE="-1" FACE="arial,sans-serif"><FONT COLOR="#229966">(\d+)</FONT></FONT></B></SUP>';
my $or_re = '\s+or\s+<B><FONT SIZE="-1" FACE="arial,sans-serif">(.+?)</FONT></B>';

my $entry_re = qr($entry_main_re(?:$sup_re)?(?:$or_re)?)o;
my $remove_pron_re = qr#<A HREF="[^"]*?.wav"><IMG BORDER="0" SRC="[^"]*?JPG/pron.jpg"></A>#o;
my $remove_banner_re = qr#<BR><FONT SIZE=2><I>The American Heritage. Dictionary of the English Language, Fourth Edition.</I> Copyright . 2002, 2000 by Houghton Mifflin Company. Published by Houghton Mifflin Company. All rights reserved.</FONT>#o;


sub debug(@) {
  print STDERR "@_\n";
}

sub get_md5($) {
  my ($data) = @_;
  $data = lc $data;
  $data =~ s/\(\d+\)$//;
  $data =~ s/-//g;
  return get_md5_raw($data);
}

sub read_file($) {
  my ($path) = @_;
  open(my $file, "<", $path) or die "can not open $path";
  read($file, my $data, (stat $file)[7]) == (stat $file)[7] or die "can not read $path";
  close($file);
  return $data;
}

sub get_md5_raw($) {
  my ($data) = @_;
  use Digest::MD5;
  my $md5 = Digest::MD5->new;
  $md5->add($data);
  return $md5->hexdigest;
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

sub get_md5_dir($) {
  my $key_md5 = get_md5($_[0]);
  my ($d1, $d2, $d3) = (split('', $key_md5))[0..2];
  return "$d1/$d2/$d3";
}

my %word_freqs;
my %defines;
my %words;
my %lc_words;

for (0..255) {
  my $prefix = sprintf "%02x", $_;
  for (glob("$prefix/*.htm")) {
    my $htm_file = $_;
    open(my $of, "<", $_) or die "can not open original file";
    read($of, my $data, (stat $of)[7]) == (stat $of)[7] or die "can not read $_";
    close($of);
    my @entries = split(/<FONT COLOR=>/, $data);
    if (@entries > 2) {
      for (@entries) {
	next unless $_;
	$_ = "<FONT COLOR=>" . $_;
	my $md5 = get_md5_raw($_);
	my $md5_path = $md5 . ".htm";
	$md5_path =~ s#^(..)#$1/#;
	open(my $nf, ">", $md5_path) or die "can not open $md5_path";
	debug "$htm_file generated $md5 " . scalar @entries;
	print $nf $_;
	close($nf);
      }
      unlink $htm_file;
    }
  }
}

for (0..256) {
  my $prefix = sprintf "%02x", $_;

  for (glob("$prefix/*.htm")) {
    my $htm_file = $_;
    open(my $of, "<", $_) or die "can not open original file";
    read($of, my $data, (stat $of)[7]) == (stat $of)[7] or die "can not read $_";
    close($of);
    open(my $nf, ">", $_) or die "can not open fixing file";
    for (split(/\n/, $data)) {
      $_ .= "\n";
      if (m/$entry_re/) {
	for (($1, $3)) {
	  my $entry_word = $_;
	  if ($entry_word) {
	    $entry_word =~ s#<FONT FACE="Minion New">([^<]*?)</FONT>#$1#g;
	    $entry_word =~ s#<SUB>(.*?)</SUB>#$1#g;
	    $entry_word =~ s#<FONT SIZE="-1">(.*?)</FONT>#$1#g;
	    $entry_word =~ s/&#183;//g;
	    $entry_word =~ s,&#([^;]*?);,encode_utf8(chr($1)),eg;
	    $entries{$entry_word} = {} unless exists $entries{$entry_word};
	    $entries{$entry_word}{$htm_file} = 1;
	    $lc_words{lc $entry_word} = 1;
	  }
	}
      }
      s#$remove_pron_re##g;
      s#$remove_banner_re##g;

      #<A HREF="PrefixToTheXREFk_a=..article&amp;view=Search&amp;fileName=R073.htm&amp;articleID=R0338500">rude</A>
      s#<A HREF="PrefixToTheXREFk_a=..article&amp;view=Search&amp;fileName=[^"]*?.htm&amp;articleID=[^"]*?">([^<]+?)</A>#$1#g;

      #<A HREF="PrefixToTheXREFk_a=..article&amp;view=Search&amp;fileName=bend.htm"><IMG BORDER="0" SRC="PrefixToTheXREFk_a=..article&amp;view=Search&amp;fileName=JPG/THbend.jpg"></A>
      s#<A HREF="PrefixToTheXREFk_a=..article&amp;view=Search&amp;fileName=[^"]*?.htm"><IMG BORDER="0" SRC="PrefixToTheXREFk_a=..article&amp;view=Search&amp;fileName=JPG/([^"]*?).jpg"></A>#<A HREF="/dict-images/LA$1.jpg"><IMG BORDER="0" SRC="/dict-images/$1.jpg"></A>#g;
      print $nf $_;
      print "finished $htm_file\n";
    }
    close($nf);
  }
}

open(my $list_file, ">", "words.list") or die "can not open word list file";
for my $entry_word (keys %entries) {
  debug "doing $entry_word";
  my $entry_key = $entry_word;

  print $list_file "$entry_word\n";

  my $key_path = name_to_path($entry_word);
  my $md5_dir = get_md5_dir($entry_word);
  mkpath($md5_dir);

  open(my $word_file, ">", "$md5_dir/$key_path") or die "can not open $entry_word for writing";

  for my $def_file (keys $entries{$entry_key}) {
    print $word_file "$def_file\n";
    my $data;
    my $size = (stat($def_file))[7];
    open(my $data_file, "<", $def_file) or die "can not open $def_file";
    read($data_file, $data, $size) != $size and die "can not read from data file for $size bytes";
    close($data_file);

    #width='-34' height='-1'>

    $data =~ s/width='[^>]*?' height='[^>]*?'>/>/g;
    $data =~ s/&#147;/&#8220;/g;
    $data =~ s/&#148;/&#8221;/g;

    my $data_strip = lc $data;
    $data_strip =~ tr/\n\r/  /;
    $data_strip =~ s/<[^>]*?>/ /g;
    $data_strip =~ s/&#183;//g;
    $data_strip =~ s,&#([^;]*?);,encode_utf8(chr($1)),eg;
    for (split(/(?:\s|\[|\]|\(|\)|;|:|,|“|”)+/, $data_strip)) {
      next unless $_;
      s/·//g;
      if (not exists $lc_words{$_}) {
	if (substr($_, -2) eq "ed" or substr($_, -2) eq "es") {
	  $_ = substr($_, 0, -1);
	  if (not exists $lc_words{$_}) {
	    $_ = substr($_, 0, -1);
	    if (not exists $lc_words{$_}) {
	      next;
	    }
	  }
	} elsif (substr($_, -1) eq "s" or substr($_, -1) eq ".") {
	  $_ = substr($_, 0, -1);
	  if (not exists $lc_words{$_}) {
	    next;
	  }
	} elsif (substr($_, -1) eq "ing") {
	  $_ = substr($_, 0, -3);
	  if (not exists $lc_words{$_}) {
	    $_ .= "e";
	    if (not exists $lc_words{$_}) {
	      next;
	    }
	  }
	} else {
	  $_ = substr($_, 0, -1);
	  if (not exists $lc_words{$_}) {
	    next;
	  }
	}
      }
      #next if m/ə|ā|ē|ō|â|ä|ô|û|ŏ|ĭ|ă|ě|à|á|æ|å/;

      $word_freqs{$_} = 0 unless exists $word_freqs{$_};
      $word_freqs{$_}++;

      $defines{$_} = {} unless exists $defines{$_};
      $defines{$_}{$entry_word} = 1;
    }
  }
  close($word_file);
}
close($list_file);
open(my $file_freq, ">", "word_freq.txt") or die "can not open word freq file for writing";
for (sort { $word_freqs{$a} <=> $word_freqs{$b} } keys %word_freqs) {
  printf $file_freq "%s %d\n", $_, $word_freqs{$_};
}
close($file_freq);

open (my $file_defines, ">", "word_defines.txt") or die "can not open word defines file for writing";
for (sort { $a cmp $b } keys %defines) {
  my $key = $_;

  if (keys %{$defines{$key}} > 5000) {
    debug "should skip define key $key as it is too common\n";
    next;
  }

  printf $file_defines "%s: %s\n", $key, join(':', sort { $a cmp $b } keys %{$defines{$key}});
}
close($file_defines);

debug 'all done.\n'
