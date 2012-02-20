#!/usr/bin/env perl

use strict;
use Encode;
use Fcntl qw(SEEK_SET);
use File::Path;

sub debug(@) {
  print STDERR "@_";
}

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

sub get_md5($) {
  my ($data) = @_;
  $data = lc $data;
  $data =~ s/\(\d+\)$//;
  $data =~ s/-//g;

  use Digest::MD5;
  my $md5 = Digest::MD5->new;
  $md5->add($data);
  return $md5->hexdigest;
}

sub get_md5_dir($) {
  my $key_md5 = get_md5($_[0]);
  my ($d1, $d2, $d3) = (split('', $key_md5))[0..2];
  return "$d1/$d2/$d3";
}

sub do_convert() {
  open(my $idx_file, "<", $ARGV[0]) or die "can not open " . $ARGV[0];
  open(my $data_file, "<", $ARGV[1]) or die "can not open " . $ARGV[1];
  open(my $syn_file, "<", $ARGV[2]) or die "can not opne " . $ARGV[2];


  my %word_freqs;
  my %defines;
  my %words;
  my %lc_words;
  my %word_index;
  my %syn_words;

  my $index = 0;
  while (1) {
    my $key = read_string($idx_file) or last;
    $key =~ s,&#(.*?);,encode_utf8(chr($1)),eg;
    $word_index{$index++} = $key;
    my $start = read_int($idx_file);
    my $size = read_int($idx_file);
    $words{$key} = [$start, $size];
    $lc_words{lc $key} = 1;
  }

  while (1) {
    my $key = read_string($syn_file) or last;
    $key =~ s,&#(.*?);,encode_utf8(chr($1)),eg;
    $lc_words{lc $key} = 1;

    my $syn_idx = read_int($syn_file);
    my $syn_key = $word_index{$syn_idx};

    if (get_md5($syn_key) eq get_md5($key)) {
      debug "syn for $key is useless\n";
      next;
    }
    $syn_words{$key} = {} unless exists $syn_words{$key};
    $syn_words{$key}{$syn_key} = 1;
  }

  close($idx_file);
  close($syn_file);

  open(my $list_file, ">", "words.list") or die "can not open word list file";
  for my $key (sort { $a cmp $b } (keys %words, keys %syn_words)) {
    print $list_file "$key\n";
    my ($start, $size);
    if (exists $words{$key}) {
      ($start, $size) = @{$words{$key}};
    } else {
      ($start, $size) = (0, 0);
    }

    my $key_path = name_to_path($key);
    my $md5_dir = get_md5_dir($key);
    mkpath($md5_dir);

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
      $defines{$_}{$key} = 1;
    }
    open(my $word_file, ">", "$md5_dir/$key_path") or die "can not open $key for writing";
    print $word_file $data;
    if (exists $syn_words{$key}) {
      if ($data) {
	print $word_file "<hr/>";
      } else {
	print $word_file "<B>$key</B>";
      }
      print $word_file "Syn. ";
      for (sort { $a cmp $b } keys $syn_words{$key}) {
	print $word_file "$key&nbsp; ";
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
    debug "done define key $key\n";
  }
  close($file_defines);

  debug 'all done.\n'
}

my $bar = <<EOF;
<script type="text/javascript">
function DoDict(form)
{
top.location="/search-dict/"+form.data.value;
}
</script>
<script type="text/javascript">
function DoDictMatching(form)
{
top.location="/search-dict-matching/"+form.data.value;
}
</script><script type="text/javascript">
function DoDefines(form)
{
top.location="/search-dict-definesines/"+form.data.value;
}
</script>

<table>
<tr>
<td>
<form name="SearchForm" onSubmit="DoDict(this.form)" method="get" action="/search-dict/">
Search dict:
<input type="text" name="data" size="15">
<input type="button" value="Submit" onclick="DoDict(this.form)">
</form>
</td><td>
<form name="SearchForm" onSubmit="DoDefines(this.form)" method="get" action="/search-dict-definesines/">
Search defines:
<input type="text" name="data" size="15">
<input type="button" value="Submit" onclick="DoDefines(this.form)">
</form>
</td><td>
<form name="SearchForm" onSubmit="DoDictMatching(this.form)" method="get" action="/search-dict-matching/">
Search matching
<input type="text" name="data" size="15">
<input type="button" value="Submit" onclick="DoDictMatching(this.form)">
</form>
</td></tr></table>
EOF

my $style = '<style type="text/css">
    hr.sep {
      color: #00f;
      background-color: #00f;
      height: 5px;
    }
    hr.subsep {
      color: #00f;
      background-color: #00f;
      height: 3px;
    }
    </style>' . $bar;

sub dict() {
  my $word = $ARGV[0];
  my $dict_html = get_definition($word);
  my $word_path = name_to_path($word);

  print $style;
  print $dict_html;
}

sub get_definition($) {
  my $word = $_[0];

  debug "get definition for $word\n";
  my $key_md5 = get_md5($word);
  my $md5_dir = get_md5_dir($word);
  debug "$md5_dir\n";

  my $dict_html = "";
  for (glob("$md5_dir/* $md5_dir/.*")) {
    -f $_ or next;

    if (get_md5(substr $_, 6) eq $key_md5) {
      open(my $entry_file, "<", $_) or die "can not open $_ for read";
      $dict_html .= "<hr class='subsep' />";
      while (<$entry_file>) {
	s#\036#/dict-images/#g; s#\037##g;
	$dict_html .= $_;
      }
    }
  }
  return $dict_html;
}

sub dict_defines() {
  my $defining_word = $ARGV[0];
  open(my $filter, "-|", "grep", "-i", "-e", "^$defining_word: ", "word_defines.txt") or die "can not open filter";
  my $defining_word_path = name_to_path($defining_word);
  while (<$filter>) {
    chomp;
    m/: (.*)/;
    my @words = split(":", $1);
    @words = ($defining_word, grep {$_ ne $defining_word} sort { lc $a cmp lc $b } @words);
    print $style;
    print "<h2>" . $defining_word . " is defining " . scalar(@words) . " words</h2>";
    print "<hr>\n";
    for (@words) {
      print "<a href='/dict-defines-sub/$defining_word/$_'>$_</a> "
    }
    my $to_display = $words[0];
    if (@ARGV == 2) {
      $to_display = $ARGV[1] unless not $ARGV[1];
    }
    my $next_idx = 0;
    for (@words) {
      $next_idx++;
      if ($to_display ne $_) {
	next;
      }
      my $def = get_definition($_);
      $def =~ s/\bh1>/h3>/g;
      print $def;
      if ($next_idx == @words) {
	$next_idx = 0;
      }
      my $next_display = $words[$next_idx];
      print "<br><a href='/dict-defines-sub/$defining_word/$next_display'>next: $next_display</a>";
      last;
    }
  }
}

chdir("/home/bhj/external/stardict/") or die "can not chdir";
if ($0 =~ m,/?stardict-convert.pl$,) {
  do_convert();
} elsif ($0 =~ m,/?dict$,) {
  dict();
} elsif ($0 =~ m,/?dict-defines$,) {
  dict_defines();
}
