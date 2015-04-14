#!/usr/bin/perl

use strict;
use Encode;
use Fcntl qw(SEEK_SET);
use File::Path;
use URI::Escape;
system("which tidy >/dev/null 2>&1") == 0 or die "tidy(1) not found";
open(STDOUT, "|-", "tidy 2>/dev/null") or die "can not tidy up";
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
      for (sort { $a cmp $b } keys %{$syn_words{$key}}) {
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

<table>
<tr>
<td>
<form name="SearchForm" action="/search-dict/">
Search dict:
<input type="text" name="data" size="15">
<input type=submit value="Submit" >
</form>
</td><td>
<form name="SearchForm" action="/search-dict-defines/">
Search defines:
<input type="text" name="data" size="15">
<input type=submit value="Submit" >
</form>
</td><td>
<form name="SearchForm" action="/search-dict-matching/">
Search matching
<input type="text" name="data" size="15">
<input type=submit value="Submit" >
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

sub get_definition($) {
  my $word = $_[0];

  debug "get definition for $word\n";
  my $key_md5 = get_md5($word);
  my $md5_dir = get_md5_dir($word);
  debug "$md5_dir\n";

  my $dict_html = "";
  my %entry_html_files;
  for (glob("$md5_dir/* $md5_dir/.*")) {
    -f $_ or next;

    if (get_md5(substr $_, 6) eq $key_md5) {
      open(my $entry_file, "<", $_) or die "can not open $_ for read";
      while (<$entry_file>) {
	debug "$word is defined in $_";
	chomp();
	$entry_html_files{$_} = 1;
      }
    }
  }

  my $num_of_def = 0;
  my %entry_md5s;
  for (sort {-s $b <=> -s $a} keys %entry_html_files) {
    my $dict_data = qx(cat $_|tr -d "\r");
    unless (exists $entry_md5s{$dict_data}) {
      $dict_html .= "<hr class='sep' />";
      $entry_md5s{$dict_data} = 1;
      $dict_html .= qx(cat $_);
    }
  }

  return "<div style=\"font-size: xx-large\">$dict_html</div>";
}

sub dict() {
  my $word = $ARGV[0];
  my $dict_html = get_definition($word);
  my $word_path = name_to_path($word);
  
  print $style;
  print $dict_html;
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
      my $prev_idx = ($next_idx - 2 + @words) % @words;
      my $next_display = $words[$next_idx];
      my $prev_display = $words[$prev_idx];
      my $a = uri_escape($next_display);
      my $b = uri_escape($prev_display);
      
      $prev_display = decode_utf8($prev_display);
      $next_display = decode_utf8($next_display);
      
      $prev_display =~ s!([^a-z0-9])!sprintf "&#%d;", ord($1)!egi;
      $next_display =~ s!([^a-z0-9])!sprintf "&#%d;", ord($1)!egi;

      $prev_display = encode_utf8($prev_display);
      $next_display = encode_utf8($next_display);
      print "<br><a href='/dict-defines-sub/$defining_word/$b'>prev: $prev_display</a>";
      print "&nbsp;<a href='/dict-defines-sub/$defining_word/$a'>next: $next_display</a><br/><br/><hr><br/>";
      last;
    }
    for (@words) {
      my $a = uri_escape($_);
      $_ = decode_utf8($_);
      $_ =~ s!([^a-z0-9])!sprintf "&#%d;", ord($1)!egi;
      $_ = decode_utf8($_);
      print "<a href='/dict-defines-sub/$defining_word/$a'>$_</a> "
    }
  }
}

sub dict_matching() {
  my $pattern = $ARGV[0];
  my $filter;
  my $filter_save;
  my $saved_pattern;
  if (-e glob("~/.cache/system-config/logs/dict_matching.txt")) {
    open($filter, "<", glob("~/.cache/system-config/logs/dict_matching.txt")) or die "can not open dict_matching.txt";
    chomp($saved_pattern = <$filter>);
  }

  unless($pattern eq $saved_pattern) {
    close($filter) if $filter;
    open($filter, "-|", "grep", "-i", "-P", "$pattern", "words.list") or die "can not open filter";
    open($filter_save, ">", glob("~/.cache/system-config/logs/dict_matching.txt")) or die "can not open dict_matching.txt";
    print $filter_save "$pattern\n";
  }


  my $num_matching = 0;
  my @words;
  print $style;
  while (<$filter>) {
    last unless $num_matching++ < 5000;
    print $filter_save $_ if $filter_save;
    chomp;
    push @words, $_;
  }
  print "<h2>" . $pattern . " matches " . scalar(@words) . " words</h2>";
  my $next_idx = 0;
  @words = sort {lc $a cmp lc $b} @words;

  my $to_display = $words[0];
  if (@ARGV == 2) {
    $to_display = $ARGV[1] unless not $ARGV[1];
  }

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

    my $prev_idx = ($next_idx - 2 + @words) % @words;
    my $next_display = $words[$next_idx];
    my $prev_display = $words[$prev_idx];
    my $a = uri_escape($next_display);
    my $b = uri_escape($prev_display);

    $prev_display = decode_utf8($prev_display);
    $next_display = decode_utf8($next_display);
    
    $prev_display =~ s!([^a-z0-9])!sprintf "&#%d;", ord($1)!egi;
    $next_display =~ s!([^a-z0-9])!sprintf "&#%d;", ord($1)!egi;

    $prev_display = encode_utf8($prev_display);
    $next_display = encode_utf8($next_display);

    debug "prev_display is $prev_display";
    print "<br><a href='/dict-matching-sub/$pattern/$b'>prev: $prev_display</a>";
    print "&nbsp;<a href='/dict-matching-sub/$pattern/$a'>next: $next_display</a><br/><br/><hr><br/>";
    last;
  }
  for (@words) {
    my $a = uri_escape($_);
    $_ = decode_utf8($_);
    $_ =~ s!([^a-z0-9])!sprintf "&#%d;", ord($1)!egi;
    $_ = encode_utf8($_);
    print "<a href='/dict-matching-sub/$pattern/$a'>$_</a> "
  }
  close($filter_save) if $filter_save;
  close($filter);
}

chdir(glob("~/external/ahd/")) or die "can not chdir";
if ($0 =~ m,/?stardict-convert.pl$,) {
  do_convert();
} elsif ($0 =~ m,/?dict$,) {
  dict();
} elsif ($0 =~ m,/?dict-defines$,) {
  dict_defines();
} elsif ($0 =~ m,/?dict-matching$,) {
  dict_matching();
}
close(STDOUT);
