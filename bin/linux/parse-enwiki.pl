#!/usr/bin/env perl
use XML::Parser;
use strict;

my $p = XML::Parser->new(Style => 'Stream', Pkg => 'MySubs');
{ 
    no strict;
    $p->parse(STDIN);
}

{
  package MySubs;
  use utf8;
  use strict;
  use Encode;
  my ($is_text, $is_title, $title, $file);

  sub StartTag {
    my ($e, $name) = @_;
    if ($name eq "title") {
      $is_title = 1;
    } elsif ($name eq "text") {
      $is_text = 1;
    }

  }

  sub EndTag {
    my ($e, $name) = @_;
    if ($name eq "text") {
      $is_text = 0;
    } elsif ($name eq "title") {
      $is_title = 0;
    }
  }

  sub title2path {
      my ($title) = @_;
      $title =~ s!(%|/)!sprintf("%%%02x", ord($1))!ge;

      my $dir = substr($title, 0, 2);
      -d $dir or system("mkdir", "--", "$dir");
      return "$dir/$title";
  }

  sub Text {
    if ($is_text) {
        if (substr($_, 0, 12) eq "#REDIRECT [[") {
            my $target = "../" . title2path(substr($_, 12, index($_, "]]")-12));
            symlink $target, title2path($title);
        } else {
            open($file, ">", title2path($title)) or die ("Error open file " . title2path($title));
            print $file $_;
            close($file);
        }
    }
    if ($is_title) {
        $title = title2path($_);
    }
  }
}
