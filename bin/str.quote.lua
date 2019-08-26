#!/usr/bin/env perl
use strict;
use List::Util qw(max);

sub quote_lua_str($) {
  my $text = $_[0];
  my $max_quote = 0;
  while ($text =~ m,[\[\]](=*)[\[\]],g) {
    $max_quote = max($max_quote, length $1);
  }

  $max_quote += 1;

  my $quote = "=" x $max_quote;
  return sprintf("[%s[%s]%s]", $quote, $text, $quote);
}

print quote_lua_str($ARGV[0]);
