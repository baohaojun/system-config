#!/usr/bin/perl

sub relative_url($$) {
  my ($s, $t)  = @_;
  my @s = split("", $s);
  my @t = split("", $t);

  my $n = 0;
  my $last_slash = 0;
  while (@s and @t) {
    if ($s[0] eq $t[0]) {
      $n ++;
      $last_slash = $n if $s[0] eq "/";
      shift @s;
      shift @t;
    } else {
      last;
    }
  }

  my $relative_s = substr($s, $last_slash);
  my $remaining_t = substr($t, $last_slash);

  $remaining_t = grep(m:/:, split("", $remaining_t));
  $relative_s = "../" x $remaining_t . $relative_s;
  return $relative_s;
}

print relative_url($ARGV[0], $ARGV[1]);
