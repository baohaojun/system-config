#!/usr/bin/env perl

use Getopt::Long;
use strict;
use String::ShellQuote;

my $pat = '';
my $pathpat = ();
chomp(my $pwd =qx/pwd/);
my $pwd_pat='no such componet';

my $ignore_case;

GetOptions(
           "e=s" => \$pat,
           "p=s" => \$pathpat,
           "d=s" => \$pwd_pat,
           "i!" => \$ignore_case,
          );

if ($ignore_case) {
  $ignore_case = '-i';
} else {
  $ignore_case = '';
}

die 'no regexp specified' unless $pat;
$pwd =~ s!($pwd_pat.*?)/.*!$1!; 
chdir $pwd or die "$pwd: $!";

$pat = shell_quote($pat);
print "pat is `$pat'\n";

system("which beagle-break.exe") == 0
  or die "Error: can not find beagle-break.exe in your $ENV{PATH}, it should be set in your .sawfishrc";

print "beagle query argument `[01;31m"
  . substr(qx/get_longest_token $pat/, 0, -1) 
  . "[0m'\n";

my $beagle_files = qx/my-beagle $pat/;
my @beagle_files = split(/\n/, $beagle_files);


open(my $grep, "|xargs grep -H -n -I $ignore_case -e $pat");

for (@beagle_files) {
  next unless m/$pathpat/i;
  print $grep "$_\n";
}
close $grep;

#see [[http://thomasbhj.spaces.live.com/blog/cns!FC1463FF7BEF1F15!243.entry]] for why
for (@beagle_files) {
  print "$_:1: [01;31m****************![0m\n" if m/$pat/;
}
