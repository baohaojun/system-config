#!/usr/bin/env perl

use Getopt::Long;
use strict;
use String::ShellQuote;

my $pat = '';
my $pathpat = '.';
chomp(my $pwd =qx/pwd/);
my $pwd_pat='no such componet';

my $ignore_case;
my $find_file_only = 0;

GetOptions(
           "e=s" => \$pat,
           "p=s" => \$pathpat,
           "d=s" => \$pwd_pat,
           "i!" => \$ignore_case,
           "f!" => \$find_file_only,
          );

if ($ignore_case) {
  $ignore_case = '-i';
} else {
  $ignore_case = '';
}


die 'no regexp specified' unless $pat;
$pwd =~ s!($pwd_pat.*?)/.*!$1!; 
chdir $pwd or die "$pwd: $!";

my $pat_save = $pat;
$pat_save =~ s/\\//g; #if it's a path name, it's unlikely to have `\' in it, maybe added by .emacs

if ($find_file_only) {
  $pat =~ s,.*/,,; #beagle can only find a filename, not a path name.
}

$pat = shell_quote($pat);
print "pat is: $pat.\n";

system("which beagle-break.exe") == 0
  or die "Error: can not find beagle-break.exe in your $ENV{PATH}, it should be set in your .sawfishrc";

print "beagle query argument `[01;31m"
  . substr(qx/get_longest_token $pat/, 0, -1) 
  . "[0m'\n";

my $beagle_files = qx/my-beagle $pat/;
my @beagle_files = split(/\n/, $beagle_files);

if (!$find_file_only) {
  open(my $grep, "|xargs grep -H -n -I $ignore_case -e $pat");

  for (@beagle_files) {
    next unless m/$pathpat/i;
    print $grep "$_\n";
  }
  close $grep;
}

#see [[http://thomasbhj.spaces.live.com/blog/cns!FC1463FF7BEF1F15!243.entry]] for why
for (@beagle_files) {
  print "$_:1: [01;31m****************![0m\n" if m/$pat_save/ and m/$pathpat/;
}
