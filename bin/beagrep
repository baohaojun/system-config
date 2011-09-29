#!/usr/bin/env perl

use Getopt::Long;
use strict;
use String::ShellQuote;

my $pat = '';
my $pathpat = '.';
my $pathdeny = '^$';
chomp(my $pwd =qx/pwd/);
my $pwd_pat='no such componet';
my $list_beagle;

my $ignore_case;
my $find_file_only = 0;
my $file_default_line = 1;
my $file_tag = "";

open(my $log, ">>", glob("~/.beagrep.log")) or die "Error openning log";
print $log "$0 @ARGV in " . $ENV{PWD} . "\n";

GetOptions(
           "e=s" => \$pat,
           "p=s" => \$pathpat,
           "v=s" => \$pathdeny,
           "d=s" => \$pwd_pat,
           "i!" => \$ignore_case,
           "f!" => \$find_file_only,
           "l!" => \$list_beagle,
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

if ($find_file_only) {
  if ($pat =~ m/:([0-9]+)$/) {
      $file_default_line = $1;
      $pat = substr($pat, 0, rindex($pat, ':'));
      if ($pat =~ m/(.*)\(/) {
          $file_tag = $1;
          $pat = substr($pat, index($pat, '(') + 1);
      }
      $pat_save = $pat;
  }
  $pat =~ s,.*/,,; #beagle can only find a filename, not a path name.
  $pat_save = $pat;
}

$pat_save =~ s/\\//g; #if it's a path name, it's unlikely to have `\' in it, maybe added by .emacs


$pat = shell_quote($pat);
print "pat is: $pat.\n";

system("which beagle-break.exe") == 0
  or die "Error: can not find beagle-break.exe in your $ENV{PATH}, it should be set in your .sawfishrc";

print "beagle query argument `[01;31m"
  . substr(qx/get_longest_token $pat/, 0, -1) 
  . "[0m'\n";

my $beagle_files = qx/my-beagle $pat/;
my @beagle_files = split(/\n/, $beagle_files);

if ($list_beagle) {
  for (@beagle_files) {
    print "$_:$file_default_line: [01;31m****************![0m\n";
  }
  exit 0;
}

if (!$find_file_only) {
  open(my $grep, "|xargs grep -H -n -I $ignore_case -e $pat");

  for (@beagle_files) {
    next unless m/$pathpat/i;
    next if m/$pathdeny/i;
    print $grep "$_\n";
  }
  close $grep;
}

#see [[http://baohaojun.wordpress.com/2010/05/06/beagle-grep-sh%E5%8A%9F%E8%83%BD%E5%8F%88%E5%8F%98%E5%BC%BA%E5%A4%A7%E4%BA%86/]] for why
if ($find_file_only || 1) {
    my %set;
    for (@beagle_files) {
        $set{$_} = 1 if m/$pat_save/i and m/$pathpat/i;
    }
    if ($find_file_only && keys %set > 1 && $file_tag) {
        my $files = qx(grep-gtags -e $file_tag|perl -ne "if (m/:[0-9]+:/) {s/:[0-9]+:.*//; print}"|sort -u);
        my @files = split("\n", $files);
        for (@files) {
            print "$_:$file_default_line: [01;31m****************![0m\n" 
                if $set{$_} and m,/$pat_save$,; # the `and' part is to make file name match totally
        }
    } else {
        for (@beagle_files) {
            print "$_:$file_default_line: [01;31m****************![0m\n" if $set{$_};
        }
    }
}
