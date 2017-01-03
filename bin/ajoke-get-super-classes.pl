#!/usr/bin/perl

open(my $debug, ">", glob("~/.cache/system-config/logs/ajoke-get-super-classes.log"))
    or die "Can not open debug log file ~/.cache/system-config/logs/ajoke-get-super-classes.log";
sub debug(@) {
    print $debug "@_\n";
}

if (@ARGV != 2) {
    die "Usage: $0 qclass file";
}

my ($qclass, $file) = @ARGV;
die "$file not exist" unless -e $file;

my $cache = "$ENV{HOME}/.cache/code-reading-cache$ENV{PWD}/$file";
unless (-e $cache) {
    my $dir = $cache;
    $dir =~ s,(.*)/.*,$1,;
    system("mkdir -p $dir");
}
system("test $cache -nt $file || flatten.pl $file > $cache");
system("cat $cache");

