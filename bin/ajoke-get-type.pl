#!/usr/bin/perl
use String::ShellQuote;
use strict;
use BhjJava;

my $resolve = shift @ARGV;
my $stem = $resolve;
$resolve = shell_quote($resolve);
open(my $pipe, "-|", "grep-gtags -e $resolve") or die "Can not open grep-gtags";

while (<$pipe>) {
    chomp;
    m/<(.*)> : (.*)/ or next;
    my ($tag, $prot) = ($1, $2);
    if ($prot =~ m/($qualified_re) \Q$stem\E(\(|;)/) {
        print "$1 $stem : $_\n";
    }
}
