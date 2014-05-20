#!/usr/bin/perl
use String::ShellQuote;
use strict;
use BhjJava;

my $resolve = shift @ARGV;
my $stem = $resolve;
$resolve = shell_quote($resolve);
open(my $pipe, "-|", "grep-gtags -e $resolve -t 'interface|class' 2>/dev/null") or die "Can not open grep-gtags";

while (<$pipe>) {
    chomp;
    m/<(.*)> : (.*)/ or next;
    my ($tag, $prot) = ($1, $2);
    print $tag;
    last;
}
