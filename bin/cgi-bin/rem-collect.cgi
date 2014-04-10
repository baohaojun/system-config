#!/usr/bin/perl
use CGI;

sub debug(@) {
    # print "@_\n";
}

my $q = CGI->new;
print $q->header('text/plain');

my %params = $q->Vars;
for my $param (keys %params) {
    print "param $param is $params{$param}\n"
}
