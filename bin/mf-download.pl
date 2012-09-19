#!/usr/bin/env perl

use strict;
use Getopt::Long;

exit if $ENV{DRYRUN} eq "true";
my $nthreads = 3;

GetOptions(
    "n=i" => \$nthreads,
    );

die "Error: wrong number of threads: must >= 1" if $nthreads < 1;
my @childrens = ();
my %config = (
    nthreads => $nthreads,
    var_path => '.',
    limit_rate  => '100m',
    );

my %pkgs = ();
sub get_variable {
    my $key = shift;
    return $config{$key};
}

sub download_urls {
    my $stage = shift;
    my @urls;
    my $i = 0;
    my $pid;
    my $nthreads = get_variable("nthreads");
    local $| = 1;

    @urls = @_;
    $nthreads = @urls if @urls < $nthreads;

    print "Downloading " . scalar(@urls) . " $stage files using $nthreads threads...\n";
    
    while(scalar @urls) {
	my @part = splice(@urls, 0, int(@urls / $nthreads));
	open URLS, ">" . get_variable("var_path") . "/$stage-urls.$i" or die("apt-mirror: can't write to intermediate file ($stage-urls.$i)");
	foreach (@part) { print URLS "$_\n"; }
	close URLS or die("apt-mirror: can't close intermediate file ($stage-urls.$i)");
	
	$pid = fork();
	
	die("apt-mirror: can't do fork in download_urls") if $pid < 0;
	
	if($pid == 0) {
	    exec 'wget', '--no-cache', '--limit-rate='.get_variable("limit_rate"), '-t', '5', '-r', '-N', '-l', 'inf', '-o', get_variable("var_path") . "/$stage-log.$i", '-i', get_variable("var_path") . "/$stage-urls.$i";
	    # shouldn't reach this unless exec fails
	    die("\n\nCould not run wget, please make sure its installed and in your path\n\n");
	}
	
	push @childrens, $pid;
	$i++; $nthreads--;
    }

    print "Begin time: " . localtime() . "\n[" . scalar(@childrens) . "]... ";
    while(scalar @childrens) {
	my $dead = wait();
	@childrens = grep { $_ != $dead } @childrens;
	print "[" . scalar(@childrens) . "]... ";
    }
    print "\nEnd time: " . localtime() . "\n\n";
}

while (<>) {
    chomp;
    $pkgs{$_} = 1;
}

download_urls('cygwin', keys %pkgs);

