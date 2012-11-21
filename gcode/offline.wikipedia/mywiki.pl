#!/usr/bin/perl -w
use strict;

sub ShowTopic {
    (my $found, my $foundLine) = @_;
    my $regexp = $foundLine;
    open RESULT, ">result";
    print RESULT "$foundLine\n";
    my $getText = 0;
    my $insideText = 0;
    my $foundEndText = 0;
    while(1) {
	open XML, "bzip2 -cd wiki-splits/\"$found\" |";
	while(<XML>) {
	    if (/<title>\Q$regexp\E<\/title>/) {
		$getText = 1;
	    }
	    if (/<text ?[^>]*>(.*)<\/text>/) {
		if ($getText) { 
		    print RESULT $1."\n"; 
		    $getText = 0; 
		    $foundEndText = 1;
		}
	    } 
	    elsif (/<text ?[^>]*>(.*)/) {
		if ($getText) { 
		    print RESULT $1."\n"; 
		    $insideText = 1; 
		}
	    }
	    if (($insideText) && (/(.*?)<\/text>$/)) {
		print RESULT $1."\n";
		$insideText = 0;
		$getText = 0;
		$foundEndText = 1;
	    } elsif ($insideText) {
		print RESULT $_;
	    }
	}
	close XML;
	if ($foundEndText) {
	    last;
	} else {
	    # Need the rest from the next bzip2 volume
	    $found =~ m/rec(\d\d\d\d\d)/;
	    my $nextNum = $1 + 1;
	    $nextNum = sprintf "%05d", $nextNum;
	    $found =~ s/rec\d\d\d\d\d/rec$nextNum/;
	}
    }
    close RESULT;
    my $p = `pwd`;
    chop $p;
    system("cd mediawiki_sa && php5 testparser.php ../result > ../result.html && cd .. && firefox file://$p/result.html");
}

die "Usage: $0 keyword1 <keyword2> ...\n"
    unless @ARGV >= 1;

my $cmd = "wiki-sorted-idx-title-query ";
while (@ARGV) {
    $cmd .= '"'.lc($ARGV[0]).'" ';
    shift;
}

my @percentages;
my @founds;
my @foundLines;

open DATA, "$cmd |";
while(<DATA>) {
    chomp;
    if (/^(\d+)%\s\[(rec[^:]+):(.+?)\]$/) {
	push @percentages, $1;
	push @founds, $2;
	push @foundLines, $3;
    }
}
close DATA;

if (scalar @founds == 0) {
    die "Nothing found.\n";
}

my $found = undef;
my $foundLine = undef;

if (scalar @founds == 1) {
    $found = $founds[0];
    $foundLine = $foundLines[0];
    ShowTopic($found, $foundLine);
} else {
    while(1) {
	my $i = undef;
	print "0: (abort)\n";
	for ($i=0; $i<@founds; $i++) {
	    print $i+1;
	    print ": (".$percentages[$i]."%) ".$foundLines[$i]."\n";
	}
	my $ans;
	while(1) {
	    print "\nSelect a number: ";
	    $ans=<>;
	    last if $ans =~ m/^\d+$/;
	}
	
	if (($ans>=1) && ($ans<=@founds)) {
	    $found = $founds[$ans - 1];
	    $foundLine = $foundLines[$ans - 1];
	    print "Selected:\n";
	    print "\t$foundLine\n";
	    print "\t$found\n";
	    ShowTopic($found, $foundLine);
	} elsif ($ans == 0) {
	    last;
	}
    }
}
