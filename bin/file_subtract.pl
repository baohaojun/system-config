#!/usr/bin/env perl



while ($line=<>) {
	chomp $line;
	
	
	$match=0;
	#print "line is $line\n";
	open(subfile, $ENV{subfile}) or die "Can't open $ENV{subfile}\n";
	while (<subfile>) {
	    chomp;
	    #print "compare $line and $_\n";
	    if ($line eq $_) {
		$match=1;
		break;
	    }
	}
	close($subfile);

	if (! $match) {
	    print $line."\n";
	}
}

