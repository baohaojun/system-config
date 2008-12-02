#!/usr/bin/perl -i

$l=-1;

while (<>) {

    $l++;

    if ($l==0)
    { 
	print $_;
	next;
    }


    if ($l%2) 
    {
	$_  =~ m/\d+\.pos=(\d+)/;
	$pos=$1;
    }

    if (!($l%2)) {
	$_ =~ m/\d+\.limit=(\d+)/;
	$limit=$1;
	if ($limit <= $pos) {
	    $l -= 2;
	    next;
	}
	
	$x=$l-2;
	$y=($pos+$limit);
	if ($y%2) {
	    $y+=1;
	}

	$y/=2;
	print "$x".".pos="."$pos"."\n";
	print "$x".".limit=".$y."\n";
	$x++;
	print "$x".".pos=".$y."\n";
	print "$x".".limit="."$limit"."\n";
    }
}

