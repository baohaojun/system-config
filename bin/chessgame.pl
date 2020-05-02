#!/usr/bin/env perl

use English;

$a{r, jiang, 1}=[0,0];
$a{r, shi, 1}=[0,0];
$a{r, shi, 2}=[0,0];
$a{r, xiang, 1}=[0,0];
$a{r, xiang, 2}=[0,0];
$a{r, ma, 1}=[0,0];
$a{r, ma, 2}=[0,0];
$a{r, ju, 1}=[0,0];
$a{r, ju, 2}=[0,0];
$a{r, pao, 1}=[0,0];
$a{r, pao, 2}=[0,0];
$a{r, bing, 1}=[0,0];
$a{r, bing, 2}=[0,0];
$a{r, bing, 3}=[0,0];
$a{r, bing, 4}=[0,0];
$a{r, bing, 5}=[0,0];

$a{b, jiang, 1}=[0,0];
$a{b, shi, 1}=[0,0];
$a{b, shi, 2}=[0,0];
$a{b, xiang, 1}=[0,0];
$a{b, xiang, 2}=[0,0];
$a{b, ma, 1}=[0,0];
$a{b, ma, 2}=[0,0];
$a{b, ju, 1}=[0,0];
$a{b, ju, 2}=[0,0];
$a{b, pao, 1}=[0,0];
$a{b, pao, 2}=[0,0];
$a{b, bing, 1}=[0,0];
$a{b, bing, 2}=[0,0];
$a{b, bing, 3}=[0,0];
$a{b, bing, 4}=[0,0];
$a{b, bing, 5}=[0,0];

while (<>) {
    chomp;
    if (m/^(\d+)($|\s*)/) {
	$row=$1;
	$col=1;
	$left=$POSTMATCH;
	while ($left) {
	    $left = match_left($left);
	}
    } else {
	print stderr "invalid row\n";
    }
}

sub match_left($) {
    ($left_arg)=@_;
    
    if ($left_arg =~ m/^(\d|shuai|jiang|shi|xiang|ma|ju|pao|bing|zhu|r|b)($|\s*)/) {
	$match=$1;
	if ($match eq "shuai") {
	    $match = "jiang";
	} elsif ($match eq "zhu") {
	    $match = "bing";
	} 
	    
	if ($match >0 && $match <10) {
	    $col = $match;
	} elsif ($match eq "r" or $match eq "b") {
	    $color = $match;
	} else {
	    for ($n=1; $n<=5; $n++) {
		if ($a{$color, $match, $n}[0]) {
		    next;
		}
		$a{$color, $match, $n} = [$col, $row];
		$col++;
		last;
	    }
	}
	return $POSTMATCH;
    } else {
	print stderr "invalid leftover $left_arg\n";
    }
    
}


printf "%c%c", $a{r, jiang, 1}[0], $a{r, jiang, 1}[1];
printf "%c%c", $a{r, shi, 1}[0], $a{r, shi, 1}[1];
printf "%c%c", $a{r, shi, 2}[0], $a{r, shi, 2}[1];
printf "%c%c", $a{r, xiang, 1}[0], $a{r, xiang, 1}[1];
printf "%c%c", $a{r, xiang, 2}[0], $a{r, xiang, 2}[1];
printf "%c%c", $a{r, ma, 1}[0], $a{r, ma, 1}[1];
printf "%c%c", $a{r, ma, 2}[0], $a{r, ma, 2}[1];
printf "%c%c", $a{r, ju, 1}[0], $a{r, ju, 1}[1];
printf "%c%c", $a{r, ju, 2}[0], $a{r, ju, 2}[1];
printf "%c%c", $a{r, pao, 1}[0], $a{r, pao, 1}[1];
printf "%c%c", $a{r, pao, 2}[0], $a{r, pao, 2}[1];
printf "%c%c", $a{r, bing, 1}[0], $a{r, bing, 1}[1];
printf "%c%c", $a{r, bing, 2}[0], $a{r, bing, 2}[1];
printf "%c%c", $a{r, bing, 3}[0], $a{r, bing, 3}[1];
printf "%c%c", $a{r, bing, 4}[0], $a{r, bing, 4}[1];
printf "%c%c", $a{r, bing, 5}[0], $a{r, bing, 5}[1];

printf "%c%c", $a{b, jiang, 1}[0], $a{b, jiang, 1}[1];
printf "%c%c", $a{b, shi, 1}[0], $a{b, shi, 1}[1];
printf "%c%c", $a{b, shi, 2}[0], $a{b, shi, 2}[1];
printf "%c%c", $a{b, xiang, 1}[0], $a{b, xiang, 1}[1];
printf "%c%c", $a{b, xiang, 2}[0], $a{b, xiang, 2}[1];
printf "%c%c", $a{b, ma, 1}[0], $a{b, ma, 1}[1];
printf "%c%c", $a{b, ma, 2}[0], $a{b, ma, 2}[1];
printf "%c%c", $a{b, ju, 1}[0], $a{b, ju, 1}[1];
printf "%c%c", $a{b, ju, 2}[0], $a{b, ju, 2}[1];
printf "%c%c", $a{b, pao, 1}[0], $a{b, pao, 1}[1];
printf "%c%c", $a{b, pao, 2}[0], $a{b, pao, 2}[1];
printf "%c%c", $a{b, bing, 1}[0], $a{b, bing, 1}[1];
printf "%c%c", $a{b, bing, 2}[0], $a{b, bing, 2}[1];
printf "%c%c", $a{b, bing, 3}[0], $a{b, bing, 3}[1];
printf "%c%c", $a{b, bing, 4}[0], $a{b, bing, 4}[1];
printf "%c%c", $a{b, bing, 5}[0], $a{b, bing, 5}[1];
