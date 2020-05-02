#!/usr/bin/env perl 
$dood=0; while( <> ){m/(.*,)(.*)$/; print $1 . ($2-$dood) . "\n"; $dood=$2;}
