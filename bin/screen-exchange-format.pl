#!/bin/perl
$indent=0;
while(<>) {
    s/^\s*//;
    if (m/entering/) {
        for ($i=0;
             $i<$indent;
             $i++) {
            print " ";
        };
        print;
        $indent+=1;
    } elsif (m/leaving/) {
        $indent-=1;
        if ($indent<0) {
            $indent = 0;
        }
        for ($i=0;
             $i<$indent;
             $i++) {
            print " ";
        };
        print;
        if ($indent == 0) {
            print "\n";
        }
    } else {
        for ($i=0;
             $i<$indent;
             $i++) {
            print " ";
        };
        print;
    }
}
