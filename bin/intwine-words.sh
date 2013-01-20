#!/bin/bash
perl -e '
$done = 0; 
while (not $done) {
    $done = 1;
    map {
        if (length $_) {
            $done = 0;
            print substr($_, 0, 1);
            $_ = substr($_, 1);
        }
    } @ARGV;
}' "$@" | putclip >/dev/null 2>&1
     
