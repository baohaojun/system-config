#!/bin/bash
out=$(
perl -e '
$done = 0;
while (not $done) {
    $done = 1;
    map {
        if (length $_) {
            $done = 0;
            print substr($_, -1);
            $_ = substr($_, 0, -1);
        }
    } @ARGV;
}' "$@" )

if is-null-output; then
    echo "$out" | putclip
    exit 0
fi

echo "$out"
if is-tty-io; then
    echo "$out" | putclip >/dev/null 2>&1
fi
