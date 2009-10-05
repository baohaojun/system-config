#!/bin/bash
mkdir -p ~/tmp/.run-locate
file=~/tmp/.run-locate/"$1"

if ! test -f "$file" || find ~/tmp/locate -type f -newer "$file"|grep . -q; then
    locateEmacs.sh -i $1 |sed -e 's/^/"/; s/$/"/'| tee "$file"
else 
    cat "$file"
fi
