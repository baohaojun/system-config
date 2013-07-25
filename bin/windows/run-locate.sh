#!/bin/bash
mkdir -p ~/.cache/.run-locate
file=~/.cache/.run-locate/"$1"

if ! test -f "$file" || find ~/.cache/locate -type f -newer "$file"|grep . -q; then
    locateEmacs.sh -i $1 |sed -e 's/^/"/; s/$/"/'| tee "$file"
else 
    cat "$file"
fi
