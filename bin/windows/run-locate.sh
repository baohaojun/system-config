#!/bin/bash
mkdir -p ~/.cache/system-config/.run-locate
file=~/.cache/system-config/.run-locate/"$1"

if ! test -f "$file" || find ~/.cache/system-config/locate -type f -newer "$file"|grep . -q; then
    locateEmacs.sh -i $1 |sed -e 's/^/"/; s/$/"/'| tee "$file"
else 
    cat "$file"
fi
