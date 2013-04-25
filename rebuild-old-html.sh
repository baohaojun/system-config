#!/bin/bash
for x in *.org; do
    if test ! -e $x; then
        continue
    fi
    org=$(readlink $x);
    html=${org/%.org/.html};
    cp "$html" .;
    perl -npe 's!(href\s*=\s*.|src\s*=\s*.)\.\.[./]*!$1!ig' -i $(basename $html);
done
