#!/bin/bash
for x in $(find blog -name '*.org'); do
    if test ! -e $x; then
        continue
    fi
    org=$(readlink -f $x);
    html=${org/%.org/.html};
    if test -e $(basename $html); then
        cp "$html" .;
        perl -npe 's!(href\s*=\s*.|src\s*=\s*.)\.\.[./]*!$1!ig' -i $(basename $html);
    fi
done
