#!/bin/bash
(
    for x in $(find ~/bin/windows/ -maxdepth 1 -type l); do
        if readlink -m "$x" | grep '/Program Files/' -i; then
            non_pf_dir=$(readlink -m "$x" | perl -npe 's!.*?program files[^/]*/!!i')
            for d in c d e; do
                found=false
                for p in "Program Files" "Program Files (x86)"; do
                    if test -e "/$d/$p/$non_pf_dir"; then
                        ln -s "/$d/$p/$non_pf_dir" ~/bin/windows/ext/$(basename $x)
                        found=true
                        break
                    fi
                done
                if test $found = true; then
                    break
                fi
            done
        fi
    done
)
