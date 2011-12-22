#!/bin/bash

set -x
cd ~/bin/windows/redirect_vc6
for x in *.sh; do
    ln -sf "$(readlink -m "$x")" ~/bin/windows/ext/$(basename "$x")
    ln -f ../shellHelper_vc6.exe ~/bin/windows/ext/`echo $x|sed -e 's/.sh$/.exe/'`; 
done

cd ~/bin/windows/redirect_cyg
for x in *.sh; do
    ln -sf "$(readlink -m "$x")" ~/bin/windows/ext/
    ln -f ../shellHelper_cyg.exe ~/bin/windows/ext/$(echo $x|sed -e 's/.sh$/.exe/');
done
