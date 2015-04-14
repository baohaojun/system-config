#!/bin/bash

set -x
cd ~/system-config/bin/windows/redirect_vc6
for x in *.sh; do
    ln -sf "$(readlink -m "$x")" ~/system-config/bin/windows/ext/$(basename "$x")
    ln -f ../shellHelper_vc6.exe ~/system-config/bin/windows/ext/`echo $x|sed -e 's/.sh$/.exe/'`; 
done

cd ~/system-config/bin/windows/redirect_cyg
for x in *.sh; do
    ln -sf "$(readlink -m "$x")" ~/system-config/bin/windows/ext/
    ln -f ../shellHelper_cyg.exe ~/system-config/bin/windows/ext/$(echo $x|sed -e 's/.sh$/.exe/');
done
