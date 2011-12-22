#!/bin/bash

cd ~/bin/windows/redirect_vc6
for x in *.sh; do
    ln -f ../shellHelper_vc6.exe `echo $x|sed -e 's/.sh$/.exe/'`; 
done

cd ~/bin/windows/redirect_cyg
for x in *.sh; do 
    ln -f ../shellHelper_cyg.exe `echo $x|sed -e 's/.sh$/.exe/'`;
done
