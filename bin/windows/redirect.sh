#!/bin/bash

cd ~/bin/windows/redirect
for x in *.sh; do
    ln -f ../shellHelper_vc6.exe `echo $x|sed -e 's/.sh$/.exe/'`; 
done

