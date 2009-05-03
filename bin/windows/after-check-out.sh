#!/bin/bash
cd ~/bin/windows/
find . -type l -exec relink.sh '{}' \;
cd ~/bin/windows/redirect
for x in *.sh; do
    ln -f ../redirect.exe `echo $x|sed -e 's/.sh$/.exe/'`; 
done

