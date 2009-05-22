#!/bin/bash
cd ~/bin/windows/
find . -type l -exec relink.sh '{}' \;
cd ~/bin/windows/redirect
for x in *.sh; do
    ln -f ../redirect.exe `echo $x|sed -e 's/.sh$/.exe/'`; 
done

ln -f ../gnuserv/*.exe . 

echo cat "'/proc/registry/HKEY_LOCAL_MACHINE/SYSTEM/CurrentControlSet/Control/Session Manager/Environment/PATH'"
for x in '\bin' '\bin\windows' '\bin\windows\redirect'; do 
    echo -n "$HOMEDRIVE""$HOMEPATH""$x"\;
done

cd ~/bin/windows
./update-pass.sh
