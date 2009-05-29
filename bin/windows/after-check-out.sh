#!/bin/bash
mount c: /c
mount d: /d
mount e: /e
mount q: /q

cd ~/bin/windows/
mkdir ~/bin/windows/lnks
find . -type l -exec relink.sh '{}' \;
cd ~/bin/windows/redirect

~/bin/windows/redirect.sh


cd ~/bin/windows
./update-password.sh
echo 'after check out update success!'
ln -sf ~/'Application Data/Microsoft/Internet Explorer/Quick Launch' ~/SendTo/
