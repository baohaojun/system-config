#!/bin/bash
set -e
cd ~/bin/windows/
mkdir ~/bin/windows/lnks
./download-external.sh
find . -type l -exec relink.sh '{}' \;

~/bin/windows/redirect.sh

cd ~/bin/windows
./update-password.sh
echo 'after check out update success!'
ln -sf ~/'Application Data/Microsoft/Internet Explorer/Quick Launch' ~/SendTo/
echo "After check out success!"
