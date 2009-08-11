#!/bin/bash
set -e
set -x

function mkdir () 
{
    command mkdir -p "$@"
}

cd ~/bin/windows/
mkdir  ~/bin/windows/lnks
DOWN=yes ./download-external.sh
find . -type l -exec relink.sh '{}' \;

~/bin/windows/redirect.sh

cd ~/bin/windows
./update-password.sh
echo 'after check out update success!'
ln -sf ~/'Application Data/Microsoft/Internet Explorer/Quick Launch' ~/SendTo/
mkdir -p ~/.fonts
cp ~/doc/MONACO.TTF /cygdrive/c/windows/fonts/simsun.ttc /cygdrive/c/windows/fonts/cour.ttf ~/.fonts || true
fc-cache
for x in {c..z}; do test -e /$x || ln -s /cygdrive/$x /; done

    

echo "After check out success!"
