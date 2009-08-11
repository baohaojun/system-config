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

FSTAB=/etc/fstab
if grep '^q:' $FSTAB -iq; 
then
    echo 'q: is already mounted case-sensitive'
else
    echo 'q: /q some_fs binary 0 0' >> $FSTAB
fi
    

echo "After check out success!"
