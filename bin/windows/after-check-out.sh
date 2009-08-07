#!/bin/bash
set -e
set -x

function mkdir () 
{
    command mkdir -p "$@"
}

function qq_cmdout () #put " at begin and end of cmd output, which should be 1 line
{
    echo -n \"
    "$@"|tr -d '\n' #if "$@" produce more than 1 line, we are screwed!
    echo -n \"
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
cp ~/doc/MONACO.TTF ~/.fonts
cp /cygdrive/c/windows/fonts/simsun.ttc ~/.fonts
cp /cygdrive/c/windows/fonts/cour.ttf ~/.fonts
fc-cache
for x in {c..z}; do test -e /$x || ln -s /cygdrive/$x /; done

regtool.exe -s set '\HKEY_CLASSES_ROOT\.sh' 'sh_auto_file'
regtool.exe -s set '\HKEY_CLASSES_ROOT\sh_auto_file\shell\open\command\' "$(qq_cmdout cygpath -alw "$BASH"; echo -n ' --rcfile ~/.bashrc-windows -i "%1" %*')"

echo "After check out success!"
