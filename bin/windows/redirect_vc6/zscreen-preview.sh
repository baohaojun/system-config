#!/bin/bash
mkdir -p ~/images/
mv "$1" ~/images/
cygpath -au ~/images/"$(basename "$1")"|tr -d '\n' > ~/.zscreen.history
cygstart ~/images/"$(basename "$1")"

quote() #from bash completion
{
    echo \'${1//\'/\'\\\'\'}\' #'# Help vim syntax highlighting
}

cygstart /bin/bash -c "$(quote "sleep 2 && echo -n ~/images/""$(basename "$1")""|putclip")"
findexec -p gimp-2.6
