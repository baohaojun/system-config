#!/bin/bash --login
rm ~/.ido.last

export CYGDIR=`cygpath -alwm /`
#export http_proxy=http://bhj3:8888
. ~/.bashrc
export CDPATH="`cygpath -alpw \"$CDPATH\"`"
unset INFOPATH
export VC_BUILD_CONFIG=release
texlivemake /d/tools/emacswin/bin/runemacs "$@" &
