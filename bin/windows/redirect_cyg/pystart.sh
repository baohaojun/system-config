#!/bin/bash

debug=false
if [[ "$1" == -x ]]
then
    debug=true
    shift
fi
pyfile="$1"
 if ! [[ ${pyfile:0:1} == / ]];
 then
     pyfile=`which "$pyfile"`
 fi
shift
pyfile=`cygpath -alw "$pyfile"`
set -x
cygstart /cygdrive/c/Python2/python \""$pyfile"\" "$@"
