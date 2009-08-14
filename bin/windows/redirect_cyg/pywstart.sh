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
cygstart /cygdrive/c/python25/pythonw.exe "$pyfile" "$@"
