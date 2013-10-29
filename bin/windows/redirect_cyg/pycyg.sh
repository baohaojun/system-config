#!/bin/bash

debug=false
if [[ "$1" == -x ]]
then
    debug=true
    shift
fi
pyfile="$1"
if ! test -e "$pyfile"
then
    pyfile=`which "$pyfile"`
fi
shift
pyfile=`cygpath -alw "$pyfile"`

wpy2 "$pyfile" "$@"
ret=$?
if [[ $debug == true ]]
then
    echo -n Press any key to exit ...
    read
fi
exit $ret
