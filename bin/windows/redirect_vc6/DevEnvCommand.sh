#!/bin/bash

if test "$1" = "File.OpenFile"; then
    set -- "$1" "$(cygpath -alw "$2")"
fi

devcmd-emacs.exe "$@"
