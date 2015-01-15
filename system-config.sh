#!/bin/bash

set -e
function die() {
    echo Error: "$@"
    exit -1
}

if echo $SHELLOPTS | grep -q xtrace; then
    export SHELLOPTS
fi

## start generated code
TEMP=$(getopt -o s --long "skip-git-clone"  -n $(basename $0) -- "$@")
skip_git_clone=
eval set -- "$TEMP"
while true; do
    case "$1" in
        -s|--skip-git-clone)
            skip_git_clone=true
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done

## end generated code

if test -e ~/system-config -a "$skip_git_clone" != true; then
    die "You already have system-config in your HOME dir, please run $0 --skip-git-clone".
fi

if test "$skip_git_clone" != true; then
    git clone --recursive https://github.com/baohaojun/system-config ~/system-config
fi

read -p 'Do full configuration? yes/No: ' ans
if test "${ans^^}" = YES; then
    ~/system-config/bin/Linux/after-check-out.sh
else
    ~/system-config/bin/after-co-ln-s.sh
fi
