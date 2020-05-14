#!/usr/bin/env bash

set -e

me=$(readlink -f $0)
if test ! -e "$me"; then
    me=$(readlink -f "$(which $0)")
    if test ! -e "$me"; then
        die "Can't find out about me"
        exit 1
    fi
fi
b0=$(basename $0)

cd $(dirname $me)/release/ext

cat <<EOF > .modules.lua
local M = {}
M.description_to_filename = {}
M.description_to_loaded_func = {}
M.descriptions = {}

M.descriptions[1] = "What do you want?"
$(
    n=2
    for x in *.lua; do
        description=$(cat $x|grep ^--|perl -npe 's/^--+\s*//')
        printf "M.description_to_filename[ [==[%s]==] ] = '%s'\n" "$description" "$x"
        printf "M.descriptions[%d] = [==[%s]==]\n" $((n++)) "$description"
    done
)
return M
EOF
