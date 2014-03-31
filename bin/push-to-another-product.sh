#!/bin/bash
set -e
function die() {
    echo Error: "$@"
    exit -1
}

if test $# != 1 -o ! -d "$1"; then
    die "Error: Usage $(basename $0) other_product_dir"
fi

cd "$1"
git fetch $OLDPWD
git cherry-pick FETCH_HEAD
