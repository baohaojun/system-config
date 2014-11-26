#!/bin/bash
cd $(dirname $(readlink -f $0))
export DOING_T1WRENCH_RELEASE=true

set -e

function die() {
    echo Error: "$@"
    exit -1
}

. .gitx

if git st -s | grep . -q; then
    git st -s
    die "Can't do release build when git not clean: see output above"
fi

git clean -xfd
git submodule foreach 'git clean -xfd'

(
    rm ~/tmp/build-t1-windows -rf
    ./build-wine.sh
    touch ~/tmp/build-t1-windows/build-ok
)&

(
    rm ~/tmp/build-t1 -rf
    ./build-linux.sh
    touch ~/tmp/build-t1/build-ok
)&

(
    ssh bhj-mac rm $(up) -rf
    rm ~/tmp/build-t1-mac -rf
    ./build-mac.sh
    touch ~/tmp/build-t1-mac/build-ok
)&

wait
echo release done.
