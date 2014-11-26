#!/bin/bash
set -e

function die() {
    echo Error: "$@"
    exit -1
}

if test ! -e .start-upgrade.hash; then
    echo -n > t1wrench.md5
    exit
fi

start=$(cat .start-upgrade.hash)
if test -e lua52.dll; then
    git co HEAD lua52.dll lua.exe luac.exe md5/des56.dll md5/core.dll
fi

gitdir=$(relative-path $(dirname $(lookup-file -e .git)) .)
git add $gitdir
git commit -m "${ReleaseVersion:-auto commit from update-md5s.sh}" --allow-empty
srcVersion=$(cd ~/src/github/T1Wrench; git log -1 --pretty=%H)
echo $srcVersion > .src-version.txt
git log -1 --pretty=%H > .bin-version.txt
git add $gitdir
git commit -m 'bump version'



git diff --name-status "$start" |
    while read type x; do
        x=$(relative-path $gitdir/$x .)
        if test ! -e $x; then
            if test "$type" != D; then
                die "Don't know how to update $type $x"
            fi
            continue;
        fi
        md5=$(md5sum $x|pn 1);
        url=$(git-info-clip $x);
        echo $x $url $md5;
    done | grep -v -P '^(./|)t1wrench.md5 ' | tee t1wrench.md5
my_md5=$(md5sum t1wrench.md5 | pn 1)
echo myself $(git-info-clip t1wrench.md5) $my_md5 |tee -a t1wrench.md5
git add $gitdir
git commit -m 'bump version' --amend
