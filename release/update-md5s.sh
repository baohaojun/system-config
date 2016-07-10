#!/bin/bash
set -e

function die() {
    echo Error: "$@"
    exit -1
}

(git co HEAD .start-upgrade.hash || true)>/dev/null 2>&1
if test ! -e .start-upgrade.hash; then
    echo -n > wrench.md5
    exit
fi

start=$(cat .start-upgrade.hash)
if test -e lua52.dll; then
    git co HEAD lua52.dll lua.exe luac.exe md5/des56.dll md5/core.dll
fi

gitdir=$(relative-path $(dirname $(lookup-file -e .git)) .)
git add $gitdir
git commit -m "${ReleaseVersion:-auto commit from update-md5s.sh}" --allow-empty
srcVersion=$(cd ~/src/github/Wrench; git log -1 --pretty=%H)
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
    done | grep -v -P '^(./|)wrench.md5 ' | tee wrench.md5
my_md5=$(md5sum wrench.md5 | pn 1)
echo myself $(git-info-clip wrench.md5) $my_md5 |tee -a wrench.md5
git add $gitdir
git commit -m 'bump version' --amend
