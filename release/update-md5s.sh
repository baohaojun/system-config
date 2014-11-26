#!/bin/bash
if test ! -e .start-upgrade.hash; then
    exit
fi
find $(relative-path $(dirname $(lookup-file -e .git)) .) -type f -o -type l | sort -u | grep -P -v '/\.git/'|xargs bash -c '
    for x in "$@"; do
        if test ! -e $x; then
            continue;
        fi
        md5=$(md5sum $x|pn 1);
        url=$(git-info-clip $x);
        echo $x $url $md5;
    done' true |grep -v -P '^\./t1wrench.md5 ' | tee t1wrench.md5
my_md5=$(md5sum t1wrench.md5 | pn 1)
echo myself $(git-info-clip t1wrench.md5) $my_md5 >> t1wrench.md5
