#!/bin/bash

cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<manifest>

  <remote  name="shgitm"
           fetch="bibler:shgit/"
           review="http://bible/gerrit/" />

  <remote  name="bpe-git"
           fetch="bibler:android/"
           review="http://bible/gerrit/" />
EOF


export ME=$(readlink -f $0)
my-rfa '
#{%sh%}

#   <project path="device/marvell/pxa978dkb" name="device/marvell/pxa978/pxa978dkb" />
path=$(repo-path)
project=$(repo-project)
revision=$(git rev-parse HEAD)
remote=$(repo-remote)
if test $remote = shgit; then
    remote=shgitm
fi
(
    flock 22
    echo "<project path=\"$path\" name=\"$project\" revision=\"$revision\" remote=\"$remote\" />"
) 22<$ME

#{%/sh%}
'

echo '</manifest>'
