#!/bin/bash

for x in ~/src/github/T1Wrench-linux ~/src/github/T1Wrench-macos/T1Wrench.app/Contents/MacOS/ ~/src/github/T1Wrench-windows; do
    (
        cd $x
        if test -e last-pic-notes.png; then
            rm -f last-pic-notes.png
        fi
        cd $(dirname $(lookup-file -e .git))
        dir=$(basename $PWD)
        cd ..

        file=~/tmp/$dir.zip
        if test "$dir" = T1Wrench-linux; then
            file=~/tmp/$dir.tgz
            tar czfv $file $dir --exclude-vcs
        else
            rm -f $file
            zip -r $file $dir -x '*/.git/*'
        fi
    )
done
