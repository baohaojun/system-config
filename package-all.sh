#!/bin/bash

for x in ~/src/github/Wrench-linux ~/src/github/Wrench-macos/Wrench.app/Contents/MacOS/ ~/src/github/Wrench-windows; do
    (
        cd $x
        if test -e last-pic-notes.png; then
            rm -f last-pic-notes.png
        fi
        cd $(dirname $(lookup-file -e .git))
        dir=$(basename $PWD)
        cd ..

        file=~/tmp/$dir.zip
        if test "$dir" = Wrench-linux; then
            file=~/tmp/$dir.tgz
            tar czfv $file $dir --exclude-vcs
        else
            rm -f $file
            zip -r $file $dir -x '*/.git/*'
        fi
    )
done
