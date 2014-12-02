#!/bin/bash
set -e
for x in ~/src/github/T1Wrench-linux ~/src/github/T1Wrench-macos/ ~/src/github/T1Wrench-windows; do
    (
        cd $x
        if test $(basename $0) = reset-all.sh; then
            git reset --hard origin/$(basename $PWD)
            exit
        fi
        git diff HEAD^^ --name-status
        git push >/dev/null 2>&1 &&
            git push origin HEAD:$(basename $PWD|perl -npe 's/.*-//')-release >/dev/null 2>&1 &&
            git st&
    )
done | cat
