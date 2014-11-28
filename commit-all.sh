#!/bin/bash
for x in ~/src/github/T1Wrench-linux ~/src/github/T1Wrench-macos/T1Wrench.app/Contents/MacOS/ ~/src/github/T1Wrench-windows; do
    (
        cd $x
        git diff HEAD^^ --name-status
        git push >/dev/null 2>&1 && git st&
    )
done | cat
