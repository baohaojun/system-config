#!/bin/bash

cd ~/src/github/Wrench/droidVncServer
for x in androidvncserver-* screencap-*; do
    target=$x
    if [[ $x =~ -arm64 ]]; then
        target=${x/arm64/aarch64}
    elif [[ $x =~ -arm ]]; then
        target=${x/-arm/-armv7l}
    fi
    relative-link $x ../release/$target -f
    md5sum $x | tee ../release/$target.md5
done

for w_dir in ~/tmp/build-wrench ~/tmp/build-wrench.*; do
    (
        if test ! -d "$w_dir"; then
            exit
        fi
        chmod u+w ${w_dir}
        relative-link ../release/androidvncserver-* ../release/screencap-* ${w_dir}

    )  >/dev/null 2>&1 || true
done
