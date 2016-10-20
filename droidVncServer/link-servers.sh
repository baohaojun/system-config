#!/bin/bash

cd ~/src/github/Wrench/droidVncServer
for x in androidvncserver-*; do
    target=$x
    if [[ $x =~ -arm64 ]]; then
        target=${x/arm64/aarch64}
    elif [[ $x =~ -arm ]]; then
        target=${x/-arm/-armv7l}
    fi
    relative-link $x ../release/$target -f
    md5sum $x | tee ../release/$target.md5
done

relative-link ../release/androidvncserver-* ~/tmp/build-wrench
