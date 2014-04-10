#!/bin/bash

if test $# = 0; then
    set -- --everything
fi
python ~/src/github/private-config/ramparser_withexe/ramparse.py --vmlinux ./vmlinux --nm-path ~/src/android/prebuilts/gcc/linux-x86/arm/arm-eabi-4.6/bin/arm-eabi-nm  --gdb-path ~/src/android/prebuilts/gcc/linux-x86/arm/arm-eabi-4.6/bin/arm-eabi-gdb --auto-dump . --outdir . "$@"
