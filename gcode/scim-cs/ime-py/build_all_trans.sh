#!/bin/bash

for version in 86 98; do
    for type in multi single; do
        export SDIM_WUBI_FLAVOR=wubi${version}_${type}
        if test -e ${SDIM_WUBI_FLAVOR}.py; then
            ./build_trans.py
        else
            echo No need to build trans for $SDIM_WUBI_FLAVOR
        fi
    done
done
