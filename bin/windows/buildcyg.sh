#!/bin/bash

gcc-switch.sh
set -e
set -o pipefail

cd "${1:-no such dir}"
mkdir -p build cygbin
cd build
../configure --prefix="$(readlink -f ../cygbin)" -v |tee -a conf.out
make |tee -a make.out
make install |tee -a install.out
