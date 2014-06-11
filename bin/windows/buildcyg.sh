#!/bin/bash

gcc-switch.sh
set -e
set -o pipefail

cd "${1:-no such dir}"
rm build cygbin -rf
mkdir -p build cygbin
cd build
../configure CFLAGS='-g -O0' CPPFLAGS='-g -O0' --enable-debugging --prefix="$(readlink -f ../cygbin)" -v |tee -a conf.out

read -e -p "configure finished Press Enter to continue..."
make |tee -a make.out
make install |tee -a install.out
cat <<EOF > install.cmd
python $(cygpath -alw $(which terminateModule.py)) cygwin1.dll
sleep 3
copy $(cygpath -alw ../cygbin/bin/cygwin1.dll)  $(cygpath -alw /bin)
copy $(cygpath -alw ./i686-pc-cygwin/winsup/cygwin/cygwin1.dbg) $(cygpath -alw /bin)
bash
cmd
EOF
u2d install.cmd
chmod +x install.cmd
