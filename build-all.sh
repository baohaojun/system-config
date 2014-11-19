#!/bin/bash
cd $(dirname $(readlink -f $0))
./build-linux.sh&
./build-mac.sh&
./build-wine.sh&
