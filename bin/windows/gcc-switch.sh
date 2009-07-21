#!/bin/bash

# We only switch the alternatives that exist for both compilers;
# gdc, gpc, g77 and gfortran never get switched.
VER=${1:-4}

for x in gcc g++ gcj gnat ; do
  /usr/sbin/alternatives --set $x /usr/bin/$x-$VER.exe
done

