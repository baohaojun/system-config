# -*- mode: shell-script ; coding: utf-8-unix -*-
#! /bin/sh
# export PATH="/cygdrive/c/cygwin-x86_64/tmp/cmake-3.1.0-win32-x86/bin/:$PATH"

rm -rf CMakeCache.txt
rm -rf cmake_install.cmake
rm -rf CMakeFiles
rm -rf Makefile

# switch compiler
# export CC=clang
# export CXX=clang++

# cmake --version
# cmake -G "Unix Makefiles" ../clang-server -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
# cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="c:/cygwin-x86_64/tmp/llvm-build-shells/sh/clang-361/build" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="/home/yaruopooner/work/llvm-build-shells/sh/clang-361/build" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
# cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="/home/yaruopooner/work/llvm-build-shells/sh/clang-361/build" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=RELWITHDEBINFO
# cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="/home/yaruopooner/work/llvm-build-shells/sh/clang-361/build" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=DEBUG
# cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="/home/yaruopooner/work/llvm-build-shells/sh/clang-361/build" -DCMAKE_INSTALL_PREFIX="~/work/test" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

# echo "please press Enter key"
# read discard_tmp

cmake --build .
# sudo make install


