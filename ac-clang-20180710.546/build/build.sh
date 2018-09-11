# -*- mode: shell-script ; coding: utf-8-unix -*-
#! /bin/sh
# export PATH="/cygdrive/c/cygwin-x86_64/tmp/cmake-3.1.0-win32-x86/bin/:$PATH"

rm -rf CMakeCache.txt
rm -rf cmake_install.cmake
rm -rf CMakeFiles
rm -rf Makefile

declare BUILD_OPTIONS="${0}.opt"

cp -up ${BUILD_OPTIONS}.template ${BUILD_OPTIONS}


# switch compiler
# export CC=clang
# export CXX=clang++
# declare TARGET_LLVM_VERSION="${1-600}"
declare TARGET_LLVM_VERSION
declare TARGET_CONFIG
declare LLVM_BUILD_SHELLS_PATH
declare INSTALL_PREFIX=""
declare -a ADDITIONAL_ARGS=()

# overwrite vars load
if [ -e "./${BUILD_OPTIONS}" ]; then
    . "./${BUILD_OPTIONS}"
fi


declare -r LLVM_LIBRARY_PATH="${LLVM_BUILD_SHELLS_PATH}/sh/llvm-${TARGET_LLVM_VERSION}/build-${TARGET_CONFIG}"

if [ -n "${INSTALL_PREFIX}" ]; then
    ADDITIONAL_ARGS+=(-DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}")
fi


declare -p TARGET_LLVM_VERSION
declare -p TARGET_CONFIG
declare -p LLVM_BUILD_SHELLS_PATH
declare -p LLVM_LIBRARY_PATH
declare -p INSTALL_PREFIX
declare -p ADDITIONAL_ARGS


cmake --version
cmake -G "Unix Makefiles" ../clang-server -DLIBRARY_PATHS="${LLVM_LIBRARY_PATH}" -DCMAKE_BUILD_TYPE=${TARGET_CONFIG} -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ${ADDITIONAL_ARGS[@]}

# echo "please press Enter key"
# read discard_tmp


# cmake --build . [--config <config>] [--target <visual studio project name>] [-- -i]
cmake --build . --config ${TARGET_CONFIG}
# sudo make install


