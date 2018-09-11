# -*- mode: shell-script ; coding: utf-8-unix -*-
#! /bin/sh



declare -a HOST_VS_VERSIONS=(
    2017
    2017
    2017
    2017
    2015
    2015
    2015
    2015
    2013
    2013
    2013
    2013
    2017
    2017
    2017
    2017
    2015
    2015
    2015
    2015
    2013
    2013
    2013
    2013
)

declare -a TARGET_LLVM_VERSIONS=(
    600
    600
    600
    600
    600
    600
    600
    600
    600
    600
    600
    600
    500
    500
    500
    500
    500
    500
    500
    500
    500
    500
    500
    500
)

declare -a TARGET_ARCHS=(
    64
    64
    32
    32
    64
    64
    32
    32
    64
    64
    32
    32
    64
    64
    32
    32
    64
    64
    32
    32
    64
    64
    32
    32
)

declare -a TARGET_CONFIGS=(
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
    Release
    Debug
)

declare -a SUFFIXS=(
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
    ""
    "-debug"
)


declare -i BUILD_COUNT="${#TARGET_LLVM_VERSIONS[@]}"

if $( [ ${BUILD_COUNT} -ne ${#HOST_VS_VERSIONS[@]} ] || [ ${BUILD_COUNT} -ne ${#TARGET_ARCHS[@]} ] || [ ${BUILD_COUNT} -ne ${#TARGET_CONFIGS[@]} ] || [ ${BUILD_COUNT} -ne ${#SUFFIXS[@]} ] ); then
    echo "don't match table count"
    exit 1
fi


declare HOST_VS_VERSION
declare TARGET_LLVM_VERSION
declare TARGET_ARCH
declare TARGET_CONFIG
declare SUFFIX

for (( i = 0; i < ${BUILD_COUNT}; ++i )); do
    HOST_VS_VERSION=${HOST_VS_VERSIONS[ ${i} ]}
    TARGET_LLVM_VERSION=${TARGET_LLVM_VERSIONS[ ${i} ]}
    TARGET_ARCH=${TARGET_ARCHS[ ${i} ]}
    TARGET_CONFIG=${TARGET_CONFIGS[ ${i} ]}
    SUFFIX=${SUFFIXS[ ${i} ]}

    cmd /c "build.bat ${HOST_VS_VERSION} ${TARGET_LLVM_VERSION} ${TARGET_ARCH} ${TARGET_CONFIG}"
    pushd /usr/local/bin
    tar -cvf "llvm-${HOST_VS_VERSION}-${TARGET_LLVM_VERSION}-${TARGET_ARCH}-${TARGET_CONFIG}.tar" "clang-server${SUFFIX}.exe" libclang.dll
    # zip -r "llvm-${HOST_VS_VERSION}-${TARGET_LLVM_VERSION}-${TARGET_ARCH}-${TARGET_CONFIG}.zip" "clang-server${SUFFIX}.exe" libclang.dll
    popd
done



