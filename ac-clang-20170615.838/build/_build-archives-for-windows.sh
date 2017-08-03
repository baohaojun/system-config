# -*- mode: shell-script ; coding: utf-8-unix -*-
#! /bin/sh



declare -a HOST_VS_VERSIONS=(
    2017
    2017
    2015
    2015
    # 2013
    # 2013
)

declare -a TARGET_CLANG_VERSIONS=(
    400
    400
    400
    400
    # 400
    # 400
)

declare -a TARGET_ARCH_TYPES=(
    64
    32
    64
    32
    # 64
    # 32
)

declare -a TARGET_ARCH_NAMES=(
    x86_64
    x86_32
    x86_64
    x86_32
    # x86_64
    # x86_32
)



declare -i BUILD_COUNT="${#TARGET_CLANG_VERSIONS[@]}"

if $( [ ${BUILD_COUNT} -ne ${#HOST_VS_VERSIONS[@]} ] || [ ${BUILD_COUNT} -ne ${#TARGET_ARCH_TYPES[@]} ] || [ ${BUILD_COUNT} -ne ${#TARGET_ARCH_NAMES[@]} ] ); then
    echo "don't match table count"
    exit 1
fi


declare SERVER_VERSION="1.7.1"
declare HOST_VS_VERSION
declare TARGET_CLANG_VERSION
declare TARGET_ARCH_TYPE
declare TARGET_ARCH_NAME
declare ARCHIVE_NAME
declare INSTALL_PREFIX
declare WORK_DIR="/tmp/clang-server-archives"

if [ ! -d ${WORK_DIR} ]; then
    mkdir -p ${WORK_DIR}
fi

for (( i = 0; i < ${BUILD_COUNT}; ++i )); do
    TARGET_CLANG_VERSION=${TARGET_CLANG_VERSIONS[ ${i} ]}
    HOST_VS_VERSION=${HOST_VS_VERSIONS[ ${i} ]}
    TARGET_ARCH_TYPE=${TARGET_ARCH_TYPES[ ${i} ]}
    TARGET_ARCH_NAME=${TARGET_ARCH_NAMES[ ${i} ]}
    ARCHIVE_NAME="clang-server-${SERVER_VERSION}-${TARGET_ARCH_NAME}-vs${HOST_VS_VERSION}"
    INSTALL_PREFIX=$( cygpath -am "${WORK_DIR}/${ARCHIVE_NAME}" )

    cmd /c "build.bat ${HOST_VS_VERSION} ${TARGET_CLANG_VERSION} ${TARGET_ARCH_TYPE} Release ${INSTALL_PREFIX}"

    pushd ${WORK_DIR}
    if [ -d ${ARCHIVE_NAME} ]; then
        tar -cvzf "${ARCHIVE_NAME}.zip" "${ARCHIVE_NAME}"
        ./${ARCHIVE_NAME}/clang-server.exe --version
    fi
    popd
done



