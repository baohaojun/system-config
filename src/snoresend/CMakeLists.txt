project( snore-send )
cmake_minimum_required( VERSION 2.8.12 )

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

include(GenerateExportHeader)



find_package(ECM 0.0.9 NO_MODULE)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${ECM_MODULE_PATH})
include(KDECompilerSettings)

find_package(LibsnoreQt5 0.5.91 REQUIRED)



add_subdirectory(src)


