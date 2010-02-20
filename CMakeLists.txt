project( SnoreNotify )
cmake_minimum_required( VERSION 2.6 )
set(CMAKE_BUILD_TYPE "RelWithDebInfo")
add_definitions ( -Wall )

set(CMAKE_INCLUDE_CURRENT_DIR ON)

find_package ( Qt4 REQUIRED )
find_package(Automoc4 REQUIRED)

set ( EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/build )
set(LIBRARY_OUTPUT_PATH ${EXECUTABLE_OUTPUT_PATH})
if(MINGW)
    include_directories (${CMAKE_SOURCE_DIR}/include)
    add_definitions(-D_WIN32_WINNT=0x0500)
endif(MINGW)

option(WITH_WEBINTERFACE     "Buld with WebInterface"           OFF)
add_subdirectory(src)
