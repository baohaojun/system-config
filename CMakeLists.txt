project( SnoreNotify )
cmake_minimum_required( VERSION 2.8 )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_PREFIX_PATH}/share/apps/cmake/modules)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

find_package( Qt4 REQUIRED )
find_package( Automoc4 REQUIRED)

include( ${QT_USE_FILE} )
include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}  ${CMAKE_CURRENT_BINARY_DIR}
        ${QT_QTCORE_INCLUDE_DIR} ${QT_QTGUI_INCLUDE_DIR}
        ${QT_QTNETWORK_INCLUDE_DIR} ${QT_QTDBUS_INCLUDE_DIR} 
		${CMAKE_SOURCE_DIR}/3party/gntp-send/headers)


if (CMAKE_COMPILER_IS_GNUCXX)
   add_definitions( -Wall )
   set (KDE4_ENABLE_EXCEPTIONS -fexceptions)
   # Select flags.
   set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")
   set(CMAKE_CXX_FLAGS_RELEASE        "-O2 -DNDEBUG -DQT_NO_DEBUG")
   set(CMAKE_CXX_FLAGS_DEBUG          "-g -O2 -fno-reorder-blocks -fno-schedule-insns -fno-inline")
   set(CMAKE_CXX_FLAGS_DEBUGFULL      "-g3 -fno-inline")
   set(CMAKE_CXX_FLAGS_PROFILE        "-g3 -fno-inline -ftest-coverage -fprofile-arcs")
   set(CMAKE_C_FLAGS_RELWITHDEBINFO   "-O2 -g")
   set(CMAKE_C_FLAGS_RELEASE          "-O2 -DNDEBUG -DQT_NO_DEBUG")
   set(CMAKE_C_FLAGS_DEBUG            "-g -O2 -fno-reorder-blocks -fno-schedule-insns -fno-inline")
   set(CMAKE_C_FLAGS_DEBUGFULL        "-g3 -fno-inline")
   set(CMAKE_C_FLAGS_PROFILE          "-g3 -fno-inline -ftest-coverage -fprofile-arcs")

   if (CMAKE_SYSTEM_NAME MATCHES Linux)
     set ( CMAKE_C_FLAGS     "${CMAKE_C_FLAGS} -Wno-long-long -std=iso9899:1990 -Wundef -Wcast-align -Werror-implicit-function-declaration -Wchar-subscripts -Wall -W -Wpointer-arith -Wwrite-strings -Wformat-security -Wmissing-format-attribute -fno-common")
     set ( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wnon-virtual-dtor -Wno-long-long -ansi -Wundef -Wcast-align -Wchar-subscripts -Wall -W -Wpointer-arith -Wformat-security -fno-exceptions -fno-check-new -fno-common")
     add_definitions (-D_BSD_SOURCE)
   endif (CMAKE_SYSTEM_NAME MATCHES Linux)

endif(CMAKE_COMPILER_IS_GNUCXX)

if(MSVC)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  /Zc:wchar_t-")
endif(MSVC)

option(WITH_FREEDESKTOP_FRONTEND "Build the freedesktop frontend" OFF)
option(WITH_GROWL_BACKEND "Build the Growl backend" ON)

set(PLUGIN_INSTALL_PATH  LIBRARY DESTINATION bin/snoreplugins)

add_subdirectory(data)
add_subdirectory(3party)
add_subdirectory(src)

