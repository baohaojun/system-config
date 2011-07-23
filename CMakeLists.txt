project( SnoreNotify )
cmake_minimum_required( VERSION 2.8 )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_PREFIX_PATH}/share/apps/cmake/modules)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

find_package(KDE4)

if(KDE4_FOUND)
	add_definitions(-DHAVE_KDE ${KDE4_DEFINITIONS} -D_UNICODE)
	include_directories(${KDE4_INCLUDES})
else(KDE4_FOUND)
	include(NoKDE)
endif(KDE4_FOUND)


find_package( CryptoPP )
find_package( Boost COMPONENTS system )


option(WITH_FREEDESKTOP_FRONTEND "Build the freedesktop frontend" OFF)
option(WITH_GROWL_BACKEND "Build the Growl backend" ON)

set(PLUGIN_INSTALL_PATH  LIBRARY DESTINATION bin/snoreplugins)

add_subdirectory(data)
add_subdirectory(share)
add_subdirectory(src)


