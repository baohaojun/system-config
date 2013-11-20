project( SnoreNotify )
cmake_minimum_required( VERSION 2.8.8 )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_PREFIX_PATH}/share/apps/cmake/modules)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


#######################################################################
option(WITH_FREEDESKTOP_FRONTEND "Build the freedesktop frontend" OFF)
option(WITH_GROWL_BACKEND "Build the Growl backend" ON)
option(WITH_SNORE_DEAMON "Build the Snore deamon, which redirects notifications" OFF)
#######################################################################

set(SNORE_VERSION_MAJOR 0)
set(SNORE_VERSION_MINOR 4)
set(SNORE_VERSION_SUFFIX "beta1")



find_package(KDE4)

if(MINGW)
SET (CMAKE_RC_COMPILER_INIT windres)
ENABLE_LANGUAGE (RC)
SET (CMAKE_RC_COMPILE_OBJECT "<CMAKE_RC_COMPILER> -O coff -i <SOURCE> -o <OBJECT>")
endif(MINGW)

if(KDE4_FOUND)
	add_definitions(-DHAVE_KDE ${KDE4_DEFINITIONS} -D_UNICODE)
	include_directories(${KDE4_INCLUDES})
else(KDE4_FOUND)
	include(NoKDE)
endif(KDE4_FOUND)


find_package( CryptoPP )
find_package( Boost COMPONENTS system thread)

if(Boost_USE_STATIC_RUNTIME)
    add_definitions(-DBOOST_THREAD_USE_LIB )
endif(Boost_USE_STATIC_RUNTIME)

set(PLUGIN_INSTALL_PATH  LIBRARY DESTINATION bin/snoreplugins)
add_definitions(-DLIBSNORE_PLUGIN_PATH="${CMAKE_INSTALL_PREFIX}/bin/snoreplugins")

add_subdirectory(data)
add_subdirectory(share)
add_subdirectory(src)


