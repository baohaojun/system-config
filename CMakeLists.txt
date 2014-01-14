project( SnoreNotify )
cmake_minimum_required( VERSION 2.8.8 )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_PREFIX_PATH}/share/apps/cmake/modules)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


#######################################################################
option(WITH_WITH_KDE "Try to build with KDE support if availibe" ON)
option(WITH_FREEDESKTOP_FRONTEND "Build the freedesktop frontend" OFF)
option(WITH_GROWL_BACKEND "Build the Growl backend" ON)
option(WITH_SNORE_DEAMON "Build the Snore deamon, which redirects notifications" OFF)
#######################################################################

set(SNORE_VERSION_MAJOR 0)
set(SNORE_VERSION_MINOR 5)
set(SNORE_VERSION_SUFFIX "pre")

if(WITH_WITH_KDE)
    find_package(KDE4)
endif()



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

set(SNORE_PLUGIN_INSTALL_PATH LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/libsnore)
add_definitions(-DLIBSNORE_PLUGIN_PATH="${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/libsnore" -DCMAKE_INSTALL_LIBDIR="${CMAKE_INSTALL_LIBDIR}")

add_subdirectory(data)
add_subdirectory(share)
add_subdirectory(src)


