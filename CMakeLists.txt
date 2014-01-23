project( SnoreNotify )
cmake_minimum_required( VERSION 2.8.8 )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_PREFIX_PATH}/share/apps/cmake/modules)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

include(GNUInstallDirs)


#######################################################################
option(WITH_KDE "Try to build with KDE support if availibe" ON)
option(WITH_FRONTENDS  "Build frontends currently only useful if WITH_SNORE_DEAMON=ON" OFF)
option(WITH_FREEDESKTOP_FRONTEND "Build the freedesktop frontend" OFF)
option(WITH_GROWL_BACKEND "Build the Growl backend" ON)
option(WITH_SNORE_DEAMON "Build the Snore deamon, which redirects notifications" OFF)
#######################################################################

set(SNORE_VERSION_MAJOR 0)
set(SNORE_VERSION_MINOR 5)
set(SNORE_VERSION_SUFFIX "pre")

if(WITH_KDE)
    find_package(KDE4)
endif()

if(WITH_SNORE_DEAMON)
    set(WITH_FRONTENDS ON)
endif()

if(KDE4_FOUND)
    add_definitions(-DHAVE_KDE ${KDE4_DEFINITIONS} -D_UNICODE)
    include_directories(${KDE4_INCLUDES})
else(KDE4_FOUND)
    include(NoKDE)
endif(KDE4_FOUND)


find_package( CryptoPP )
find_package( Boost COMPONENTS system thread)

find_package(Doxygen)

if(DOXYGEN_FOUND)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in" "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" @only)

    add_custom_target(doc
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMENT "Generating API documentation with Doxygen" VERBATIM
    )
else(DOXYGEN_FOUND)
    message(WARNING "No doxygen package found")
endif(DOXYGEN_FOUND)


set(LIBSNORE_PLUGIN_PATH  ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/libsnore)
set(SNORE_PLUGIN_INSTALL_PATH LIBRARY DESTINATION ${LIBSNORE_PLUGIN_PATH})
add_definitions(-DLIBSNORE_PLUGIN_PATH="${LIBSNORE_PLUGIN_PATH}")
message(STATUS "Installing plugins to ${LIBSNORE_PLUGIN_PATH}")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


add_subdirectory(data)
add_subdirectory(share)
add_subdirectory(src)


