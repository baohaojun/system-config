project( SnoreNotify )
cmake_minimum_required( VERSION 2.8.9 )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules ${CMAKE_PREFIX_PATH}/share/apps/cmake/modules)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

cmake_policy(SET CMP0020 NEW)

include(GNUInstallDirs)
include(CMakeDependentOption)


#######################################################################
option(WITH_KDE "Try to build with KDE support if availibe" ON)
option(WITH_FRONTENDS  "Build frontends currently only useful if WITH_SNORE_DEAMON=ON" OFF)
cmake_dependent_option(WITH_FREEDESKTOP_FRONTEND "Build the freedesktop frontend" OFF "WITH_SNORE_DEAMON" ON)
option(WITH_GROWL_BACKEND "Build the Growl backend" ON)
option(WITH_SNORE_DEAMON "Build the Snore deamon, which redirects notifications" OFF)
option(WITH_QT4 "Use Qt4" ON)
#######################################################################

set(SNORE_VERSION_MAJOR 0)
set(SNORE_VERSION_MINOR 5)
set(SNORE_VERSION_SUFFIX "pre")

if(WITH_QT4 AND WITH_KDE)
    find_package(KDE4)
endif()

set(SNORE_SUFFIX "")
if(KDE4_FOUND)
    add_definitions(-DHAVE_KDE ${KDE4_DEFINITIONS} -D_UNICODE)
    include_directories(${KDE4_INCLUDES})
else()
    if(WITH_QT4)
        find_package(Qt4 REQUIRED)
    else()
        find_package(Qt5Core REQUIRED)
        find_package(Qt5Widgets REQUIRED)
        find_package(Qt5Network REQUIRED)
        find_package(Qt5DBus)
        include(ECMQt4To5Porting)
        set(SNORE_SUFFIX "-qt5")
    endif()
    include_directories( ${QT_INCLUDES} )
    include(NoKDE)
endif()


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
    message(STATUS "No doxygen package found")
endif(DOXYGEN_FOUND)


set(LIBSNORE_PLUGIN_PATH  ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/libsnore${SNORE_SUFFIX})
set(SNORE_PLUGIN_INSTALL_PATH LIBRARY DESTINATION ${LIBSNORE_PLUGIN_PATH})
add_definitions(-DLIBSNORE_PLUGIN_PATH="${LIBSNORE_PLUGIN_PATH}" -DSNORE_SUFFIX="${SNORE_SUFFIX}")
message(STATUS "Installing plugins to ${LIBSNORE_PLUGIN_PATH}")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


add_subdirectory(data)
add_subdirectory(share)
add_subdirectory(src)


