project( SnoreNotify )
cmake_minimum_required( VERSION 2.8.12 )

find_package(ECM 1.7.0 REQUIRED NO_MODULE)

set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH} ${ECM_KDE_MODULE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules )

include(KDEInstallDirs)
include(KDECompilerSettings)
include(KDECMakeSettings)

include(ECMAddAppIcon)
include(ECMInstallIcons)

include(GenerateExportHeader)
include(FeatureSummary)



#######################################################################
option(WITH_FRONTENDS  "Build frontends currently only useful if WITH_SNORE_DAEMON=ON" OFF)
option(WITH_SNORE_DAEMON "Build the Snore daemon, which redirects notifications" OFF)
#######################################################################

set(SNORE_VERSION_MAJOR 0)
set(SNORE_VERSION_MINOR 5)
set(SNORE_VERSION_PATCH 90)
set(SNORE_VERSION_SUFFIX "")

set(SNORE_SUFFIX "-qt5")
set(SNORE_CamelCase_SUFFIX "Qt5")


find_package(Qt5Core REQUIRED)
find_package(Qt5Widgets REQUIRED)
find_package(Qt5Network REQUIRED)
find_package(Qt5Quick REQUIRED)


find_package(Doxygen QUIET)
set_package_properties(Doxygen  PROPERTIES
                                DESCRIPTION "Doxygen a tool for generating documentation."
                                URL "http://www.stack.nl/~dimitri/doxygen/"
                                PURPOSE "Generation of API documentation"
                                TYPE OPTIONAL)

if(DOXYGEN_FOUND)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in" "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" @only)

    add_custom_target(doc
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMENT "Generating API documentation with Doxygen" VERBATIM
    )
endif(DOXYGEN_FOUND)



set(LIBSNORE_PLUGIN_PATH ${KDE_INSTALL_PLUGINDIR}/libsnore${SNORE_SUFFIX})
set(SNORE_PLUGIN_INSTALL_PATH LIBRARY DESTINATION ${LIBSNORE_PLUGIN_PATH})
message(STATUS "Installing plugins to ${LIBSNORE_PLUGIN_PATH}")


add_subdirectory(data)
add_subdirectory(share)
add_subdirectory(src)


feature_summary(WHAT ALL INCLUDE_QUIET_PACKAGES)
