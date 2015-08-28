project( SnoreNotify )
cmake_minimum_required( VERSION 2.8.12 )

include(FeatureSummary)

find_package(ECM 1.7.0 REQUIRED NO_MODULE)
set_package_properties(ECM      PROPERTIES
                                DESCRIPTION "Extra modules and scripts for CMake."
                                URL "https://projects.kde.org/projects/kdesupport/extra-cmake-modules"
                                TYPE REQUIRED)

set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH} ${ECM_KDE_MODULE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules )

include(KDEInstallDirs)
include(KDECMakeSettings)
include(KDEFrameworkCompilerSettings)

include(GenerateExportHeader)
include (CMakeDependentOption)

include(ECMAddAppIcon)
include(ECMGeneratePriFile)
include(ECMInstallIcons)
include(ECMMarkNonGuiExecutable)
include(ECMOptionalAddSubdirectory)
include(ECMPackageConfigHelpers)
include(ECMPoQmTools)
include(ECMSetupVersion)


set(SNORE_VERSION_MAJOR 0)
set(SNORE_VERSION_MINOR 5)
set(SNORE_VERSION_PATCH 73)

set(SNORE_SUFFIX "-qt5")
set(SNORE_CamelCase_SUFFIX "Qt5")


find_package(Qt5Core REQUIRED)
find_package(Qt5Network REQUIRED)
find_package(Qt5Widgets REQUIRED)


set(LIBSNORE_PLUGIN_PATH ${KDE_INSTALL_PLUGINDIR}/libsnore${SNORE_SUFFIX})
set(SNORE_PLUGIN_INSTALL_PATH LIBRARY DESTINATION ${LIBSNORE_PLUGIN_PATH})
message(STATUS "Installing plugins to ${LIBSNORE_PLUGIN_PATH}")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)

add_subdirectory(data)
add_subdirectory(src)
add_subdirectory(autotest)

if (IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/po")
    ecm_install_po_files_as_qm(po)
endif()


feature_summary(WHAT ALL INCLUDE_QUIET_PACKAGES)
