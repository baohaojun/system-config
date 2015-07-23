include(GetGitRevisionDescription)

get_git_head_revision(GIT_REFSPEC SNORE_REVISION)

if(NOT SNORE_REVISION)
    set(SNORE_REVISION "")
endif()

set(LIBSNORE_INCLUDE_DIR ${KDE_INSTALL_INCLUDEDIR}/libsnore)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/version.cpp.in" "${CMAKE_CURRENT_BINARY_DIR}/version.cpp")

QT5_ADD_RESOURCES(SNORENOTIFY_RCS ${SNORE_RCS})

add_subdirectory(notification)
add_subdirectory(plugins)


qt5_wrap_ui(UI settingsdialog.ui)

list(APPEND SnoreNotify_SRCS
        snore.cpp
        snore_p.cpp
        application.cpp
        application_p.cpp
        alert.cpp
        alert_p.cpp
        hint.cpp
        log.cpp
        settingsdialog.cpp
        utils.cpp
        ${UI}
        ${CMAKE_CURRENT_BINARY_DIR}/version.cpp
        ${SNORENOTIFY_RCS}
        )

list(APPEND SnoreNotify_HDR
        snore.h
        snore_p.h
        application.h
        alert.h
        hint.h
        log.h
        settingsdialog.h
        snoreglobals.h
        utils.h
        ${CMAKE_CURRENT_BINARY_DIR}/snore_exports.h
        version.h
        )


add_library( libsnore SHARED ${SnoreNotify_SRCS})
set_target_properties( libsnore PROPERTIES
                                OUTPUT_NAME "snore${SNORE_SUFFIX}"
                                VERSION "${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}.${SNORE_VERSION_PATCH}.${SNORE_VERSION_SUFFIX}"
                                SOVERSION "${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}"
                                EXPORT_NAME Libsnore)
set_property( TARGET libsnore
              APPEND
              PROPERTY COMPILE_DEFINITIONS LIBSNORE_PLUGIN_PATH="${KDE_INSTALL_FULL_PLUGINDIR}/libsnore${SNORE_SUFFIX}" SNORE_SUFFIX="${SNORE_SUFFIX}"
)
target_link_libraries( libsnore PUBLIC Qt5::Core Qt5::Gui Qt5::Widgets Qt5::Network)

add_library(Snore::Libsnore ALIAS libsnore)

generate_export_header(libsnore
  EXPORT_FILE_NAME snore_exports.h
  EXPORT_MACRO_NAME SNORE_EXPORT
)

target_include_directories( libsnore PUBLIC
                            $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
                            $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>)


ecm_setup_version("${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}.${SNORE_VERSION_PATCH}" VARIABLE_PREFIX SOLID
                        PACKAGE_VERSION_FILE "${CMAKE_CURRENT_BINARY_DIR}/Libsnore${SNORE_CamelCase_SUFFIX}ConfigVersion.cmake"
                        SOVERSION ${SNORE_VERSION_MAJOR})

ecm_configure_package_config_file(  "${CMAKE_CURRENT_SOURCE_DIR}/LibsnoreConfig.cmake.in"
                                    "${CMAKE_CURRENT_BINARY_DIR}/Libsnore${SNORE_CamelCase_SUFFIX}Config.cmake"
                                    INSTALL_DESTINATION ${KDE_INSTALL_CMAKEPACKAGEDIR}/libsnore${SNORE_CamelCase_SUFFIX}
                              )


ecm_generate_pri_file(BASE_NAME Libsnore${SNORE_CamelCase_SUFFIX}
                      LIB_NAME snore${SNORE_SUFFIX}
                      DEPS "core gui widgets network"
                      INCLUDE_INSTALL_DIR ${KDE_INSTALL_INCLUDEDIR}
                      FILENAME_VAR pri_filename)

install(FILES ${pri_filename} DESTINATION ${ECM_MKSPECS_INSTALL_DIR})

install(TARGETS libsnore EXPORT Libsnore${SNORE_CamelCase_SUFFIX}Target ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})

install(EXPORT Libsnore${SNORE_CamelCase_SUFFIX}Target DESTINATION ${KDE_INSTALL_CMAKEPACKAGEDIR}/libsnore${SNORE_CamelCase_SUFFIX} NAMESPACE Snore::)

install(FILES ${SnoreNotify_HDR} DESTINATION ${LIBSNORE_INCLUDE_DIR})

install(FILES
        ${CMAKE_CURRENT_BINARY_DIR}/Libsnore${SNORE_CamelCase_SUFFIX}Config.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/Libsnore${SNORE_CamelCase_SUFFIX}ConfigVersion.cmake
        DESTINATION
        ${KDE_INSTALL_CMAKEPACKAGEDIR}/libsnore${SNORE_CamelCase_SUFFIX})

