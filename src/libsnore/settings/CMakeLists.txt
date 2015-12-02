QT5_ADD_RESOURCES(SNORENOTIFY_RCS ${SNORE_RCS})


qt5_wrap_ui(UI settingsdialog.ui)

set(snoresettings_SRCS
        settings.cpp
        settingsdialog.cpp
        pluginsettingswidget.cpp
        ${UI}
        )

set(snoresettings_HDR
    settings.h
    settingsdialog.h
    pluginsettingswidget.h
    ${CMAKE_CURRENT_BINARY_DIR}/snore_settings_exports.h
    )


add_library( libsnoresettings SHARED ${snoresettings_SRCS})
set_target_properties( libsnoresettings PROPERTIES
                                OUTPUT_NAME "snoresettings${SNORE_SUFFIX}"
                                VERSION "${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}.${SNORE_VERSION_PATCH}"
                                SOVERSION "${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}"
                                EXPORT_NAME LibsnoreSettings)
                                
target_link_libraries( libsnoresettings PUBLIC Qt5::Widgets Snore::Libsnore)

add_library(Snore::LibsnoreSettings ALIAS libsnoresettings)

generate_export_header(libsnoresettings
  EXPORT_FILE_NAME snore_settings_exports.h
  EXPORT_MACRO_NAME SNORE_SETTINGS_EXPORT
)


ecm_setup_version("${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}.${SNORE_VERSION_PATCH}" VARIABLE_PREFIX SNORE
                        PACKAGE_VERSION_FILE "${CMAKE_CURRENT_BINARY_DIR}/LibsnoreSettings${SNORE_CamelCase_SUFFIX}ConfigVersion.cmake"
                        SOVERSION ${SNORE_VERSION_MAJOR})

ecm_configure_package_config_file(  "${CMAKE_CURRENT_SOURCE_DIR}/LibsnoreSettingsConfig.cmake.in"
                                    "${CMAKE_CURRENT_BINARY_DIR}/LibsnoreSettings${SNORE_CamelCase_SUFFIX}Config.cmake"
                                    INSTALL_DESTINATION ${KDE_INSTALL_CMAKEPACKAGEDIR}/libsnoresettings${SNORE_CamelCase_SUFFIX}
                              )


ecm_generate_pri_file(BASE_NAME LibsnoreSettings${SNORE_CamelCase_SUFFIX}
                      LIB_NAME snoresettings${SNORE_SUFFIX}
                      DEPS "core gui widgets network LibsnoreQt5"
                      INCLUDE_INSTALL_DIR ${KDE_INSTALL_INCLUDEDIR}
                      FILENAME_VAR pri_filename)

install(FILES ${pri_filename} DESTINATION ${ECM_MKSPECS_INSTALL_DIR})

install(TARGETS libsnoresettings EXPORT LibsnoreSettings${SNORE_CamelCase_SUFFIX}Target ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})

install(EXPORT LibsnoreSettings${SNORE_CamelCase_SUFFIX}Target DESTINATION ${KDE_INSTALL_CMAKEPACKAGEDIR}/libsnoresettings${SNORE_CamelCase_SUFFIX} NAMESPACE Snore::)

install(FILES ${snoresettings_HDR} DESTINATION ${LIBSNORE_INCLUDE_DIR}/settings)

install(FILES
        ${CMAKE_CURRENT_BINARY_DIR}/LibsnoreSettings${SNORE_CamelCase_SUFFIX}Config.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/LibsnoreSettings${SNORE_CamelCase_SUFFIX}ConfigVersion.cmake
        DESTINATION
        ${KDE_INSTALL_CMAKEPACKAGEDIR}/libsnoresettings${SNORE_CamelCase_SUFFIX})
        
