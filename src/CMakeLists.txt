include(libsnore/SnoreAddPlugin.cmake)

add_subdirectory(libsnore)

if(Qt5Widgets_FOUND)
    ecm_optional_add_subdirectory(daemon)
    add_feature_info(BUILD_daemon BUILD_daemon "Build and install the snorenotify daemon which receives and redirects notifications.")
endif()

ecm_optional_add_subdirectory(settings)
add_feature_info(BUILD_settings BUILD_settings "Build and install the snorensettings application which allows the modification of the settings for all applications using libsnore.")

ecm_optional_add_subdirectory(snoresend)
add_feature_info(BUILD_snoresend BUILD_snoresend "Build and install the snoresend application, a commandline interface for sendig notifications.")

add_subdirectory(plugins)
