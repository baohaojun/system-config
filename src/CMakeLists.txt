add_subdirectory(libsnore)
ecm_optional_add_subdirectory(daemon)
add_feature_info(BUILD_daemon BUILD_daemon "Build and installd the snorenotify daemon which recieves and redirects notifications.")

ecm_optional_add_subdirectory(settings)
add_feature_info(BUILD_settings BUILD_settings "Build and installd the snorensettings application which allows the modification of the settings for all applications using libsnore.")

ecm_optional_add_subdirectory(snoresend)
add_feature_info(BUILD_snoresend BUILD_snoresend "Build and installd the snoresend application, a commandline interface for sendig notifications.")

add_subdirectory(plugins)
