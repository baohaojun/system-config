ecm_optional_add_subdirectory(backends)
add_feature_info(BUILD_backends BUILD_backends "Build and install the backends, which are used to display interactive notifications.")

ecm_optional_add_subdirectory(secondary_backends)
add_feature_info(BUILD_secondary_backends BUILD_secondary_backends "Build and install the secondary_backends, which are used to display non interactive notifications.")

ecm_optional_add_subdirectory(frontends)
add_feature_info(BUILD_frontends BUILD_frontends "Build and install the frontends, which are used to receive notifications.")

ecm_optional_add_subdirectory(plugins)
