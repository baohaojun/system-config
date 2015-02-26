ecm_add_app_icon(SNORENOTIFY_SETTINGS_DEPS ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png)

add_executable( snoresettings WIN32 main.cpp ${UI} ${SNORENOTIFY_SETTINGS_DEPS})
target_link_libraries( snoresettings Snore::Libsnore )
install(TARGETS snoresettings ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})

#if(UNIX)
#     ecm_install_icons(ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png DESTINATION ${KDE_INSTALL_ICONDIR})
#     configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
#     install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${KDE_INSTALL_APPDIR})
#endif()

