ecm_add_app_icon(SNORENOTIFY_DAEMON_DEPS ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png)

add_executable( snorenotify main.cpp snorenotify.cpp trayicon.cpp ${SNORENOTIFY_DAEMON_DEPS})
target_link_libraries( snorenotify Snore::Libsnore Snore::LibsnoreSettings)
install(TARGETS snorenotify ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})

if(UNIX)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${KDE_INSTALL_APPDIR})
endif()
