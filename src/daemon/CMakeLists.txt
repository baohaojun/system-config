if(WITH_SNORE_DAEMON)
    include(ECMAddAppIcon)
    include(ECMInstallIcons)
    ecm_add_app_icon(SNORENOTIFY_DAEMON_DEPS ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png)

    qt5_wrap_ui(UI settingsdialog.ui)

    add_executable( snorenotify WIN32 main.cpp snorenotify.cpp trayicon.cpp settingsdialog.cpp ${UI} ${SNORENOTIFY_DAEMON_DEPS})
    target_link_libraries( snorenotify libsnore Qt5::Gui Qt5::Widgets )
    install(TARGETS snorenotify ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})

    if(UNIX)
        ecm_install_icons(ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png DESTINATION ${KDE_INSTALL_ICONDIR})
        configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
        install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${KDE_INSTALL_APPDIR})
    endif()

endif()
