ecm_add_app_icon(SNORENOTIFY_SETTINGS_DEPS ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png)

add_executable( snoresettings-cli main-cli.cpp settingsutils.cpp ${SNORENOTIFY_SETTINGS_DEPS})
ecm_mark_nongui_executable(snoresettings-cli)
target_link_libraries( snoresettings-cli Snore::Libsnore)

if(Qt5Widgets_FOUND)
    qt5_wrap_ui(UI settingswindow.ui)
    add_executable( snoresettings main.cpp settingswindow.cpp settingsutils.cpp ${UI} ${SNORENOTIFY_SETTINGS_DEPS})
    target_link_libraries( snoresettings Snore::Libsnore Snore::LibsnoreSettings)
    install(TARGETS snoresettings snoresettings-cli ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})

    if(UNIX)
        configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snoresettings.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snoresettings.desktop" @ONLY)
        install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snoresettings.desktop" DESTINATION ${KDE_INSTALL_APPDIR})
    endif()
endif()
