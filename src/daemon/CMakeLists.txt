if(WITH_SNORE_DAEMON)
    include(ECMAddAppIcon)
    ecm_add_app_icon(SNORENOTIFY_DEAMON_DEPS ICONS ${PROJECT_SOURCE_DIR}/data/128-snore.png)

    add_executable( snorenotify WIN32 main.cpp snorenotify.cpp trayicon.cpp ${SNORENOTIFY_DEAMON_DEPS})
    target_link_libraries( snorenotify libsnore Qt5::Gui Qt5::Widgets )
    install(TARGETS snorenotify RUNTIME DESTINATION bin
                                LIBRARY DESTINATION lib
                                ARCHIVE DESTINATION lib)

    if(UNIX)
        install(FILES ${PROJECT_SOURCE_DIR }/data/128-snore.png DESTINATION ${KDE_ICONDIR}/snore.png)
        configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
        install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${KDE_APPDIR})
    endif()

endif()
