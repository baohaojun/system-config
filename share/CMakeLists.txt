if(UNIX AND WITH_SNORE_DEAMON)
    install(FILES 128x128-snore.png DESTINATION ${KDE_ICONDIR}/snore.png)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${KDE_APPDIR})
endif()

if(WIN32)
    add_subdirectory(snoretoast)
endif()
