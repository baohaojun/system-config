include_directories(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/core ${CMAKE_CURRENT_BINARY_DIR}/core)

add_subdirectory(core)

if(WITH_SNORE_DEAMON)

    add_executable( snorenotify WIN32 main.cpp snorenotify.cpp trayicon.cpp ${SNORENOTIFY_DEAMON_DEPS})
    target_link_libraries( snorenotify libsnore ${QT_QTGUI_LIBRARY} )

    if(KDE5_FOUND)
        target_link_libraries( snorenotify KF5::CoreAddons KF5::I18n)
    endif()

    install(TARGETS snorenotify RUNTIME DESTINATION bin
                              LIBRARY DESTINATION lib
                              ARCHIVE DESTINATION lib)
endif()

add_subdirectory(plugins)
