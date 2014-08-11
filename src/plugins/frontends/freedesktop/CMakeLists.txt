if(QT_QTDBUS_FOUND AND WITH_FREEDESKTOP_FRONTEND)
    message(STATUS "Adding freedesktop notification frontend")
    find_package(KF5 COMPONENTS IconThemes)

    set( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC
            freedesktopnotificationfrontend.cpp
            ../../backends/freedesktop/fredesktopnotification.cpp
        )

    qt4_add_dbus_adaptor( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC org.freedesktop.Notifications.xml freedesktopnotificationfrontend.h FreedesktopFrontend)

    add_library(libsnore_frontend_freedesktop MODULE  ${FREEDESKTOP_NOTIFICATION_FRONTEND_SRC} )
    target_link_libraries(libsnore_frontend_freedesktop libsnore ${QT_QTDBUS_LIBRARY} ${QT_QTGUI_LIBRARY} )

    if(KF5_IconThemes_FOUND)
        target_link_libraries(libsnore_frontend_freedesktop KF5::IconThemes)
    endif()


    #install the dbus interface
    if(WITH_SNORE_DEAMON)
        if(WIN32)
            set(SNORE_LOCATION ../bin/snorenotify)
        else(WIN32)
                set(SNORE_LOCATION ${CMAKE_INSTALL_PREFIX}/bin/snorenotify)
        endif(WIN32)
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/org.freedesktop.Notifications.service.cmake ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service)

        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service
                              DESTINATION share/dbus-1/services/)
    endif(WITH_SNORE_DEAMON)

    install(TARGETS libsnore_frontend_freedesktop ${SNORE_PLUGIN_INSTALL_PATH})

endif(QT_QTDBUS_FOUND AND WITH_FREEDESKTOP_FRONTEND)



