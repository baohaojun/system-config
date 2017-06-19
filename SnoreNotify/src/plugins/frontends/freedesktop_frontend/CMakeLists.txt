find_package(Qt5DBus QUIET)
set_package_properties(Qt5DBus  PROPERTIES
                                PURPOSE "Support for the Freedesktop Notification Frontend."
                                TYPE OPTIONAL)
if(Qt5DBus_FOUND)
    cmake_dependent_option(INSTALL_freedesktop_notification_service "Install snorenotify as a dbus service for recieving Notifications." ON "BUILD_daemon;WIN32 OR APPLE" OFF)
    add_feature_info(INSTALL_freedesktop_notification_service INSTALL_freedesktop_notification_service "Install snorenotify as a dbus service for recieving Notifications.")


    set( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC
            freedesktopnotificationfrontend.cpp
            ../../backends/freedesktop_backend/fredesktopnotification.cpp
        )

    qt5_add_dbus_adaptor( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC org.freedesktop.Notifications.xml freedesktopnotificationfrontend.h FreedesktopFrontend)
    
    add_snore_plugin(Freedesktop SOURCES ${FREEDESKTOP_NOTIFICATION_FRONTEND_SRC} TYPE Frontend LIBS Qt5::DBus)


    #install the dbus interface
    if(INSTALL_freedesktop_notification_service)
        if(WIN32)
            set(SNORE_LOCATION ../bin/snorenotify)
        else()
            set(SNORE_LOCATION ${KDE_INSTALL_BINDIR}/snorenotify)
        endif()
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/org.freedesktop.Notifications.service.cmake ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service)

        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service
                              DESTINATION ${KDE_INSTALL_DBUSSERVICEDIR})
    endif()

    install(TARGETS libsnore_frontend_freedesktop ${SNORE_PLUGIN_INSTALL_PATH})

endif()



