if(WITH_FREEDESKTOP_FRONTEND)
    find_package(Qt5DBus QUIET)
    set_package_properties(Qt5DBus  PROPERTIES
                                    PURPOSE "Support for Freedesktop Notifications"
                                    TYPE OPTIONAL)

    if(Qt5DBus_FOUND)
        message(STATUS "Adding freedesktop notification frontend")

        set( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC
                freedesktopnotificationfrontend.cpp
                ../../backends/freedesktop/fredesktopnotification.cpp
            )

        qt5_add_dbus_adaptor( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC org.freedesktop.Notifications.xml freedesktopnotificationfrontend.h FreedesktopFrontend)

        add_library(libsnore_frontend_freedesktop MODULE  ${FREEDESKTOP_NOTIFICATION_FRONTEND_SRC} )
        target_link_libraries(libsnore_frontend_freedesktop Snore::Libsnore Qt5::DBus )


        #install the dbus interface
        if(WITH_SNORE_DAEMON)
            if(WIN32)
                set(SNORE_LOCATION ../bin/snorenotify)
            else()
                set(SNORE_LOCATION ${CMAKE_INSTALL_PREFIX}/bin/snorenotify)
            endif()
            configure_file(${CMAKE_CURRENT_SOURCE_DIR}/org.freedesktop.Notifications.service.cmake ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service)

            install(FILES ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service
                                  DESTINATION share/dbus-1/services/)
        endif(WITH_SNORE_DAEMON)

        install(TARGETS libsnore_frontend_freedesktop ${SNORE_PLUGIN_INSTALL_PATH})

    endif()
endif()



