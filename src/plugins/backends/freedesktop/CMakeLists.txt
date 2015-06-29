if(NOT WITH_FREEDESKTOP_FRONTEND AND UNIX AND NOT APPLE)
    find_package(Qt5DBus QUIET)
    set_package_properties(Qt5DBus  PROPERTIES
                                    PURPOSE "Support for the Freedesktop Notification Backend."
                                    TYPE OPTIONAL)
    if(Qt5DBus_FOUND)

        set ( FREEDESKTOP_NOTIFICATION_SRC
              freedesktopnotification_backend.cpp
              fredesktopnotification.cpp
          )

        qt5_add_dbus_interface( FREEDESKTOP_NOTIFICATION_SRC ../../frontends/freedesktop/org.freedesktop.Notifications.xml notificationinterface )


        add_library(libsnore_backend_freedesktop MODULE  ${FREEDESKTOP_NOTIFICATION_SRC} )
        target_link_libraries(libsnore_backend_freedesktop Snore::Libsnore Qt5::DBus )

        install(TARGETS libsnore_backend_freedesktop ${SNORE_PLUGIN_INSTALL_PATH})
    endif()
endif()

