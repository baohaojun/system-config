find_package(Qt5DBus QUIET)
set_package_properties(Qt5DBus  PROPERTIES
                                PURPOSE "Support for the Freedesktop Notification Backend."
                                TYPE OPTIONAL)
if(Qt5DBus_FOUND)

    set ( FREEDESKTOP_NOTIFICATION_SRC
          freedesktopnotification_backend.cpp
          fredesktopnotification.cpp
      )

    qt5_add_dbus_interface( FREEDESKTOP_NOTIFICATION_SRC ../../frontends/freedesktop_frontend/org.freedesktop.Notifications.xml notificationinterface )

    add_snore_plugin(Freedesktop SOURCES ${FREEDESKTOP_NOTIFICATION_SRC} TYPE Backend LIBS Qt5::DBus)

endif()

