if(QT_QTDBUS_FOUND AND NOT WITH_FREEDESKTOP_FRONTEND AND UNIX AND NOT APPLE)
      message(STATUS "Adding Freedesktop notification backend")

      set ( FREEDESKTOP_NOTIFICATION_SRC
              freedesktopnotification_backend.cpp
              fredesktopnotification.cpp
          )

      qt4_add_dbus_interface( FREEDESKTOP_NOTIFICATION_SRC ../../frontends/freedesktop/org.freedesktop.Notifications.xml notificationinterface )


    add_library(libsnore_backend_freedesktop MODULE  ${FREEDESKTOP_NOTIFICATION_SRC} )
    target_link_libraries(libsnore_backend_freedesktop snorecore  ${QT_QTGUI_LIBRARY} ${QT_QTDBUS_LIBRARY} )

    install(TARGETS libsnore_backend_freedesktop ${SNORE_PLUGIN_INSTALL_PATH})
endif(QT_QTDBUS_FOUND AND NOT WITH_FREEDESKTOP_FRONTEND AND UNIX AND NOT APPLE)

