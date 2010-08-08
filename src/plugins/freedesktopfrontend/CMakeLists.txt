if(QT_QTDBUS_FOUND AND WITH_FREEDESKTOP_FRONTEND)
    message(STATUS "Adding freedesktop notification frontend")

    set ( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC
	    freedesktopnotificationfrontend.cpp
            ../freedesktopnotification/fredesktopnotification.cpp
	)

    qt4_add_dbus_adaptor( FREEDESKTOP_NOTIFICATION_FRONTEND_SRC org.freedesktop.Notifications.xml freedesktopnotificationfrontend.h FreedesktopNotification_Frontend)

    automoc4_add_library(freedesktop_frontend MODULE  ${FREEDESKTOP_NOTIFICATION_FRONTEND_SRC} )
    target_link_libraries(freedesktop_frontend  snorecore ${QT_QTDBUS_LIBRARY} ${QT_QTGUI_LIBRARY} )


    install(TARGETS freedesktop_frontend ${PLUGIN_INSTALL_PATH})
endif(QT_QTDBUS_FOUND AND WITH_FREEDESKTOP_FRONTEND)



