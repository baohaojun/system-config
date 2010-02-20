if(QT_QTDBUS_FOUND)
      message(STATUS "Adding Freedesktop notification backend")
      set ( FREEDESKTOP_NOTIFICATION_SRC
              freedesktopnotification_backend.cpp
              fredesktopnotification.cpp
          )
    automoc4_add_library(freedesktop_backend MODULE  ${FREEDESKTOP_NOTIFICATION_SRC} )
    target_link_libraries(freedesktop_backend  snore  ${QT_QTGUI_LIBRARY} ${QT_QTDBUS_LIBRARY} )
else(QT_QTDBUS_FOUND)
    message(STATUS "Adding Freedesktop notification backend.... for Freedesktop notification backend support QDBus is needed")
endif(QT_QTDBUS_FOUND)

