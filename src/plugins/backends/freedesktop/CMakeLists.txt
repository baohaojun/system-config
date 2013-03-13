if(QT_QTDBUS_FOUND AND NOT WITH_FREEDESKTOP_FRONTEND AND UNIX AND NOT APPLE)
      message(STATUS "Adding Freedesktop notification backend")
      set ( FREEDESKTOP_NOTIFICATION_SRC
              freedesktopnotification_backend.cpp
              fredesktopnotification.cpp
          )
    add_library(freedesktop_backend MODULE  ${FREEDESKTOP_NOTIFICATION_SRC} )
    target_link_libraries(freedesktop_backend  snorecore  ${QT_QTGUI_LIBRARY} ${QT_QTDBUS_LIBRARY} )

    install(TARGETS freedesktop_backend ${SNORE_BACKEND_INSTALL_PATH})
endif(QT_QTDBUS_FOUND AND NOT WITH_FREEDESKTOP_FRONTEND AND UNIX AND NOT APPLE)

