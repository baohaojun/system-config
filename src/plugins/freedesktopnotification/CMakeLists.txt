if(QT_QTDBUS_FOUND AND UNIX AND NOT APPLE)
      message(STATUS "Adding Freedesktop notification backend")
      set ( FREEDESKTOP_NOTIFICATION_SRC
              freedesktopnotification_backend.cpp
              fredesktopnotification.cpp
          )
    automoc4_add_library(freedesktop_backend MODULE  ${FREEDESKTOP_NOTIFICATION_SRC} )
    target_link_libraries(freedesktop_backend  snorecore  ${QT_QTGUI_LIBRARY} ${QT_QTDBUS_LIBRARY} )

    install(TARGETS freedesktop_backend RUNTIME DESTINATION bin/snoreplugins
                                         LIBRARY DESTINATION lib
                                         ARCHIVE DESTINATION lib)
else(QT_QTDBUS_FOUND AND UNIX AND NOT APPLE)
    message(STATUS "Adding Freedesktop notification backend.... ")
endif(QT_QTDBUS_FOUND AND UNIX AND NOT APPLE)

