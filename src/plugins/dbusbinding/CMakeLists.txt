if(QT_QTDBUS_FOUND)
	message(STATUS "Found QtDBus, enabling D-Bus support")
	add_definitions(-DHAVE_DBUS)
	set( DBUSBINDING_SRC
	    dbusbinding.cpp
	    )
        automoc4_add_library(dbusbinding MODULE  ${DBUSBINDING_SRC} )
        target_link_libraries(dbusbinding ${QT_QTDBUS_LIBRARY} snore  )
endif(QT_QTDBUS_FOUND)
