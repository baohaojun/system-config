
if(WIN32)
    message(STATUS "Adding SnoreToast notification backend")
    set( TOASTER_SRC
        snoretoast.cpp
        )
    add_library(snoretoast MODULE ${TOASTER_SRC} )
    target_link_libraries(snoretoast snorecore ${QT_QTCORE_LIBRARY} )

    install(TARGETS snoretoast ${SNORE_BACKEND_INSTALL_PATH})

endif(WIN32)
