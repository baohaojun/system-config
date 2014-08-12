
if(WIN32)
    message(STATUS "Adding SnoreToast notification backend")
    set( TOASTER_SRC
        snoretoast.cpp
        )
    add_library(libsnore_backend_snoretoast MODULE ${TOASTER_SRC} )
    target_link_libraries(libsnore_backend_snoretoast Snore::Libsnore ${QT_QTCORE_LIBRARY} )

    install(TARGETS libsnore_backend_snoretoast ${SNORE_PLUGIN_INSTALL_PATH})

endif(WIN32)
