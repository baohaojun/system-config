
if(WIN32)
    message(STATUS "Adding Snarl notification backend")
    set( SNARL_SRC
        SnarlInterface.cpp
        snarl.cpp
        )
    add_library(snarl MODULE ${SNARL_SRC} )
    target_link_libraries(snarl snorecore ${QT_QTCORE_LIBRARY} )

    install(TARGETS snarl ${SNORE_BACKEND_INSTALL_PATH})

endif(WIN32)
