if(WIN32)
    message(STATUS "Adding Snarl notification backend")
    set( SNARL__SRC
        SnarlInterface.cpp
        snarl_backend.cpp
        )
    automoc4_add_library(snarln_backend MODULE  ${SNARL__SRC} )
    target_link_libraries(snarln_backend snorecore ${QT_QTCORE_LIBRARY} )

    install(TARGETS snarln_backend ${SNORE_BACKEND_INSTALL_PATH})

endif(WIN32)
