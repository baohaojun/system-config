
if(WIN32)
    message(STATUS "Adding Snarl notification backend")


    set( SNARL_SRC
        SnarlInterface.cpp
        snarl.cpp
        )
    add_library(libsnore_backend_snarl MODULE ${SNARL_SRC} )
    target_link_libraries(libsnore_backend_snarl Snore::Libsnore ${QT_QTCORE_LIBRARY} )
    if(MINGW)
        set_target_properties(libsnore_backend_snarl PROPERTIES COMPILE_FLAGS "-Wno-conversion-null -Wno-unused")
    endif(MINGW)

    install(TARGETS libsnore_backend_snarl ${SNORE_PLUGIN_INSTALL_PATH})

endif(WIN32)
