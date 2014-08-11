set( SNARL_NETWORK_SRC
    snarlnetwork.cpp
    parser.cpp
    )
add_library(libsnore_frontend_snarlnetwork MODULE  ${SNARL_NETWORK_SRC} )
target_link_libraries(libsnore_frontend_snarlnetwork libsnore ${QT_QTNETWORK_LIBRARY} )

install(TARGETS libsnore_frontend_snarlnetwork ${SNORE_PLUGIN_INSTALL_PATH})
