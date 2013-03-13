#deprecated
set( SNARL_NETWORK_SRC
    snarlnetwork.cpp
    parser.cpp
    )
add_library(snarlnetwork MODULE  ${SNARL_NETWORK_SRC} )
target_link_libraries(snarlnetwork snorecore ${QT_QTNETWORK_LIBRARY} )

install(TARGETS snarlnetwork ${SNORE_FRONTEND_INSTALL_PATH})
