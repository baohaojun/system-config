set( SNARL_NETWORK_SRC
    snarlnetwork.cpp
    parser.cpp
    )
automoc4_add_library(snarlnetwork MODULE  ${SNARL_NETWORK_SRC} )
target_link_libraries(snarlnetwork snorecore ${QT_QTNETWORK_LIBRARY} )
