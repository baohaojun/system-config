set( trayicon_SRC
    trayiconnotifer.cpp
    )
add_library(libsnore_backend_trayicon MODULE  ${trayicon_SRC} )
target_link_libraries(libsnore_backend_trayicon Snore::Libsnore ${QT_QTCORE_LIBRARY} )

install(TARGETS libsnore_backend_trayicon ${SNORE_PLUGIN_INSTALL_PATH})
