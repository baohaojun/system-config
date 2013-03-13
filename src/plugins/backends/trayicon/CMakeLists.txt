set( trayicon_SRC
    trayiconnotifer.cpp
    )
add_library(trayicon MODULE  ${trayicon_SRC} )
target_link_libraries(trayicon snorecore ${QT_QTCORE_LIBRARY} )

install(TARGETS trayicon ${SNORE_BACKEND_INSTALL_PATH})
