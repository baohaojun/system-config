QT4_ADD_RESOURCES(SNORE_RCS ${CMAKE_CURRENT_SOURCE_DIR}/snore_notification.qrc)

set( SNORE_SRC
    snorenotifier.cpp
    notifywidget.cpp
    ${SNORE_RCS}
    )


add_library(libsnore_backend_snore MODULE ${SNORE_SRC} )
target_link_libraries(libsnore_backend_snore Snore::Libsnore ${QT_QTCORE_LIBRARY} ${QT_QTDECLARATIVE_LIBRARY})

install(TARGETS libsnore_backend_snore ${SNORE_PLUGIN_INSTALL_PATH})

