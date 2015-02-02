QT5_ADD_RESOURCES(SNORENOTIFIER_RCS ${CMAKE_CURRENT_SOURCE_DIR}/snore_notification.qrc)

qt5_wrap_ui(UI snorenotifiersettings.ui)

set( SNORE_SRC
    snorenotifier.cpp
    snorenotifiersettings.cpp
    notifywidget.cpp
    ${SNORENOTIFIER_RCS}
    ${UI}
    )


add_library(libsnore_backend_snore MODULE ${SNORE_SRC} )
target_link_libraries(libsnore_backend_snore Snore::Libsnore Qt5::Quick)

install(TARGETS libsnore_backend_snore ${SNORE_PLUGIN_INSTALL_PATH})

