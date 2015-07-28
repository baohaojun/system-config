find_package(Qt5Quick QUIET)
set_package_properties(Qt5Quick PROPERTIES
                                PURPOSE "Adding buildin notifiaction backend."
                                TYPE OPTIONAL)

                                
if(Qt5Quick_FOUND)
    QT5_ADD_RESOURCES(SNORENOTIFIER_RCS ${CMAKE_CURRENT_SOURCE_DIR}/snore_notification.qrc)

    set( SNORE_SRC
        snorenotifier.cpp
        snorenotifiersettings.cpp
        notifywidget.cpp
        ${SNORENOTIFIER_RCS}
        )


    add_library(libsnore_backend_snore MODULE ${SNORE_SRC} )
    target_link_libraries(libsnore_backend_snore Snore::Libsnore Qt5::Quick)

    install(TARGETS libsnore_backend_snore ${SNORE_PLUGIN_INSTALL_PATH})
endif()