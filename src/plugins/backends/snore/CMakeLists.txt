find_package(Qt5Quick QUIET)
set_package_properties(Qt5Quick PROPERTIES
                                PURPOSE "Adding buildin notifiaction backend."
                                TYPE OPTIONAL)

                                
if(Qt5Quick_FOUND)
    QT5_ADD_RESOURCES(SNORENOTIFIER_RCS ${CMAKE_CURRENT_SOURCE_DIR}/snore_notification.qrc)

    add_snore_plugin(snore SOURCES snorenotifier.cpp
                                   notifywidget.cpp
                                   ${SNORENOTIFIER_RCS}
                           TYPE backend SETTINGS_SOURCES snorenotifiersettings.cpp LIBS Qt5::Quick)
endif()
