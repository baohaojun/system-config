set( SNORE_SRC
    snorenotifier.cpp
    notifywidget.cpp
    DPIScaler.cpp
    )

set( SNORE_FORMS
    notifywidget.ui)

qt4_wrap_ui(SNORE_UI ${SNORE_FORMS})

add_library(libsnore_backend_snore MODULE ${SNORE_SRC} ${SNORE_UI} )
target_link_libraries(libsnore_backend_snore snorecore ${QT_QTCORE_LIBRARY} )

install(TARGETS libsnore_backend_snore ${SNORE_PLUGIN_INSTALL_PATH})

