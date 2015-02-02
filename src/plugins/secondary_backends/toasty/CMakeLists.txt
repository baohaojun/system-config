qt5_wrap_ui(UI toastysettings.ui)

set( TOASTY_SRC
    toasty.cpp
    toastysettings.cpp
    ${UI}
    )

add_library(libsnore_secondary_backend_toasty MODULE ${TOASTY_SRC} )
target_link_libraries(libsnore_secondary_backend_toasty Snore::Libsnore)

install(TARGETS libsnore_secondary_backend_toasty ${SNORE_PLUGIN_INSTALL_PATH})

