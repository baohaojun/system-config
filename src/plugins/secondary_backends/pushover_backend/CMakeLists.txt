set( PUSHOVER_SRC
    pushover.cpp
    pushoversettings.cpp
    )

add_library(libsnore_secondary_backend_pushover MODULE ${PUSHOVER_SRC} )
target_link_libraries(libsnore_secondary_backend_pushover Snore::Libsnore)

install(TARGETS libsnore_secondary_backend_pushover ${SNORE_PLUGIN_INSTALL_PATH})

