set( NMA_SRC
    nma.cpp
    nmasettings.cpp
    )

add_library(libsnore_secondary_backend_nma MODULE ${NMA_SRC} )
target_link_libraries(libsnore_secondary_backend_nma Snore::Libsnore)

install(TARGETS libsnore_secondary_backend_nma ${SNORE_PLUGIN_INSTALL_PATH})

