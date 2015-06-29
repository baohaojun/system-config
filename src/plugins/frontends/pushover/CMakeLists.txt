find_package(Qt5WebSockets QUIET)

set_package_properties(Qt5WebSockets  PROPERTIES
                                PURPOSE "Support for the Pushover Notification frontend."
                                TYPE OPTIONAL)

if(Qt5WebSockets_FOUND)
    set( PUSHOVER_FRONTEND_SRC
         pushover_frontend.cpp
        pushoversettings.cpp
        )

    add_library(libsnore_frontend_pushover MODULE  ${PUSHOVER_FRONTEND_SRC} )
    target_link_libraries(libsnore_frontend_pushover Snore::Libsnore Qt5::WebSockets )

endif()
