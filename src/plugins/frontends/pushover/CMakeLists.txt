find_package(Qt5WebSockets REQUIRED)

set( PUSHOVER_FRONTEND_SRC
     pushover_frontend.cpp
    )

add_library(libsnore_frontend_pushover MODULE  ${PUSHOVER_FRONTEND_SRC} )
target_link_libraries(libsnore_frontend_pushover Snore::Libsnore Qt5::WebSockets )

