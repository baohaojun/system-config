find_package(Qt5WebSockets QUIET)

set_package_properties(Qt5WebSockets  PROPERTIES
                                PURPOSE "Support for the Pushover Notification frontend."
                                TYPE OPTIONAL)

if(Qt5WebSockets_FOUND)
    add_snore_plugin(Pushover   SOURCES pushover_frontend.cpp pushoverclient.cpp LIBS Qt5::WebSockets
                                SETTINGS_SOURCES pushoversettings.cpp pushoverclient.cpp SETTINGS_LIBS Qt5::WebSockets
                                TYPE Frontend )
endif()
