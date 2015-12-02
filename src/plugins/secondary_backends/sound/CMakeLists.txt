find_package(Qt5Multimedia QUIET)
set_package_properties(Qt5Multimedia PROPERTIES
                                    PURPOSE "Support for sound Notifications"
                                    TYPE OPTIONAL)
if(Qt5Multimedia_FOUND)
    add_snore_plugin(sound SOURCES sound.cpp SETTINGS_SOURCES soundsettings.cpp TYPE secondary_backend LIBS Qt5::Multimedia)
endif()
