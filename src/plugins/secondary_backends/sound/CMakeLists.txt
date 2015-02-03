find_package(Qt5Multimedia QUIET)
set_package_properties(Qt5Multimedia PROPERTIES
                                    PURPOSE "Support for sound Notifications"
                                    TYPE OPTIONAL)
if(Qt5Multimedia_FOUND)

    set( SOUND_SRC
        sound.cpp
        soundsettings.cpp
        )

    add_library(libsnore_secondary_backend_sound MODULE ${SOUND_SRC} )
    target_link_libraries(libsnore_secondary_backend_sound Snore::Libsnore Qt5::Multimedia)

    install(TARGETS libsnore_secondary_backend_sound ${SNORE_PLUGIN_INSTALL_PATH})
endif()
