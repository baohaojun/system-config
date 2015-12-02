find_package(SnoreGrowl++ QUIET)
set_package_properties(SnoreGrowl++ PROPERTIES
                                    DESCRIPTION "SnoreGrowl is a library providing Growl network notifications."
                                    URL "https://github.com/Snorenotify/SnoreGrowl"
                                    PURPOSE "Enable libgrowl backend"
                                    TYPE RECOMMENDED)

if(SnoreGrowl++_FOUND)
    add_snore_plugin(growl SOURCES growlbackend.cpp SETTINGS_SOURCES growlsettings.cpp TYPE backend LIBS Snore::SnoreGrowl++)
endif()
