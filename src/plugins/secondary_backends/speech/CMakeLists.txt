find_package(Qt5TextToSpeech QUIET)
set_package_properties(Qt5TextToSpeech PROPERTIES
                                    PURPOSE "Support for speech Notifications"
                                    TYPE OPTIONAL)
if(Qt5TextToSpeech_FOUND)
    add_snore_plugin(Speech SOURCES speech.cpp TYPE SecondaryBackend LIBS Qt5::TextToSpeech)
endif()
