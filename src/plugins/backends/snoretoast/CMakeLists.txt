if(WIN32)
    add_snore_plugin(snoretoast SOURCES snoretoast.cpp TYPE backend)
    install(FILES SnoreToast.exe DESTINATION bin)
endif(WIN32)
