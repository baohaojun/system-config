if(WIN32)
    add_snore_plugin("Windows Toast" SOURCES snoretoast.cpp TYPE Backend)
    install(FILES SnoreToast.exe DESTINATION bin)
endif(WIN32)
