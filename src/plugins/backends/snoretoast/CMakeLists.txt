if(WIN32)
    add_snore_plugin(snoretoast SOURCES snoretoast.cpp TYPE backend)
    install(FILES SnoreToast.exe ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})
endif(WIN32)
