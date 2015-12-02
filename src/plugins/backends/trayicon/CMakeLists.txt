if(Qt5Widgets_FOUND)
    add_snore_plugin(trayicon SOURCES trayiconnotifer.cpp TYPE backend LIBS Qt5::Widgets)
endif()