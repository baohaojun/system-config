if(Qt5Widgets_FOUND)
    add_snore_plugin(Trayicon SOURCES trayiconnotifer.cpp TYPE Backend LIBS Qt5::Widgets)
endif()
