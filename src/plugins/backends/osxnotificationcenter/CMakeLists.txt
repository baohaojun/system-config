if(APPLE)
    add_snore_plugin(osxnotificationcenter SOURCES osxnotificationcenter.mm TYPE backend LIBS /System/Library/Frameworks/Foundation.framework)
endif(APPLE)
