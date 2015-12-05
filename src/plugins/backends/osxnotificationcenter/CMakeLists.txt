if(APPLE)
    add_snore_plugin("OSX Notification Center" SOURCES osxnotificationcenter.mm TYPE Backend LIBS /System/Library/Frameworks/Foundation.framework)
endif(APPLE)
