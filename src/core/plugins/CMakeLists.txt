set ( SnoreNotify_SRCS ${SnoreNotify_SRCS}
        plugins/plugincontainer.cpp
        plugins/plugins.cpp
        plugins/snorefrontend.cpp
        plugins/snorebackend.cpp
        PARENT_SCOPE)

set ( Plugins_HDR
        plugins.h
        snorefrontend.h
        snorebackend.h
        )

install(FILES ${Plugins_HDR} DESTINATION include/snore/core/plugins)




