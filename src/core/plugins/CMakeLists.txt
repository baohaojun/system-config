set(SnoreNotify_SRCS ${SnoreNotify_SRCS}
        plugins/plugincontainer.cpp
        plugins/plugins.cpp
        plugins/snorefrontend.cpp
        plugins/snorebackend.cpp
        plugins/pluginsettingswidget.cpp
        PARENT_SCOPE)

set(SnoreNotify_HDR
        plugins.h
        snorefrontend.h
        snorebackend.h
        pluginsettingswidget.h)


install(FILES ${SnoreNotify_HDR} DESTINATION ${KDE_INSTALL_INCLUDEDIR}/snore/core/plugins)
