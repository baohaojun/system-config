set(SnoreNotify_SRCS ${SnoreNotify_SRCS}
        plugins/plugincontainer.cpp
        plugins/plugins.cpp
        plugins/snorefrontend.cpp
        plugins/snorebackend.cpp
        plugins/snoresecondarybackend.cpp
        plugins/settingsplugin.cpp
        PARENT_SCOPE)

set(SnoreNotify_HDR
        plugins.h
        snorefrontend.h
        snorebackend.h
        snoresecondarybackend.h
        settingsplugin.h)


install(FILES ${SnoreNotify_HDR} DESTINATION ${LIBSNORE_INCLUDE_DIR}/plugins)
