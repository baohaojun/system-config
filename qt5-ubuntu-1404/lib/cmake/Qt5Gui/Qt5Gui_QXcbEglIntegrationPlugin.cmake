
add_library(Qt5::QXcbEglIntegrationPlugin MODULE IMPORTED)

_populate_Gui_plugin_properties(QXcbEglIntegrationPlugin RELEASE "xcbglintegrations/libqxcb-egl-integration.so")

list(APPEND Qt5Gui_PLUGINS Qt5::QXcbEglIntegrationPlugin)
