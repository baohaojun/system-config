
add_library(Qt5::QEglFSX11IntegrationPlugin MODULE IMPORTED)

_populate_Gui_plugin_properties(QEglFSX11IntegrationPlugin RELEASE "egldeviceintegrations/libqeglfs-x11-integration.so")

list(APPEND Qt5Gui_PLUGINS Qt5::QEglFSX11IntegrationPlugin)
