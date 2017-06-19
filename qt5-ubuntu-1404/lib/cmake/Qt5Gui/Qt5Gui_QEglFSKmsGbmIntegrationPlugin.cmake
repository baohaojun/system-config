
add_library(Qt5::QEglFSKmsGbmIntegrationPlugin MODULE IMPORTED)

_populate_Gui_plugin_properties(QEglFSKmsGbmIntegrationPlugin RELEASE "egldeviceintegrations/libqeglfs-kms-integration.so")

list(APPEND Qt5Gui_PLUGINS Qt5::QEglFSKmsGbmIntegrationPlugin)
