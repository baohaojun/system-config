
add_library(Qt5::QGtk3ThemePlugin MODULE IMPORTED)

_populate_Gui_plugin_properties(QGtk3ThemePlugin RELEASE "platformthemes/libqgtk3.so")

list(APPEND Qt5Gui_PLUGINS Qt5::QGtk3ThemePlugin)
