
add_library(Qt5::QTDSDriverPlugin MODULE IMPORTED)

_populate_Sql_plugin_properties(QTDSDriverPlugin RELEASE "sqldrivers/libqsqltds.so")

list(APPEND Qt5Sql_PLUGINS Qt5::QTDSDriverPlugin)
