
add_library(Qt5::QODBCDriverPlugin MODULE IMPORTED)

_populate_Sql_plugin_properties(QODBCDriverPlugin RELEASE "sqldrivers/libqsqlodbc.so")

list(APPEND Qt5Sql_PLUGINS Qt5::QODBCDriverPlugin)
