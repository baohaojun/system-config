include ( ${QT_USE_FILE} )	
include_directories (
        ${CMAKE_CURRENT_SOURCE_DIR}  ${CMAKE_CURRENT_BINARY_DIR} 
	${QT_QTCORE_INCLUDE_DIR} ${QT_QTGUI_INCLUDE_DIR}
        ${QT_QTNETWORK_INCLUDE_DIR} ${QT_QTDBUS_INCLUDE_DIR}
	)

add_subdirectory(core)

add_executable ( SnoreNotify main.cpp )
target_link_libraries ( SnoreNotify  snore ${QT_QTGUI_LIBRARY})
add_dependencies(SnoreNotify snore)

add_subdirectory(webinterface)
add_subdirectory(plugins)

