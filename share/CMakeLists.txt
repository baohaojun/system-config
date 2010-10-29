set(SNORE_LOCATION ${CMAKE_INSTALL_PREFIX}/bin/snorenotify)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/org.freedesktop.Notifications.service.cmake ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/org.freedesktop.Notifications.service
			  DESTINATION share/dbus-1/services/)
		

install(FILES share/FindLibsnore.cmake DESTINATION share/apps/cmake/modules)