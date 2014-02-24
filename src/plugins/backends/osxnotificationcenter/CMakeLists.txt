if(APPLE)
	message(STATUS "adding osx notification center backend")
	#find_library(FOUNDATION Foundation)
	set(OSXNOTIFICATIONCENTER_SRC
		osxnotificationcenter.mm
		#NotificationCenterDelegate.m
	)
	add_library(libsnore_backend_osxnotificationcenter MODULE ${OSXNOTIFICATIONCENTER_SRC})
	#target_link_libraries(libsnore_backend_osxnotificationcenter snorecore ${FOUNDATION})
	target_link_libraries(libsnore_backend_osxnotificationcenter snorecore /System/Library/Frameworks/Foundation.framework)
	install(TARGETS libsnore_backend_osxnotificationcenter ${SNORE_PLUGIN_INSTALL_PATH})
endif(APPLE)
