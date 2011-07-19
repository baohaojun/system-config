if( WITH_GROWL_BACKEND )
	if(CRYPTOPP_LIBRARIES AND Boost_SYSTEM_LIBRARY)
		message( STATUS "Found Boost and Cryptopp, adding libgrowl backend" )
		set( GROWL__SRC
			growl_backend.cpp
			)

		automoc4_add_library(growl_backend MODULE  ${GROWL__SRC} )
		target_link_libraries(growl_backend snorecore ${QT_QTCORE_LIBRARY} ${CRYPTOPP_LIBRARIES} ${Boost_SYSTEM_LIBRARY} wsock32 ws2_32)

		install(TARGETS growl_backend ${PLUGIN_INSTALL_PATH})
	else(CRYPTOPP_LIBRARIES AND Boost_SYSTEM_LIBRARY)
		if(NOT CRYPTOPP_LIBRARIES)
			message(STATUS "Cant build the growl backend because the dependency Cryptopp is missing")
		endif(NOT CRYPTOPP_LIBRARIES)
		if(NOT Boost_SYSTEM_LIBRARY)
			message(STATUS "Cant build the growl backend because the dependency BOOST_SYSTEM is missing")
		endif(NOT Boost_SYSTEM_LIBRARY)
	endif(CRYPTOPP_LIBRARIES AND Boost_SYSTEM_LIBRARY)
endif( WITH_GROWL_BACKEND )
