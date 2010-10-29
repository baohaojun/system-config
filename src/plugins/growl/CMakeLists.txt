

if( WITH_GROWL_BACKEND )
    message( STATUS "Found libgrowl, adding libgrowl backend" )
    set( GROWL__SRC
        growl_backend.cpp
        )
		
    automoc4_add_library(growl_backend MODULE  ${GROWL__SRC} )
    target_link_libraries(growl_backend snorecore ${QT_QTCORE_LIBRARY} ${GROWL_CPP} growl_cpp  )
	set_target_properties( growl_backend PROPERTIES COMPILE_FLAGS "-DGROWL_CPP_STATIC -DGROWL_STATIC"  )

    install(TARGETS growl_backend ${PLUGIN_INSTALL_PATH})

endif( WITH_GROWL_BACKEND )
