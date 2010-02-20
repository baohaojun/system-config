set ( SnoreNotify_SRCS	
	notification.cpp
	snoreserver.cpp
	application.cpp
        interface.cpp
	)	

automoc4_add_library( snore  SHARED ${SnoreNotify_SRCS})
set_target_properties( snore PROPERTIES COMPILE_FLAGS "-DSNORECORE_DLL" )
target_link_libraries ( snore ${QT_QTCORE_LIBRARY}  )


