set ( SnoreNotify_SRCS	
	notification.cpp
	snoreserver.cpp
	application.cpp
        interface.cpp
        utils.cpp
        )

set ( SnoreNotify_HDR
        notification.h
        snoreserver.h
        application.h
        interface.h
        snore_exports.h
        utils.h
        )

automoc4_add_library( snorecore  SHARED ${SnoreNotify_SRCS})
set_target_properties( snorecore PROPERTIES OUTPUT_NAME "snore" COMPILE_FLAGS "-DSNORECORE_DLL" )
target_link_libraries ( snorecore ${QT_QTCORE_LIBRARY}  ${QT_QTGUI_LIBRARY}  )


install(TARGETS snorecore RUNTIME DESTINATION bin
                          LIBRARY DESTINATION lib
                          ARCHIVE DESTINATION lib)

install(FILES ${SnoreNotify_HDR} DESTINATION include/snore)


