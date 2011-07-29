include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_subdirectory(notification)

set ( SnoreNotify_SRCS ${SnoreNotify_SRCS}
        snoreserver.cpp
        application.cpp
        interface.cpp
        trayiconnotifer.cpp
        )

set ( SnoreNotify_HDR ${SnoreNotify_HDR}
        snoreserver.h
        application.h
        interface.h
        snore_exports.h
        )

automoc4_add_library( snorecore  SHARED ${SnoreNotify_SRCS})
set_target_properties( snorecore PROPERTIES OUTPUT_NAME "snore" DEFINE_SYMBOL "SNORECORE_DLL" )
target_link_libraries ( snorecore ${QT_QTCORE_LIBRARY}  ${QT_QTGUI_LIBRARY}  )


install(TARGETS snorecore RUNTIME DESTINATION bin
                          LIBRARY DESTINATION lib
                          ARCHIVE DESTINATION lib)

install(FILES ${SnoreNotify_HDR} DESTINATION include/snore/core)


