include_directories(${CMAKE_CURRENT_SOURCE_DIR}  ${CMAKE_CURRENT_BINARY_DIR})

include(GetGitRevisionDescription)


get_git_head_revision(GIT_REFSPEC SNORE_REVISION)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/version.cpp.in" "${CMAKE_CURRENT_BINARY_DIR}/version.cpp")

QT4_ADD_RESOURCES(SNORENOTIFY_RCS ${SNORE_RCS})

add_subdirectory(notification)
add_subdirectory(plugins)

set ( SnoreNotify_SRCS ${SnoreNotify_SRCS}
        snore.cpp
        snore_p.cpp
        application.cpp
        hint.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/version.cpp
        ${SNORENOTIFY_RCS}
        )

set ( SnoreNotify_HDR ${SnoreNotify_HDR}
        snore.h
        application.h
        hint.h
        snore_exports.h
        version.h
        )

add_library( snorecore  SHARED ${SnoreNotify_SRCS})
set_target_properties( snorecore PROPERTIES OUTPUT_NAME "snore" DEFINE_SYMBOL "SNORECORE_DLL" )
target_link_libraries ( snorecore ${QT_QTCORE_LIBRARY}  ${QT_QTGUI_LIBRARY}  )


install(TARGETS snorecore RUNTIME DESTINATION bin
                          LIBRARY DESTINATION lib
                          ARCHIVE DESTINATION lib)

install(FILES ${SnoreNotify_HDR} DESTINATION include/snore/core)


