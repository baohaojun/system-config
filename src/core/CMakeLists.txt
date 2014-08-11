include_directories(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
include(GetGitRevisionDescription)

get_git_head_revision(GIT_REFSPEC SNORE_REVISION)

if(NOT SNORE_REVISION)
    set(SNORE_REVISION "")
endif()

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/version.cpp.in" "${CMAKE_CURRENT_BINARY_DIR}/version.cpp")

QT4_ADD_RESOURCES(SNORENOTIFY_RCS ${SNORE_RCS})

add_subdirectory(notification)
add_subdirectory(plugins)

set ( SnoreNotify_SRCS ${SnoreNotify_SRCS}
        snore.cpp
        snore_p.cpp
        application.cpp
        application_p.cpp
        alert.cpp
        alert_p.cpp
        hint.cpp
        log.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/version.cpp
        ${SNORENOTIFY_RCS}
        )

set ( SnoreNotify_HDR ${SnoreNotify_HDR}
        snore.h
        snore_p.h
        application.h
        alert.h
        hint.h
        log.h
        ${CMAKE_CURRENT_BINARY_DIR}/snore_exports.h
        version.h
        )


add_library( libsnore SHARED ${SnoreNotify_SRCS})
set_target_properties( libsnore PROPERTIES OUTPUT_NAME "snore${SNORE_SUFFIX}")
target_link_libraries( libsnore PUBLIC ${QT_QTCORE_LIBRARY} ${QT_QTGUI_LIBRARY} ${QT_QTNETWORK_LIBRARY}  )

generate_export_header(libsnore
  EXPORT_FILE_NAME snore_exports.h
  EXPORT_MACRO_NAME SNORE_EXPORT
)

install(TARGETS libsnore EXPORT Libsnore${SNORE_CamelCase_SUFFIX}Config
                         RUNTIME DESTINATION bin
                         LIBRARY DESTINATION lib
                         ARCHIVE DESTINATION lib
                         INCLUDES DESTINATION include/snore)

install(EXPORT Libsnore${SNORE_CamelCase_SUFFIX}Config DESTINATION lib/cmake/libsnore${SNORE_CamelCase_SUFFIX})

install(FILES ${SnoreNotify_HDR} DESTINATION include/snore/core)


