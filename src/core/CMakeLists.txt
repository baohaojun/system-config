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
set_target_properties( libsnore PROPERTIES
                                OUTPUT_NAME "snore${SNORE_SUFFIX}"
                                VERSION "${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}.${SNORE_VERSION_PATCH}.${SNORE_VERSION_SUFFIX}"
                                SOVERSION "${SNORE_VERSION_MAJOR}.${SNORE_VERSION_MINOR}"
                                EXPORT_NAME Libsnore)
target_link_libraries( libsnore PUBLIC ${QT_QTCORE_LIBRARY} ${QT_QTGUI_LIBRARY} ${QT_QTNETWORK_LIBRARY}  )

add_library(Snore::Libsnore ALIAS libsnore)



generate_export_header(libsnore
  EXPORT_FILE_NAME snore_exports.h
  EXPORT_MACRO_NAME SNORE_EXPORT
)

#TODO: drop /core/
target_include_directories(libsnore PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
$<INSTALL_INTERFACE:include>
$<INSTALL_INTERFACE:include/snore/core>)

install(TARGETS libsnore EXPORT LibsnoreTarget
                         RUNTIME DESTINATION bin
                         LIBRARY DESTINATION lib
                         ARCHIVE DESTINATION lib)

install(EXPORT LibsnoreTarget DESTINATION lib/cmake/libsnore${SNORE_CamelCase_SUFFIX} NAMESPACE Snore::)

install(FILES ${SnoreNotify_HDR} DESTINATION include/snore/core)

install(FILES LibsnoreConfig.cmake DESTINATION lib/cmake/libsnore${SNORE_CamelCase_SUFFIX} RENAME Libsnore${SNORE_CamelCase_SUFFIX}Config.cmake)

