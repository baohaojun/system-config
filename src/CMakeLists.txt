include_directories(${CMAKE_CURRENT_SOURCE_DIR}  ${CMAKE_CURRENT_BINARY_DIR})
add_subdirectory(core)

QT4_ADD_RESOURCES(SNORENOTIFY_RCS ${SNORE_RCS})
set(SNORENOTIFY_DEPS ${SNORENOTIFY_DEPS} ${SNORENOTIFY_RCS})

if(WIN32)
  set(WIN32_SPECIFIC WIN32)
else(WIN32)
  set(WIN32_SPECIFIC)
endif(WIN32)

add_executable( snorenotify ${WIN32_SPECIFIC}  main.cpp snorenotify.cpp trayicon.cpp ${SNORENOTIFY_DEPS})

target_link_libraries( snorenotify snorecore ${QT_QTGUI_LIBRARY} )

if(MSVC)
	set_target_properties(snorenotify PROPERTIES   LINK_FLAGS "/ENTRY:\"mainCRTStartup\"")
endif(MSVC)

add_dependencies(snorenotify snorecore)

install(TARGETS snorenotify RUNTIME DESTINATION bin
                          LIBRARY DESTINATION lib
                          ARCHIVE DESTINATION lib)

add_subdirectory(plugins)
