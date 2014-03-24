include_directories(${CMAKE_CURRENT_SOURCE_DIR}  ${CMAKE_CURRENT_BINARY_DIR})
add_subdirectory(core)

if(WITH_SNORE_DEAMON)

    add_executable( snorenotify WIN32 main.cpp snorenotify.cpp trayicon.cpp ${SNORENOTIFY_DEAMON_DEPS})
    target_link_libraries( snorenotify snorecore ${QT_QTGUI_LIBRARY} )

    if(KDE4_FOUND)
        target_link_libraries( snorenotify ${KDE4_KDEUI_LIBS} )
    endif(KDE4_FOUND)

    if(MSVC)
            set_target_properties(snorenotify PROPERTIES   LINK_FLAGS "/ENTRY:\"mainCRTStartup\"")
    endif(MSVC)

    add_dependencies(snorenotify snorecore)

    install(TARGETS snorenotify RUNTIME DESTINATION bin
                              LIBRARY DESTINATION lib
                              ARCHIVE DESTINATION lib)
endif()

add_subdirectory(plugins)
