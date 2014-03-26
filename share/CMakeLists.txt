configure_file("${CMAKE_CURRENT_SOURCE_DIR}/LibsnoreConfig.cmake.in" "${CMAKE_CURRENT_BINARY_DIR}/Libsnore${SNORE_CamelCase_SUFFIX}Config.cmake" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/Libsnore${SNORE_CamelCase_SUFFIX}Config.cmake" DESTINATION lib/cmake/libsnore${SNORE_CamelCase_SUFFIX})

if(UNIX  AND WITH_SNORE_DEAMON)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${XDG_APPS_INSTALL_DIR})
endif(UNIX AND WITH_SNORE_DEAMON)

if(WIN32)
    add_subdirectory(snoretoast)
endif(WIN32)
