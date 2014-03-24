configure_file("${CMAKE_CURRENT_SOURCE_DIR}/FindLibsnore.cmake.in" "${CMAKE_CURRENT_BINARY_DIR}/FindLibsnore${SNORE_CamelCase_SUFFIX}.cmake" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/FindLibsnore${SNORE_CamelCase_SUFFIX}.cmake" DESTINATION share/apps/cmake/modules)

if(UNIX  AND WITH_SNORE_DEAMON)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" @ONLY)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify.desktop" DESTINATION ${XDG_APPS_INSTALL_DIR})
endif(UNIX AND WITH_SNORE_DEAMON)

if(WIN32)
    add_subdirectory(snoretoast)
endif(WIN32)
