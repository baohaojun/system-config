configure_file("${CMAKE_CURRENT_SOURCE_DIR}/FindLibsnore.cmake.in" "${CMAKE_CURRENT_BINARY_DIR}/FindLibsnore${SNORE_SUFFIX}.cmake" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/FindLibsnore${SNORE_SUFFIX}.cmake" DESTINATION share/apps/cmake/modules)

if(UNIX  AND WITH_SNORE_DEAMON)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/snorenotify.desktop.in" "${CMAKE_CURRENT_BINARY_DIR}/snorenotify${SNORE_SUFFIX}.desktop" @ONLY)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/snorenotify${SNORE_SUFFIX}.desktop" DESTINATION ${CMAKE_INSTALL_PREFIX}/share/applications)
endif(UNIX AND WITH_SNORE_DEAMON)

if(WIN32)
  add_subdirectory(snoretoast)
endif(WIN32)
