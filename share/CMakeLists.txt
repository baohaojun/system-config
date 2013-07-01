install(FILES FindLibsnore.cmake DESTINATION share/apps/cmake/modules)

if(KDE4_FOUND)
  install(FILES snorenotify.desktop DESTINATION ${XDG_APPS_INSTALL_DIR})
endif(KDE4_FOUND)

if(WIN32)
  add_subdirectory(snoretoast)
endif(WIN32)