if(WITH_WEBINTERFACE)
  message(STATUS "Enabling WebInterface support")
  set( WEBINTERFACE_SRC
      webinterface.cpp
      )
  automoc4_add_library(snorewebinterface SHARED  ${WEBINTERFACE_SRC} )
  set_target_properties( snorewebinterface PROPERTIES COMPILE_FLAGS "-DWEBINTERFACE_DLL" )
  target_link_libraries(snorewebinterface snorecore  ${QT_QTNETWORK_LIBRARY} )

  install(TARGETS snorewebinterface RUNTIME DESTINATION bin
                               LIBRARY DESTINATION lib
                               ARCHIVE DESTINATION lib)
endif(WITH_WEBINTERFACE)

