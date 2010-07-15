if(WITH_WEBINTERFACE)
  message(STATUS "Enabling WebInterface support")
  set( WEBINTERFACE_SRC
      webinterface.cpp
      )
  automoc4_add_library(webinterface SHARED  ${WEBINTERFACE_SRC} )
  set_target_properties( webinterface PROPERTIES COMPILE_FLAGS "-DWEBINTERFACE_DLL" )
  target_link_libraries(webinterface snorecore  ${QT_QTNETWORK_LIBRARY} )
endif(WITH_WEBINTERFACE)

