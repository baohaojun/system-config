if(WITH_WEBINTERFACE)
  set( REGISTREDAPPS_SRC
      registredapps.cpp
      )

  automoc4_add_library(registredapps MODULE   ${REGISTREDAPPS_SRC} )
  target_link_libraries(registredapps snore webinterface)
endif(WITH_WEBINTERFACE)
