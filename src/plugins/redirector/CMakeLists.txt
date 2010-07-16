if(WITH_WEBINTERFACE)
  set( REDIRECTOR_SRC
      redirector.cpp
      )
  automoc4_add_library(redirector MODULE  ${REDIRECTOR_SRC})
  target_link_libraries(redirector  snorecore snorewebinterface)
  install(TARGETS redirector RUNTIME DESTINATION bin/snoreplugins
                             LIBRARY DESTINATION lib
                             ARCHIVE DESTINATION lib)
endif(WITH_WEBINTERFACE)
