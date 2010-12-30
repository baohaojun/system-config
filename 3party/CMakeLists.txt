if(WITH_GROWL_BACKEND)

if( WIN32 )
  link_libraries ( ws2_32 )
endif( WIN32 )
set( GROWL_SEND_SRC
        gntp-send/source/growl.c
        gntp-send/source/tcp.c
        gntp-send/source/md5.c )

add_library( growl STATIC ${GROWL_SEND_SRC} )
set_target_properties( growl PROPERTIES DEFINE_SYMBOL "GROWL_STATIC" )


add_library( growl_cpp STATIC gntp-send/source/growl++.cpp)
set_target_properties( growl_cpp PROPERTIES DEFINE_SYMBOL "DGROWL_CPP_STATIC" )
target_link_libraries ( growl_cpp  growl)

endif(WITH_GROWL_BACKEND)
