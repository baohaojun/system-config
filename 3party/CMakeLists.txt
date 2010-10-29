if(WITH_GROWL_BACKEND)

if( WIN32 )
  link_libraries ( ws2_32 )
endif( WIN32 )

add_definitions("-DGROWL_CPP_STATIC -DGROWL_STATIC")

set( GROWL_SEND_SRC
        gntp-send/source/growl.c
        gntp-send/source/tcp.c
        gntp-send/source/md5.c )

add_library( growl  STATIC ${GROWL_SEND_SRC} )


add_library( growl_cpp STATIC gntp-send/source/growl++.cpp)
target_link_libraries ( growl_cpp  growl)

endif(WITH_GROWL_BACKEND)




