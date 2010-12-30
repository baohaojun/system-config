if(WITH_GROWL_BACKEND)

add_definitions("-DGROWL_CPP_STATIC -DGROWL_STATIC")

if( WIN32 )
  link_libraries ( ws2_32 )
  if(MSVC)
    add_definitions("-D_CRT_SECURE_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS -D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES=1 -D_CRT_NONSTDC_NO_WARNINGS")
  endif(MSVC)
endif( WIN32 )

set( GROWL_SEND_SRC
        gntp-send/source/growl.c
        gntp-send/source/tcp.c
        gntp-send/source/md5.c )

add_library( growl STATIC ${GROWL_SEND_SRC} )


add_library( growl_cpp STATIC gntp-send/source/growl++.cpp)
target_link_libraries ( growl_cpp  growl)

endif(WITH_GROWL_BACKEND)
