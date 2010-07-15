
find_library(GROWL_CPP NAMES growl++ )
if( GROWL_CPP )
    message( STATUS "Found libgrowl, adding libgrowl backend" )
    set( GROWL__SRC
        growl_backend.cpp
        )
    automoc4_add_library(growl_backend MODULE  ${GROWL__SRC} )
    target_link_libraries(growl_backend snorecore ${QT_QTCORE_LIBRARY} ${GROWL_CPP} )
else( GROWL_CPP  )
    message( STATUS "libgrowl not found..., get it here http://github.com/mattn/gntp-send" )
endif( GROWL_CPP )
