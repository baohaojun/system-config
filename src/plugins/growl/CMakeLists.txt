
find_library(GROWL_CPP NAMES growl++ )
set( GROWL__SRC
    growl_backend.cpp
    )
automoc4_add_library(growl_backend MODULE  ${GROWL__SRC} )
target_link_libraries(growl_backend snore ${QT_QTCORE_LIBRARY} ${GROWL_CPP} )

