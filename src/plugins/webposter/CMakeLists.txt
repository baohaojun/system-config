set( WEBPOSTER_SRC
    webposter.cpp
    )
automoc4_add_library(webposter MODULE   ${WEBPOSTER_SRC} )
target_link_libraries(webposter snore ${QT_QTNETWORK_LIBRARY}  )
