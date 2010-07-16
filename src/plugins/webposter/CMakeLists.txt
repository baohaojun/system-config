set( WEBPOSTER_SRC
    webposter.cpp
    )
automoc4_add_library(webposter MODULE   ${WEBPOSTER_SRC} )
target_link_libraries(webposter snorecore ${QT_QTNETWORK_LIBRARY}  )

install(TARGETS webposter RUNTIME DESTINATION bin/snoreplugins
                          LIBRARY DESTINATION lib
                          ARCHIVE DESTINATION lib)
