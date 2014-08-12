if(EXISTS ${CMAKE_SOURCE_DIR}/thirdparty/gntp-send/)
    message( STATUS "Adding libgrowl backend" )

    find_package(Threads REQUIRED)
    include_directories(${CMAKE_SOURCE_DIR}/thirdparty/gntp-send/include)

    set( GROWL_SRC growlbackend.cpp
                    ${CMAKE_SOURCE_DIR}/thirdparty/gntp-send/src/tcp.c
                    ${CMAKE_SOURCE_DIR}/thirdparty/gntp-send/src/md5.c
                    ${CMAKE_SOURCE_DIR}/thirdparty/gntp-send/src/growl.c
                    ${CMAKE_SOURCE_DIR}/thirdparty/gntp-send/src/growl.cpp)

    add_library(libsnore_backend_growl MODULE  ${GROWL_SRC} )
    target_link_libraries(libsnore_backend_growl Snore::Libsnore ${QT_QTCORE_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})
    set_target_properties(libsnore_backend_growl PROPERTIES COMPILE_FLAGS "-DGROWL_DLL -DGROWL_CPP_DLL" )

    if( WIN32 )
      target_link_libraries(libsnore_backend_growl ws2_32 )
    endif()

    install(TARGETS libsnore_backend_growl ${SNORE_PLUGIN_INSTALL_PATH})
else()
    message( WARNING "To build the Growl Backend please checkout the submodule" )
endif()

