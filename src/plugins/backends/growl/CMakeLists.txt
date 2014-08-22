message( STATUS "Adding libgrowl backend" )
find_package(SnoreGrowl REQUIRED)
find_package(SnoreGrowl++ REQUIRED)

find_package(Threads REQUIRED)

set( GROWL_SRC growlbackend.cpp)

add_library(libsnore_backend_growl MODULE ${GROWL_SRC} )
target_link_libraries(libsnore_backend_growl Snore::Libsnore Snore::SnoreGrowl++ ${QT_QTCORE_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})


install(TARGETS libsnore_backend_growl ${SNORE_PLUGIN_INSTALL_PATH})
