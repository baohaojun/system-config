find_package(SnoreGrowl++)
set_package_properties(SnoreGrowl++ PROPERTIES
                                    DESCRIPTION "SnoreGrowl is a library providing Growl network notifications."
                                    URL "https://github.com/Snorenotify/SnoreGrowl"
                                    PURPOSE "Enable libgrowl backend"
                                    TYPE RECOMMENDED)

if(SnoreGrowl++_FOUND)
    set( GROWL_SRC growlbackend.cpp)

    add_library(libsnore_backend_growl MODULE ${GROWL_SRC} )
    target_link_libraries(libsnore_backend_growl Snore::Libsnore Snore::SnoreGrowl++ ${QT_QTCORE_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})


    install(TARGETS libsnore_backend_growl ${SNORE_PLUGIN_INSTALL_PATH})
endif()
