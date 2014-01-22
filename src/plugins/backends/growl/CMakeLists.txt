if( WITH_GROWL_BACKEND )
        if(CRYPTOPP_LIBRARIES AND Boost_SYSTEM_LIBRARY)
                message( STATUS "Found Boost and Cryptopp, adding libgrowl backend" )
                if(CMAKE_COMPILER_IS_GNUCXX)
                     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fexceptions")
                endif(CMAKE_COMPILER_IS_GNUCXX)
                set( GROWL__SRC
                        growl.cpp
                        )

                add_library(libsnore_backend_growl MODULE  ${GROWL__SRC} )
                target_link_libraries(libsnore_backend_growl snorecore ${QT_QTCORE_LIBRARY} ${CRYPTOPP_LIBRARIES} ${Boost_SYSTEM_LIBRARY} ${Boost_THREAD_LIBRARY})

                if(MINGW)
                    #fiexes a multiple defenition error with static boost
                    SET_TARGET_PROPERTIES(libsnore_backend_growl PROPERTIES LINK_FLAGS -Wl,--allow-multiple-definition COMPILE_FLAGS
                    "-Wno-undef -Wno-unused-parameter -Wno-unknown-pragmas -Wno-unused-local-typedefs -Wno-missing-field-initializers -Wno-strict-aliasing -Wno-reorder -Wno-unused-variable -Wno-unused-function" )
                endif(MINGW)

                if(WIN32)
                    target_link_libraries(libsnore_backend_growl wsock32 ws2_32)
                endif(WIN32)

                if(UNIX)
                    target_link_libraries(libsnore_backend_growl pthread)
                endif(UNIX)

                install(TARGETS libsnore_backend_growl ${SNORE_PLUGIN_INSTALL_PATH})
        else(CRYPTOPP_LIBRARIES AND Boost_SYSTEM_LIBRARY)
                if(NOT CRYPTOPP_LIBRARIES)
                        message(STATUS "Cant build the growl backend because the dependency Cryptopp is missing")
                endif(NOT CRYPTOPP_LIBRARIES)
                if(NOT Boost_SYSTEM_LIBRARY)
                        message(STATUS "Cant build the growl backend because the dependency BOOST_SYSTEM is missing")
                endif(NOT Boost_SYSTEM_LIBRARY)
        endif(CRYPTOPP_LIBRARIES AND Boost_SYSTEM_LIBRARY)
endif( WITH_GROWL_BACKEND )
