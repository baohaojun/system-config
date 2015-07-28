find_package(Qt5LinguistTools QUIET)
set_package_properties(Qt5LinguistTools PROPERTIES
                                PURPOSE "Support for translations."
                                TYPE OPTIONAL)

                                
if(Qt5LinguistTools_FOUND)
    set(LIBSNORE_LANGS de en)

    set(TS_FILES )
    set( snore_i18n_content "<!DOCTYPE RCC><RCC version=\"1.0\">\n<qresource prefix=\"/lang/libsnore\">\n" )
    foreach( lang ${LIBSNORE_LANGS} )
        list(APPEND TS_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${lang}.ts)
        set( snore_i18n_content "${snore_i18n_content}<file>${lang}.qm</file>\n" )
    endforeach()
    set( snore_i18n_content "${snore_i18n_content}</qresource>\n</RCC>\n" )

    file( WRITE ${CMAKE_BINARY_DIR}/lang/snore_i18n.qrc.in "${snore_i18n_content}" )
    
    qt5_add_translation(libsnore_lang ${TS_FILES})

    add_custom_command(OUTPUT snore_i18n.qrc
                        COMMAND ${CMAKE_COMMAND}
                        ARGS -E copy ${CMAKE_BINARY_DIR}/lang/snore_i18n.qrc.in ${CMAKE_BINARY_DIR}/lang/snore_i18n.qrc
                        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")


    add_custom_target(libsnore_lang_translation ALL DEPENDS ${libsnore_lang} DEPENDS snore_i18n.qrc)


    set(SNORE_RCS ${SNORE_RCS} ${CMAKE_BINARY_DIR}/lang/snore_i18n.qrc PARENT_SCOPE)
endif()
