find_package(Qt5LinguistTools QUIET)
set_package_properties(Qt5LinguistTools PROPERTIES
                                PURPOSE "Support for translations."
                                TYPE OPTIONAL)

                                
if(Qt5LinguistTools_FOUND)
    set(LIBSNORE_LANGS de en)

    set(TS_FILES )
    set( snore_i18n_content "" )
    foreach( lang ${LIBSNORE_LANGS} )
        list(APPEND TS_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${lang}.ts)
        set( snore_i18n_content "${snore_i18n_content}\n\t\t<file>${lang}.qm</file>" )
    endforeach()

    QT5_CREATE_TRANSLATION(libsnore_lang ${TS_FILES} ${PROJECT_SOURCE_DIR}/src)


    configure_file(snore_i18n.qrc.in snore_i18n.qrc @ONLY)
    add_custom_target(libsnore_lang_translation ALL DEPENDS ${libsnore_lang})


    set(SNORE_RCS ${SNORE_RCS} ${CMAKE_BINARY_DIR}/lang/snore_i18n.qrc PARENT_SCOPE)
endif()
