set(LIBSNORE_LANGS de en)

set(TS_FILES )
set( snore_i18n_content "<!DOCTYPE RCC><RCC version=\"1.0\">\n<qresource prefix=\"/lang/libsnore\">\n" )
foreach( lang ${LIBSNORE_LANGS} )
    list(APPEND TS_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${lang}.ts)
    set( snore_i18n_content "${snore_i18n_content}<file>${lang}.qm</file>\n" )
endforeach()
set( snore_i18n_content "${snore_i18n_content}</qresource>\n</RCC>\n" )

file( WRITE ${CMAKE_BINARY_DIR}/lang/snore_i18n.qrc "${snore_i18n_content}" )

qt5_add_translation(libsnore_lang ${TS_FILES})

add_custom_target(libsnore_lang_translation ALL DEPENDS ${libsnore_lang})
