
include(CMakeParseArguments)

function(_test_type TYPE)
    set(PLUGINS "(Backend|SecondaryBackend|Frontend)" )
    string(REGEX MATCH ${PLUGINS} RESULT ${TYPE})
    if(NOT RESULT)
        message(FATAL_ERROR "Plugin type ${TYPE} is not ins ${PLUGINS}")
    endif()
endfunction()

function(_test_name UPPER LOWER)
    string(COMPARE EQUAL "${UPPER}" "${LOWER}" RESULT)
    if(RESULT)
        message(FATAL_ERROR "Plugin name ${UPPER} is not camel case")
    endif()
endfunction()

function(add_snore_plugin SNORE_NAME)
    set(options)
    set(oneValueArgs TYPE)
    set(multiValueArgs SETTINGS_SOURCES SETTINGS_LIBS SOURCES LIBS)
    cmake_parse_arguments(SNORE "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    string( TOLOWER "${SNORE_NAME}" SNORE_NAME_LOWERCASE)
    _test_name(${SNORE_NAME} ${SNORE_NAME_LOWERCASE})
    string(REGEX REPLACE "[ \t\r\n]" "" SNORE_NAME_LOWERCASE ${SNORE_NAME_LOWERCASE})

    _test_type(${SNORE_TYPE})
    string( TOLOWER "${SNORE_TYPE}" SNORE_TYPE_LOWERCASE)

    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/snore_plugin.json "{ \"name\" : \"${SNORE_NAME}\" }")
    add_library(libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} MODULE ${SNORE_SOURCES})
    target_link_libraries(libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} Snore::Libsnore ${SNORE_LIBS})
    install(TARGETS libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} ${SNORE_PLUGIN_INSTALL_PATH})
    
    if(SNORE_SETTINGS_SOURCES AND Qt5Widgets_FOUND)
        file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/snore_settings_plugin.json "{ \"name\" : \"${SNORE_NAME}${SNORE_TYPE}\" }")
        add_library(libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} MODULE ${SNORE_SETTINGS_SOURCES} )
        target_link_libraries(libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} Snore::Libsnore Snore::LibsnoreSettings ${SNORE_SETTINGS_LIBS})
        install(TARGETS libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} ${SNORE_PLUGIN_INSTALL_PATH})
    endif()
    
endfunction() 
