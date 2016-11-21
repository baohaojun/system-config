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


function(generate_snore_plugin_header)
    set(SNORE_PLUGIN_LOADING "")
    foreach(PLUGIN ${SNORE_PLUGIN_LIST})
        set(SNORE_PLUGIN_LOADING "${SNORE_PLUGIN_LOADING}Q_IMPORT_PLUGIN(${PLUGIN})\n")
    endforeach()
    set(SNORE_RESOURCE_LOADING "Q_INIT_RESOURCE(snore);")
    foreach(RESOURCE ${SNORE_PLUGIN_RESOURCES})
        set(SNORE_RESOURCE_LOADING "${SNORE_RESOURCE_LOADING}\n             Q_INIT_RESOURCE(${RESOURCE});")
    endforeach()
    configure_file("${PROJECT_SOURCE_DIR}/src/libsnore/snore_static_plugins.h.in" "${PROJECT_BINARY_DIR}/src/libsnore/snore_static_plugins.h")
endfunction()

function(add_snore_plugin SNORE_NAME)
    set(options)
    set(oneValueArgs TYPE)
    set(multiValueArgs SETTINGS_SOURCES SETTINGS_LIBS SOURCES LIBS RESOURCES)
    cmake_parse_arguments(SNORE "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    string(REGEX REPLACE "[ \t\r\n]" "" SNORE_NAME_NO_SPACE ${SNORE_NAME})
    string( TOLOWER "${SNORE_NAME_NO_SPACE}" SNORE_NAME_LOWERCASE)
    _test_name(${SNORE_NAME} ${SNORE_NAME_LOWERCASE})

    _test_type(${SNORE_TYPE})
    string( TOLOWER "${SNORE_TYPE}" SNORE_TYPE_LOWERCASE)

    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/snore_plugin.json "{ \"name\" : \"${SNORE_NAME}\" }")

    if(NOT SNORE_STATIC)
        add_library(libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} MODULE ${SNORE_SOURCES})
        install(TARGETS libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} ${SNORE_PLUGIN_INSTALL_PATH})
    else()
        list(APPEND SNORE_PLUGIN_LIST "${SNORE_NAME_NO_SPACE}")
        add_library(libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} STATIC ${SNORE_SOURCES})
        #todo install and export the plugins
        #install(TARGETS libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})
        set_property( TARGET libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE}
                      APPEND
                      PROPERTY COMPILE_DEFINITIONS QT_STATICPLUGIN)
    endif()

    target_link_libraries(libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} Snore::Libsnore ${SNORE_LIBS})

    if(SNORE_SETTINGS_SOURCES AND Qt5Widgets_FOUND)
        file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/snore_settings_plugin.json "{ \"name\" : \"${SNORE_NAME}${SNORE_TYPE}\" }")

        if(NOT SNORE_STATIC)
            add_library(libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} MODULE ${SNORE_SETTINGS_SOURCES} )
            install(TARGETS libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} ${SNORE_PLUGIN_INSTALL_PATH})
        else()
             add_library(libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} STATIC ${SNORE_SETTINGS_SOURCES} )
             set_property( TARGET libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE}
                           APPEND
                           PROPERTY COMPILE_DEFINITIONS QT_STATICPLUGIN)
             list(APPEND SNORE_PLUGINS libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} )
             list(APPEND SNORE_PLUGIN_LIST "${SNORE_NAME_NO_SPACE}SettingsPlugin")
        endif()
        target_link_libraries(libsnore_settings_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE} Snore::Libsnore Snore::LibsnoreSettings ${SNORE_SETTINGS_LIBS})
    endif()

    if(SNORE_STATIC)
        list(REMOVE_DUPLICATES SNORE_PLUGIN_LIST)
        set(SNORE_PLUGIN_LIST ${SNORE_PLUGIN_LIST} CACHE INTERNAL "A list of all plugins names." FORCE)

        list(APPEND SNORE_PLUGIN_RESOURCES "${SNORE_RESOURCES}")
        list(APPEND SNORE_PLUGINS libsnore_${SNORE_TYPE_LOWERCASE}_${SNORE_NAME_LOWERCASE})

        list(REMOVE_DUPLICATES SNORE_PLUGINS)
        set(SNORE_PLUGINS ${SNORE_PLUGINS} CACHE INTERNAL "A list of all plugins." FORCE)
        list(REMOVE_DUPLICATES SNORE_PLUGIN_RESOURCES)
        set(SNORE_PLUGIN_RESOURCES ${SNORE_PLUGIN_RESOURCES} CACHE INTERNAL "A list of all plugins resources." FORCE)
    endif()
    
endfunction() 
