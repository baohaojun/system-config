
include(CMakeParseArguments)

function(add_snore_plugin name)  
    set(options)
    set(oneValueArgs TYPE)
    set(multiValueArgs SETTINGS_SOURCES SETTINGS_LIBS SOURCES LIBS)
    cmake_parse_arguments(ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    
    add_library(libsnore_${ARG_TYPE}_${name} MODULE ${ARG_SOURCES})
    target_link_libraries(libsnore_${ARG_TYPE}_${name} Snore::Libsnore ${ARG_LIBS})
    install(TARGETS libsnore_${ARG_TYPE}_${name} ${SNORE_PLUGIN_INSTALL_PATH})
    
    if(ARG_SETTINGS_SOURCES)
      add_library(libsnore_settings_${ARG_TYPE}_${name} MODULE ${ARG_SETTINGS_SOURCES} )
      target_link_libraries(libsnore_settings_${ARG_TYPE}_${name} Snore::Libsnore Snore::LibsnoreSettings ${ARG_SETTINGS_LIBS})
      install(TARGETS libsnore_settings_${ARG_TYPE}_${name} ${SNORE_PLUGIN_INSTALL_PATH})
    endif()
    
endfunction() 
