# -*- mode: cmake ; coding: utf-8-unix -*-

cmake_minimum_required(VERSION 2.8)

cmake_policy(SET CMP0015 NEW)
cmake_policy(SET CMP0022 NEW)

include(CheckCXXCompilerFlag)


# ----------------------------------------
# functions
# ----------------------------------------

function(display_vars vars prefix)
  message("${prefix}")
  foreach(IT ${vars})
    message("${prefix}${IT} = ${${IT}}")
  endforeach()
endfunction()


function(get_symlink_follow_list _output_follow_list _absolute_paths)
  foreach(IT ${_absolute_paths})
    list(APPEND resolved_list ${IT})
    set(resolved_apath ${IT})

    while(IS_SYMLINK ${resolved_apath})
      # Don't use get_filename_component function.
      # Because, if you use the "REALPATH",
      # Can be obtained with only "begin-symlink" and "end-symlink", "middle-symlink" is excluded.
      # get_filename_component(resolved_apath ${resolved_apath} REALPATH)

      execute_process(COMMAND readlink -n ${resolved_apath} RESULT_VARIABLE cmd_result OUTPUT_VARIABLE cmd_output ERROR_VARIABLE cmd_error)

      if((NOT ${cmd_result}) AND (NOT (IS_ABSOLUTE ${cmd_output})))
        # generate absolute path
        get_filename_component(parent_directory ${resolved_apath} DIRECTORY)
        set(resolved_apath ${parent_directory}/${cmd_output})
      endif()

      # display_vars("cmd_result;cmd_output;cmd_error;parent_directory;resolved_apath" "  -- ")

      if(EXISTS ${resolved_apath})
        list(APPEND resolved_list ${resolved_apath})
      else()
        message("get_symlink_follow_list : resolved_apath not exist. --> ${resolved_apath}")
        break()
      endif()
    endwhile()
  endforeach()
  list(REMOVE_DUPLICATES resolved_list)

  set(${_output_follow_list} ${resolved_list} PARENT_SCOPE)

  # display_vars("resolved_list;" "  - ")
endfunction()


# ----------------------------------------
# before common setting
# ----------------------------------------

set(project_name clang-server)
set(project_version "2.1.1")

# set(platform_name x86_64)
# set(platform_name x86_32)
# set(project_elf_name ${project_name}-${platform_name})
set(project_elf_name ${project_name})
if(UNIX)
  set(project_elf_name ${project_elf_name}-${project_version})
endif()


# CMAKE_INSTALL_PREFIX check designated value before function Project()
if(CMAKE_INSTALL_PREFIX)
  # message("user_install_prefix TRUE")
  set(user_install_prefix TRUE)
else()
  # message("user_install_prefix FALSE")
endif()


# The set before project declaration.
set(CMAKE_CONFIGURATION_TYPES Release RelWithDebInfo Debug CACHE TYPE INTERNAL FORCE)
# setup library per configuration
string(TOUPPER "${CMAKE_CONFIGURATION_TYPES}" project_configurations)
# set(CMAKE_PREFIX_PATH "/usr/local/bin;")


Project(${project_name})

enable_language(CXX)


# The set after project declaration.
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

string(TOUPPER ${CMAKE_BUILD_TYPE} project_build_type)



# CMAKE_INSTALL_PREFIX 
# Windows (exe,dll)
# Default: C:/Program Files/clang-server/
# prefix : <user designate directory>
# 
# Linux (elf)
# Default: /usr/local/bin/
# prefix : <user designate directory>
# Linux (so)
# Default: /usr/local/lib/clang-server/
# prefix : <user designate directory>

set(CMAKE_SKIP_RPATH TRUE)
# set(CMAKE_SKIP_BUILD_RPATH FALSE)
# set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
# set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)


set(project_sources 
                    main.cpp
                    ClangServer.cpp
                    ClangServer.hpp
                    ClangSession.cpp
                    ClangSession.hpp
                    Command.cpp
                    Command.hpp
                    Common.cpp
                    Common.hpp
                    DataObject.hpp
                    Profiler.hpp
                    parser/CommandLine.hpp
                    parser/Lisp.hpp
                    parser/json.hpp
                    clang-c/BuildSystem.h
                    clang-c/CXCompilationDatabase.h
                    clang-c/CXErrorCode.h
                    clang-c/CXString.h
                    clang-c/Documentation.h
                    clang-c/Index.h
                    clang-c/Platform.h)


add_library(libclang SHARED IMPORTED)

set(dependency_libraries libclang)


list(APPEND CMAKE_FIND_LIBRARY_PREFIXES lib)
list(REMOVE_DUPLICATES CMAKE_FIND_LIBRARY_PREFIXES)

# set(libclang_names clang-${platform_name} clang)
set(libclang_names clang)



set(echo_vars)
list(APPEND echo_vars CMAKE_C_COMPILER_ID CMAKE_C_STANDARD CMAKE_CXX_COMPILER_ID CMAKE_CXX_STANDARD CYGWIN WIN32 UNIX MSVC MSVC_VERSION CMAKE_GNUtoMS CMAKE_SYSTEM_NAME CMAKE_COMPILER_IS_GNUC CMAKE_COMPILER_IS_GNUCXX CMAKE_COMPILER_IS_MINGW CMAKE_COMPILER_IS_CYGWIN
 CMAKE_GNU_COMPILER_ID CMAKE_CLANG_COMPILER_ID CMAKE_COMPILER_IS_CLANG)
display_vars("${echo_vars}" "env: ")


# ----------------------------------------
# platform dependency setting
# ----------------------------------------

if(MSVC)
  # ----------------------------------------
  # Microsoft Visual Studio
  # ----------------------------------------
  message("environment: MSVC")
  
  set(echo_vars)
  list(APPEND echo_vars CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE)
  list(APPEND echo_vars CMAKE_EXE_LINKER_FLAGS CMAKE_EXE_LINKER_FLAGS_DEBUG CMAKE_EXE_LINKER_FLAGS_RELEASE)
  list(APPEND echo_vars IMPORTED_IMPLIB IMPORTED_IMPLIB_DEBUG IMPORTED_IMPLIB_RELEASE)
  display_vars("${echo_vars}" "msvc: ")

  # vc project filter
  source_group(TREE "${CMAKE_SOURCE_DIR}/../" FILES ${project_sources})
  
  # set_target_properties(TARGET)

  # setup compile & link flags
  set(project_defs -D_CONSOLE -DUNICODE -D_UNICODE -D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_WARNINGS)

  set(CMAKE_CXX_FLAGS "/GR /EHsc /W3 /Zi")
  set(CMAKE_CXX_FLAGS_DEBUG "/Gm /Od")
  set(CMAKE_CXX_FLAGS_RELEASE "/GL /Gy /Oi")
  set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
  set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /LTCG /DEBUG /OPT:ICF /OPT:REF")

  set(dependency_libraries odbc32 odbccp32 ${dependency_libraries})

  
  # search & setup link library properties
  list(INSERT CMAKE_FIND_LIBRARY_SUFFIXES 0 .imp)
  list(REMOVE_DUPLICATES CMAKE_FIND_LIBRARY_SUFFIXES)


  # search priority EXTERNAL > CURRENT
  set(libclang_search_paths ${LIBRARY_PATHS} ${CMAKE_SOURCE_DIR})

  foreach(IT_SEARCH_PATH ${libclang_search_paths})
    message("")
    message("IT_SEARCH_PATH : ${IT_SEARCH_PATH}")

    # collect absolute file paths
    file(GLOB_RECURSE collected_library_apath ${IT_SEARCH_PATH} ${IT_SEARCH_PATH}/*.imp ${IT_SEARCH_PATH}/*.lib ${IT_SEARCH_PATH}/*.dll)

    if(collected_library_apath)
      # generate absolute directory paths
      set(library_apaths ${IT_SEARCH_PATH})
      foreach(IT_PATH ${collected_library_apath})
        get_filename_component(library_dir_path ${IT_PATH} DIRECTORY)
        list(APPEND library_apaths ${library_dir_path})
      endforeach()
      list(REMOVE_DUPLICATES library_apaths)

      # message("collected_library_apath : ${collected_library_apath}")
      message("library_apaths : ${library_apaths}")

      # find library > generate relative path > set property per configuration.
      foreach(IT_CONFIG ${project_configurations})
        if(NOT matched_${IT_CONFIG})
          # collect a absolute path that includes the same configuration name.
          # IT_SEARCH_PATH is always accept.
          # set(filtered_library_apaths ${IT_SEARCH_PATH})
          # foreach(IT_PATH ${library_apaths})
          #   string(TOUPPER ${IT_PATH} CHECK_PATH_STR)
          #   if(${CHECK_PATH_STR} MATCHES /${IT_CONFIG})
          #     list(APPEND filtered_library_apaths ${IT_PATH})
          #   endif()
          # endforeach()
          # set(filtered_library_apaths ${library_apaths})

          set(CMAKE_FIND_LIBRARY_SUFFIXES .imp .lib)
          find_library(libclang_${IT_CONFIG}_found_lib_apath NAMES ${libclang_names} PATHS ${library_apaths} NO_DEFAULT_PATH)

          set(CMAKE_FIND_LIBRARY_SUFFIXES .dll)
          find_library(libclang_${IT_CONFIG}_found_dll_apath NAMES ${libclang_names} PATHS ${library_apaths} NO_DEFAULT_PATH)

          display_vars("library_apaths;libclang_${IT_CONFIG}_found_lib_apath;libclang_${IT_CONFIG}_found_dll_apath" "${IT_CONFIG}: ")

          if(libclang_${IT_CONFIG}_found_lib_apath AND libclang_${IT_CONFIG}_found_dll_apath)
            file(RELATIVE_PATH libclang_${IT_CONFIG}_rpath ${IT_SEARCH_PATH} ${libclang_${IT_CONFIG}_found_lib_apath})

            set_property(TARGET libclang PROPERTY IMPORTED_IMPLIB_${IT_CONFIG} ${libclang_${IT_CONFIG}_rpath})
            set_property(TARGET libclang PROPERTY INTERFACE_LINK_LIBRARIES_${IT_CONFIG} ${libclang_${IT_CONFIG}_rpath})
            set_property(TARGET libclang PROPERTY IMPORTED_LOCATION_${IT_CONFIG} ${libclang_${IT_CONFIG}_found_lib_apath})
            link_directories(${IT_SEARCH_PATH})

            set(libclang_${IT_CONFIG}_found_apath ${libclang_${IT_CONFIG}_found_dll_apath})
            message("${IT_CONFIG}: found library at ${IT_SEARCH_PATH}")
            set(matched_${IT_CONFIG} TRUE)
          endif()
        endif()
      endforeach()
    endif()
  endforeach()


  # install info
  set(binary_destination_apath ${CMAKE_INSTALL_PREFIX})
  set(library_destination_apath ${CMAKE_INSTALL_PREFIX})

  
  foreach(IT_CONFIG ${project_configurations})
    if(NOT matched_${IT_CONFIG})
      message("not found ${IT_CONFIG} library")
    endif()
  endforeach()

elseif(UNIX)
  # ----------------------------------------
  # UNIX, Linux
  # ----------------------------------------
  message("environment: UNIX")

  # setup compile & link flags
  check_cxx_compiler_flag(-std=c++17 COMPILER_SUPPORTS_CXX17)
  check_cxx_compiler_flag(-std=c++1z COMPILER_SUPPORTS_CXX1Z)
  check_cxx_compiler_flag(-std=c++14 COMPILER_SUPPORTS_CXX14)
  check_cxx_compiler_flag(-std=c++1y COMPILER_SUPPORTS_CXX1Y)
  check_cxx_compiler_flag(-std=c++11 COMPILER_SUPPORTS_CXX11)
  check_cxx_compiler_flag(-std=c++0x COMPILER_SUPPORTS_CXX0X)

  # setup compile & link flags
  if(COMPILER_SUPPORTS_CXX17)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
  elseif(COMPILER_SUPPORTS_CXX1Z)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1z")
  elseif(COMPILER_SUPPORTS_CXX14)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
  elseif(COMPILER_SUPPORTS_CXX1Y)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
  elseif(COMPILER_SUPPORTS_CXX11)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
  elseif(COMPILER_SUPPORTS_CXX0X)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
  else()
    message(FATAL_ERROR "The compiler has no C++11 support.")
  endif()

  # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wunused-result")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unused-result")


  # search priority EXTERNAL > CURRENT

  if(CYGWIN)
    message("CYGWIN")
    # set(dependency_libraries clang.dll)
    # set(dependency_libraries libclang.a)

    # find_library(CYG_LIBCLANG_PATH NAMES clang.dll PATHS ${library_apaths})
    # find_library(CYG_LIBCLANG_PATH NAMES libclang.dll clang.dll clang PATHS "/lib" "c:/cygwin-x86_64/lib" )
    # find_library(CYG_LIBCLANG_PATH NAMES libclang.dll PATHS "/lib" "c:/cygwin-x86_64/lib" )
    # find_library(CYG_LIBCLANG_PATH NAMES libclang libclang.dll PATHS "c:/cygwin-x86_64/lib" )
    message("CYG_LIBCLANG_PATH : ${CYG_LIBCLANG_PATH}")

  else()
    message("non CYGWIN")
  endif()


  # search priority EXTERNAL > CURRENT
  set(libclang_search_paths ${LIBRARY_PATHS} ${CMAKE_SOURCE_DIR})

  foreach(IT_SEARCH_PATH ${libclang_search_paths})
    message("IT_SEARCH_PATH : ${IT_SEARCH_PATH}")

    # collect absolute file paths
    file(GLOB_RECURSE collected_library_apath ${IT_SEARCH_PATH} ${IT_SEARCH_PATH}/*.so ${IT_SEARCH_PATH}/*.a)

    if(collected_library_apath)
      # generate absolute directory paths
      set(library_apaths ${IT_SEARCH_PATH})
      foreach(IT_PATH ${collected_library_apath})
        get_filename_component(library_dir_path ${IT_PATH} DIRECTORY)
        list(APPEND library_apaths ${library_dir_path})
      endforeach()
      list(REMOVE_DUPLICATES library_apaths)

      # message("collected_library_apath : ${collected_library_apath}")
      message("library_apaths : ${library_apaths}")

      # find library > generate relative path > set property per configuration.
      foreach(IT_CONFIG ${project_configurations})
        if(NOT matched_${IT_CONFIG})
          # find library > generate relative path
          find_library(libclang_${IT_CONFIG}_found_so_apath NAMES ${libclang_names} PATHS ${library_apaths} NO_DEFAULT_PATH)

          display_vars("libclang_${IT_CONFIG}_found_so_apath" "${IT_CONFIG}: ")

          if(libclang_${IT_CONFIG}_found_so_apath)
            # message("found library at ${libclang_${IT_CONFIG}_found_so_apath}")
            get_filename_component(libclang_link_apath ${libclang_${IT_CONFIG}_found_so_apath} DIRECTORY)
            link_directories(${libclang_link_apath})
            get_filename_component(libclang_found_name ${libclang_${IT_CONFIG}_found_so_apath} NAME_WE)
            string(REGEX REPLACE "^lib" "" libclang_found_name ${libclang_found_name})

            # message("libclang_found_name at ${libclang_found_name}")
            set_property(TARGET libclang PROPERTY INTERFACE_LINK_LIBRARIES_${IT_CONFIG} ${libclang_found_name})
            set_property(TARGET libclang PROPERTY IMPORTED_LOCATION_${IT_CONFIG} ${libclang_${IT_CONFIG}_found_so_apath})

            # set(libclang_${IT_CONFIG}_found_apath ${libclang_${IT_CONFIG}_found_so_apath})
            get_symlink_follow_list(libclang_${IT_CONFIG}_found_apath ${libclang_${IT_CONFIG}_found_so_apath})
            message("${IT_CONFIG}: found library at ${libclang_link_apath}")
            set(matched_${IT_CONFIG} TRUE)
          endif()
        endif()
      endforeach()
    endif()
  endforeach()


  # install info
  if(user_install_prefix)
    string(REGEX REPLACE "^~" "$ENV{HOME}" CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
    get_filename_component(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} ABSOLUTE)
    set(binary_destination_apath ${CMAKE_INSTALL_PREFIX})
    set(library_destination_apath ${CMAKE_INSTALL_PREFIX})
  else()
    set(binary_destination_apath ${CMAKE_INSTALL_PREFIX}/bin)
    set(library_destination_apath ${CMAKE_INSTALL_PREFIX}/lib/${project_name})
  endif()

  
  # The rpath is runtime library search path. The rpath specify path where you want to install a shared object.
  set(CMAKE_EXE_LINKER_FLAGS "-Wl,-rpath,${library_destination_apath}")

  
  foreach(IT_CONFIG ${project_configurations})
    if(NOT matched_${IT_CONFIG})
      message("not found ${IT_CONFIG} library")
    endif()
  endforeach()

  
else()

  message("sorry, not support environment")

endif()



# ----------------------------------------
# after common setting
# ----------------------------------------

add_executable(${project_name} ${project_sources})

# set_property(TARGET ${project_name} PROPERTY CXX_STANDARD 11)

set_property(TARGET ${project_name} PROPERTY OUTPUT_NAME_DEBUG "${project_elf_name}-debug")
set_property(TARGET ${project_name} PROPERTY OUTPUT_NAME_RELEASE "${project_elf_name}")
# set_property(TARGET ${project_name} PROPERTY OUTPUT_NAME_MINSIZEREL "${project_elf_name}")
set_property(TARGET ${project_name} PROPERTY OUTPUT_NAME_RELWITHDEBINFO "${project_elf_name}-reldbg")


list(APPEND project_defs -DCMAKE_GENERATOR="${CMAKE_GENERATOR}" -DCMAKE_HOST_SYSTEM_PROCESSOR="${CMAKE_HOST_SYSTEM_PROCESSOR}" -DPROJECT_VERSION="${project_version}")

# list(APPEND project_defs CMAKE_GENERATOR="${CMAKE_GENERATOR}")
# set_property(TARGET ${project_name} PROPERTY COMPILE_DEFINITIONS ${project_defs})
# set_property(TARGET ${project_name} PROPERTY COMPILE_DEFINITIONS_DEBUG ${project_defs})
# set_property(TARGET ${project_name} PROPERTY COMPILE_DEFINITIONS_RELEASE ${project_defs})
add_definitions(${project_defs})

include_directories(./)


target_link_libraries(${project_name} ${dependency_libraries})


get_target_property(project_binary_output_name ${project_name} OUTPUT_NAME_${project_build_type})

# if(UNIX)
#   add_custom_command(TARGET ${project_name} POST_BUILD COMMAND ${CMAKE_COMMAND} -E create_symlink ${project_binary_output_name} ${project_name} 
#     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} COMMENT "-- Generate Symbolic Link : ${project_binary_output_name} -> ${project_name}")
#   install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${project_name} DESTINATION ${binary_destination_apath})
# endif()



# ----------------------------------------
# install setting
# ----------------------------------------

set(CMAKE_INSTALL_RPATH ${library_destination_apath})
install(TARGETS ${project_name} RUNTIME DESTINATION ${binary_destination_apath})
foreach(IT_CONFIG ${project_configurations})
  install(FILES ${libclang_${IT_CONFIG}_found_apath} DESTINATION ${library_destination_apath} CONFIGURATIONS ${IT_CONFIG})
endforeach()

if(UNIX)
  install(CODE "
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${project_binary_output_name} ${project_name} WORKING_DIRECTORY ${binary_destination_apath})
  message(\"-- Generate Symbolic Link : ${project_binary_output_name} -> ${project_name}\")
")
endif()


# ----------------------------------------
# debug display for variables
# ----------------------------------------


# get_directory_property(DirDefs DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS)
# foreach(IT_DEF ${DirDefs})
#   message(STATUS "Found Define: " ${IT_DEF})
# endforeach()
# message( "DirDefs: " ${DirDefs} )

set(echo_vars)
list(APPEND echo_vars CMAKE_BUILD_TYPE CMAKE_CONFIGURATION_TYPES project_configurations CMAKE_GENERATOR CMAKE_HOST_SYSTEM_PROCESSOR CMAKE_CURRENT_SOURCE_DIR CMAKE_CURRENT_BINARY_DIR collected_library_apath library_apaths)
list(APPEND echo_vars CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE project_defs)
list(APPEND echo_vars CMAKE_EXE_LINKER_FLAGS CMAKE_EXE_LINKER_FLAGS_DEBUG CMAKE_EXE_LINKER_FLAGS_RELEASE)
list(APPEND echo_vars project_binary_output_name)
list(APPEND echo_vars dependency_libraries binary_destination_apath library_destination_apath)
list(APPEND echo_vars CMAKE_FIND_LIBRARY_PREFIXES CMAKE_FIND_LIBRARY_SUFFIXES)
# list(APPEND echo_vars CMAKE_IMPORT_LIBRARY_SUFFIXES CMAKE_LINK_LIBRARY_SUFFIX CMAKE_SHARED_LIBRARY_SUFFIX CMAKE_SHARED_MODULE_SUFFIX CMAKE_STATIC_LIBRARY_SUFFIX)
# list(APPEND echo_vars PROJECT_SOURCE_DIR PROJECT_BINARY_DIR CMAKE_SOURCE_DIR CMAKE_BINARY_DIR CMAKE_RUNTIME_OUTPUT_DIRECTORY CMAKE_LIBRARY_OUTPUT_DIRECTORY CMAKE_INSTALL_NAME_DIR CMAKE_INCLUDE_CURRENT_DIR CMAKE_HOME_DIRECTORY CMAKE_CURRENT_LIST_DIR CMAKE_CURRENT_BINARY_DIR CMAKE_BINARY_DIR CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${project_name}_SOURCE_DIR ${project_name}_BINARY_DIR)
list(APPEND echo_vars CMAKE_INSTALL_PREFIX CMAKE_INSTALL_RPATH CMAKE_SKIP_BUILD_RPATH CMAKE_BUILD_WITH_INSTALL_RPATH CMAKE_SKIP_RPATH CMAKE_SKIP_INSTALL_RPATH)
list(APPEND echo_vars CMAKE_PREFIX_PATH CMAKE_LIBRARY_PATH CMAKE_FRAMEWORK_PATH CMAKE_SYSTEM_PREFIX_PATH CMAKE_SYSTEM_LIBRARY_PATH CMAKE_SYSTEM_FRAMEWORK_PATH)
display_vars("${echo_vars}" "final: ")


