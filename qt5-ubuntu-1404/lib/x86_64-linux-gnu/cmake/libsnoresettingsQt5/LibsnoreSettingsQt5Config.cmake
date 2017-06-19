
####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() (ECM variant) #######
####### Any changes to this file will be overwritten by the next CMake run            #######
####### The input file was LibsnoreSettingsConfig.cmake.in                                           #######

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../../" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

include(CMakeFindDependencyMacro OPTIONAL RESULT_VARIABLE _CMakeFindDependencyMacro_FOUND)

if (NOT _CMakeFindDependencyMacro_FOUND)
  macro(find_dependency dep)
    if (NOT ${dep}_FOUND)

      set(ecm_fd_version)
      if (${ARGC} GREATER 1)
        set(ecm_fd_version ${ARGV1})
      endif()
      set(ecm_fd_exact_arg)
      if(${CMAKE_FIND_PACKAGE_NAME}_FIND_VERSION_EXACT)
        set(ecm_fd_exact_arg EXACT)
      endif()
      set(ecm_fd_quiet_arg)
      if(${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY)
        set(ecm_fd_quiet_arg QUIET)
      endif()
      set(ecm_fd_required_arg)
      if(${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED)
        set(ecm_fd_required_arg REQUIRED)
      endif()

      find_package(${dep} ${ecm_fd_version}
          ${ecm_fd_exact_arg}
          ${ecm_fd_quiet_arg}
          ${ecm_fd_required_arg}
      )

      if (NOT ${dep}_FOUND)
        set(${CMAKE_FIND_PACKAGE_NAME}_NOT_FOUND_MESSAGE "${CMAKE_FIND_PACKAGE_NAME} could not be found because dependency ${dep} could not be found.")
        set(${CMAKE_FIND_PACKAGE_NAME}_FOUND False)
        return()
      endif()

      set(ecm_fd_version)
      set(ecm_fd_required_arg)
      set(ecm_fd_quiet_arg)
      set(ecm_fd_exact_arg)
    endif()
  endmacro()
endif()


macro(check_required_components _NAME)
  foreach(comp ${${_NAME}_FIND_COMPONENTS})
    if(NOT ${_NAME}_${comp}_FOUND)
      if(${_NAME}_FIND_REQUIRED_${comp})
        set(${_NAME}_FOUND FALSE)
      endif()
    endif()
  endforeach()
endmacro()

####################################################################################

find_dependency(LibsnoreQt5 REQUIRED)
find_dependency(Qt5Widgets REQUIRED)

include("${CMAKE_CURRENT_LIST_DIR}/LibsnoreSettingsQt5Target.cmake")
