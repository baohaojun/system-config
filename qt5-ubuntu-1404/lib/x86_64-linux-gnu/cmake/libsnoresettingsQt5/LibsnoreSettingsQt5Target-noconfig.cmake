#----------------------------------------------------------------
# Generated CMake target import file for configuration "".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "Snore::LibsnoreSettings" for configuration ""
set_property(TARGET Snore::LibsnoreSettings APPEND PROPERTY IMPORTED_CONFIGURATIONS NOCONFIG)
set_target_properties(Snore::LibsnoreSettings PROPERTIES
  IMPORTED_LOCATION_NOCONFIG "${_IMPORT_PREFIX}/lib/x86_64-linux-gnu/libsnoresettings-qt5.so.0.7.0"
  IMPORTED_SONAME_NOCONFIG "libsnoresettings-qt5.so.0.7"
  )

list(APPEND _IMPORT_CHECK_TARGETS Snore::LibsnoreSettings )
list(APPEND _IMPORT_CHECK_FILES_FOR_Snore::LibsnoreSettings "${_IMPORT_PREFIX}/lib/x86_64-linux-gnu/libsnoresettings-qt5.so.0.7.0" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
